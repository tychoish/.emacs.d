;;; magit-gh-ci.el --- Fetch GitHub Actions CI logs to disk -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (magit "4.0") (annotated-completing-read "0.1"))
;; Keywords: vc, tools, magit, github, ci
;; URL: https://github.com/tychoish/dot-emacs

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Provides `magit-gh-ci-fetch', which downloads GitHub Actions CI logs for
;; the current branch to a local directory under plans/.  A sequential
;; async pipeline fetches the run list, run metadata, full logs, and
;; optionally a failed-steps-only log, then writes an index file summarising
;; the run state and collected artifacts.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'magit-gh-extras)

(declare-function magit-toplevel "magit-git")
(declare-function magit-get-current-branch "magit-git")

;;; Configuration

(defvar magit-gh-ci-run-limit 10
  "Maximum number of CI runs shown when interactively selecting a run.")

(defvar magit-gh-ci-include-failed-log t
  "When non-nil, download a failed-steps-only log alongside the full run log.")

(defvar magit-gh-ci-open-dired nil
  "When non-nil, open a dired buffer in the artifact directory after fetch completes.")

;;; Log viewer mode

(define-derived-mode magit-gh-ci-log-mode special-mode "GH-Log"
  "Major mode for viewing GitHub Actions CI log files.
Applies ANSI colour sequences and provides read-only navigation."
  (when (fboundp 'ansi-color-apply-on-region)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))

(add-to-list 'auto-mode-alist '("\\.ghlog\\'" . magit-gh-ci-log-mode))

;;; Internal helpers

(defun magit-gh-ci--failure-p (conclusion)
  "Return non-nil when CONCLUSION string indicates a failed run."
  (member conclusion '("failure" "timed_out" "startup_failure")))

(defun magit-gh-ci--run-annotation (run &optional ordinal)
  "Return a one-line annotation string for RUN alist.
ORDINAL, when non-nil, is an integer position in the sorted run list (1 = most recent)."
  (let ((sha (map-elt run 'headSha)))
    (format "%4s  %-12s %-12s  %-30s  %s"
            (if ordinal (format "#%d" ordinal) "")
            (or (map-elt run 'status) "")
            (or (map-elt run 'conclusion) "in_progress")
            (or (map-elt run 'workflowName) "")
            (if sha (substring sha 0 (min 8 (length sha))) ""))))

(defun magit-gh-ci--sort-runs (runs)
  "Return RUNS sorted most-recent first by createdAt."
  (sort (copy-sequence runs)
        (lambda (a b)
          (string> (or (map-elt a 'createdAt) "")
                   (or (map-elt b 'createdAt) "")))))

(defun magit-gh-ci--select-run (runs)
  "Prompt the user to select from RUNS via annotated-completing-read.
Runs are presented most-recent first.  The annotation shows an ordinal
\\=#N (where #1 is most recent) and the first 8 characters of the commit SHA.
When RUNS has exactly one entry it is returned directly without prompting."
  (if (= (length runs) 1)
      (car runs)
    (let ((table (make-hash-table :test #'equal))
          (sorted (magit-gh-ci--sort-runs runs)))
      (seq-map-indexed
       (lambda (run i)
         (let ((key (format "#%s %s"
                            (map-elt run 'databaseId)
                            (or (map-elt run 'name) ""))))
           (map-put! table key (magit-gh-ci--run-annotation run (1+ i)))))
       sorted)
      (let* ((choice (annotated-completing-read
                      table
                      :prompt "CI run => "
                      :require-match t))
             (id (when (string-match "\\`#\\([0-9]+\\) " choice)
                   (string-to-number (match-string 1 choice)))))
        (seq-find (lambda (r) (= (map-elt r 'databaseId) id)) sorted)))))

;;; Pipeline steps

(defun magit-gh-ci--step-finalize (ctx)
  "Write the index file and report completion to the user."
  (let* ((dir      (plist-get ctx :dir))
         (run-info (plist-get ctx :run-info))
         (files    (plist-get ctx :files))
         (conclusion (or (map-elt run-info 'conclusion) "in_progress"))
         (data (magit-gh--index-table
                "collected_at"  (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t)
                "type"          "ci"
                "branch"        (or (map-elt run-info 'headBranch) "")
                "run_id"        (or (map-elt run-info 'databaseId) 0)
                "workflow"      (or (map-elt run-info 'workflowName) "")
                "status"        (or (map-elt run-info 'status) "")
                "conclusion"    conclusion
                "has_failure"   (if (magit-gh-ci--failure-p conclusion) t :false)
                "artifact_count" (length files)
                "files"         (apply #'vector
                                       (seq-map (lambda (f)
                                                  (magit-gh--file-table
                                                   (plist-get f :path)
                                                   (plist-get f :type)))
                                                files)))))
    (magit-gh--write-index dir data)
    (message "magit-gh-ci: done — %d file(s) in %s" (length files) dir)
    (when magit-gh-ci-open-dired
      (dired dir))))

(defun magit-gh-ci--step-failed-logs (ctx)
  "Fetch failed-step-only logs when applicable, then finalize."
  (let* ((run-info   (plist-get ctx :run-info))
         (conclusion (map-elt run-info 'conclusion))
         (run-id     (number-to-string (map-elt run-info 'databaseId)))
         (repo-dir   (plist-get ctx :repo-dir)))
    (if (and magit-gh-ci-include-failed-log (magit-gh-ci--failure-p conclusion))
        (progn
          (message "magit-gh-ci: fetching failed-step logs...")
          (magit-gh--run-process
           (list "run" "view" run-id "--log-failed")
           repo-dir
           (lambda (output)
             (let ((file "run-failed-logs.ghlog"))
               (with-temp-file (expand-file-name file (plist-get ctx :dir))
                 (insert output))
               (magit-gh-ci--step-finalize
                (magit-gh--add-file ctx file "failed-logs"))))
           (magit-gh--make-error-handler "magit-gh-ci" "failed-logs")))
      (magit-gh-ci--step-finalize ctx))))

(defun magit-gh-ci--step-logs (ctx)
  "Fetch the full text log for the selected run, then continue."
  (let* ((run-info (plist-get ctx :run-info))
         (run-id   (number-to-string (map-elt run-info 'databaseId)))
         (repo-dir (plist-get ctx :repo-dir)))
    (message "magit-gh-ci: fetching full logs (run #%s, may be large)..." run-id)
    (magit-gh--run-process
     (list "run" "view" run-id "--log")
     repo-dir
     (lambda (output)
       (let ((file "run-logs.ghlog"))
         (with-temp-file (expand-file-name file (plist-get ctx :dir))
           (insert output))
         (magit-gh-ci--step-failed-logs
          (magit-gh--add-file ctx file "logs"))))
     (magit-gh--make-error-handler "magit-gh-ci" "logs"))))

(defun magit-gh-ci--step-run-info (ctx)
  "Fetch full run metadata, write run-info.json, then fetch logs."
  (let* ((run-id   (number-to-string (plist-get ctx :run-id)))
         (repo-dir (plist-get ctx :repo-dir)))
    (message "magit-gh-ci: fetching run info for #%s..." run-id)
    (magit-gh--run-process
     (list "run" "view" run-id "--json"
           "databaseId,name,status,conclusion,createdAt,updatedAt,headBranch,headSha,event,workflowName,jobs")
     repo-dir
     (lambda (output)
       (let* ((run-info (json-parse-string output :object-type 'alist))
              (file     "run-info.json")
              (slug     (magit-gh--branch-slug
                         (or (map-elt run-info 'headBranch)
                             (plist-get ctx :branch))))
              (dir      (magit-gh--collect-dir
                         (plist-get ctx :root) 'ci slug
                         (map-elt run-info 'databaseId)))
              (ctx2     (plist-put (copy-sequence ctx) :run-info run-info))
              (ctx2     (plist-put ctx2 :dir dir)))
         (with-temp-file (expand-file-name file dir)
           (insert output))
         (magit-gh-ci--step-logs
          (magit-gh--add-file ctx2 file "metadata"))))
     (magit-gh--make-error-handler "magit-gh-ci" "run-info"))))

(defun magit-gh-ci--step-list (ctx)
  "List recent CI runs for the current branch and let the user select one."
  (let* ((branch   (plist-get ctx :branch))
         (repo-dir (plist-get ctx :repo-dir)))
    (message "magit-gh-ci: listing runs for %s..." branch)
    (magit-gh--run-process
     (list "run" "list"
           "--branch" branch
           "--limit" (number-to-string magit-gh-ci-run-limit)
           "--json" "databaseId,name,status,conclusion,createdAt,headBranch,headSha,event,workflowName")
     repo-dir
     (lambda (output)
       (let ((runs (json-parse-string output :array-type 'list :object-type 'alist)))
         (when (null runs)
           (user-error "magit-gh-ci: no CI runs found for branch %s" branch))
         (let* ((run    (magit-gh-ci--select-run runs))
                (run-id (map-elt run 'databaseId)))
           (magit-gh-ci--step-run-info
            (plist-put ctx :run-id run-id)))))
     (magit-gh--make-error-handler "magit-gh-ci" "run-list"))))

;;; Public API

;;;###autoload
(defun magit-gh-ci-fetch (&optional run-id)
  "Download GitHub Actions CI logs for the current branch.
When RUN-ID is non-nil, fetch that specific run without prompting.
Otherwise list recent runs and prompt for a selection.

Creates an artifact directory under plans/ containing:
  run-info.json        — full run metadata
  run-logs.ghlog       — complete step logs (opens in `magit-gh-ci-log-mode')
  run-failed-logs.ghlog — failed-step logs (when `magit-gh-ci-include-failed-log')
  index.json           — collection summary"
  (interactive)
  (magit-gh--check-gh)
  (let* ((repo-dir (magit-gh--repo-dir))
         (root     (or (magit-toplevel)
                       (user-error "Not inside a git repository")))
         (branch   (or (magit-get-current-branch)
                       (user-error "Not on a branch")))
         (ctx      (list :branch branch
                         :root root
                         :repo-dir repo-dir
                         :files nil)))
    (if run-id
        (magit-gh-ci--step-run-info (plist-put ctx :run-id run-id))
      (magit-gh-ci--step-list ctx))))

(provide 'magit-gh-ci)

;;; magit-gh-ci.el ends here
