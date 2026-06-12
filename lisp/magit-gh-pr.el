;;; magit-gh-pr.el --- Fetch GitHub PR comments and review threads to disk -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (magit "4.0") (annotated-completing-read "0.1"))
;; Keywords: vc, tools, magit, github
;; URL: https://github.com/tychoish/dot-emacs

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Provides `magit-gh-pr-fetch', which downloads PR review threads and
;; issue comments for the current branch's pull request to a local directory
;; under plans/.  Writes an index file that records the PR state, which
;; threads are unresolved, who created each thread, and who last commented.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'transient)
(require 'magit-gh-extras)

(declare-function magit-toplevel "magit-git")
(declare-function magit-get-current-branch "magit-git")
(declare-function magit-dash--format-age "magit-dash")

;;; Configuration

(defvar magit-gh-pr-thread-limit 100
  "Maximum number of review threads to fetch per pull request.")

;;; Internal helpers

(defun magit-gh-pr--parse-threads (graphql-response)
  "Extract thread summaries from GRAPHQL-RESPONSE alist.
Returns a list of plists :id :resolved :path :creator :last-commentor.
:resolved is t when the thread is resolved, nil otherwise."
  (when-let* ((data      (map-elt graphql-response 'data))
              (repo      (map-elt data 'repository))
              (pr        (map-elt repo 'pullRequest))
              (rt        (map-elt pr 'reviewThreads))
              (nodes     (map-elt rt 'nodes)))
    (seq-map
     (lambda (thread)
       (let* ((comments     (map-elt (map-elt thread 'comments) 'nodes))
              (first-comment (car comments))
              (last-comment  (car (last comments))))
         (list :id             (or (map-elt thread 'id) "")
               :resolved       (eq (map-elt thread 'isResolved) t)
               :path           (or (map-elt thread 'path) "")
               :creator        (or (map-elt (map-elt first-comment 'author) 'login) "")
               :last-commentor (or (map-elt (map-elt last-comment  'author) 'login) ""))))
     nodes)))

(defun magit-gh-pr--thread-table (thread)
  "Convert THREAD plist to a string-keyed hash-table for serialization."
  (let ((ht (make-hash-table :test #'equal)))
    (puthash "id"             (or (plist-get thread :id) "")                  ht)
    (puthash "resolved"       (if (plist-get thread :resolved) t :false)      ht)
    (puthash "path"           (or (plist-get thread :path) "")                ht)
    (puthash "creator"        (or (plist-get thread :creator) "")             ht)
    (puthash "last_commentor" (or (plist-get thread :last-commentor) "")      ht)
    ht))

(defun magit-gh-pr--graphql-query ()
  "Return the GraphQL query string for fetching review threads."
  (format "query($owner:String! $repo:String! $number:Int!){
  repository(owner:$owner name:$repo){
    pullRequest(number:$number){
      reviewThreads(first:%d){
        nodes{
          id isResolved path
          comments(first:100){
            nodes{ author{login} body createdAt }
          }
        }
      }
    }
  }
}" magit-gh-pr-thread-limit))

;;; Pipeline steps

(defun magit-gh-pr--step-finalize (ctx)
  "Write the index file and report completion to the user."
  (let* ((dir      (plist-get ctx :dir))
         (pr-info  (plist-get ctx :pr-info))
         (threads  (plist-get ctx :threads))
         (files    (plist-get ctx :files))
         (author   (map-elt (map-elt pr-info 'author) 'login))
         (has-unresolved (seq-some (lambda (th) (not (plist-get th :resolved)))
                                   threads))
         (data (magit-gh--index-table
                "collected_at"           (format-time-string "%Y-%m-%dT%H:%M:%SZ" nil t)
                "type"                   "pr"
                "pr_number"              (or (map-elt pr-info 'number) 0)
                "title"                  (or (map-elt pr-info 'title) "")
                "branch"                 (or (map-elt pr-info 'headRefName) "")
                "state"                  (or (map-elt pr-info 'state) "")
                "author"                 (or author "")
                "review_decision"        (or (map-elt pr-info 'reviewDecision) "")
                "has_unresolved_threads" (if has-unresolved t :false)
                "artifact_count"         (length files)
                "files"                  (apply #'vector
                                                (seq-map (lambda (f)
                                                           (magit-gh--file-table
                                                            (plist-get f :path)
                                                            (plist-get f :type)))
                                                         files))
                "threads"                (apply #'vector
                                                (seq-map #'magit-gh-pr--thread-table
                                                         threads)))))
    (magit-gh--write-index dir data)
    (message "magit-gh-pr: done — %d file(s) in %s" (length files) dir)))

(defun magit-gh-pr--step-comments (ctx)
  "Fetch issue-style comments on the PR, write pr-issue-comments.json."
  (let* ((owner    (plist-get ctx :owner))
         (repo     (plist-get ctx :repo))
         (pr-num   (plist-get ctx :pr-number))
         (repo-dir (plist-get ctx :repo-dir))
         (endpoint (format "/repos/%s/%s/issues/%s/comments" owner repo pr-num)))
    (message "magit-gh-pr: fetching issue comments...")
    (magit-gh--run-process
     (list "api" endpoint "--paginate")
     repo-dir
     (lambda (output)
       (let ((file "pr-issue-comments.json"))
         (with-temp-file (expand-file-name file (plist-get ctx :dir))
           (insert output))
         (magit-gh-pr--step-finalize
          (magit-gh--add-file ctx file "issue-comments"))))
     (magit-gh--make-error-handler "magit-gh-pr" "issue-comments"))))

(defun magit-gh-pr--step-threads (ctx)
  "Fetch review threads via GraphQL, write pr-review-threads.json."
  (let* ((owner    (plist-get ctx :owner))
         (repo     (plist-get ctx :repo))
         (pr-num   (plist-get ctx :pr-number))
         (repo-dir (plist-get ctx :repo-dir)))
    (message "magit-gh-pr: fetching review threads...")
    (magit-gh--run-process
     (list "api" "graphql"
           "-f" (format "query=%s" (magit-gh-pr--graphql-query))
           "-F" (format "owner=%s" owner)
           "-F" (format "repo=%s"  repo)
           "-F" (format "number=%s" pr-num))
     repo-dir
     (lambda (output)
       (let* ((file    "pr-review-threads.json")
              (parsed  (json-parse-string output
                                          :array-type  'list
                                          :object-type 'alist))
              (threads (or (magit-gh-pr--parse-threads parsed) nil)))
         (with-temp-file (expand-file-name file (plist-get ctx :dir))
           (insert output))
         (magit-gh-pr--step-comments
          (magit-gh--add-file
           (plist-put ctx :threads threads)
           file "review-threads"))))
     (magit-gh--make-error-handler "magit-gh-pr" "review-threads"))))

(defun magit-gh-pr--step-info (ctx)
  "Fetch PR metadata, write pr-info.json, then continue to threads."
  (let* ((pr-arg   (plist-get ctx :pr-arg))
         (repo-dir (plist-get ctx :repo-dir))
         (args     (append
                    '("pr" "view")
                    (when pr-arg (list (number-to-string pr-arg)))
                    '("--json"
                      "number,title,state,author,body,createdAt,updatedAt,reviewDecision,headRefName,baseRefName"))))
    (message "magit-gh-pr: fetching PR info...")
    (magit-gh--run-process
     args
     repo-dir
     (lambda (output)
       (let* ((pr-info  (json-parse-string output :object-type 'alist))
              (pr-num   (map-elt pr-info 'number))
              (branch   (or (map-elt pr-info 'headRefName)
                            (plist-get ctx :branch)))
              (slug     (magit-gh--branch-slug branch))
              (dir      (magit-gh--collect-dir
                         (plist-get ctx :root) 'pr slug pr-num))
              (file     "pr-info.json")
              (ctx2     (copy-sequence ctx))
              (ctx2     (plist-put ctx2 :pr-info pr-info))
              (ctx2     (plist-put ctx2 :pr-number pr-num))
              (ctx2     (plist-put ctx2 :dir dir)))
         (with-temp-file (expand-file-name file dir)
           (insert output))
         (magit-gh-pr--step-threads
          (magit-gh--add-file ctx2 file "metadata"))))
     (magit-gh--make-error-handler "magit-gh-pr" "pr-info"))))

;;; Public API

;;;###autoload
(defun magit-gh-pr-fetch (&optional pr-number)
  "Download comments and review threads for PR-NUMBER (or the current branch's PR).
When PR-NUMBER is nil, auto-detects the PR for the current branch.

Creates an artifact directory under plans/ containing:
  pr-info.json            — PR metadata and review decision
  pr-review-threads.json  — all review threads with resolved status
  pr-issue-comments.json  — top-level (non-review) comments
  index.json              — collection summary with per-thread creator/last-commentor"
  (interactive)
  (magit-gh--check-gh)
  (let* ((repo-dir  (magit-gh--repo-dir))
         (root      (or (magit-toplevel)
                        (user-error "Not inside a git repository")))
         (branch    (magit-get-current-branch))
         (repo-info (magit-gh--repo-info))
         (ctx       (list :branch   branch
                          :root     root
                          :repo-dir repo-dir
                          :owner    (plist-get repo-info :owner)
                          :repo     (plist-get repo-info :repo)
                          :pr-arg   pr-number
                          :files    nil
                          :threads  nil)))
    (magit-gh-pr--step-info ctx)))

;;;; PR dashboard mode

(defvar-local magit-gh-pr-dashboard--filters
  (list :state "open" :author "@me" :repo nil :org nil)
  "Current PR dashboard filters plist.
Keys: :state (\"open\"/\"closed\"), :author, :repo (OWNER/NAME), :org.")

(defface magit-gh-pr-ci-pass-face
  '((t :inherit success))
  "Face for passing CI status in the PR dashboard.")

(defface magit-gh-pr-ci-fail-face
  '((t :inherit error))
  "Face for failing CI status in the PR dashboard.")

(defface magit-gh-pr-ci-pending-face
  '((t :inherit warning))
  "Face for pending CI status in the PR dashboard.")

(defconst magit-gh-pr-dashboard--format
  [("Repo" 22 t)
   ("PR#" 5 nil)
   ("Title" 38 t)
   ("CI" 8 t)
   ("Age" 6 nil)
   ("Cmts" 4 nil)
   ("Review" 14 t)]
  "Column format for `magit-gh-pr-dashboard-mode'.")

(defvar magit-gh-pr-dashboard-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") #'magit-gh-pr-dashboard-open-browser)
    (define-key m (kbd "g")   #'magit-gh-pr-dashboard-refresh)
    (define-key m (kbd "r")   #'magit-gh-pr-dashboard-refresh)
    (define-key m (kbd "f")   #'magit-gh-pr-dashboard-set-filter)
    (define-key m (kbd "F")   #'magit-gh-pr-dashboard-clear-filters)
    (define-key m (kbd "m")   #'magit-gh-pr-dashboard-menu)
    (define-key m (kbd "?")   #'magit-gh-pr-dashboard-menu)
    (define-key m (kbd "q")   #'quit-window)
    m)
  "Keymap for `magit-gh-pr-dashboard-mode'.")

(define-derived-mode magit-gh-pr-dashboard-mode tabulated-list-mode "PRs"
  "Major mode for the pull request dashboard."
  (setq tabulated-list-format magit-gh-pr-dashboard--format)
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header)
  (setq-local magit-gh-pr-dashboard--filters
              (list :state "open" :author "@me" :repo nil :org nil)))

(defun magit-gh-pr-dashboard--format-ci (rollup-state)
  "Format CI ROLLUP-STATE string for display."
  (pcase rollup-state
    ("SUCCESS" (propertize "pass" 'face 'magit-gh-pr-ci-pass-face))
    ("FAILURE" (propertize "fail" 'face 'magit-gh-pr-ci-fail-face))
    ("PENDING" (propertize "pending" 'face 'magit-gh-pr-ci-pending-face))
    ("ERROR" (propertize "error" 'face 'magit-gh-pr-ci-fail-face))
    (_ "—")))

(defun magit-gh-pr-dashboard--format-review (decision)
  "Format review DECISION string for display."
  (pcase decision
    ("APPROVED" (propertize "approved" 'face 'success))
    ("CHANGES_REQUESTED" (propertize "changes req" 'face 'error))
    ("REVIEW_REQUIRED" (propertize "needed" 'face 'warning))
    (_ "")))

(defun magit-gh-pr-dashboard--format-age (updated-at)
  "Format UPDATED-AT ISO timestamp as a human-readable age string."
  (when (and updated-at (not (equal updated-at "")))
    (condition-case nil
        (magit-dash--format-age
         (float-time (time-since (date-to-time updated-at))))
      (error "?"))))

(defun magit-gh-pr-dashboard--comments-count (pr)
  "Return the comment count from PR alist."
  (let ((c (map-elt pr 'commentsCount)))
    (if (numberp c) c 0)))

(defun magit-gh-pr-dashboard--build-args (filters)
  "Return a gh args list for fetching PRs matching FILTERS plist.
When :repo is set, uses `gh pr list -R REPO' (includes CI status).
Otherwise uses `gh search prs' for cross-repo listing."
  (let* ((state (or (plist-get filters :state) "open"))
         (repo (plist-get filters :repo))
         (author (plist-get filters :author))
         (org (plist-get filters :org)))
    (if repo
        (append
         (list "pr" "list" "-R" repo "--state" state
               "--json"
               "number,title,state,author,updatedAt,commentsCount,reviewDecision,statusCheckRollup,isDraft,url")
         (when author (list "--author" author)))
      (append
       (list "search" "prs"
             "--state" (if (member state '("open" "closed")) state "open")
             "--json"
             "number,title,state,repository,author,updatedAt,commentsCount,isDraft,url")
       (when author
         (list "--author" author))
       (when org
         (list "--owner" org))))))

(defconst magit-gh-pr-dashboard--title-width 36
  "Maximum display width for PR title column.")

(defun magit-gh-pr-dashboard--build-entry (pr repo-name)
  "Return a `tabulated-list-entries' entry for PR alist in REPO-NAME."
  (let ((number (map-elt pr 'number)))
    (list (list :number number :repo repo-name :pr pr)
          (vector
           repo-name
           (number-to-string number)
           (truncate-string-to-width
            (or (map-elt pr 'title) "") magit-gh-pr-dashboard--title-width nil nil "…")
           (magit-gh-pr-dashboard--format-ci
            (map-elt (map-elt pr 'statusCheckRollup) 'state))
           (or (magit-gh-pr-dashboard--format-age (map-elt pr 'updatedAt)) "?")
           (number-to-string (magit-gh-pr-dashboard--comments-count pr))
           (magit-gh-pr-dashboard--format-review (map-elt pr 'reviewDecision))))))

(defun magit-gh-pr-dashboard--parse-output (output filters)
  "Parse gh JSON OUTPUT into tabulated list entries using FILTERS for context.
Returns nil when OUTPUT is not a JSON array."
  (let ((trimmed (string-trim output)))
    (when (string-prefix-p "[" trimmed)
      (when-let* ((prs (json-parse-string trimmed :array-type 'list :object-type 'alist)))
        (let ((per-repo-p (plist-get filters :repo)))
          (seq-map (lambda (pr)
                     (magit-gh-pr-dashboard--build-entry
                      pr
                      (if per-repo-p
                          (plist-get filters :repo)
                        (or (map-elt (map-elt pr 'repository) 'nameWithOwner)
                            "unknown"))))
                   prs))))))

(defun magit-gh-pr-dashboard-refresh ()
  "Fetch PRs matching current filters and repopulate the PR table."
  (interactive)
  (let* ((filters magit-gh-pr-dashboard--filters)
         (args (magit-gh-pr-dashboard--build-args filters))
         (buf (current-buffer))
         (dir (or (ignore-errors (magit-gh--repo-dir))
                  (expand-file-name "~"))))
    (setq tabulated-list-entries nil)
    (tabulated-list-print t)
    (message "magit-gh: fetching PRs...")
    (magit-gh--run-process
     args dir
     (lambda (output)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (setq tabulated-list-entries
                 (or (magit-gh-pr-dashboard--parse-output output filters) nil))
           (tabulated-list-print t)
           (message "magit-gh: %d PR(s)"
                    (length tabulated-list-entries))))))))

(defun magit-gh-pr-dashboard--entry-at-point ()
  "Return the PR entry plist at point or signal `user-error'."
  (or (tabulated-list-get-id)
      (user-error "No pull request at point")))

(defun magit-gh-pr-dashboard-open-browser ()
  "Open the pull request at point in a web browser."
  (interactive)
  (when-let* ((entry (magit-gh-pr-dashboard--entry-at-point))
              (pr (plist-get entry :pr))
              (url (map-elt pr 'url)))
    (browse-url url)))

(defun magit-gh-pr-dashboard-set-filter ()
  "Interactively set one PR dashboard filter field."
  (interactive)
  (let* ((current magit-gh-pr-dashboard--filters)
         (field (completing-read "Filter field: "
                                 '("state" "author" "repo" "org") nil t))
         (value (pcase field
                  ("state"
                   (completing-read "State: " '("open" "closed") nil t))
                  ("author"
                   (read-string "Author (@me for current user): "
                                (or (plist-get current :author) "@me")))
                  ("repo"
                   (read-string "Repo (OWNER/NAME, empty to clear): "
                                (or (plist-get current :repo) "")))
                  ("org"
                   (read-string "Org (empty to clear): "
                                (or (plist-get current :org) "")))))
         (key (intern (format ":%s" field)))
         (new-val (if (string-empty-p value) nil value)))
    (setq magit-gh-pr-dashboard--filters
          (plist-put (copy-sequence current) key new-val))
    (magit-gh-pr-dashboard-refresh)))

(defun magit-gh-pr-dashboard-clear-filters ()
  "Reset PR dashboard filters to defaults (open PRs, current user)."
  (interactive)
  (setq magit-gh-pr-dashboard--filters
        (list :state "open" :author "@me" :repo nil :org nil))
  (magit-gh-pr-dashboard-refresh))

;;;###autoload
(defun magit-gh-pr-dashboard-open ()
  "Open the pull request dashboard buffer."
  (interactive)
  (magit-gh--check-gh)
  (let ((buf (get-buffer-create "*magit-gh-prs*")))
    (with-current-buffer buf
      (magit-gh-pr-dashboard-mode)
      (magit-gh-pr-dashboard-refresh))
    (pop-to-buffer buf)))

(transient-define-prefix magit-gh-pr-dashboard-menu ()
  "Actions for the PR dashboard."
  [["Pull Request"
    ("RET" "Open in browser" magit-gh-pr-dashboard-open-browser)]
   ["Filter"
    ("fs" "Set filter…" magit-gh-pr-dashboard-set-filter)
    ("fc" "Clear all filters" magit-gh-pr-dashboard-clear-filters)]
   ["Dashboard"
    ("g" "Refresh" magit-gh-pr-dashboard-refresh)
    ("q" "Quit" quit-window)]])

(provide 'magit-gh-pr)

;;; magit-gh-pr.el ends here
