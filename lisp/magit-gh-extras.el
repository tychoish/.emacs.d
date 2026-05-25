;;; magit-gh-extras.el --- Branch pruning extension for magit-gh -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (magit "4.0") (annotated-completing-read "0.1"))
;; Keywords: vc, tools, magit, github
;; URL: https://github.com/tychoish/dot-emacs

;; This file is not part of GNU Emacs

;;; Commentary:

;; Provides `magit-gh-prune-merged-branches', an interactive menu for
;; pruning local branches whose remote-tracking PRs are merged or
;; closed. Per-repo candidate and marked-branch state is cached on a
;; hidden state buffer keyed by repository toplevel.

;;; Code:

(require 'cl-lib)
(require 'map)

(require 'annotated-completing-read)

(declare-function magit-toplevel "magit-git")
(declare-function magit-list-local-branch-names "magit-git")
(declare-function magit-get-current-branch "magit-git")
(declare-function magit-branch-delete "magit-branch")
(declare-function magit-gh--repo-dir "magit-gh")
(declare-function magit-gh--check-gh "magit-gh")

(defvar magit-gh-prune-pr-limit 100
  "Maximum number of PRs to fetch from GitHub on each incremental scan.")

(defvar magit-gh-prune-cache-dir nil
  "Directory for per-repo closed-PR cache files, or nil to disable disk caching.")

(defvar-local magit-gh--prune-state nil
  "Buffer-local prune state for the current repo.
A plist with `:candidates' (alist of (BRANCH . PR-ALIST)) and
`:marked' (list of branch names selected for batch pruning).
Lives on a hidden state buffer per repository.")

(defun magit-gh--pr-closed-p (pr)
  "Return non-nil when PR alist represents a merged or closed PR."
  (member (map-elt pr 'state) '("MERGED" "CLOSED")))

(defun magit-gh--default-branch ()
  "Return the repository's default branch name, falling back to \"main\"."
  (let* ((default-directory (magit-gh--repo-dir))
	 (output (string-trim
		  (shell-command-to-string
		   "gh repo view --json defaultBranchRef --jq .defaultBranchRef.name"))))
    (if (string-empty-p output) "main" output)))

(defun magit-gh--prune-cache-file ()
  "Return the cache file path for the current repo, or nil when caching is disabled.
The filename uses the repo's directory basename plus a short hash for uniqueness."
  (when magit-gh-prune-cache-dir
    (let* ((dir (magit-gh--repo-dir))
	   (basename (file-name-nondirectory (directory-file-name dir)))
	   (short-hash (substring (secure-hash 'sha1 dir) 0 8)))
      (expand-file-name
       (format "%s-%s.eld" basename short-hash)
       magit-gh-prune-cache-dir))))

(defun magit-gh--prune-load-cache ()
  "Return a hash table of headRefName→PR-alist from the repo's cache file.
Returns an empty hash table when caching is disabled, no cache exists,
or the file is unreadable."
  (let ((table (make-hash-table :test #'equal)))
    (when-let ((file (magit-gh--prune-cache-file)))
      (when (file-exists-p file)
	(condition-case err
	    (with-temp-buffer
	      (insert-file-contents file)
	      (goto-char (point-min))
	      (dolist (pr (read (current-buffer)))
		(map-put! table (map-elt pr 'headRefName) pr)))
	  (error (message "magit-gh prune: ignoring unreadable cache: %s" err)))))
    table))

(defun magit-gh--prune-save-cache (table)
  "Persist the closed-PR hash TABLE to the repo cache file.
No-op when `magit-gh-prune-cache-dir' is nil."
  (when-let ((file (magit-gh--prune-cache-file)))
    (make-directory magit-gh-prune-cache-dir t)
    (let (prs)
      (map-do (lambda (_branch pr) (push pr prs)) table)
      (with-temp-file file
	(prin1 prs (current-buffer))))))

(defun magit-gh--fetch-closed-prs (&optional table)
  "Fetch recent PRs from GitHub, merge closed ones into TABLE, persist and return it.
TABLE defaults to the on-disk cache for the current repo.
Uses a single gh call fetching up to `magit-gh-prune-pr-limit' PRs."
  (let* ((default-directory (magit-gh--repo-dir))
	 (table (or table (magit-gh--prune-load-cache)))
	 (cmd (format "gh pr list --state all --limit %d --json number,state,headRefName,title"
		      magit-gh-prune-pr-limit))
	 (output (string-trim (shell-command-to-string cmd))))
    (when (string-prefix-p "[" output)
      (dolist (pr (json-parse-string output :array-type 'list :object-type 'alist))
	(when (magit-gh--pr-closed-p pr)
	  (let ((branch (map-elt pr 'headRefName)))
	    (unless (map-elt table branch)
	      (map-put! table branch pr))))))
    (magit-gh--prune-save-cache table)
    table))

(defun magit-gh--prune-state-buffer ()
  "Return the hidden state buffer for the current repo, creating it if absent.
Errors when not inside a git repository."
  (let* ((dir (or (magit-toplevel)
		  (user-error "Not inside a git repository")))
	 (name (format " *magit-gh-prune:%s*" (directory-file-name dir)))
	 (buf (get-buffer name)))
    (or buf
	(with-current-buffer (get-buffer-create name)
	  (setq default-directory dir)
	  (current-buffer)))))

(defun magit-gh--prune-scan ()
  "Re-scan all local branches against the closed/merged PR cache.
Merges fresh GitHub data into the in-memory table stored in `:closed-prs'
(seeded from disk on first call), persists the result, and rebuilds
`:candidates'. Excludes the default branch and the currently checked-out
branch, catching squash-merged branches whose remote tracking ref is gone.
Stale marked branches are dropped from `:marked'. Returns new candidates."
  (let* ((default-directory (magit-gh--repo-dir))
	 (prev-marked (plist-get magit-gh--prune-state :marked))
	 (prev-prs (plist-get magit-gh--prune-state :closed-prs))
	 (protected (list (magit-gh--default-branch)
			  (magit-get-current-branch)))
	 (closed-prs (magit-gh--fetch-closed-prs prev-prs))
	 (candidates nil))
    (dolist (branch (magit-list-local-branch-names))
      (unless (member branch protected)
	(let ((pr (map-elt closed-prs branch)))
	  (when pr
	    (push (cons branch pr) candidates)))))
    (setq candidates (nreverse candidates))
    (setq magit-gh--prune-state
	  (list :candidates candidates
		:marked (cl-intersection prev-marked
					 (mapcar #'car candidates)
					 :test #'equal)
		:closed-prs closed-prs))
    candidates))

(defun magit-gh--prune-format-annotation (pr)
  "Return a one-line annotation string describing PR alist."
  (format "PR #%s %s: %s"
	  (map-elt pr 'number)
	  (map-elt pr 'state)
	  (map-elt pr 'title)))

(defun magit-gh--prune-build-menu (candidates marked)
  "Build a hash-table menu for the prune command.
CANDIDATES is the alist of (BRANCH . PR-ALIST). MARKED is the list of
branch names currently marked for batch pruning. Branch entries are
labeled `prune: BRANCH' with a ` [marked]' suffix when applicable."
  (let ((table (make-hash-table :test #'equal)))
    (map-put! table "exit menu" "leave the menu without further action")
    (map-put! table "refresh" "re-scan PRs and rebuild the cache (returns to menu)")
    (when candidates
      (map-put! table "prune all branches (no prompt)"
	       (format "delete all %d candidate branch(es)" (length candidates)))
      (map-put! table "prune all branches (with prompt)"
	       (format "delete %d candidate(s), confirming each (y/n/q/!)"
		       (length candidates)))
      (map-put! table "mark branch for pruning"
	       "toggle the mark on a branch for batch pruning"))
    (when marked
      (map-put! table "prune marked branches"
	       (format "delete %d marked branch(es)" (length marked))))
    (dolist (entry candidates)
      (let* ((branch (car entry))
	     (pr (cdr entry))
	     (suffix (if (member branch marked) " [marked]" "")))
	(map-put! table (format "prune: %s%s" branch suffix)
		 (magit-gh--prune-format-annotation pr))))
    table))

(defun magit-gh--prune-parse-branch-label (label)
  "Extract BRANCH from a menu LABEL like `prune: BRANCH [marked]'.
Returns nil when LABEL is not a branch entry."
  (when (string-match "\\`prune: \\(.+?\\)\\(?: \\[marked\\]\\)?\\'" label)
    (match-string 1 label)))

(defun magit-gh--prune-delete-branches (branches buf &optional prompt-p)
  "Delete each branch in BRANCHES, prompting only when PROMPT-P.
After deletion, re-scan the cache in BUF.
Returns a plist (:deleted N :skipped M :quit BOOL).
With PROMPT-P, the user is prompted y/n/q/! per branch; `!' enables
yes-to-all for the remainder, `q' terminates the loop."
  (let ((deleted 0)
	(skipped 0)
	(yes-to-all (not prompt-p))
	(quit nil))
    (catch 'done
      (dolist (branch branches)
	(let ((answer (if yes-to-all ?y
			(read-char-choice
			 (format "Delete %s? (y/n/q/!) " branch)
			 '(?y ?n ?q ?!)))))
	  (pcase answer
	    (?y (magit-branch-delete (list branch) t) (cl-incf deleted))
	    (?n (cl-incf skipped))
	    (?q (setq quit t) (throw 'done nil))
	    (?! (setq yes-to-all t)
		(magit-branch-delete (list branch) t)
		(cl-incf deleted))))))
    (with-current-buffer buf (magit-gh--prune-scan))
    (message "magit-gh prune: deleted %d, skipped %d%s"
	     deleted skipped (if quit " (quit)" ""))
    (list :deleted deleted :skipped skipped :quit quit)))

(defun magit-gh--prune-toggle-mark (buf)
  "Prompt for a branch from the cache in BUF and toggle its mark.
Updates `:marked' in the buffer-local state of BUF."
  (let* ((state (buffer-local-value 'magit-gh--prune-state buf))
	 (candidates (plist-get state :candidates))
	 (marked (plist-get state :marked))
	 (table (make-hash-table :test #'equal)))
    (unless candidates
      (user-error "No candidate branches to mark"))
    (dolist (entry candidates)
      (let* ((branch (car entry))
	     (pr (cdr entry))
	     (label (format "%s%s" branch
			    (if (member branch marked) " [marked]" ""))))
	(map-put! table label (magit-gh--prune-format-annotation pr))))
    (let* ((label (annotated-completing-read table
					     :prompt "toggle mark => "
					     :category 'magit-gh-mark
					     :require-match t))
	   (branch (replace-regexp-in-string " \\[marked\\]\\'" "" label))
	   (new-marked (if (member branch marked)
			   (delete branch (copy-sequence marked))
			 (cons branch marked))))
      (with-current-buffer buf
	(setq magit-gh--prune-state
	      (plist-put magit-gh--prune-state :marked new-marked))))))

(defun magit-gh--prune-dispatch (label buf)
  "Dispatch the menu action for LABEL given state buffer BUF.
Throws `magit-gh--prune-exit' to terminate the menu loop."
  (let* ((state (buffer-local-value 'magit-gh--prune-state buf))
	 (candidates (plist-get state :candidates))
	 (marked (plist-get state :marked)))
    (cond
     ((equal label "exit menu")
      (throw 'magit-gh--prune-exit nil))
     ((equal label "refresh")
      (with-current-buffer buf (magit-gh--prune-scan)))
     ((equal label "prune all branches (no prompt)")
      (magit-gh--prune-delete-branches (mapcar #'car candidates) buf nil))
     ((equal label "prune all branches (with prompt)")
      (magit-gh--prune-delete-branches (mapcar #'car candidates) buf t))
     ((equal label "mark branch for pruning")
      (magit-gh--prune-toggle-mark buf))
     ((equal label "prune marked branches")
      (magit-gh--prune-delete-branches marked buf nil))
     ((magit-gh--prune-parse-branch-label label)
      (magit-gh--prune-delete-branches
       (list (magit-gh--prune-parse-branch-label label)) buf nil))
     (t
      (user-error "Unknown menu label: %s" label)))))

;;;###autoload
(defun magit-gh-prune-merged-branches ()
  "Open a menu to prune local branches whose PRs are merged or closed.

Per-repo cache (candidates + marked list) lives on a hidden state buffer
keyed by repository toplevel. The menu loops until you select `exit
menu'. The menu offers:

  - exit menu
  - refresh
  - prune all branches (no prompt)
  - prune all branches (with prompt)
  - mark branch for pruning
  - prune marked branches (when any are marked)
  - prune: BRANCH [marked]   (one entry per candidate)"
  (interactive)
  (magit-gh--check-gh)
  (let ((buf (magit-gh--prune-state-buffer)))
    (with-current-buffer buf
      (unless magit-gh--prune-state
	(message "magit-gh prune: scanning for branches with closed PRs...")
	(magit-gh--prune-scan)))
    (catch 'magit-gh--prune-exit
      (while t
	(let* ((state (buffer-local-value 'magit-gh--prune-state buf))
	       (table (magit-gh--prune-build-menu
		       (plist-get state :candidates)
		       (plist-get state :marked)))
	       (label (annotated-completing-read
		       table
		       :prompt "magit-gh prune => "
		       :category 'magit-gh-prune
		       :require-match t)))
	  (magit-gh--prune-dispatch label buf))))))

;;;###autoload
(defun magit-gh-prune-prefetch ()
  "Seed the in-memory closed-PR cache from disk for the current repo.
Makes no GitHub API calls; safe to call on status load.
Skips silently when caching is disabled, not in a git repo, or the
in-memory cache is already populated."
  (when (and magit-gh-prune-cache-dir
	     (ignore-errors (magit-toplevel)))
    (let ((buf (magit-gh--prune-state-buffer)))
      (unless (plist-get (buffer-local-value 'magit-gh--prune-state buf) :closed-prs)
	(with-current-buffer buf
	  (let ((table (magit-gh--prune-load-cache)))
	    (setq magit-gh--prune-state
		  (plist-put magit-gh--prune-state :closed-prs table))))))))

(provide 'magit-gh-extras)

;;; magit-gh-extras.el ends here
