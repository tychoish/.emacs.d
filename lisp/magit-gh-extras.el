;;; magit-gh-extras.el --- Branch pruning extension for magit-gh -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Version: 0.1
;; Package-Requires: ((emacs "29.1") (magit "4.0") (ht "2.3") (annotated-completing-read "0.1"))
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
(require 'ht)
(require 'annotated-completing-read)

(declare-function magit-toplevel "magit-git")
(declare-function magit-list-local-branch-names "magit-git")
(declare-function magit-get-upstream-branch "magit-git")
(declare-function magit-branch-delete "magit-branch")
(declare-function magit-gh--repo-dir "magit-gh")
(declare-function magit-gh--check-gh "magit-gh")

(defvar-local magit-gh--prune-state nil
  "Buffer-local prune state for the current repo.
A plist with `:candidates' (alist of (BRANCH . PR-ALIST)) and
`:marked' (list of branch names selected for batch pruning).
Lives on a hidden state buffer per repository.")

(defun magit-gh--tracking-branches ()
  "Return a list of local branches that have an upstream tracking branch."
  (let (result)
    (dolist (branch (magit-list-local-branch-names))
      (when (magit-get-upstream-branch branch)
	(push branch result)))
    (nreverse result)))

(defun magit-gh--pr-for-branch (branch)
  "Return the PR alist for BRANCH from `gh pr list --state all', else nil.
Matches against `headRefName' as exposed by `gh pr list'."
  (let* ((default-directory (magit-gh--repo-dir))
	 (cmd (format "gh pr list --head %s --state all --json number,state,headRefName,title --limit 1"
		      (shell-quote-argument branch)))
	 (output (string-trim (shell-command-to-string cmd))))
    (when (string-prefix-p "[" output)
      (car (json-parse-string output :array-type 'list :object-type 'alist)))))

(defun magit-gh--pr-closed-p (pr)
  "Return non-nil when PR alist represents a merged or closed PR."
  (member (alist-get 'state pr) '("MERGED" "CLOSED")))

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
  "Re-scan tracking branches against PR state in the current buffer's repo.
Updates buffer-local `magit-gh--prune-state'. Stale marked branches
(branches no longer in the candidate set) are dropped from `:marked'.
Returns the new candidates alist."
  (let* ((default-directory (magit-gh--repo-dir))
	 (prev-marked (plist-get magit-gh--prune-state :marked))
	 (candidates nil))
    (dolist (branch (magit-gh--tracking-branches))
      (let ((pr (magit-gh--pr-for-branch branch)))
	(when (and pr (magit-gh--pr-closed-p pr))
	  (push (cons branch pr) candidates))))
    (setq candidates (nreverse candidates))
    (setq magit-gh--prune-state
	  (list :candidates candidates
		:marked (cl-intersection prev-marked
					 (mapcar #'car candidates)
					 :test #'equal)))
    candidates))

(defun magit-gh--prune-format-annotation (pr)
  "Return a one-line annotation string describing PR alist."
  (format "PR #%s %s: %s"
	  (alist-get 'number pr)
	  (alist-get 'state pr)
	  (alist-get 'title pr)))

(defun magit-gh--prune-build-menu (candidates marked)
  "Build a hash-table menu for the prune command.
CANDIDATES is the alist of (BRANCH . PR-ALIST). MARKED is the list of
branch names currently marked for batch pruning. Branch entries are
labeled `prune: BRANCH' with a ` [marked]' suffix when applicable."
  (let ((table (ht-create)))
    (ht-set table "exit menu" "leave the menu without further action")
    (ht-set table "refresh" "re-scan PRs and rebuild the cache (returns to menu)")
    (when candidates
      (ht-set table "prune all branches (no prompt)"
	      (format "delete all %d candidate branch(es)" (length candidates)))
      (ht-set table "prune all branches (with prompt)"
	      (format "delete %d candidate(s), confirming each (y/n/q/!)"
		      (length candidates)))
      (ht-set table "mark branch for pruning"
	      "toggle the mark on a branch for batch pruning"))
    (when marked
      (ht-set table "prune marked branches"
	      (format "delete %d marked branch(es)" (length marked))))
    (dolist (entry candidates)
      (let* ((branch (car entry))
	     (pr (cdr entry))
	     (suffix (if (member branch marked) " [marked]" "")))
	(ht-set table (format "prune: %s%s" branch suffix)
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
	    (?y (magit-branch-delete (list branch)) (cl-incf deleted))
	    (?n (cl-incf skipped))
	    (?q (setq quit t) (throw 'done nil))
	    (?! (setq yes-to-all t)
		(magit-branch-delete (list branch))
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
	 (table (ht-create)))
    (unless candidates
      (user-error "No candidate branches to mark"))
    (dolist (entry candidates)
      (let* ((branch (car entry))
	     (pr (cdr entry))
	     (label (format "%s%s" branch
			    (if (member branch marked) " [marked]" ""))))
	(ht-set table label (magit-gh--prune-format-annotation pr))))
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

(provide 'magit-gh-extras)

;;; magit-gh-extras.el ends here
