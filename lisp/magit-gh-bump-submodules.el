;;; magit-gh-bump-submodules.el --- Recursive submodule bump for magit-gh -*- lexical-binding: t -*-

;; Author: tycho garen
;; Keywords: vc, tools, magit, git
;; URL: https://github.com/tychoish/dot-emacs

;; This file is not part of GNU Emacs

;;; Commentary:
;; Provides `magit-gh-bump-submodules-menu' for committing updated submodule
;; pointers as "bump: <names>" commits and pushing repos children-first.
;; Only commits in repos where all working-tree changes are submodule pointer
;; updates; repos with other modifications are skipped with a message.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'transient)
(require 'magit)
(require 'magit-gh-extras)

(declare-function magit-gh-repo-dashboard--run-git "magit-gh-repo-dashboard")
(declare-function magit-gh-repo-dashboard--repo-at-point "magit-gh-repo-dashboard")
(declare-function magit-gh-repo-dashboard--maybe-refresh "magit-gh-repo-dashboard")
(declare-function magit-gh-repo-path "magit-gh-repo-dashboard")

(defvar magit-gh-repo-list)

;;;; Parsing utilities (pure, no I/O)

(defun magit-gh-bump--parse-dirty-submodule-lines (lines)
  "Parse LINES from `git submodule status' and return dirty pairs.
Returns an alist of (rel-path . name) for each line whose first character is
`+', meaning the submodule HEAD differs from the hash the parent recorded."
  (thread-last lines
    (seq-filter (lambda (line)
                  (and (> (length line) 0) (eq ?+ (aref line 0)))))
    (seq-map (lambda (line)
               (when (string-match "^[+][0-9a-f]+ \\([^ ]+\\)" line)
                 (let ((rel-path (match-string 1 line)))
                   (cons rel-path
                         (file-name-nondirectory
                          (directory-file-name rel-path)))))))
    (seq-remove #'null)))

(defun magit-gh-bump--parse-all-submodule-relpaths (lines)
  "Parse LINES from `git submodule status' and return all relative paths.
Returns relative paths for every initialized, dirty, or conflicted submodule."
  (thread-last lines
    (seq-filter (lambda (line) (> (length line) 0)))
    (seq-map (lambda (line)
               (when (string-match "^[+ U-][0-9a-f]+ \\([^ ]+\\)" line)
                 (match-string 1 line))))
    (seq-remove #'null)))

(defun magit-gh-bump--parse-porcelain-tracked (lines)
  "Parse LINES from `git status --porcelain' and return tracked changed paths.
Excludes untracked files (lines starting with `??')."
  (thread-last lines
    (seq-filter (lambda (line)
                  (and (>= (length line) 4)
                       (not (string-prefix-p "??" line)))))
    (seq-map (lambda (line) (string-trim (substring line 3))))))

(defun magit-gh-bump--all-changes-are-submodules-p (tracked-paths dirty-relpaths)
  "Return t when every path in TRACKED-PATHS is present in DIRTY-RELPATHS.
Returns t trivially when TRACKED-PATHS is empty."
  (let ((allowed (make-hash-table :test #'equal)))
    (seq-do (lambda (rp) (puthash rp t allowed)) dirty-relpaths)
    (seq-every-p (lambda (cp) (gethash cp allowed)) tracked-paths)))

;;;; Synchronous git I/O (discovery phase only)

(defun magit-gh-bump--git-lines (path &rest args)
  "Run git ARGS synchronously in PATH; return a list of output lines or nil."
  (ignore-errors
    (let ((default-directory path))
      (apply #'magit-git-lines args))))

(defun magit-gh-bump--repo-root-p (path)
  "Return t when PATH is the root of a git repository."
  (file-exists-p (expand-file-name ".git" path)))

(defun magit-gh-bump--dirty-submodule-pairs (path)
  "Return alist of (rel-path . name) for submodules with new commits in PATH.
Calls `git submodule status' and returns only entries prefixed with `+'."
  (magit-gh-bump--parse-dirty-submodule-lines
   (magit-gh-bump--git-lines path "submodule" "status")))

(defun magit-gh-bump--all-submodule-relpaths (path)
  "Return all initialized submodule relative paths in repo at PATH."
  (magit-gh-bump--parse-all-submodule-relpaths
   (magit-gh-bump--git-lines path "submodule" "status")))

(defun magit-gh-bump--only-submodule-changes-p (path dirty-relpaths)
  "Return t when the only tracked working-tree changes in PATH are DIRTY-RELPATHS."
  (magit-gh-bump--all-changes-are-submodules-p
   (magit-gh-bump--parse-porcelain-tracked
    (magit-gh-bump--git-lines path "status" "--porcelain"))
   dirty-relpaths))

;;;; Tree discovery

(defun magit-gh-bump--parent-repo (path)
  "Return the absolute path of the git repo that contains PATH as a submodule.
Returns nil when PATH is not recognized as a submodule one directory level up.
The returned path never has a trailing slash."
  (let* ((normalized (directory-file-name (expand-file-name path)))
         (parent-dir (file-name-directory normalized)))
    (when (and parent-dir
               (not (equal parent-dir normalized))
               (magit-gh-bump--repo-root-p parent-dir))
      (let ((rel (file-relative-name normalized parent-dir)))
        (when (seq-some (lambda (sp) (equal rel sp))
                        (magit-gh-bump--all-submodule-relpaths parent-dir))
          (directory-file-name parent-dir))))))

(defun magit-gh-bump--topmost-parent (path)
  "Return the topmost git repo that contains PATH as a (possibly nested) submodule.
Returns PATH itself when it is not a submodule of any parent."
  (let ((current (expand-file-name path))
        (top (expand-file-name path)))
    (while (when-let* ((p (magit-gh-bump--parent-repo current)))
             (setq top p current p)))
    top))

(defun magit-gh-bump--walk-tree (root)
  "Return all repo paths in the submodule tree rooted at ROOT.
ROOT itself is listed first; submodules follow depth-first.  Only directories
that have a `.git' entry are included."
  (let ((result (list root)))
    (seq-do
     (lambda (rel-path)
       (let ((abs-path (expand-file-name rel-path root)))
         (when (and (file-directory-p abs-path)
                    (magit-gh-bump--repo-root-p abs-path))
           (setq result (append result (magit-gh-bump--walk-tree abs-path))))))
     (magit-gh-bump--all-submodule-relpaths root))
    result))

;;;; Utility

(defun magit-gh-bump--path-depth (path)
  "Return the number of directory components in PATH."
  (length (seq-remove #'string-empty-p
                      (split-string (expand-file-name path) "/"))))

(defun magit-gh-bump--make-bump-message (dirty-pairs)
  "Return a \"bump: name1, name2\" commit message for DIRTY-PAIRS.
DIRTY-PAIRS is an alist of (rel-path . name)."
  (format "bump: %s" (mapconcat #'cdr dirty-pairs ", ")))

(defun magit-gh-bump--args-flag (args flag)
  "Return t when FLAG is present in the transient ARGS list."
  (and (member flag args) t))

;;;; Parallel fetch / pull

(defun magit-gh-bump--fetch-pull-parallel (paths do-fetch do-pull on-all-done)
  "Run fetch or pull on all PATHS in parallel; call ON-ALL-DONE with results.
DO-PULL takes precedence over DO-FETCH when both are non-nil.
Results is an alist of (path . status) passed to ON-ALL-DONE."
  (if (not (or do-fetch do-pull))
      (funcall on-all-done nil)
    (let* ((total (length paths))
           (remaining (list total))
           (results nil))
      (if (= total 0)
          (funcall on-all-done nil)
        (seq-do
         (lambda (path)
           (let ((name (file-name-nondirectory (directory-file-name path)))
                 (finish (lambda (status &optional err)
                           (when (eq status 'error)
                             (message "magit-gh-bump: fetch/pull error in %s: %s"
                                      (file-name-nondirectory (directory-file-name path)) err))
                           (push (cons path status) results)
                           (setcar remaining (1- (car remaining)))
                           (when (= 0 (car remaining))
                             (funcall on-all-done results)))))
             (if do-pull
                 (progn
                   (message "magit-gh-bump: pulling %s..." name)
                   (magit-gh-repo-dashboard--run-git
                    path '("pull" "--rebase")
                    (lambda (_) (funcall finish 'ok))
                    (lambda (out code)
                      (funcall finish 'error (format "exit %d: %s" code out)))))
               (message "magit-gh-bump: fetching %s..." name)
               (magit-gh-repo-dashboard--run-git
                path '("fetch" "--all")
                (lambda (_) (funcall finish 'ok))
                (lambda (out code)
                  (funcall finish 'error (format "exit %d: %s" code out)))))))
         paths)))))

;;;; Per-repo commit pipeline

(defun magit-gh-bump--process-one-repo (path push dry-run on-complete)
  "Check PATH for dirty submodules and commit a bump if appropriate.
Validates that all working-tree changes are submodule pointer updates before
staging.  PUSH causes a `git push' after the commit.  DRY-RUN logs the planned
commit without making changes.  Calls ON-COMPLETE with `ok', `skipped', or
`error'."
  (let* ((name (file-name-nondirectory (directory-file-name path)))
         (dirty (magit-gh-bump--dirty-submodule-pairs path)))
    (cond
     ((null dirty)
      (message "magit-gh-bump: %s — no dirty submodules, skipping" name)
      (funcall on-complete 'skipped))
     ((not (magit-gh-bump--only-submodule-changes-p path (seq-map #'car dirty)))
      (message "magit-gh-bump: %s — non-submodule changes present, skipping" name)
      (funcall on-complete 'skipped))
     (dry-run
      (message "magit-gh-bump: [dry-run] %s — would commit: %s"
               name (magit-gh-bump--make-bump-message dirty))
      (funcall on-complete 'ok))
     (t
      (let ((msg (magit-gh-bump--make-bump-message dirty))
            (rel-paths (seq-map #'car dirty)))
        (message "magit-gh-bump: %s — staging %s"
                 name (string-join rel-paths " "))
        (magit-gh-repo-dashboard--run-git
         path (cons "add" rel-paths)
         (lambda (_)
           (message "magit-gh-bump: %s — committing: %s" name msg)
           (magit-gh-repo-dashboard--run-git
            path (list "commit" "-m" msg)
            (lambda (_)
              (message "magit-gh-bump: %s — committed" name)
              (if (not push)
                  (funcall on-complete 'ok)
                (message "magit-gh-bump: %s — pushing..." name)
                (magit-gh-repo-dashboard--run-git
                 path '("push")
                 (lambda (_)
                   (message "magit-gh-bump: %s — pushed" name)
                   (funcall on-complete 'ok))
                 (lambda (out code)
                   (message "magit-gh-bump: %s — push failed (exit %d): %s"
                            name code out)
                   (funcall on-complete 'error
                            (format "push failed (exit %d): %s" code out))))))
            (lambda (out code)
              (message "magit-gh-bump: %s — commit failed (exit %d): %s" name code out)
              (funcall on-complete 'error
                       (format "commit failed (exit %d): %s" code out)))))
         (lambda (out code)
           (message "magit-gh-bump: %s — add failed (exit %d): %s" name code out)
           (funcall on-complete 'error
                    (format "add failed (exit %d): %s" code out)))))))))

;;;; Sequential processing loop

(defun magit-gh-bump--process-remaining (remaining results push dry-run on-done)
  "Process the next path in REMAINING and recurse until the list is exhausted.
RESULTS accumulates (name . status) pairs.  Calls ON-DONE with the final alist."
  (if (null remaining)
      (let ((ok (seq-count (lambda (r) (eq 'ok (cdr r))) results))
            (skipped (seq-count (lambda (r) (eq 'skipped (cdr r))) results))
            (errors (seq-filter (lambda (r) (eq 'error (cdr r))) results)))
        (message "magit-gh-bump: complete — %d committed, %d skipped%s"
                 ok skipped
                 (if errors
                     (format ", %d errors (%s)"
                             (length errors)
                             (mapconcat #'car errors ", "))
                   ""))
        (when on-done (funcall on-done results)))
    (let* ((path (car remaining))
           (name (file-name-nondirectory (directory-file-name path))))
      (magit-gh-bump--process-one-repo
       path push dry-run
       (lambda (status &optional _err)
         (magit-gh-bump--process-remaining
          (cdr remaining)
          (cons (cons name status) results)
          push dry-run on-done))))))

;;;; Main entry point

(cl-defun magit-gh-bump-submodules-run (path &key fetch pull push dry-run recursive on-done)
  "Bump submodule pointers in the repository tree containing PATH.

Scans for submodules whose HEAD differs from the hash recorded by their parent
(shown as `+' in `git submodule status') and creates a single \"bump: <names>\"
commit per parent repo.  Processing proceeds deepest-first so that after a
child repo is committed, its parent picks up the new pointer on the next check.
Only repos where ALL working-tree changes are submodule pointer updates are
committed; repos with other modifications are skipped with a log message.

Keyword arguments:
  :fetch     Run `git fetch --all' in each repo before committing.
  :pull      Run `git pull --rebase' in each repo before committing.
  :push      Push each repo after committing; children are pushed before parents.
  :dry-run   Log what would be committed without making any changes.
  :recursive Walk up to the topmost parent and process the full submodule tree.
  :on-done   Called with an alist of (name . status) when everything finishes."
  (let* ((root (if recursive (magit-gh-bump--topmost-parent path) path))
         (root-name (file-name-nondirectory (directory-file-name root)))
         (all-paths (magit-gh-bump--walk-tree root))
         (sorted-paths (seq-sort
                        (lambda (a b)
                          (> (magit-gh-bump--path-depth a)
                             (magit-gh-bump--path-depth b)))
                        all-paths)))
    (message "magit-gh-bump: scanning %d repo(s) from %s%s"
             (length sorted-paths) root-name
             (if dry-run " [dry-run]" ""))
    (magit-gh-bump--fetch-pull-parallel
     sorted-paths fetch pull
     (lambda (_)
       (magit-gh-bump--process-remaining
        sorted-paths nil push dry-run
        (lambda (results)
          (magit-gh-repo-dashboard--maybe-refresh)
          (when on-done (funcall on-done results))))))))

;;;; Interactive commands

(defun magit-gh-bump-submodules-at-point ()
  "Bump submodule pointers for the repository at point in the dashboard."
  (interactive)
  (let* ((args (transient-args 'magit-gh-bump-submodules-menu))
         (path (magit-gh-repo-path (magit-gh-repo-dashboard--repo-at-point))))
    (magit-gh-bump-submodules-run
     path
     :fetch (magit-gh-bump--args-flag args "--fetch")
     :pull (magit-gh-bump--args-flag args "--pull")
     :push (magit-gh-bump--args-flag args "--push")
     :dry-run (magit-gh-bump--args-flag args "--dry-run")
     :recursive (magit-gh-bump--args-flag args "--recursive"))))

(defun magit-gh-bump-submodules-all ()
  "Bump submodule pointers across all registered dashboard repositories."
  (interactive)
  (let* ((args (transient-args 'magit-gh-bump-submodules-menu))
         (fetch (magit-gh-bump--args-flag args "--fetch"))
         (pull (magit-gh-bump--args-flag args "--pull"))
         (push (magit-gh-bump--args-flag args "--push"))
         (dry-run (magit-gh-bump--args-flag args "--dry-run"))
         (recursive (magit-gh-bump--args-flag args "--recursive"))
         (roots (delete-dups
                 (seq-map
                  (lambda (repo)
                    (let ((rpath (magit-gh-repo-path repo)))
                      (if recursive (magit-gh-bump--topmost-parent rpath) rpath)))
                  magit-gh-repo-list))))
    (unless roots
      (user-error "No repositories registered in magit-gh-repo-list"))
    (let* ((all-paths (delete-dups (seq-mapcat #'magit-gh-bump--walk-tree roots)))
           (sorted-paths (seq-sort
                          (lambda (a b)
                            (> (magit-gh-bump--path-depth a)
                               (magit-gh-bump--path-depth b)))
                          all-paths)))
      (message "magit-gh-bump: scanning %d repo(s) across all registered roots%s"
               (length sorted-paths) (if dry-run " [dry-run]" ""))
      (magit-gh-bump--fetch-pull-parallel
       sorted-paths fetch pull
       (lambda (_)
         (magit-gh-bump--process-remaining
          sorted-paths nil push dry-run
          (lambda (_) (magit-gh-repo-dashboard--maybe-refresh))))))))

;;;; Transient menu

(transient-define-prefix magit-gh-bump-submodules-menu ()
  "Bump submodule pointers across the repository tree.
Commits each parent repo that has submodules with new commits, using a single
\"bump: <names>\" commit per parent.  Processing order is deepest-first so
children are committed and pushed before their parents record the new hashes."
  :value '("--push")
  [["Options"
    ("-f" "Fetch remotes before committing" "--fetch")
    ("-p" "Pull (--rebase) before committing" "--pull")
    ("-P" "Push after committing (default on)" "--push")
    ("-n" "Dry run: log without making changes" "--dry-run")
    ("-r" "Recursive: walk up to topmost parent" "--recursive")]
   ["Actions"
    ("b" "Bump repo at point" magit-gh-bump-submodules-at-point
     :inapt-if-not (lambda ()
                     (and (derived-mode-p 'magit-gh-repo-dashboard-mode)
                          (tabulated-list-get-id))))
    ("B" "Bump all registered repos" magit-gh-bump-submodules-all)]])

(provide 'magit-gh-bump-submodules)
;;; magit-gh-bump-submodules.el ends here
