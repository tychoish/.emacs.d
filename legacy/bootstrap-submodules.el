(declare-function magit-list-module-paths "magit-submodule")
(declare-function magit-run-git "magit-process")

(defun bootstrap--git-repo-p (dir)
  "Return non-nil when DIR is the top of a git working tree.
Both worktree roots and submodule directories qualify: in either case
DIR contains a `.git' entry (a directory or a gitlink file)."
  (file-exists-p (expand-file-name ".git" dir)))

(defun bootstrap--gitmodules-paths (root)
  "Return submodule paths declared in ROOT/.gitmodules.
Parses the file directly so this works without magit or git."
  (let ((path (expand-file-name ".gitmodules" root))
        out)
    (when (file-readable-p path)
      (with-temp-buffer
        (insert-file-contents path)
        (goto-char (point-min))
        (while (re-search-forward "^[ \t]*path[ \t]*=[ \t]*\\(.+?\\)[ \t]*$" nil t)
          (push (match-string 1) out))))
    (nreverse out)))

(defun bootstrap--submodule-checked-out-p (root sub)
  "Return non-nil when submodule SUB under ROOT is checked out."
  (bootstrap--git-repo-p (expand-file-name sub root)))

(defun bootstrap--emacs-conf-uninstalled-submodules ()
  "Return submodule paths in `user-emacs-directory' that are registered but not checked out."
  (let ((root (expand-file-name user-emacs-directory)))
    (seq-remove (lambda (sub) (bootstrap--submodule-checked-out-p root sub))
                (bootstrap--gitmodules-paths root))))

(defun bootstrap--emacs-conf-check-submodules ()
  "Warn if any `user-emacs-directory' submodules are registered but not checked out.

Missing submodules referenced via `:load-path' in `use-package' forms
otherwise fail silently when their autoloaded hooks fire, e.g. aborting
the rest of a mode-hook chain.

No-ops when `.emacs.d' is not itself a git working tree, which suggests
a non-git or partial bootstrap installation."
  (interactive)
  (let ((root (expand-file-name user-emacs-directory)))
    (when (bootstrap--git-repo-p root)
      (when-let* ((missing (bootstrap--emacs-conf-uninstalled-submodules)))
        (message "uninstalled submodules in %s: %s — run: (cd %s && git submodule update --init %s)"
                 root
                 (mapconcat #'identity missing " ")
                 root
                 (mapconcat #'identity missing " "))
        missing))))

(defun bootstrap--emacs-conf-pull-submodules ()
  "Run `git pull origin' in each submodule of `user-emacs-directory'.
Submodules are enumerated via `magit-list-module-paths'.  For each one,
prompts y/n/a (yes/no/abort).  Pulls run synchronously via
`magit-run-git'; per-pull output lands in the magit process buffer."
  (interactive)
  (require 'magit-submodule)
  (require 'magit-process)
  (let* ((root (file-name-as-directory (expand-file-name user-emacs-directory)))
         (default-directory root)
         (modules (magit-list-module-paths)))
    (unless modules
      (user-error "no submodules registered under %s" root))
    (catch 'abort
      (dolist (sub modules)
        (pcase (car (read-multiple-choice
                     (format "pull %s? " sub)
                     '((?y "yes"   "git pull origin in this submodule")
                       (?n "no"    "skip this submodule")
                       (?a "abort" "stop iterating"))))
          (?a (message "submodule pull aborted")
              (throw 'abort nil))
          (?n (message "skip %s" sub))
          (?y (let ((default-directory
                     (file-name-as-directory (expand-file-name sub root))))
                (message "pulling %s..." sub)
                (magit-run-git "pull" "origin"))))))
    (message "submodule pull complete")))
