;;; test-magit-open-repo.el --- ERT tests for magit-open-repo.el -*- lexical-binding: t -*-

;;; Commentary:
;; Run inside a live Emacs session with full config loaded:
;;   M-x ert RET t RET
;; or filtered:
;;   (ert "^magit-open-repo/")

(require 'ert)
(require 'cl-lib)
(require 'magit-open-repo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit-open-repo--git-p

(ert-deftest magit-open-repo/git-p-detects-git-dir ()
  "Returns non-nil for a directory containing a .git entry."
  (let ((dir (make-temp-file "ert-magit-git-p-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" dir))
          (should (magit-open-repo--git-p dir)))
      (delete-directory dir t))))

(ert-deftest magit-open-repo/git-p-rejects-plain-dir ()
  "Returns nil for a directory without a .git entry."
  (let ((dir (make-temp-file "ert-magit-git-p-plain-" t)))
    (unwind-protect
        (should-not (magit-open-repo--git-p dir))
      (delete-directory dir t))))

(ert-deftest magit-open-repo/git-p-git-file-counts ()
  "Returns non-nil when .git is a file (worktree) rather than a directory."
  (let ((dir (make-temp-file "ert-magit-git-p-file-" t)))
    (unwind-protect
        (progn
          (write-region "gitdir: /some/path" nil (expand-file-name ".git" dir))
          (should (magit-open-repo--git-p dir)))
      (delete-directory dir t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit-open-repo--subdirs

(ert-deftest magit-open-repo/subdirs-returns-directories ()
  "Returns only directories, not plain files."
  (let ((dir (make-temp-file "ert-magit-subdirs-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "subdir" dir))
          (write-region "" nil (expand-file-name "file.txt" dir))
          (let ((result (magit-open-repo--subdirs dir)))
            (should (= 1 (length result)))
            (should (string-suffix-p "subdir" (car result)))))
      (delete-directory dir t))))

(ert-deftest magit-open-repo/subdirs-excludes-hidden ()
  "Hidden directories (starting with .) are excluded."
  (let ((dir (make-temp-file "ert-magit-subdirs-hidden-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".hidden" dir))
          (make-directory (expand-file-name "visible" dir))
          (let ((result (magit-open-repo--subdirs dir)))
            (should (= 1 (length result)))
            (should (string-suffix-p "visible" (car result)))))
      (delete-directory dir t))))

(ert-deftest magit-open-repo/subdirs-empty-dir ()
  "Returns nil for an empty directory."
  (let ((dir (make-temp-file "ert-magit-subdirs-empty-" t)))
    (unwind-protect
        (should-not (magit-open-repo--subdirs dir))
      (delete-directory dir t))))

(ert-deftest magit-open-repo/subdirs-returns-absolute-paths ()
  "All returned paths are absolute."
  (let ((dir (make-temp-file "ert-magit-subdirs-abs-" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "child" dir))
          (should (seq-every-p #'file-name-absolute-p (magit-open-repo--subdirs dir))))
      (delete-directory dir t))))

(ert-deftest magit-open-repo/subdirs-inaccessible-returns-nil ()
  "Returns nil for a path that is not a directory."
  (should-not (magit-open-repo--subdirs "/no/such/path/xyz")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit-open-repo--find-open-buffer

(ert-deftest magit-open-repo/find-open-buffer-matches-exact ()
  "Returns the buffer when path matches a key in open-buffers."
  (let* ((dir (make-temp-file "ert-magit-find-buf-" t))
         (norm (file-name-as-directory (expand-file-name dir)))
         (buf (get-buffer-create " *ert-magit-find-buf*"))
         (open-buffers (list (cons norm buf))))
    (unwind-protect
        (should (eq buf (magit-open-repo--find-open-buffer dir open-buffers)))
      (kill-buffer buf)
      (delete-directory dir t))))

(ert-deftest magit-open-repo/find-open-buffer-trailing-slash-insensitive ()
  "Matches regardless of whether path has a trailing slash."
  (let* ((dir (make-temp-file "ert-magit-find-slash-" t))
         (norm (file-name-as-directory (expand-file-name dir)))
         (buf (get-buffer-create " *ert-magit-find-slash*"))
         (open-buffers (list (cons norm buf))))
    (unwind-protect
        (progn
          (should (eq buf (magit-open-repo--find-open-buffer dir open-buffers)))
          (should (eq buf (magit-open-repo--find-open-buffer (directory-file-name dir) open-buffers))))
      (kill-buffer buf)
      (delete-directory dir t))))

(ert-deftest magit-open-repo/find-open-buffer-no-match ()
  "Returns nil when no buffer matches the path."
  (let* ((dir (make-temp-file "ert-magit-find-none-" t))
         (other "/tmp/completely-different-path/"))
    (unwind-protect
        (should-not (magit-open-repo--find-open-buffer dir (list (cons other nil))))
      (delete-directory dir t))))

(ert-deftest magit-open-repo/find-open-buffer-empty-list ()
  "Returns nil for an empty open-buffers alist."
  (let ((dir (make-temp-file "ert-magit-find-empty-" t)))
    (unwind-protect
        (should-not (magit-open-repo--find-open-buffer dir nil))
      (delete-directory dir t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit-open-repo--worktree-paths

(ert-deftest magit-open-repo/worktree-paths-extracts-additional-worktrees ()
  "Returns paths of additional worktrees, excluding the main worktree."
  (cl-letf (((symbol-function 'magit-open-repo--run-git)
             (lambda (&rest _)
               '("worktree /repo/main"
                 "HEAD abc123"
                 "branch refs/heads/main"
                 ""
                 "worktree /repo/feature"
                 "HEAD def456"
                 "branch refs/heads/feature"
                 ""))))
    (should (equal '("/repo/feature") (magit-open-repo--worktree-paths "/repo/main")))))

(ert-deftest magit-open-repo/worktree-paths-single-worktree ()
  "Returns nil when only the main worktree exists."
  (cl-letf (((symbol-function 'magit-open-repo--run-git)
             (lambda (&rest _)
               '("worktree /repo/main"
                 "HEAD abc123"
                 "branch refs/heads/main"
                 ""))))
    (should-not (magit-open-repo--worktree-paths "/repo/main"))))

(ert-deftest magit-open-repo/worktree-paths-no-git-output ()
  "Returns nil when git returns nil (not a repo or git error)."
  (cl-letf (((symbol-function 'magit-open-repo--run-git) (lambda (&rest _) nil)))
    (should-not (magit-open-repo--worktree-paths "/no/repo"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; magit-open-repo--submodule-paths

(ert-deftest magit-open-repo/submodule-paths-extracts-paths ()
  "Returns absolute submodule paths from git submodule status output."
  (cl-letf (((symbol-function 'magit-open-repo--run-git)
             (lambda (&rest _)
               '(" abc123 sub/module1 (v1.0)"
                 " def456 sub/module2 (v2.0)"))))
    (let ((result (magit-open-repo--submodule-paths "/repo")))
      (should (= 2 (length result)))
      (should (string-suffix-p "sub/module1" (car result)))
      (should (string-suffix-p "sub/module2" (cadr result))))))

(ert-deftest magit-open-repo/submodule-paths-empty ()
  "Returns nil when there are no submodules."
  (cl-letf (((symbol-function 'magit-open-repo--run-git) (lambda (&rest _) nil)))
    (should-not (magit-open-repo--submodule-paths "/repo"))))

(ert-deftest magit-open-repo/submodule-paths-returns-absolute ()
  "All returned paths are absolute."
  (cl-letf (((symbol-function 'magit-open-repo--run-git)
             (lambda (&rest _) '(" abc123 vendor/lib (v1)"))))
    (should (seq-every-p #'file-name-absolute-p
                         (magit-open-repo--submodule-paths "/repo")))))

(provide 'test-magit-open-repo)
;;; test-magit-open-repo.el ends here
