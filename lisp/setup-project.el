;;; setup-project.el --- Projectile and project management configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Extracted from setup-core.el for modularity and isolation of Projectile setup.

;;; Code:

(require 'cl-lib)
(require 'generator)
(require 'seq)
(require 'subr-x)
(require 'project)
(require 'projectile)
(require 'sprite nil t)

(autoload 'annotated-completing-read "acr")

(cl-defmethod project-root ((project (head cmake-root))) (cdr project))

(defun project-find-cmake-project (dir)
  (when-let* ((root (locate-dominating-file dir "CMakeLists.txt")))
    (cons 'cmake-root root)))

(add-hook 'project-find-functions #'project-find-cmake-project)

(cl-defmethod project-root ((project (head go-module))) (cdr project))

(defun project-find-go-module (dir)
  (when-let* ((root (or (locate-dominating-file dir "go.work")
                        (locate-dominating-file dir "go.mod"))))
    (cons 'go-module root)))

(add-hook 'project-find-functions #'project-find-go-module)

(defvar compilation-save-buffers-predicate)

(setq compilation-save-buffers-predicate #'projectile-project-root)

;;;###autoload
(defun turn-on-projectile-mode ()
  "Enable projectile-mode with project.el integration."
  (interactive)
  (require 'project)
  (projectile-mode 1))

(add-hook 'prog-mode-hook #'turn-on-projectile-mode)
(add-hook 'text-mode-hook #'turn-on-projectile-mode)
(add-hook 'dired-mode-hook #'turn-on-projectile-mode)

;;;###autoload
(defun projectile-save-buffers-in-directory (dir)
  "Save all buffers visiting files within DIR using projectile."
  (let ((default-directory (expand-file-name dir)))
    (when (require 'alert nil t)
      (alert (projectile-save-project-buffers)
	     :title (format "emacs<%s> (%s)" (or (bound-and-true-p sprite-instance-id) "primary") dir)))))

;;;###autoload
(defun projectile-save-project-buffers-picker ()
  "Interactively pick a project directory from open buffers and save its files."
  (interactive)
  (projectile-save-buffers-in-directory
   (annotated-completing-read
    (thread-last
      (buffer-list)
      (seq-map #'buffer-file-name)
      (seq-filter 'identity)
      (seq-sort #'string<)
      (seq-map #'projectile-project-root)
      (seq-uniq)
      (seq-filter 'identity)
      (seq-map (lambda (dir) (cons dir nil))))
    :prompt "directories =>"
    :category 'file)))

(setq projectile-use-git-grep t)
(setq projectile-enable-caching 'persistent)
(setq projectile-require-project-root nil)
(defun projectile-state-path (name)
  "Return path for state file NAME, delegating to `sprite-state-path' if available."
  (if (fboundp 'sprite-state-path)
      (sprite-state-path name)
    (expand-file-name name user-emacs-directory)))

(setq projectile-known-projects-file (projectile-state-path "projectile-bookmarks.el"))
(setq projectile-frecency-file (projectile-state-path "projectile-frecency.el"))

(defvar projectile-cache-fallback-directory
  (projectile-state-path "projectile-cache")
  "Directory used for projectile caches when project root is not writable.")

(defun projectile-cache--fallback-file (name-source)
  "Return the fallback cache file path for NAME-SOURCE, ensuring dir exists."
  (let* ((cache-dir (file-name-as-directory (expand-file-name projectile-cache-fallback-directory)))
         (norm (directory-file-name (expand-file-name name-source)))
         (slug (concat (file-name-nondirectory norm)
                       "-"
                       (md5 norm)
                       ".eld")))
    (unless (file-directory-p cache-dir)
      (ignore-errors (make-directory cache-dir t)))
    (expand-file-name slug cache-dir)))

(defun projectile-project-cache-file-fallback (orig-fn &optional project-root)
  "Return cache file for PROJECT-ROOT using fallback dir when non-writable."
  (let* ((root (or project-root (projectile-project-root)))
         (in-tree (and root (funcall orig-fn root))))
    (if (or (null root) (null in-tree))
        in-tree
      (let* ((norm-root (directory-file-name (expand-file-name root)))
             (in-tree-writable (if (file-exists-p in-tree)
                                   (file-writable-p in-tree)
                                 (file-writable-p norm-root))))
        (if in-tree-writable
            in-tree
          (projectile-cache--fallback-file norm-root))))))

(advice-add 'projectile-project-cache-file :around #'projectile-project-cache-file-fallback)

(defun projectile-serialize-silent-fallback (orig-fn data filename)
  "Silently serialize DATA to fallback file when FILENAME is not writable."
  (if (file-writable-p filename)
      (funcall orig-fn data filename)
    (let ((fallback-file (projectile-cache--fallback-file filename)))
      (when (file-writable-p fallback-file)
        (with-temp-file fallback-file
          (insert (let (print-length) (prin1-to-string data))))))))

(advice-add 'projectile-serialize :around #'projectile-serialize-silent-fallback)

;;;###autoload
(defun projectile-mode-enable-for-buffer (buf)
  "Enable `projectile-mode' in buffer BUF."
  (with-current-buffer buf
    (projectile-mode 1)))

;;;###autoload
(defun projectile-mode-disable-for-buffer (buf)
  "Disable `projectile-mode' in buffer BUF."
  (with-current-buffer buf
    (projectile-mode -1)))

;;;###autoload
(cl-defun projectile-enable-all-buffers (&key delay on-done (progress-interval 10))
  "Enable `projectile-mode' in all live buffers incrementally.
Uses `gen.el' yielding generator iterators to enable `projectile-mode'
cooperatively without blocking the Emacs event loop."
  (interactive)
  (let* ((bufs (buffer-list))
         (total (length bufs))
         (count 0)
         (processed 0)
         (del (or delay 0.0)))
    (if (and (require 'gen nil t) (fboundp 'gen-run-async))
        (progn
          (when (called-interactively-p 'any)
            (message "Enabling projectile-mode across %d buffers..." total))
          (gen-run-async
           (lambda (buf done-cb)
             (when (buffer-live-p buf)
               (projectile-mode-enable-for-buffer buf)
               (cl-incf count))
             (cl-incf processed)
             (when (and progress-interval
                        (or (= processed total)
                            (zerop (mod processed progress-interval))))
               (message "Enabling projectile-mode [%d/%d]..." processed total))
             (funcall done-cb))
           bufs
           :concurrency 1
           :delay del
           :on-done (lambda ()
                      (when (called-interactively-p 'any)
                        (message "Enabled projectile-mode in %d buffer%s (scanned %d)."
                                 count (if (= count 1) "" "s") total))
                      (when on-done (funcall on-done count)))
           :on-error (lambda (err)
                       (message "Error enabling projectile-mode: %S" err))))
      ;; Synchronous fallback
      (seq-do #'projectile-mode-enable-for-buffer bufs)
      (when (called-interactively-p 'any)
        (message "Enabled projectile-mode in %d buffer%s."
                 total (if (= total 1) "" "s")))
      (when on-done (funcall on-done total)))))

;;;###autoload
(cl-defun projectile-disable-all-buffers (&key delay on-done (progress-interval 10))
  "Disable `projectile-mode' in all live buffers incrementally.
Uses `gen.el' yielding generator iterators to disable `projectile-mode'
cooperatively without blocking the Emacs event loop."
  (interactive)
  (let* ((bufs (buffer-list))
         (total (length bufs))
         (count 0)
         (processed 0)
         (del (or delay 0.0)))
    (if (and (require 'gen nil t) (fboundp 'gen-run-async))
        (progn
          (when (called-interactively-p 'any)
            (message "Disabling projectile-mode across %d buffers..." total))
          (gen-run-async
           (lambda (buf done-cb)
             (when (buffer-live-p buf)
               (projectile-mode-disable-for-buffer buf)
               (cl-incf count))
             (cl-incf processed)
             (when (and progress-interval
                        (or (= processed total)
                            (zerop (mod processed progress-interval))))
               (message "Disabling projectile-mode [%d/%d]..." processed total))
             (funcall done-cb))
           bufs
           :concurrency 1
           :delay del
           :on-done (lambda ()
                      (when (called-interactively-p 'any)
                        (message "Disabled projectile-mode in %d buffer%s (scanned %d)."
                                 count (if (= count 1) "" "s") total))
                      (when on-done (funcall on-done count)))
           :on-error (lambda (err)
                       (message "Error disabling projectile-mode: %S" err))))
      ;; Synchronous fallback
      (seq-do #'projectile-mode-disable-for-buffer bufs)
      (when (called-interactively-p 'any)
        (message "Disabled projectile-mode in %d buffer%s."
                 total (if (= total 1) "" "s")))
      (when on-done (funcall on-done total)))))

;;;###autoload
(cl-defun projectile-load-project-buffers (&optional project &key delay on-done (progress-interval 5))
  "Load project files into buffers incrementally without blocking.
PROJECT defaults to the current project's root.  Uses `gen.el' yielding
generator iterators to open files sequentially with cooperative delays."
  (interactive)
  (let* ((root (or project (projectile-project-root)))
         (files (and root (projectile-project-files root))))
    (unless root
      (user-error "Not in a project"))
    (unless files
      (user-error "No files found in project %s" root))
    (let* ((total (length files))
           (loaded 0)
           (processed 0)
           (del (or delay 0.0)))
      (if (and (require 'gen nil t) (fboundp 'gen-run-async))
          (progn
            (message "Loading %d buffers for project %s..." total (projectile-project-name))
            (gen-run-async
             (lambda (file done-cb)
               (let ((fpath (expand-file-name file root)))
                 (when (and (file-regular-p fpath)
                            (not (find-buffer-visiting fpath)))
                   (find-file-noselect fpath)
                   (cl-incf loaded)))
               (cl-incf processed)
               (when (and progress-interval
                          (or (= processed total)
                              (zerop (mod processed progress-interval))))
                 (message "Loading project buffers [%d/%d]..." processed total))
               (funcall done-cb))
             files
             :concurrency 1
             :delay del
             :on-done (lambda ()
                        (message "Loaded %d new buffer%s for project %s (scanned %d)."
                                 loaded (if (= loaded 1) "" "s")
                                 (projectile-project-name) total)
                        (when on-done (funcall on-done loaded)))
             :on-error (lambda (err)
                         (message "Error loading project buffers: %S" err))))
        ;; Fallback
        (dolist (file files)
          (let ((fpath (expand-file-name file root)))
            (when (and (file-regular-p fpath)
                       (not (find-buffer-visiting fpath)))
              (find-file-noselect fpath)
              (cl-incf loaded))))
        (message "Loaded %d new buffer%s for project %s."
                 loaded (if (= loaded 1) "" "s") (projectile-project-name))
        (when on-done (funcall on-done loaded))))))

;;;###autoload
(cl-defun projectile-session-restore-async (&optional project &key delay on-done)
  "Restore PROJECT's saved session buffers incrementally without blocking.
PROJECT defaults to the current project's root.  Uses `gen.el' yielding
generator iterators to recreate buffers cooperatively."
  (interactive)
  (let* ((root (or project (projectile-project-root)))
         (data (and root (fboundp 'projectile-session--read) (projectile-session--read root))))
    (cond
     (data
      (let* ((saved-bufs (plist-get data :buffers))
             (total (length saved-bufs))
             (recreated 0)
             (processed 0)
             (del (or delay 0.0)))
        (if (and (require 'gen nil t) (fboundp 'gen-run-async))
            (progn
              (message "Restoring %d session buffers for %s incrementally..." total root)
              (gen-run-async
               (lambda (saved done-cb)
                 (when (ignore-errors (buffer-live-p (projectile-session--recreate-buffer saved)))
                   (cl-incf recreated))
                 (cl-incf processed)
                 (funcall done-cb))
               saved-bufs
               :concurrency 1
               :delay del
               :on-done (lambda ()
                          (when-let* ((state (plist-get data :window-state)))
                            (condition-case err
                                (window-state-put (projectile-session--sanitize-window-state state)
                                                  (frame-root-window) 'safe)
                              (error
                               (message "projectile-session: could not restore window layout: %S" err))))
                          (message "Restored %d session buffer%s for %s."
                                   recreated (if (= recreated 1) "" "s") root)
                          (when on-done (funcall on-done recreated)))))
          ;; Synchronous fallback
          (projectile-session-restore root)
          (when on-done (funcall on-done t)))))
     ((called-interactively-p 'any)
      (user-error "No saved session for %s" (or root "current project"))))))

(provide 'setup-project)
;;; setup-project.el ends here
