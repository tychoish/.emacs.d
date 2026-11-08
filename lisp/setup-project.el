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

(require 'sprite)
(require 'annotated-completing-read)

(defvar compilation-save-buffers-predicate)

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

(with-eval-after-load 'popper
  (setq popper-group-function #'popper-group-by-projectile))

;;;###autoload
(defun projectile-save-buffers-in-directory (dir)
  "Save all buffers visiting files within DIR using projectile."
  (let ((default-directory (expand-file-name dir)))
    (require 'alert)
    (alert (projectile-save-project-buffers)
	     :title (format "emacs<%s> (%s)" (or (bound-and-true-p sprite-instance-id) "primary") dir))))

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

(defvar projectile-cache-fallback-directory
  (sprite-state-path "projectile-cache")
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

(cl-defun projectile--toggle-all-buffers (enable &key delay on-done (progress-interval 10))
  "Enable or disable `projectile-mode' in all live buffers incrementally.
When ENABLE is non-nil, enable; otherwise disable.  Uses `gen.el' yielding
generator iterators to toggle `projectile-mode' cooperatively."
  (let* ((verb-active (if enable "Enabling" "Disabling"))
         (verb-past (if enable "Enabled" "Disabled"))
         (action-fn (if enable #'projectile-mode-enable-for-buffer #'projectile-mode-disable-for-buffer))
         (bufs (buffer-list))
         (total (length bufs))
         (count 0)
         (processed 0)
         (del (or delay 0.0)))
    (if (and (require 'gen nil t) (fboundp 'gen-run-async))
        (progn
          (when (called-interactively-p 'any)
            (message "%s projectile-mode across %d buffers..." verb-active total))
          (gen-run-async
           (lambda (buf done-cb)
             (when (buffer-live-p buf)
               (funcall action-fn buf)
               (setq count (1+ count)))
             (setq processed (1+ processed))
             (when (and progress-interval
                        (or (= processed total)
                            (zerop (mod processed progress-interval))))
               (message "%s projectile-mode [%d/%d]..." verb-active processed total))
             (funcall done-cb))
           bufs
           :concurrency 1
           :delay del
           :on-done (lambda ()
                      (when (called-interactively-p 'any)
                        (message "%s projectile-mode in %d buffer%s (scanned %d)."
                                 verb-past count (if (= count 1) "" "s") total))
                      (when on-done (funcall on-done count)))
           :on-error (lambda (err)
                       (message "Error %s projectile-mode: %S" (downcase verb-active) err))))
      ;; Synchronous fallback
      (seq-do action-fn bufs)
      (when (called-interactively-p 'any)
        (message "%s projectile-mode in %d buffer%s."
                 verb-past total (if (= total 1) "" "s")))
      (when on-done (funcall on-done total)))))

;;;###autoload
(cl-defun projectile-enable-all-buffers (&key delay on-done (progress-interval 10))
  "Enable `projectile-mode' in all live buffers incrementally.
Uses `gen.el' yielding generator iterators to enable `projectile-mode'
cooperatively without blocking the Emacs event loop."
  (interactive)
  (projectile--toggle-all-buffers t :delay delay :on-done on-done :progress-interval progress-interval))

;;;###autoload
(cl-defun projectile-disable-all-buffers (&key delay on-done (progress-interval 10))
  "Disable `projectile-mode' in all live buffers incrementally.
Uses `gen.el' yielding generator iterators to disable `projectile-mode'
cooperatively without blocking the Emacs event loop."
  (interactive)
  (projectile--toggle-all-buffers nil :delay delay :on-done on-done :progress-interval progress-interval))

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
                   (setq loaded (1+ loaded))))
               (setq processed (1+ processed))
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
        (seq-do
         (lambda (file)
           (let ((fpath (expand-file-name file root)))
             (when (and (file-regular-p fpath)
                        (not (find-buffer-visiting fpath)))
               (find-file-noselect fpath)
               (setq loaded (1+ loaded)))))
         files)
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
                   (setq recreated (1+ recreated)))
                 (setq processed (1+ processed))
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
