;;; setup-project.el --- Projectile and project management configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Extracted from setup-core.el for modularity and isolation of Projectile setup.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'project)
(require 'projectile)
(require 'gen)
(require 'sprite)
(require 'annotated-completing-read)

(defvar compilation-save-buffers-predicate)
(defvar popper-group-function)
(declare-function popper-group-by-projectile "popper")

(defvar projectile-cache-fallback-directory
  (sprite-state-path "projectile-cache")
  "Directory used for projectile caches when project root is not writable.")

(with-eval-after-load 'popper
  (setq popper-group-function #'popper-group-by-projectile))

(setq compilation-save-buffers-predicate #'projectile-project-root)
(setq projectile-use-git-grep t)
(setq projectile-enable-caching 'persistent)
(setq projectile-require-project-root nil)

(advice-add 'projectile-project-cache-file :around #'projectile-project-cache-file-fallback)
(advice-add 'projectile-serialize :around #'projectile-serialize-silent-fallback)
(advice-add 'projectile-purge-file-from-cache :around #'projectile-purge-file-from-cache-debounced)

;;;###autoload
(defun projectile-save-buffers-in-directory (dir)
  "Save all buffers visiting files within DIR using projectile."
  (let ((default-directory (expand-file-name dir)))
    (alert (projectile-save-project-buffers)
	   :title (format "emacs<%s> (%s)" (or sprite-instance-id "primary") dir))))

;;;###autoload
(defun projectile-save-project-buffers-picker ()
  "Interactively pick a project directory from open buffers and save its files."
  (interactive)
  (projectile-save-buffers-in-directory
   (annotated-completing-read
    (thread-last
      (buffer-list)
      (seq-keep #'buffer-file-name)
      (seq-keep #'projectile-project-root)
      (seq-uniq)
      (seq-sort #'string<)
      (seq-map (lambda (dir) (cons dir nil))))
    :prompt "directories =>"
    :category 'file)))

(defun projectile-cache--fallback-file (name-source)
  "Return the fallback cache file path for NAME-SOURCE, ensuring dir exists."
  (let ((norm (directory-file-name (expand-file-name name-source))))
    (make-directory projectile-cache-fallback-directory t)
    (expand-file-name (concat (file-name-nondirectory norm) "-" (md5 norm) ".eld")
                      projectile-cache-fallback-directory)))

(defun projectile-project-cache-file-fallback (orig-fn &optional project-root)
  "Return cache file for PROJECT-ROOT using fallback dir when non-writable."
  (when-let* ((root (or project-root (projectile-project-root)))
              (in-tree (funcall orig-fn root)))
    (let ((norm-root (directory-file-name (expand-file-name root))))
      (if (file-writable-p (if (file-exists-p in-tree) in-tree norm-root))
          in-tree
        (projectile-cache--fallback-file norm-root)))))

(defun projectile-serialize-silent-fallback (orig-fn data filename)
  "Silently serialize DATA to fallback file when FILENAME is not writable."
  (if (file-writable-p filename)
      (funcall orig-fn data filename)
    (let ((fallback-file (projectile-cache--fallback-file filename)))
      (when (file-writable-p fallback-file)
        (with-temp-file fallback-file
          (insert (let (print-length) (prin1-to-string data))))))))


(cl-defun projectile--run-incrementally (fn items &key label delay on-done (progress-interval 10))
  "Call FN on each of ITEMS cooperatively via `gen-run-async'.
FN returns non-nil when the item counted as handled.  LABEL names the
operation in progress messages.  Calls ON-DONE with the handled count."
  (let ((total (length items))
        (count 0)
        (processed 0))
    (gen-run-async
     (lambda (item done-cb)
       (when (funcall fn item)
         (setq count (1+ count)))
       (setq processed (1+ processed))
       (when (and progress-interval
                  (or (= processed total)
                      (zerop (mod processed progress-interval))))
         (message "%s [%d/%d]..." label processed total))
       (funcall done-cb))
     items
     :delay (or delay 0.0)
     :on-done (lambda ()
                (message "%s: handled %d of %d." label count total)
                (when on-done
                  (funcall on-done count)))
     :on-error (lambda (err)
                 (message "%s: error %S" label err)))))

(defun projectile--set-mode-in-buffer (arg buf)
  "Call `projectile-mode' with ARG in live buffer BUF; return non-nil if it ran."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (projectile-mode arg))
    t))

;;;###autoload
(cl-defun projectile-enable-all-buffers (&key delay on-done (progress-interval 10))
  "Enable `projectile-mode' in all live buffers without blocking."
  (interactive)
  (projectile--run-incrementally
   (apply-partially #'projectile--set-mode-in-buffer 1) (buffer-list)
   :label "Enabling projectile-mode" :delay delay :on-done on-done
   :progress-interval progress-interval))

;;;###autoload
(cl-defun projectile-disable-all-buffers (&key delay on-done (progress-interval 10))
  "Disable `projectile-mode' in all live buffers without blocking."
  (interactive)
  (projectile--run-incrementally
   (apply-partially #'projectile--set-mode-in-buffer -1) (buffer-list)
   :label "Disabling projectile-mode" :delay delay :on-done on-done
   :progress-interval progress-interval))

;;;###autoload
(cl-defun projectile-load-project-buffers (&optional project &key delay on-done (progress-interval 5))
  "Visit the files of PROJECT (default: current root) without blocking."
  (interactive)
  (let ((root (or project (projectile-project-root))))
    (unless root
      (user-error "Not in a project"))
    (let ((files (projectile-project-files root)))
      (unless files
        (user-error "No files found in project %s" root))
      (projectile--run-incrementally
       (lambda (file)
         (let ((path (expand-file-name file root)))
           (when (and (file-regular-p path)
                      (not (find-buffer-visiting path)))
             (find-file-noselect path)
             t)))
       files
       :label (format "Loading buffers for %s" (projectile-project-name))
       :delay delay :on-done on-done :progress-interval progress-interval))))

;;;###autoload
(cl-defun projectile-session-restore-async (&optional project &key delay on-done)
  "Restore PROJECT's (default: current) saved session buffers without blocking."
  (interactive)
  (let* ((root (or project (projectile-project-root)))
         (data (and root (projectile-session--read root))))
    (cond
     (data
      (projectile--run-incrementally
       (lambda (saved)
         (buffer-live-p (ignore-errors (projectile-session--recreate-buffer saved))))
       (plist-get data :buffers)
       :label (format "Restoring session for %s" root)
       :delay delay :progress-interval nil
       :on-done (lambda (count)
                  (when-let* ((state (plist-get data :window-state)))
                    (condition-case err
                        (window-state-put (projectile-session--sanitize-window-state state)
                                          (frame-root-window) 'safe)
                      (error
                       (message "projectile-session: could not restore window layout: %S" err))))
                  (when on-done
                    (funcall on-done count)))))
     ((called-interactively-p 'any)
      (user-error "No saved session for %s" (or root "current project"))))))

(provide 'setup-project)
;;; setup-project.el ends here
