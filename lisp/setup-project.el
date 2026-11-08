;;; setup-project.el --- Projectile and project management configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Extracted from setup-core.el for modularity and isolation of Projectile setup.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'alert)
(require 'project)
(require 'projectile)
(require 'gen)
(require 'sprite)
(require 'annotated-completing-read)

(defvar compilation-save-buffers-predicate)
(defvar popper-group-function)
(declare-function popper-group-by-projectile "popper")

(defvar projectile-cache-fallback-directory (sprite-state-path "projectile-cache")
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

(defun projectile-purge-file-from-cache-debounced (_orig-fn file)
  "Purge FILE from cache with debounced idle flush.
Replaces synchronous serialization with `projectile--schedule-cache-flush'."
  (let* ((project-root (projectile-project-root))
         (project-cache (map-elt projectile-projects-cache project-root)))
    (if (projectile-file-cached-p file project-root)
        (let ((new-cache (remove file project-cache)))
          (setf (map-elt projectile-projects-cache project-root) new-cache)
          (when (projectile-persistent-cache-p)
            (projectile--schedule-cache-flush project-root))
          (projectile--maybe-watch-project project-root new-cache)
          (projectile--message "%s removed from cache" file))
      (user-error "%s is not in the cache" file))))

(cl-defun projectile--run-incrementally (fn items &key label delay on-done (progress-interval 10))
  "Call FN on each of ITEMS cooperatively via `gen-run-async'.
FN returns non-nil when the item counted as handled.  LABEL names the
operation in progress messages.  Calls ON-DONE with the handled count."
  (let ((total (length items))
        (count 0)
        (processed 0))
    (gen-run-async
     (lambda (item)
       (when (funcall fn item)
         (setq count (1+ count)))
       (setq processed (1+ processed))
       (when (and progress-interval
                  (or (= processed total)
                      (zerop (mod processed progress-interval))))
         (message "%s [%d/%d]..." label processed total)))
     items
     :name label
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

(cl-defun projectile--toggle-all-buffers (arg &key delay on-done (progress-interval 10))
  "Call `projectile-mode' with ARG across all live buffers incrementally."
  (projectile--run-incrementally
   (apply-partially #'projectile--set-mode-in-buffer arg) (buffer-list)
   :label (if (> arg 0) "Enabling projectile-mode" "Disabling projectile-mode")
   :delay delay :on-done on-done :progress-interval progress-interval))

;;;###autoload
(cl-defun projectile-enable-all-buffers (&key delay on-done (progress-interval 10))
  "Enable `projectile-mode' in all live buffers without blocking."
  (interactive)
  (projectile--toggle-all-buffers 1 :delay delay :on-done on-done :progress-interval progress-interval))

;;;###autoload
(cl-defun projectile-disable-all-buffers (&key delay on-done (progress-interval 10))
  "Disable `projectile-mode' in all live buffers without blocking."
  (interactive)
  (projectile--toggle-all-buffers -1 :delay delay :on-done on-done :progress-interval progress-interval))

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
       :label (format "Loading buffers for %s" (projectile-project-name root))
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

(cl-defun projectile--filter-cache-incrementally (pred &optional project
                                                       &key label delay on-done
                                                       (chunk-size 1000)
                                                       (progress-interval 1))
  "Remove paths matching PRED from PROJECT's cache without blocking.
Calls PRED on each cached path.  When PRED returns non-nil, the path is purged.
LABEL names the operation in progress messages.  ON-DONE is called with
the count of purged files."
  (let* ((root (or project (projectile-project-root)))
         (cache (and root (map-elt projectile-projects-cache root))))
    (unless root
      (user-error "Not in a project"))
    (unless cache
      (user-error "No cache found for project %s" root))
    (let* ((chunks (seq-partition cache (max 1 chunk-size)))
           (kept-chunks nil)
           (purged-count 0))
      (projectile--run-incrementally
       (lambda (chunk)
         (let ((filtered (seq-remove pred chunk)))
           (setq purged-count (+ purged-count (- (length chunk) (length filtered))))
           (push filtered kept-chunks)
           t))
       chunks
       :label (or label (format "Purging cache for %s" (projectile-project-name root)))
       :delay delay
       :progress-interval progress-interval
       :on-done (lambda (_count)
                  (if (zerop purged-count)
                      (when on-done (funcall on-done 0))
                    (let ((new-cache (apply #'nconc (nreverse kept-chunks))))
                      (setf (map-elt projectile-projects-cache root) new-cache)
                      (when (projectile-persistent-cache-p)
                        (projectile--schedule-cache-flush root))
                      (projectile--maybe-watch-project root new-cache)
                      (when on-done
                        (funcall on-done purged-count)))))))))

;;;###autoload
(cl-defun projectile-purge-dir-from-cache (dir &key delay on-done (chunk-size 1000) (progress-interval 1))
  "Purge DIR from the cache of the current project cooperatively without blocking.
Optionally accepts DELAY, ON-DONE callback, CHUNK-SIZE, and PROGRESS-INTERVAL."
  (interactive
   (list (projectile-completing-read
          "Remove directory from cache: "
          (projectile-current-project-dirs)
          :caller 'projectile-read-directory)))
  (let ((root (projectile-project-root)))
    (projectile--filter-cache-incrementally
     (lambda (str) (string-prefix-p dir str))
     root
     :label (format "Purging %s from %s" dir (projectile-project-name root))
     :delay delay :chunk-size chunk-size :progress-interval progress-interval
     :on-done (lambda (count)
                (message "Purged %s from %s (%d files removed)."
                         dir (projectile-project-name root) count)
                (when on-done
                  (funcall on-done count))))))

;;;###autoload
(cl-defun projectile-purge-files-from-cache (files &optional project &key delay on-done (chunk-size 1000) (progress-interval 1))
  "Purge FILES from PROJECT's cache cooperatively without blocking."
  (interactive
   (list (completing-read-multiple
          "Remove files from cache (comma-separated): "
          (projectile-current-project-files))))
  (let* ((root (or project (projectile-project-root)))
         (file-set (make-hash-table :test #'equal)))
    (dolist (f files)
      (setf (map-elt file-set f) t))
    (projectile--filter-cache-incrementally
     (lambda (f) (map-elt file-set f))
     root
     :label (format "Purging files from %s" (projectile-project-name root))
     :delay delay :chunk-size chunk-size :progress-interval progress-interval
     :on-done (lambda (count)
                (message "Purged %d files from %s." count (projectile-project-name root))
                (when on-done
                  (funcall on-done count))))))

(provide 'setup-project)
;;; setup-project.el ends here
