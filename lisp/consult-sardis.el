;;; consult-sardis.el --- sards cmdr consult helpers -*- lexical-binding: t -*-

(defun tychoish/compile--post-hook-collection (selection buffer-name started-at)
  (let* ((end-at (current-time))
         (duration (float-time (time-subtract end-at started-at)))
         (msg (format "completed %s in %.06fs" selection duration)))
    ;; (if (> duration 300)
    ;; (shell-command (concat "sardis notify send '" msg "'") "*sardis-logs*" "*sardis-logs*"))
    (alert
     :title selection
     :buffer (get-buffer buffer-name))
    (when (buffer-live-p buffer-name)
      (with-current-buffer buffer-name
        (insert (format "\n--- %s completed in %.06fs at %s\n" selection duration
                        (format-time-string "%Y-%m-%d %H:%M:%S" end-at)))))))

(defmacro tychoish/compile-post-hook-function (operation &rest args)
  `(apply #',operation ,args))

(defun consult-sardis--select-cmd ()
  (consult--read
   (sardis-commands (lit-string (shell-command-to-string "sardis cmd") "\n" t))
   :prompt "sards.cmds=>: "
   :group (consult--type-group sardis-commands)
   :narrow (consult--type-narrow sardis-commands)
   :require-match nil
   :category 'tychoish/sardis-cmds)))

;;;###autoload
(defun consult-sardis-run ()
  "select and run a sardis command in a compile buffer"
  (interactive)
  (let* ((selection (consult-sardis--select-cmd))
         ;; setup the environment
         (start-at (current-time))
         (compile-command (concat "sardis cmd " selection))
         (compilation-ask-about-save nil)
         (compilation-arguments nil)
         (compilation-read-command (lambda (_ignored) compile-command))
         (task-id (format "sardis-cmd-%s" selection))
         (op-buffer-name (concat "*" task-id "*"))
         (compilation-buffer-name-function (lambda (&optional args) op-buffer-name))
         (post-hook (tychoish/compile-post-hook-function selection op-buffer-name start-at))

    (add-hygenic-one-shot-hook
     :name task-id
     :hook compilation-finish-functions
     :function post-hook
     :local t)

    (let ((buf (get-buffer op-buffer-name)))
      (when buf
        (with-current-buffer buf
          (save-excursion
            (setq-local buffer-read-only nil)
            (end-of-buffer)
            (insert (format "\n--- [%s] -- %s\n" selection (format-time-string "%Y-%m-%d %H:%M:%S" start-at)))
            (setq-local buffer-read-only t)))))

    (compilation-start compile-command nil nil nil t)))


(provide 'consult-sardis)
