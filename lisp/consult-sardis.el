;;; consult-sardis.el --- sards cmdr consult helpers -*- lexical-binding: t -*-

(defun --quiet-log-when (cond msg &rest args)
  (when cond (apply #'--quiet-log (cons msg args))))

(defun --quiet-log (msg &rest args)
  (let ((inhibit-message t))
    (apply #'message msg args)))

(defun tychoish/compile--post-hook-collection (selection buffer-name started-at)
  (let* ((end-at (current-time))
	 (duration (time-subtract end-at started-at))
	 (msg (format "completed %s in %.06fs" selection (float-time duration))))

    (when (> (float-time duration) 300)
      (async-start-process "sardis-notify"
         "sardis"
	 (lambda (out) (--quiet-log-when out "notify process completed [%s] for %s" out selection))
	 "notify" "send" msg))

    (alert
     msg
     :title selection
     :buffer (get-buffer buffer-name))

    (with-current-buffer (get-buffer buffer-name)
      (save-excursion
	(setq buffer-read-only nil)
	(beginning-of-buffer)
	(replace-regexp "\\(^Compilation.*\n$\\|\n{2,}\\)" "")
	(end-of-buffer)
	(compilation-insert-annotation
	 (format "--- %s completed in %.06fs at %s\n\n"
		 selection (float-time duration)
		 (format-time-string "%Y-%m-%d %H:%M:%S" end-at)))
	 (setq buffer-read-only t)))))

(defun consult-sardis--select-cmd ()
  (let ((sardis-commands (split-string (shell-command-to-string "sardis cmd") "\n" t))   )
    (consult--read
     sardis-commands
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
         (task-id (format "sardis-cmd-%s" selection))
         (op-buffer-name (concat "*" task-id "*")))

    (setq compilation-finish-functions nil)

    (with-current-buffer (get-buffer-create op-buffer-name)
      (add-hygenic-one-shot-hook
       :name task-id
       :hook compilation-finish-functions
       :function (lambda ()
		  (tychoish/compile--post-hook-collection
		  selection op-buffer-name start-at))
       :local nil)

      (save-excursion
        (end-of-buffer)
        (setq buffer-read-only nil)

	(if (zerop (buffer-size))
	    (compilation-insert-annotation (format "# %s\n\n" selection))
	  (compilation-insert-annotation "\n"))

        (compilation-insert-annotation
	 (format "--- [%s] -- %s --\n" selection (format-time-string "%Y-%m-%d %H:%M:%S" start-at)))
	(setq buffer-read-only t)))

    (compilation-start
     (concat "sardis cmd " selection)
     nil
     (lambda (&optional _) op-buffer-name)
     nil t)))

(provide 'consult-sardis)
