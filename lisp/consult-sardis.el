;;; consult-sardis.el --- sards cmdr consult helpers -*- lexical-binding: t -*-

(require 'consult)

(require 'tychoish-common)
(require 'consult-tycho)

(defun tychoish/compile--post-hook-collection (selection buffer-name started-at)
  (let* ((end-at (current-time))
	 (duration (time-subtract end-at started-at))
	 (msg (format "completed %s in %.06fs" selection (float-time duration))))

    (when (> (float-time duration) 300)
      (async-start-process
       (pa "emacs-process-name" :is "sardis-notify")
       (pa "program" :is " sardis")
       (pa "on-finish" :is (lambda (out) (with-quiet (message "notify process completed [%s] for %s" out selection))))
       ;; args
       "notify" "send" msg))

    (alert msg
     :title selection
     :buffer (get-buffer buffer-name))

    (with-current-buffer (get-buffer buffer-name)
      (save-excursion
	(setq buffer-read-only nil)
	(goto-char (point-min))
	(unless (eq (point-min) (re-search-forward "\\(^Compilation.*\n$\\|\n{2,}\\)"))
	  (replace-match ""))
	(goto-char (point-max))
	(compilation-insert-annotation
	 (format "--- %s completed in %.06fs at %s\n\n"
		 selection (float-time duration)
		 (format-time-string "%Y-%m-%d %H:%M:%S" end-at)))
	 (setq buffer-read-only t)))))

(defun consult-sardis--select-cmd ()
  (let ((table (ht-create)))

    (->> (split-string (shell-command-to-string "sardis cmd --annotate") "\n")
	 (--map (split-string it "\t" t "[ \s\t\n]"))
	 (-non-nil)
	 (--mapc (ht-set table (car it) (cadr it))))

    (consult-tycho--read-annotated
     table
     :prompt "sards.cmds => "
     :require-match nil
     :command 'consult-sardis
     :category 'tychoish/sardis-cmds)))

(defalias 'sardis-run 'consult-sardis-run)

;;;###autoload
(defun consult-sardis-run (&optional sardis-command)
  "select and run a sardis command in a compile buffer"
  (interactive)
  (let* ((selection (or sardis-command
			(consult-sardis--select-cmd)))
         ;; setup the environment
         (start-at (current-time))
         (task-id (format "sardis-cmd-%s" selection))
         (op-buffer-name (concat "*" task-id "*")))

    (with-current-buffer (get-buffer-create op-buffer-name)
      (add-hygenic-one-shot-hook
       :name "task-id"
       :hook 'compilation-finish-functions
       :local t
       :make-unique t
       :function (lambda () (tychoish/compile--post-hook-collection
			     selection op-buffer-name start-at)))

      (save-excursion
        (goto-char (point-min))
	(with-force-write
	    (if (zerop (buffer-size))
		(compilation-insert-annotation (format "# %s\n\n" selection))
	      (compilation-insert-annotation "\n"))

          (compilation-insert-annotation
	   (format "--- [%s] -- %s --\n" selection (format-time-string "%Y-%m-%d %H:%M:%S" start-at))))))

    (compilation-start
     (concat "sardis cmd " selection)
     (pa "mode" :is nil)
     (compile-buffer-name op-buffer-name)
     (pa "highlight-regexp" :is nil)
     (pa "continue" :is nil))))

(provide 'consult-sardis)
