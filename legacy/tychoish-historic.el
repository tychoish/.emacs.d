(defun tychoish-legacy-mode-line ()
  (interactive)
  (setq-default mode-line-format
                (list
                 mode-line-mule-info
                 mode-line-client
                 mode-line-modified
                 mode-line-remote
                 mode-line-frame-identification
                 "<"
                 tychoish-emacs-identifier
                 ">:"
                 mode-line-buffer-identification
                 " "
                 mode-line-position
                 ;; '(vc-mode vc-mode)
                 "%M"
                 global-mode-string
                 ""
                 mode-line-modes)))

(defmacro with-timer (name &rest body)
  "Report on NAME and the time taken to execute BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%s: %.06fs" ,name (float-time (time-since time)))))

(defmacro with-slow-op-timer (name threshold &rest body)
  "Send a message the BODY operation of NAME takes longer to execute than the THRESHOLD."
  `(let ((time (current-time)))
     ,@body
     (tychoish--threshold-logger ,threshold (time-to-seconds (time-since time)) ,name)))





(provide 'tychoish-historic)
