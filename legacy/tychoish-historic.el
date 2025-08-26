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
                 tychoish/emacs-instance-id
                 ">:"
                 mode-line-buffer-identification
                 " "
                 mode-line-position
                 ;; '(vc-mode vc-mode)
                 "%M"
                 global-mode-string
                 ""
                 mode-line-modes)))


(provide 'tychoish-historic)
