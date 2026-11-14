(defun tychoish--agent-shell-buffer-p (buffer _action)
  "Return non-nil if BUFFER derives from `agent-shell-mode'."
  (with-current-buffer buffer
    (derived-mode-p 'agent-shell-mode)))

;; display-buffer: mu4e buffers prefer the frame they were first shown on
(defun tychoish--mu4e-buffer-p (buf _action)
  "Return non-nil if BUF uses a mu4e major mode."
  (with-current-buffer buf
    (derived-mode-p 'mu4e-main-mode 'mu4e-headers-mode
                    'mu4e-view-mode 'mu4e-compose-mode)))

(add-to-list 'display-buffer-alist '(tychoish--agent-shell-buffer-p (display-buffer-reuse-window display-buffer-use-some-window) (reusable-frames . t)))
(add-to-list 'display-buffer-alist '("\\`\\*denote-sequence-hierarchy" (display-buffer-reuse-window display-buffer-pop-up-window)))
(add-to-list 'display-buffer-alist '(tychoish--mu4e-buffer-p (display-buffer-reuse-window) (reusable-frames . t)))



