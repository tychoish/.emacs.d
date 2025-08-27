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

(use-package clang-format
  :ensure t
  :after (c++-mode)
  :bind (([C-M-tab] . clang-format-region))
  :commands (clang-format clang-format-buffer clang-format-region)
  :init
  (defun clang-format-before-save ()
    (interactive)
    (when (or (eq major-mode 'c++-mode)
              (eq major-mode 'c-mode)
              (eq major-mode 'c-ts-mode)
              (eq major-mode 'c++-ts-mode))
      (clang-format-buffer)))

  (add-hook 'before-save-hook 'clang-format-before-save)
  :config
  (setq clang-format-style "Google"))

(use-package auctex
  :ensure t
  :init
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (setq reftex-plug-into-AUCTeX t)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-PDF-mode t))


(provide 'tychoish-historic)
