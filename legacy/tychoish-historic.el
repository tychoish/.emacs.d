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

(use-package solidity-mode
  :ensure t
  :mode ("\\.sol$'" . solidity-mode)
  :config
  (setq solidity-comment-style 'slash)

  (with-eval-after-load 'cape
    (defun tychoish/soliditiysolidity-capf-setup ()
      (setq-local completion-at-point-functions
                  (list #'tychoish/capf-solidity
			#'tychoish/capf-line
			#'yasnippet-capf
			#'cape-emoji
			#'cape-file)))

    (add-hook 'solidity-mode-hook #'tychoish/solidity-capf-setup)))

(use-package solidity-flycheck
  :ensure t
  :hook (solidity-mode . tychoish/solidity-flycheck-setup)
  :commands (solidity-flycheck)
  :config
  (defun tychoish/solidity-flycheck-setup ()
    (setq-local solidity-flycheck-solc-checker-active t))

  (add-to-list 'flycheck-checkers 'solidity-flycheck))

(use-package code-review
  :ensure t
  :bind (:map forge-topic-mode-map
	 ("C-c r" . code-review-forge-pr-at-point)
	 :map code-review-feedback-section-map
	 ("k" . code-review-section-delete-comment)
	 :map code-review-local-comment-section-map
	 ("k" . code-review-section-delete-comment)
	 :map code-review-reply-comment-section-map
	 ("k" . code-review-section-delete-comment))
  :commands (code-review-start)
  :config
  (add-hook 'code-review-mode-hook #'emojify-mode)
  (setq code-review-fill-column 80)
  (setq code-review-download-dir "/tmp/code-review/"))

(provide 'tychoish-historic)
