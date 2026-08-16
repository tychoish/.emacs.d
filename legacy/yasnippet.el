;;; yasnippet.el --- Legacy YASnippet configuration -*- lexical-binding: t -*-

(use-package yasnippet
  :ensure t
  :commands (yas-insert-snippet yas-expand-snippet yas-lookup-snippet)
  :init
  (add-hook 'text-mode-hook #'yas-minor-mode)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  :config
  (delight 'yas-minor-mode " ys")
  (add-to-list 'load-path (file-name-concat user-emacs-directory "snippets"))
  (keymap-set hud-yasnippet-map "C-s" #'yas-insert-snippet)
  (keymap-set hud-yasnippet-map "C-n" #'yas-new-snippet)
  (keymap-set hud-yasnippet-map "C-v" #'yas-visit-snippet-file)
  (keymap-set yas-minor-mode-map "C-c &" '("yasnippet" . hud-yasnippet-map)))

(use-package yasnippet-capf
  :ensure t
  :defer t
  :init
  (keymap-set hud-completion-map "s" #'yasnippet-capf)
  (declare-function yasnippet-capf "yasnippet-capf")
  (add-hook 'completion-at-point-functions #'yasnippet-capf))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :defer t)

(use-package consult-yasnippet
  :ensure t
  :defer t
  :init
  (keymap-set hud-consult-mode-map "s" #'consult-yasnippet)
  (keymap-set hud-completion-map "y" #'consult-yasnippet))

(provide 'yasnippet)
