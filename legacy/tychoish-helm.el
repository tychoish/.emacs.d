;; unused helm packages and configurations
;; (mostly use consult+vertico now)

(use-package helm-mu
  :ensure t
  :bind (:map tychoish/helm-center-menu-map ; "C-c h"
         ("m" . helm-mu)
         ("v" . helm-mu-contacts)))

(use-package helm-slime
  :ensure t
  :after (slime helm)
  :commands (helm-slime-mode)
  :hook (slime-lisp-mode . helm-slime-mode))


(use-package helm-eww
  :ensure t
  :bind (:map tychoish/helm-center-menu-map
         ("C-c h c" . helm-eww-history)))


(use-package helm-swoop
  :ensure t
  :bind (:map tychoish/helm-center-menu-map
         :prefix "s"
         :prefix-map tychoish/helm-swoop-map
         ("s" . helm-swoop)
         ("m" . helm-multi-swoop)
         ("a" . helm-multi-swoop-all)
	 ("o" . helm-multi-swoop-org)
	 ("p" . helm-multi-swoop-projectile))
  :commands (helm-swoop)
  :config
  (setq helm-swoop-split-with-multiple-windows t)
  (setq helm-swoop-split-direction 'split-window-vertically))

(use-package helm-make
  :ensure t
  :commands (helm-make)
  :config
  (setq helm-make-named-buffer t)
  (setq helm-make-fuzzy-matching t)
  (setq helm-make-cache-targets t)
  (setq helm-make-do-save t)
  (setq helm-make-sort-targets t))

(use-package helm-flycheck
  :ensure t
  :bind (:map tychoish/helm-center-menu-map
         ("f" . 'helm-flycheck))
  :commands (helm-flycheck))

(use-package helm-c-yasnippet
  :ensure t
  :defer t
  :commands (helm-yas-complete)
  :bind (:map tychoish/helm-center-menu-map
	 ("C-e" . helm-yas-complete)
	 ("C-." . helm-yas-complete))
  :config (setq helm-yas-space-match-any-greedy t))

(use-package helm-xref
  :ensure t
  :after (xref helm))
