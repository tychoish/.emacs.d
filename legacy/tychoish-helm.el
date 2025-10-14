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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; helm tools (legacy)

(use-package helm
  :ensure t
  :defines (helm-completing-read-handlers-alist)
  :bind (:prefix "C-c h"
         :prefix-map tychoish/helm-center-menu-map
         ;; most interesting helm menus are under one prefix
         ("a" . helm-apropos)
         ("b" . helm-buffers-list)
         ;; ("c" . helm-company)
         ("d" . helm-stumpwm-commands)
         ;; ("e" . helm-flyspell-correct)
         ;; ("f" . helm-flycheck)
         ;; ("g" . tychoish/helm-grep-tools-map)
         ;; ("h" . help)
         ("i" . helm-imenu)
         ("j" . helm-etags-select)
         ("k" . helm-register)
         ("l" . helm-locate)
         ;; ("m" . helm-<...>)
         ;; ("n" . helm-<...>)
         ;; ("o" . tychoish/org-personal-helm-map)
         ;; ("p" . tychoish/helm-projectile-tools-map)
         ;; ("q" . tychoish/helm-query-tools-map)
         ("r" . helm-recentf)
         ;; ("r" . helm-recentf)
         ;; ("s" . helm-<prefix>-swoop)
         ("t" . helm-top)
         ;; ("u" . helm-<...>)
         ;; ("v" . helm-<...>)
         ("w" . helm-mini)
         ;; ("x" . helm-<...>)
         ("y" . helm-show-kill-ring)
         ;; ("x" . helm-<...>)

         ;; helm searchch/queries
         :map tychoish/helm-center-menu-map ;; "C-c h"
         :prefix "q"
         :prefix-map tychoish/helm-query-tools-map ;; "C-c h q"
         ("i" . helm-info)
         ("o" . helm-occur)
         ("m" . helm-man-woman)
         ("g" . helm-google-suggest)
         ("f" . helm-grep-do-git-grep)

         :map tychoish/helm-center-menu-map ;; "C-c h"
         :prefix "g"
         :prefix-map tychoish/helm-grep-tools-map ;; "C-c h g"
         ("f" . helm-find-files-grep)
         ("g" . helm-grep-do-git-grep)

         :map helm-map
	 ("<tab>" . helm-execute-persistent-action)
         ("C-j" . helm-select-action))
  :commands (helm-mode helm-autoresize-mode)
  :config
  (setq history-delete-duplicates t)
  (setq history-length 250)

  (setq helm-apropos-fuzzy-match t)
  (setq helm-M-x-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-display-function 'helm-default-display-buffer)

  (setq helm-move-to-line-cycle-in-source nil)
  (setq helm-autoresize-max-height 40)
  (setq helm-autoresize-min-height 20)
  (setq helm-autoresize-mode nil)
  (setq helm-candidate-number-limit 250)
  (setq helm-case-fold-search t)
  (setq helm-display-header-line nil)
  (setq helm-ff-cache-mode-max-idle-time 300)
  (setq helm-ff-keep-cached-candidates "local")
  (setq helm-ff-keep-cached-candidates nil)
  (setq helm-ff-refresh-cache-delay 300)
  (setq helm-grep-ag-command "rg --color=always --colors 'match:fg:black' --colors 'match:bg:yellow' --smart-case --no-heading --line-number %s %s %s")
  (setq helm-grep-ag-pipe-cmd-switches '("--colors 'match:fg:black'" "--colors 'match:bg:yellow'"))
  (setq helm-input-idle-delay 0.0)
  (setq helm-man-or-woman-function 'woman)
  (setq helm-split-window-inside-p t)
  (setq helm-c-adaptive-history-file (tychoish/conf-state-path "helm-c-adaptive-history.el"))
  (setq helm-c-adaptive-sorting t)
  (helm-autoresize-mode 1)
  (when (window-system)
    (set-face-attribute 'helm-source-header nil :height 0.98 :family "Source Code Pro" :weight 'semibold :background 'unspecified)))

(use-package helm-projectile
  :ensure t
  :bind (:map tychoish/helm-grep-tools-map ;; "C-c h g"
         :prefix "p"
         :prefix-map tychoish/helm-projectile-grep-map ;; "C-c h g p"
         ("g" . helm-projectile-grep)
         ("a" . helm-projectile-ag)
         ("c" . helm-projectile-ak)
         ("r" . helm-projectile-rg)
         :map tychoish/helm-center-menu-map ;; "C-c h"
         :prefix "p"
         :prefix-map tychoish/helm-projectile-tools-map ;; "C-c h p"
         ("m" . helm-make)
         ("p" . helm-projectile)
         ("r" . helm-projectile-rg)
         ("a" . helm-projectile-ag)
         ("g" . helm-projectile-grep)
         ("f" . helm-projectile-find-file-dwim)
         ("d" . helm-projectile-find-dir)
         ("b" . helm-projectile-switch-to-buffer))
  :commands (helm-projectile helm-projectile-rg)
  :init
  (add-hook 'projectile-mode-hook 'helm-projectile-on))

(use-package helm-ag
  :bind (:map tychoish/helm-grep-tools-map
	 :prefix "a"
	 :prefix-map tychoish/helm-ag-map
	 ("s" . helm-do-grep-ag)
         ("i" . helm-do-ag)
         ("f" . helm-do-ag-this-file)
         ("p" . helm-do-ag-project-root)
         ("b" . helm-do-ag-buffers))
  :init
  (defalias 'helm-do-grep-rg 'helm-do-grep-ag)
  :config
  (setq helm-ag-insert-at-point 'word)
  (setq helm-ag-fuzzy-match t))

(use-package helm-rg
  :ensure t
  :bind (:map tychoish/helm-grep-tools-map ;; "C-c h g"
         ("r" . helm-rg)
         ("s" . helm-rg))
  :commands (helm-rg)
  :config
  (when (window-system)
    (set-face-attribute 'helm-rg-error-message nil :foreground "pink4" :background 'unspecified :weight 'normal)
    (set-face-attribute 'helm-rg-active-arg-face nil :foreground "olive drab")
    (set-face-attribute 'helm-rg-base-rg-cmd-face nil :foreground "dim gray")
    (set-face-attribute 'helm-rg-directory-cmd-face nil :foreground "brown")
    (set-face-attribute 'helm-rg-directory-header-face nil :foreground 'unspecified :weight 'extra-bold)
    (set-face-attribute 'helm-rg-extra-arg-face nil :foreground "yellow4")
    (set-face-attribute 'helm-rg-file-match-face nil :foreground "#088")
    (set-face-attribute 'helm-rg-inactive-arg-face nil :foreground "dim gray")
    (set-face-attribute 'helm-rg-title-face nil :foreground "purple" :weight 'bold)))

(use-package helm-org
  :ensure t
  :defer t
  :bind (:map tychoish/helm-center-menu-map
	 :prefix ";"
	 :prefix-map tychoish/helm-org-mode-map
	 ("c" . helm-org-capture-templates)
	 ("f". helm-org-in-buffer-heddings)
	 ("a" . helm-org-agenda-files-headings)
	 :map tychoish/helm-center-menu-map
	 :prefix "o"
	 :prefix-map tychoish/org-mode-personal-helm-map
	 ("b" . helm-org-in-buffer-headings)
	 ("p" . helm-org-parent-headings)
	 ("a" . helm-org-agenda-files-headings))
  :commands (helm-org-capture-templates
             helm-org-in-buffer-heddings
             helm-org-agenda-files-headings)
  :config
  (add-to-list 'helm-completing-read-handlers-alist '(org-capture . helm-org-completing-read-tags))
  (add-to-list 'helm-completing-read-handlers-alist '(org-set-tags . helm-org-completing-read-tags)))

(use-package helm-flyspell
  :ensure t
  :bind (:map tychoish/helm-center-menu-map
         ("e" . helm-flyspell-correct)))

(use-package flyspell-correct-helm
  :ensure t
  :after (helm-flyspell)
  :config
  (setq flyspell-correct-interface #'flyspell-correct-helm))

(with-eval-after-load 'eglot
  (when (and (featurep 'helm) helm-mode
	     (not (or (featurep 'vertico)
		      vertico-mode)))
    (bind-keys :map eglot-mode-map
               ("C-c l m" . helm-imenu))))
