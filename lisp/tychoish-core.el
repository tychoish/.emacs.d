;;; tychoish-core -- contains use-package forms for managing configuration.

;;; Commentary:

;; Provides my collection of use-package forms and related
;; configuration for loading and configuring all Emacs packages.

;; This configuration optimizes for lazy-loading so that configuration
;; only loads when called directly or a mode is activated.

;;; Code:

(use-package auto-package-update
  :ensure t
  :commands (auto-package-update-maybe auto-package-update-now)
  :init
  :config
  (setq auto-package-update-hide-results t
	auto-package-update-interval 9001)
  (auto-package-update-maybe))

(use-package use-package-ensure-system-package
  :after (use-package)
  :ensure t)

(use-package eldoc
  :diminish
  :commands (eldoc-mode))

(use-package diminish
  :ensure t
  :commands (diminish))

(use-package delight
  :ensure t
  :commands (delight))

(use-package autorevert
  :commands (auto-revert-mode)
  :init
  (defalias 'rb 'revert-buffer)
  (defalias 'revert 'revert-buffer)
  :config
  (diminish 'auto-revert-mode)
  (setq auto-revert-verbose nil)
  (setq auto-revert-interval 5))

(use-package abbrev
  :after (tychoish-bootstrap company-mode)
  :commands (abbrev-mode expand-abbrev)
  :hook ((text-mode prog-mode) . abbrev-mode)
  :diminish
  :config
  (setq save-abbrevs t)
  (setq abbrev-file-name (tychoish-get-config-file-path "abbrev"))
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

(use-package f
  :ensure t
  :commands (f-exists? f-join f-glob))

(use-package dash
  :ensure t
  :commands (--remove))

(use-package anzu
  :ensure t
  :diminish
  :commands (anzu-query-replace anzu-query-replace-regexp global-anzu-mode anzu-mode)
  :hook ((text-mode prog-mode isearch-mode) . anzu-mode)
  :bind (("C-c q r" . anzu-query-replace)
	 ("C-c q x" . anzu-query-replace-regexp))
  :init
  (defalias 'srr 'string-replace-regexp)
  (defalias 'sr 'string-replace)
  (setq query-replace-highlight t)
  (setq search-highlight t)
  :config
  (defalias 'qrr 'anzu-query-replace-regexp)
  (defalias 'qr 'anzu-query-replace)
  (global-anzu-mode +1))

(use-package doom-modeline
  :ensure t
  :commands (doom-modeline-mode
	     tychoish-legacy-mode-line
	     tychoish-setup-modeline)
  :bind (("C-c t i" . toggle-modeline-icons))
  :init
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq find-file-visit-truename t)
  (setq size-indication-mode t)
  (setq display-time-mode nil)
  :config
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
		   '(vc-mode vc-mode)
		   "%M"
		   global-mode-string
		   ""
		   mode-line-modes)))

  (defun tychoish-setup-modeline ()
    (interactive)
    (doom-modeline-def-segment misc-info
      '("" mode-line-misc-info))

    (doom-modeline-mode 1)

    (delight 'emacs-lisp-mode "elisp")
    (delight 'fundamental-mode "fund")
    (delight 'auto-fill-mode "afm")
    (diminish 'overwrite-mode "om")
    (diminish 'refill-mode "rf")
    (diminish 'auto-fill-mode "afm")
    (diminish 'visual-line-mode "wr"))

  (defvar *tychoish-modeline-icon-state* nil)

  (defun toggle-modeline-icons ()
    (interactive)
    (setq doom-modeline-icon (not *tychoish-modeline-icon-state*))
    (setq *tychoish-modeline-icon-state* doom-modeline-icon))

  (defun my-doom-modeline--font-height ()
    "Calculate the actual char height of the mode-line."
    (/ (frame-char-height) 4))

  (advice-add #'doom-modeline--font-height :override #'my-doom-modeline--font-height)

  (setq doom-modeline-buffer-file-name-style 'relative-to-project)
  (setq doom-modeline-height 1)
  (setq doom-modeline-irc t)
  (setq doom-modeline-irc-stylize 'identity)
  (setq doom-modeline-height 0)
  (setq doom-modeline-bar-width 1)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-icon *tychoish-modeline-icon-state*)
  (setq doom-modeline-unicode-fallback nil)
  (setq doom-modeline-minor-modes t)
  (setq doom-modeline-enable-word-count t)
  (setq doom-modeline-continuous-word-count-modes '(text-mode rst-mode org-mode))
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-env-version t)
  (setq doom-modeline-github nil)
  (setq doom-modeline-lsp t)
  (add-to-list 'mode-line-misc-info (format "[%s]" tychoish-emacs-identifier)))

(use-package doom-themes
  :ensure t
  :defer t)

(use-package base16-theme
  :ensure t
  :defer t)

(use-package modus-operandi-theme
  :ensure t
  :defer t)

(use-package modus-vivendi-theme
  :ensure t
  :defer t)

(use-package winum
  :ensure t
  :after (doom-modeline)
  :commands (winum-select-window-by-number winum-mode)
  :bind (("C-x w n" . winum-select-window-by-number))
  :config
  (setq winum-auto-setup-mode-line nil)
  (setq winum-scope 'frame-local)
  (winum-mode 1))

(use-package helm
  :ensure t
  :after (tychoish-bootstrap)
  :bind (("C-c M-s" . helm-multi-swoop)
	 ("C-x M-s" . helm-multi-swoop-all)
	 ("C-c o s" . helm-multi-swoop-org)

	 ;; most interesting helm menus are under one prefix
	 ("C-c h h" . helm-mini)
	 ("C-c h i" . helm-info)
	 ("C-c h m" . helm-man-woman)
	 ("C-c h a" . helm-apropos)
	 ("C-c h l" . helm-locate)
	 ("C-c h i" . helm-imenu)
	 ("C-c h t" . helm-top)
	 ("C-c h r" . helm-recentf)
	 ("C-c h w" . helm-google-suggest)
	 ("C-c h s" . helm-swoop)

	 ("M-p" . helm-browse-project)
	 ("M-." . helm-etags-select)
	 ("C-x r h" . helm-register)
	 ("C-x h" . helm-mini)

	 ;; helm alternatives for common standard operations
	 ("C-x C-f" . helm-find-files)
	 ("C-x d" . helm-find-files)
	 ("C-x b" . helm-buffers-list)
	 ("C-x C-d" . helm-find-files)
	 ("C-x f" . helm-for-files)
	 ("M-x" . helm-M-x)
	 ("M-y" . helm-show-kill-ring)

	 ;; I've had these keybindings as alternates for M-x forever...
	 ("C-x m" . helm-M-x)
	 ("C-x C-m" . helm-M-x)

	 :map helm-map
	 ("TAB" . helm-execute-persistent-action)
	 ("C-j" . helm-select-action))
  :config
  (set-face-attribute 'helm-source-header nil :height 0.98 :family "Source Code Pro" :weight 'semibold)
  (helm-autoresize-mode 1)

  (setq history-length 100)
  (setq history-delete-duplicates t)
  (setq helm-ff-keep-cached-candidates "local")
  (setq helm-ff-refresh-cache-delay 300)
  (setq helm-ff-cache-mode-max-idle-time 300)
  (setq helm-ff-keep-cached-candidates nil)
  (setq helm-M-x-fuzzy-match nil)
  (setq helm-autoresize-max-height 40)
  (setq helm-autoresize-min-height 20)
  (setq helm-autoresize-mode nil)
  (setq helm-c-adaptive-sorting t)
  (setq helm-c-adaptive-history-file (tychoish-get-config-file-path "helm-c-adaptive-history"))
  (setq helm-c-source-kill-ring '())
  (setq helm-candidate-number-limit 100)
  (setq helm-case-fold-search t)
  (setq helm-display-header-line nil)
  (setq helm-input-idle-delay 0)
  (setq helm-man-or-woman-function 'woman)
  (setq helm-split-window-in-side-p t))

(use-package helm-make
  :ensure t
  :bind (("C-c h c" . helm-make-projectile))
  :config
  (setq helm-make-named-buffer t)
  (setq helm-make-fuzzy-matching t)
  (setq helm-make-cache-targets t)
  (setq helm-make-do-save t)
  (setq helm-make-sort-targets t))

(use-package helm-swoop
  :ensure t
  :bind (("M-m" . helm-swoop)
	 ("M-s" . helm-swoop)
	 ("M-M" . helm-swoop-back-to-last-point))
  :init
  (bind-key "M-m" 'helm-swoop-from-isearch isearch-mode-map)
  :config
  (setq helm-swoop-split-with-multiple-windows t)
  (setq helm-swoop-split-direction 'split-window-vertically))

(use-package helm-ls-git
  :ensure t
  :bind (("C-c o g" . helm-ls-git-ls)
	 ("C-c o b" . helm-browse-project)))

(use-package helm-eww
  :ensure t
  :bind (("C-c w o" . helm-eww)))

(use-package eww
  :bind (("C-c w d" . browse-url-generic)
	 ("C-c w e" . browse-url)
	 ("C-c w f" . browse-url-firefox)
	 ("C-c w c" . browse-url-chromium)
	 ("C-c w g" . eww-search-words))
  :init
  (setq browse-url-browser-function 'eww-browse-url)
  (setq browse-url-generic-program "chromium")
  (setq shr-color-visible-luminance-min 80)
  (setq shr-use-colors nil)
  (setq shr-use-fonts nil)
  :config
  (setq eww-search-prefix "https://www.google.com/search?q="))

(use-package ace-link
  :ensure t
  :commands (ace-link)
  :bind (("C-c t f" . ace-link))
  :config
  (ace-link-setup-default))

(use-package helm-ag
  :ensure t
  :ensure-system-package ((ag . the_silver_searcher))
  :bind (("C-c a S" . helm-ag)
	 ("C-c a B" . helm-ag-buffers)
	 ("C-c a P" . helm-ag-project-root)
	 ("C-c a b" . helm-do-ag-buffers)
	 ("C-c a p" . helm-do-ag-project-root)
	 ("C-c a s" . helm-do-ag)
	 ("C-c h s" . helm-do-ag)))

(use-package ripgrep
  :ensure t
  :commands (projectile-ripgrep ripgrep-regexp)
  :ensure-system-package ((rg . ripgrep))
  :bind (("C-c r g" . tychoish-rg)
	 ("C-c r r" . tychoish-rg-repo)
	 ("C-c r m" . tychoish-find-merges))
  :config
  (defun tychoish-rg (regexp)
    (interactive (list (read-from-minibuffer "ripgrep for: " (thing-at-point 'symbol))))
    (ripgrep-regexp regexp default-directory))
  (defun tychoish-rg-repo (regexp)
    (interactive (list (read-from-minibuffer "ripgrep for: " (thing-at-point 'symbol))))
    (ripgrep-regexp regexp (projectile-project-root)))
  (defun tychoish-find-merges ()
    (interactive)
    (ripgrep-regexp "^(=======$|<<<<<<<|>>>>>>>)" (projectile-project-root))))

(use-package helm-rg
  :ensure t
  :ensure-system-package ((rg . ripgrep))
  :bind (("C-c r s" . helm-rg)
	 ("C-c r p" . helm-projectile-rg))
  :config
  (set-face-attribute 'helm-rg-active-arg-face nil :foreground "dim gray")
  (set-face-attribute 'helm-rg-title-face nil :foreground "dark blue" :background nil)
  (set-face-attribute 'helm-rg-colon-separator-ripgrep-output-face nil :foreground "dim gray")
  (set-face-attribute 'helm-rg-directory-cmd-face nil :foreground "dark blue" :background nil :weight 'normal)
  (set-face-attribute 'helm-rg-directory-header-face nil :foreground "dark blue" :background nil)
  (set-face-attribute 'helm-rg-extra-arg-face nil :foreground " orange" :weight 'normal)
  (set-face-attribute 'helm-rg-file-match-face nil :foreground "dark gray" :underline t)
  (set-face-attribute 'helm-rg-inactive-arg-face nil :foreground "dim gray" :weight 'normal)
  (set-face-attribute 'helm-rg-base-rg-cmd-face nil :foreground "dim gray" :weight 'normal)
  (set-face-attribute 'helm-rg-error-message nil :foreground "dark red" :background "nil" :weight 'normal))

(use-package helm-c-yasnippet
  :ensure t
  :after (yasnippet helm)
  :init
  (setq helm-yas-space-match-any-greedy t))

(use-package compile
  :functions (tychoish-uniq-compile-buffer)
  :commands (tychoish-compile-project)
  :bind (("C-c t c" . tychoish-compile-project)
	 ("C-c C-t c" . compile))
  :config
  (setq compilation-ask-about-save nil)
  (setq compilation-scroll-output t)

  (defun tychoish-compile-project ()
    (interactive)
    (let* ((project-directory (if (eq "" (projectile-project-root))
				  (default-directory)
				(projectile-project-root)))
	   (project-name (if (eq "" (projectile-project-name))
			     (file-name-nondirectory (s-chop-suffix "/" project-directory))
			   (projectile-project-name)))
	   (project-compile-buffer (concat "*" project-name "-build" "*")))

      (if (get-buffer project-compile-buffer)
	  (switch-to-buffer-other-window (get-buffer project-compile-buffer))
	(progn
	  (let ((default-directory project-directory))
	    (compile "time make -k build"))
	  (switch-to-buffer-other-window "*compilation*")
	  (rename-buffer project-compile-buffer)))))

  (defun tychoish-uniq-compile-buffer (compile-buffer-name &optional cmd)
    (if (get-buffer compile-buffer-name)
	(progn
	  (switch-to-buffer-other-window (get-buffer compile-buffer-name))
	  (recompile))
      (progn
	(if cmd
	    (compile cmd)
	  (compile))
	(switch-to-buffer-other-window "*compilation*")
	(rename-buffer compile-buffer-name)
	nil))))

(use-package projectile
  :ensure t
  :after (f)
  :delight '(:eval (tychoish-projectile-modeline-string))
  :bind-keymap ("C-c p" . projectile-command-map)
  :commands (projectile-mode projectile-project-root)
  :defer 1
  :config
  (setq projectile-known-projects-file (f-join user-emacs-directory (tychoish-get-config-file-prefix "projectile-bookmarks")))
  (defun tychoish-projectile-modeline-string ()
    (let ((pname (projectile-project-name)))
      (if (equal pname "-")
	  ""
	(concat " p:" pname))))

  (setq projectile-enable-caching t)
  (setq projectile-use-git-grep 1)
  (setq projectile-completion-system 'helm)
  (setq projectile-require-project-root 'prompt)
  (projectile-mode +1))

(use-package helm-projectile
  :ensure t
  :after (projectile helm)
  :bind (("M-p" . helm-projectile)
	 ("C-c g s" . helm-projectile-grep))
  :config
  (helm-projectile-on))

(use-package go-projectile
  :ensure t
  :after (go-mode projectile))

(use-package helm-dash
  :ensure t
  :commands (helm-dash helm-dash-at-point helm-dash-install-docset helm-dash-activate-docset))

(use-package helm-company
  :ensure t
  :after (helm company)
  :bind (("C-c C-," . helm-company))
  :commands (helm-company)
  :init
  (define-key company-mode-map (kbd "C-,") 'helm-company)
  (define-key company-active-map (kbd "C-,") 'helm-company))

(use-package helm-flycheck
  :ensure t
  :after (helm flycheck)
  :config (define-key flycheck-mode-map (kbd "C-c f h") 'helm-flycheck))

(use-package wgrep
  :ensure t
  :bind (:map grep-mode-map
	 ("C-x C-q" . wgrep-change-to-wgrep-mode))
  :config
  (setq wgrep-enable-key "r"))

(use-package page-break-lines
  :ensure t
  :diminish page-break-lines-mode
  :hook ((text-mode prog-mode) . page-break-lines-mode)
  :commands (global-page-break-lines-mode)
  :config
  (setq page-break-lines-modes '(emacs-lisp-mode
				 lisp-mode
				 scheme-mode
				 help-mode
				 c-mode
				 cc-mode
				 eww-mode
				 go-mode
				 special-mode)))

(use-package writeroom-mode
  :ensure t
  :commands (writeroom-mode))

(use-package revbufs
  :ensure t
  :commands (revbufs)
  :bind (("C-c C-g" . revbufs)))

(use-package shell-pop
  :ensure t
  :commands (shell-pop)
  :bind (("<f9>" . shell-pop))
  :config
  (setq shell-pop-universal-key "<f9>")
  (setq shell-pop-window-position "top")
  (setq shell-pop-window-size 25)
  (setq shell-pop-window-size 25)
  (setq shell-pop-autocd-to-working-dir t)
  (setq shell-pop-cleanup-buffer-at-process-exit t))

(use-package session
  :ensure t
  :commands (session-initialize)
  :bind (("C-c t ;" . session-toggle-permanent-flag))
  :after (tychoish-bootstrap)
  :config
  (setq session-save-file (tychoish-get-config-file-path "session"))

  ;; use session-save to save the desktop manually
  (defun tychoish-save-session ()
    "Save an emacs session... sometimes"
    (interactive)

    (if (> 30 (random 100))
	(session-save-session t)))

  (add-hook 'after-save-hook 'tychoish-save-desktop)
  (setq session-save-print-spec '(t nil 40000)))

(use-package winner
  :defer 2
  :commands (winner-mode)
  :init
  (winner-mode 1))

(use-package emacs-everywhere
  :ensure t
  :commands (emacs-everywhere))

(use-package desktop
  :commands (desktop-save-mode desktop-read tychoish-save-desktop)
  :after (tychoish-bootstrap)
  :config
  (setq desktop-base-file-name (tychoish-get-config-file-prefix "desktop-file"))
  (setq desktop-base-lock-name (tychoish-get-config-file-prefix "desktop-lock"))

  ;; use session-save to save the desktop manually
  (defun tychoish-save-desktop ()
    "Save an emacs session... sometimes"
    (interactive)

    (if (> 50 (random 100))
	(desktop-save desktop-dirname)))

  (add-hook 'after-save-hook 'tychoish-save-desktop)
  (setq ad-redefinition-action 'accept)
  (defun emacs-process-p (pid)
    "If pid is the process ID of an emacs process, return t, else nil. Also returns nil if pid is nil."
    (when pid
      (let* ((cmdline-file (concat "/proc/" (int-to-string pid) "/cmdline")))
	(when (file-exists-p cmdline-file)
	  (with-temp-buffer
	    (insert-file-contents-literally cmdline-file)
	    (goto-char (point-min))
	    (search-forward "emacs" nil t)
	    pid)))))

  (defadvice desktop-owner (after pry-from-cold-dead-hands activate)
    "Don't allow dead emacsen to own the desktop file."
    (when (not (emacs-process-p ad-return-value))
      (setq ad-return-value nil)))

  (defadvice desktop-save (around stfu compile activate)
    (cl-flet ((yes-or-no-p (&rest args) t)
	      (y-or-n-p (&rest args) t))
      ad-do-it))

  ;; use session-restore to restore the desktop manually
  (defun session-restore ()
    "Restore a saved emacs session."
    (interactive)
    (if (file-exists-p (concat desktop-dirname "/" desktop-base-file-name))
	(desktop-read)
      (message "No desktop found.")))

  (setq desktop-path (list user-emacs-directory))
  (setq desktop-enable t)
  (setq desktop-dirname user-emacs-directory)
  (setq desktop-restore-frames nil)
  (setq desktop-buffers-not-to-save
	(concat "\\("
		"^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
		"\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
		"\\)$"))

  (add-to-list 'desktop-globals-to-save 'register-alist)
  (add-to-list 'desktop-globals-to-save 'file-name-history)
  (add-to-list 'desktop-modes-not-to-save 'dired-mode)
  (add-to-list 'desktop-modes-not-to-save 'Info-mode)
  (add-to-list 'desktop-modes-not-to-save 'org-mode)
  (add-to-list 'desktop-modes-not-to-save 'eww-mode)
  (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
  (add-to-list 'desktop-modes-not-to-save 'fundamental-mode))

(use-package recentf
  :commands (recentf-mode)
  :after (tychoish-bootstrap)
  :config
  (setq recentf-auto-cleanup 'never)
  (setq recentf-keep '(file-remote-p file-readable-p))
  (setq recentf-max-menu-items 50)
  (setq recentf-save-file (tychoish-get-config-file-path "recentf")))

(use-package magit
  :ensure t
  :commands (magit-toplevel)
  :bind (("C-x g s" . magit-status)
	 ("C-x g b" . magit-branch-manager)
	 ("C-x g o b" . magit-blame))
  :init
  (setq version-control t)
  (setq vc-follow-symlinks t)
  (setq vc-handled-backends nil)
  :config
  (setq magit-auto-revert-mode nil)
  (add-to-list 'magit-status-sections-hook 'magit-insert-modules t)
  (setq magit-module-sections-nested nil))

(use-package magithub
  :ensure t
  :after magit)

(use-package gist
  :ensure t
  :commands (gist-region gist-buffer gist-mode gist-region-private gist-buffer-private))

(use-package github-review
  :ensure t
  :commands (github-review-start
	     github-review-forge-pr-at-point
	     github-review-approve
	     github-review-reject))

(use-package forge
  :ensure t
  :after (magit))

(use-package yasnippet
  :ensure t
  :diminish "ys"
  :commands (yas-global-mode)
  :hook ((text-mode prog-mode) . yas-minor-mode)
  :config
  (add-to-list 'load-path (f-join user-emacs-directory "snippets"))
  (setq yas-prompt-functions '(helm-yas-complete  yas-dropdown-prompt yas-ido-prompt))

  (diminish 'yas-minor-mode "ys")

  (add-hook 'org-mode-hook (lambda ()
			     (make-variable-buffer-local 'yas-trigger-key)
			     (setq yas-trigger-key [tab])
			     (define-key yas-keymap [tab] 'yas-next-field)))
  (yas-reload-all))


(use-package helm-c-yasnippet
  :after yasnippet
  :ensure t
  :bind (("C-c s y" . 'helm-yas-complete)
	 ("C-c C-y" . 'helm-yas-complete)))

(use-package yasnippet-snippets
  :after yasnippet
  :ensure t)

(use-package company
  :ensure t
  :delight
  :bind (("C-c ." . company-complete)
	 ("C-c C-." . company-complete)
	 ("C-c s s" . company-yasnippet)
	 :map company-active-map
	 ("C-n" . company-select-next)
	 ("C-p" . company-select-previous)
	 ("C-d" . company-show-doc-buffer)
	 ("M-." . company-show-location))
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  (add-hook 'text-mode-hook 'company-mode)
  :config
  (eval-after-load 'c-mode
    '(define-key c-mode-map (kbd "[tab]") 'company-complete))

  (setq company-tooltip-limit 20)
  (setq company-show-numbers t)
  (setq company-dabbrev-downcase nil)
  (setq company-idle-delay 0)
  (setq company-echo-delay 0)
  (setq company-ispell-dictionary (expand-file-name "~/.aspell.en.pws"))

  (setq company-backends '(company-capf
			   company-keywords
			   company-semantic
			   company-files
			   company-etags
			   company-elisp
			   company-emoji
			   company-clang
			   company-irony-c-headers
			   company-irony
			   company-jedi
			   company-cmake
			   company-yasnippet))

  (global-company-mode))

(use-package company-irony
  :ensure t
  :after (company irony)
  :commands (company-irony)
  :config
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands))

(use-package company-irony-c-headers
  :ensure t
  :commands (company-irony-c-headers)
  :after company-irony)

(use-package company-jedi
  :ensure t
  :commands (company-jedi)
  :after (company python-mode))

(use-package company-quickhelp
  :ensure t
  :after company
  :config
  (setq company-quickhelp-idle-delay 0.1)
  (company-quickhelp-mode 1))

(use-package company-emoji
  :ensure t
  :after company)

(use-package company-wordfreq
  :ensure t
  :after company
  :init
  (defun setup-local-word-complete ()
    (setq-local company-backends '(company-wordfreq))
    (setq-local company-idle-delay 0.15)
    (setq-local company-echo-delay 0.15)
    (setq-local company-transformers nil))

  (add-hook 'markdown-mode-hook 'setup-local-word-complete)
  (add-hook 'rst-mode-hook 'setup-local-word-complete))

(use-package irony
  :ensure t
  :commands (irony-mode)
  :bind (:map irony-mode-map
	 ([remap completion-at-point] . irony-completion-at-point-async)
	 ([remap complete-symbol] . 'irony-completion-at-point-async))
  :init
  (add-hook 'c-mode-common-hook 'irony-mode)
  :config
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options))

(use-package irony-eldoc
  :ensure t
  :after irony
  :config
  (add-hook 'irony-mode-hook 'irony-eldoc))

(use-package flycheck
  :ensure t
  :diminish (flycheck-mode . "fc")
  :bind (("C-c f f" . flycheck-mode))
  :init
  (add-hook 'c-mode-common-hook 'flycheck-mode)
  (add-hook 'emacs-lisp-mode-hook 'flycheck-mode)
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11")))
  :config
  ;; the order of the following 3 operations is important.
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (setq flycheck-keymap-prefix (kbd "C-c f"))
  (define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map)

  (setq flycheck-check-syntax-automatically '(save new-line idle-change idle-buffer-switch))
  (setq flycheck-idle-change-delay 3)
  (setq flycheck-idle-buffer-switch-delay 2)
  (setq flycheck-checker-error-threshold nil)
  (setq flycheck-flake8-maximum-line-length 100)
  (setq flycheck-go-vet-shadow t)
  (setq flycheck-go-vet-print-functions t))

(use-package flycheck-aspell
  :after (flycheck)
  :ensure t
  :config
  (flycheck-aspell-define-checker "cpp"
    "C++" ("--add-filter" "url" "--add-filter" "ccpp")
    (c++-mode)))

(use-package flycheck-golangci-lint
  :ensure t
  :hook (go-mode . flycheck-golangci-lint-setup)
  :after (go-mode flycheck)
  :config
  (setq flycheck-golangci-lint-fast nil)
  (setq flycheck-golangci-lint-tests t))

(use-package flycheck-irony
  :after (flycheck irony)
  :ensure t
  :commands (flycheck-irony-setup)
  :init (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)
  :config
  (flycheck-irony-setup))

(use-package go-mode
  :ensure t
  :delight "go"
  :bind (:map go-mode-map
	      ("M-." . godef-jump))
  :mode (("\\.go$" . go-mode)
	 ("\\.go" . go-mode)
	 ("\\.go\\'" . go-mode))
  :config
  (unless (getenv "GOPATH")
    (setenv "GOPATH" (expand-file-name "~/goprojects")))

  (setq local-go-bin (concat (getenv "GOPATH") "/bin"))
  (setq exec-path (cons local-go-bin exec-path))
  (setenv "PATH" (format "%s:%s" local-go-bin (getenv "PATH")))
  (setenv "GO111MODULE" "auto")
  (add-to-list 'exec-path local-go-bin)

  (add-hook 'go-mode-hook (lambda ()
			    (flyspell-prog-mode)
			    (setq-local comment-auto-fill-only-comments t)
			    (auto-fill-mode 1)))

  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'flycheck-mode)
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
	   "go build -v && go test -v && go vet")))

(use-package go-guru
  :ensure t
  :after go-mode
  :config
  (add-hook 'go-mode-hook #'go-guru-hl-identifier-mode))

(use-package go-eldoc
  :ensure t
  :after go-mode
  :config
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package go-complete
  :ensure t
  :after (go-mode company)
  :ensure-system-package ((gocode . "go install github.com/mdempsky/gocode"))
  :config
  (add-hook 'completion-at-point-functions 'go-complete-at-point))

(use-package helm-go-package
  :ensure t
  :after (go-mode helm)
  :bind (("C-c h g" .'helm-go-package))
  :config
  (substitute-key-definition 'go-import-add 'helm-go-package go-mode-map))

(use-package protobuf-mode
  :ensure t
  :mode "\\.proto$'")

(use-package dockerfile-mode
  :ensure t
  :delight "dckr"
  :mode "Dockerfile")

(use-package cmake-mode
  :ensure t
  :mode "CMakeLists.txt")

(use-package cmake-project
  :ensure t
  :after (cmake-mode)
  :init
  (defun tychoish-cmake-project-p ()
    (let* ((project-directory (if (eq "" (projectile-project-root))
				  (default-directory)
				(projectile-project-root)))
	   (f-exists? (f-join project-directory "CMakeLists.txt")))))

  (add-hook 'c-mode-common-hook  'tychoish-cmake-project-p)
  :config)

(use-package clang-format
  :ensure t
  :after (c++-mode)
  :commands (clang-format clang-format-buffer clang-format-region)
  :bind (([C-M-tab] . clang-format-region))
  :init
  (defun clang-format-before-save ()
    (interactive)
    (when (or (eq major-mode 'c++-mode)
	      (eq major-mode 'c-mode))
      (clang-format-buffer)))

  (add-hook 'before-save-hook 'clang-format-before-save)
  :config
  (setq clang-format-style "Google"))

(use-package c-eldoc
  :ensure t
  :commands (c-turn-on-eldoc-mode)
  :init
  (add-hook 'c-mode-common-hook 'c-turn-on-eldoc-mode)
  (setq c-eldoc-buffer-regenerate-time 60))

(use-package cc-mode
  :mode (("\\.cc$'" . c++-mode)
	 ("\\.cpp$'" . c++-mode)
	 ("\\.cxx$'" . c++-mode)
	 ("\\.h$'" . c++-mode)))

(use-package cpputils-cmake
  :ensure t
  :after (cmake-mode c++-mode c-mode)
  :bind (("C-c C-c C-g" . (lambda ()(interactive) (gud-gdb (concat "gdb --fullname " (cppcm-get-exe-path-current-buffer))))))
  :config
  (add-hook 'c-mode-common-hook
	    (lambda ()
	      (if (derived-mode-p 'c-mode 'c++-mode)
		  (cppcm-reload-all)
		)))
  (add-hook 'c90-mode-hook (lambda () (cppcm-reload-all))))

(use-package modern-cpp-font-lock
  :ensure t
  :after (c++-mode)
  :commands (modern-c++-font-lock-mode)
  :init
  (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)
  (add-hook 'c++-mode-hook (lambda () (setq show-trailing-whitespace t)))
  (font-lock-add-keywords 'c++-mode (font-lock-width-keyword 100))
  (font-lock-add-keywords 'c-mode (font-lock-width-keyword 100)))

(use-package make-mode
  :mode (("makefile" . makefile-mode)
	 ("Makefile" . makefile-mode)
	 ("\\.mk%" . makefile-mode))
  :config
  (setq makefile-electric-keys t))

(use-package js2-mode
  :ensure t
  :mode ("\\.js$" "\\.json$")
  :after (tychoish-editing)
  :init
  (font-lock-add-keywords 'javascript-mode (font-lock-show-tabs))
  (font-lock-add-keywords 'javascript-mode (font-lock-width-keyword 100))
  :config
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode))

(use-package js2-refactor
  :ensure t
  :after (js2-mode)
  :config
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-r")
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill))

(use-package typescript-mode
  :ensure t
  :commands (typescript-mode)
  :mode ("\\.tsx?\\'" "\\.ts?\\'"))

(use-package xref-js2
  :ensure t
  :after (js2-mode js2-refactor)
  :init
  (define-key js-mode-map (kbd "M-.") nil)
  :config
  (add-hook 'js2-mode-hook (lambda () (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))

(use-package ctags-update
  :ensure t
  :bind (("C-c E" . ctags-update))
  :commands (turn-on-ctags-auto-update-mode create-tags)
  :diminish
  :config
  (setq tags-add-tables nil)
  (setq etags-table-search-up-depth 10)
  (setq path-to-ctags (executable-find "ctags"))
  (setq ctags-update-delay-seconds 300)

  (defun create-tags (dir-name)
    "Create tags file for the DIR-NAME directory."
    (interactive "DDirectory: ")
    (let ((cmd-str (format "%s -e -u -f %s/TAGS %s -R %s" path-to-ctags dir-name dir-name (directory-file-name dir-name))))
      (message cmd-str)
      (shell-command cmd-str)))
  (diminish 'ctags-auto-update-mode)
  (add-hook 'c-mode-common-hook  'turn-on-ctags-auto-update-mode)
  (add-hook 'emacs-lisp-mode-hook  'turn-on-ctags-auto-update-mode))

(use-package git-link
  :ensure t
  :bind (("C-c g l" . git-link)))

(use-package cython-mode
  :ensure t
  :mode (("\\.pyx\\'" . cython-mode)
	 ("\\.pxd\\'" . cython-mode)
	 ("\\.pxi\\'" . cython-mode)))

(use-package python-mode
  :ensure t
  :delight "py"
  :mode ("\\.py\\'" "\\.py3\\'" "SConstruct" "SConscript")
  :after (tychoish-editing)
  :bind (:map python-mode-map
	      ("M-<right>" . balle-python-shift-right)
	      ("M-<left>" . balle-python-shift-left)
	      ([remap completion-at-point] . company-complete)
	      ([tab] . company-complete)
	      ("C-m" . py-newline-and-indent)
	      ("C-c C-." . company-complete)
	      ("C-c ." . company-complete))
  :init
  (setq ad-redefinition-action 'accept)

  (defun balle-python-shift-left ()
    (interactive)
    (let (start end bds)
      (if (and transient-mark-mode
	       mark-active)
	  (setq start (region-beginning) end (region-end))
	(progn
	  (setq bds (bounds-of-thing-at-point 'line))
	  (setq start (car bds) end (cdr bds))))
      (python-indent-shift-left start end))
    (setq deactivate-mark nil))

  (defun balle-python-shift-right ()
    (interactive)
    (let (start end bds)
      (if (and transient-mark-mode
	       mark-active)
	  (setq start (region-beginning) end (region-end))
	(progn
	  (setq bds (bounds-of-thing-at-point 'line))
	  (setq start (car bds) end (cdr bds))))
      (python-indent-shift-right start end))
    (setq deactivate-mark nil))
  :config
  (add-hook 'python-mode-hook (lambda ()
				(flyspell-prog-mode)
				(define-key python-mode-map "'" 'tychoish-electric-pair)
				(set-fill-column 95)))

  (setq python-indent-offset 4)

  (if (eq system-type 'darwin)
      (setq py-python-command "/usr/bin/python"))
  (if (eq system-type 'gnu/linux)
      (setq py-python-command "/usr/bin/python2"))

  (font-lock-add-keywords 'python-mode (font-lock-show-tabs))
  (font-lock-add-keywords 'python-mode (font-lock-width-keyword 100)))

(use-package flycheck-pyflakes
  :ensure t
  :after (python-mode)
  :config
  (add-hook 'python-mode-hook 'flycheck-mode)
  (add-to-list 'flycheck-disabled-checkers 'python-flake8)
  (add-to-list 'flycheck-disabled-checkers 'python-pylint))

(use-package py-autopep8
  :ensure t
  :after (python-mode)
  :config
  (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
  (setq py-autopep8-options '("--max-line-length=100")))

(use-package virtualenvwrapper
  :ensure t
  :after (python-mode)
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell))

(use-package web-mode
  :ensure t
  :mode (("\\.html$" . web-mode)
	 ("\\.htm\\'". web-mode)
	 ("\\.html\\'". web-mode)
	 ("\\.as[cp]x\\'" . web-mode)
	 ("\\.djhtml\\'" . web-mode)
	 ("\\.erb" . web-mode)
	 ("\\.jsp\\'" . web-mode)
	 ("\\.mustache\\'" . web-mode)
	 ("\\.phtml\\'" . web-mode)
	 ("\\.tpl\\.php\\'" . web-mode))
  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq js-indent-level 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-expanding t)
  (setq web-mode-enable-css-colorization t)
  (add-hook 'web-mode-hook 'electric-pair-mode))

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

(use-package json-reformat
  :ensure t
  :after (json-mode))

(use-package json-snatcher
  :ensure t
  :after (json-mode))

(use-package jinja2-mode
  :ensure t
  :mode "\\.jinja\\'")

(use-package yaml-mode
  :ensure t
  :mode (("\\.yaml\\'" . yaml-mode)
	 ("\\.yml\\'" . yaml-mode)
	 ("\\.yaml$" . yaml-mode)
	 ("\\.lock$" . yaml-mode)
	 ("\\.yml$" . yaml-mode))
  :init
  (add-hook 'yaml-mode-hook 'flyspell-mode))

(use-package ninja-mode
  :ensure t
  :mode "\\.ninja\\'")

(use-package compile
  :commands (compile)
  :init
  (defun colorize-compilation-buffer ()
    (toggle-read-only)
    (ansi-color-apply-on-region compilation-filter-start (point))
    (toggle-read-only))

  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
  :config
  (define-key compilation-mode-map (kbd "C") 'compile))

(use-package slime
  :ensure t
  :after (f tychoish-bootstrap)
  :commands (slime)
  :bind (("C-c h l" . hyperspec-lookup))
  :config
  (setq ls-lisp-dirs-first t)
  (setq quicklisp-path (expand-file-name "~/quicklisp"))
  (setq inferior-lisp-program "sbcl")
  (add-to-list 'load-path quicklisp-path)

  (defun load-quicklisp-file (fn)
    (let ((path (f-join quicklisp-path fn))
	  (inhibit-message t))
	(when (f-exists-p path)
	  (with-slow-op-timer (format "loading: %s" path) .5
	    (load (expand-file-name path) t t t)))))

  (load-quicklisp-file "slime-helper.el")
  (load-quicklisp-file "clhs-use-local.el")
  (load-quicklisp-file "log4slime-setup.el")

  (setq slime-contribs '(slime-scratch slime-editing-commands slime-fancy slime-company))

  (delight 'lisp-mode "lisp")
  (delight 'lisp-interaction-mode "li")
  (diminish 'slime-autodoc-mode)
  (diminish 'slime-mode "sl"))

(use-package slime-company
  :ensure t
  :after (slime)
  :commands (slime-company))

(use-package common-lisp-snippets
  :after (slime))

(use-package helm-slime
  :ensure t
  :after (slime helm)
  :config
  (global-helm-slime-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; org-mode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :mode (("\\.org$" . org-mode))
  :delight "org"
  :commands (tychoish-add-org-capture-template org-save-all-org-buffers)
  :ensure org-plus-contrib
  :bind (("C-c o a" . org-agenda)
	 ("C-c o l s" . org-store-link)
	 ("C-c o l i" . org-insert-link)
	 ("C-c o j" . org-capture)
	 ("C-c o c" . org-capture)
	 ("C-c o k l" . org-capture-goto-last-stored)
	 ("C-c o k t" . org-capture-goto-target)
	 ("C-c o k w" . org-capture-refile)
	 ("C-c o l a" . org-annotate-file)
	 ("C-c o f o" . (lambda () (interactive) (find-file (concat org-directory "/organizer.org"))))
	 :map org-mode-map
	      ("C-c o w" . org-refile)
	      ("C-c o l o". org-open-link-from-string)
	      ("C-c o l a o" . org-agenda-open-link)
	      ("C-c o t" . org-set-tags-command)
	      ("C-c o p" . org-insert-property-drawer)
	      ("C-c o d" . org-date)
	      ("C-c o s" . org-archive-subtree)
	      ("C-c o f" . org-archive-set-tag)
	      ("C-c o n" . org-narrow-to-subtree)
	      ("C-c o b t" . org-ctags-create-tags)
	      ("M-TAB" . org-cycle)
	      ("C-c o r c" . org-bibtex-create)
	      ("C-c o r r" . org-bibtex-create-in-current-entry)
	      ("C-c o r k" . org-bibtex-export-to-kill-ring)
	      ("C-c o r v v" . org-bibtex-check)
	      ("C-c o r v a" . org-bibtex-check-all)
	      ("C-c o r s" . org-bibtex-search)
	      ("C-c o r e" . org-bibtex)
	      ("C-c o h p" . outline-previous-visable-heading)
	      ("C-c C-p" . set-mark-command))
  :init
  (setq org-directory (concat local-notes-directory "/org"))
  (setq org-agenda-files (list org-directory))

  (defvar org-odt-data-dir "~/.emacs.d/org/etc")

  (defun org-set-weekday-of-timestamp ()
    "Check if cursor is within a timestamp and compute weekday from numeric date"
    (interactive)
    (when (org-at-timestamp-p t)
      (org-timestamp-change 0 'year)
      (message "Weekday of timestamp has been adjusted.")
      t))
  (setq org-capture-templates '())
  (defun tychoish-add-org-capture-template (prefix-key name)
    "Defines a set of capture mode templates for adding notes and tasks to a file."
    (let ((org-filename (concat org-directory "/" (downcase name) ".org")))

      (when prefix-key
	(add-to-list 'org-capture-templates `(,prefix-key ,name)))

      (add-to-list 'org-capture-templates `(,(concat prefix-key "r") "routines"))
      (add-to-list 'org-capture-templates `(,(concat prefix-key "rd") "Daily Routine"
					    entry (file+olp ,org-filename "Loops" "Daily")
					    "* %^{Title}\nSCHEDULED: <%(org-read-date nil nil \"+1d\")>\n%?" :prepend t))
      (add-to-list 'org-capture-templates `(,(concat prefix-key "rw") "Weekly Routine"
					    entry (file+olp ,org-filename "Loops" "Weekly")
					    "* %^{Title}\nSCHEDULED: <%(org-read-date nil nil \"+1w\")>\n%?" :prepend t))
      (add-to-list 'org-capture-templates `(,(concat prefix-key "rm") "Monthly Routine"
					    entry (file+olp ,org-filename "Loops" "Weekly")
					    "* %^{Title}\nSCHEDULED: <%(org-read-date nil nil \"+4w\")>\n%?" :prepend t))
      (add-to-list 'org-capture-templates `(,(concat prefix-key "rq") "Quarterly Routine"
					    entry (file+olp ,org-filename "Loops" "Quarterly")
					    "* %^{Title}\nSCHEDULED: <%(org-read-date nil nil \"+12w\")>\n%?" :prepend t))
      (add-to-list 'org-capture-templates `(,(concat prefix-key "ry") "Yearly Routine"
					    entry (file+olp ,org-filename "Loops" "Yearly")
					    "* %^{Title}\nSCHEDULED: <%(org-read-date nil nil \"+52w\")>\n%?" :prepend t))

      ;; journal, for date related content
      (add-to-list 'org-capture-templates `(,(concat prefix-key "l") "date journal"
					    entry (file+datetree ,org-filename "Journal")
					    "* %t\n%?" :prepend t))

      (add-to-list 'org-capture-templates `(,(concat prefix-key "n") "notes"))
      (add-to-list 'org-capture-templates `(,(concat prefix-key "nn") "basic notes"
					    entry (file+headline ,org-filename "Inbox")
					    "* %?" :prepend t))
      (add-to-list 'org-capture-templates `(,(concat prefix-key "nl") "notes (org-link)"
					    entry (file+headline ,org-filename "Inbox")
					    "* %?\n%a" :prepend t))
      (add-to-list 'org-capture-templates `(,(concat prefix-key "nk") "notes (kill buffer)"
					    entry (file+headline ,org-filename "Inbox")
					    "* %?\n%c" :prepend t))
      (add-to-list 'org-capture-templates `(,(concat prefix-key "nx") "notes (xbuffer)"
					    entry (file+headline ,org-filename "Inbox")
					    "* %?\n%x" :prepend t))
      (add-to-list 'org-capture-templates `(,(concat prefix-key "ns") "notes (selection)"
					    entry (file+headline ,org-filename "Inbox")
					    "* %?\n%i" :prepend t))

      (add-to-list 'org-capture-templates `(,(concat prefix-key "t") "tasks"))
      (add-to-list 'org-capture-templates `(,(concat prefix-key "tt") "basic tasks"
					    entry (file+headline ,org-filename "Tasks")
					    "* TODO %?" :prepend t))
      (add-to-list 'org-capture-templates `(,(concat prefix-key "tl") "tasks (org-link)"
					    entry (file+headline ,org-filename "Tasks")
					    "* TODO %?\n%a" :prepend t))
      (add-to-list 'org-capture-templates `(,(concat prefix-key "tk") "tasks (kill buffer)"
					    entry (file+headline ,org-filename "Tasks")
					    "* TODO %?\n%c" :prepend t))
      (add-to-list 'org-capture-templates `(,(concat prefix-key "tx") "tasks (xbuffer)"
					    entry (file+headline ,org-filename "Tasks")
					    "* TODO %?\n%x" :prepend t))
      (add-to-list 'org-capture-templates `(,(concat prefix-key "ts") "tasks (selection)"
					    entry (file+headline ,org-filename "Tasks")
					    "* TODO %?\n%i" :prepend t))))
  :config
  (diminish 'org-indent-mode)
  (diminish 'org-capture-mode)

  (delight 'org-agenda-mode "agenda")

  (defadvice org-capture-finalize
      (after delete-capture-frame activate)
    "Advise capture-finalize to close the frame"
    (when (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

  (defadvice org-capture-destroy
      (after delete-capture-frame activate)
    "Advise capture-destroy to close the frame"
    (when (equal "capture" (frame-parameter nil 'name))
	(delete-frame)))

  (defun make-capture-frame (&optional keys show-menu)
    "Create a new frame and run org-capture."
    (interactive)
    (make-frame '((name . "capture")
		  (width . 120)
		  (height . 15)))
    (select-frame-by-name "capture")
    (setq word-wrap 1)
    (setq truncate-lines nil)
    ;; Using the second argument to org-capture, we bypass interactive selection
    ;; and use an existing template, unless you pass t rather than a template trigger
    (cond
     ((stringp keys)
      (org-capture nil keys))
     ((or show-menu (not keys))
      (org-capture))
     (t
      (org-capture nil "tt"))))

  (defun org-agenda-files-reload ()
    "reloads all agenda files"
    (interactive)
    (setq org-agenda-files (--remove (s-matches? "archive.org$" it) (f-glob "*.org" org-directory))))

  (defadvice org-capture-finalize (after delete-capture-frame activate)
    "Advise capture-finalize to close the frame if it is the capture frame"
    (if (equal "capture" (frame-parameter nil 'name))
	(delete-frame)))

  (delight 'org-agenda)
  (diminish 'org-indent)

  (add-hook 'org-ctrl-c-ctrl-c-hook 'org-set-weekday-of-timestamp)
  (add-hook 'org-agenda-mode-hook 'revbufs)
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)
  (add-hook 'org-mode-hook 'flyspell-mode)
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook 'visual-line-mode)

  (org-load-modules-maybe t)

  (add-to-list 'org-speed-commands-user '("N" org-narrow-to-subtree))
  (add-to-list 'org-speed-commands-user '("W" widen))

  (setq org-agenda-include-all-todo t)
  (setq auto-mode-alist (cons '("\\.org$" . org-mode) auto-mode-alist))
  (setq org-archive-location (concat org-directory "/archive.org::datetree/"))
  (setq org-default-notes-file (concat org-directory "/organizer.org"))
  (setq org-annotate-file-storage-file (concat org-directory "/annotations.org"))

  (setq org-modules '(org-velocity
		      org-notify
		      org-depend
		      org-man
		      org-annotate-file
		      org-datetree
		      org-habit
		      org-ctags
		      org-info))

  (setq org-agenda-custom-commands
	'(("b" "Backlog" tags-todo "+backlog|+inbox|TODO=BLOCKED")
	  ("g" "Groomed" tags-todo "-backlog|+groomed")
	  ("c" "Super view"
	   ((agenda "" ((org-agenda-overriding-header "")
			(org-super-agenda-groups
			 '((:name "Today"
				  :time-grid t
				  :date today
				  :order 1)))))
	    (alltodo "" ((org-agenda-overriding-header "")
			 (org-super-agenda-groups
			  '((:log t)
			    (:name "To refile"
				   :file-path (concat tychoish-org-fn-main "/refile"))
			    (:name "Today's tasks"
				   :file-path "journal/")
			    (:name "Due Today"
				   :deadline today
				   :order 2)
			    (:name "Scheduled Soon"
				   :scheduled future
				   :order 8)
			    (:name "Overdue"
				   :deadline past
				   :order 7)
			    (:discard (:not (:todo "TODO")))))))))))

  (setq org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d)")
			    (sequence "BLOCKED(s)" "BACKLOG(p)" "|" "GONEAWAY(g)" "TRASH(c)")
			    (sequence "INPROGRESS(w)" "|" "INCOMPLETE(i)")))

  (setq org-todo-keyword-faces '(("TODO" . org-warning)
				 ("INPROGRESS" . "orange")
				 ("INCOMPLETE" . "orange")
				 ("SCHEDULED" . "green")
				 ("BACKLOG" . (:foreground "orange" :weight bold))
				 ("PROJECT" . (:foreground "blue" :weight bold))))

  (setq org-tag-alist '((:startgroup . nil)
			  ("inbox" . ?i)
			  ("backlog" . ?b)
			  ("groomed" . ?g)
			(:endgroup . nil)
			(:startgroup . nil)
			  ("@desk" . ?d)
			  ("@personal" . ?p)
			  ("@work" . ?w)
			(:endgroup . nil)))

  (add-hook 'org-mode-hook 'flyspell-mode)
  (setq org-CUA-compatible t)
  (setq org-agenda-block-separator nil)
  (setq org-agenda-columns-add-appointments-to-effort-sum t)
  (setq org-agenda-compact-blocks t)
  (setq org-agenda-default-appointment-duration 60)
  (setq org-agenda-dim-blocked-tasks 'invisible)
  (setq org-agenda-include-deadlines t)
  (setq org-agenda-include-diary nil)
  (setq org-agenda-inhibit-startup nil)
  (setq org-agenda-mouse-1-follows-link t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-scheduled-if-deadline-is-shown nil)
  (setq org-agenda-skip-unavailable-files t)
  (setq org-agenda-span 1)
  (setq org-agenda-start-day nil) ;; i.e. today
  (setq org-agenda-start-on-weekday nil)
  (setq org-agenda-todo-ignore-deadlines nil)
  (setq org-agenda-todo-ignore-scheduled nil)
  (setq org-agenda-todo-ignore-with-date nil)
  (setq org-agenda-use-time-grid nil)
  (setq org-drawers '("PROPERTIES" "CLOCK" "LOGBOOK" "CITATION" "NOTES"))
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-enforce-todo-dependencies t)
  (setq org-export-creator-string nil)
  (setq org-export-email-info t)
  (setq org-export-headline-levels 5)
  (setq org-export-with-planning nil)
  (setq org-export-with-priority nil)
  (setq org-export-with-properties nil)
  (setq org-export-with-section-numbers nil)
  (setq org-export-with-smart-quotes nil)
  (setq org-export-with-toc nil)
  (setq org-fast-tag-selection-include-todo t)
  (setq org-fontify-done-headline t)
  (setq org-footnote-auto-label nil)
  (setq org-footnote-define-inline nil)
  (setq org-footnote-section nil)
  (setq org-goto-interface 'outline-path-completion)
  (setq org-hide-leading-stars t)
  (setq org-outline-path-complete-in-steps t)
  (setq org-provide-todo-statistics t)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-targets '((org-agenda-files :maxlevel . 4)))
  (setq org-refile-use-outline-path 'file)
  (setq org-replace-disputed-keys t)
  (setq org-return-follows-link t)
  (setq org-reverse-note-order t)
  (setq org-startup-folded "folded")
  (setq org-startup-indented t)
  (setq org-tags-exclude-from-inheritance '("project"))
  (setq org-track-ordered-property-with-tag t)
  (setq org-use-fast-tag-selection t)
  (setq org-use-fast-todo-selection t)
  (setq org-use-speed-commands (lambda () (and (looking-at org-outline-regexp) (looking-back "^\**"))))
  (setq org-agenda-skip-scheduled-if-done t))

(use-package org-mu4e
  :after (org mu4e))

(use-package ox-rst
  :ensure t
  :after (org)
  :commands (org-rst-export-to-rst org-rst-export-as-rst)
  :config
  (setq org-rst-headline-underline-characters (list ?= ?- ?~ ?' ?^ ?`)))

(use-package ox-leanpub
  :ensure t
  :after (org)
  :config
  (require 'ox-leanpub-markua)
  (org-leanpub-book-setup-menu-markua))

(use-package helm-org
  :ensure t
  :bind (("C-c h o c" . helm-org-capture-templates)
	 ("C-c h o s" . helm-org-agenda-file-headings))
  :config
  (add-to-list 'helm-completing-read-handlers-alist '(org-capture . helm-org-completing-read-tags))
  (add-to-list 'helm-completing-read-handlers-alist '(org-set-tags . helm-org-completing-read-tags)))

(use-package org-roam
  :ensure t
  :bind (:map org-roam-mode-map
	      (("C-c n l" . org-roam)
	       ("C-c n f" . org-roam-find-file)
	       ("C-c n j" . org-roam-jump-to-index)
	       ("C-c n b" . org-roam-switch-to-buffer)
	       ("C-c n g" . org-roam-graph))
	      :map org-mode-map
	      (("C-c n i" . org-roam-insert)))
  :config
  (setq org-roam-directory (concat local-notes-directory "/roam"))
  (setq org-roam-index-file "index.org"))

(use-package tychoish-bootstrap
  :commands (tychoish-setup-global-modes
	     tychoish-setup-user-local-config
	     tychoish-setup-font
	     tychoish-get-config-file-prefix
	     tychoish-get-config-file-path)
  :bind (("C-c f =" . text-scale-increase)
	 ("C-c f -" . text-scale-decrease)
	 ("C-c f 0" . text-scale-reset)
	 ("C-c C-=" . opacity-increase)
	 ("C-c C--" . opacity-decrease)
	 ("C-c f C-0" . opacity-reset)
	 ("C-c t t d" . disable-theme)
	 ("C-c t t D" . disable-all-themes)
	 ("C-c t t e" . enable-theme)
	 ("C-c t t l" . load-theme)
	 ("C-c C-r" . rename-buffer))
  :functions (gui-p default-string with-timer with-slow-op-timer)
  :init
  (setq server-use-tcp t)
  ;; (setq server-host "127.0.0.1")
  ;; (setq server-port 2286)

  (setq starttls-use-gnutls t)
  (setq gnutls-log-level 0)

  (setq safe-local-variable-values '((encoding . utf-8)))
  (setq warnings-to-ignore '())
  (add-to-list 'warnings-to-ignore '((free-vars) (nresolved) (callargs)
				     (redefine) (obsolete) (noruntine)
				     (cl-functions) (interactive-only)))
  (setq byte-compile-warnings warnings-to-ignore)

  (setq backup-by-copying t)
  (setq make-backup-files t)
  (setq delete-old-versions t)

  (fset 'yes-or-no-p 'y-or-n-p)
  (put 'dired-find-alternate-file 'disabled nil)
  (put 'list-timers 'disabled nil)

  (setq confirm-kill-processes nil)
  (setq confirm-nonexistent-file-or-buffer nil)
  (setq kill-buffer-query-functions
	(remq 'process-kill-buffer-query-function
	      kill-buffer-query-functions))

  (defalias 'eb 'eval-buffer)

  (setq inhibit-startup-echo-area-message (user-login-name))
  (setq inhibit-startup-message t)
  (setq initial-major-mode 'fundamental-mode)
  (setq initial-scratch-message nil)
  (setq fringe-mode 'half-width)
  (setq split-height-threshold 100)

  (when (eq system-type 'darwin)
    (setq ns-use-srgb-colorspace nil)
    (setq display-highres t))

  (setq use-dialog-box nil)
  (setq ring-bell-function (lambda () nil))

  (setq scroll-conservatively 25)
  (setq scroll-preserve-screen-position 1)
  (setq cursor-in-non-selected-windows nil)
  (setq indicate-empty-lines t)

  (setq font-lock-support-mode 'jit-lock-mode)
  (setq jit-lock-stealth-time nil)
  (setq jit-lock-defer-time 0.2)
  (setq jit-lock-stealth-nice 0.2)
  (setq jit-lock-stealth-load 100)

  (set-default 'truncate-lines t)
  (set-face-attribute 'header-line nil :background nil :weight 'bold)

  (let ((theme-directory (concat (expand-file-name user-emacs-directory) "theme")))
    (setq custom-theme-directory theme-directory)
    (add-to-list 'custom-theme-load-path theme-directory)
    (add-to-list 'load-path theme-directory))
  :config
  (tychoish-set-backup-directory tychoish-backup-directory)

  (defvar tychoish-emacs-identifier (default-string "solo" (daemonp)))

  (setq frame-title-format '(:eval (if (stringp (daemonp))
				       (format "%s:%s" (daemonp) (buffer-name))
				     (concat "solo:" (buffer-name)))))
  (setq bookmark-save-flag 1)
  (setq bookmark-default-file (tychoish-get-config-file-path "bookmarks")))

(use-package cus-edit
  :after (tychoish-bootstrap)
  :defer 1
  :init
  (setq custom-file (tychoish-get-config-file-path "custom.el"))
  :config
  (when (and custom-file (file-exists-p custom-file))
    (let ((inhibit-message t))
      (load (expand-file-name custom-file) t t t))))

(use-package tychoish-blogging
  :after (f)
  :commands (tychoish-blog-insert-date
	     tychoish-blog-publish-post
	     tychoish-blog-create-post
	     tychoish-blog-push
	     tychoish-blog-open-drafts-dired)
  :bind (("C-c t b m" . tychoish-blog-insert-date)
	 ("C-c t b p" . tychoish-blog-publish-post)
	 ("C-c t b n" . tychoish-blog-create-post)
	 ("C-c t b C-p" . tychoish-blog-push)
	 ("C-c t b d" . tychoish-blog-open-drafts-dired))
  :config
  (setq tychoish-blog-path (expand-file-name "~/projects/blog")))

(use-package tychoish-editing
  :commands (markdown-indent-code
	     rst-indent-code
	     uniquify-region-lines
	     uniquify-buffer-lines
	     font-lock-show-tabs
	     font-lock-width-keyword)
  :bind (("M-<up>" . move-text-up)
	 ("M-<down>" . move-text-down)
	 ("C-c t w" . tycho-toggle-hooks)
	 ("C-x C-n" . word-count)
	 ("C-w" . kill-region)
	 ("(" . tychoish-electric-pair)
	 ("[" . tychoish-electric-pair)
	 ("{" . tychoish-electric-pair)
	 ("<" . tychoish-electric-pair)
	 ("\"" . tychoish-electric-pair)
	 ("*" . tychoish-electric-pair)
	 ("_" . tychoish-electric-pair)
	 ("RET" . electrify-return-if-match))
  :init
  (setq show-paren-delay 0.25)
  (setq indent-tabs-mode nil)

  (defalias 'dr 'delete-region)
  (defalias 'dw 'delete-trailing-witespace)

  (setq next-line-add-newlines nil)
  (setq undo-auto-current-boundary-timer t)

  (when (eq system-type 'darwin)
    (setq ns-function-modifier 'hyper)
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'super))

  (when (eq system-type 'gnu/linux)
    (setq x-alt-keysym 'meta)
    (setq x-super-keysym 'super))

  (global-set-key (kbd "s-x") 'clipboard-kill-region) ;;cut
  (global-set-key (kbd "s-c") 'clipboard-kill-ring-save) ;;copy
  (global-set-key (kbd "s-v") 'clipboard-yank) ;;paste
  (global-set-key (kbd "C-z") 'undo)

  (global-unset-key (kbd "C-x C-u"))
  (global-set-key (kbd "C-x C-u t") 'upcase-initials-region)
  (global-set-key (kbd "C-x C-u r") 'upcase-region)
  (global-set-key (kbd "C-x C-u w") 'upcase-word)

  (global-set-key (kbd "C-x l") 'goto-line)
  (global-set-key (kbd "C-c C-f") 'set-fill-column)
  (global-set-key (kbd "C-c C-p") 'set-mark-command)
  (global-set-key (kbd "C-c c") 'comment-region)
  (global-set-key (kbd "C-c i") 'indent-region)
  (global-set-key (kbd "C-c s w") 'ispell-word)
  (global-set-key (kbd "C-h") 'backward-kill-word)
  (global-set-key (kbd "C-x C-x") 'exchange-dot-and-mark)
  (global-set-key (kbd "M-<SPC>") 'set-mark-command)
  (global-set-key (kbd "M-C-q") 'fill-region))

(use-package git-grep
  :ensure t
  :bind (("C-c g g" . git-grep)
	 ("C-c g r" . git-grep-repo))
  :init
  (global-set-key (kbd "C-c g f") 'find-grep)
  (global-set-key (kbd "C-c C-o") 'occur)
  (define-key isearch-mode-map (kbd "C-o") 'isearch-occur))

(use-package whitespace
  :bind (("C-c C-w" . whitespace-cleanup)))

(use-package tramp
  :commands (sshra ssh-reagent)
  :init
  (setq tramp-default-method "ssh")

  (defun ssh-reagent ()
    (interactive)
    (let* ((sshdir (car (directory-files "/tmp" nil "ssh-*")))
	   (agent (car (directory-files (concat "/tmp/" sshdir) nil "agent.*"))))
      (setenv "SSH_AUTH_SOCK" (concat "/tmp/" sshdir "/" agent)))
    (message "Attached to SSH Session"))

  (defalias 'sshra 'ssh-reagent))

(use-package docker-tramp
  :after tramp
  :ensure t)

(use-package docker
  :ensure t
  :commands (docker)
  :bind ("C-c C-d" . docker))

(use-package docker)

(use-package lpr
  :commands (lpr-region lpr-buffer)
  :config
  (setq lpr-add-switches "-T ''"))

(use-package comint
  :commands (comint-mode)
  :bind (:map comint-mode-map
	      ("M-n" . comint-next-input)
	      ("M-p" . comint-previous-input)
	      ([down] . comint-next-matching-input-from-input)
	      ([up] . comint-previous-matching-input-from-input))
  :config
  (setq ansi-color-for-comint-mode t))

(use-package windmove
  :bind ((("M-j" . windmove-down)
	  ("M-k" . windmove-up)
	  ("M-h" . windmove-left)
	  ("M-l" . windmove-right)))
  :init
  (windmove-default-keybindings)

  (global-set-key (kbd "M-J") (lambda () (interactive) (enlarge-window 1)))
  (global-set-key (kbd "M-K") (lambda () (interactive) (enlarge-window -1)))
  (global-set-key (kbd "M-H") (lambda () (interactive) (enlarge-window -1 t)))
  (global-set-key (kbd "M-L") (lambda () (interactive) (enlarge-window 1 t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; email (mu4e) configuration
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package emojify
  :ensure t
  :commands (global-emojify-mode)
  :diminish emojify-mode
  :after (tychoish-editing)
  :demand t
  :config
  (setq emojify-display-style 'image)
  (setq emojify-emoji-styles '(unicode))
  (setq emojify-company-tooltips-p t)
  (setq emojify-point-entered-behaviour 'echo))

(use-package message
  :mode ((".*mutt.*" . message-mode)
	 ("/mutt" . message-mode))
  :init
  (setq message-citation-line-format "On %A, %B %d %Y, %T, %N wrote:\n")
  (setq message-citation-line-function 'message-insert-formatted-citation-line)
  (setq message-default-mail-headers "Cc: \nBcc: \n")
  (setq message-from-style 'angles)
  (setq message-interactive nil)
  (setq message-kill-buffer-on-exit nil)
  (setq message-send-mail-function 'message-send-mail-with-sendmail))

(use-package mu4e
  :bind (("C-c m m" . mu4e)
	 ("C-c m d" . mu4e~headers-jump-to-maildir)
	 ("C-c m b" . mu4e-headers-search-bookmark)
	 ("C-c m c" . mu4e-compose-new)
	 :map mu4e-headers-mode-map
	 ("r" . mu4e-headers-mark-for-read)
	 ("o" . mu4e-headers-mark-for-unread)
	 ("*" . mu4e-headers-mark-for-something)
	 ("#" . mu4e-mark-resolve-deferred-marks)
	 (";" . mu4e-mark-resolve-deferred-marks))
  :commands (mu4e-mail-view-actions
	     mu4e-compose-new
	     mu4e-update-mail-and-index
	     mu4e-headers-jump-to-maildir
	     mu4e-headers-search-bookmark
	     tychoish-set-up-email)
  :init
  (setq mc-gpg-user-id (getenv "GPG_KEY_ID"))
  (setq mu4e-reply-to-address user-mail-address)
  (setq mu4e-headers-results-limit 1000)
  (setq completion-ignore-case t)
  (setq compose-mail-user-agent-warnings nil)
  (setq mail-header-separator "--------------------------")
  (setq mail-imenu-generic-expression
	'(("Subject"  "^Subject: *\\(.*\\)" 1)
	  ("Cc"     "^C[Cc]: *\\(.*\\)" 1)
	  ("To"     "^To: *\\(.*\\)" 1)
	  ("From"  "^From: *\\(.*\\)" 1)))
  (setq mail-signature t)
  (setq mail-specify-envelope-from t)
  (setq mu4e-compose-complete-addresses t)
  (setq mu4e-compose-dont-reply-to-self t)
  (setq mu4e-compose-keep-self-cc nil)
  (setq mu4e-compose-signature t)
  (setq mu4e-compose-format-flowed t)
  (setq mu4e-compose-complete-only-after "2015-01-01")
  (setq mu4e-maildir-shortcuts nil)
  (setq mu4e~maildir-list nil)
  (setq mu4e-headers-include-related nil)
  (setq mu4e-drafts-folder "/drafts")
  (setq mu4e-sent-folder "/sent")
  (setq mu4e-trash-folder "/trash")
  (setq sendmail-program "msmtp")
  (setq smtpmail-queue-mail nil)
  (setq mu4e-view-show-images t)
  (setq mml2015-sign-with-sender t)
  (setq mail-user-agent 'mu4e-user-agent)
  (setq mu4e-user-agent-string nil)

  (add-hook 'mu4e-compose-mode-hook
	    (lambda ()
	      "My settings for message composition."
	      (flush-lines "^\\(> \n\\)*> -- \n\\(\n?> .*\\)*") ;;; Kills quoted sigs.
	      (not-modified) ;;; We haven't changed the buffer, haven't we? *g*
	      (message-goto-body) ;;; Jumps to the beginning of the mail text
	      (setq make-backup-files nil) ;;; No backups necessary.
	      (setq fill-column 70)
	      (visual-fill-column-mode 1)
	      (flyspell-mode 1)))

  (defun tychoish-add-standard-mail-bookmarks ()
    "add a standard/generic list of bookmarks"
    (add-to-list 'mu4e-bookmarks '("mime:image/*" "Messages with images" ?p))
    (add-to-list 'mu4e-bookmarks '("date:today..now" "Today's messages" ?t))
    (add-to-list 'mu4e-bookmarks '("date:7d..now" "This Week's messages" ?w))
    (add-to-list 'mu4e-bookmarks '("m:/inbox AND flag:seen" "Inbox (seen)" ?s))
    (add-to-list 'mu4e-bookmarks '("m:/inbox" "Inbox (all)" ?i))
    (add-to-list 'mu4e-bookmarks '("m:/rss.*" "RSS Read" ?R))
    (add-to-list 'mu4e-bookmarks '("m:/rss.*  AND flag:unread" "RSS Unread" ?r))
    (add-to-list 'mu4e-bookmarks '("m:/inbox OR flag:unread AND NOT (flag:trashed OR m:/sent OR m:/trash)" "all unread message" ?a))
    (add-to-list 'mu4e-bookmarks '("flag:unread AND NOT flag:trashed AND NOT m:/rss.*" "Unread messages (no RSS)" ?u))
    (add-to-list 'mu4e-bookmarks '("m:/inbox OR flag:unread AND NOT (m:/rss.* OR m:/sent OR flag:trashed OR m:/trash)"
				   "to read/process queue" ?q)))

  :config
  (defun tychoish-set-up-email (maildir name address)
    (let* ((mudir (f-join maildir ".mu")))
      (setq message-directory maildir)
      (setq mu4e-maildir maildir)
      (setq mu4e-mu-home mudir))

    (setq mu4e-bookmarks nil)
    (tychoish-add-standard-mail-bookmarks)

    (setq user-mail-address address)
    (setq user-full-name name)
    (setq mu4e-reply-to-address address)
    (setq smtpmail-queue-dir (f-join maildir "queue/cur"))
    (setq message-signature-file (f-join maildir "tools" "signatures" address))
    (setq mail-host-address (s-replace-regexp ".*@" "" address))
    (setq message-sendmail-extra-arguments `("-a" ,address))
    (setq message-auto-save-directory (f-join mu4e-maildir "drafts"))
    (message (format "mail: configured address [%s]" address))
    (tychoish-change-email-body user-full-name address))

  (defun tychoish-change-email-body (name address)
    "change an email address on an extant mail buffer"
    (beginning-of-buffer)
    (let ((new-from (concat "From: " name " <" address ">")))
      (while (re-search-forward "^From:.*$" nil t 1)
	(replace-match new-from nil nil))))

  (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)
  (defun mu4e~draft-insert-mail-header-separator ()
    ;; we set this here explicitly, since (as it has happened) a wrong
    ;; value for this (such as "") breaks address completion and other things
    (set (make-local-variable 'mail-header-separator)
	 (purecopy "--------------------------"))
    (put 'mail-header-separator 'permanent-local t)
    (save-excursion
      (let ((sepa (propertize mail-header-separator
			      'intangible t
			      'read-only "Can't touch this"
			      'rear-nonsticky t
			      'font-lock-face 'mu4e-system-face)))
	(goto-char (point-min))
	;; search for the first empty line
	(if (search-forward-regexp "^$" nil t)
	    (replace-match (concat sepa))
	  (progn  ;; no empty line? then prepend one
	    (goto-char (point-max))
	    (insert "\n" sepa)))))))

(use-package messages-are-flowing
  :after (mu4e)
  :ensure t
  :init
  (defun messages-are-flowing-use-and-mark-hard-newlines ()
    (interactive)
    (use-hard-newlines 1 'always)
    (add-hook 'after-change-functions 'messages-are-flowing--mark-hard-newlines nil t))

  (setq messages-are-flowing-newline-marker "◀")
  (add-hook 'message-mode-hook 'messages-are-flowing-use-and-mark-hard-newlines))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; writing (english) configuration
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package deft
  :ensure t
  :bind (("C-c C-d" . deft)
	 ("C-c d o" . deft)
	 ("C-c x d" . deft)
	 ("C-c d n" . tychoish-deft-create)
	 ("C-c x n" . tychoish-deft-create)
	 ("C-c d d" . (lambda () (interactive) (find-file deft-directory))))
  :init
  (setq deft-directory (concat local-notes-directory "/deft"))
  (setq deft-directories '(deft-directory org-roam-directory))

  (defun deft-file-make-slug (s)
    "Turn a string into a slug."
    (replace-regexp-in-string
     " " "-" (downcase
	      (replace-regexp-in-string
	       "[^A-Za-z0-9 ]" "" s))))

  (defun tychoish-deft-create (title)
    "Create a new deft entry."
    (interactive "sBwO Title: ")
    (let ((draft-file (concat deft-directory
			      (deft-file-make-slug title)
			      "."
			      deft-extension)))
      (if (file-exists-p draft-file)
	  (find-file draft-file)
	(find-file draft-file)
	(insert (title)))))
  :config
  (setq deft-extension "txt")
  (setq deft-text-mode 'markdown-mode)
  (setq deft-use-filename-as-title t)
  (setq deft-auto-save-interval 0)
  (setq deft-auto-save-interval nil))


(use-package artbollocks-mode
  :ensure t
  :commands (artbollocks-mode)
  :config
  (setq weasel-words-regex
	(concat "\\b" (regexp-opt
		       '("one of the"
			 "very"
			 "sort of"
			 "a lot"
			 "probably"
			 "maybe"
			 "perhaps"
			 "I think"
			 "really"
			 "nice"
			 "utilize"
			 "leverage") t) "\\b"))

  (setq lexical-illusions nil)
  (setq weasl-words t)
  (setq passive-voice t)

  ;; Make sure keywords are case-insensitive
  (defadvice search-for-keyword (around sacha activate)
    "Match in a case-insensitive way."
    (let ((case-fold-search t))
      ad-do-it)))

(use-package pocket-reader
  :ensure t
  :commands (pocket-reader pocket-reader-add-link))

(use-package google-this
  :ensure t
  :diminish google-this-mode
  :bind-keymap ("C-c /" . google-this-mode-submap)
  :config
  (google-this-mode 1))

(use-package tex
  :defer t
  :ensure auctex
  :config
  (setq tex-dvi-view-command "(f=*; pdflatex \"${f%.dvi}.tex\" && open \"${f%.dvi}.pdf\")")
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-PDF-mode t)
  (setq TeX-master nil)
  (setq auto-mode-alist (cons '("\\.tex" . latex-mode) auto-mode-alist))
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'latex-mode-hook 'flyspell-mode)
  (add-hook 'latex-mode-hook 'turn-on-auto-fill))

(use-package markdown-mode
  :ensure t
  :delight "mdwn"
  :mode ("\\.mdwn" "\\.md" "\\.markdown")
  :init
  (setq auto-mode-alist (cons '("\\.mdwn" . markdown-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))
  (setq auto-mode-alist (cons '("\\.markdown" . markdown-mode) auto-mode-alist))
  :config
  (if (eq system-type 'darwin)
      (setq markdown-command "/usr/local/bin/mmd --nosmart")
    (setq markdown-command "/usr/bin/markdown"))

  (add-hook 'markdown-mode-hook 'flyspell-mode)
  (add-hook 'markdown-mode-hook 'turn-on-auto-fill)
  (add-hook 'markdown-mode-hook (lambda () (setq imenu-generic-expression markdown-imenu-generic-expression)))

  (setq markdown-imenu-generic-expression
	'(("title"  "^\\(.*\\)[\n]=+$" 1)
	  ("h2-"    "^\\(.*\\)[\n]-+$" 1)
	  ("h1"   "^# \\(.*\\)$" 1)
	  ("h2"   "^## \\(.*\\)$" 1)
	  ("h3"   "^### \\(.*\\)$" 1)
	  ("h4"   "^#### \\(.*\\)$" 1)
	  ("h5"   "^##### \\(.*\\)$" 1)
	  ("h6"   "^###### \\(.*\\)$" 1)
	  ("fn"   "^\\[\\^\\(.*\\)\\]" 1)
	  )))

(use-package flyspell-correct-helm
  :ensure t
  :after (flyspell)
  :bind ("C-;" . flyspell-correct-wrapper)
  :config
  (setq flyspell-correct-interface #'flyspell-correct-helm))

(use-package flyspell
  :ensure t
  :defer t
  :diminish (flyspell-mode . "fs")
  :commands (flyspell-mode flyspell-prog-mode)
  :init
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'text-mode-hook 'flyspell-mode)
  :config
  (defalias 'fsb 'flyspell-buffer)
  (defalias 'fs 'flyspell-mode)

  (setq flyspell-issue-welcome-flag nil)
  (setq flyspell-issue-message-flag nil)
  (setq ispell-program-name "aspell")
  (setq ispell-extra-args '("--sug-mode=ultra" "--run-together"))

  (setq flyspell-sort-corrections nil)
  (defvar ispell-list-command "list")
  (defvar flyspell-dictionaries '("english"))
  (defvar flyspell-guess-size 76)
  (defvar flyspell-guess-slots 3)
  (defvar flyspell-focus 50) ;; between 0 and 100
  (defvar flyspell-timeout 3)
  (defvar flyspell-polling-timeout 10)
  (defvar flyspell-idle-timeout 1)
  (defvar flyspell-indicator-format "[%s]--")
  (defvar flyspell-timer nil)
  (defvar flyspell-timer-aux nil)
  (defvar flyspell-origin)
  (defvar flyspell-guess-indicator nil)
  (defvar flyspell-min-buffer-size (* flyspell-guess-size flyspell-guess-slots)))

(use-package fountain-mode
  :ensure t
  :mode ("\\.script" "\\.sp"))

(use-package rst-mode
  :delight "rst"
  :mode ("\\.rst" "\\.txt")
  :init
  (add-hook 'rst-mode-hook
	    (lambda ()
	      (turn-on-auto-fill)
	      (setq fill-column 78)
	      (setq rst-level-face-max 0)
	      (set-face-background 'rst-level-1 nil)
	      (set-face-background 'rst-level-2 nil)
	      (set-face-background 'rst-level-3 nil)
	      (set-face-background 'rst-level-4 nil)
	      (set-face-background 'rst-level-5 nil)
	      (set-face-background 'rst-level-6 nil)

	      (local-set-key (kbd "C-M-h") 'backward-kill-word)
	      (define-key rst-mode-map "\"" 'tychoish-electric-pair)
	      (define-key rst-mode-map "\'" 'tychoish-electric-pair)
	      (define-key rst-mode-map "\*" 'tychoish-electric-pair)
	      (define-key rst-mode-map "\_" 'tychoish-electric-pair)
	      (define-key rst-mode-map "(" 'tychoish-electric-pair)
	      (define-key rst-mode-map "[" 'tychoish-electric-pair)
	      (define-key rst-mode-map "{" 'tychoish-electric-pair)
	      (define-key rst-mode-map "<" 'tychoish-electric-pair)
	      (define-key rst-mode-map (kbd "C-c C-t h") 'rst-adjust)
	      (local-unset-key (kbd "C-c C-s")))))

(use-package wc-mode
  :ensure t
  :bind (("C-c w w" . wc-mode))
  ;; :hook ((text-mode . wc-mode))
  :commands (wc-mode)
  :config
  (setq wc-modeline-format "wc[%tl:%tw]"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; irc (erc) configuration
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package alert
  :functions (alert)
  :commands (alert alert-sardis)
  :ensure t
  :config
  (cond
   ((eq system-type 'darwin)
    (setq alert-default-style 'osx-notifier))
   ((executable-find "notify-send")
    (setq alert-default-style 'libnotify))
   ((eq system-type 'gnu/linux)
    (setq alert-default-style 'notifications))
   ((executable-find "sardis")
    (setq alert-default-style 'sardis))
   (t (setq alert-default-style 'message)))

  (setq alert-log-messages t)

  (alert-define-style
   'sardis
   :title "sardis"
   :notifier
   (lambda (info)
     (call-process "sardis" nil 0 nil
		   "--name" (plist-get info :title)
		   "notify" "send"
		   (plist-get info :message))))

  (defun alert-sardis (message &optional &key title)
    (let ((alert-default-style 'sardis)
	  (m-title (or title (buffer-name (current-buffer)))))
      (alert message :title m-title))))

(use-package ercn
  :ensure t
  :after (erc alert)
  :config
  (setq ercn-suppress-rules '((system . all)
			      (fool . all)
			      (dangerous-host . all)))

  (setq ercn-notify-rules '((current-nick . all)
			    (query-buffer . all)
			    (message . ("#unclear"))))

  (defun do-erc-notify (nickname message)
    "Hook implementation of a notification."
    (catch 'early-return
      (let* ((channel (buffer-name))
	     (check (when (or (string-prefix-p "*irc-" channel)
			      (string= "bot" nickname)
			      (search "bitlbee" (downcase channel)))
		      (throw 'early-return "skip notification noise")))
	     (msg (s-trim (s-collapse-whitespace message)))
	     (title (if (string-match-p (concat "^" nickname) channel)
			nickname
		      (concat nickname " (" channel ")"))))

      (alert msg :title title))))

  (add-hook 'ercn-notify-hook 'do-erc-notify))

(use-package znc
  :ensure t
  :commands (znc-all)
  :bind (("C-c e a" . znc-all)))

(use-package erc-yank
  :ensure t
  :after (erc)
  :bind (:map erc-mode-map
	 ("C-y" . erc-yank)))

(use-package erc
  :ensure t
  :commands (erc)
  :bind (("C-c e r" . reset-erc-track-mode)
	 ("C-c e n" . tychoish-next-erc-buffer)
	 ("C-c <SPC>" . erc-next-channel-buffer)
	 ("C-c C-<SPC>" . erc-next-channel-buffer)
	 ("C-c e k" . kill-all-erc-buffers)
	 ("C-c e b" . ido-erc-buffer)
	 ([backtab] . erc-button-url-previous)
	 ("C-c C-d" . erc-truncate-buffer))
  :config
  (add-hook 'erc-mode-hook (lambda ()
			     (visual-line-mode 1)
			     (auto-fill-mode 0)))
  (add-hook 'erc-insert-pre-hook
	    (lambda (s)
	      (when (erc-foolish-content s)
		(setq erc-insert-this nil))))

  (defvar erc-foolish-content '("No such nick/channel"))

  (setq erc-modules '(stamp
		      completion
		      autojoin
		      irccontrols
		      tweet
		      list
		      match
		      menu
		      tweet
		      move-to-prompt
		      netsplit
		      networks
		      noncommands
		      readonly
		      ring
		      spelling
		      track))

  (make-variable-buffer-local 'erc-fill-column)

  (setq erc-ignore-list '("*@*facebook" "&bitlbee"))
  (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
				    "324" "329" "332" "333" "353" "477"))
  (setq erc-hide-list '("MODE"))

  (setq erc-current-nick-highlight-type 'nick)
  (setq erc-insert-timestamp-function 'erc-insert-timestamp-left)
  (setq erc-keywords '("\\berc[-a-z]*\\b" "\\bemms[-a-z]*\\b"))

  (setq erc-timestamp-format "[%H:%M] ")
  (setq erc-fill-prefix "        ")
  (setq erc-fill-column 80)

  (setq erc-rename-buffers t)
  (setq erc-auto-query 'bury)
  (setq erc-format-query-as-channel-p t)
  (setq erc-header-line-format nil)
  (setq erc-input-line-position -2)
  (setq erc-interpret-controls-p 'remove)
  (setq erc-interpret-mirc-color t)
  (setq erc-join-buffer 'bury)
  (setq erc-kill-buffer-on-part t)
  (setq erc-kill-queries-on-quit t)
  (setq erc-kill-server-buffer-on-quit t)
  (setq erc-nicklist-use-icons nil)
  (setq erc-server-303-functions nil)
  (setq erc-server-coding-system '(utf-8 . utf-8))
  (setq erc-timestamp-format "%H:%M ")
  (setq erc-track-enable-keybindings t)
  (setq erc-track-exclude-server-buffer t)
  (setq erc-track-faces-priority-list nil)
  (setq erc-track-priority-faces-only nil)
  (setq erc-track-use-faces nil)
  (setq erc-truncate-buffer-on-save t)
  (setq erc-max-buffer-size (* 10 1000))

  (add-hook 'erc-insert-post-hook 'erc-truncate-buffer)

  (setq erc-prompt (lambda ()
		     (if erc-network
			 (concat "[" (symbol-name erc-network) "]")
		       (concat "[" (car erc-default-recipients) "]"))))

  (add-hook 'window-configuration-change-hook
	    '(lambda ()
	      (save-excursion
		(walk-windows
		 (lambda (w)
		   (let ((buffer (window-buffer w)))
		     (set-buffer buffer)
		     (when (eq major-mode 'erc-mode)
		       (setq erc-fill-column (- (window-width w) 2)))))))))

  (defun reset-erc-track-mode ()
    (interactive)
    (setq erc-modified-channels-alist nil)
    (erc-modified-channels-update)
    (erc-track-switch-buffer 1)
    (erc-track-switch-buffer -1))

  (defadvice erc-track-find-face (around erc-track-find-face-promote-query activate)
    (if (erc-query-buffer-p)
	(setq ad-return-value (intern "erc-current-nick-face"))
      ad-do-it))

  (defun erc-foolish-content (msg)
    "Check whether MSG is foolish."
    (erc-list-match erc-foolish-content msg))

  (defun erc-button-url-previous ()
    "Go to the previous URL button in this buffer."
    (interactive)
    (let* ((point (point))
	   (found (catch 'found
		    (while (setq point (previous-single-property-change point 'erc-callback))
		      (when (eq (get-tbext-property point 'erc-callback) 'browse-url)
			(throw 'found point))))))
      (if found
	  (goto-char found)
	(error "No previous URL button"))))

  (defun kill-all-erc-buffers ()
    "Kill all erc buffers."
    (interactive)
    (save-excursion
      (let((count 0))
	(dolist(buffer (buffer-list))
	  (set-buffer buffer)
	  (when (equal major-mode 'erc-mode)
	    (setq count (1+ count))
	    (kill-buffer buffer)))
	(message "Killed %i erc buffer(s)." count ))))

  (defvar tychoish-erc-disable-connection-status nil
    "When t, disable setting 'mode-line-process' with the connection status")

  (defun erc-custom-modeline (buffer)
    (with-current-buffer buffer
      (if tychoish-erc-disable-connection-status
	  (setq mode-line-process '())
	(let ((process-status (cond ((and (erc-server-process-alive)
					  (not erc-server-connected))
				     ":C")
				    ((erc-server-process-alive)
				     ":A")
				    (t
				     ":X"))))
	  (setq mode-line-process (list process-status))))))

  (advice-add 'erc-update-mode-line-buffer :after #'erc-custom-modeline)

  (defun tychoish-erc-update-modelines ()
    (interactive)
    (dolist (buffer (buffer-list))
      (set-buffer buffer)
      (when (equal major-mode 'erc-mode)
	(erc-custom-modeline buffer))))

  (defvar erc-channels-to-visit nil
    "Channels that have not yet been visited by erc-next-channel-buffer")
  (defun erc-next-channel-buffer ()
    "Switch to the next unvisited channel. See erc-channels-to-visit"
    (interactive)
    (when (null erc-channels-to-visit)
      (setq erc-channels-to-visit
	    (remove (current-buffer) (erc-channel-list nil))))
    (let ((target (pop erc-channels-to-visit)))
      (if target
	  (switch-to-buffer target))))

  (defun tychoish-next-erc-buffer ()
    "Switch to an IRC buffer, or run `erc-select'.
    When called repeatedly, cycle through the buffers."
    (interactive)
    (let ((buffers (and (fboundp 'erc-buffer-list)
			(erc-buffer-list))))
      (when (eq (current-buffer) (car buffers))
	(bury-buffer)
	(setq buffers (cdr buffers)))))

  (defun ido-erc-buffer nil
    "Switch to ERC buffer using IDO to choose which one, or start ERC if not already started."
    (interactive)
    (let (final-list (list ))
      (dolist (buf (buffer-list) final-list)
	(if (equal 'erc-mode (with-current-buffer buf major-mode))
	    (setq final-list (append (list (buffer-name buf)) final-list))))
      (if final-list
	  (switch-to-buffer (ido-completing-read "Buffer: " final-list))
	(call-interactively 'erc))))

  (setq erc-track-priority-faces-only (remove "&bitlbee" erc-channel-list))
  (global-emojify-mode 1)
  (erc-update-modules)
  (erc-add-scroll-to-bottom)
  (erc-timestamp-mode 1)
  (erc-notifications-mode 0)
  (erc-fill-mode 1)
  (erc-spelling-mode 1)
  (erc-track-mode 1))

(use-package erc-hl-nicks
  :ensure t
  :after (erc))

(use-package erc-tweet
  :ensure t
  :after (erc))

(use-package pkgbuild-mode
  :ensure t
  :mode ("/PKGBUILD$"))

(use-package conf-mode
  :mode (("\\.service$'" . conf-unix-mode)
	 ("\\.timer$'" . conf-unix-mode)
	 ("\\.target$'" . conf-unix-mode)
	 ("\\.mount$'" . conf-unix-mode)
	 ("\\.automount$'" . conf-unix-mode)
	 ("\\.slice$'" . conf-unix-mode)
	 ("\\.socket$'" . conf-unix-mode)
	 ("\\.path$'" . conf-unix-mode)
	 ("\\.conf$'" . conf-unix-mode)))

(use-package sh-script
  :mode (("\\.bash$'" . sh-mode)
	 ("\\.sh$'" . sh-mode)
	 ("\\.zsh$'" . sh-mode)
	 ("\\.zshrc$'" . sh-mode)
	 ("\\.bashrc$'" . sh-mode)
	 ("\\.bash_profile$'" . sh-mode)))

(use-package nxml-mode
  :mode (("\\.xml$'". nxml-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; language server protocol code
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package helm-lsp
  :ensure t
  :after (lsp-mode)
  :commands (helm-lsp-workspace-symbol)
  :init (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol))

(use-package lsp-mode
  :diminish (lsp-mode . "lsp")
  :bind (:map lsp-mode-map
	 ("C-c C-d" . lsp-describe-thing-at-point))
  :hook ((python-mode . lsp-deferred)
	 (js-mode . lsp-deferred)
	 (js2-mode . lsp-deferred)
	 (dockerfile-mode . lsp-deferred)
	 (sh-mode . lsp-deferred)
	 (typescript-mode . lsp-deferred)
	 (go-mode . lsp-deferred))
  :init
  (setq lsp-auto-guess-root t       ; Detect project root
	lsp-log-io nil
	lsp-enable-indentation t
	lsp-enable-imenu t
	lsp-keymap-prefix "C-c l"
	lsp-enable-file-watchers t

	lsp-file-watch-threshold 5000
	lsp-prefer-flymake nil)      ; Use lsp-ui and flycheck

  (defun lsp-on-save-operation ()
    (when (or (boundp 'lsp-mode)
	      (bound-p 'lsp-deferred))
      (lsp-organize-imports)
      (lsp-format-buffer))))

(use-package lsp-clients
  :ensure nil
  :after (lsp-mode)
  :init (setq lsp-clients-python-library-directories '("/usr/local/" "/usr/")))

(use-package lsp-ui
  :ensure t
  :after (lsp-mode)
  :commands lsp-ui-doc-hide
  :bind (:map lsp-ui-mode-map
	      ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
	      ([remap xref-find-references] . lsp-ui-peek-find-references)
	      ("C-c u" . lsp-ui-imenu))
  :init (setq lsp-ui-doc-enable t
	      lsp-ui-doc-use-webkit nil
	      lsp-ui-doc-header nil
	      lsp-ui-doc-delay 0.2
	      lsp-ui-doc-include-signature t
	      lsp-ui-doc-alignment 'at-point
	      lsp-ui-doc-use-childframe nil
	      lsp-ui-doc-border (face-foreground 'default)
	      lsp-ui-peek-enable t
	      lsp-ui-peek-show-directory t
	      lsp-ui-sideline-delay 10
	      lsp-ui-sideline-update-mode 'point
	      lsp-ui-sideline-enable t
	      lsp-ui-sideline-show-code-actions t
	      lsp-ui-sideline-show-hover t
	      lsp-ui-sideline-ignore-duplicate t
	      lsp-gopls-use-placeholders nil)
  :config
  (setq lsp-completion-provider :capf)
  (setq lsp-idle-delay 0.500)
  (setq lsp-print-performance t)

  (add-to-list 'lsp-ui-doc-frame-parameters '(right-fringe . 8))

  ;; `C-g'to close doc
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)

  ;; Reset `lsp-ui-doc-background' after loading theme
  (add-hook 'after-load-theme-hook
	    (lambda ()
	      (setq lsp-ui-doc-border (face-foreground 'default))
	      (set-face-background 'lsp-ui-doc-background
				   (face-background 'tooltip))))

  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; @see https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil)))

;; debugger
(use-package dap-mode
  :disabled t
  :diminish dap-mode
  :after (lsp-mode)
  :functions dap-hydra/nil
  :bind (:map lsp-mode-map
	      ("<f5>" . dap-debug)
	      ("M-<f5>" . dap-hydra))
  :hook ((dap-mode . dap-ui-mode)
	 (dap-session-created . (lambda (&_rest) (dap-hydra)))
	 (dap-terminated . (lambda (&_rest) (dap-hydra/nil)))))

(use-package lsp-treemacs
  :after (lsp-mode treemacs)
  :ensure t
  :commands lsp-treemacs-errors-list
  :bind (:map lsp-mode-map
	      ("M-9" . lsp-treemacs-errors-list)))

(use-package treemacs
  :ensure t
  :commands (treemacs)
  :after (lsp-mode))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :after (helm)
  :commands (which-key-mode)
  :config
  (setq which-key-idle-delay 1.75)
  (setq which-key-idle-secondary-delay 0.5)
  (which-key-setup-minibuffer))

(provide 'tychoish-core)
;;; programming.el ends here
