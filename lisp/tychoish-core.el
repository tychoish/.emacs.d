;;; tychoish-core.el -- contains all major use-package forms -*- lexical-binding: t -*-
;;; Commentary:

;; Provides my collection of use-package forms and related
;; configuration for loading and configuring all Emacs packages.

;; This configuration optimizes for lazy-loading so that configuration
;; only loads when called directly or a mode is activated.

;;; Code:

;; (setq use-package-expand-minimally t)
;; (setq use-package-verbose t)
(setq use-package-compute-statistics t)
(setq use-package-minimum-reported-time 0.5)

(use-package async
  :ensure t
  :defer t
  :delight
  (async-bytecomp-package-mode "")
  (dired-async-mode "")
  :commands (async-start
	     async-start-process
	     async-bytecomp-package-mode
	     dired-async-mode)
  :init
  (add-hook 'package--post-download-archives-hook 'async-bytecomp-package-mode)
  (add-hook 'dired-mode-hook 'dired-async-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; UI, Display, Rendering, Window Management

(use-package modus-themes
  :ensure t
  :defer t
  :defines (modus-themes-deuteranopia modus-themes-common-palette-overrides)
  :init
  (add-to-list 'term-file-aliases '("alacritty" . "xterm"))

  (let ((theme-directory (concat (expand-file-name user-emacs-directory) "theme")))
    (setq-default custom-theme-directory theme-directory)
    (add-to-list 'custom-theme-load-path theme-directory)
    (add-to-list 'load-path theme-directory))

  (setq modus-themes-deuteranopia t)
  (setq modus-themes-common-palette-overrides
        '((border-mode-line-active bg-mode-line-active)
          (border-mode-line-inactive bg-mode-line-inactive)
          (message-separator bg-main))))

;; (use-package modus-themes-exporter
;;   :after modus-themes
;;   :commands (modus-themes-exporter-export))

(use-package nerd-icons
  :ensure t
  :defer t)

(use-package nerd-icons-dired
  :ensure t
  :delight (nerd-icons-dired-mode "")
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package doom-modeline
  :ensure t
  :commands (doom-modeline-mode
             tychoish-legacy-mode-line)
  :defines (doom-modeline-icon)
  :init
  (create-toggle-functions
   doom-modeline-icon
   :keymap tychoish/theme-map
   :key "i")

  (add-hygenic-one-shot-hook
   :name "doom-modeline"
   :function doom-modeline-mode
   :hook (if (daemonp)
	     'server-after-make-frame-hook
	   'window-setup-hook))
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)
  (setq find-file-visit-truename t)
  (setq size-indication-mode t)
  (setq doom-modeline-buffer-file-name-style 'relative-to-project)
  (setq doom-modeline-height 0)
  (setq doom-modeline-bar-width 1)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-unicode-fallback nil)
  (setq doom-modeline-minor-modes t)
  (setq doom-modeline-enable-word-count t)
  (setq doom-modeline-continuous-word-count-modes '(text-mode))
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-env-version nil)
  (setq doom-modeline-irc-stylize 'identity)
  (setq doom-modeline-irc t)

  (defun my-doom-modeline--font-height ()
    "Calculate the actual char height of the mode-line."
    (/ (frame-char-height) 4))

  (advice-add #'doom-modeline--font-height :override #'my-doom-modeline--font-height))

(use-package winum
  :ensure t
  :bind (("C-x w n" . winum-select-window-by-number)
         ("C-x w w" . winumw-mode))
  :commands (winum-mode)
  :config
  (setq winum-auto-setup-mode-line nil)
  (setq winum-scope 'frame-local))

(use-package writeroom-mode
  :ensure t
  :bind (:map tychoish/display-map
	 ("i" . writeroom-mode)))

(use-package page-break-lines
  :ensure t
  :delight page-break-lines-mode
  :hook ((text-mode prog-mode) . page-break-lines-mode)
  :commands (global-page-break-lines-mode)
  :config
  (setq page-break-lines-modes '(text-mode prog-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Project / Repository Tools

(use-package projectile
  :ensure t
  :delight '(:eval (tychoish-projectile-modeline-string))
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (:map tychoish/ecclectic-grep-project-map
         ("a" . projectile-ag)
         ("r" . projectile-ripgrep)
         ("p" . projectile-grep))
  :commands (projectile-mode
	     projectile-project-name
             projectile-project-root
             projectile-mode-on
             projectile-save-project-buffers)
  :init
  (which-key-add-key-based-replacements "C-c p" "projectile")
  ;; previously added projectile-mode to the after-init-hook (probably
  ;; to get keybindings to load correctly,) appears unnecessary, but
  ;; wanted to leave note here.
  (defun tychoish/save-buffers-in-project-directory (dir)
    (let ((default-directory (expand-file-name dir)))
      (alert (projectile-save-project-buffers)
             :title (format "emacs<%s> (%s)" tychoish/emacs-instance-id dir))))

  (defun projectile-get-project-roots-for-current-buffers ()
    (->>
     (buffer-list)
     (--map #'buffer-file-name)
     (-non-nil)
     (--map #'f-dirname)
     (-sort #'string<)
     (--map #'projectile-project-root)
     (-distinct)
     (-non-nil)))

  (defun tychoish/save-project-buffers ()
    (interactive)
    (tychoish/save-buffers-in-project-directory
     (completing-read "directories: " (projectile-get-project-roots-for-current-buffers))))

  (defun tychoish-projectile-modeline-string ()
    (let ((pname (projectile-project-name)))
      (if (equal pname "-")
          ""
        (concat " p:" pname))))
  :config
  (setq projectile-enable-caching t)
  (setq projectile-use-git-grep t)
  (setq projectile-completion-system 'auto)
  (setq projectile-require-project-root nil)
  (setq projectile-known-projects-file (tychoish/conf-state-path "projectile-bookmarks.el")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; grep/search

(use-package git-grep
  :ensure t
  :bind (:map tychoish/ecclectic-grep-project-map
         ("g" . git-grep)))

(use-package ripgrep
  :ensure t
  :bind (:map tychoish/ecclectic-grep-map ;; "C-c g"
         :prefix "r"
         :prefix-map tychoish/ecclectic-rg-map ;; "C-c g r"
         ("d" . tychoish-rg)
         ("c" . consult-rg-compile)
         ("m" . tychoish-find-merges)
         :map tychoish/ecclectic-grep-project-map
         ("r" . consult-rg-compile)
         ("p" . tychoish-rg-repo))
  :defines (tychoish/ecclectic-rg-map)
  :commands (tychoish-rg tychoish-rg-repo tychoish-find-merges ripgrep-regexp)
  :init
  (which-key-add-keymap-based-replacements tychoish/ecclectic-grep-map
    "r" '("rg-grep" . tychoish/ecclectic-rg-map))
  :config
  (setenv "RIPGREP_CONFIG_PATH" (f-expand "~/.ripgreprc"))
  (defvar ripgrep-regexp-history nil)

  (defun consult-rg-compile (&optional initial)
    (interactive "P")
    (let ((default-directory (consult--select-directory)))
      (tychoish-rg initial)))

  (defun tychoish-rg (regexp)
    (interactive "P")
    (let ((compilation-buffer-name-function (compile-buffer-name (format "*%s-rg*" (approximate-project-name)))))
      (ripgrep-regexp
       (consult--read
	(consult-tycho--context-base-list ripgrep-regexp-history)
	:prompt (format "[%s]<rg>: " default-directory)
	:command this-command
	:initial regexp
	:history ripgrep-regexp-history
	:require-match nil)
       default-directory)))

  (defun tychoish-rg-repo (&optional regexp)
    (interactive "P")
    (let ((default-directory (approximate-project-root)) )
      (tychoish-rg regexp)))

  (defun tychoish-find-merges ()
    (interactive)
    (ripgrep-regexp "^(=======$|<<<<<<<|>>>>>>>)" (projectile-project-root))))

(use-package deadgrep
  :ensure t
  :bind (:map tychoish/ecclectic-grep-map
         ("d" . #'deadgrep)
         :map tychoish/ecclectic-rg-map
	 ("x" . #'deadgrep))
  :commands (deadgrep))

(use-package wgrep
  :ensure t
  :after (grep)
  :bind (:map grep-mode-map
         ("r" . wgrep-change-to-wgrep-mode))
  :config
  (setq wgrep-enable-key "r"))

(use-package google-this
  :ensure t
  :delight google-this-mode
  :bind-keymap ("C-c /" . google-this-mode-submap)
  :commands (google-this-mode)
  :config
  (which-key-add-key-based-replacements "C-c /" "google-this")
  (setq google-this-browse-url-function 'browse-url-default-browser)
  (google-this-mode 1))

(use-package anzu
  :ensure t
  :delight anzu-mode
  :commands (anzu-query-replace anzu-query-replace-regexp global-anzu-mode anzu-mode)
  :hook ((isearch-mode) . anzu-mode)
  :bind (:prefix "C-c q"
	 :prefix-map tychoish/anzu-map
	 ("r" . anzu-query-replace)
         ("e" . anzu-query-replace-regexp)
         :map isearch-mode-map
         ("C-o" . isearch-occur)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Completion/Snippets Menus

(use-package capf-wordfreq
  :load-path "external/"
  :commands (capf-wordfreq-completion-at-point-function capf-wordfreq--dictionary)
  :init
  (defun capf-wordfreq-avalible-p ()
    (and (fboundp 'capf-wordfreq-completion-at-point-function)
	 (fboundp 'capf-wordfreq--dictionary)
	 (f-exists-p (capf-wordfreq--dictionary))))
  (setq capf-wordfreq-minimal-candidate-length 5))

(use-package cape
  :ensure t
  :bind (:map tychoish/completion-map ;; "C-c ."
         ;; this is mostly copy-pasta from cape-mode-map, with tweaks
         ("t" . complete-tag)
         ("d" . cape-dabbrev)
         ("h" . cape-history)
         ("f" . cape-file)
         ("n" . cape-elisp-symbol)
         ("b" . cape-elisp-block)
         ("a" . cape-abbrev)
         ("l" . cape-line)
         ("w" . cape-dict)
         ("k" . cape-keyword)
         (":" . cape-emoji)
         ("e" . cape-emoji)
         ("\\" . cape-tex)
         ("/" . cape-sgml)
         ("u" . cape-rfc1345))
  :init
  (declare-function capf-wordfreq-avalible-p "tychoish-core")
  (declare-function cape-capf-inside-code "cape")

  (defun tychoish/maybe-capf-dict ()
    (and (boundp 'cape-dict-file) (f-exists-p cape-dict-file) #'cape-dict))

  (defun tychoish/maybe-capf-wordfreq ()
    (disabled (when (capf-wordfreq-avalible-p)
		#'capf-wordfreq-completion-at-point-function)))

  (defun tychoish/get-available-word-capfs ()
    (->> (list (tychoish/maybe-capf-wordfreq)
	       (tychoish/maybe-capf-dict))
	 (-non-nil)
	 (-filter #'symbolp)))

  (defun tychoish/text-mode-capf-setup ()
    "so here is the"
    (setq-local completion-at-point-functions
		(->> (-concat
		      (tychoish/get-available-word-capfs)
		      (list #'cape-dabbrev
			  #'yasnippet-capf
			  #'cape-rfc1345
			  #'cape-emoji
			  #'cape-file))
		     (-flatten)
		     (-non-nil))))

  (defun tychoish/elisp-capf-setup  ()
    (require 'cape)
    (setq-local completion-at-point-functions
                (->> (list #'cape-elisp-symbol
			   (cape-capf-wrapper cape-capf-inside-code cape-elisp-block)
			   #'cape-dabbrev
			   (cape-capf-wrapper cape-capf-inside-code cape-keyword)
			   #'yasnippet-capf
			   (->> (tychoish/get-available-word-capfs)
				(-map (lambda (in)
					`(progn
					   (list (cape-capf-wrapper cape-capf-inside-comment ,in)
					   (cape-capf-wrapper cape-capf-inside-string ,in)))))
				(-map 'eval))
			   #'cape-emoji)
		     (-flatten)
		     (-non-nil)
		     (-distinct))))

  (defun tychoish/eglot-capf-setup ()
    (interactive) ;; todo remove
    (setq-local completion-category-defaults nil)
    (setq-local completion-at-point-functions
                (-> (list #'eglot-completion-at-point
			  #'cape-dabbrev
			   (->> (tychoish/get-available-word-capfs)
				(-map (lambda (in)
					`(progn
					   (list (cape-capf-wrapper cape-capf-inside-comment ,in)
					   (cape-capf-wrapper cape-capf-inside-string ,in)))))
				(-map 'eval))
			  #'yasnippet-capf
			  #'cape-emoji
			  #'cape-file)
		     (-flatten)
		     (-non-nil)
		     (-distinct))))

  (defun cape--project-buffers ()
    (let ((directory (approximate-project-root)))
      (cape--buffer-list (lambda (buf)
			   (string-prefix-p directory (buffer-file-name buf))))))

  (with-eval-after-load 'eglot
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

  (add-hook 'eglot-managed-mode-hook #'tychoish/eglot-capf-setup)
  (add-hook 'emacs-lisp-mode-hook #'tychoish/elisp-capf-setup)
  (add-hook 'telega-chat-mode-hook #'tychoish/text-mode-capf-setup)
  (add-hook 'text-mode-hook #'tychoish/text-mode-capf-setup)

  (declare-function yasnippet-capf "yasnippet-capf")
  (add-hook 'completion-at-point-functions #'yasnippet-capf)
  (add-hook 'completion-at-point-functions (cape-capf-wrapper cape-capf-inside-code cape-keyword))
  (add-hook 'completion-at-point-functions #'cape-rfc1345)
  (add-hook 'completion-at-point-functions #'cape-emoji)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-history))

(use-package yasnippet
  :ensure t
  :delight (yas-minor-mode " ys")
  :commands (yas-global-mode yas-insert-snippet yas-minor-mode yas-expand-snippet yas-lookup-snippet)
  :hook ((text-mode prog-mode) . yas-minor-mode)
  :config
  (add-to-list 'load-path (f-join user-emacs-directory "snippets"))
  (which-key-add-key-based-replacements "C-c &" "yasnippet"))

(use-package yasnippet-capf
  :ensure t
  :bind (:map tychoish/completion-map
         ("s" . yasnippet-capf))
  :commands (yasnippet-capf))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package prescient
  :ensure t
  :commands (prescient-persist-mode)
  :init
  (add-hygenic-one-shot-hook
   :name "prescient"
   :operation 'prescient-persist-mode
   :hook '(vertico-mode-hook corfu-mode-hook))
  :config
  (setq prescient-filter-method '(literal prefix initialism anchored fuzzy regexp))
  (setq prescient-sort-full-matches-first t)
  (setq prescient-completion-highlight-matches t)
  (setq prescient-sort-length-enable nil)
  (setq completion-preview-sort-function #'prescient-completion-sort)
  (setq prescient-save-file (tychoish/conf-state-path "prescient.el")))

(use-package vertico
  :ensure t
  :defines (vertico-multiform-categories vertico-sort-function vertico-multiform-commands)
  :commands (vertico-mode)
  :init
  (add-hygenic-one-shot-hook
   :name "vertico"
   :operation 'vertico-mode
   :hook 'doom-modeline-mode-hook)

  (add-hook 'vertico-mode-hook 'vertico-multiform-mode)

  (setq vertico-resize t)
  (setq vertico-count 25)
  (setq vertico-cycle t)
  :config
  (defvar vertico-multiform-categories nil)
  (defvar vertico-multiform-commands nil)
  (defmacro tychoish/vertico-disable-sort-for (command)
    "Disable sorting in vertico rendering."
    `(add-to-list 'vertico-multiform-categories '(,command (vertico-sort-function . nil))))

  (tychoish/vertico-disable-sort-for yank)
  (tychoish/vertico-disable-sort-for yank-from-kill-ring)
  (tychoish/vertico-disable-sort-for consult-yank-from-kill-ring)
  (tychoish/vertico-disable-sort-for consult-yank-pop)

  (add-to-list 'vertico-multiform-commands
	       '("\\`execute-extended-command"
                 (vertico-flat-annotate . t)
                 (marginalia-annotators (command marginalia-annotate-command marginalia-annotate-binding)))))

(use-package vertico-prescient
  :ensure t
  :hook (vertico-mode . vertico-prescient-mode)
  :config
  (setq vertico-prescient-override-sorting t)
  (setq vertico-prescient-enable-sorting t)
  (setq vertico-prescient-enable-filtering t))

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
         ("C-c a" . marginalia-cycle))
  :commands (marginalia-mode)
  :init
  (add-hygenic-one-shot-hook
   :name "marginalia"
   :function marginalia-mode
   :hook 'minibuffer-setup-hook)
  :config
  (add-to-list 'marginalia-command-categories '(consult-completion-in-region . imenu)))

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)         ;; pick some comfortable binding
	 ("C-c H" . embark-bindings) ;; alternative for `describe-bindings'
	 ("C-;" . embark-dwim))        ;; good alternative: M-.
  :config
  (setq embark-prompter #'embark-keymap-prompter)
  ;; (prefix-help-command #'embark-prefix-help-command)
  ;; (eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  (setq embark-quit-after-action '((kill-buffer . nil)))
  :config
  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid))
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package corfu
  :ensure t
  :defines (corfu-margin-formatters corfu-continue-commands corfu-popupinfo--function)
  :bind (:map tychoish/completion-map
	 ("m" . corfu-at-point)
	 ("x" . corfu-at-point))
  :hook (((prog-mode text-mode) . corfu-mode)
         ((shell-mode eshell-mode eat-mode) . corfu-mode)
         (telega-chat-mode . corfu-mode))
  :init
  (defun tychoish/corfu-text-mode-setup ()
    (setq-local corfu-auto-prefix 2))

  (defun tychoish/corfu-prog-mode-setup ()
    (setq-local corfu-auto-prefix 3))

  (defun corfu-at-point ()
    "Run `completion-at-point', but force using corfu, which may be useful in gui terminals"
    (interactive)
    (let ((completion-in-region-function #'corfu--in-region))
      (completion-at-point)))

  (add-hook 'text-mode-hook 'tychoish/corfu-text-mode-setup)
  (add-hook 'prog-mode-hook 'tychoish/corfu-prog-mode-setup)

  (add-hook 'corfu-mode-hook 'corfu-history-mode)
  (add-hook 'corfu-mode-hook 'corfu-indexed-mode)
  (add-hook 'corfu-mode-hook 'corfu-popupinfo-mode)
  :config
  (bind-keys :map corfu-map
             ("C-<tab>" . corfu-quick-complete)
             ([tab] . corfu-complete)
             ("<return>" . corfu-insert)
             ("C-<return>" . corfu-quick-insert)
             ("M-SPC" . corfu-insert-separator)
             ("C-SPC" . corfu-insert-separator)
             ([backtab] . corfu-insert-separator)
             ("C-j" . corfu-quick-jump)
             ("C-j" . corfu-quick-jump)
             ("M-d" . corfu-popupinfo-toggle)
             ("C-i" . corfu-popupinfo-toggle)
             ("M-m" . corfu-move-to-minibuffer)
             ("C-m" . corfu-move-to-minibuffer)
             ("M-S-m" . corfu-move-to-minibuffer))

  (setq corfu-cycle t)
  (setq corfu-quit-at-boundary t)
  (setq corfu-quit-no-match t)
  (setq corfu-preview-current t)
  (setq corfu-preselect nil)
  (setq corfu-on-exact-match nil)
  (setq corfu-auto t)
  (setq corfu-auto-delay 0.15)
  (setq corfu-popupinfo-delay .125)
  (setq corfu-indexed-start 1)
  (setq global-corfu-minibuffer nil)
  (setq read-file-name-completion-ignore-case t)
  (setq read-buffer-completion-ignore-case t)
  (setq completion-ignore-case t)

  (defun corfu-move-to-minibuffer ()
    (interactive)
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
       (let ((completion-extra-properties extras)
	     (this-command 'consult-completion-in-region)
             completion-cycle-threshold
	     completion-cycling)
         (consult-completion-in-region beg end table pred))))))

(use-package corfu-prescient
  :ensure t
  :after (prescient)
  :hook (corfu-mode . corfu-prescient-mode)
  :config
  (setq corfu-prescient-override-sorting t)
  (setq corfu-prescient-enable-sorting t)
  (setq corfu-prescient-enable-filtering t))

(use-package popon
  :ensure t
  :commands (popon-kill popon-create popon-x-y-at-posn))

(use-package corfu-terminal
  :load-path "external/"
  :commands (corfu-terminal-mode)
  :init
  (add-hook 'corfu-mode-hook #'corfu-terminal-mode)
  :config
  (setq corfu-terminal-disable-on-gui t)
  (setq corfu-terminal-enable-on-minibuffer nil))

(use-package nerd-icons-corfu
  :ensure t
  :after (corfu nerd-icons)
  :commands (nerd-icons-corfu-formatter)
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-xref
  :ensure t
  :hook (nerd-icons-completion-mode . nerd-icons-xref-mode))

(use-package nerd-icons-completion
  :ensure t
  :hook ((marginalia-mode . nerd-icons-completion-marginalia-setup))
  :commands (nerd-icons-completion-mode)
  :init
  (add-hygenic-one-shot-hook
   :name "nerd-icons-completion"
   :operation #'nerd-icons-completion-mode
   :hook '(corfu-mode-hook vertico-mode-hook)))


(use-package consult
  :ensure t
  :bind (("C-c C-x C-m" . consult-mode-command)
         ("C-c i" . consult-info)
         ("C-c C-; m" . consult-kmacro)
         ("C-c C-x r r" . consult-register)
         ("C-c C-x r s" . consult-register-store)
         ("C-c C-x r l" . consult-register-load)
         ("C-c e" . consult-compile-error)
         ("C-c C-s" . consult-line)
         ("C-c f C-f" . consult-find)
         ;; tychoish/wacky
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x C-b" . consult-buffer)            ;; orig. list-buffers
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-from-kill-ring)                ;; orig. yank-pop
         ("C-M-I" . consult-imenu-multi)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g s" . consult-line)
         ("M-i" . consult-imenu)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-find
         ("M-s c" . consult-locate)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
	 :map tychoish/completion-map
	 ("r" . consult-at-point)
	 ("c" . consult-at-point)
	 :map tychoish/docs-map
         ("m" . consult-man)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history)                 ;; orig. previous-matching-history-element
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi))            ;; needed by consult-line to detect isearch
  :bind (:prefix "C-c C-;"
         :prefix-map tychoish/consult-mode-map
         ("h" . consult-history))
  :bind (:map tychoish/ecclectic-grep-map ;; "C-c g"
         ("f" . consult-grep)
	 :map tychoish/ecclectic-grep-map ;; "C-c g"
	 :prefix "s"
	 :prefix-map tychoish/consult-search-map
         ("l" . consult-line)
         ("m" . consult-line-multi)
         ("k" . consult-keep-lines)
         ("f" . consult-focus-lines)
	 :map tychoish/ecclectic-grep-project-map ;; "C-c g p"
         ("g" . consult-git-grep))                 ;; for git(?)
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :functions (consult-xref consult--read consult-completion-in-region consult-register-window)
  :defines (consult-preview-key)
  :commands (consult-find consult-git-grep consult-grep)
  :init
  (defun consult-at-point ()
    "Run `completion-at-point', but force using consult, which may be useful in tty terminals"
    (interactive)
    (let ((completion-in-region-function #'consult-completion-in-region))
      (completion-at-point)))
  :config
  (setq register-preview-delay 0.05)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref)

  (setq consult-narrow-key "C-+")
  (setq consult-async-split-style 'semicolon)
  (setq consult-async-min-input 2)
  (setq consult-async-input-debounce 0.05)
  (setq consult-async-input-throttle 0.125)
  (setq consult-async-refresh-delay 0.05)
  (setq consult-project-function #'projectile-project-root)

  (setq consult-preview-key '("M-." "M-?" :debounce 0))

  (add-to-list 'consult-mode-histories '(compilation-mode compile-history))

  (advice-add #'register-preview :override #'consult-register-window)

  ;; (setq completion-in-region-function #'consult-completion-in-region)
  ;; (setq completion-in-region-function #'corfu--in-region)

  (defun consult-ripgrep--up-directory ()
    (interactive)
    (let ((parent-dir (file-name-directory (directory-file-name default-directory))))
      (when parent-dir
        (run-at-time 0 nil
                     #'consult-ripgrep
                     parent-dir
                     (ignore-errors
                       (buffer-substring-no-properties
                        (1+ (minibuffer-prompt-end)) (point-max))))))
    (minibuffer-quit-recursive-edit))

  (consult-customize
   consult-yank-from-kill-ring consult-yank-pop consult-yank-replace
   :preview-key 'any
   :sort nil)

  (consult-customize consult-find
   :require-match nil
   :initial (or (thing-at-point 'existing-filename)
		(thing-at-point 'filename)
		"./"))

  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-rg consult-rg-project consult-rg-pwd consult-rg-for-thing consult-rg-project-wizard consult-rg-pwd-wizard
   :require-match nil
   :group nil
   :keymap
   (with-temp-keymap map
     (define-key map (kbd "C-l") #'consult-ripgrep-up-directory))))

(use-package consult-flycheck
  :ensure t
  :bind (("M-g f" . flycheck)
	 :map tychoish/consult-mode-map
	 ("c" . consult-flycheck)
	 :map flycheck-command-map
	 ("\;" . consult-flycheck)))

(use-package consult-flyspell
  :ensure t
  :after (flyspell)
  :bind (:map tychoish/consult-mode-map
         ("f" . consult-flyspell))
  :commands (consult-flyspell flyspell-correct-consult)
  :init
  (defun consult-flyspell--round-trip ()
    (flyspell-correct-at-point)
    (consult-flyspell))
  (setq consult-flyspell-select-function 'consult-flyspell--round-trip))

(use-package consult-eglot
  :ensure t
  :after (eglot)
  :bind (:map tychoish/docs-map
         ("a" . consult-eglot-symbols))
  :commands (consult-eglot-symbols))

(use-package consult-gh
  :ensure t
  :commands (consult-gh))

(use-package consult-yasnippet
  :ensure t
  :after (yasnippet)
  :bind (:map tychoish/consult-mode-map
         ("s" . consult-yasnippet)
	 :map tychoish/completion-map
	 ("s" . consult-yasnippet)))

(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult-builder
  :after (compile)
  :bind (:map compilation-mode-map
         ("d" . compilation-buffer-change-directory))
  :commands (consult--select-directory
	     make-compilation-candidate
	     register-compilation-candidates
	     tychoish--compilation-read-command
	     tychoish/compile-project))

(use-package consult-tycho
  :bind (("M-g r" . consult-rg)
	 :map tychoish-core-map
	 ("r" . consult-sardis-run)
	 :map tychoish/consult-mode-map ;; "C-c C-;"
         ("d" . consult-rg-pwd)
	 ("r" . consult-rg)
	 :map tychoish/global-org-map
	 ("j" . consult-org-capture)
	 ("c" . consult-org-capture)
 	 :map tychoish/ecclectic-rg-map ;; C-c g r
         ("g" . consult-rg)
         ("s" . consult-rg-pwd)
         ("l" . consult-rg-pwd-wizard)
         ("r" . consult-rg-project)
         ("p" . consult-rg-project-wizard)
	 :map tychoish-core-map
         :prefix "b"
	 :prefix-map tychoish/blogging-map
	 ("m" . tychoish-insert-date)
         ("p" . tychoish-blog-publish-post)
         ("n" . tychoish-blog-create-post)
         ("d" . tychoish-blog-open-drafts-dired))
  :commands (consult-rg-for-thing
             consult-rg
	     consult-tycho--read-annotated
	     tychoish-define-project-notes
	     get-directory-parents
             consult-org-capture
             consult-org-capture-target))

(use-package revbufs
  :ensure t
  :bind (:prefix "C-c b"
	 :prefix-map tychoish/buffer-control-map
	 ("r" . revbufs))
  :commands (revbufs)
  :config
  (bind-key "C-k" 'revbufs-kill 'revbufs-mode-map)
  (defalias 'revbufs-kill
   (kmacro "C-f C-f C-f C-k")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; version control

(use-package magit
  :ensure t
  :bind (:prefix "C-x g"
	 :prefix-map tychoish/magit-map
	 ("s" . magit-status)
         ("f" . magit-branch)
         ("b" . magit-blame))
  :commands (magit-toplevel)
  :config
  (setq vc-follow-symlinks t)
  (setq version-control t)
  (setq magit-auto-revert-mode nil)
  (setq magit-module-sections-nested nil)
  (magit-auto-revert-mode -1)
  (put 'magit-diff-edit-hunk-commit 'disabled nil)
  (add-to-list 'magit-status-sections-hook 'magit-insert-modules t))

(use-package emacsql
  :ensure t
  :defer t)

(use-package forge
  :ensure t
  :commands (forge-dispatch forge-configure)
  :config
  (setq forge-database-file (expand-file-name (f-join user-emacs-directory tychoish/conf-state-directory-name "state" "forge-database.sqlite"))))

(use-package gist
  :ensure t
  :commands (gist-region gist-buffer gist-list gist-region-private gist-buffer-private))

(use-package git-link
  :ensure t
  :bind (:map tychoish/magit-map
	 ("l" . git-link)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; chat and notifications

(use-package alert
  :ensure t
  :commands (alert)
  :defines (alert-styles alert-libnotify-command)
  :config
  (setq alert-log-messages nil)
  (setq alert-fade-time 15)

  (cond
   ((and (eq system-type 'darwin))
    (cond
     ((window-system)
      (setq-default alert-default-style 'osx-notifier))
     ((daemonp)
      (setq alert-default-style 'notifier)
      (add-hook 'server-after-make-frame-hook 'tychoish/darwin-alert-config-for-server))
     (t
      (setq-default alert-default-style 'notifier))))
   (alert-libnotify-command
      (setq-default alert-default-style 'libnotify))
   ((eql system-type 'gnu/linux)
    (setq-default alert-default-style 'notifications))
   (t (setq-default alert-default-style 'message)))

  (defun tychoish/darwin-alert-config-for-server ()
    (if (and (window-system)
	     (eq system-type 'darwin)
	     (not (eq alert-default-style 'osx-notifier)))
	(setq-default alert-default-style 'osx-notifier)
      (remove-hook 'server-after-make-frame 'tychoish/darwin-alert-config-for-server)
      (unintern 'tychoish/darwin-alert-config-for-server obarray))))

(use-package emojify
  :ensure t
  :commands (global-emojify-mode)
  :delight emojify-mode
  :config
  (setq emojify-emoji-styles '(ascii unicode github))
  (setq emojify-display-style 'unicode)
  (setq emojify-point-entered-behaviour 'echo))

(use-package tracking
  :ensure t
  :after (:any telega erc))

(use-package telega
  :ensure t
  :delight
  (telega-chat-auto-fill-mode "")
  :defines (telega-chat-mode-hook)
  :bind-keymap (("C-c v" . telega-prefix-map)
                ("C-c n" . telega-prefix-map))
  :bind (:map telega-prefix-map
         :prefix "d"
         :prefix-map tychoish/telega-buffer-management-map
         ("h" . tychoish/telega-bury-chat-buffers)
         ("k" . tychoish/telega-kill-chat-buffers)
         :map telega-chat-mode-map
         ("C-c C-f" . telega-chat-buffer-auto-fill)
         :map telega-root-mode-map
         ("C-c C-f" . telega-root-buffer-auto-fill)
         ("<tab>" . telega-root-cycle-next))
  :commands (telega
             telega-chat-mode
             tychoish/telega-switch-to-root
             tychoish/telega-kill-chat-buffers
             tychoish/telega-bury-chat-buffers
             tychoish/telega-force-kill)
  :init
  (which-key-add-key-based-replacements "C-c n" "telega-prefix")
  (which-key-add-key-based-replacements "C-c v" "telega-prefix")
  :config
  (add-hook 'telega-load-hook 'tychoish/make-telega-root-default-buffer)
  (add-hook 'telega-kill-hook 'tychoish/remove-telega-root-as-default-buffer)
  (add-hook 'telega-chat-mode-hook 'tychoish/telega-set-up-chat-mode)

  (when (eq system-type 'darwin)
    (setq telega-server-libs-prefix "/opt/homebrew")
    (setq-default alert-default-style 'osx-notifier))

  (when (eq system-type 'gnu/linux)
      (setq telega-server-libs-prefix "/usr"))

  (setq telega-emoji-use-images t)

  (setq telega-root-view-grouping-folders t)
  (setq telega-folder-icons-alist nil)

  (setq telega-root-view-grouping-other-chats nil)
  (setq telega-root-view-grouping-alist
        '(("open" . has-chatbuf)
          ("personal" chat-list "personal")
          ("groups" type basicgroup supergroup)))

  (setq telega-filter-custom-show-folders nil)
  (setq telega-filter-button-width
        '(.2 12 16))
  (setq telega-filters-custom
        '(("main" main)
          ("groups" type basicgroup supergroup)
          ("broadcast" type channel)
          ("archive" . archive)))

  (setq telega-root-view-top-categories
        '(("Users" . 10) ("Groups" . 10) ("Channels" . 10) ("Bots" . 10)
          ("InlineBots" . 10) ("Calls" . 10)))
  (setq telega-chat-folders-exclude
        '("unknowns" "InlineBots" "Calls" "Personal"))

  (setq telega-chat-folders-insexp #'telega-folders-insert-default)

  (setq telega-use-images t)
  (setq telega-chat-input-markups '("markdown2"))
  (setq telega-use-tracking-for '(or unmuted mention unread))
  (setq telega-markdown2-backquotes-as-precode t)
  (setq telega-debug nil)

  (setq telega-chat--display-buffer-action
        '((display-buffer-reuse-window display-buffer-use-some-window)))

  (require 'telega-alert)
  (telega-mode-line-mode 1)
  (telega-alert-mode 1)

  (defun telega-chat-folders (_chat) nil)

  (defun telega-root-cycle-next (chat)
    "Either expand if forum or cycle to next `CHAT' at point."
    (interactive (list (telega-chat-at (point))))

    (if (telega-chat-match-p chat 'is-forum)
        (telega-chat-button-toggle-view chat)
      (ignore-errors
        (telega-root-next-important (point))
        (telega-root-next-mention (point))
        (telega-root-next-reaction (point))
        (telega-root-next-unread (point)))))

  (defun tychoish/telega-set-up-chat-mode ()
    ;; (require 'telega-mnz)
    ;; (telega-mnz-mode 1)
    (telega-chat-auto-fill-mode 1))

  (defun telega-toggle-debug ()
    (interactive)
    (setq telega-debug (not telega-debug))
    (if telega-debug
        (message "telega-debug mode enabled")
      (message "telega-debug mode disabled")))

  (defun telega--chat-observable-p (msg)
    (let ((chat (telega-msg-chat msg)))
      (with-telega-chatbuf chat
        (and (telega-chatbuf--msg-observable-p msg)
             (not (telega-chatbuf--history-state-get :newer-freezed))))))

  (defun telega-notifications-msg-notify-p (msg)
    "tychoish's custom override for notificationable"
    (let* ((chat (telega-msg-chat msg))
           (title (plist-get chat :title)))
      (cond
       ;; chat window is open and viable: skip notify
       ((telega--chat-observable-p msg)
        (progn (telega-debug "NOTIFY-CHECK: observed chat [%s], skip notify" title) nil))

       ;; if it's muted: skip notify
       ((telega-chat-muted-p chat)
        (progn (telega-debug "NOTIFY-CHECK: muted chat [%s], skip notify" title) nil))

       ;; overly clear, but for groupchats where I am not a member: skip notify
       ;; after: https://github.com/zevlg/ytelega.el/issues/224
       ((telega-chat-match-p chat '(and (type basicgroup supergroup channel) (not me-is-member)))
        (progn (telega-debug "NOTIFY-CHECK: group chat where I am not a member [%s], skip notify" title) nil))

       ;; message I sent (from another device): skip notify
       ((telega-msg-match-p msg '(sender me))
        (progn (telega-debug "NOTIFY-CHECK: message I sent [%s], skip notify" title) nil))

       ;; message that is a mention but notification of mentions are
       ;; disabled: skip notify
       ((and
         (plist-get msg :contains_unread_mention)
         (telega-chat-notification-setting chat :disable_mention_notifications))
        (progn (telega-debug "NOTIFY-CHECK: contains a mention [%s], notify" title) nil))

       ;; notify for messages in chats that are directly sent to me, including bots
       ((telega-chat-match-p chat '(or (type private secret bot)))
        (progn (telega-debug "NOTIFY-CHECK: is DM or BOT [%s], can notify" title) t))

       ;; for things that are a group, and I am a member, notify
       ((telega-chat-match-p chat 'me-is-member)
        (progn (telega-debug "NOTIFY-CHECK: member of a group [%s], can notify" title) t))

       ;; nothing has matched, this is probably "cases we haven't
       ;; explicitly called out above, probably a skip, but should be
       ;; explict:" notify for now
       (t
        (progn (message (format "TELEGA-NOTIFY: unexpected message [%s], notifying anyway" title)) t)))))

  (defun telega-chat-buf-mode-p (buffer)
    "returns `t' for all chat buffers, and `nil' otherwise"
    (with-current-buffer buffer
      (when (eq major-mode 'telega-chat-mode)
        t)))

  (defun tychoish/telega-bury-chat-buffers ()
    "iterates through all currently visable frames and windows and changes
all visable `telega-chat-mode buffers' to the `*Telega Root*` buffer."
    (interactive)
    (let ((target-buffer (get-buffer "*Telega Root*"))
          (count 0))
      (when target-buffer
        (dolist (frame (frame-list))
          (dolist (window (window-list frame))
            (let ((buf (window-buffer window)))
              (when (telega-chat-buf-mode-p buf)
                (bury-buffer buf)
                (set-window-buffer window target-buffer)
                (setq count (+ count 1)))))))
      (unless (zerop count)
        (alert (format "burried %d telega-chat-%s" 1
                       (if (= 1 1)
                           "buffer"
                         "buffers"))
               :title (format "emacs.%s.telega" tychoish/emacs-instance-id)
               :persistent t))))

  (defun tychoish/telega-kill-chat-buffers ()
    (interactive)
    (kill-buffers-matching-mode 'telega-chat-mode))

  (defun tychoish/telega-force-kill ()
    (interactive)
    (telega-kill t))

  (defun tychoish/make-telega-root-default-buffer ()
    (add-hook 'after-make-frame-functions #'tychoish/telega-switch-to-root)
    (add-hook 'server-after-make-frame-hook #'tychoish/telega-switch-to-root)
    (setq initial-buffer-choice #'tychoish/telega-switch-to-root))

  (defun tychoish/remove-telega-root-as-default-buffer ()
    (remove-hook 'after-make-frame-functions #'tychoish/telega-switch-to-root)
    (remove-hook 'server-after-make-frame-hook #'tychoish/telega-switch-to-root)
    (setq initial-buffer-choice nil))

  (defun tychoish/telega-switch-to-root ()
    (interactive)
    (switch-to-buffer (or (get-buffer telega-root-buffer-name)
			  (when (bufferp initial-buffer-choice) initial-buffer-choice)
			  (when (stringp initial-buffer-choice) (get-buffer initial-buffer-choice))
			  (last-buffer)
			  (get-buffer "*scratch*"))))

  (defun tychoish/telega-default-root-buffer (toggle)
    (setq initial-buffer-choice
          (if toggle
              #'tychoish/telega-switch-to-root
             nil))
    (when (daemonp)
      (if toggle
          (add-hook 'server-after-make-frame-hook  #'tychoish/telega-switch-to-root)
        (remove-hook 'server-after-make-frame-hook #'tychoish/telega-switch-to-root))
      (message "%sset telega-root as the default buffer for the [%s] daemon" (if toggle "" "un") (daemonp)))
    toggle)

  (defun tychoish/toggle-root-buffer-default ()
    (interactive)
    (let ((toggle (not (eq initial-buffer-choice 'tychoish/telega-switch-to-root))))
      (tychoish/telega-default-root-buffer toggle))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; writing (english) configuration

(use-package deft
  :ensure t
  :bind (:map tychoish/docs-map
	 ("o" . deft)
         ("n" . tychoish-deft-create)
         ("f" . deft-find-file))
  :init
  (defun deft-find-file ()
    (interactive)
    (find-file deft-directory))

  (defun deft-file-make-slug (s)
    "Turn a string into a slug."
    (replace-regexp-in-string
     " " "-" (downcase
              (replace-regexp-in-string
               "[^A-Za-z0-9 ]" "" s))))

  (defun tychoish-deft-create (title)
    "Create a new deft entry."
    (interactive "sNote Title: ")
    (let ((draft-file (concat deft-directory
                              (deft-file-make-slug title)
                              "."
                              deft-extensions)))
      (if (file-exists-p draft-file)
          (find-file draft-file)
        (find-file draft-file)
        (insert title))))
  :config
  (setq deft-extensions '("md" "mdwn" "markdown" "txt" "text" "rst"))
  (setq deft-new-file-format "%Y-%m-%dT.%H%M")
  (setq deft-default-extension "md")
  (setq deft-text-mode 'markdown-mode)
  (setq deft-use-filename-as-title t)
  (setq deft-auto-save-interval 0)
  (setq deft-auto-save-interval nil))

(use-package markdown-mode
  :ensure t
  :delight (markdown-mode "mdwn")
  :mode ("\\.mdwn" "\\.md" "\\.markdown" "\\.txt")
  :init
  (defalias 'markdown-indent-code (kmacro "SPC SPC SPC SPC SPC C-a C-n"))
  (add-hook 'markdown-mode-hook 'turn-off-auto-fill)
  (add-hook 'markdown-mode-hook 'turn-on-soft-wrap)
  (add-hook 'markdown-mode-hook (lambda () (setq imenu-generic-expression markdown-imenu-generic-expression)))
  :config
  (if (eq system-type 'darwin)
      (setq markdown-command "/usr/local/bin/mmd --nosmart")
    (setq markdown-command "/usr/bin/markdown"))

  (setq markdown-imenu-generic-expression
        '(("title"  "^\\(.*\\)[\n]=+$" 1)
          ("h2-"    "^\\(.*\\)[\n]-+$" 1)
          ("h1"   "^# \\(.*\\)$" 1)
          ("h2"   "^## \\(.*\\)$" 1)
          ("h3"   "^### \\(.*\\)$" 1)
          ("h4"   "^#### \\(.*\\)$" 1)
          ("h5"   "^##### \\(.*\\)$" 1)
          ("h6"   "^###### \\(.*\\)$" 1)
          ("fn"   "^\\[\\^\\(.*\\)\\]" 1))))

(use-package fountain-mode
  :ensure t
  :mode ("\\.script" "\\.sp"))

(use-package rst
  :delight (rst-mode "rst")
  :mode ("\\.rst" . rst-mode)
  :bind (:map rst-mode-map
	 ("C-c C-t h" . rst-adjust))
  :init
  (defalias 'rst-indent-code (kmacro "SPC SPC SPC C-a C-n"))
  (defun tychoish/set-up-rst-mode ()
    (turn-on-auto-fill)
    (setq-local fill-column 78)
    (setq-local rst-level-face-max 0)
    (set-face-background 'rst-level-1 nil)
    (set-face-background 'rst-level-2 nil)
    (set-face-background 'rst-level-3 nil)
    (set-face-background 'rst-level-4 nil)
    (set-face-background 'rst-level-5 nil)
    (set-face-background 'rst-level-6 nil)
    (define-key rst-mode-map (kbd "C-c C-t h") 'rst-adjust)
    (local-unset-key (kbd "C-c C-s")))
  (add-hook 'rst-mode-hook 'tychoish/set-up-rst-mode))

(use-package flyspell
  :ensure t
  :defer t
  :delight (flyspell-mode " fs")
  :hook ((prog-mode . flyspell-prog-mode)
         (text-mode . flyspell-mode)
         (telega-chat-mode . flyspell-mode))
  :bind (("C-c [" . flyspell-correct-next)
         ("C-c ]" . flyspell-correct-previous)
         ("M-$" . flyspell-correct-at-point)
         ("C-;" . flyspell-correct-previous))
  :commands (flyspell-mode flyspell-prog-mode flyspell-correct-wrapper)
  :config
  (setq ispell-list-command "list")
  (setq flyspell-issue-message-flag nil)
  (setq ispell-program-name "aspell")
  (setq ispell-extra-args '("--sug-mode=ultra" "--run-together"))
  (setq flyspell-sort-corrections nil)
  (setq flyspell-dictionaries '("english"))
  (setq flyspell-guess-size 76)
  (setq flyspell-guess-slots 3)
  (setq flyspell-focus 50) ;; between 0 and 100
  (setq flyspell-timeout 3)
  (setq flyspell-polling-timeout 10)
  (setq flyspell-delay 2)
  (setq flyspell-indicator-format "[%s]--")
  (setq flyspell-timer nil)
  (setq flyspell-timer-aux nil)
  (setq flyspell-guess-indicator nil)
  (setq flyspell-min-buffer-size (* flyspell-guess-size flyspell-guess-slots)))

(use-package grammarly
  :ensure t
  :defer t
  :defines (grammarly-on-message-function-list
            grammarly-on-open-function-list
            grammarly-on-close-function-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; modes for programming languages other formats

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; programming major-modes

(use-package yaml-ts-mode
  :ensure t
  :mode (("\\.yaml$" . yaml-ts-mode)
	 ("\\.yml$" . yaml-ts-mode))
  :init
  (add-to-list 'tychoish/eglot-default-server-configuration
                '((:yaml (:format
                          :enable t
                          :singleQuote :json-false
                          :bracketSpacing t
                          :proseWrap "preserve" ;; preserve/always/never
                          :printWidth 80
                          :validate t
                          :hover t
                          :completion t))))
  :config
  (add-hook 'yaml-ts-mode-hook (tychoish/set-tab-width 2)))

(use-package yaml-pro
  :ensure t
  :commands (yaml-pro-ts-mode)
  :hook (yaml-ts-mode . yaml-pro-ts-mode))

(use-package go-ts-mode
  :ensure nil
  :delight
  (go-ts-mode "go.ts")
  (go-mod-ts-mode "go.mod.ts")
  (go-mode "go")
  :mode (("\\.go$" . go-ts-mode)
         ("go.work" . go-mod-ts-mode)
         ("go.mod" . go-mod-ts-mode))
  :init
  (defun tychoish/go-mode-setup ()
    (setq-local tab-width 8)
    (setq-local fill-column 100))

  (defun tychoish/go-mode-setup-for-buffer (buf)
    (with-current-buffer buf
      (setq-local flycheck-disabled-checkers '(go-unconvert go-staticcheck go-vet go-build go-test go-gofmt golangci-lint))
      (tychoish/go-mode-setup)))

  (defun tychoish/go-mode-refresh-current-buffers ()
    (->> (mode-buffers 'go-ts-mode)
	 (-mapc #'tychoish/go-mode-setup-for-buffer)))

  (add-to-list 'major-mode-remap-alist '((go-mode . go-ts-mode)))
  (add-to-list 'major-mode-remap-alist '((go-mod-mode . go-mod-ts-mode)))
  (add-to-list 'tychoish/eglot-default-server-configuration
                '(:gopls :gofumpt t
                         :usePlaceholders :json-false
                         :hoverKind "FullDocumentation"
                         :analyses (:unreachable t
                                    :unusedvariable t)
                         :hints (:parameterNames :json-false
                                 :ignoredError t
                                 :compositeLiteralTypes :json-false
                                 :compositeLiteralFields :json-false
                                 :rangeVariableTypes :json-false
                                 :functionTypeParameters :json-false)))

  (add-hook 'go-ts-mode-hook 'tychoish/go-mode-setup)
  :config
  (let ((current-path (getenv "PATH"))
        (gopath (getenv "GOPATH")))

    (unless gopath
      (setq gopath (setenv "GOPATH" (expand-file-name "~/go"))))

    (setq local-go-bin (f-expand (concat gopath "/bin")))
    (add-to-list 'exec-path local-go-bin)

    (unless (s-contains? local-go-bin current-path)
      (setenv "PATH" (format "%s:%s" current-path local-go-bin)))))

(use-package rustic
  :ensure t
  :delight
  (rust-mode "rs")
  (rustic-mode "rs(x)")
  :after (rust-mode flycheck)
  :mode (("\\.rs$" . rustic-mode)
         ("\\.rs" . rustic-mode)
         ("\\.rs\\'" . rustic-mode)
	 ("Cargo.lock" . toml-ts-mode))
  :commands (rust-resolve-fmt-path rustic-mode)
  :init
  (add-hook 'rustic-mode-hook 'rustic-mode-auto-save-hook)

  (defun rustic-mode-auto-save-hook ()
    "Enable auto-saving in rustic-mode buffers."
    (when buffer-file-name
      (setq-local compilation-ask-about-save nil)))
  :config
  (setq rust-mode-treesitter-derive t)
  (setq rustic-lsp-client 'eglot)
  (setq rustic-format-trigger 'on-save)
  (setq rustic-cargo-clippy-trigger-fix 'on-compile)
  (setq rustic-use-rust-save-some-buffers t)
  (setq rustic-clippy-arguments "--all --all-features -- --deny warnings")

  (let* ((rustup-path (executable-find "rustup"))
	 (rustup-p (not (string-empty-p rustup-path)))
	 (rustup-toolchain (if rustup-p "nightly" "stable")))
    (when rustup-p
      (setq rustic-rustfmt-args "+nightly")
      (setq rustic-analyzer-command '(rustup-path "run" "stable" "rust-analyzer")))

  (add-to-list 'tychoish/eglot-default-server-configuration
	       `((:rust-analyzer :initializationOptions
                   (:server (:extraEnv (:RUSTUP_TOOLCHAIN ,rustup-toolchain))
                             :rust (:analyzerTargetDir t)
                             :cargo (:buildScripts (:enable t :features "all"))
                             :procMacro (:enable :json-false :attributes (:enable t))
                             :check (:workspace t))))))

  (add-to-list 'flycheck-checkers 'rustic-clippy))

(use-package python-ts-mode
  :ensure nil
  :delight
  (python-ts-mode "py.ts")
  (python-mode "py")
  :mode (("\\.py$" . python-ts-mode))
  :init
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  (add-hook 'python-ts-mode 'tychoish/python-setup)
  (add-to-list 'tychoish/eglot-default-server-configuration
               '((:pylsp (:plugins
                          :black (:enabled t)
                          :jedi_completion (:enabled t
					    :fuzzy t
					    :include_params t)
                          :ruff (:enabled t
  			         :formatEnabled t
				 :lineLength 100
    			         :format ["I"]
				 :extendSelect ["I"])
                          :rope (:enabled t)
                          :flake8 (:enabled :json-false)
                          :pycodestyle (:enabled :json-false)
                          :mccabe (:enabled :json-false)
                          :autopep8 (:enabled :json-false)
                          :pyflakes (:enabled :json-false)
                          :pycodestyle (:enabled :json-false)))))

  (defun tychoish/python-setup ()
    (setq-local python-indent-offset 4)
    (setq-local tab-width 4)
    (setq-local fill-column 100))
  :config
  (require 'python)

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

  (bind-key "M-<left>" 'balle-python-shift-left python-ts-mode-map)
  (bind-key "M-<right>" 'balle-python-shift-right python-ts-mode-map)

; (setenv "PYTHON_KEYRING_BACKEND" "keyring.backends.null.Keyring")
  (font-lock-add-keywords 'python-ts-mode (font-lock-show-tabs))
  (font-lock-add-keywords 'python-ts-mode (font-lock-width-keyword 100)))

(use-package pkgbuild-mode
  :ensure t
  :mode ("PKGBUILD$"))

(use-package protobuf-mode
  :ensure t
  :mode "\\.proto$'")

(use-package graphviz-dot-mode
  :ensure t
  :mode ("\\.gv" "\\.dot")
  :commands (graphviz graphviz-dot-mode)
  :init
  (setq graphviz-dot-indent-width 4))

(use-package terraform-mode
  :ensure t
  :delight (terraform-mode "tf")
  :mode ("\\.tf" "\\.tfvars" "\\.tfvars.example")
  :config
  (setq terraform-format-on-save t)
  (setq terraform-indent-level 2))

(use-package just-mode
  :ensure t
  :after (consult-builder)
  :mode (("justfile" . just-mode)
         ("Justfile" . just-mode)
         ("\\.just%" . just-mode)))

(use-package nxml-mode
  :mode (("\\.xml$'". nxml-mode)))

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
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq js-indent-level 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-expanding t)
  (setq web-mode-enable-css-colorization t))

(use-package jinja2-mode
  :ensure t
  :mode "\\.jinja\\'")

(use-package ninja-mode
  :ensure t
  :mode "\\.ninja\\'")

(use-package slime
  :delight
  (lisp-mode "lisp")
  (slime-mode "sl")
  (slime-autodoc-mode "")
  :mode ("\\.lisp" . lisp-mode)
  :bind (:map tychoish/docs-map
	 ("c" . hyperspec-lookup))
  :commands (slime slime-connect)
  :config
  (setq ls-lisp-dirs-first t)
  (setq inferior-lisp-program "sbcl"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; programming adjacent tools

(use-package journalctl-mode
  :ensure t
  :bind (:map tychoish-core-map
	 ("j" . journalctl)))

(use-package docker
  :ensure t
  :commands (docker)
  :bind ("C-c C-c" . docker))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; build/compilation support

(use-package flycheck
  :ensure t
  :delight (flycheck-mode "fc")
  :bind (("C-c f f" . flycheck-mode))
  :defines (flycheck-checkers)
  :commands (flycheck-disable-checker flycheck-mode global-flycheck-mode)
  :init
  (defun tychoish/flycheck-prefer-eldoc ()
    (add-hook 'eldoc-documentation-functions #'flycheck-eldoc nil t) ;; local
    (setq-local eldoc-docuemntation-stratedgy #'eldoc-documentation-compose-eagerly)
    (setq-local flycheck-display-errors-function nil)
    (setq-local flycheck-help-echo nil))
  (add-hook 'flycheck-mode-hook 'tychoish/flycheck-prefer-eldoc)
  :config
  (setq-default flycheck-disable-checker '(go-unconvert go-staticcheck go-vet go-build go-test go-gofmt))
  (setq flycheck-keymap-prefix (kbd "C-c f"))
  ;; the order of the following 3 operations is important.
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map)
  (setq flycheck-keymap-prefix (kbd "C-c f"))

  (bind-key "m" #'consult-flycheck flycheck-command-map)

  (defun flycheck-eldoc (callback &rest _ignored)
    "Print flycheck messages at point by calling CALLBACK." ;; from masteringemacs.org
    (when-let ((flycheck-errors (and flycheck-mode (flycheck-overlay-errors-at (point)))))
      (mapc
       (lambda (err)
         (funcall callback
                  (format "%s: %s"
                          (let ((level (flycheck-error-level err)))
                            (pcase level
                              ('info (propertize "I" 'face 'flycheck-error-list-info))
                              ('error (propertize "E" 'face 'flycheck-error-list-error))
                              ('warning (propertize "W" 'face 'flycheck-error-list-warning))
                              (_ level)))
                          (flycheck-error-message err))
                  :thing (or (flycheck-error-id err)
                             (flycheck-error-group err))
                  :face 'font-lock-doc-face))
       flycheck-errors)))
  (setq flycheck-indication-mode nil)
  (setq flycheck-check-syntax-automatically '(save new-line idle-change idle-buffer-switch))
  (setq flycheck-idle-change-delay 1)
  (setq flycheck-idle-buffer-switch-delay 1)
  (setq flycheck-checker-error-threshold nil)
  (setq flycheck-display-errors-delay 0.5)
  (setq flycheck-flake8-maximum-line-length 100))

(use-package flycheck-golangci-lint
  :ensure t
  :defines (golangci-lint)
  :after (flycheck go-ts-mode)
  :commands (flycheck-golangci-lint-setup)
  :config
  (setq flycheck-go-vet-shadow t)
  (setq flycheck-go-build-install-deps nil)
  (setq flycheck-golangci-lint-fast t)
  (setq flycheck-golangci-lint-tests t))

(use-package compile
  :defines (compile-add-error-syntax compilation-mode-map)
  :bind (:map tychoish-core-map
	 ("c" . tychoish-compile)
	 :map compilation-mode-map
	 ("C" . compile))
  :config
  (defun compile-add-error-syntax (name regexp file line &optional col level)
    "Register new compilation error syntax."
    (add-to-list 'compilation-error-regexp-alist-alist (list name regexp file line col level))
    (add-to-list 'compilation-error-regexp-alist name))

  (declare-function 'ansi-color-apply-on-region "ansi-color")

  (defun colorize-compilation-buffer ()
    (ansi-color-apply-on-region compilation-filter-start (point)))

  (defun tychoish-compile ()
    "Run compile operation selecting compile buffer and commands."
    (interactive)
    (tychoish/compile-project))

  (defun tychoish-compile-project-super-lint ()
    (interactive)
    (let* ((project-directory (approximate-project-root))
           (options (list "VALIDATE_YAML=true"
                          "VALIDATE_OPENAPI=true"
                          "VALIDATE_MD=true"
                          "MARKDOWN_CONFIG_FILE=.markdownlint.yml"
                          "VALIDATE_ALL_CODEBASE=true"
                          "LINTER_RULES_PATH=."
                          "RUN_LOCAL=true"))
           (optstr (format "-e %s" (s-join " -e " options)))
           (command-string (format "docker run %s -v %s:/tmp/lint github/super-linter" optstr project-directory)))
      (tychoish/compile-project "super-lint" command-string)))

  (with-eval-after-load 'rust-mode
    (require 'rust-compile))

  (defun tychoish-compilation-read-command (command)
    (let* ((had-initial-command (not (null command)))
	   (results (tychoish--compilation-read-command command))
	   (name (car results))
	   (candidates (cdr results))
	   (command (tychoish-compilation-candidate-command (ht-get candidates name))))
      (read-from-minibuffer "edit command => " command)))

  (setq-default compilation-save-buffers-predicate #'approximate-project-root)
  (compile-add-error-syntax 'rust-pretty-logfile "^\s+ at \\(.*\\):\\([0-9]+\\)" 1 2)
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
  (advice-add 'compilation-read-command :override 'tychoish-compilation-read-command))

(use-package cargo
  :ensure t
  :after (rustic)
  :config
  (setq cargo-process--command-fmt "+nightly fmt --all")
  (add-hook 'rustic-mode-hook 'cargo-minor-mode))

(use-package flycheck-aspell
  :ensure t
  :defer t
  :after (flycheck flyspell
  	  (:any org-mode rst-mode markdown-mode message-mode sgml-mode html-mode html-ts-mode))
  :defines (c-aspell-dynamic markdow-aspell-dynamic mail-aspell-dynamic html-aspell-dynamic)
  :functions (flycheck-aspell--start-checker flycheck-aspell--parse)
  :config
  (add-to-list 'flycheck-checkers 'c-aspell-dynamic)
  (add-to-list 'flycheck-checkers 'org-aspell-dynamic)
  (add-to-list 'flycheck-checkers 'rst-aspell-dynamic)
  (add-to-list 'flycheck-checkers 'html-aspell-dynamic)
  (add-to-list 'flycheck-checkers 'markdown-aspell-dynamic)
  (add-to-list 'flycheck-checkers 'mail-aspell-dynamic)

  (flycheck-aspell-define-checker "org" "Org" ("--add-filter" "url") (org-mode))
  (flycheck-aspell-define-checker "rst" "reStructuredText" ("--add-filter" "url") (rst-mode)))

(use-package flycheck-grammarly
  :ensure t
  :after (flycheck flycheck-aspell grammarly)
  :commands (flycheck-grammarly-setup tychoish/flycheck-grammarly-enable tychoish/flycheck-grammarly-disable)
  :functions (flycheck-remove-next-checker flycheck-add-next-checker)
  :config
  (setq flycheck-grammarly-check-time 0.8)

  (defun tychoish/flycheck-grammarly-enable ()
    (interactive)
    (add-to-list 'flycheck-checkers 'grammarly)

    (add-to-list 'grammarly-on-open-function-list 'flycheck-grammarly--on-open)
    (add-to-list 'grammarly-on-message-function-list 'flycheck-grammarly--on-message)
    (add-to-list 'grammarly-on-close-function-list 'flycheck-grammarly--on-close)

    (flycheck-add-next-checker 'markdown-aspell-dynamic 'grammarly)
    (flycheck-add-next-checker 'mail-aspell-dynamic 'grammarly))

  (defun tychoish/flycheck-grammarly-disable ()
    (interactive)
    (flycheck-remove-next-checker 'markdown-aspell-dynamic 'grammarly)
    (flycheck-remove-next-checker 'mail-aspell-dynamic 'grammarly)
    (setq flycheck-checkers (remove 'grammarly flycheck-checkers))
    (setq grammarly-on-open-function-list (remove 'flycheck-grammarly--on-open 'grammarly-on-open-function-list))
    (setq grammarly-on-message-function-list (remove 'flycheck-grammarly--on-message 'grammarly-on-open-function-list))
    (setq grammarly-on-close-function-list (remove 'flycheck-grammarly--on-close 'grammarly-on-close-function-list))))

(use-package flycheck-vale
  :ensure t
  :defer t
  :after (flycheck (:any markdown-mode rst-mode org-mode))
  :commands (flycheck-vale-setup flycheck-vale-toggle-enabled tychoish/flycheck-vale-enable tychoish/flycheck-vale-disable)
  :config
  (defun tychoish/flycheck-vale-enable ()
    (interactive)
    (add-to-list 'flycheck-checkers 'vale)
    (setq flycheck-vale-enable t)
    (flycheck-add-next-checker 'mail-aspell-dynamic 'vale)
    (flycheck-add-next-checker 'markdown-aspell-dynamic 'vale)
    (flycheck-add-next-checker 'org-aspell-dynamic 'vale)
    (flycheck-add-next-checker 'rst-aspell-dynamic 'vale))

  (defun tychoish/flycheck-vale-disable ()
    (interactive)
    (setq flycheck-checkers (remove 'vale flycheck-checkers))
    (setq flycheck-vale-enabled nil)
    (flycheck-remove-next-checker 'mail-aspell-dynamic 'vale)
    (flycheck-remove-next-checker 'markdown-aspell-dynamic 'vale)
    (flycheck-remove-next-checker 'org-aspell-dynamic 'vale)
    (flycheck-remove-next-checker 'rst-aspell-dynamic 'vale)))

(use-package ctags-update
  :ensure t
  :bind (("C-c E" . ctags-update))
  :commands (turn-on-ctags-auto-update-mode create-tags)
  :delight ctags-auto-update-mode
  :config
  (setq tags-add-tables nil)
  (setq etags-table-search-up-depth 10)
  (setq ctags-update-delay-seconds 300)
  (defun create-tags (dir-name)
    "Create tags file for the DIR-NAME directory."
    (interactive "DDirectory: ")
    (let ((cmd-str (format "%s -e -u -f %s/TAGS %s -R %s" path-to-ctags dir-name dir-name (directory-file-name dir-name))))
      (message cmd-str)
      (shell-command cmd-str)))
  (setq path-to-ctags (executable-find "ctags"))
  (add-hook 'emacs-lisp-mode-hook  'turn-on-ctags-auto-update-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; language server protocol (lsp) [eglot] + treesitter
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eglot
  :ensure nil
  :defines (eglot-mode-map eglot-alternatives)
  :functions (eglot-format-buffer eglot-managed-p eglot-completion-at-point)
  :hook (((js-mode
           js-ts-mode
           typescript-ts-mode
	   go-ts-mode
	   go-mod-ts-mode
	   yaml-ts-mode
	   rust-mode
	   rust-ts-mode
	   rustic-mode
	   c++-ts-mode
	   c-ts-mode
	   c-or-c++-mode
	   python-mode
	   python-ts-mode
	   go-mode
	   go-mod-mode
           bash-ts-mode
           bash-mode
           sh-mode) . #'eglot-ensure))
  :bind (:map tychoish/ide-map ;; "C-c l"
	 :prefix "l"
	 :prefix-map tychoish/eglot-global-map
	 ("s" . eglot)
         ("r" . eglot-reconnect) ;; TODO this should only be on when minor mode
         ("k" . eglot-shutdown)
         ("l" . eglot-list-connections)
         ("g" . eglot-forget-pending-continuations))
  :commands (eglot-code-action-rewrite eglot-code-action-extract eglot-code-actions eglot-format eglot-rename eglot-code-action-organize-imports)
  :functions (eglot-alternatives)
  :init
  (defun tychoish/eglot-ensure-hook ()
    (setq-local eldoc-docuemntation-stratedgy 'eldoc-documentation-compose-eagerly)
    ;; toggle it on and off so that the left-fringe isn't weird.
    (flycheck-eglot-mode -1)
    (flycheck-eglot-mode 1))

  (add-hook 'eglot-managed-mode-hook 'tychoish/eglot-ensure-hook)
  :config
  (bind-keys :map eglot-mode-map
	     :prefix "C-c l"
	     :prefix-map tychoish/eglot-map
             ("r" . eglot-rename)
             ("f" . eglot-format)
             ("a" . eglot-code-actions)
             ("o" . eglot-code-action-organize-imports)
             ;; ("i" . eglot-code-action-inline)
             ;; ("c" . eglot-call-type-hierarchy)
             ;; ("t" . eglot-show-type-hierarchy)
             ("e" . eglot-code-action-extract)
             ("w" . eglot-code-action-rewrite))

  (setq eglot-menu-string "eg")

  (setq eglot-autoshutdown t)
  (setq eglot-extend-to-xref t)

  (add-to-list 'eglot-stay-out-of 'flymake)
  (add-to-list 'eglot-stay-out-of 'company)

  (add-hook 'before-save-hook 'eglot-organize-imports 10)
  (add-hook 'before-save-hook 'eglot-format-for-hook -10)

  (add-to-list 'eglot-server-programs
               `((go-mode go-dot-mod-mode go-dot-work-mode go-ts-mode go-mod-ts-mode)
                 . ,(eglot-alternatives
                     `(("gopls" ,(format "-remote=unix;/run/user/%d/gopls.socket" (user-uid)))
		       ("gopls" "-remote=unix;/tmp/gopls.socket")
		       ("gopls" "-remote=auto")
		       ("gopls")))))

  (add-to-list 'eglot-server-programs
	       `((c-mode c++-mode c-ts-mode c++-ts-mode c-or-c++-ts-mode c-or-c++-mode)
		 . ,(eglot-alternatives
		     `(("clangd" "-j=8" "--background-index" "--clang-tidy" "--cross-file-rename" "--completion-style=detailed")
		       ("clangd")
		       ("ccls")))))

  (setq-default eglot-workspace-configuration tychoish/eglot-default-server-configuration)

  (defun eglot-organize-imports ()
    (interactive)
    (when (eglot-managed-p)
      (with-demoted-errors "WARN (`eglot-organize-imports'): %S"
        (eglot-code-actions nil nil "source.organizeImports" t))))

  (defun eglot-code-action-inline ()
    (interactive)
    (when (eglot-managed-p)
      (with-demoted-errors "WARN (`eglot-code-action-inline'): %S"
        (eglot-code-actions nil nil "source.inline" t))))

  (defun eglot-format-for-hook ()
    (interactive)
    (when (eglot-managed-p)
      (eglot-format-buffer))))

(use-package flycheck-eglot
  :ensure t
  :hook (eglot-managed-mode . flycheck-eglot-mode)
  :commands (flycheck-eglot-mode)
  :init
  (setq-default flycheck-eglot-exclusive nil)
  :config
  (add-to-list 'flycheck-checkers 'eglot-check)
  (setq flycheck-eglot-enable-diagnostic-tags nil)
  (flycheck-add-next-checker 'eglot-check 'go-gofmt))

(use-package treesit
  :mode (("\\.sh\\'" . bash-ts-mode)
         ("\\.bash\\'" . bash-ts-mode)
         ("\\.bashrc\\'" . bash-ts-mode)
         ("Dockerfile" . dockerfile-ts-mode)
         ;; -- js/web
         ("\\.ts\\'" . typescript-ts-mode)
         ("\\.js\\'" . js-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)
         ("\\.css\\'" . css-ts-mode)
         ("\\.json\\'" . json-ts-mode)
         ("\\.toml\\'" . toml-ts-mode)
         ;; -- c/c++
         ("CMakeLists.txt" . cmake-ts-mode)
	 ("\\.h\\'" . c-or-c++-mode)
         ("\\.c\\'" . c-ts-mode)
	 ("\\.cpp\\'" . c++-ts-mode)
         ("\\.cc\\'" . c++-ts-mode)
         ("\\.hh\\'" . c++-ts-mode)
         ("\\.cxx\\'" . c++-ts-mode)
         ;; -- jvm
         ("\\.java\\'" . java-ts-mode))
  :init
  (add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode))
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  (add-to-list 'major-mode-remap-alist '(java-mode . java-ts-mode))
  (add-to-list 'major-mode-remap-alist '(css-mode . css-ts-mode))
  (add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode))

  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c-or-c++-mode . c-or-c++-ts-mode))
  :config
  (add-hook 'js-ts-mode-hook (create-run-hooks-function-for js-mode))
  (add-hook 'c-ts-mode-hook (create-run-hooks-function-for c-mode))
  (add-hook 'c++-ts-mode-hook (create-run-hooks-function-for c++-mode))

  (font-lock-add-keywords 'c-mode (font-lock-width-keyword 100))
  (font-lock-add-keywords 'c++-mode (font-lock-width-keyword 100))

  (font-lock-add-keywords 'c-ts-mode (font-lock-width-keyword 100))
  (font-lock-add-keywords 'c++-ts-mode (font-lock-width-keyword 100))

  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (java "https://github.com/tree-sitter/tree-sitter-java")
	  (dockerfile "https://github.com/camdencheek/tree-sitter-dockerfile")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")))

  (defun tychoish-background-rebuild-treesit-bindings ()
    (interactive)
    (async-start
     `(lambda ()
        ,(async-inject-variables "treesit-language-source-alist")
        (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))
     (lambda (result)
       (message "rebuilt treesit grammars for %s" result)))))

(use-package dape
  :ensure t
  :bind-keymap ("C-c C-d" . dape-global-map)
  :config
  (add-hook 'kill-emacs-hook 'dape-bakepoint-save)
  (add-hook 'dape-start-hook #'save-all-buffers)
  ;; (setq dape-info-hide-mode-line nil)
  ;;
  ;; Pulse source line (performance hit)
  ;; (add-hook 'dape-display-source-hook #'pulse-momentary-highlight-one-line)
  (setq dape-key-prefix (kbd "C-c C-d"))
  (which-key-add-key-based-replacements "C-c C-d" "dape")
  (setq dape-buffer-window-arrangement 'right)
  (setq dape-cwd-function #'approximate-project-root))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ROBOTS (AI) Integration
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package google-gemini
  :defer t
  :ensure nil
  :commands (google-gemini-chat-prompt
             google-gemini-content-prompt
             google-gemini-count-tokens-prompt
             google-gemini-list-models
             google-gemini-model-info))

(use-package gptel
  :ensure t
  :vc (:url "https://github.com/karthink/gptel" :rev newest)
  :bind (:prefix "C-c r"
	 :prefix-map tychoish/robot-map
	 ("g" . gptel)
	 :map tychoish/robot-map
	 :prefix "g"
	 :prefix-map tychoish/robot-gptel-map
	 ("g" . gptel)
         ("r" . gptel-rewrite)
         :map gptel-mode-map
	 ("C-c m" . gptel-menu))
  :functions (gptel-make-anthropic gptel-make-gh-copilot gptel-make-gemini)
  :commands gptel
  :init
  (defvar gemini-api-key nil)
  (defvar anthropic-api-key nil)
  (defvar openai-api-key nil)
  :config
  (setq gptel-include-reasoning 'ignore)

  (tychoish/gptel-set-up-backend
   :name "gemini"
   :key "g"
   :model 'gemini-2.5-pro-preview-06-05
   :backend (gptel-make-gemini "gemini" :key gemini-api-key :stream t))

  (tychoish/gptel-set-up-backend
   :name "copilot"
   :key "c"
   :model 'claude-3.5-sonnet
   :backend (gptel-make-gh-copilot "copilot"))

  (tychoish/gptel-set-up-backend
   :name "anthropic"
   :key "a"
   :model 'claude-3-5-sonnet-20241022
   :api-key anthropic-api-key
   :backend (gptel-make-anthropic "claude" :key anthropic-api-key :stream t))

  (tychoish/gptel-set-up-backend
   :name "gpt-5"
   :key "s"
   :model 'gpt-5
   :api-key openai-api-key
   :backend (gptel-make-openai "openai" :key openai-api-key))

  (tychoish/gptel-set-up-backend
   :name "gpt-5-mini"
   :key "m"
   :model 'gpt-5
   :api-key openai-api-key
   :backend (gptel-make-openai "openai" :key openai-api-key))

  (tychoish/gptel-set-up-backend
   :name "o4-mini"
   :key "o"
   :model 'o4-mini
   :backend (gptel-make-openai "openai" :key openai-api-key))

  (tychoish/gptel-set-up-backend
   :name "gpt-5-nano"
   :key "n"
   :model 'gpt-5
   :backend (gptel-make-openai "openai" :key openai-api-key))

  (require 'gptel-integrations))

(use-package gptel-aibo
  :ensure t
  :bind (:map tychoish/robot-gptel-map
         ("w" . gptel-aibo-summon))
  :commands (gptel-aibo-summon gptel-aibo))

(use-package mcp
  :ensure t
  :commands (mcp-hub-start-all-servers)
  :config
  (require 'mcp-hub)
  (add-to-list 'mcp-hub-servers '("time" . (:command "uvx" :args ("mcp-server-time"))))
  (add-to-list 'mcp-hub-servers '("fetch" . (:command "uvx" :args ("mcp-server-fetch"))))
  (add-to-list 'mcp-hub-servers '("git" . (:command "uvx" :args ("mcp-server-git")))))

(use-package copilot
  :ensure t
  :bind (:map tychoish/robot-map ;; "C-c r"
	 :prefix "c"
	 :prefix-map tychoish/robot-copilot-map ;; "C-c r c"
	 ("s" . copilot-complete)
         ("g" . copilot-clear-overlay)
	 :map tychoish/completion-map
	 ("r" . copilot-complete)
         :map copilot-completion-map
         ("C-c a" . copilot-acept-completion)
         ("C-c l" . copilot-acept-completion-by-line)
         ("C-c w" . copilot-acept-completion-by-word)
         ("C-c n" . copilot-next-completion)
         ("C-c p" . copilot-previous-completion)
         ("C-c C-i" . copilot-insert-completion))
  :commands (copilot-login copilot-mode)
  :config
  (add-to-list 'copilot-major-mode-alist '("rustic-mode" . "rust"))
  (add-to-list 'copilot-major-mode-alist '("go-ts-mode" . "go"))
  (add-to-list 'copilot-major-mode-alist '("c-ts-mode" . "c"))
  (add-to-list 'copilot-major-mode-alist '("c++-ts-mode" . "cpp"))
  (add-to-list 'copilot-major-mode-alist '("python-ts-mode" . "python"))
  (add-to-list 'copilot-major-mode-alist '("yaml-ts-mode" . "yaml"))
  (add-to-list 'copilot-major-mode-alist '("bash-ts-mode" . "shellscript")))

(use-package copilot-chat
  :ensure t
  :bind (:map tychoish/robot-copilot-map ;; "C-c r c"
	 ("m" . copilot-chat-transient)
         ("t" . copilot-chat))
  :config
  (setq copilot-chat-frontend 'markdown)
  (setq copilot-chat-follow t)
  (setq copilot-chat-markdown-prompt "##"))

(use-package aidermacs
  :ensure t
  :bind (:map tychoish/robot-map ;; "C-c r"
	 :prefix "a"
	 :prefix-map tychoish/robot-aider-map
	 ("m" . aidermacs-transient-menu)
	 ("C-m" . execute-extended-aidermacs-command)
	 ("l" . execute-extended-aidermacs-model-command))
  :config
  (setq aidermacs-default-chat-mode 'architect)
  (setq aidermacs-program "aider")
  (cl-defmacro make-aidermacs-model-selection-function (model &optional &key name)
    (unless name
      (setq name model))

    (let ((symbol-name (format "aidermacs-model-use-%s" name)))
      `(defun ,(intern symbol-name) ()
	 ,(format "Switch to using `%s' (%s)as the default model for aidermacs." model name)
	 (interactive)
	 (setq aidermacs-default-model ,model)
	 (when (aidermacs-select-buffer-name)
	   (aidermacs-change-model ,model)))))

  (make-aidermacs-model-selection-function "sonnet" :name "claude-sonnet")
  (make-aidermacs-model-selection-function "haiku" :name "claude-haiku")
  (make-aidermacs-model-selection-function "gemini")
  (make-aidermacs-model-selection-function "4" :name "gpt4")
  (make-aidermacs-model-selection-function "4o" :name "gpt4o")

  (make-read-extended-command-for-prefix "aidermacs")
  (make-read-extended-command-for-prefix "aidermacs-model")

  (setq aidermacs-default-model "sonnet")
  (add-to-list 'aidermacs-extra-args "--notifications")
  (add-to-list 'aidermacs-extra-args "--cache-prompts")
  (add-to-list 'aidermacs-extra-args "--cache-keepalive-pings 12")
  (add-hook 'aidermacs-before-run-backend-hook 'tychoish/set-up-aider-env-vars))

(use-package aider
  :ensure t
  :bind (:map tychoish/robot-aider-map ;; "C-c r a"
	 ("a" . aider-transient-menu))
  :config
  (tychoish/set-up-aider-env-vars)
  (add-to-list 'aider-args "--notifications")
  (add-to-list 'aider-args "--cache-prompts")
  (add-to-list 'aider-args "--cache-keepalive-pings 12")
  (add-to-list 'yas-snippet-dirs (f-join (f-dirname (find-library-name "aider")) "snippets")))

(use-package monet
  ;; :vc (:url "https://github.com/stevemolitor/monet" :rev :newest)
  :load-path "elpa/monet"
  :hook (claude-code-mode . monet-mode)
  :defines (monet-command-map)
  :bind-keymap ("C-c r i" . monet-command-map)
  :commands (monet-start-server monet-start-server-function monet-mode)
  :init
  (setq monet-prefix-key nil)
  (which-key-add-key-based-replacements "C-c r i" "monet-command-map")
  (bind-key "i" 'monet-command-map 'tychoish/robot-map)
  :config
  (which-key-add-keymap-based-replacements 'tychoish/robot-map "i" (cons "monet-map"  monet-command-map)))

(use-package claude-code
  ;; :vc (claude-code
  ;;      :url "https://github.com/stevemolitor/claude-code.el"
  ;;      :rev :newest)
  :load-path "elpa/claude-code"
  :defines (claude-code-command-map)
  :bind-keymap ("C-c r m" . claude-code-command-map)
  :commands (claude-code-mode)
  :init
  (which-key-add-key-based-replacements "C-c r m" "claude-code-command-map")
  (bind-key "m" 'claude-code-command-map 'tychoish/robot-map)
  (setq claude-code-terminal-backend 'eat)
  :config
  (which-key-add-keymap-based-replacements 'tychoish/robot-map "m" (cons "claude-code" claude-code-command-map))
  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function))

(use-package inheritenv
  :ensure t
  :after (claude-code))

(use-package eat
  :ensure t
  :after (claude-code)
  :defer t)

(use-package uuidgen
  :ensure t
  :defer t)

(provide 'tychoish-core)
;;; tychoish-core.el ends here
