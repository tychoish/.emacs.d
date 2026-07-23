;;; tychoish-core.el -- contains all major use-package forms -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Provides my collection of use-package forms and related
;; configuration for loading and configuring all Emacs packages.

;; This configuration optimizes for lazy-loading so that configuration
;; only loads when called directly or a mode is activated.

;;; Code:

(declare-function approximate-project-root "xtd-project")
(declare-function approximate-project-name "xtd-project")

;; Required eagerly (not deferred via :commands below) since many other
;; use-package :init/:config blocks below reference hud-mode-map/hud-*-map
;; directly, at load time, not through an autoloaded command.

(use-package hud-mode
  :ensure nil
  :demand t
  :commands (hud-mode
	     buffer-line-count
	     buffer-directory
	     save-all-buffers
	     kill-eldoc-and-help-buffers
	     kill-buffers-in-directory
	     kill-buffers-matching-name
	     force-kill-buffers-matching-path
	     kill-buffers-matching-path
	     kill-all-reference-and-source-buffers
	     kill-buffers-matching-mode
	     kill-buffers-visiting-missing-files
	     kill-buffer-and-delete-file
	     pin-buffer-to-window-toggle
	     clean-kill-ring-clean
	     hud-opacity-increase
	     hud-opacity-decrease
	     hud-opacity-reset)
  :config
  (make-read-extended-command-for-prefix "clipboard"
    :bind-map hud-mode-map
    :bind-key "C-x x c")

  (create-toggle-functions slow-op-reporting)
  (create-toggle-functions electric-pair-inhibition)
  (create-toggle-functions electric-pair-eagerness))

(use-package f
  :ensure t
  :commands (f-glob f-collapse-homedir f-entries f-ancestor-of-p))

(use-package cond-let
  :ensure t
  :defer t)

(use-package delight
  :ensure t
  :commands (delight)
  :config
  (delight 'fundamental-mode "fun" 'simple)

  (delight 'abbrev-mode "abb")

  (delight 'emacs-lisp-mode '("el" (lexical-binding ":l" ":d")) 'elisp-mode)
  (delight 'lisp-interaction-mode "lisp" 'elisp-mode)
  (delight 'sh-mode "sh" 'sh-script)
  (delight 'org-mode "org" 'org-mode)
  (delight 'org-agenda-mode "agenda" 'org-agenda)
  (delight 'rst-mode "rst" 'rst-mode)

  (delight 'projectile-mode nil 'projectile)
  (delight 'eglot--managed-mode nil 'eglot)
  (delight 'eldoc-mode nil 'eldoc)
  (delight 'telega-chat-auto-fill-mode nil)

  (delight 'anzu-mode " az" 'anzu)
  (delight 'flyspell-mode " fs" 'flyspell)
  (delight 'flycheck-mode " fc" 'flycheck)
  (delight 'visual-line-mode " wr" 'simple)
  (delight 'auto-fill-function " afm" 'simple)
  (delight 'overwrite-mode " om" 'simple)
  (delight 'refill-mode " rf" 'refill)
  (delight 'auto-revert-mode nil 'autorevert))

(use-package uuidgen
  :ensure t
  :commands (uuidgen))

(use-package async
  :ensure t
  :commands (async-start
	     async-start-process
	     async-bytecomp-package-mode
	     dired-async-mode)
  :init
  (add-hook 'package--post-download-archives-hook 'async-bytecomp-package-mode)
  (add-hook 'dired-mode-hook 'dired-async-mode)
  :config
  (delight 'async-bytecomp-package-mode "" 'async-bytecomp)
  (delight 'dired-async-mode "" 'dired-async)
  (declare-function package-desc-p "package")
  (autoload 'async-package-do-action "async-package")

  (defun async-package-operation (op pkgs)
    (let* ((ops '(install upgrade 'reinstall))
           (valid-packages (seq-filter (lambda (it) (or (symbolp it)kage-desc-p it)) pkgs))
           (filename (concat (file-name-concat temporary-file-directory
                                                (string-join (list
                                                               "emacs" sprite-instance-id
                                                               "async-package"
                                                               (symbol-name op))
                                                              "-")) ".log")))
      (unless (member op ops)
        (user-error "%s is not a valid operation %S" op ops))

      (unless valid-packages
        (user-error "must define one or more valid packages %s [%s]" valid-packages pkgs))

      (async-package-do-action op valid-packages filename))))

(use-package package-build
  :ensure t
  :commands (package-build-archive package-build-all))

(use-package annotated-completing-read
  :commands (annotated-completing-read
	     annotated-completing-read-context-from-point
	     annotated-completing-read-directory))

(use-package sprite
  :init
  (add-one-shot-hook
   :name "set-custom-file"
   :hook 'after-init-hook
   :form (setq custom-file (sprite-state-path "custom.el"))
   ;; Depth below 0: must run before `package--save-selected-packages'.
   :depth -90)
  :config
  (setq frame-title-format '(:eval (format "%s:%s" sprite-instance-id (buffer-name)))))

(use-package hud
  :ensure nil
  :commands (hud-dispatch hud-select)
  :init
  (keymap-set hud-mode-map "C-x ." #'hud-dispatch)
  (keymap-set hud-mode-map "C-x ," #'hud-select)
  (keymap-set hud-core-map "m" #'hud-dispatch)
  (keymap-set hud-core-map "," #'hud-select))

(use-package arch
  :ensure nil
  :commands (arch-cache-drop
             arch-cache-reload
             arch-dispatch
             arch-find-package
             arch-install
             arch-kill-info-buffers
             arch-list
             arch-remove
             arch-search
             arch-show-info
             arch-sync
             arch-sync-force
             arch-upgrade
             arch-upgrade-all
             arch-upgrade-all-yay
             arch-upgrade-system
             arch-abs-install
             arch-abs-rebuild))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; UI, Display, Rendering, Window Management

(use-package windmove
  :ensure nil
  :defer t
  :init
  (keymap-set hud-mode-map "M-h" #'windmove-left)
  (keymap-set hud-mode-map "M-j" #'windmove-down)
  (keymap-set hud-mode-map "M-k" #'windmove-up)
  (keymap-set hud-mode-map "M-l" #'windmove-right)
  (keymap-set hud-mode-map "S-<left>" #'windmove-left)
  (keymap-set hud-mode-map "S-<down>" #'windmove-down)
  (keymap-set hud-mode-map "S-<up>" #'windmove-up)
  (keymap-set hud-mode-map "S-<right>" #'windmove-right))

(use-package modus-themes
  :ensure t
  :defer t
  :defines (modus-themes-deuteranopia modus-themes-common-palette-overrides)
  :init
  (keymap-set hud-theme-map "r" #'disable-all-themes) ;; "C-c t t"
  (keymap-set hud-theme-map "d" #'bootstrap-load-dark-theme) ;; "C-c t t"
  (keymap-set hud-theme-map "l" #'bootstrap-load-light-theme) ;; "C-c t t"

  (add-to-list 'term-file-aliases '("alacritty" . "xterm"))
  (add-to-list 'term-file-aliases '("ghostty" . "xterm-ghostty"))

  (defun bootstrap-load-light-theme ()
    (interactive)

    (unless (member 'modus-operandi custom-enabled-themes)
      (when custom-enabled-themes
	(disable-all-themes))

      (if (custom-theme-p 'modus-operandi)
	  (enable-theme 'modus-operandi)
	(load-theme 'modus-operandi t nil)))

    (unless (map-elt default-frame-alist 'alpha)
      (add-to-list 'default-frame-alist '(alpha . 97))))

  (defun bootstrap-ensure-light-theme ()
    (unless custom-enabled-themes
      (bootstrap-load-light-theme)))

  (add-one-shot-hook
   :name "<modus-themes> ensure light theme"
   :hook after-first-frame-created
   :form (bootstrap-ensure-light-theme)
   :idle-timer 0.01)

  :config
  (let ((theme-directory (concat (expand-file-name user-emacs-directory) "theme")))
    (setq-default custom-theme-directory theme-directory)
    (add-to-list 'custom-theme-load-path theme-directory)
    (add-to-list 'load-path theme-directory))

  (defun disable-all-themes ()
    (interactive)
    (mapc #'disable-theme custom-enabled-themes))

  (defun bootstrap-ensure-dark-theme ()
    (unless custom-enabled-themes
      (bootstrap-load-dark-theme)))

  (defun bootstrap-load-dark-theme ()
    (interactive)
    (disable-all-themes)
    (when (load-theme 'modus-vivendi t t)
      (enable-theme 'modus-vivendi))
    (add-to-list 'default-frame-alist '(alpha . 95)))

  (setq modus-themes-deuteranopia t)
  (setq modus-themes-common-palette-overrides
	'((border-mode-line-active bg-mode-line-active)
	  (border-mode-line-inactive bg-mode-line-inactive)
	  (message-separator bg-main))))

(use-package nerd-icons
  :ensure t
  :defer t
  :init
  (defun ad:nerd-icons-icon-for-buffer-safe (orig &rest args)
    "Return empty string instead of signaling for an unresolvable buffer icon."
    (condition-case nil
	(apply orig args)
      (error "")))
  :config
  (add-to-list 'nerd-icons-mode-icon-alist
               '(agent-shell-queue-item-view-mode nerd-icons-codicon "nf-cod-checklist" :face nerd-icons-green))
  (advice-add 'nerd-icons-icon-for-buffer :around #'ad:nerd-icons-icon-for-buffer-safe))

(use-package nerd-icons-dired
  :ensure t
  :commands (nerd-icons-dired-mode)
  :init
  (add-hook 'dired-mode-hook #'nerd-icons-dired-mode)
  :config
  (delight 'nerd-icons-dired-mode ""))

(use-package hud-modeline
  :ensure nil
  :commands (hud-modeline-mode)
  :init
  (add-one-shot-hook
   :name "hud-modeline"
   :form (run-with-idle-timer 0.1 nil #'hud-modeline-mode 1)
   :hook (if (daemonp)
	     'server-after-make-frame-hook
	   'window-setup-hook))
  :config
  (create-toggle-functions hud-modeline-icons
			   :keymap hud-theme-map
			   :key "i")
  (create-toggle-functions hud-modeline-show-buffer-size
			   :keymap hud-theme-map
			   :key "s"))

(use-package which-key
  :ensure nil
  :defer t
  :commands (which-key-setup-side-window-bottom)
  :init
  (add-hook 'which-key-mode-hook #'which-key-setup-side-window-bottom)
  :config
  (setq which-key-idle-delay .25)
  (setq which-key-idle-secondary-delay 0.125)
  (setq which-key-lighter ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Project / Repository Tools

(use-package projectile
  :ensure t
  :commands (projectile-mode
	     projectile-project-name
	     projectile-project-root
	     projectile-save-project-buffers)
  :init
  (defun turn-on-projectile-mode ()
    (interactive)
    (projectile-mode 1))
  (add-hook 'prog-mode-hook #'turn-on-projectile-mode)
  (add-hook 'text-mode-hook #'turn-on-projectile-mode)
  :config
  (keymap-set hud-mode-map "C-c p" (cons "projectile" projectile-command-map))
  (keymap-set hud-ecclectic-grep-project-map "a" #'projectile-ag)

  (defun tychoish/save-buffers-in-project-directory (dir)
    (let ((default-directory (expand-file-name dir)))
      (alert (projectile-save-project-buffers)
	     :title (format "emacs<%s> (%s)" sprite-instance-id dir))))

  (defun tychoish/save-project-buffers ()
    (interactive)
    (tychoish/save-buffers-in-project-directory
     (annotated-completing-read
      (thread-last
	(buffer-list)
	(seq-map #'buffer-file-name)
	(seq-filter 'identity)
	(seq-sort #'string<)
	(seq-map #'projectile-project-root)
	(seq-uniq)
	(seq-filter 'identity)
	(seq-map (lambda (dir) (cons dir nil))))
      :prompt "directories =>"
      :category 'file)))

  (setq projectile-enable-caching t)
  (setq projectile-use-git-grep t)
  (setq projectile-completion-system 'default)
  (setq projectile-require-project-root nil)
  (setq projectile-known-projects-file (sprite-state-path "projectile-bookmarks.el"))
  (setq projectile-frecency-file (sprite-state-path "projectile-frecency.eld"))

  (defun projectile-mode-enable-for-buffer (buf)
    (with-current-buffer buf
      (projectile-mode 1)))

  (defun projectile-mode-disable-for-buffer (buf)
    (with-current-buffer buf
      (projectile-mode -1)))

  (defun projectile-enable-all-buffers ()
    "Enable `projectile-mode' in all live buffers."
    (interactive)
    (seq-do #'projectile-mode-enable-for-buffer (buffer-list)))

  (defun projectile-disable-all-buffers ()
    "Disable `projectile-mode' in all live buffers."
    (interactive)
    (seq-do #'projectile-mode-disable-for-buffer (buffer-list))))

(use-package dired
  :ensure nil
  :defer t
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  (keymap-set dired-mode-map "w" #'wdired-change-to-wdired-mode))

(use-package recentf
  :ensure nil
  :defer t
  :init
  (keymap-set global-map "C-x C-r" #'recentf)
  (setq recentf-auto-cleanup 'never)
  (setq recentf-keep '(file-remote-p file-readable-p))
  (setq recentf-max-menu-items 100)
  :config
  (setq recentf-save-file (sprite-state-path "recentf.el")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; grep/search

(use-package ripgrep
  :ensure t
  :defines (hud-ecclectic-rg-map)
  :commands (consult-rg
	     consult-rg-pwd
	     consult-rg-pwd-wizard
	     consult-rg-project
	     consult-rg-project-wizard
	     find-ripgrep
	     find-ripgrep-compile
	     find-ripgrep-project
	     find-merge-conflicts
	     ripgrep-regexp)
  :init
  (keymap-set global-map "M-g r" #'consult-rg)
  ;; "C-c C-;"
  (keymap-set hud-consult-mode-map "d" #'consult-rg-pwd)
  (keymap-set hud-consult-mode-map "r" #'consult-rg)
  ;; "C-c g r"
  (keymap-set hud-ecclectic-rg-map "d" #'find-ripgrep)
  (keymap-set hud-ecclectic-rg-map "c" #'find-ripgrep-compile)
  (keymap-set hud-ecclectic-rg-map "m" #'find-merge-conflicts)
  (keymap-set hud-ecclectic-rg-map "g" #'consult-rg)
  (keymap-set hud-ecclectic-rg-map "s" #'consult-rg-pwd)
  (keymap-set hud-ecclectic-rg-map "l" #'consult-rg-pwd-wizard)
  (keymap-set hud-ecclectic-rg-map "r" #'consult-rg-project)
  (keymap-set hud-ecclectic-rg-map "p" #'consult-rg-project-wizard)
  (keymap-set hud-ecclectic-grep-project-map "r" #'consult-rg-project)
  (keymap-set hud-ecclectic-grep-project-map "p" #'find-ripgrep-project)
  :config
  (setenv "RIPGREP_CONFIG_PATH" (expand-file-name "~/.ripgreprc"))
  (defvar ripgrep-regexp-history nil)

  (cl-defun consult-rg (&key directory initial context)
    "Start an iterative rg session. DIR and INITIAL integrate with the consult-grep API."
    (interactive "P")
    (let ((context (or context current-prefix-arg)))
      (consult-ripgrep
       (or (when directory (string-trim directory))
	   (annotated-completing-read-directory)
	   (approximate-project-root))
       (if (and (or context (not initial)) (not (eq context 'override)))
	   (annotated-completing-read-context-from-point
	    :prompt "rg(init):")
	 initial))))

  (cl-defun consult-rg-project (&optional initial &key context)
    "Start an iterative rg session in the project root, if possible, falling back as necessary."
    (interactive "P")
    (consult-rg
     :directory (or (approximate-project-root)
		    (annotated-completing-read-directory))
     :initial initial
     :context (or context current-prefix-arg 'override)))

  (cl-defun consult-rg-pwd (&optional initial &key context)
    "Start an iterative rg session for the current directory."
    (interactive "P")

    (consult-rg
     :directory (or default-directory (annotated-completing-read-directory))
     :initial initial
     :context (or context current-prefix-arg 'override)))

  (defun consult-rg-pwd-wizard (&optional initial)
    "Start an iterative rg session with context, with prompting to start a query for a collection of likely candidates."
    (interactive "P")
    (consult-rg-pwd initial :context t))

  (defun consult-rg-project-wizard (&optional initial)
    "Start an iterative rg session with context. Always run the search in the project root, falling back if there isn't a discernable root."
    (interactive "P")
    (consult-rg-project initial :context t))

  ;; find-ripgrep -- compilation buffer wrappers

  (cl-defun ripgrep-compile (&key regexp directory buffer-name)
    (let ((compilation-buffer-name-function (lambda (&optional _) (or buffer-name (format "*%s-rg*" (approximate-project-name))))))
      (ripgrep-regexp regexp directory)))

  (cl-defun find-ripgrep--resolve-regexp (&key regexp directory)
    (annotated-completing-read-context-from-point
     :prompt (format "[%s]<rg>:" directory)
     :history 'ripgrep-regexp-history
     :initial-input regexp))

  (defun find-ripgrep ()
    "Run `rg' (ripgrep) from the system in a compile buffer with the provided regular expression."
    (interactive)
    (ripgrep-compile
     :directory default-directory
     :buffer-name (format "*%s-rg*" (file-name-nondirectory default-directory))
     :regexp (find-ripgrep--resolve-regexp
	      :directory default-directory)))

  (defun find-ripgrep-compile (&optional initial)
    (interactive "P")
    (let ((directory (annotated-completing-read-directory)))
      (ripgrep-compile
       :directory directory
       :buffer-name (format "*%s-rg*" (file-name-nondirectory (directory-file-name directory)))
       :regexp (find-ripgrep--resolve-regexp
		:regexp initial
		:directory directory))))

  (defun find-ripgrep-project ()
    "Run `rg' from the system at the project root. Output is written to a compile buffer."
    (interactive)
    (ripgrep-compile
     :directory (approximate-project-root)
     :regexp (find-ripgrep--resolve-regexp
	      :directory (approximate-project-root))))

  (defun find-merge-conflicts ()
    "Use ripgrep to identify all merge conflict artifacts"
    (interactive)
    (let ((root (approximate-project-root)))
      (ripgrep-compile
       :regexp "^(=======$|<<<<<<<|>>>>>>>)"
       :directory root
       :buffer-name (format "*%s-merge-conflicts*"
                            (file-name-nondirectory (directory-file-name root)))))))

(use-package deadgrep
  :ensure t
  :commands (deadgrep)
  :init
  (keymap-set hud-ecclectic-rg-map "x" #'deadgrep))

(use-package wgrep
  :ensure t
  :after (grep)
  :commands (wgrep-change-to-wgrep-mode)
  :init
  (keymap-set grep-mode-map "w" #'wgrep-change-to-wgrep-mode)
  :config
  (setq wgrep-enable-key "w"))

(use-package anzu
  :ensure t
  :commands (anzu-query-replace anzu-query-replace-regexp global-anzu-mode anzu-mode)
  :init
  (keymap-set hud-anzu-map "r" #'anzu-query-replace)
  (keymap-set hud-anzu-map "e" #'anzu-query-replace-regexp)
  (keymap-set isearch-mode-map "C-o" #'isearch-occur)
  (add-hook 'isearch-mode-hook #'anzu-mode)
  (setq anzu-cons-mode-line-p nil)
  :config
  (delight 'anzu-mode)
  (seq-do #'make-variable-buffer-local
          '(anzu--total-matched anzu--current-position anzu--state
            anzu--cached-count anzu--cached-positions anzu--last-command
            anzu--last-isearch-string anzu--overflow-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Completion/Snippets Menus

(use-package cape
  :ensure t
  :commands (cape-capf-inside-code
	     cape-capf-inside-comment
	     cape-capf-inside-string
	     cape-dabbrev
	     cape-history
	     cape-file
	     cape-elisp-symbol
	     cape-elisp-block
	     cape-abbrev
	     cape-line
	     cape-dict
	     cape-keyword
	     cape-emoji
	     cape-tex
	     cape-sgml
	     cape-rfc1345)
  :init
  ;; "C-c ." this is mostly copy-pasta from cape-mode-map, with tweaks
  (keymap-set hud-completion-map "t" #'complete-tag)
  (keymap-set hud-completion-map "d" #'cape-dabbrev)
  (keymap-set hud-completion-map "h" #'cape-history)
  (keymap-set hud-completion-map "f" #'cape-file)
  (keymap-set hud-completion-map "n" #'cape-elisp-symbol)
  (keymap-set hud-completion-map "b" #'cape-elisp-block)
  (keymap-set hud-completion-map "a" #'cape-abbrev)
  (keymap-set hud-completion-map "l" #'cape-line)
  (keymap-set hud-completion-map "w" #'cape-dict)
  (keymap-set hud-completion-map "k" #'cape-keyword)
  (keymap-set hud-completion-map ":" #'cape-emoji)
  (keymap-set hud-completion-map "e" #'cape-emoji)
  (keymap-set hud-completion-map "\\" #'cape-tex)
  (keymap-set hud-completion-map "/" #'cape-sgml)
  (keymap-set hud-completion-map "u" #'cape-rfc1345)
  (eval-and-compile
    (defmacro cape-capf-wrapper (wrapper inner)
      (when inner
	(let* ((wrapper (if (stringp wrapper) (intern wrapper) wrapper))
	       (wrapper-name (symbol-name wrapper))
	       (capf-name (symbol-name inner))
	       (name (format "%s-<%s>" capf-name wrapper-name))
	       (symb (intern name)))
	  `(defun ,symb ()
	     (funcall ',wrapper ',inner))))))

  (defun tychoish/maybe-capf-dict ()
    (and (boundp 'cape-dict-file) (file-exists-p cape-dict-file) #'cape-dict))

  (defun tychoish/get-available-word-capfs ()
    (seq-filter #'symbolp (list (tychoish/maybe-capf-dict))))

  (defun tychoish/text-mode-capf-setup ()
    "so here is the"
    (setq-local completion-at-point-functions
		(thread-last
		  (list #'cape-dabbrev
			#'yasnippet-capf
			#'cape-rfc1345
			#'cape-emoji
			#'cape-file
			(tychoish/maybe-capf-dict))
		  (flatten-tree)
		  (seq-filter 'identity))))

  (defun tychoish/elisp-capf-setup  ()
    (setq-local completion-at-point-functions
		(thread-last (list #'cape-elisp-symbol
				   (cape-capf-wrapper cape-capf-inside-code cape-elisp-block)
				   #'cape-dabbrev
				   (cape-capf-wrapper cape-capf-inside-code cape-keyword)
				   #'yasnippet-capf
				   (thread-last (tychoish/get-available-word-capfs)
						(seq-map (lambda (in)
							   `(progn
							      (list (cape-capf-wrapper cape-capf-inside-comment ,in)
								    (cape-capf-wrapper cape-capf-inside-string ,in)))))
						(seq-map 'eval))
				   #'cape-emoji)
			     (flatten-tree)
			     (seq-filter 'identity)
			     (seq-uniq))))

  (defun tychoish/eglot-capf-setup ()
    (interactive) ;; todo remove
    (setq-local completion-category-defaults nil)
    (setq-local completion-at-point-functions
		(thread-last (list #'eglot-completion-at-point
				   #'cape-dabbrev
				   (thread-last (tychoish/get-available-word-capfs)
						(seq-map (lambda (in)
							   `(progn
							      (list (cape-capf-wrapper cape-capf-inside-comment ,in)
								    (cape-capf-wrapper cape-capf-inside-string ,in)))))
						(seq-map 'eval))
				   #'yasnippet-capf
				   #'cape-emoji
				   #'cape-file)
			     (flatten-tree)
			     (seq-filter 'identity)
			     (seq-uniq))))

  (with-eval-after-load 'eglot
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

  (add-hook 'completion-at-point-functions #'cape-rfc1345)
  (add-hook 'completion-at-point-functions #'cape-emoji)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-history)
  (add-hook 'completion-at-point-functions (cape-capf-wrapper cape-capf-inside-code cape-keyword))

  (add-hook 'eglot-managed-mode-hook #'tychoish/eglot-capf-setup)
  (add-hook 'emacs-lisp-mode-hook #'tychoish/elisp-capf-setup)
  (add-hook 'telega-chat-mode-hook #'tychoish/text-mode-capf-setup)
  (add-hook 'text-mode-hook #'tychoish/text-mode-capf-setup))

(use-package yasnippet
  :ensure t
  :commands (yas-global-mode yas-insert-snippet yas-minor-mode yas-expand-snippet yas-lookup-snippet)
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
  :commands (yasnippet-capf)
  :init
  (keymap-set hud-completion-map "s" #'yasnippet-capf)
  (declare-function yasnippet-capf "yasnippet-capf")
  (add-hook 'completion-at-point-functions #'yasnippet-capf))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :defer t)

(use-package dabbrev
  :ensure nil
  :defer t
  :init
  (keymap-set global-map "M-/" #'dabbrev-completion)
  (keymap-set global-map "C-M-/" #'dabbrev-expand)
  (keymap-set hud-completion-map "/" #'dabbrev-completion)
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'authinfo-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(use-package vertico
  :ensure t
  :defines (vertico-multiform-categories vertico-sort-function vertico-multiform-commands)
  :commands (vertico-mode vertico-multiform-mode)
  :init
  (add-lazy-init
   :name "<core> vertico primary"
   :delay 0.5
   :operation 'vertico-mode)
  (add-one-shot-hook
   :name "<core> vertico multiform"
   :hook vertico-mode-hook
   :function #'vertico-multiform-mode)

  (setq vertico-resize t)
  (setq vertico-count 25)
  (setq vertico-cycle t)
  (defvar vertico-multiform-categories nil)
  (defvar vertico-multiform-commands nil)
  :config
  (add-to-list 'vertico-multiform-commands '(yank (vertico-sort-function . nil)
						  (vertico-sort-override-function . nil)))
  (add-to-list 'vertico-multiform-commands '(yank-from-kill-ring (vertico-sort-function . nil)
								 (vertico-sort-override-function . nil)))
  (add-to-list 'vertico-multiform-commands '(consult-yank-from-kill-ring (vertico-sort-function . nil)
									 (vertico-sort-override-function . nil)))
  (add-to-list 'vertico-multiform-commands '(consult-yank-pop (vertico-sort-function . nil)
							      (vertico-sort-override-function . nil)))

  (add-to-list 'vertico-multiform-commands '("\\`execute-extended-command" (vertico-flat-annotate . t)
					     (marginalia-annotators (command marginalia-annotate-command marginalia-annotate-binding)))))

(use-package orderless
  :ensure t
  :after (vertico)
  :defer t
  :init
  (setq completion-styles '(orderless basic))
  (setq completion-category-overrides
	'((file (styles orderless basic partial-completion))
          (consult-grep (styles basic))
          (buffer (styles orderless basic))
          (command (styles orderless basic))
          (symbol (styles orderless basic))))
  (setq completion-preview-sort-function nil)
  :config
  ;; Orderless's own behavior knobs only; cross-cutting `completion-styles' and
  ;; `completion-category-overrides' are owned by the completion-flavor system.
  (setq orderless-component-separator #'orderless-escapable-split)
  (setq orderless-matching-styles
	'(orderless-literal orderless-prefixes orderless-initialism orderless-regexp)))

(use-package marginalia
  :ensure t
  :commands (marginalia-mode marginalia-cycle)
  :init
  (keymap-set minibuffer-local-map "C-c a" #'marginalia-cycle)
  (add-one-shot-hook
   :name "marginalia"
   :function marginalia-mode
   :hook 'minibuffer-setup-hook)
  :config
  (add-to-list 'marginalia-command-categories '(consult-completion-in-region . imenu)))

(use-package embark
  :ensure t
  :commands (embark-act embark-bindings embark-dwim)
  :init
  (keymap-set global-map "C-." #'embark-act)	      ;; pick some comfortable binding
  (keymap-set global-map "C-c H" #'embark-bindings) ;; alternative for `describe-bindings'
  (keymap-set global-map "M-;" #'embark-dwim)	       ;; good alternative: M-.
  :config
  (setq embark-prompter #'embark-keymap-prompter)
  ;; (prefix-help-command #'embark-prefix-help-command)
  (setq embark-quit-after-action '((kill-buffer . nil)))
  :config
  (add-to-list 'vertico-multiform-categories '(embark-keybinding grid))
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none)))))

(use-package corfu
  :ensure t
  :defer t
  :commands (corfu-mode corfu--in-region)
  :init
  (defun tychoish/corfu-text-mode-setup ()
    (corfu-mode)
    (setq-local corfu-auto-prefix 2))

  (defun tychoish/corfu-prog-mode-setup ()
    (corfu-mode)
    (setq-local corfu-auto-prefix 3))

  (add-hook 'text-mode-hook #'tychoish/corfu-text-mode-setup)
  (add-hook 'prog-mode-hook #'tychoish/corfu-prog-mode-setup)
  :config
  (add-hook 'corfu-mode-hook #'tychoish--corfu-maybe-terminal)
  (defun corfu-at-point ()
    "Run `completion-at-point', but force using corfu, which may be useful in gui terminals"
    (interactive)
    (let ((completion-in-region-function #'corfu--in-region))
      (completion-at-point)))

  (keymap-set hud-completion-map "m" #'corfu-at-point)
  (keymap-set hud-completion-map "x" #'corfu-at-point)

  (add-hook 'corfu-mode-hook 'corfu-history-mode)
  (add-hook 'corfu-mode-hook 'corfu-indexed-mode)
  (add-hook 'corfu-mode-hook 'corfu-popupinfo-mode)

  (keymap-set corfu-map "C-<tab>" #'corfu-quick-complete)
  (keymap-set corfu-map "<tab>" #'corfu-complete)
  (keymap-set corfu-map "<return>" #'corfu-insert)
  (keymap-set corfu-map "C-<return>" #'corfu-quick-insert)
  (keymap-set corfu-map "M-SPC" #'corfu-insert-separator)
  (keymap-set corfu-map "C-SPC" #'corfu-insert-separator)
  (keymap-set corfu-map "<backtab>" #'corfu-insert-separator)
  (keymap-set corfu-map "C-j" #'corfu-quick-jump)
  (keymap-set corfu-map "M-d" #'corfu-popupinfo-toggle)

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
  (setq completion-ignore-case t))

(use-package corfu-terminal
  :ensure t
  :commands (corfu-terminal-mode)
  :init
  (defun tychoish--corfu-maybe-terminal ()
    (unless (or (bound-and-true-p corfu-terminal-mode)
		(display-graphic-p))
      (corfu-terminal-mode +1)))
  :config
  (setq corfu-terminal-disable-on-gui t)
  (setq corfu-terminal-enable-on-minibuffer nil))

(use-package popon
  :ensure t
  :commands (popon-kill popon-create popon-x-y-at-posn))

(use-package nerd-icons-corfu
  :ensure t
  ;; :after (corfu nerd-icons)
  :commands (nerd-icons-corfu-formatter)
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-xref
  :ensure t
  :commands (nerd-icons-xref-mode)
  :init
  (add-hook 'nerd-icons-completion-mode-hook #'nerd-icons-xref-mode))

(use-package nerd-icons-completion
  :ensure t
  :commands (nerd-icons-completion-mode nerd-icons-completion-marginalia-setup)
  :init
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)
  (add-lazy-init
   :name "<core> nerd icons"
   :delay 0.8
   :operation 'nerd-icons-completion-mode))

(use-package xref
  :ensure nil
  :defer t
  :init
  (keymap-set global-map "M-." #'xref-find-definitions)
  ;; "C-c l"
  (keymap-set hud-ide-map "c" #'xref-find-references)
  (keymap-set hud-ide-map "d" #'xref-find-definitions)
  (keymap-set hud-ide-map "p" #'xref-go-back)
  (keymap-set hud-ide-map "n" #'xref-go-forward)
  (keymap-set hud-ide-map "o" #'xref-find-definitions-other-window))

(use-package consult
  :ensure t
  :functions (consult-xref consult--read consult-completion-in-region consult-register-window)
  :defines (consult-preview-key)
  :commands (consult-find
	     consult-git-grep
	     consult-grep
	     consult-mode-command
	     consult-kmacro
	     consult-register
	     consult-register-store
	     consult-register-load
	     consult-compile-error
	     consult-line
	     consult-info
	     consult-complex-command
	     consult-buffer
	     consult-buffer-other-window
	     consult-buffer-other-frame
	     consult-buffer-other-tab
	     consult-bookmark
	     consult-project-buffer
	     consult-recent-file
	     consult-yank-from-kill-ring
	     consult-imenu-multi
	     consult-goto-line
	     consult-mark
	     consult-global-mark
	     consult-imenu
	     consult-locate
	     consult-isearch-history
	     consult-line-multi
	     consult-keep-lines
	     consult-focus-lines
	     consult-history
	     consult-man
	     consult-at-point
	     consult-preview-at-point-mode)
  :init
  (add-hook 'completion-list-mode-hook #'consult-preview-at-point-mode)
  (keymap-set global-map "C-c C-x C-m" #'consult-mode-command)
  (keymap-set global-map "C-c C-; m" #'consult-kmacro)
  (keymap-set global-map "C-c C-x r r" #'consult-register)
  (keymap-set global-map "C-c C-x r s" #'consult-register-store)
  (keymap-set global-map "C-c C-x r l" #'consult-register-load)
  (keymap-set global-map "C-c e" #'consult-compile-error)
  (keymap-set global-map "C-c C-s" #'consult-line)
  (keymap-set global-map "C-c f C-f" #'consult-find)
  ;; tychoish/wacky
  (keymap-set global-map "<remap> <Info-search>" #'consult-info)
  ;; C-x bindings in `ctl-x-map'
  (keymap-set global-map "C-x :" #'consult-complex-command)	   ;; orig. repeat-complex-command
  (keymap-set global-map "C-x b" #'consult-buffer)		 ;; orig. list-buffers
  (keymap-set global-map "C-x 4 b" #'consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
  (keymap-set global-map "C-x 5 b" #'consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
  (keymap-set global-map "C-x t b" #'consult-buffer-other-tab)	   ;; orig. switch-to-buffer-other-tab
  (keymap-set global-map "C-x r b" #'consult-bookmark)		   ;; orig. bookmark-jump
  (keymap-set global-map "C-x p b" #'consult-project-buffer)	   ;; orig. project-switch-to-buffer
  (keymap-set global-map "C-x C-r" #'consult-recent-file)	   ;; orig. recentf
  ;; Custom M-# bindings for fast register access
  (keymap-set global-map "M-'" #'consult-register-store)	   ;; orig. abbrev-prefix-mark (unrelated)
  (keymap-set global-map "M-#" #'consult-register-load)
  (keymap-set global-map "C-M-#" #'consult-register)
  ;; Other custom bindings
  (keymap-set global-map "M-y" #'consult-yank-from-kill-ring)		      ;; orig. yank-pop
  (keymap-set global-map "C-M-I" #'consult-imenu-multi)
  ;; M-g bindings in `goto-map'
  (keymap-set global-map "M-g e" #'consult-compile-error)
  (keymap-set global-map "M-g g" #'consult-goto-line)		   ;; orig. goto-line
  (keymap-set global-map "M-g M-g" #'consult-goto-line)	   ;; orig. goto-line
  (keymap-set global-map "M-g m" #'consult-mark)
  (keymap-set global-map "M-g k" #'consult-global-mark)
  (keymap-set global-map "M-g i" #'consult-imenu)
  (keymap-set global-map "M-g s" #'consult-line)
  (keymap-set global-map "M-i" #'consult-imenu)
  ;; M-s bindings in `search-map'
  (keymap-set global-map "M-s d" #'consult-find)		   ;; Alternative: consult-find
  (keymap-set global-map "M-s c" #'consult-locate)
  ;; Isearch integration
  (keymap-set global-map "M-s e" #'consult-isearch-history)
  (keymap-set hud-completion-map "c" #'consult-at-point)
  (keymap-set hud-docs-map "i" #'consult-info)
  (keymap-set hud-docs-map "m" #'consult-man)
  (keymap-set hud-consult-mode-map "h" #'consult-history)
  ;; "C-c g"
  (keymap-set hud-ecclectic-grep-map "f" #'consult-grep)
  ;; "C-c g p"
  (keymap-set hud-ecclectic-grep-project-map "g" #'consult-git-grep)        	  ;; for git(?)
  ;; Minibuffer history
  (keymap-set minibuffer-local-map "M-s" #'consult-history)		   ;; orig. next-matching-history-element
  (keymap-set minibuffer-local-map "M-r" #'consult-history)		   ;; orig. previous-matching-history-element
  (keymap-set isearch-mode-map "M-e" #'consult-isearch-history)	   ;; orig. isearch-edit-string
  (keymap-set isearch-mode-map "M-s e" #'consult-isearch-history)	   ;; orig. isearch-edit-string
  (keymap-set isearch-mode-map "M-s l" #'consult-line)		   ;; needed by consult-line to detect isearch
  (keymap-set isearch-mode-map "M-s L" #'consult-line-multi) 	   ;; needed by consult-line to detect isearch
  ;; "C-c g s"
  (keymap-set hud-consult-search-map "l" #'consult-line)
  (keymap-set hud-consult-search-map "m" #'consult-line-multi)
  (keymap-set hud-consult-search-map "k" #'consult-keep-lines)
  (keymap-set hud-consult-search-map "f" #'consult-focus-lines)
  :config
  (defun consult-at-point ()
    "Run `completion-at-point', but force using consult, which may be useful in tty terminals"
    (interactive)
    (let ((completion-in-region-function #'consult-completion-in-region))
      (completion-at-point)))
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

  (consult-customize consult-ripgrep
		     :prompt "rg: "
		     :async-input-debounce 0.025
		     :async-input-throttle 0.05
		     :async-refresh-delay 0.025
		     :async-min-input 2)

  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   :require-match nil
   :group nil
   :keymap
   (with-temp-keymap map
     (define-key map (kbd "C-l") #'consult-ripgrep--up-directory))))

(use-package consult-flycheck
  :ensure t
  :commands (consult-flycheck)
  :init
  (keymap-set global-map "M-g f" #'flycheck-mode)
  (keymap-set hud-consult-mode-map "c" #'consult-flycheck)
  (with-eval-after-load 'flycheck
    (keymap-set flycheck-command-map ";" #'consult-flycheck)))

(use-package consult-flyspell
  :ensure t
  :after (flyspell)
  :commands (consult-flyspell flyspell-correct-consult)
  :init
  (keymap-set hud-consult-mode-map "f" #'consult-flyspell)
  :config
  (defun consult-flyspell--round-trip ()
    (flyspell-correct-at-point)
    (consult-flyspell))
  (setq consult-flyspell-select-function 'consult-flyspell--round-trip))

(use-package consult-eglot
  :ensure t
  :after (eglot)
  :commands (consult-eglot-symbols)
  :init
  (keymap-set hud-docs-map "a" #'consult-eglot-symbols)
  :config
  (consult-customize
   consult-eglot-symbols
   :initial (or (thing-at-point 'symbol)
		(thing-at-point 'defun)
		(thing-at-point 'sexp))))

(use-package consult-gh
  :ensure t
  :commands (consult-gh))

(use-package consult-yasnippet
  :ensure t
  :after (yasnippet)
  :commands (consult-yasnippet)
  :init
  (keymap-set hud-consult-mode-map "s" #'consult-yasnippet)
  (keymap-set hud-completion-map "y" #'consult-yasnippet))

(use-package embark-consult
  :ensure t
  :defer t
  :init
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

(use-package builder
  :commands (annotated-completing-read-directory
	     make-builder-candidate
	     builder-register-candidates
	     builder--read-command
	     builder-compile-project
	     builder-change-directory
	     builder-emacs-conf-run-ci-tests
	     builder-emacs-conf-byte-compile-and-delete-artifact
	     builder-emacs-conf-native-compile-all
	     builder-emacs-conf-byte-recompile-directory
	     builder-emacs-conf-recompile-vendored-packages)
  :init
  (with-eval-after-load 'compile
    (keymap-set compilation-mode-map "d" #'builder-change-directory)))

(use-package revbufs
  :ensure t
  :commands (revbufs)
  :init
  (keymap-set global-map "C-x x a" #'revbufs)
  (keymap-set hud-buffer-control-map "d" #'kill-buffers-in-directory)
  (keymap-set hud-buffer-control-map "<SPC>" #'revert-buffer-quick)
  (keymap-set hud-buffer-control-map "m" #'kill-buffers-matching-mode)
  (keymap-set hud-buffer-control-map "h" #'bury-buffer)
  (keymap-set hud-buffer-control-map "r" #'revbufs)
  (keymap-set hud-buffer-control-map "b" #'switch-to-buffer)
  (keymap-set hud-buffer-control-map "n" #'switch-to-buffer-other-window)
  :config
  (keymap-set revbufs-mode-map "C-k" #'revbufs-kill)
  (keymap-set revbufs-mode-map "q" #'bury-buffer)

  (defalias 'revbufs-kill
    (kmacro "C-f C-f C-f C-k")))

(use-package popper
  :ensure t
  :commands (popper-mode popper-echo-mode popper-cycle popper-toggle popper-kill-latest-popup)
  :init
  (keymap-set global-map "C-c '" #'popper-toggle)
  (keymap-set global-map "C-c \\" #'popper-cycle)
  (keymap-set global-map "C-c C-'" #'popper-kill-latest-popup)
  :config
  (add-hook 'popper-mode-hook 'popper-echo-mode)
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*Warnings\\*"
          "\\*Backtrace\\*"
          "\\*Compile-Log\\*"
          "\\*compilation\\*"
          "\\*grep\\*"
          "\\*xref\\*"
          "\\*Flymake diagnostics.*\\*"
          help-mode
          compilation-mode))
  (setq popper-echo-dispatch-keys '("C-1" "C-2" "C-3" "C-4" "C-5" "C-6" "C-7" "C-8" "C-9"))
  (setq popper-display-control nil)
  (setq popper-group-function #'popper-group-by-projectile))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; version control

(use-package magit
  :ensure t
  :after (cond-let)
  :commands (magit-toplevel magit-status magit-branch magit-blame)
  :init
  (keymap-set hud-magit-map "s" #'magit-status)
  (keymap-set hud-magit-map "f" #'magit-branch)
  (keymap-set hud-magit-map "b" #'magit-blame)
  (make-read-extended-command-for-prefix "magit"
    :bind-map hud-magit-map
    :bind-key "x")
  :config
  (setq vc-follow-symlinks t)
  (setq version-control t)
  (setq magit-module-sections-nested t)
  (put 'magit-diff-edit-hunk-commit 'disabled nil)
  (add-hook 'magit-status-sections-hook 'magit-insert-worktrees t)
  (add-hook 'magit-status-sections-hook 'magit-insert-modules t)

  ;; magit-20260609+ uses `thread$' from cond-let which requires cond-let>=0.3;
  ;; the installed cond-let doesn't define cond-let--thread$ yet.  Redefine the
  ;; one affected function using nested calls until cond-let catches up.
  ;; `magit--with-refresh-cache' (vendored, not ours) expands to `incf', which
  ;; triggers an obsolete-alias warning at this call site; suppressed since
  ;; there's no local fix for a third-party macro's expansion.
  (with-suppressed-warnings ((obsolete incf))
    (defun magit-config-get-from-cached-list (key)
      (gethash
       (replace-regexp-in-string "[^.]+\\'" #'downcase
         (replace-regexp-in-string "\\`[^.]+" #'downcase key t t)
         t t)
       (magit--with-refresh-cache (cons (magit-toplevel) 'config)
         (let ((configs (make-hash-table :test #'equal)))
           (dolist (conf (magit-git-items "config" "--list" "-z"))
             (let* ((nl-pos (cl-position ?\n conf))
                    (key (substring conf 0 nl-pos))
                    (val (if nl-pos (substring conf (1+ nl-pos)) "")))
               (puthash key (nconc (gethash key configs) (list val)) configs)))
           configs)))))

  (defvar-keymap magit-command-mode-map) ;; "/"
  (keymap-set magit-mode-map "/" '(magit-command-mode-map . "magit-command"))
  (keymap-set magit-command-mode-map "x" '(execute-extended-magit-command . "magit-commands"))
  (keymap-set magit-command-mode-map "m" '(execute-extended-magit-command . "(s)merge-commands"))

  (keymap-set magit-status-mode-map "C-n" #'next-line)
  (keymap-set magit-status-mode-map "C-p" #'previous-line)

  (when (fboundp 'hud-modeline-set-segment-action)
    (hud-modeline-set-segment-action 'vc #'magit-dispatch)))

(use-package magit-gh
  :ensure t
  :commands (magit-gh))

(use-package magit-dash
  :commands (magit-dash-view
	     magit-dash-open
	     magit-dash-open-repo
	     magit-dash-open-other-window
	     magit-dash-gh-pr-dashboard-open)
  :init
  (keymap-set hud-magit-map "d" #'magit-dash-open)
  (keymap-set hud-magit-map "o" #'magit-dash-open-repo)
  :config
  (require 'magit-gh)
  (require 'magit-dash-open)
  (require 'magit-dash-submodules)
  (require 'magit-dash-gh-pr)
  (require 'magit-dash-gh)
  (require 'magit-dash-gh-actions)
  (require 'magit-dash-timer)
  (setq magit-dash-gh-prune-cache-dir (sprite-state-path "magit-dash-gh-prune"))
  (setq magit-dash-gh-prune-pr-limit 50)
  (setq magit-dash-show-discovered-submodules nil)
  (add-hook 'magit-status-mode-hook
	    (lambda ()
	      (run-with-idle-timer 3 nil #'magit-dash-gh-prune-prefetch)))
  (keymap-set magit-mode-map "C-c C-d" #'magit-dash-open-other-window)

  (with-eval-after-load 'nerd-icons
    (setq nerd-icons-mode-icon-alist
          (seq-remove (lambda (entry)
                        (memq (car entry) '(magit-dash-mode
                                            magit-dash-gh-pr-dashboard-mode
                                            magit-dash-gh-actions-log-mode)))
                      nerd-icons-mode-icon-alist))
    (seq-do (lambda (entry)
              (add-to-list 'nerd-icons-mode-icon-alist entry))
            '((magit-dash-mode nerd-icons-devicon "nf-dev-git" :face nerd-icons-orange)
              (magit-dash-gh-pr-dashboard-mode nerd-icons-octicon "nf-oct-git_pull_request" :face nerd-icons-orange)
              (magit-dash-gh-actions-log-mode nerd-icons-octicon "nf-oct-workflow" :face nerd-icons-orange)))))

(use-package smerge-mode
  :after (magit)
  :defer t
  :commands (smerge-kill-and-vc-next-conflict)
  :init
  (keymap-set hud-smerge-map "n" #'smerge-vc-next-conflict)
  (keymap-set hud-smerge-map "k" #'smerge-kill-current)
  (keymap-set hud-smerge-map "s" #'smerge-start-session)
  (keymap-set hud-smerge-map "r" #'smerge-kill-and-vc-next-conflict)
  (keymap-set hud-smerge-map "t" #'smerge-keep-current)
  (make-read-extended-command-for-prefix "smerge"
    :bind-map hud-smerge-map
    :bind-key "x")
  :config
  (defun smerge-kill-and-vc-next-conflict ()
    "Kill the current conflict option and move to the next conflict."
    (interactive)
    (smerge-kill-current)
    (smerge-vc-next-conflict)))

(use-package sqlite-mode-extras
  :ensure t
  :commands (sqlite-extras-minor-mode)
  :init
  (add-hook 'sqlite-mode-hook #'sqlite-extras-minor-mode))

(use-package gist
  :ensure t
  :commands (gist-region gist-buffer gist-list gist-region-private gist-buffer-private))

(use-package git-link
  :ensure t
  :commands (git-link)
  :init
  (keymap-set hud-magit-map "l" #'git-link)
  :config
  (defun ad:git-link--new-copy-to-clipboard (link)
    "Also copy LINK to the system clipboard.
`select-enable-clipboard' is nil, so `kill-new' (called by
`git-link--new') only reaches the Emacs kill-ring, not the OS
clipboard."
    (with-temp-buffer
      (insert link)
      (clipboard-kill-ring-save (point-min) (point-max))))

  (advice-add 'git-link--new :after #'ad:git-link--new-copy-to-clipboard))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; chat and notifications

(use-package alert
  :ensure t
  :defines (alert-styles alert-libnotify-command)
  :commands (alert)
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
   ((and (eql system-type 'gnu/linux) (getenv "DBUS_SESSION_BUS_ADDRESS"))
    (setq-default alert-default-style 'notifications))
   (t (setq-default alert-default-style 'message)))

  (defun tychoish/darwin-alert-config-for-server ()
    (if (and (window-system)
	     (eq system-type 'darwin)
	     (not (eq alert-default-style 'osx-notifier)))
	(setq-default alert-default-style 'osx-notifier)
      (remove-hook 'server-after-make-frame-hook 'tychoish/darwin-alert-config-for-server)
      (unintern 'tychoish/darwin-alert-config-for-server obarray))))

(use-package emojify
  :ensure t
  :commands (global-emojify-mode)
  :config
  (delight 'emojify-mode)
  (setq emojify-emoji-styles '(ascii unicode github))
  (setq emojify-display-style 'unicode)
  (setq emojify-point-entered-behaviour 'echo))

(use-package tracking
  :ensure t
  :after (:any telega erc)
  :defer t
  :config
  (setq tracking-max-mode-line-entries 3))

(use-package telega
  :ensure t
  :defines (telega-chat-mode-hook)
  :commands (telega
	     telega-chat-mode
	     telega-extras-switch-to-root
	     telega-extras-force-kill
	     telega-extras-disconnect)
  :init
  (keymap-set hud-mode-map "C-c v" (cons "+telega-prefix" telega-prefix-map))
  (keymap-set hud-mode-map "C-c n" (cons "+telega-prefix" telega-prefix-map))
  :config
  (add-hook 'telega-chat-mode-hook 'tychoish/corfu-text-mode-setup)

  (defvar-keymap tychoish/telega-buffer-management-map) ;; "d"
  (keymap-set telega-prefix-map "d" '(tychoish/telega-buffer-management-map . "telega-buffer-management"))
  (keymap-set tychoish/telega-buffer-management-map "h" #'telega-extras-bury-chat-buffers)
  (keymap-set tychoish/telega-buffer-management-map "k" #'telega-extras-kill-chat-buffers)
  (keymap-set telega-chat-mode-map "C-c C-f" #'telega-chat-buffer-auto-fill)
  (keymap-set telega-root-mode-map "C-c C-f" #'telega-root-buffer-auto-fill)
  (keymap-set telega-root-mode-map "<tab>" #'telega-extras-root-cycle-next)
  (keymap-set telega-root-mode-map "h" #'telega-extras-bury-chat-buffers)
  (keymap-set telega-root-mode-map "C-k" #'telega-extras-kill-chat-buffers)
  (keymap-set telega-root-mode-map "d" #'telega-extras-disconnect)

  (make-read-extended-command-for-prefix "telega"
    :bind-map telega-prefix-map
    :bind-key "x"
    :key-alias "telega-commands")
  (add-hook 'telega-chat-mode-hook #'telega-chat-auto-fill-mode)

  (when (eq system-type 'darwin)
    (setq telega-server-libs-prefix "/opt/homebrew")
    (setq-default alert-default-style 'osx-notifier))

  (when (eq system-type 'gnu/linux)
    (setq telega-server-libs-prefix "/usr"))

  (setq telega-emoji-use-images t)

  (setq telega-root-view-grouping-folders t)
  (setq telega-folder-icons-alist nil)

  (setq telega-root-view-grouping-other-chats nil)
  (setq telega-root-view-grouping-alist '(("open" . has-chatbuf)
					  ("personal" chat-list "personal")
					  ("groups" type basicgroup supergroup)))

  (setq telega-filter-custom-show-folders nil)
  (setq telega-filter-button-width '(.2 12 16))
  (setq telega-filters-custom '(("main" main)
				("groups" type basicgroup supergroup)
				("broadcast" type channel)
				("archive" archive)))
  (setq telega-root-view-top-categories
	'(("Users" . 10) ("Groups" . 10) ("Channels" . 10) ("Bots" . 10)
	  ("InlineBots" . 10) ("Calls" . 10)))
  (setq telega-chat-folders-exclude '("unknowns" "InlineBots" "Calls" "Personal"))

  (setq telega-chat-folders-insexp #'telega-folders-insert-default)

  (setq telega-use-images t)
  (setq telega-chat-input-markups '("markdown2"))
  (setq telega-use-tracking-for '(or unmuted mention unread))
  (setq telega-markdown2-backquotes-as-precode t)
  (setq telega-debug nil)

  (setq telega-chat--display-buffer-action
	'((display-buffer-reuse-window display-buffer-use-some-window)))

  (require 'telega-alert)
  (require 'telega-extras)
  (setq telega-chat-auto-fill-mode-lighter "")
  (telega-mode-line-mode 1)
  (telega-alert-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; writing (english) configuration

(use-package denote
  :ensure t
  :commands (denote
	     denote-backlinks
	     denote-dired
	     denote-link
	     denote-open-or-create
	     denote-org-capture
	     denote-rename-file
	     denote-rename-file-using-front-matter)
  :init
  (keymap-set hud-denote-map "n" #'denote)
  (keymap-set hud-denote-map "m" #'denote-open-or-create)
  (keymap-set hud-denote-map "f" #'consult-denote-find)
  (keymap-set hud-denote-map "l" #'denote-link)
  (keymap-set hud-denote-map "b" #'denote-backlinks)
  (keymap-set hud-denote-map "r" #'denote-dash-rename-file)
  (keymap-set hud-denote-map "." #'denote-dash-dispatch)
  (keymap-set hud-denote-map "," #'denote-dash)
  (keymap-set hud-denote-map "k" #'denote-dash-save-and-kill-all-notes)
  (keymap-set hud-denote-map "u" #'denote-dash-rename-file-using-front-matter)
  (make-read-extended-command-for-prefix "denote"
    :bind-map hud-denote-map
    :bind-key "x")
  :config
  (consult-denote-mode)
  (when (default-boundp 'denote-directory)
    (setq denote-directory '()))
  (add-to-list 'denote-directory (file-name-concat (or local-notes-directory (expand-file-name "~/notes")) "denote"))
  (setq denote-file-type 'org)
  (setq denote-known-keywords '("project" "reference" "journal" "idea" "writing" "migration" "agent" "plan" "singing"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-prompts '(title keywords file-type))
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'denote--title-history)
    (add-to-list 'savehist-additional-variables 'denote--keywords-history))
  (defun tychoish--denote-rename-buffer (&optional buffer)
    "Rename denote BUFFER to `[D:SEQ] TITLE' or just TITLE when there is no sequence."
    (when-let* ((file (buffer-file-name buffer))
                ((denote-file-has-identifier-p file)))
      (let* ((sig   (denote-retrieve-filename-signature file))
             (title (or (denote-retrieve-filename-title file)
                        (file-name-base file)))
             (name  (if (and sig (not (string-empty-p sig)))
                        (format "[D:%s] %s" sig title)
                      title)))
        (rename-buffer name :unique))))
  (setq denote-rename-buffer-function #'tychoish--denote-rename-buffer)
  (denote-rename-buffer-mode 1)

  (defun ad:denote-rename-file-using-front-matter--no-confirm (fn &rest args)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'y-or-n-p)    (lambda (&rest _) t)))
      (apply fn args)))
  (advice-add 'denote-rename-file-using-front-matter :around
              #'ad:denote-rename-file-using-front-matter--no-confirm)

  (defun ad:denote-link-ol-get-heading--org-only (fn &rest args)
    "Only call FN in `org-mode' buffers.
`denote-link-ol-store' calls this unconditionally, even from
`markdown-mode' buffers, which makes `org-get-heading' warn via
`org-element-at-point' about running outside an Org buffer."
    (when (derived-mode-p 'org-mode)
      (apply fn args)))
  (advice-add 'denote-link-ol-get-heading :around
              #'ad:denote-link-ol-get-heading--org-only))

(use-package denote-dash
  :ensure nil
  :after transient
  :commands (denote-dash
	     denote-dash-dispatch
	     denote-dash-close-all-notes
	     denote-dash-save-and-kill-all-notes
	     denote-dash-rename-file
	     denote-dash-retag-file
	     denote-dash-rename-file-using-front-matter))

(use-package denote-dash-repack
  :ensure nil
  :after denote-dash
  :commands (denote-dash-lint-sequences
	     denote-dash-fix-sequence-frontmatter
	     denote-dash-fix-all-sequence-frontmatter
	     denote-dash-repack-sequence-children
	     denote-dash-swap-with-parent
	     denote-dash-swap-with-previous
	     denote-dash-swap-with-next
	     denote-dash-reparent
	     denote-dash-reparent-recursive
	     denote-dash-renumber-recursive
	     denote-dash-insert-sequence-note
	     denote-dash-retag-sequence))

(use-package denote-notion
  :ensure nil
  :commands (denote-notion-push
	     denote-notion-pull))

(use-package org-docsgen
  :ensure nil
  :commands (org-docsgen-run
	     org-docsgen-regenerate-readme))

(use-package consult-notes
  :ensure t
  :commands (consult-notes consult-notes-search-in-all-notes)
  :init
  (keymap-set hud-denote-map ";" #'consult-notes)
  (keymap-set hud-denote-map "/" #'consult-notes-search-in-all-notes)
  :config
  (consult-notes-denote-mode 1)

  (defun ad:consult-notes--single-directory-string (fn &rest args)
    "Let-bind `denote-directory' to a single string for the duration of FN.
`consult-notes' and `consult-notes-denote' treat `denote-directory' as a
bare directory string in several places (`expand-file-name',
`file-relative-name'), but this config sets it to a list per the
`denote-directories' multi-directory convention.  Substituting the common
root here keeps those call sites working without patching the vendored
package.  The let-binding covers the whole interactive session (including
any async candidate recomputation), since the advised commands don't
return until the minibuffer session ends."
    (let ((denote-directory (denote-directories-get-common-root)))
      (apply fn args)))
  (advice-add 'consult-notes :around #'ad:consult-notes--single-directory-string)
  (advice-add 'consult-notes-search-in-all-notes :around #'ad:consult-notes--single-directory-string))

(use-package consult-denote
  :ensure t
  :commands (consult-denote-find consult-denote-grep consult-denote-mode)
  :init
  (keymap-set hud-denote-map "f" #'consult-denote-find)
  (keymap-set hud-denote-map "g" #'consult-denote-grep))

(use-package denote-journal
  :ensure t
  :after denote
  :commands (denote-journal-new-entry denote-journal-new-entry-after-last)
  :init
  (keymap-set hud-denote-map "j" #'denote-journal-new-entry)
  :config
  (setq denote-journal-title-format nil)
  (setq denote-journal-directory (file-name-concat local-notes-directory "denote" "journal")))

(use-package denote-sequence
  :ensure t
  :after denote
  :commands (denote-sequence-new-child denote-sequence-new-sibling
             denote-sequence-new-parent denote-sequence-link
             denote-sequence-rename-as-parent)
  :init
  (keymap-set hud-denote-sequence-map "c" #'denote-sequence-new-child)
  (keymap-set hud-denote-sequence-map "s" #'denote-sequence-new-sibling)
  (keymap-set hud-denote-sequence-map "r" #'denote-sequence-rename-as-parent)
  (keymap-set hud-denote-sequence-map "p" #'denote-sequence-new-parent)
  (keymap-set hud-denote-sequence-map "l" #'denote-sequence-link)
  (keymap-set hud-denote-sequence-map "m" #'denote-dash-reparent)
  (keymap-set hud-denote-sequence-map "n" #'denote-dash-renumber-recursive)
  (keymap-set hud-denote-sequence-map "i" #'denote-dash-insert-sequence-note)
  :config
  (setq denote-sequence-scheme 'alphanumeric)
  (add-to-list 'display-buffer-alist
               '("\\`\\*denote-sequence-hierarchy "
                 (display-buffer-same-window))))

(use-package denote-markdown
  :ensure t
  :after (denote markdown-mode)
  :commands (denote-markdown-convert-links-to-markdown-format
             denote-markdown-convert-links-to-denote-format))

(use-package denote-org
  :ensure t
  :after denote
  :commands (denote-org-link-to-heading
             denote-org-backlinks-for-heading
             denote-org-extract-org-subtree
             denote-org-convert-links-to-file-type
             denote-org-convert-links-to-denote-type
             denote-org-dblock-insert-links
             denote-org-dblock-insert-missing-links
             denote-org-dblock-insert-backlinks
             denote-org-dblock-insert-files
             denote-org-dblock-insert-files-as-headings
             orgx-migrate-subtree-to-denote)
  :init
  (keymap-set hud-denote-org-map "l" #'denote-org-link-to-heading)
  (keymap-set hud-denote-org-map "b" #'denote-org-backlinks-for-heading)
  (keymap-set hud-denote-org-map "x" #'denote-org-extract-org-subtree)
  (keymap-set hud-denote-org-map "r" #'orgx-migrate-subtree-to-denote)
  (keymap-set hud-denote-org-map "d" #'denote-org-dblock-insert-links)
  (keymap-set hud-denote-org-map "p" #'denote-org-dblock-insert-backlinks)
  (keymap-set hud-denote-org-map "f" #'denote-org-dblock-insert-files))

(use-package denote-explore
  :ensure t
  :after denote
  :commands (denote-explore-count-notes
             denote-explore-count-keywords
             denote-explore-random-note
             denote-explore-random-link
             denote-explore-random-keyword
             denote-explore-duplicate-notes
             denote-explore-single-keywords
             denote-explore-zero-keywords
             denote-explore-rename-keyword
             denote-explore-sync-metadata
             denote-explore-missing-links
             denote-explore-barchart-keywords
             denote-explore-barchart-timeline
             denote-explore-barchart-filetypes)
  :init
  (keymap-set hud-denote-explore-map "n" #'denote-explore-count-notes)
  (keymap-set hud-denote-explore-map "c" #'denote-explore-count-keywords)
  (keymap-set hud-denote-explore-map "r" #'denote-explore-random-note)
  (keymap-set hud-denote-explore-map "l" #'denote-explore-random-link)
  (keymap-set hud-denote-explore-map "d" #'denote-explore-duplicate-notes)
  (keymap-set hud-denote-explore-map "s" #'denote-explore-single-keywords)
  (keymap-set hud-denote-explore-map "z" #'denote-explore-zero-keywords)
  (keymap-set hud-denote-explore-map "k" #'denote-explore-rename-keyword)
  (keymap-set hud-denote-explore-map "m" #'denote-explore-missing-links)
  (keymap-set hud-denote-explore-map "y" #'denote-explore-sync-metadata)
  (keymap-set hud-denote-explore-map "b" #'denote-explore-barchart-keywords)
  (keymap-set hud-denote-explore-map "t" #'denote-explore-barchart-timeline)
  :config
  (setq denote-explore-network-directory (file-name-concat (or local-notes-directory (expand-file-name "~/notes")) "explore"))
  (setq denote-explore-network-format "graphviz")
  (setq denote-explore-network-graphviz-filetype "svg"))

(use-package denote-review
  :ensure t
  :after denote
  :commands (denote-review-set-date-dired-marked-files
             denote-review-set-date
             denote-review-display-list)
  :init
  (keymap-set hud-denote-review-map "d" #'denote-review-set-date)
  (keymap-set hud-denote-review-map "l" #'denote-review-display-list)
  :config
  (setq denote-review-insert-after "date"))

(use-package denote-journal-capture
  :ensure t
  :after (denote denote-journal)
  :defer t)

(use-package orgx
  :ensure nil
  :commands (orgx-capture
	     orgx-agenda-view
	     orgx-agenda-files-open
	     orgx-agenda-files-reload
	     orgx-agenda-untagged-in-file
	     orgx-agenda-for-file
	     orgx-minor-mode-turn-on
	     orgx-agenda-minor-mode-turn-on
	     orgx--install-auxiliary-packages
	     ad:org-agenda--open-files
	     bootstrap-set-notes-directory
	     org-annotate-file)
  :init
  ;; "C-c o"
  (keymap-set orgx-global-map "a" #'orgx-agenda-view)
  (keymap-set orgx-global-map "c" #'orgx-capture)
  (keymap-set orgx-global-map "4" #'org-agenda)
  (keymap-set orgx-global-map "k" #'org-capture)
  (keymap-set orgx-global-map "f" #'orgx-agenda-files-open)
  (keymap-set orgx-global-map "s" #'org-save-all-org-buffers)
  (keymap-set orgx-global-map "r" #'orgx-agenda-files-reload)
  (keymap-set orgx-global-map "j" #'orgx-capture)
  (keymap-set orgx-global-map "u" #'orgx-agenda-untagged-in-file)
  (keymap-set orgx-global-map "/" #'orgx-agenda-for-file)
  ;; "C-c o l"
  (keymap-set orgx-link-map "s" #'org-store-link)
  (keymap-set orgx-link-map "i" #'org-insert-link)
  (keymap-set orgx-link-map "a" #'org-annotate-file)
  (add-hook 'org-mode-hook #'orgx-minor-mode-turn-on)
  (add-hook 'org-agenda-mode-hook #'orgx-agenda-minor-mode-turn-on)
  (add-one-shot-hook
   :name "org-install-aux-packages"
   :hook 'org-mode-hook
   :operation #'orgx--install-auxiliary-packages)
  :config
  (advice-add 'org-agenda :before #'ad:org-agenda--open-files))

(use-package orgx-capture
  :ensure nil
  :commands (orgx-capture-add-note-templates
	     orgx-add-project-file-capture-templates
	     orgx-capture-add-journal-templates
	     orgx-capture-add-task-templates)
  :init
  (with-eval-after-load 'org
    (require 'orgx-capture))
  :config
  (orgx--setup-standard-capture-templates))

(use-package markdown-mode
  :ensure t
  :mode ("\\.mdwn" "\\.md" "\\.markdown" "\\.txt")
  :commands (tychoish/markdown-align-tables
             tychoish/markdown-align-tables-in-file
             tychoish/markdown-align-tables-dired)
  :config
  (delight 'markdown-mode "mdwn")
  (defalias 'markdown-indent-code (kmacro "SPC SPC SPC SPC SPC C-a C-n"))
  (add-hook 'markdown-mode-hook 'turn-off-auto-fill)
  (defun tychoish/markdown-setup-imenu ()
    (setq imenu-generic-expression markdown-imenu-generic-expression))
  (add-hook 'markdown-mode-hook #'tychoish/markdown-setup-imenu)

  (if (eq system-type 'darwin)
      (setq markdown-command "/usr/local/bin/mmd --nosmart")
    (setq markdown-command "/usr/bin/markdown"))

  (setq markdown-imenu-generic-expression
	'(("title"  "^\\(.*\\)[\n]=+$" 1)
	  ("h2-"    "^\\(.*\\)[\n]-+$" 1)
	  ("h1"	  "^# \\(.*\\)$" 1)
	  ("h2"	  "^## \\(.*\\)$" 1)
	  ("h3"	  "^### \\(.*\\)$" 1)
	  ("h4"	  "^#### \\(.*\\)$" 1)
	  ("h5"	  "^##### \\(.*\\)$" 1)
	  ("h6"	  "^###### \\(.*\\)$" 1)
	  ("fn"	  "^\\[\\^\\(.*\\)\\]" 1)))

  (defun tychoish/markdown-align-tables ()
    "Align all tables in the current buffer."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (let ((count 0))
        (while (re-search-forward "^|" nil t)
          (when (markdown-table-at-point-p)
            (markdown-table-align)
            (setq count (1+ count))
            (goto-char (markdown-table-end))))
        (message "Aligned %d table(s)" count))))

  (defun tychoish/markdown-align-tables-in-file (file)
    "Align all markdown tables in FILE."
    (interactive "fMarkdown file: ")
    (with-current-buffer (find-file-noselect file)
      (tychoish/markdown-align-tables)
      (save-buffer)))

  (defun tychoish/markdown-align-tables-dired ()
    "Align tables in all marked files in the current dired buffer."
    (interactive)
    (seq-do #'tychoish/markdown-align-tables-in-file (dired-get-marked-files))))

(use-package fountain-mode
  :ensure t
  :mode ("\\.script" "\\.sp"))

(use-package flyspell
  :ensure t
  :defer t
  :init
  (defun tychoish--flyspell-run-in-text-buffer (buf)
    (when (buffer-live-p buf)
      (with-current-buffer buf
	(flyspell-mode 1))))

  (defun tychoish--flyspell-run-in-prog-buffer (buf)
    (when (buffer-live-p buf)
      (with-current-buffer buf
	(flyspell-prog-mode))))

  (defun tychoish--flyspell-mode-idle ()
    "Enable `flyspell-mode' in the current buffer once Emacs is idle."
    (run-with-idle-timer 0.2 nil #'tychoish--flyspell-run-in-text-buffer (current-buffer)))

  (defun tychoish--flyspell-prog-mode-idle ()
    "Enable `flyspell-prog-mode' in the current buffer once Emacs is idle."
    (run-with-idle-timer 0.2 nil #'tychoish--flyspell-run-in-prog-buffer (current-buffer)))

  (keymap-set global-map "C-c [" #'flyspell-correct-next)
  (keymap-set global-map "C-c ]" #'flyspell-correct-previous)
  (keymap-set global-map "M-$" #'flyspell-correct-at-point)
  (keymap-set global-map "C-;" #'flyspell-correct-previous)

  (add-hook 'prog-mode-hook #'tychoish--flyspell-prog-mode-idle)
  (add-hook 'text-mode-hook #'tychoish--flyspell-mode-idle)
  (add-hook 'telega-chat-mode-hook #'tychoish--flyspell-mode-idle)
  :commands (flyspell-mode flyspell-prog-mode flyspell-correct-wrapper)
  :config
  (delight 'flyspell-mode " fs")
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

(use-package flyspell-correct
  :ensure t
  :after flyspell
  :commands (flyspell-correct-at-point
             flyspell-correct-next
             flyspell-correct-previous
             flyspell-correct-wrapper))

(use-package whitespace
  :ensure nil
  :defer t
  :commands (toggle-local-whitespace-cleanup)
  :init
  (defun bootstrap-set-up-show-whitespace ()
    (setq-local show-trailing-whitespace t))

  (keymap-set global-map "C-c C-w" #'whitespace-cleanup)
  (keymap-set hud-core-map "s" #'whitespace-cleanup)
  (keymap-set hud-core-map "w" #'toggle-local-whitespace-cleanup)

  (add-hook 'prog-mode-hook #'bootstrap-set-up-show-whitespace)
  (add-hook 'text-mode-hook #'bootstrap-set-up-show-whitespace)
  :config
  (defun toggle-local-whitespace-cleanup ()
    "Reset the before-save hook to preven cleaning up."
    (interactive)
    (if (setq-local show-trailing-whitespace (not show-trailing-whitespace))
        (progn
          (add-hook 'before-save-hook 'whitespace-cleanup nil t)
          (message "turned on whitespace-cleanup for '%s'" (buffer-file-name (current-buffer))))
      (remove-hook 'before-save-hook 'whitespace-cleanup)
      (message "turned off whitespace-cleanup for '%s'" (buffer-file-name (current-buffer)))))

  (setq whitespace-style
        '(face
          trailing
          tabs
          spaces
          lines
          newline
          missing-newline-at-eof
          empty
          space-mark
          tab-mark
          newline-mark)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; programming major-modes

(use-package make-mode
  :ensure nil
  :defer t
  :mode (("[Mm]akefile\\'" . makefile-mode)
         ("\\.mk\\'" . makefile-mode))
  :config
  (setq makefile-electric-keys t))

(use-package conf-mode
  :ensure nil
  :defer t
  :mode (("\\.service\\'" . conf-unix-mode)
         ("\\.timer\\'" . conf-unix-mode)
         ("\\.target\\'" . conf-unix-mode)
         ("\\.mount\\'" . conf-unix-mode)
         ("\\.automount\\'" . conf-unix-mode)
         ("\\.slice\\'" . conf-unix-mode)
         ("\\.socket\\'" . conf-unix-mode)
         ("\\.path\\'" . conf-unix-mode)
         ("\\.conf\\'" . conf-unix-mode)))

(use-package yaml-mode
  :ensure t
  :mode (("\\.yaml$" . yaml-mode)
	 ("\\.yml$" . yaml-mode))
  :init
  ;; Emacs's own built-in `\.yaml\'' → `yaml-ts-mode' entry in
  ;; `auto-mode-alist' sits ahead of the :mode entries above, so it wins
  ;; the match regardless of load order.  Remapping `yaml-ts-mode' to
  ;; `yaml-mode' here intercepts at mode-selection time instead of
  ;; `auto-mode-alist' match time, so `yaml-mode' wins unconditionally.
  (add-to-list 'major-mode-remap-alist '(yaml-ts-mode . yaml-mode))
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
  (add-hook 'yaml-mode-hook (hud-set-tab-width 2)))

(use-package yaml-pro
  :ensure t
  :commands (yaml-pro-mode)
  :init
  (add-hook 'yaml-mode-hook #'yaml-pro-mode))

(use-package go-ts-mode
  :ensure nil
  :mode (("\\.go$" . go-ts-mode)
	 ("go.work" . go-mod-ts-mode)
	 ("go.mod" . go-mod-ts-mode))
  :init
  (cl-defmethod project-root ((project (head go-module))) (cdr project))

  (defun project-find-go-module (dir)
    (when-let* ((root (or (locate-dominating-file dir "go.work")
                          (locate-dominating-file dir "go.mod"))))
      (cons 'go-module root)))

  (add-hook 'project-find-functions #'project-find-go-module)

  (add-to-list 'major-mode-remap-alist '((go-mode . go-ts-mode)))
  (add-to-list 'major-mode-remap-alist '((go-mod-mode . go-mod-ts-mode)))
  (add-to-list 'tychoish/eglot-default-server-configuration
	       '(:gopls :gofumpt t
			:usePlaceholders :json-false
			:hoverKind "FullDocumentation"
			:linkTarget "pkg.go.dev"
			:analyses (:unreachable t
						:unusedvariable t)
			:codelenses (:test t
					   :tidy t
					   :upgradeDependency t
					   :generate t)
			:hints (:parameterNames :json-false
						:ignoredError t
						:constantValues t
						:compositeLiteralTypes :json-false
						:compositeLiteralFields :json-false
						:rangeVariableTypes :json-false
						:functionTypeParameters :json-false)))
  :config
  (delight 'go-ts-mode "go.ts")
  (delight 'go-mod-ts-mode "go.mod.ts")
  (delight 'go-mode "go")
  (defun tychoish/go-mode-setup ()
    (setq-local tab-width 8)
    (setq-local fill-column 100)
    (setq-local compilation-error-screen-columns nil)
    (setq-local flycheck-disabled-checkers '(go-unconvert go-errcheck go-staticcheck go-vet go-build go-test go-gofmt golangci-lint)))

  (defun tychoish/go-mode-setup-for-buffer (buf)
    (with-current-buffer buf
      (tychoish/go-mode-setup)))

  (defun go-mode-set-up-all-buffers ()
    (interactive)
    (thread-last (buffer-list)
		 (seq-filter (lambda (buf) (with-current-buffer buf
					     (or (derived-mode-p 'go-ts-mode)
						 (derived-mode-p 'go-mode)))))
		 (mapc #'tychoish/go-mode-setup-for-buffer)))

  (add-hook 'go-ts-mode-hook 'tychoish/go-mode-setup)
  (add-hook 'go-mode-hook 'tychoish/go-mode-setup)

  (let ((current-path (getenv "PATH"))
	(gopath (getenv "GOPATH")))

    (unless gopath
      (setq gopath (setenv "GOPATH" (expand-file-name "~/go"))))

    (setq local-go-bin (expand-file-name (concat gopath "/bin")))
    (add-to-list 'exec-path local-go-bin)

    (unless (string-search local-go-bin current-path)
      (setenv "PATH" (format "%s:%s" current-path local-go-bin)))))

(use-package rustic
  :ensure t
  :after (rust-mode flycheck)
  :mode (("\\.rs\\'" . rustic-mode)
	 ("Cargo.lock" . toml-ts-mode))
  :commands (rust-resolve-fmt-path rustic-mode)
  :config
  (delight 'rust-mode "rs")
  (delight 'rustic-mode "rs(x)")
  (defun rustic-mode-auto-save-hook ()
    "Enable auto-saving in rustic-mode buffers."
    (when buffer-file-name
      (setq-local compilation-ask-about-save nil)))
  (add-hook 'rustic-mode-hook 'rustic-mode-auto-save-hook)

  (setq rust-mode-treesitter-derive t)
  (setq rustic-lsp-client 'eglot)
  (setq rustic-format-trigger 'on-save)
  (setq rustic-cargo-clippy-trigger-fix 'on-compile)
  (setq rustic-use-rust-save-some-buffers t)
  (setq rustic-clippy-arguments "--all --all-features -- --deny warnings")

  (let* ((rustup-path (executable-find "rustup"))
	 (rustup-p (and rustup-path (not (string-empty-p rustup-path))))
	 (rustup-toolchain (if rustup-p "nightly" "stable")))
    (when rustup-p
      (setq rustic-rustfmt-args "+nightly"))

    (add-to-list 'tychoish/eglot-default-server-configuration
		 `((:rust-analyzer :initializationOptions
				   (:server (:extraEnv (:RUSTUP_TOOLCHAIN ,rustup-toolchain))
					    :rust (:analyzerTargetDir t)
					    :cargo (:buildScripts (:enable t :features "all"))
					    :procMacro (:enable :json-false :attributes (:enable t))
					    :check (:workspace t))))))

  (add-to-list 'flycheck-checkers 'rustic-clippy))

(use-package python
  :ensure nil
  :mode (("\\.py$" . python-ts-mode))
  :init
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
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

  :config
  (delight 'python-ts-mode "py.ts")
  (delight 'python-mode "py")
  (defun tychoish/python-setup ()
    (setq-local python-indent-offset 4)
    (setq-local tab-width 4)
    (setq-local fill-column 100))
  (add-hook 'python-ts-mode-hook 'tychoish/python-setup)

  (declare-function python-indent-shift-left "python")
  (declare-function python-indent-shift-right "python")

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

(use-package terraform-mode
  :ensure t
  :mode ("\\.tf" "\\.tfvars" "\\.tfvars.example")
  :config
  (delight 'terraform-mode "tf")
  (setq terraform-format-on-save t)
  (setq terraform-indent-level 2))

(use-package nxml-mode
  :mode (("\\.xml$'". nxml-mode)))

(use-package rst
  :mode ("\\.rst\\'" . rst-mode)
  :config
  (keymap-set rst-mode-map "C-c C-t h" #'rst-adjust)
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
    (local-unset-key (kbd "C-c C-s")))

  (add-hook 'rst-mode-hook 'tychoish/set-up-rst-mode))

(use-package tex-mode
  :mode ("\\.tex\\'" . LaTeX-mode)
  :init
  (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook #'visual-line-mode)
  (add-hook 'LaTeX-mode-hook #'turn-off-auto-fill)
  :config
  (setq tex-dvi-view-command "(f=*; pdflatex \"${f%.dvi}.tex\" && open \"${f%.dvi}.pdf\")"))

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

(use-package ninja-mode
  :ensure t
  :mode "\\.ninja\\'")

(use-package slime
  :mode ("\\.lisp" . lisp-mode)
  :commands (slime slime-connect hyperspec-lookup)
  :init
  (keymap-set hud-docs-map "c" #'hyperspec-lookup)
  :config
  (delight 'lisp-mode "lisp")
  (delight 'slime-mode "sl")
  (delight 'slime-autodoc-mode "")
  (make-read-extended-command-for-prefix "slime"
    :key-alias "slime-commands"
    :bind-map hud-ide-map
    :bind-key "s")
  (setq ls-lisp-dirs-first t)
  (setq inferior-lisp-program "sbcl"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; programming adjacent tools

(use-package journalctl-mode
  :ensure t
  :commands (journalctl)
  :init
  (keymap-set hud-core-map "j" #'journalctl))

(use-package docker
  :ensure t
  :commands (docker docker-containers docker-images docker-volumes docker-contexts docker-compose)
  :init
  (keymap-set hud-docker-map "d" #'docker)
  (keymap-set hud-docker-map "c" #'docker-containers)
  (keymap-set hud-docker-map "i" #'docker-images)
  (keymap-set hud-docker-map "v" #'docker-volumes)
  (keymap-set hud-docker-map "m" #'docker-contexts)
  (keymap-set hud-docker-map "p" #'docker-compose)
  :config
  (setq docker-terminal-backend 'eat)
  (make-read-extended-command-for-prefix "docker"
    :bind-key "x"
    :bind-map hud-docker-map)
  (transient-insert-suffix 'docker '(-1 0) '("m" "emacs docker commands" execute-extended-docker-command)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; build/compilation support

(use-package flycheck
  :ensure t
  :defines (flycheck-checkers)
  :commands (flycheck-disable-checker flycheck-mode global-flycheck-mode)
  :init
  (keymap-set global-map "C-c f f" #'flycheck-mode)
  :config
  (delight 'flycheck-mode " fc")
  (defun tychoish/flycheck-prefer-eldoc ()
    (add-hook 'eldoc-documentation-functions #'flycheck-eldoc nil t) ;; local
    (setq-local eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
    (setq-local flycheck-display-errors-function nil)
    (setq-local flycheck-help-echo nil))
  (add-hook 'flycheck-mode-hook 'tychoish/flycheck-prefer-eldoc)

  (setq-default flycheck-disable-checker '(go-unconvert go-errcheck go-staticcheck go-vet go-build go-test go-gofmt))
  (setq flycheck-keymap-prefix (kbd "C-c f"))
  ;; the order of the following 3 operations is important.
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map)

  (bind-key "m" #'consult-flycheck flycheck-command-map)

  (defun flycheck-eldoc (callback &rest _ignored)
    "Print flycheck messages at point by calling CALLBACK." ;; from masteringemacs.org
    (when-let* ((flycheck-errors (and flycheck-mode (flycheck-overlay-errors-at (point)))))
      (mapc
       (lambda (err)
	 (funcall callback
		  (format "%s: %s"
			  (let ((level (flycheck-error-level err)))
			    (cond
			     ((eq level 'info) (propertize "I" 'face 'flycheck-error-list-info))
			     ((eq level 'error) (propertize "E" 'face 'flycheck-error-list-error))
			     ((eq level 'warning) (propertize "W" 'face 'flycheck-error-list-warning))
			     (t level)))
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
  :commands (tychoish-compile)
  :init
  (keymap-set hud-core-map "c" #'tychoish-compile)
  :config
  (keymap-set compilation-mode-map "C" #'compile)
  (add-to-list 'compilation-error-regexp-alist 'go-test)
  (add-to-list 'compilation-error-regexp-alist 'go-panic)

  (setq compilation-error-regexp-alist-alist ; first remove the standard conf; it's not good.
        (remove 'go-panic (remove 'go-test compilation-error-regexp-alist-alist)))

  (add-to-list 'compilation-error-regexp-alist-alist
               ;; '(go-test . ("^\\s-+\\k([^()\t\n]+\\):\\([0-9]+\\):? .*$" 1 2)) t) ;; the standard, it works (ish)
               '(go-test . ("^[[:space:]]*\\([_a-zA-Z./][_a-zA-Z0-9./]*\\):\\([0-9]+\\):" 1 2)))
  (add-to-list 'compilation-error-regexp-alist-alist
               '(go-panic . ("^[[:space:]]*\\([_a-zA-Z./][_a-zA-Z0-9./]*\\):\\([0-9]+\\):" 1 2)))

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
    (builder-compile-project))

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
	   (optstr (format "-e %s" (string-join options " -e ")))
	   (command-string (format "docker run %s -v %s:/tmp/lint github/super-linter" optstr project-directory)))
      (builder-compile-project "super-lint" command-string)))

  (with-eval-after-load 'rust-mode
    (require 'rust-compile))

  (defun tychoish-compilation-read-command (command)
    (let* ((results (builder--read-command command))
	   (name (car results))
	   (candidates (cdr results))
	   (command (builder-candidate-command (map-elt candidates name))))
      (read-from-minibuffer "edit command => " command)))

  (add-hook 'compilation-finish-functions #'alert-after-finish-in-background)

  (defun alert-after-finish-in-background (buf str)
    (when (or (not (get-buffer-window buf 'visible)) (not (frame-focus-state)))
      (alert str :buffer buf)))

  (setq compilation-max-output-line-length nil)
  (setq compilation-scroll-output t)

  (setq-default compilation-save-buffers-predicate #'approximate-project-root)
  (compile-add-error-syntax 'rust-pretty-logfile "^\s+ at \\(.*\\):\\([0-9]+\\)" 1 2)
  (compile-add-error-syntax 'go-test "^\\(.+?\\.go\\):\\([0-9]+\\):" '(tychoish/go-test-filename-from-package (match-string 1)) 2)
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)
  (advice-add 'compilation-read-command :override 'tychoish-compilation-read-command))

(use-package flycheck-aspell
  :ensure t
  :defer t
  :after (flycheck
	  flyspell
	  (:any org-mode rst-mode markdown-mode message-mode sgml-mode html-mode html-ts-mode))
  :config
  (eval-when-compile
    (require 'xtd-dash))
  (mapc (make-add-to-list-fn flycheck-checkers)
	'(c-aspell-dynamic org-aspell-dynamic rst-aspell-dynamic html-aspell-dynamic markdown-aspell-dynamic mail-aspell-dynamic))

  (flycheck-aspell-define-checker "org" "Org" ("--add-filter" "url") (org-mode))
  (flycheck-aspell-define-checker "rst" "reStructuredText" ("--add-filter" "url") (rst-mode)))

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

(use-package warnings
  :ensure nil
  :defer t
  :init
  (put 'downcase-region 'disabled nil)
  (put 'narrow-to-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'list-timers 'disabled nil)
  (put 'list-threads 'disabled nil)

  (setq byte-compile-warnings
        ;; OMIT: free-vars docstrings-wide
        '(callargs
          constants
          docstrings
          docstrings-non-ascii-quotes
          docstrings-control-chars
          empty-body
          ignored-return-value
          interactive-only
          lexical
          lexical-dynamic
          make-local
          mutate-constant
          noruntime
          not-unused
          obsolete
          redefine
          suspicious
          unresolved))
  :config
  (add-to-list 'warning-suppress-log-types '(frameset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; language server protocol (lsp) [eglot] + treesitter

(use-package eglot
  :ensure nil
  ;; :defines (eglot-mode-map eglot-alternatives)
  ;; :functions (eglot-format-buffer eglot-managed-p eglot-completion-at-point eglot-alternatives)
  :commands (eglot-code-action-rewrite
	     eglot-code-action-extract
	     eglot-code-actions
	     eglot-format
	     eglot-rename
	     eglot-code-action-organize-imports
	     eglot-code-action-inline
	     eglot-update-workspace
	     execute-extended-eglot-command)
  :init
  (add-hook 'js-mode-hook #'eglot-ensure)
  (add-hook 'js-ts-mode-hook #'eglot-ensure)
  (add-hook 'typescript-ts-mode-hook #'eglot-ensure)
  (add-hook 'go-ts-mode-hook #'eglot-ensure)
  (add-hook 'go-mod-ts-mode-hook #'eglot-ensure)
  (add-hook 'yaml-mode-hook #'eglot-ensure)
  (add-hook 'rustic-mode-hook #'eglot-ensure)
  (add-hook 'c++-ts-mode-hook #'eglot-ensure)
  (add-hook 'c-ts-mode-hook #'eglot-ensure)
  (add-hook 'c-or-c++-mode-hook #'eglot-ensure)
  (add-hook 'python-mode-hook #'eglot-ensure)
  (add-hook 'python-ts-mode-hook #'eglot-ensure)
  (add-hook 'go-mode-hook #'eglot-ensure)
  (add-hook 'go-mod-mode-hook #'eglot-ensure)
  (add-hook 'bash-ts-mode-hook #'eglot-ensure)
  (add-hook 'bash-mode-hook #'eglot-ensure)
  (add-hook 'sh-mode-hook #'eglot-ensure)
  ;; "C-c l l"
  (keymap-set hud-eglot-global-map "s" #'eglot)
  (keymap-set hud-eglot-global-map "r" #'eglot-reconnect) ;; TODO this should only be on when minor mode
  (keymap-set hud-eglot-global-map "k" #'eglot-shutdown)
  (keymap-set hud-eglot-global-map "l" #'eglot-list-connections)
  (keymap-set hud-eglot-global-map "c" #'execute-extended-eglot-command)
  (keymap-set hud-eglot-global-map "g" #'eglot-forget-pending-continuations)
  :config
  (defun tychoish/eglot-ensure-hook ()
    ;; toggle it on and off so that the left-fringe isn't weird.
    (flycheck-eglot-mode -1)
    (flycheck-eglot-mode 1))

  (add-hook 'eglot-managed-mode-hook 'tychoish/eglot-ensure-hook)

  (autoload 'eglot-test-at-point "eglot-test-at-point")

  (defvar-keymap tychoish/eglot-map) ;; "C-c l"
  (keymap-set eglot-mode-map "C-c l" '(tychoish/eglot-map . "eglot"))
  (keymap-set tychoish/eglot-map "s" #'consult-eglot-symbols)
  (keymap-set tychoish/eglot-map "r" #'eglot-rename)
  (keymap-set tychoish/eglot-map "a" #'eglot-code-actions)
  (keymap-set tychoish/eglot-map "i" #'eglot-code-action-inline)
  (keymap-set tychoish/eglot-map "f" #'eglot-format-buffer)
  (keymap-set tychoish/eglot-map "o" #'eglot-code-action-organize-imports)
  (keymap-set tychoish/eglot-map "q" #'eglot-code-action-quickfix)
  (keymap-set tychoish/eglot-map "w" #'eglot-code-action-rewrite)
  (keymap-set tychoish/eglot-map "t" #'eglot-test-at-point)
  (keymap-set tychoish/eglot-map "u" #'eglot-update-workspace)

  (make-read-extended-command-for-prefix "eglot"
    :bind-map tychoish/eglot-map
    :bind-key "e")

  (make-read-extended-command-for-prefix "xref"
    :bind-map hud-ide-map
    :bind-key "x")

  (setq eglot-menu-string "eg")

  (setq eglot-autoshutdown t)
  (setq eglot-extend-to-xref t)
  (setq eglot-report-progress nil)
  (add-to-list 'eglot-stay-out-of 'flymake)
  (add-to-list 'eglot-stay-out-of 'company)
  (set-face-attribute 'eglot-highlight-symbol-face nil :underline nil :weight 'bold)

  ;; Advertising `workspace/didChangeWatchedFiles' makes most language
  ;; servers send a `client/registerCapability' request as part of session
  ;; startup. Its handler (`eglot-register-capability') calls `project-files'
  ;; on the server's project, and when `projectile-mode' resolves that to
  ;; Projectile's backend with a cold file cache, Projectile's async indexer
  ;; waits on `accept-process-output' from *inside this very jsonrpc process
  ;; filter* — a reentrant context that can stall for minutes until manually
  ;; interrupted. Two complementary fixes below: warm the cache before eglot
  ;; ever connects, and defer file-watch registration instead of blocking the
  ;; process filter when the cache is still cold.

  (defconst tychoish/projectile-warm-cache-idle-delay 3
    "Idle seconds between warming successive queued projects' Projectile caches.")

  (defvar tychoish/projectile-warm-cache-queue nil
    "Project roots waiting to be indexed by `tychoish/projectile-warm-cache-process-queue'.")

  (defvar tychoish/projectile-warm-cache-idle-timer nil
    "Idle timer that pops one root off `tychoish/projectile-warm-cache-queue' at a time.")

  (defun tychoish/projectile-warm-cache-process-queue ()
    "Index the next queued project, or cancel the timer once the queue is empty.
Runs on `tychoish/projectile-warm-cache-idle-delay' idle windows, one
project per firing, so a desktop restore with many cached projects
doesn't launch an indexing process per project all at once."
    (if-let* ((root (pop tychoish/projectile-warm-cache-queue)))
        (unless (tychoish/projectile-cache-warm-p root)
          (projectile-index-project-async root))
      (when tychoish/projectile-warm-cache-idle-timer
        (cancel-timer tychoish/projectile-warm-cache-idle-timer)
        (setq tychoish/projectile-warm-cache-idle-timer nil))))

  (declare-function tychoish/projectile-warm-cache-process-queue "tychoish-core")
  (declare-function tychoish/projectile-cache-warm-p "tychoish-core")

  (defun tychoish/projectile-warm-cache-enqueue (root)
    "Queue ROOT for background Projectile indexing on the next idle window."
    (unless (or (member root tychoish/projectile-warm-cache-queue)
                (tychoish/projectile-cache-warm-p root))
      (setq tychoish/projectile-warm-cache-queue
            (append tychoish/projectile-warm-cache-queue (list root))))
    (unless tychoish/projectile-warm-cache-idle-timer
      (setq tychoish/projectile-warm-cache-idle-timer
            (run-with-idle-timer tychoish/projectile-warm-cache-idle-delay t
                                  #'tychoish/projectile-warm-cache-process-queue))))

  (defun tychoish/projectile-warm-cache-for-buffer ()
    "Queue background Projectile indexing for the current buffer's project.
Runs on the first visit to a project in a session, well before
`eglot-ensure' would otherwise force a synchronous, cold index. Queued
rather than dispatched immediately so a desktop restore with many
cached projects doesn't index all of them at once."
    (when-let* ((root (and (bound-and-true-p projectile-mode)
                            (projectile-project-root))))
      (tychoish/projectile-warm-cache-enqueue root)))

  (defun tychoish/projectile-warm-cache-on-project-change (_new-root _previous-root)
    (tychoish/projectile-warm-cache-for-buffer))

  (declare-function tychoish/projectile-warm-cache-on-project-change "tychoish-core")
  (add-hook 'projectile-project-changed-functions
            #'tychoish/projectile-warm-cache-on-project-change)

  (defun tychoish/projectile-cache-warm-p (root)
    "Return non-nil if Projectile's in-memory file cache for ROOT is populated."
    (and (bound-and-true-p projectile-enable-caching)
         (map-elt projectile-projects-cache root)))

  (defun tychoish/eglot-defer-file-watch-registration (server id watchers root)
    "Register SERVER's file watches for ID/WATCHERS once ROOT's cache is warm.
Polls at most two minutes before giving up and registering anyway from
inside the timer callback — still safer than the original jsonrpc
process-filter context, since a timer callback isn't nested inside the
process filter it might end up pumping."
    (projectile-index-project-async root)
    (let ((tries 0) timer)
      (setq timer
            (run-with-timer
             1 1
             (lambda ()
               (cl-incf tries)
               (when (or (tychoish/projectile-cache-warm-p root)
                         (>= tries 120))
                 (cancel-timer timer)
                 (when (process-live-p (jsonrpc--process server))
                   (eglot-register-capability
                    server 'workspace/didChangeWatchedFiles id :watchers watchers))))))))

  ;; `cl-defmethod' forms defeat the byte-compiler's forward-reference
  ;; tracking for sibling `defun's in this same `eval-after-load' block, so
  ;; declare them explicitly even though they're defined a few lines above.
  (declare-function tychoish/projectile-warm-cache-for-buffer "tychoish-core")
  (declare-function tychoish/projectile-cache-warm-p "tychoish-core")
  (declare-function tychoish/eglot-defer-file-watch-registration "tychoish-core")

  (cl-defmethod eglot-register-capability :around
    (server (_method (eql workspace/didChangeWatchedFiles)) id &key watchers)
    (let* ((project (eglot--project server))
           (root (and (eq (car-safe project) 'projectile) (cdr project))))
      (if (and root (not (tychoish/projectile-cache-warm-p root)))
          (tychoish/eglot-defer-file-watch-registration server id watchers root)
        (cl-call-next-method))))

  (defun tychoish/eglot-before-save-hook ()
    (add-hook 'before-save-hook #'eglot-format-for-hook nil t)
    (add-hook 'before-save-hook #'eglot-code-action-organize-imports nil t))

  (add-hook 'eglot-managed-mode-hook #'tychoish/eglot-before-save-hook)

  (defun tychoish/eglot-prune-dead-servers ()
    "Remove non-live server structs from `eglot--servers-by-project'.
`eglot--on-shutdown' can abort partway through its cleanup (for
instance on a `track-changes' assertion failure), leaving a dead
`eglot-lsp-server' stuck in `eglot--servers-by-project' forever."
    (map-do
     (lambda (project servers)
       (let ((live (seq-filter (lambda (s) (process-live-p (jsonrpc--process s))) servers)))
	 (unless (eq (length live) (length servers))
	   (setf (map-elt eglot--servers-by-project project) live))))
     eglot--servers-by-project))

  (defun tychoish/eglot-reconnect-orphaned-buffers ()
    "Reattach buffers whose cached Eglot server is no longer live.
A buffer left pointing at a dead server otherwise errors on every
eldoc/xref request until manually reconnected."
    (seq-do
     (lambda (buffer)
       (with-current-buffer buffer
	 (when (and (bound-and-true-p eglot--managed-mode)
		    eglot--cached-server
		    (not (process-live-p (jsonrpc--process eglot--cached-server))))
	   (setq eglot--cached-server nil)
	   (ignore-errors (eglot--managed-mode-off))
	   (when buffer-file-name
	     (ignore-errors (eglot-ensure))))))
     (buffer-list)))

  (defun tychoish/eglot-cleanup-stale-connections ()
    "Prune dead Eglot servers and reconnect any buffers orphaned by them."
    (interactive)
    (tychoish/eglot-prune-dead-servers)
    (tychoish/eglot-reconnect-orphaned-buffers))

  (defun ad:eglot--on-shutdown-cleanup-stale (orig-fn server)
    "Run stale-connection cleanup even if ORIG-FN's teardown aborts partway.
`eglot--on-shutdown' can hit a `track-changes' assertion failure
mid-cleanup, which otherwise leaves the dead SERVER stuck in
`eglot--servers-by-project' and orphans its buffers."
    (ignore-errors (funcall orig-fn server))
    (tychoish/eglot-cleanup-stale-connections))

  (advice-add 'eglot--on-shutdown :around #'ad:eglot--on-shutdown-cleanup-stale)

  (defvar tychoish/eglot-cleanup-stale-connections-timer nil
    "Idle timer running `tychoish/eglot-cleanup-stale-connections'.")

  (when (timerp tychoish/eglot-cleanup-stale-connections-timer)
    (cancel-timer tychoish/eglot-cleanup-stale-connections-timer))

  (setq tychoish/eglot-cleanup-stale-connections-timer
	(run-with-idle-timer 300 t #'tychoish/eglot-cleanup-stale-connections))

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

  (defun eglot-update-workspace ()
    "Push updated workspace configuration to the current eglot server.
Useful after changing `eglot-workspace-configuration' or
`tychoish/eglot-default-server-configuration' without restarting eglot."
    (interactive)
    (eglot-signal-didChangeConfiguration (eglot--current-server-or-lose)))

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
  :commands (flycheck-eglot-mode)
  :init
  (add-hook 'eglot-managed-mode-hook #'flycheck-eglot-mode)
  :config
  (setq-default flycheck-eglot-exclusive nil)
  (add-to-list 'flycheck-checkers 'eglot-check)
  (setq flycheck-eglot-enable-diagnostic-tags nil)
  (flycheck-add-next-checker 'eglot-check 'go-gofmt))

(use-package cmake-ts-mode
  :defer t
  :init
  (cl-defmethod project-root ((project (head cmake-root))) (cdr project))

  (defun project-find-cmake-project (dir)
    (when-let* ((root (locate-dominating-file dir "CMakeLists.txt")))
      (cons 'cmake-root root)))

  (add-hook 'project-find-functions #'project-find-cmake-project))

(use-package treesit
  :defer t
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
  (add-hook 'js-ts-mode-hook (make-run-hooks-function-for js-mode))
  (add-hook 'c-ts-mode-hook (make-run-hooks-function-for c-mode))
  (add-hook 'c++-ts-mode-hook (make-run-hooks-function-for c++-mode))

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
	(mapc #'treesit-install-language-grammar (seq-map #'car treesit-language-source-alist)))
     (lambda (result)
       (message "rebuilt treesit grammars for %s" result)))))

(use-package dape
  :ensure t
  :defer t
  :init
  (keymap-set hud-mode-map "C-c C-d" '(dape-global-map . "dape-map"))
  :config
  (add-hook 'kill-emacs-hook 'dape-bakepoint-save)
  (add-hook 'dape-start-hook #'save-all-buffers)
  ;; (setq dape-info-hide-mode-line nil)
  ;;
  ;; Pulse source line (performance hit)
  ;; (add-hook 'dape-display-source-hook #'pulse-momentary-highlight-one-line)
  (setq dape-key-prefix (kbd "C-c C-d"))
  (setq dape-buffer-window-arrangement 'right)
  (setq dape-cwd-function #'approximate-project-root))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ROBOTS (AI) Integration
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package gptel
  :functions (gptel-make-anthropic gptel-make-gh-copilot gptel-make-gemini)
  :commands (gptel gptel-rewrite gptel-menu)
  :init
  (keymap-set hud-robot-gptel-map "g" #'gptel)
  (keymap-set hud-robot-gptel-map "r" #'gptel-rewrite)
  (keymap-set hud-robot-gptel-map "m" #'gptel-menu)

  (make-read-extended-command-for-prefix "gptel"
    :bind-map hud-robot-gptel-map
    :bind-key "x")

  (make-read-extended-command-for-prefix "gptel-set-backend"
    :bind-map hud-robot-gptel-map
    :bind-key "b")
  :config
  (defvar gemini-api-key nil)
  (defvar anthropic-api-key nil)
  (defvar openai-api-key nil)

  (cl-defmacro make-gptel-set-up-backend-functions (&key name model backend key api-key)
    (let ((local-function-symbol (intern (format "gptel-set-backend-%s" name)))
          (default-function-symbol (intern (format "gptel-set-backend-default-%s" name))))
      `(progn
	 (defun ,local-function-symbol ()
           ,(format "Set LLM backend for the current buffer to `%s'" model)
           (interactive)
           (setq-local gptel-model ,model)
           ,(when api-key
              `(setq-local gptel-api-key (lambda () ,api-key)))
           (setq-local gptel-backend ,backend)
           (message "[gptel] set backend to %s for the local buffer" ,name))

	 (defun ,default-function-symbol ()
           ,(format "Set the default LLM backend for the current session to `%s'" model)
           (interactive)
           (setq-default gptel-model ,model)
           ,(when api-key
              `(setq-default gptel-api-key (lambda () ,api-key)))
           (setq-default gptel-backend ,backend)
           (message "[gptel] set default backend to %s" ,name))

	 (keymap-set gptel-mode-map ,(format "C-c r a m %s" (upcase key)) #',default-function-symbol)
	 (keymap-set gptel-mode-map ,(format "C-c r a m %s" (downcase key)) #',local-function-symbol)
	 (keymap-set hud-robot-gptel-set-default-model-map ,(downcase key) #',default-function-symbol))))

  (keymap-set gptel-mode-map "C-c m" #'gptel-menu)

  (setq gptel-include-reasoning 'ignore)

  (make-gptel-set-up-backend-functions
   :name "claude-opus-4-6"
   :key "o"
   :backend (gptel-make-anthropic "claude-opus" :key anthropic-api-key :stream t)
   :model 'claude-opus-4-6
   :api-key anthropic-api-key)

  (make-gptel-set-up-backend-functions
   :name "claude-sonnet-4-6"
   :key "s"
   :backend (gptel-make-anthropic "claude-sonnet" :key anthropic-api-key :stream t)
   :model 'claude-sonnet-4-6
   :api-key anthropic-api-key)

  (make-gptel-set-up-backend-functions
   :name "claude-haiku-4-5"
   :key "h"
   :backend (gptel-make-anthropic "claude" :key anthropic-api-key :stream t)
   :model 'claude-haiku-4-5
   :api-key anthropic-api-key)

  (make-gptel-set-up-backend-functions
   :name "gemini-pro-latest"
   :key "g"
   :backend (gptel-make-gemini "gemini" :key gemini-api-key :stream t)
   :model 'gemini-flash-lite-latest)

  (make-gptel-set-up-backend-functions
   :name "gemini-flash"
   :key "f"
   :backend (gptel-make-gemini "gemini" :key gemini-api-key :stream t)
   :model 'gemini-flash-lite-latest)

  (make-gptel-set-up-backend-functions
   :name "gemini-flash-lite"
   :key "l"
   :backend (gptel-make-gemini "gemini" :key gemini-api-key :stream t)
   :model 'gemini-flash-lite-latest)

  (make-gptel-set-up-backend-functions
   :name "copilot"
   :key "c"
   :backend (gptel-make-gh-copilot "copilot")
   :model 'claude-3.5-sonnet)

  (make-gptel-set-up-backend-functions
   :name "gpt-5"
   :key "5"
   :backend (gptel-make-openai "openai" :key openai-api-key)
   :model 'gpt-5
   :api-key openai-api-key)

  (make-gptel-set-up-backend-functions
   :name "gpt-5-mini"
   :key "m"
   :backend (gptel-make-openai "openai" :key openai-api-key)
   :model 'gpt-5-mini
   :api-key openai-api-key)

  (make-gptel-set-up-backend-functions
   :name "gpt-5-nano"
   :key "n"
   :backend (gptel-make-openai "openai" :key openai-api-key)
   :model 'gpt-5-nano
   :api-key openai-api-key)

  (make-gptel-set-up-backend-functions
   :name "gpt-4o"
   :key "4"
   :backend (gptel-make-openai "openai" :key openai-api-key)
   :model 'gpt-4o)

  (make-gptel-set-up-backend-functions
   :name "gpt-4o-mini"
   :key "M"
   :backend (gptel-make-openai "openai" :key openai-api-key)
   :model 'gpt-4o-mini)

  (gptel-set-backend-default-claude-sonnet-4-6)
  (require 'gptel-integrations))

(use-package gptel-aibo
  :commands (gptel-aibo-summon gptel-aibo)
  :init
  (keymap-set hud-robot-gptel-map "w" #'gptel-aibo-summon))

(use-package gptel-agent
  :after (gptel)
  :commands (gptel-agent)
  :init
  (keymap-set hud-robot-gptel-map "a" #'gptel-agent))

(use-package eat
  :ensure t
  :commands (eat eat-project eat-other-window eat-project-other-window)
  :init
  ;; "C-c s e"
  (keymap-set hud-shell-eat-map "e" #'eat)
  (keymap-set hud-shell-eat-map "o" #'eat-other-window)
  (keymap-set hud-shell-eat-map "p" #'eat-project)
  (keymap-set hud-shell-eat-map "P" #'eat-project-other-window)

  (make-read-extended-command-for-prefix "eat"
    :bind-map hud-shell-eat-map
    :bind-key "m")
  :config
  (add-hook 'eat-mode-hook 'tychoish/corfu-prog-mode-setup))

(use-package eshell
  :ensure nil
  :defer t
  :init
  (keymap-set hud-shell-map "m" #'eshell)
  :config
  (add-hook 'eshell-mode-hook 'tychoish/corfu-prog-mode-setup)

  (setq eshell-history-file-name (file-name-concat user-emacs-directory sprite--conf-state-directory (sprite-state-file-prefix "eshell")))
  (with-eval-after-load "em-cmpl"
    (add-hook 'eshell-mode 'eshell-cmpl-initialize)))

(use-package comint
  :ensure nil
  :defer t
  :config
  (add-hook 'shell-mode-hook 'tychoish/corfu-prog-mode-setup)
  (keymap-set comint-mode-map "M-n" #'comint-next-input)
  (keymap-set comint-mode-map "M-p" #'comint-previous-input)
  (keymap-set comint-mode-map "<down>" #'comint-next-matching-input-from-input)
  (keymap-set comint-mode-map "<up>" #'comint-previous-matching-input-from-input))

(use-package shell-maker
  :ensure t
  :defer t
  :config
  (defalias 'shell-maker-map 'shell-maker-major-mode-map)
  (setq shell-maker-root-path (sprite-state-path "shell-maker")))

(use-package agent-shell
  :ensure t
  :commands (agent-shell agent-shell-new-shell agent-shell-new-temp-shell agent-shell-new-worktree-shell agent-shell-toggle agent-shell-resolve-permissions)
  :init
  (keymap-set hud-robot-agent-shell-map "o" #'agent-shell)
  (keymap-set hud-robot-agent-shell-map "t" #'agent-shell-toggle)
  (keymap-set hud-robot-agent-shell-map "b" #'agent-shell-switch-buffer)
  (keymap-set hud-robot-agent-shell-map "n" #'agent-shell-new-shell)
  (keymap-set hud-robot-agent-shell-map "e" #'agent-shell-new-temp-shell)
  (keymap-set hud-robot-agent-shell-map "w" #'agent-shell-new-worktree-shell)
  (keymap-set hud-robot-agent-shell-map "v" #'tychoish/agent-shell-toggle-terse-output)
  (make-read-extended-command-for-prefix "agent-shell"
    :bind-map hud-robot-agent-shell-map
    :bind-key "x")
  (with-eval-after-load 'which-key
    (push '((nil . "^agent-shell-") . (nil . "")) which-key-replacement-alist))
  :config
  (delight 'agent-shell-completion-mode nil 'agent-shell-completion)
  (delight 'agent-shell-ui-mode nil 'agent-shell-ui)
  (defconst tychoish/agent-shell-terse-persona
    "Be EXTREMELY concise. No preambles. No conversational filler. Provide direct answers, code, or commands immediately."
    "CLAUDE_PERSONA value that requests terse output from the agent.")

  (defvar tychoish/agent-shell-terse-output t
    "When non-nil, pass `tychoish/agent-shell-terse-persona' to every agent session.")

  (defun tychoish/agent-shell--apply-environment ()
    "Set `agent-shell-anthropic-claude-environment' from current toggle state."
    (setq agent-shell-anthropic-claude-environment
          (agent-shell-make-environment-variables
           "CLAUDE_PERSONA" tychoish/agent-shell-terse-persona
	   "ENABLE_CLAUDEAI_MCP_SERVERS" "false"
           :inherit-env t))

    (setq agent-shell-omp-environment
	  (agent-shell-make-environment-variables
	   "ENABLE_CLAUDEAI_MCP_SERVERS" "false"
	   "OMP_DOCS_DIR" (file-name-concat (or local-notes-directory (expand-file-name "~/notes")) "omp" "projects"))))

  (defun tychoish/agent-shell-toggle-terse-output ()
    "Toggle terse agent output on or off and update the running environment."
    (interactive)
    (setq tychoish/agent-shell-terse-output (not tychoish/agent-shell-terse-output))
    (tychoish/agent-shell--apply-environment)
    (message "Agent terse output: %s"
             (if tychoish/agent-shell-terse-output "on" "off")))

  (defun agent-shell-dot-subdir (subdir)
    "Resolve SUBDIR under the per-instance agent-shell state path."
    (file-name-concat (sprite-state-path "agent-shell") subdir))

  (defun agent-shell-corfu-setup ()
    "Configure corfu auto-completion for agent-shell buffers."
    (corfu-mode +1)
    (setq-local corfu-auto-prefix 2)
    (setq-local completion-at-point-functions
		(cons #'cape-dabbrev (remq t completion-at-point-functions))))

  (defun agent-shell-bold-input-setup ()
    "Render submitted prompt text in bold in agent-shell buffers."
    (face-remap-add-relative 'comint-highlight-input :weight 'bold))

  (setq agent-shell-github-acp-command '("gh" "copilot" "--acp"))
  (setq agent-shell-file-completion-enabled t)
  (setq agent-shell-dot-subdir-function #'agent-shell-dot-subdir)
  (setq agent-shell-header-style 'text)
  (setq agent-shell-thought-process-expand-by-default nil)
  (setq agent-shell-tool-use-expand-by-default nil)
  (setq agent-shell-user-message-expand-by-default nil)
  (setq agent-shell-buffer-name-format
	(lambda (agent-name project-name)
          (let* ((raw (string-trim project-name))
		 (base (file-name-nondirectory (directory-file-name raw)))
		 (stripped (replace-regexp-in-string "\\`[./]+" "" base))
		 (slug (downcase (replace-regexp-in-string "\\s-+" "-"
                                                           (if (string-empty-p stripped) base stripped)))))
            (format "*%s-%s*"
                    (car (split-string (downcase (string-trim agent-name))))
                    slug))))

  (require 'agent-shell-menu)

  (setq agent-shell-anthropic-authentication (agent-shell-anthropic-make-authentication :login t))
  (setq agent-shell-anthropic-default-session-mode-id "auto")
  (setq agent-shell-pi-acp-command '("npx" "-y" "pi-acp"))
  (add-to-list 'agent-shell-agent-configs (agent-shell-omp-make-agent-config))

  (keymap-set agent-shell-mode-map "C-c C-c" #'agent-shell-submit)
  (keymap-set agent-shell-mode-map "C-c C-k" #'agent-shell-interrupt)
  (keymap-set agent-shell-mode-map "C-c C-p" #'agent-shell-resolve-permissions)
  (keymap-set agent-shell-mode-map "C-c C-a" #'agent-shell-menu-select-action)
  (keymap-set agent-shell-mode-map "C-c b" #'agent-shell-switch-buffer)
  (keymap-set agent-shell-mode-map "C-c j" '(hud-robot-agent-shell-map . "robot-agent-shell"))
  (keymap-set agent-shell-mode-map "C-c m" #'agent-shell-menu-select-action)
  (keymap-set agent-shell-mode-map "C-c x" #'agent-shell-menu-select-command)
  (keymap-set agent-shell-mode-map "C-c TAB" #'agent-shell-menu-select-collapse)
  (keymap-set agent-shell-mode-map "C-TAB" #'agent-shell-next-item)
  (keymap-set agent-shell-mode-map "M-SPC" #'agent-shell-menu-select-session-mode)

  (add-hook 'agent-shell-mode-hook #'agent-shell-corfu-setup)
  (add-hook 'agent-shell-mode-hook #'agent-shell-bold-input-setup)

  (defun tychoish--agent-shell-buffer-p (buffer _action)
    "Return non-nil if BUFFER is an agent-shell communication buffer."
    (with-current-buffer buffer
      (derived-mode-p 'agent-shell-mode)))

  (add-to-list 'display-buffer-alist
               '(tychoish--agent-shell-buffer-p
                 (display-buffer-reuse-window
                  display-buffer-use-some-window)
                 (reusable-frames . t)))

  (defun ad:agent-shell--refresh-session-title (orig-fn &optional event)
    (let ((agent-name (map-nested-elt agent-shell--state '(:agent-config :mode-line-name)))
          (title (map-nested-elt agent-shell--state '(:session :title))))
      (unless (and (equal agent-name "Claude")
                   (stringp title)
                   (not (string-empty-p title)))
        (funcall orig-fn event))))

  (advice-add 'agent-shell--refresh-session-title :around
              #'ad:agent-shell--refresh-session-title)

  (tychoish/agent-shell--apply-environment))

(use-package agent-shell-queue
  :after agent-shell
  :defer t
  :commands (agent-shell-queue-buffer-open
             agent-shell-queue-enqueue
             agent-shell-queue-edit-task
             agent-shell-queue-pause
             agent-shell-queue-resume
             agent-shell-queue-capture
             agent-shell-queue-capture-unassigned
             agent-shell-queue-capture-from-region
             agent-shell-queue-capture-from-context
             agent-shell-queue-org-refile-from-heading
             agent-shell-queue-capture-from-clipboard
             agent-shell-queue-insert-pause
             agent-shell-queue-insert-clear-context
             agent-shell-queue-raw-edit
             agent-shell-queue-import
             agent-shell-queue-reload
             agent-shell-queue-set-scope
             agent-shell-queue-scope-global
             agent-shell-queue-export
             agent-shell-queue-enqueue-emacs
             agent-shell-queue-insert-wait
             agent-shell-queue-item-menu
	     agent-shell-menu-project-buffers)
  :config
  (defvar-keymap hud-robot-agent-shell-map)
  (setq agent-shell-queue-write-log-enabled t)
  (require 'agent-shell-menu)
  (keymap-set agent-shell-queue-mode-map "C-c j" '(hud-robot-agent-shell-map . "robot-agent-shell"))
  (keymap-set hud-robot-agent-shell-map "q" #'agent-shell-queue-buffer-open)
  (keymap-set hud-robot-agent-shell-map "/" #'agent-shell-queue-capture)
  (keymap-set hud-robot-agent-shell-map "m" #'agent-shell-menu-dispatch)
  (keymap-unset agent-shell-mode-map "<spc>")

  (agent-shell-menu-mode-key "?" agent-shell-menu-dispatch)
  (agent-shell-menu-mode-key "p" agent-shell-menu-resolve-permission)
  (agent-shell-menu-mode-key "a" agent-shell-menu-select-action)
  (agent-shell-menu-mode-key "b" agent-shell-switch-buffer)
  (agent-shell-menu-mode-key "x" execute-extended-agent-shell-command)
  (agent-shell-menu-mode-key "f" agent-shell-menu-select-collapse)
  (agent-shell-menu-mode-key "c" agent-shell-menu-select-command)
  (agent-shell-menu-mode-key "t" agent-shell-set-session-thought-level)
  (agent-shell-menu-mode-key "m" agent-shell-menu-dispatch)
  (agent-shell-menu-mode-key " " agent-shell-set-session-mode)
  (agent-shell-menu-mode-key "TAB" agent-shell-ui-toggle-fragment)
  (agent-shell-menu-mode-key "q" agent-shell-queue-buffer-open)

  (unbind-key "SPC" 'agent-shell-mode-map)

  (defun agent-shell-queue-capture-corfu-setup ()
    "Configure corfu and dabbrev completion for agent-shell-queue capture/edit buffers."
    (corfu-mode +1)
    (setq-local corfu-auto-prefix 2)
    (setq-local completion-at-point-functions
		(append '(cape-dabbrev agent-shell-queue-capture--slash-command-capf)
			(remq t completion-at-point-functions))))

  (add-hook 'agent-shell-queue-capture-mode-hook #'agent-shell-queue-capture-corfu-setup)
  (add-hook 'agent-shell-queue-edit-mode-hook #'agent-shell-queue-capture-corfu-setup)

  (defun agent-shell-queue-capture--slash-command-capf ()
    "Complete agent slash commands after / in capture buffers with a live target."
    (when-let* ((shell-buf (and (boundp 'agent-shell-queue--capture-target)
				(buffer-live-p agent-shell-queue--capture-target)
				agent-shell-queue--capture-target))
		(commands (with-current-buffer shell-buf
                            (map-elt agent-shell--state :available-commands)))
		((not (seq-empty-p commands))))
      (save-excursion
	(let ((end (point)))
          (when (re-search-backward "/" (line-beginning-position) t)
            (list (1+ (point)) end
                  (seq-map (lambda (c) (map-elt c 'name)) commands)
                  :annotation-function
                  (lambda (name)
                    (let ((cmd (seq-find (lambda (c) (equal (map-elt c 'name) name)) commands)))
                      (concat "  " (or (and cmd (map-elt cmd 'description)) ""))))
                  :exclusive 'no))))))

  (defun tychoish--agent-shell-queue-state-file ()
    "Queue state file under the per-instance agent-shell state directory."
    (let ((ext (cond
                ((eq agent-shell-queue-serialization-format 'json) "json")
                ((eq agent-shell-queue-serialization-format 'yaml) "yaml")
                (t "el"))))
      (expand-file-name (concat "queue." ext)
                        (sprite-state-path "agent-shell"))))

  (defun tychoish--agent-shell-queue-archive-file ()
    "Archive file under the per-instance agent-shell state directory."
    (expand-file-name "queue-archive.jsonl"
                      (sprite-state-path "agent-shell")))

  (add-hook 'agent-shell-queue-capture-mode-hook #'agent-shell-queue-capture-corfu-setup)
  (add-hook 'agent-shell-queue-edit-mode-hook #'agent-shell-queue-capture-corfu-setup)

  (setq agent-shell-queue-state-file-function #'tychoish--agent-shell-queue-state-file)
  (setq agent-shell-queue-archive-file-function #'tychoish--agent-shell-queue-archive-file)
  (setq agent-shell-queue-archive-enabled t)
  (setq agent-shell-queue-safe-save t)
  (setq agent-shell-queue-safe-save-max-files 50)
  (setq agent-shell-queue-pick-buffer-function #'agent-shell-menu--pick-buffer)
  (setq agent-shell-queue-show-ordinal-column nil)

  (with-eval-after-load 'nerd-icons
    (seq-do (lambda (entry)
              (add-to-list 'nerd-icons-mode-icon-alist entry))
            '((agent-shell-queue-mode nerd-icons-codicon "nf-cod-checklist" :face nerd-icons-purple)
              (agent-shell-queue-item-view-mode nerd-icons-codicon "nf-cod-preview" :face nerd-icons-purple)
              (agent-shell-queue-edit-mode nerd-icons-codicon "nf-cod-edit" :face nerd-icons-purple)
              (agent-shell-queue-capture-mode nerd-icons-codicon "nf-cod-record" :face nerd-icons-purple)
              (agent-shell-queue-raw-edit-mode nerd-icons-codicon "nf-cod-file_code" :face nerd-icons-purple)
              (agent-shell-queue-interjection-mode nerd-icons-codicon "nf-cod-comment_discussion" :face nerd-icons-purple)))))

(use-package agent-shell-manager
  :after (agent-shell)
  :commands (agent-shell-manager-toggle agent-shell-manager-find-buffer)
  :init
  (keymap-set hud-robot-agent-shell-map "," #'agent-shell-manager-toggle)
  :config
  (setq agent-shell-manager-side 'bottom)
  (make-read-extended-command-for-prefix "agent-shell-manager"
    :bind-map agent-shell-manager-mode-map
    :bind-key "x")
  (keymap-set agent-shell-manager-mode-map "?" #'execute-extended-agent-shell-manager-command)
  (agent-shell-menu-mode-key "," agent-shell-manager-toggle))

(use-package agent-shell-notifications
  :load-path "external/agent-shell-notifications"
  :ensure nil
  :after (agent-shell alert)
  :commands (agent-shell-notifications-mode)
  :init
  (add-hook 'agent-shell-mode-hook #'agent-shell-notifications-mode)
  (add-hook 'agent-shell-viewport-edit-mode-hook #'agent-shell-notifications-mode)
  (add-hook 'agent-shell-viewport-edit-view-hook #'agent-shell-notifications-mode)
  :config
  (delight 'agent-shell-notifications-mode nil 'agent-shell-manager)

  (defun tychoish/agent-shell-notifications-alert-send (plist)
    "Send agent-shell notification PLIST through `alert'."
    (let* ((title (plist-get plist :title))
	   (body (plist-get plist :body))
	   (icon (plist-get plist :app-icon))
	   (timeout (plist-get plist :timeout))
	   (buf-name (plist-get plist :shell-buffer-name))
	   (alert-fade-time (if (and (numberp timeout) (> timeout 0))
				timeout
			      alert-fade-time)))
      (alert (or body title "")
	     :title (if (and buf-name (not (string-empty-p buf-name)))
			(format "%s <%s>" (or title "agent-shell") buf-name)
		      (or title "agent-shell"))
	     :icon icon
	     :category 'agent-shell
	     :severity 'normal))
    nil)

  (defun tychoish/agent-shell-notifications-alert-close (_id)
    "No-op close: `alert' styles dismiss themselves." nil)

  (defun tychoish/agent-shell-notifications--add-buffer-name (orig type shell-buffer event)
    "Adds `:shell-buffer-name' to the plist alert payload."
    (append (funcall orig type shell-buffer event)
	    (list :shell-buffer-name (buffer-name shell-buffer))))

  (advice-add 'agent-shell-notifications--make-notification-plist :around #'tychoish/agent-shell-notifications--add-buffer-name)

  (setq agent-shell-notifications-provider nil)
  (setq agent-shell-notifications-send-function #'tychoish/agent-shell-notifications-alert-send)
  (setq agent-shell-notifications-close-function #'tychoish/agent-shell-notifications-alert-close)
  (setq agent-shell-notifications-transform-function #'identity)
  (setq agent-shell-notifications-transform-timeout-function #'identity)
  (setq agent-shell-notifications-timeout 30))

(use-package tychoish-mail
  :ensure nil
  :commands (tychoish-mail-select-account
	     consult-mu-bookmark)
  :init
  (keymap-set hud-mail-map "a" #'tychoish-mail-select-account)
  (keymap-set hud-mail-map "m" #'mu4e)
  (keymap-set hud-mail-map "d" #'mu4e-search-maildir)
  (keymap-set hud-mail-map "b" #'mu4e-search-bookmark)
  (keymap-set hud-mail-map "c" #'mu4e-compose-new)
  (keymap-set hud-mail-map "C-;" #'consult-mu)
  (keymap-set hud-mail-map ";" #'consult-mu-bookmark))

(provide 'tychoish-core)
;;; tychoish-core.el ends here
