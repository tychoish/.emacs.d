;;; tychoish-core.el -- contains all major use-package forms -*- lexical-binding: t; -*-

;;; Commentary:

;; Provides my collection of use-package forms and related
;; configuration for loading and configuring all Emacs packages.

;; This configuration optimizes for lazy-loading so that configuration
;; only loads when called directly or a mode is activated.

;;; Code:
(use-package delight
  :ensure t
  :commands (delight))

(use-package f
  :ensure t
  :commands (f-exists? f-join f-glob f-expand))

(use-package s
  :ensure t
  :commands (s-contains? s-ends-with?))

(use-package dash
  :ensure t
  :commands (--remove --select ->>))

(use-package emacs
  :delight
  (auto-fill-function "afm")
  (overwrite-mode "om")
  (refill-mode "rf")
  (visual-line-mode "wr")
  (fundamental-mode "fun")
  :init
  ;; we don't need autoload magic from use-package
  (bind-keys ("C-x m" . execute-extended-command)
	     ("C-x C-m" . execute-extended-command)
	     ("M-X" . execute-extended-command-for-buffer)
             ("C-x l" . goto-line)
             ("C-x f" . find-file)
             ("C-x d" . dired)
             ("C-x h" . help)
             ("C-x C-x" . exchange-point-and-mark)
	     ;; ("C-x C-u w" . upcase-word)
	     ;; ("C-x C-u t" . upcase-initials-region)
	     ;; ("C-x C-u r" . upcase-region)
             ("C-x C-d" . dired)
             ("C-x C-n" . count-words)
             ("C-c i" . indent-region)
             ("C-<backspace>" . backward-kill-word)
             ("C-h" . backward-kill-word)
             ("C-c d k s" . backward-kill-sentence)
             ("C-c d k p" . backward-kill-paragraph)
             ("C-c d k f" . backward-kill-sexp)
             ("C-c d k d" . delete-region)
             ("C-c d k w" . delete-trailing-whitespace)
	     ;; ("C-c C-w" . whitespace-cleanup)
             ("C-c c" . comment-region)
             ("C-c C-f" . set-fill-column)
             ("C-c C-p" . set-mark-command)
             ("C-c t t d" . disable-theme)
             ("C-c t t e" . enable-theme)
             ("C-c t t l" . load-theme)
             ("C-c C-r" . rename-buffer)
	     ("M-<SPC>" . set-mark-command)
	     ("M-<SPC>" . set-mark-command)
	     ;; ("C-c h a" . mark-whole-buffer)
	     ("s-c" . clipboard-kill-ring-save) ;; (CUA/macOS) copy
	     ("s-v" . clipboard-yank)           ;; (CUA/macOS) paste
	     ("s-x" . clipboard-kill-region)    ;; (CUA/macOS) cut
             ("<mouse-2>" . clipboard-yank)
	     ("C-z" . undo)
             ("C-w" . kill-region)
             ("C-<tab>" . completion-at-point)
             :prefix "C-c g"
             :prefix-map tychoish/ecclectic-grep-map ;;  "C-c g"
             ("o" . occur)
             ("g" . grep)
             :map tychoish/ecclectic-grep-map
             :prefix "p"
             :prefix-map tychoish/ecclectic-grep-project-map ;; "C-c g p"
             ("f" . find-grep))
  (setq custom-file "/dev/null")
  (setq server-use-tcp t)

  (setq gnutls-log-level 0)
  (setq native-comp-jit-compilation t)
  (setq backup-by-copying t)
  (setq make-backup-files t)
  (setq delete-old-versions t)
  (setq ad-redefinition-action 'accept)

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
  (setq fringe-mode 0)
  (setq ring-bell-function (lambda () nil))
  (setq font-lock-support-mode 'jit-lock-mode)
  (setq jit-lock-stealth-time nil)
  (setq jit-lock-defer-time 0.2)
  (setq jit-lock-stealth-nice 0.2)
  (setq jit-lock-stealth-load 100)
  (setq truncate-lines t)
  (setq use-dialog-box nil)
  (setq indent-tabs-mode nil) ; (setq tab-width 4)
  (setq tab-always-indent t)
  (setq cursor-in-non-selected-windows nil)
  (setq comment-auto-fill-only-comments t)

  (setq split-height-threshold 100)
  (setq scroll-conservatively 25)
  (setq scroll-preserve-screen-position t)
  (setq indicate-empty-lines t)
  (setq use-short-answers t) ;; (fset 'yes-or-no-p 'y-or-n-p)
  (setq shell-command-dont-erase-buffer 'end-last-out)
  (setq show-paren-delay 0.25)

  (setq completion-cycle-threshold 2)
  (setq completion-ignore-case t)
  (setq enable-recursive-minibuffers t)
  (setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  (setq read-buffer-completion-ignore-case t)
  (setq read-extended-command-predicate #'command-completion-default-include-p)
  (setq read-file-name-completion-ignore-case t)
  (setq text-mode-ispell-word-completion nil)

  (setq next-line-add-newlines nil)
  (setq undo-auto-current-boundary-timer t)
  (setq read-file-name-completion-ignore-case t)

  (setq select-enable-primary nil)
  (setq select-enable-clipboard nil)
  :config
  (setq confirm-kill-processes nil)
  (setq confirm-nonexistent-file-or-buffer nil)
  (setq confirm-kill-emacs nil)

  ;; (put 'list-timers 'disabled nil)

  ;; (setq server-host "127.0.0.1")
  ;; (setq server-port 2286)
  (cond
   ((eq system-type 'darwin)
    (setq ns-function-modifier 'hyper)
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'super)
    (setq ns-use-srgb-colorspace nil)
    (setq display-highres t))
   ((eq system-type 'gnu/linux)
    (setq x-alt-keysym 'meta)
    (setq x-super-keysym 'super))))

(use-package tychoish-bootstrap
  :functions (gui-p default-string)
  :bind (("C-c f =" . text-scale-increase)
         ("C-c f -" . text-scale-decrease)
         ("C-c f 0" . text-scale-reset)
         ("C-c C-=" . opacity-increase)
         ("C-c C--" . opacity-decrease)
         ("C-c f C-0" . opacity-reset)
         ("<f5>" . xterm-mouse-mode-toggle)
         ("C-c t t D" . disable-all-themes)
         ("C-c t w" . toggle-local-whitespace-cleanup)
	 ("M-<up>" . move-text-up)
         ("M-<down>" . move-text-down))
  :hook (((text-mode prog-mode) . tychoish/setup-show-whitespace)
	 (after-init . tychoish-set-up-user-local-config)
	 (after-init . tychoish/bootstrap-after-init-hook-fn)
	 (auto-save . tychoish/setup-auto-save))
  :commands (tychoish-set-up-user-local-config
	     tychoish/resolve-instance-id
             tychoish-setup-font
             tychoish-get-config-file-prefix
             tychoish-get-config-file-path
	     tychoish/set-tab-width
             add-hygenic-one-shot-hook
             create-toggle-functions
             create-run-hooks-function-for
             with-silence
	     without-messages
             with-temp-keymap
	     uniquify-region-lines
             uniquify-buffer-lines
             font-lock-show-tabs
             font-lock-width-keyword))

(use-package auto-package-update
  :ensure t
  :commands (auto-package-update-maybe auto-package-update-now)
  :defines (auto-package-update-delete-old-versions
            auto-package-update-hide-results
            auto-package-update-interval)
  :init
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (setq auto-package-update-interval 9001))

(use-package async
  :ensure t
  :defer t
  :hook ((after-init . tychoish/async-mode-setup))
  :commands (async-start
	     async-start-process
	     async-bytecomp-package-mode
	     dired-async-mode)
  :init
  (defun tychoish/async-mode-setup ()
    (async-bytecomp-package-mode 1)
    (dired-async-mode 1)))

(use-package esup
  :ensure t
  :commands (esup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; UI, Display, Rendering, Window Man

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

(use-package popon
  :ensure t
  :commands (popon-kill popon-create popon-x-y-at-posn))

(use-package nerd-icons
  :ensure t
  :defer t)

(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package doom-modeline
  :ensure t
  :bind (("C-c t i" . toggle-modeline-icons))
  :hook ((after-init . doom-modeline-mode))
  :commands (doom-modeline-mode
             tychoish-legacy-mode-line)
  :defines (doom-modeline-icon)
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

  (create-toggle-functions doom-modeline-icon)

  (defun my-doom-modeline--font-height ()
    "Calculate the actual char height of the mode-line."
    (/ (frame-char-height) 4))

  (advice-add #'doom-modeline--font-height :override #'my-doom-modeline--font-height)
  (add-hook 'after-init-hook 'turn-on-doom-modeline-icon))

(use-package winner
  :hook (after-init . turn-on-winner-mode)
  :commands (winner-mode winner-undo winner-redo)
  :init
  (defun turn-on-winner-mode ()
    (interactive)
    (unless winner-mode
      (winner-mode))))

(use-package winum
  :ensure t
  :bind (("C-x w n" . winum-select-window-by-number)
         ("C-x w w" . winumw-mode))
  :commands (winum-mode)
  :config
  (setq winum-auto-setup-mode-line nil)
  (setq winum-scope 'frame-local))

(use-package which-key
  :ensure t
  :delight which-key-mode
  :hook ((emacs-startup . which-key-mode)
         (which-key-mode . which-key-setup-side-window-bottom))
  :config
  (setq which-key-idle-delay .25)
  (setq which-key-idle-secondary-delay 0.125))

(use-package windmove
  :bind (("M-h" . windmove-left)
         ("M-j" . windmove-down)
         ("M-k" . windmove-up)
         ("M-l" . windmove-right)
         ("S-<left>" . windmove-left)
         ("S-<down>" . windmove-down)
         ("S-<right>" . windmove-right)
         ("S-<up>" . windmove-up)
         ("M-H" . increase-window-left)
         ("M-J" . increase-window-down)
         ("M-K" . increase-window-up)
         ("M-L" . increase-window-right)
         ("M-<left>" . increase-window-left)
         ("M-<down>" . increase-window-down)
         ("M-<up>" . increase-window-up)
         ("M-<right>" . increase-window-right))
  :init
  (defun increase-window-up () (interactive) (enlarge-window 1 nil))
  (defun increase-window-down () (interactive) (enlarge-window -1 nil))
  (defun increase-window-left () (interactive) (enlarge-window 1 t))
  (defun increase-window-right () (interactive) (enlarge-window -1 t)))

(use-package writeroom-mode
  :ensure t
  :bind (("C-c t i" . writeroom-mode)))

(use-package page-break-lines
  :ensure t
  :delight page-break-lines-mode
  :hook ((text-mode prog-mode) . page-break-lines-mode)
  :commands (global-page-break-lines-mode)
  :config
  (setq page-break-lines-modes '(text-mode prog-mode)))

(use-package whitespace
  :ensure t
  :bind (("C-c C-w" . whitespace-cleanup))
  :commands (whitespace-report)
  :config
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

(use-package elec-pair
  :hook ((after-init . electric-pair-mode))
  :bind (("C-c t p" . #'toggle-electric-pair-inhibition)
         ("C-c t e" . #'toggle-electric-pair-eagerness))
  :init
  (defvar electric-pair-inhibition nil)
  (defvar electric-pair-eagerness t)
  (defun tychoish/electric-pair-inhibition (char)
    (if electric-pair-inhibition
	nil
      (if electric-pair-eagerness
          (electric-pair-default-inhibit char)
        (electric-pair-conservative-inhibit char))))
  :config
  (setq electric-pair-inhibit-predicate #'tychoish/electric-pair-inhibition)
  (create-toggle-functions electric-pair-inhibition)
  (create-toggle-functions electric-pair-eagerness)
  (add-to-list 'electric-pair-pairs '(?< . ?>)))

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
  (setq projectile-known-projects-file (tychoish-get-config-file-path "projectile-bookmarks.el")))

(use-package project
  :after (:any go-ts-mode go-mode c++-mode c-mode c++-ts-mode c-ts-mode)
  :init
  (defun project-find-go-module (dir)
    (when-let ((root (or (locate-dominating-file dir "go.work")
                         (locate-dominating-file dir "go.mod"))))
      (cons 'go-module root)))

  (defun project-find-cmake-project (dir)
    (when-let ((root (locate-dominating-file dir "CMakeLists.txt")))
      (cons 'cmake-root root)))

  (cl-defmethod project-root ((project (head go-module))) (cdr project))
  (cl-defmethod project-root ((project (head cmake-root))) (cdr project))

  (add-hook 'project-find-functions #'project-find-go-module)
  (add-hook 'project-find-functions #'project-find-cmake-project))

(use-package dired
  :bind (("C-x d" . dired)
	 :map dired-mode-map
	 ("r" . wdired-change-to-wdired-mode))
  :config
  (put 'dired-find-alternate-file 'disabled nil))

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
         ("d" . tychoish-rg-repo)
         ("g" . tychoish-rg)
         ("m" . tychoish-find-merges)
         :map tychoish/ecclectic-grep-project-map
         ("r" . tychoish-rg-repo))
  :defines (tychoish/ecclectic-rg-map)
  :commands (tychoish-rg tychoish-rg-repo tychoish-find-merges)
  :config
  (setenv "RIPGREP_CONFIG_PATH" (f-expand "~/.ripgreprc"))

  (defun tychoish-rg (regexp)
    (interactive (list (read-from-minibuffer "ripgrep for: " (thing-at-point 'symbol))))
    (ripgrep-regexp regexp default-directory))

  (defun tychoish-rg-repo (regexp)
    (interactive (list (read-from-minibuffer "ripgrep for: " (thing-at-point 'symbol))))
    (ripgrep-regexp regexp (projectile-project-root)))

  (defun tychoish-find-merges ()
    (interactive)
    (ripgrep-regexp "^(=======$|<<<<<<<|>>>>>>>)" (projectile-project-root))))

(use-package deadgrep
  :ensure t
  :bind (:map tychoish/ecclectic-rg-map
	      ("x" . #'deadgrep))
  :after (ripgrep))

(use-package ag
  :ensure t
  :bind (:map tychoish/ecclectic-grep-map ;; "C-c g"
	 :prefix "a"
	 :prefix-map tychoish/ecclectic-ag-grep-map
	 ("s" . ag)
	 ("f" . ag-files)
	 ("p" . ag-project)
	 ("o" . ag-project-at-point)
	 :map tychoish/ecclectic-ag-grep-map
	 :prefix "d"
	 :prefix-map tychoish/ecclectic-ag-dired-map
	 ("f" . ag-dired)
	 ("p" . ag-project-dired))
  :config
  (setq ag-highlight-search t))

(use-package grep-mode
  :defer t
  :defines (grep-mode-map)
  :bind (:map tychoish/ecclectic-grep-map
         ("g" . grep)))


(use-package wgrep
  :ensure t
  :after (grep)
  :bind (:map grep-mode-map
         ("r" . wgrep-change-to-wgrep-mode))
  :config
  (setq wgrep-enable-key "r"))

(use-package eww
  :bind (("C-c w d" . browse-url-generic)
         ("C-c w e" . browse-url)
         ("C-c w f" . browse-url-firefox)
         ("C-c w c" . browse-url-chrome)
         ("C-c w g" . eww-search-words))
  :commands (eww eww-browse-url)
  :init 
  (setq browse-url-browser-function 'eww-browse-url)
  :config
  (setq browse-url-generic-program "chrome")
  (setq shr-color-visible-luminance-min 80)
  (setq shr-use-colors nil)
  (setq shr-use-fonts nil)
  (setq eww-search-prefix "https://www.google.com/search?q="))

(use-package google-this
  :ensure t
  :delight google-this-mode
  :bind-keymap ("C-c /" . google-this-mode-submap)
  :commands (google-this-mode)
  :config
  (google-this-mode 1))

(use-package eldoc
  :delight eldoc-mode
  :bind (("C-c d d" . eldoc)
	 ("C-c d e" . eldoc-doc-buffer))
  :commands (eldoc-mode
	     global-eldoc-mode
             eldoc
	     eldoc-print-current-symbol-info
             eldoc-documentation-compose-eagerly
             turn-on-eldoc-mode)
  :init
  (bind-keys ("C-c d q" . #'kill-eldoc-and-help-buffers)
	     ("C-c d j" . jump-to-elisp-help))

  (defun kill-eldoc-and-help-buffers ()
    "Kills all eldoc and help buffers"
    (interactive)
    (kill-matching-buffers "\*Help\\*\\|*eldoc.*\\*" nil t))

  (defun jump-to-elisp-help ()
    (interactive)
    (apropos-documentation (symbol-name (intern-soft (thing-at-point 'symbol)))))
  :config
  (setq eldoc-echo-area-use-multiline-p t)
  (setq eldoc-echo-area-prefer-doc-buffer nil)
  (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly))

(use-package anzu
  :ensure t
  :delight anzu-mode
  :commands (anzu-query-replace anzu-query-replace-regexp global-anzu-mode anzu-mode)
  :hook ((isearch-mode) . anzu-mode)
  :bind (("C-c q r" . anzu-query-replace)
         ("C-c q x" . anzu-query-replace-regexp)
         :map isearch-mode-map
         ("C-o" . isearch-occur))
  :config
  (setq query-replace-highlight t)
  (setq search-highlight t)
  (defalias 'srr 'string-replace-regexp)
  (defalias 'sr 'string-replace)
  (defalias 'qrr 'anzu-query-replace-regexp)
  (defalias 'qr 'anzu-query-replace))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Session/State Management

(use-package revbufs
  :ensure t
  :bind (("C-c C-g" . revbufs))
  :commands (revbufs))

(use-package session
  :ensure t
  :bind (("C-c t ;" . session-toggle-permanent-flag))
  :hook ((server-after-make-frame . tychoish/setup-daemon-session)
	 (after-save . tychoish-save-session))
  :commands (session-initialize session-save-session)
  :init
  (defun tychoish/setup-daemon-session ()
    (session-initialize))

  ;; use session-save to save the desktop manually
  (defun tychoish-save-session ()
    "Save an emacs session... sometimes"
    (interactive)

    (when (> 40 (random 100))
        (session-save-session t)))
  :config
  (setq session-save-file-coding-system 'utf-8-emacs)
  (setq session-save-print-spec '(t nil 40000))
  (setq session-save-file (tychoish-get-config-file-path "session.el")))

(use-package desktop
  :ensure t
  :hook ((after-save . tychoish-save-desktop))
  :commands (desktop-save-mode desktop-read desktop-save-in-desktop-dir)
  :init
  (setq desktop-dirname user-emacs-directory)
  (add-hook 'emacs-startup-hook 'tychoish/desktop-read-init)

  (defun tychoish/desktop-read-init ()
    (setq desktop-base-file-name (tychoish-get-config-file-prefix "desktop.el"))
    (when (file-exists-p (f-join desktop-dirname desktop-base-file-name))
      (let ((gc-cons-threshold 800000)
	    (inhibit-message t))
        (desktop-read))))

  (defun tychoish-save-desktop ()
    "Save desktop... sometimes"
    (interactive)

    (when (> 40 (random 100))
      (desktop-save-in-desktop-dir)))
  :config
  (setq desktop-load-locked-desktop t)
  (setq desktop-restore-frames t)
  (setq desktop-restore-eager t)
  (setq desktop-path `(,user-emacs-directory))

  (setq desktop-buffers-not-to-save
        (concat "\\("
                "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS\\|"
                "\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
                "\\)$"))

  (setq desktop-files-not-to-save
        (concat "\\(\\`/[^/:]*:\\|(ftp)\\'\\)" ;; default
                "^/usr/lib/go/.*\\|"
                "^/usr/lib/rustlib/.*\\|"
                "^/home.+go/pkg/mod\\|"
                "^/home.+\\.cargo"))
  (setq desktop-base-file-name (tychoish-get-config-file-prefix "desktop.el"))
  (setq desktop-base-lock-name (tychoish-get-config-file-prefix (format "desktop-%d.lock" (emacs-pid))))
  (setq desktop-base-file-name (tychoish-get-config-file-prefix "desktop.el"))

  (add-to-list 'desktop-globals-to-save 'register-alist)
  (add-to-list 'desktop-globals-to-save 'file-name-history)
  (add-to-list 'desktop-modes-not-to-save 'dired-mode)
  (add-to-list 'desktop-modes-not-to-save 'Info-mode)
  (add-to-list 'desktop-modes-not-to-save 'org-mode)
  (add-to-list 'desktop-modes-not-to-save 'eww-mode)
  (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
  (add-to-list 'desktop-modes-not-to-save 'fundamental-mode))

(use-package savehist
  :hook ((prescient-persist-mode . savehist-mode-setup))
  :init
  (defun savehist-mode-setup ()
    (savehist-mode 1)
    (setq-default savehist-file (tychoish-get-config-file-path "savehist.el")))
  :config
  (setq savehist-coding-system 'utf-8-emacs))

(use-package bookmark
  :hook (prescient-persist-mode . tychoish/bookmark-setup)
  :init
  (defun tychoish/bookmark-setup ()
    ;; setting the list before calling the function reduce the total time
    (setq bookmark-default-file (tychoish-get-config-file-path "bookmarks.el")))
  :custom
  (setq bookmark-save-flag 1))

(use-package autorevert
  :hook (emacs-startup . global-auto-revert-mode)
  :delight auto-revert-mode
  :commands (auto-revert-mode)
  :config
  (setq auto-revert-verbose nil)
  (setq auto-revert-avoid-polling t)
  (setq auto-revert-interval 60))

(use-package recentf
  :bind ("C-x C-r" . recentf)
  :hook ((prescient-persist-mode . quiet-start-recentf-mode))
  :init
  (defun quiet-start-recentf-mode ()
    (with-silence (recentf-mode t)))

  (with-eval-after-load 'consult
    ;; rebind key after lazy loading package
    (bind-key "C-x C-r" #'consult-recent-file 'global-map))
  :config
  (setq recentf-auto-cleanup 'never)
  (setq recentf-keep '(file-remote-p file-readable-p))
  (setq recentf-max-menu-items 100)
  (setq recentf-save-file (tychoish-get-config-file-path "recentf.el")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; org-mode, tychoish custom, etc

(use-package org
  :mode (("\\.org$" . org-mode))
  :delight
  (org-mode "org")
  (org-agenda-mode "agenda")
  :bind (:prefix "C-c o"
         :prefix-map tychoish/global-org-map
         ("a" . org-agenda)
         ("k" . org-capture)
         :map tychoish/global-org-map
         :prefix "l"
         :prefix-map tychoish/org-link-mode-map
         ("s" . org-store-link)
         ("i" . org-insert-link)
         ("a" . org-annotate-file)
         :map org-mode-map
         ("C-c l o" . org-link-open-from-string)
         ("C-c C-p" . set-mark-command)
         ("M-TAB" . org-cycle)
         ("C-M-TAB" . org-cycle-force-archived)
         :map org-mode-map
         :prefix "C-c o"
         :prefix-map tychoish/org-mode-personal-map ;; "C-c o"
         ("a" . org-agenda)
         ("t" . org-set-tags-command)
         ("n" . org-narrow-to-subtree)
         ("w" . widen)
         ("p" . org-insert-property-drawer)
         ("w" . org-refile)
         ("d" . tychoish-org-date-now)
         ("i" . org-ctags-create-tags)
         :map tychoish/org-mode-personal-map
         :prefix "c"
         :prefix-map tychoish/org-mode-capture-map
         ("c" . org-capture)
         ("p" . org-capture-goto-last-stored)
         ("l" . org-capture-goto-last-stored)
         ("t" . org-capture-goto-target)
         ("r" . org-capture-refile)
         ("w" . org-capture-refile)
         :map tychoish/org-mode-personal-map
         :prefix "a"
         :prefix-map tychoish/org-mode-personal-archive-map
         ("d" . tychoish-org-mark-done-and-archive)
         ("e" . org-cycle-force-archived)
         ("f" . org-archive-set-tag)
         ("s" . org-archive-to-archive-sibling)
         :map tychoish/org-mode-personal-map
         :prefix "r"
         :prefix-map tychoish/org-mode-personal-bibtex-map
         ("a" . org-bibtex-check-all)
         ("c" . org-bibtex-create)
         ("e" . org-bibtex)
         ("k" . org-bibtex-export-to-kill-ring)
         ("r" . org-bibtex-create-in-current-entry)
         ("s" . org-bibtex-search)
         ("v" . org-bibtex-check))
  :hook ((org-ctrl-c-ctrl-c-hook . org-set-weekday-of-timestamp))
  :commands (org-save-all-org-buffers)
  :defines (org-mode-agenda-map org-mode-map)
  :config
  (setq org-agenda-files `(,org-directory))
  (setq org-agenda-include-diary nil)
  (setq org-agenda-custom-commands
        '(("b" "Backlog" tags "+backlog|+inbox-ITEM=\"Inbox\"|TODO=BLOCKED")
          ("c" "SuperView"
           ((agenda "" ((org-agenda-overriding-header "Schedule:")
                        (org-super-agenda-groups
                         '((:name "Today"
                                  :time-grid t
                                  :date today
                                  :order 1)))))
            (alltodo "" ((org-agenda-overriding-header "Tasks:")
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

  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "DONE(d!)")
          (sequence "BLOCKED(s)" "BACKLOG(b)" "INPROGRESS(p)" "|" "SKIPPED" "GONEAWAY(g@)" "INCOMPLETE(i@)")))

  (setq org-todo-keyword-faces
        '(("TODO" . org-warning)
          ("INPROGRESS" . "orange")
          ("INCOMPLETE" . "orange")
          ("SCHEDULED" . "green")
          ("BACKLOG" . (:foreground "orange" :weight bold))
          ("PROJECT" . (:foreground "blue" :weight bold))))

  (setq org-tag-alist
        '((:startgroup . nil)
          ("inbox" . ?i)
          ("backlog" . ?b)
          (:endgroup . nil)
          (:startgroup . nil)
          ("@desk" . ?d)
          ("@personal" . ?p)
          ("@work" . ?w)
          (:endgroup . nil)))

  (setq org-modules
        '(org-capture
          org-datetree
          org-annotate-file
          org-depend
          org-habit))

  (setq org-CUA-compatible t)
  (setq org-tags-column -70)
  (setq org-agenda-block-separator nil)
  (setq org-agenda-columns-add-appointments-to-effort-sum t)
  (setq org-agenda-compact-blocks t)
  (setq org-agenda-default-appointment-duration 60)
  (setq org-agenda-inhibit-startup nil)
  (setq org-agenda-mouse-1-follows-link t)
  (setq org-agenda-use-time-grid t)
  (setq org-agenda-skip-deadline-if-done nil)
  (setq org-agenda-skip-scheduled-if-deadline-is-shown nil)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-unavailable-files t)
  (setq org-agenda-skip-timestamp-if-done t)
  (setq org-agenda-todo-ignore-deadlines t)
  (setq org-agenda-todo-ignore-scheduled t)
  (setq org-agenda-start-on-weekday nil)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-enforce-todo-dependencies t)
  (setq org-fast-tag-selection-include-todo t)
  (setq org-fontify-done-headline t)
  (setq org-footnote-auto-label nil)
  (setq org-footnote-define-inline nil)
  (setq org-footnote-section nil)
  (setq org-log-into-drawer t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-provide-todo-statistics t)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-targets '((org-agenda-files :maxlevel . 4)))
  (setq org-refile-use-outline-path 'file)
  (setq org-replace-disputed-keys t)
  (setq org-return-follows-link t)
  (setq org-reverse-note-order t)
  (setq org-startup-folded 'content)
  (setq org-startup-indented nil)
  (setq org-tags-exclude-from-inheritance '("project"))
  (setq org-track-ordered-property-with-tag t)
  (setq org-use-fast-tag-selection 'auto)
  (setq org-use-fast-todo-selection 'auto)
  (setq org-use-speed-commands (lambda () (and (looking-at org-outline-regexp) (looking-back "^\**"))))

  (setq org-agenda-files (cl-remove-duplicates (append org-agenda-files user-org-directories)))
  (setq org-annotate-file-storage-file (concat org-directory "/records.org"))
  (setq org-archive-location (concat org-directory "/archive/%s::datetree/"))
  (setq org-default-notes-file (concat org-directory "/records.org"))
  (setq org-directory (concat local-notes-directory "/org"))

  (add-hook 'org-mode-hook 'turn-on-soft-wrap)
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)

  (with-eval-after-load 'org-agenda
    (bind-keys :map org-agenda-mode-map
	       ("C-l" . org-agenda-open-link)
	       ("M-c" . org-agenda-goto-calendar)))

  (with-eval-after-load 'helm
    (bind-keys :map tychoish/helm-center-menu-map
               :prefix "o"
               :prefix-map tychoish/org-mode-personal-helm-map
               ("b" . helm-org-in-buffer-headings)
               ("c" . helm-capture-templates)
               ("p" . helm-org-parent-headings)
               ("a" . helm-org-agenda-files-headings))))

(use-package toc-org
  :ensure t
  :commands (toc-org-insert-toc)
  :autoload (tychoish--add-toc-org-hook)
  :config
  (defun tychoish--add-toc-org-op ()
    (save-excursion (toc-org-insert-toc)))

  (defun tychoish--add-toc-org-hook ()
    (add-hook 'write-contents-functions 'tychoish--add-toc-org-op nil t)))

(use-package ox-gist
  :ensure t
  :bind (:map tychoish/org-mode-personal-map
              ("e g p" . org-gist-export-to-private-gist)
              ("e g g" . org-gist-export-to-public-gist))
  :commands (org-gist-export-private-gist org-gist-export-to-public-gist)
  :init
  (defun org-gist-export-private-gist ()
    (interactive)
    (org-gist-export-to-gist))
  (defun org-gist-export-public-gist ()
    (interactive)
    (org-gist-export-to-gist 'public)))

(use-package ox-hugo
  :ensure t
  :after ox)

(use-package ox-rst
  :ensure t
  :after ox
  :commands (org-rst-export-to-rst org-rst-export-as-rst)
  :config
  (setq org-rst-headline-underline-characters (list ?= ?- ?~ ?' ?^ ?`)))

(use-package ox-leanpub
  :ensure t
  :after ox
  :commands (org-leanpub-book-export-markdown
	     org-leanpub-book-export-markua
	     org-leanpub-markua-export-to-markua
	     org-leanpub-markua-export-as-markua
	     org-leanpub-markdown-export-to-markdown
	     org-leanpub-markdown-export-as-markdown)
  :config
  (require 'ox-leanpub-markua)
  (org-leanpub-book-setup-menu-markua))

(use-package tychoish-org
  :hook ((org-mode . tychoish/background-revbufs-for-hook)
         (org-mode . tychoish--add-toc-org-hook))
  :commands (tychoish-org-setup-standard-capture-templates
             tychoish-org-add-project-file-capture-templates
             tychoish-org-reset-capture-templates
             org-agenda-files-reload
             tychoish-org-date-now
             org-set-weekday-of-timestamp
             tychoish-org-mark-done-and-archive)
  :config
  (tychoish-org-reset-capture-templates)
  (tychoish-org-setup-standard-capture-templates)
  (org-load-modules-maybe t))

(use-package tychoish-blogging
  :bind (("C-c t b m" . tychoish-blog-insert-date)
         ("C-c t b p" . tychoish-blog-publish-post)
         ("C-c t b n" . tychoish-blog-create-post)
         ("C-c t b C-p" . tychoish-blog-push)
         ("C-c t b d" . tychoish-blog-open-drafts-dired))
  :commands (tychoish-create-note-file
             make-filename-slug)
  :config
  (setq tychoish-blog-path (expand-file-name "~/src/blog")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Completion/Snippets Menus

(use-package cape
  :ensure t
  :bind (:prefix "C-c ."
         :prefix-map tychoish/completion-map
         ;; this is mostly copy-pasta from cape-mode-map, with tweaks
         ("TAB" . completion-at-point)
         ("." . completion-at-point)
         ("p" . completion-at-point)
         ("t" . complete-tag)
         ("d" . cape-dabbrev)
         ("h" . cape-history)
         ("f" . cape-file)
         ("s" . cape-elisp-symbol)
         ("e" . cape-elisp-block)
         ("a" . cape-abbrev)
         ("l" . cape-line)
         ("w" . cape-dict)
         ("k" . cape-keyword)
         (":" . cape-emoji)
         ("e" . cape-emoji)
         ("\\" . cape-tex)
         ("_" . cape-tex)
         ("^" . cape-tex)
         ("&" . cape-sgml)
         ("r" . cape-rfc1345))
  :init
  (defun tychoish/capf-line ()
    (cape-wrap-prefix-length #'cape-line 5))

  (defun tychoish/capf-local-sources ()
    (cape-wrap-super #'cape-dabbrev #'cape-dict #'tychoish/capf-line))

  (defun tychoish/capf-elisp-combined ()
    (cape-wrap-super #'cape-elisp-symbol #'cape-elisp-block
                     #'cape-keyword #'cape-dabbrev))

  (defun tychoish/text-mode-capf-setup ()
    (setq-local completion-at-point-functions
                (list #'tychoish/capf-local-sources
                      #'yasnippet-capf
                      #'cape-rfc1345
                      #'cape-emoji
                      #'cape-file)))

  (defun tychoish/elisp-capf-setup  ()
    (setq-local completion-at-point-functions
                (list #'tychoish/capf-elisp-combined
                      #'yasnippet-capf
                      ;; #'tychoish/capf-line
                      #'cape-file
                      #'cape-emoji)))

  (defun tychoish/eglot-capf-setup ()
    (setq-local completion-category-defaults nil)
    (setq-local completion-at-point-functions
                (list #'eglot-completion-at-point
                      #'tychoish/capf-local-sources
                      #'yasnippet-capf
                      #'cape-emoji
                      #'cape-file)))

  (with-eval-after-load 'eglot
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

  (defun tychoish/soliditiy-capf-setup ()
    (setq-local completion-at-point-functions
                (list #'tychoish/capf-solidity
                      #'tychoish/capf-line
                      #'yasnippet-capf
                      #'cape-emoji
                      #'cape-file)))

  (add-hook 'solidity-mode-hook #'tychoish/soliditiy-capf-setup)
  (add-hook 'eglot-managed-mode-hook #'tychoish/eglot-capf-setup)
  (add-hook 'emacs-lisp-mode-hook 'tychoish/elisp-capf-setup)
  (add-hook 'telega-chat-mode-hook #'tychoish/text-mode-capf-setup)
  (add-hook 'text-mode-hook #'tychoish/text-mode-capf-setup)

  (add-hook 'completion-at-point-functions #'tychoish/capf-local-sources)
  (add-hook 'completion-at-point-functions #'yasnippet-capf)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-rfc1345)
  (add-hook 'completion-at-point-functions #'cape-emoji)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-history))

(use-package yasnippet
  :ensure t
  :delight (yas-minor-mode " ys")
  :commands (yas-global-mode yas-insert-snippet yas-minor-mode)
  :hook ((text-mode prog-mode) . yas-minor-mode)
  :config
  (add-to-list 'load-path (f-join user-emacs-directory "snippets")))

(use-package yasnippet-capf
  :ensure t
  :bind (:map tychoish/completion-map
         ("s" . yasnippet-capf))
  :commands (yasnippet-capf))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'authinfo-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

;; (use-package abbrev
;;   :defer t
;;   :commands (abbrev-mode expand-abbrev abbrev-suggest)
;;   :delight (abbrev-mode "abb")
;;   :config
;;   (setq save-abbrevs t)
;;   (setq abbrev-file-name (tychoish-get-config-file-path "abbrev.el"))
;;   (if (file-exists-p abbrev-file-name)
;;       (quietly-read-abbrev-file)))

(use-package prescient
  :ensure t
  :hook (after-init . prescient-persist-mode)
  :config
  (setq prescient-filter-method '(literal prefix initialism anchored fuzzy regexp))
  (setq prescient-sort-full-matches-first t)
  (setq prescient-completion-highlight-matches t)
  (setq prescient-sort-length-enable nil)
  (setq completion-preview-sort-function #'prescient-completion-sort)
  :config
  (setq prescient-save-file (tychoish-get-config-file-path "prescient.el")))

(use-package vertico
  :ensure t
  :hook (after-init . vertico-mode)
  :defines (vertico-multiform-categories tychoish/vertico-disable-sort-for vertico-sort-function)
  :init
  (defmacro tychoish/vertico-disable-sort-for (command)
    "Disable sorting in vertico rendering."
    `(add-to-list 'vertico-multiform-categories '(,command (vertico-sort-function . nil))))
  :config
  (setq vertico-resize t)
  (setq vertico-count 25)
  (setq vertico-cycle t)
  (vertico-multiform-mode 1)
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
  (setq vertico-prescient-enabl-sorting t)
  (setq vertico-prescient-enable-filtering t))

(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :hook (after-init . marginalia-mode))

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
  :defines (corfu-margin-formatters corfu-continue-commands corfu-ap )
  :hook (((prog-mode shell-mode eshell-mode text-mode) . corfu-mode)
         (telega-chat-mode . corfu-mode))
  :bind (:map corfu-map
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
         ("M-m" . corfu-move-to-minibuffer))
  :init
  (add-hook 'text-mode-hook 'tychoish/corfu-text-mode-setup)
  (add-hook 'prog-mode-hook 'tychoish/corfu-prog-mode-setup)
    (defun tychoish/corfu-text-mode-setup ()
    (setq-local corfu-auto-prefix 2))

  (defun tychoish/corfu-prog-mode-setup ()
    (setq-local corfu-auto-prefix 3))

  (defun corfu-move-to-minibuffer ()
    (interactive)
    (pcase completion-in-region--data
      (`(,beg ,end ,table ,pred ,extras)
       (let ((completion-extra-properties extras)
             completion-cycle-threshold completion-cycling)
         (consult-completion-in-region beg end table pred)))))
  :config
  (setq corfu-cycle t)
  (setq corfu-quit-at-boundary t)
  (setq corfu-quit-no-match t)
  (setq corfu-preview-current t)
  (setq corfu-preselect 'valid)
  (setq corfu-on-exact-match 'insert)
  (setq corfu-auto t)
  (setq corfu-auto-delay 0.15)
  (setq corfu-popupinfo-delay .1)
  (setq corfu-indexed-start 1)
  (setq global-corfu-minibuffer nil)
  (add-to-list 'corfu-continue-commands #'corfu-move-to-minibuffer)
  (corfu-indexed-mode)
  (corfu-history-mode)
  (corfu-popupinfo-mode))

(use-package corfu-prescient
  :ensure t
  :after (prescient)
  :hook (corfu-mode . corfu-prescient-mode)
  :config
  (setq corfu-prescient-override-sorting t)
  (setq corfu-prescient-enable-sorting t)
  (setq corfu-prescient-enable-filtering t))

(use-package corfu-terminal
  :hook ((global-corfu-mode corfu-mode) . corfu-terminal-mode)
  :after (popon)
  :defines (corfu-terminal-disable-on-gui)
  :commands (corfu-terminal-mode))

(use-package nerd-icons-corfu
  :ensure t
  :after (corfu nerd-icons)
  :commands (nerd-icons-corfu-formatter)
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-xref
  :ensure t
  :after (xref)
  :hook (nerd-icons-completion-mode . nerd-icons-xref-mode))

(use-package nerd-icons-completion
  :ensure t
  :hook (((marginalia-mode) . nerd-icons-completion-marginalia-setup)
         ((corfu-mode vertico-mode) . nerd-icons-completion-mode)))

(use-package consult
  :ensure t
  :bind (;; C-c bindings in `mode-specific-map'
         ("C-c C-x C-m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ("C-c i" . consult-info)
         ("C-c d m" . consult-man)
         ("C-c C-x r r" . consult-register)
         ("C-c C-x r s" . consult-register-store)
         ("C-c C-x r l" . consult-register-load)
         ;; tychoish/wacky
         ("C-c d s" . describe-symbol)
         ([remap Info-search] . consult-info)
         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x C-b" . switch-to-buffer)            ;; orig. list-buffers
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ("C-x C-f" . consult-find)
         ;; Custom M-# bindings for fast register access
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-from-kill-ring)                ;; orig. yank-pop
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("C-c e" . consult-compile-error)
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("C-c C-s" . consult-line)
         ("M-g s" . consult-line)
         ("M-i" . consult-imenu)
         ("C-M-I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find)                  ;; Alternative: consult-find
         ("M-s c" . consult-locate)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         :prefix "C-c C-."
         :prefix-map tychoish/consult-mode-map
         ("h" . consult-history)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("C-g" . tychoish/super-abort-minibuffers)
         ("C-j" . tychoish/super-abort-minibuffers)
         ("C-l" . backward-kill-word)
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history)                 ;; orig. previous-matching-history-element
	 :map tychoish/ecclectic-grep-map ;; "C-c g"
         ("f" . consult-grep)
         ("s l" . consult-line)
         ("s m" . consult-line-multi)
         ("s k" . consult-keep-lines)
         ("s f" . consult-focus-lines)
	 :map tychoish/ecclectic-grep-project-map ;; "C-c g p"
         ("g" . consult-git-grep)                  ;; for git(?)
	 :map tychoish/org-mode-personal-map ;; "C-c o"
         ("h" . consult-org-heading)               ;; Alternative: consult-org-heading (for jump)
         ("s" . consult-org-agenda))               ;; Alternative: consult-org-heading (for jump))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :functions (consult-xref consult--read consult-completion-in-region consult-register-window)
  :defines (consult-preview-key)
  :commands (consult-find consult-git-grep consult-grep)
  :init
  (defun tychoish/super-abort-minibuffers ()
    (interactive)
    (if (not (minibuffer-selected-window))
        (keyboard-quit)
      (abort-minibuffers)
      (minibuffer-quit))
    (when (minibuffer-selected-window)
      (move-beginning-of-line nil)
      (kill-line)
      (abort-minibuffers)))

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

  :config
  (setq register-preview-delay 0.05)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref)

  (setq consult-async-split-style 'semicolon)
  (setq consult-async-min-input 2)
  (setq consult-async-input-debounce 0.05)
  (setq consult-async-input-throttle 0.125)
  (setq consult-async-refresh-delay 0.05)

  (setq consult-preview-key '("M-." "M-?"))
  (setq consult-preview-key (append consult-preview-key '(:debounce 0.25)))

  (advice-add #'register-preview :override #'consult-register-window)
  (consult-customize consult-find
   :require-match nil
   :initial "./")
  (consult-customize consult-ripgrep consult-git-grep consult-grep
   :require-match nil
   :keymap
   (with-temp-keymap map
     (define-key map (kbd "C-l") #'consult-ripgrep-up-directory))))

(use-package consult-flycheck
  :ensure t
  :bind (("C-c f m" . consult-flycheck)
         ("M-g f" . consult-flycheck)))

(use-package consult-flyspell
  :ensure t
  :after (consult flyspell)
  :bind (("C-c ;" . consult-flyspell)
         ("C-;" . flyspell-correct-wrapper)
         ("C-c a f" . consult-flyspell))
  :commands (consult-flyspell flyspell-correct-consult)
  :init
  (defun flyspell-correct-consult (candidates word)
    (let ((completing-read-function
           (lambda (prompt collection &rest _)
	     (consult--read collection
	                    :initial word
	                    :prompt prompt))))
      (flyspell-correct-completing-read
       candidates
       word)))
  (setq flyspell-correct-interface #'flyspell-correct-consult))

(use-package consult-eglot
  :ensure t
  :after (consult eglot)
  :bind ("C-c d a" . consult-eglot-symbols)
  :commands (consult-eglot-symbols))

(use-package consult-gh
  :ensure t
  :after (consult)
  :commands (consult-gh))

(use-package consult-ag
  :after (consult)
  :bind (("M-g a" . consult-ag)
	 :map tychoish/ecclectic-ag-grep-map
	 ("g" . consult-ag)
         :map tychoish/consult-mode-map
         ("a" . consult-ag))
  :commands (consult-ag))

(use-package consult-yasnippet
  :ensure t
  :after (consult yasnippet)
  :bind (:map tychoish/consult-mode-map
         ("s" . consult-yasnippet)))

(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult-tycho
  :bind (("M-g r" . consult-rg)
	 :map tychoish/consult-mode-map
         ("t" . consult-rg-for-thing)
         ("s" . consult-rg-pwd)
	 ("r" . consult-rg)
	 :map tychoish/ecclectic-rg-map
         ("t" . consult-rg-for-thing)
         ("r" . consult-rg)
         ("s" . consult-rg-pwd)
	 ("p" . consult-rg)
	 :map tychoish/ecclectic-grep-map ;; "C-c g"
         ("s r" . consult-rg)
	 :map tychoish/global-org-map ;; "C-c o"
	 ("j" . consult-org-capture)
	 ("c" . consult-org-capture)
	 :map tychoish/org-mode-capture-map
	 ("j" . consult-org-capture)
	 ("h" . consult-org-capture-target))
  :commands (consult-rg-for-thing
             consult-rg
             consult-org-capture
             consult-org-capture-target))

(use-package consult-sardis
  :bind ("C-c t r" . consult-sardis-run))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; helm tools (legacy)

(use-package helm
  :ensure t
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
         ;; ("o" . tychoish/orgmode-personal-helm-map)
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
  (setq helm-split-window-in-side-p t)
  (setq helm-c-adaptive-history-file (tychoish-get-config-file-path "helm-c-adaptive-history.el"))
  (setq helm-c-adaptive-sorting t)
  (helm-autoresize-mode 1)
  (set-face-attribute 'helm-source-header nil :height 0.98 :family "Source Code Pro" :weight 'semibold :background 'unspecified))

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

(use-package helm-make
  :ensure t
  :commands (helm-make)
  :config
  (setq helm-make-named-buffer t)
  (setq helm-make-fuzzy-matching t)
  (setq helm-make-cache-targets t)
  (setq helm-make-do-save t)
  (setq helm-make-sort-targets t))

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
         ("r" . helm-rg))
  :commands (helm-rg)
  :config
  (set-face-attribute 'helm-rg-error-message nil :foreground "pink4" :background 'unspecified :weight 'normal)
  (set-face-attribute 'helm-rg-active-arg-face nil :foreground "olive drab")
  (set-face-attribute 'helm-rg-base-rg-cmd-face nil :foreground "dim gray")
  (set-face-attribute 'helm-rg-directory-cmd-face nil :foreground "brown")
  (set-face-attribute 'helm-rg-directory-header-face nil :foreground 'unspecified :weight 'extra-bold)
  (set-face-attribute 'helm-rg-extra-arg-face nil :foreground "yellow4")
  (set-face-attribute 'helm-rg-file-match-face nil :foreground "#088")
  (set-face-attribute 'helm-rg-inactive-arg-face nil :foreground "dim gray")
  (set-face-attribute 'helm-rg-title-face nil :foreground "purple" :weight 'bold))

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

(use-package helm-c-yasnippet
  :ensure t
  :defer t
  :commands (helm-yas-complete)
  :bind (:map tychoish/helm-center-menu-map
	 ("C-e" . helm-yas-complete)
	 ("C-." . helm-yas-complete))
  :config (setq helm-yas-space-match-any-greedy t))

(use-package helm-eww
  :ensure t
  :bind (:map tychoish/helm-center-menu-map
         ("C-c h c" . helm-eww-history)))

(use-package helm-flycheck
  :ensure t
  :bind (:map tychoish/helm-center-menu-map
         ("f" . 'helm-flycheck))
  :commands (helm-flycheck))

(use-package helm-org
  :ensure t
  :defer t
  :bind (:map tychoish/helm-center-menu-map
	 :prefix ";"
	 :prefix-map tychoish/helm-org-mode-map
	 ("c" . helm-org-capture-templates)
	 ("f". helm-org-in-buffer-heddings)
	 ("a" . helm-org-agenda-files-headings))
  :commands (helm-org-capture-templates
             helm-org-in-buffer-heddings
             helm-org-agenda-files-headings)
  :config 
  (setq add-to-list 'helm-completing-read-handlers-alist '(org-capture . helm-org-completing-read-tags))
  (setq add-to-list 'helm-completing-read-handlers-alist '(org-set-tags . helm-org-completing-read-tags)))

(use-package eshell
  :commands (eshell)
  :after (helm)
  :bind (("C-c s e" . eshell)
         :map eshell-command-mode-map
         ("M-p" . helm-eshell-history)
         ([remap eshell-pcomplete] . helm-esh-pcomplete)
         ("M-s f" . helm-eshell-prompts-all)
         ("M-r" . helm-eshell-history))
  :hook (eshell-mode . eshell-cmpl-initialize))

(use-package helm-mu
  :ensure t
  :bind (:map tychoish/helm-center-menu-map ; "C-c h"
         ("m" . helm-mu)
         ("v" . helm-mu-contacts)))

(use-package helm-flyspell
  :ensure t
  :bind (:map tychoish/helm-center-menu-map
         ("e" . helm-flyspell-correct)))

(use-package flyspell-correct-helm
  :ensure t
  :after (helm-flyspell)
  :config
  (setq flyspell-correct-interface #'flyspell-correct-helm))

(use-package helm-xref
  :ensure t
  :after (xref helm))

;; (use-package helm-slime
;;   :ensure t
;;   :after (slime helm)
;;   :commands (helm-slime-mode)
;;   :hook (slime-lisp-mode . helm-slime-mode))

(use-package tychoish-company
  :disabled)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; version control

(use-package magit
  :ensure t
  :bind (("C-x g s" . magit-status)
         ("C-x g f" . magit-branch)
         ("C-x g b" . magit-blame))
  :commands (magit-toplevel)
  :config 
  (setq vc-follow-symlinks t)
  (setq version-control t)
  (setq magit-auto-revert-mode nil)
  (setq magit-module-sections-nested nil)
  (magit-auto-revert-mode -1)
  (put 'magit-diff-edit-hunk-commit 'disabled nil)
  (add-to-list 'magit-status-sections-hook 'magit-insert-modules t))

(use-package forge
  :ensure t
  :commands (forge-dispatch forge-configure))

(use-package gist
  :ensure t
  :commands (gist-region gist-buffer gist-list gist-region-private gist-buffer-private))

(use-package github-review
  :ensure t
  :commands (github-review-start
             github-review-forge-pr-at-point
             github-review-approve
             github-review-reject))

(use-package git-link
  :ensure t
  :bind (("C-c g l" . git-link)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; email (mu4e) configuration

(use-package emojify
  :ensure t
  :commands (global-emojify-mode)
  :delight emojify-mode
  :config
  (setq emojify-emoji-styles '(ascii unicode github))
  (setq emojify-display-style 'unicode)
  (setq emojify-company-tooltips-p t)
  (setq emojify-point-entered-behaviour 'echo))

(use-package message
  :mode ((".*mutt.*" . message-mode)
         ("/mutt" . message-mode))
  :config
  (bind-key "M-q" 'ignore message-mode-map)
  (setq-default message-citation-line-format "On %A, %B %d %Y, %T, %N wrote:\n")
  (setq-default message-citation-line-function 'message-insert-formatted-citation-line)
  (setq-default message-interactive t)
  (setq-default message-kill-buffer-on-exit nil)
  (setq-default message-send-mail-function 'message-send-mail-with-sendmail)
  (setq-default message-forward-as-mime nil)
  (setq-default message-fill-column 72)
  (setq-default message-cite-style message-cite-style-gmail)
  (add-to-list 'mm-discouraged-alternatives "text/html")
  (add-to-list 'mm-discouraged-alternatives "text/richtext")
  (set-face-attribute 'message-separator nil :background (face-attribute 'default :background nil)))

(use-package mu4e
  :ensure nil
  :bind (("C-c m m" . mu4e)
         ("C-c m d" . mu4e~headers-jump-to-maildir)
         ("C-c m b" . mu4e-headers-search-bookmark)
         ("C-c m c" . mu4e-compose-new))
  :commands (mu4e
             mu4e-compose-new
             mu4e-headers-jump-to-maildir
             mu4e-headers-search-bookmark
             mu4e-update-mail-and-index)
  :defines (mu4e-mail-view-actions mu4e-get-mail-command mu4e-compose-minor-mode-map)
  :init
  (add-hook 'mu4e-compose-mode-hook 'turn-off-hard-wrap)
  (add-hook 'mu4e-compose-mode-hook 'whitespace-cleanup)
  (add-hook 'mu4e-compose-mode-hook 'tychoish/set-up-message-mode-buffer)
  (defun tychoish/set-up-message-mode-buffer ()
    (setq-local use-hard-newlines t)
    (setq-local make-backup-files nil))

  (defun tychoish/set-maildir (maildir)
    (setq smtpmail-queue-dir (f-join maildir "queue" "cur"))
    (setq mu4e-mu-home (f-join maildir ".mu"))
    (setq mu4e-maildir maildir)
    (setq message-directory maildir)
    (setq message-auto-save-directory (f-join maildir "drafts"))
    (setq message-signature-directory (f-join maildir "tools" "signatures")))

  (defun tychoish/set-email-address (name address)
    (setq user-mail-address address)
    (setq message-signature-file address)
    (setq user-full-name name)
    (setq mu4e-compose-reply-to-address address)
    (setq mu4e-reply-to-address user-mail-address)
    (setq mail-host-address (s-replace-regexp ".*@" "" address))
    (setq message-sendmail-extra-arguments `("-a" ,address))
    (tychoish-change-email-body
       user-full-name
       user-mail-address)
    (message "mail: configured address [%s]" address))

  (defun tychoish/initialize-standard-mail-bookmarks ()
    "Add a standard/generic litst of bookmarks. Resets/removes all existing bookmarks."
    (setq mu4e-bookmarks nil)
    (add-to-list 'mu4e-bookmarks '("mime:image/*" "Messages with images" ?p))
    (add-to-list 'mu4e-bookmarks '("date:today..now" "Today's messages" ?t))
    (add-to-list 'mu4e-bookmarks '("date:7d..now" "This Week's messages" ?w))
    (add-to-list 'mu4e-bookmarks '("m:/inbox OR m:/prof" "Inbox (all)" ?i))
    (add-to-list 'mu4e-bookmarks '("m:/inbox OR flag:unread AND NOT (flag:trashed OR m:/sent OR m:/trash)" "all unread message" ?a))
    (add-to-list 'mu4e-bookmarks '("flag:unread AND NOT flag:trashed" "Unread messages (no RSS)" ?u))
    (add-to-list 'mu4e-bookmarks '("m:/inbox OR flag:unread AND NOT (OR m:/sent OR flag:trashed OR m:/trash)"
                                   "to read/process queue" ?q))
    (add-to-list 'mu4e-bookmarks '("m:/inbox OR m:/prof" "unread primary queues to file"?f))
    (add-to-list 'mu4e-bookmarks '("(NOT m:/inbox AND NOT m:/prof) AND flag:unread" "all sorted email" ?s)))

  (defun tychoish-change-email-body (name address)
    "change an email address on an extant mail buffer"
    (when (equal major-mode 'mu4e-compose-mode)
      (goto-char (point-min))
      (let ((new-from (concat "From: " name " <" address ">")))
        (while (re-search-forward "^From:.*$" nil t 1)
          (replace-match new-from nil nil)))))

  (defun compose-reply-wide-or-not-please-ask ()
    "Ask whether to reply-to-all or not."
    (interactive)
    (mu4e-compose-reply (yes-or-no-p "Reply to all?")))
  :config
  (bind-keys :map mu4e-compose-minor-mode-map
             ("R" . compose-reply-wide-or-not-please-ask)
             ("r" . mu4e-headers-mark-for-read))
  (bind-keys :map mu4e-headers-mode-map
             ("R" . compose-reply-wide-or-not-please-ask)
             ("C-r" . compose-reply-wide-or-not-please-ask)
             ("r" . mu4e-headers-mark-for-read)
             ("o" . mu4e-headers-mark-for-unread)
             ("u" . mu4e-headers-mark-for-unread)
             ("*" . mu4e-headers-mark-for-something)
             ("#" . mu4e-mark-resolve-deferred-marks)
             (";" . mu4e-mark-resolve-deferred-marks))

  (setq mail-imenu-generic-expression
        '(("Subject"  "^Subject: *\\(.*\\)" 1)
          ("Cc"       "^C[Cc]: *\\(.*\\)" 1)
          ("Bcc"      "^B[Cc]: *\\(.*\\)" 1)
          ("To"       "^To: *\\(.*\\)" 1)
          ("From"     "^From: *\\(.*\\)" 1)))

  (setq mu4e-compose-complete-addresses t)
  (setq mu4e-compose-complete-only-after "2015-01-01")
  (setq mu4e-compose-dont-reply-to-self t)
  (setq mu4e-compose-keep-self-cc nil)
  (setq mu4e-compose-signature t)
  (setq mu4e-drafts-folder "/drafts")
  (setq mu4e-headers-include-related nil)
  (setq mu4e-headers-results-limit 1000)
  (setq mu4e-maildir-shortcuts nil)
  (setq mu4e-sent-folder "/sent")
  (setq mu4e-trash-folder "/trash")
  (setq mu4e-user-agent-string nil)
  (setq mu4e-view-show-images t)

  (setq mail-signature t)
  (setq mail-specify-envelope-from t)
  (setq mail-user-agent 'mu4e-user-agent)
  (setq mml-secure-openpgp-sign-with-sender t)
  (setq mc-gpg-user-id (getenv "GPG_KEY_ID"))

  (setq compose-mail-user-agent-warnings nil)
  (setq sendmail-program "msmtp")
  (setq smtpmail-queue-mail nil)
  (setq mail-header-separator "--------------------------")

  (setq mu4e--header-separator (propertize mail-header-separator 'read-only t 'intangible t))
  (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)
  (tychoish/initialize-standard-mail-bookmarks))

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
   ((eq system-type 'darwin)
    (setq-default alert-default-style 'osx-notifier))
   (alert-libnotify-command
      (setq-default alert-default-style 'libnotify))
   ((eql system-type 'gnu/linux)
    (setq-default alert-default-style 'notifications))
   (t (setq-default alert-default-style 'message))))

(use-package ercn
  :ensure t
  :after (erc alert)
  :config
  (setq ercn-suppress-rules '((system . all)
                              (fool . all)
                              (dangerous-host . all)))

  (setq ercn-notify-rules '((current-nick . all)
                            (query-buffer . all)
                            (message . ("#unclear" "#general"))))

  (defun do-erc-notify (nickname message)
    "Hook implementation of a notification."
    (catch 'early-return
      (let* ((channel (buffer-name))
             (_check (when (or (string-prefix-p "*irc-" channel)
                              (string= "bot" nickname)
                              (search "bitlbee" (downcase channel)))
                      (throw 'early-return "skip notification noise")))
             (msg (s-trim (s-collapse-whitespace message)))
             (title (if (string-match-p (concat "^" nickname) channel)
                        nickname
                      (concat nickname " (" channel ")"))))
        (alert msg :title title))))

  (add-hook 'ercn-notify-hook 'do-erc-notify))

(use-package tracking
  :ensure t
  :after (telega erc))


(use-package telega-mnz
  :delight telega-mnz-mode
  :hook (telega-chat-mode . telega-mnz-mode))

(use-package telega
  :ensure t
  :delight telega-chat-auto-fill-mode
  :commands (telega
             telega-chat-mode
             tychoish/telega-switch-to-root
             tychoish/telega-kill-chat-buffers
             tychoish/telega-bury-chat-buffers
             tychoish/telega-force-kill)
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
  :config
  (setq telega-emoji-use-images t)

  (require 'telega-alert)

  (telega-mode-line-mode 1)
  (telega-alert-mode 1)

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

  (defun telega-chat-folders (chat) nil)
  (setq telega-chat-folders-insexp #'telega-folders-insert-default)

  (setq telega-use-images t)
  (setq telega-chat-input-markups '("markdown2"))
  (setq telega-server-libs-prefix "/usr")
  (setq telega-use-tracking-for '(or unmuted mention unread))
  (setq telega-markdown2-backquotes-as-precode t)
  (setq telega-debug nil)

  (with-eval-after-load 'company
    (setq telega-company-emoji-fuzzy-match t)
    (setq telega-company-username-show-avatars t))

  (setq telega-chat--display-buffer-action
        '((display-buffer-reuse-window display-buffer-use-some-window)))

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
    "returns 't' for all chat buffers, and nil otherwise"
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

  (defun tychoish/telega-switch-to-root ()
    (interactive)
    (switch-to-buffer (get-buffer telega-root-buffer-name)))

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
  :bind (("C-c d o" . deft)
         ("C-c d n" . tychoish-deft-create)
         ("C-c d d" . deft-find-file))
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
                              deft-extension)))
      (if (file-exists-p draft-file)
          (find-file draft-file)
        (find-file draft-file)
        (insert (title)))))
  :config
  (setq deft-directory (concat local-notes-directory "/deft"))
  (setq deft-extension "txt")
  (setq deft-text-mode 'markdown-mode)
  (setq deft-use-filename-as-title t)
  (setq deft-auto-save-interval 0)
  (setq deft-auto-save-interval nil))

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
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'latex-mode-hook 'turn-on-auto-fill))

(use-package markdown-mode
  :ensure t
  :delight (markdown-mode "mdwn")
  :mode ("\\.mdwn" "\\.md" "\\.markdown")
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
  :mode ("\\.rst" "\\.txt")
  :bind (:map rst-mode-map
	 ("C-c C-t h" . rst-adjust))
  :init
  (defalias 'rst-indent-code (kmacro "SPC SPC SPC C-a C-n"))
  (defun tychoish/rst-setup-mode ()
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
  (setq passive-voice t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; modes for programming languages other formats

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; programming major-modes

(use-package yaml-ts-mode
  :ensure nil
  :after (eglot treesit)
  :defer t
  :mode (("\\.yaml$" . yaml-ts-mode)
	 ("\\.yml$" . yaml-ts-mode))
  :init
  (defun tychoish/yaml-company-setup ()
    (when (and (featurep 'company) (not corfu-mode)))
      (setq-local company-backends
		  '((company-keywords company-dabbrev company-capf company-files
		                      :with company-yasnippet))))

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

  (add-hook 'yaml-ts-mode-hook 'tychoish/yaml-company-setup)
  (add-hook 'yaml-ts-mode-hook (tychoish/set-tab-width 2)))

(use-package go-ts-mode
  :ensure nil
  :delight
  (go-ts-mode "go.ts")
  (go-mod-ts-mode "go.mod.ts")
  (go-mode "go.ts")
  :mode (("\\.go$" . go-ts-mode)
         ("go.work" . go-mod-ts-mode)
         ("go.mod" . go-mod-ts-mode))
  :init
  (defun tychoish/go-mode-setup ()
    (setq-local tab-width 8)
    (setq-local fill-column 128))

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
  (unless (getenv "GOPATH")
    (setenv "GOPATH" (expand-file-name "~/go")))
  (setq local-go-bin (concat (getenv "GOPATH") "/bin"))
  (setq exec-path (cons local-go-bin exec-path))
  (setenv "PATH" (format "%s:%s" (getenv "PATH") local-go-bin ))
  (add-to-list 'exec-path local-go-bin))

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
  :delight
  (python-ts-mode "py.ts")
  (python-mode "py")
  :mode (("\\.py$" . python-ts-mode))
  :bind (:map python-ts-mode-map
         ("M-<right>" . balle-python-shift-right)
         ("M-<left>" . balle-python-shift-left)
         ("C-m" . py-newline-and-indent))
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
					  :lineLength 80
					  :format ["I"]
					  :extendSelect ["I"])
                          :flake8 (:enabled :json-false)
                          :rope (:enabled :json-false)
                          :pycodestyle (:enabled :json-false)
                          :mccabe (:enabled :json-false)
                          :autopep8 (:enabled :json-false)
                          :pyflakes (:enabled :json-false)
                          :pycodestyle (:enabled :json-false)))))

  (defun tychoish/python-setup ()
    (setq-local python-indent-offset 4)
    (setq-local tab-width 4)
    (setq-local fill-column 100))

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
  ; (setenv "PYTHON_KEYRING_BACKEND" "keyring.backends.null.Keyring")
  (font-lock-add-keywords 'python-ts-mode (font-lock-show-tabs))
  (font-lock-add-keywords 'python-ts-mode (font-lock-width-keyword 100)))

(use-package cython-mode
  :ensure t
  :mode (("\\.pyx\\'" . cython-mode)
         ("\\.pxd\\'" . cython-mode)
         ("\\.pxi\\'" . cython-mode)))

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

(use-package make-mode
  :ensure t
  :mode (("makefile" . makefile-mode)
         ("Makefile" . makefile-mode)
         ("\\.mk%" . makefile-mode))
  :config
  (setq makefile-electric-keys t))

(use-package just-mode
  :ensure t
  :mode (("justfile" . just-mode)
         ("justfile" . just-mode)
         ("\\.just%" . just-mode)))

(use-package conf-mode
  :ensure t
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
  :ensure t
  :mode (("\\.zsh$'" . sh-mode)
         ("\\.zshrc$'" . sh-mode)
         ("\\.bash_profile$'" . sh-mode)))

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

(use-package emacs-lisp-mode
  :mode (("\\.el$" . emacs-lisp-mode))
  :config
  (delight 'emacs-lisp-mode '("el" (lexical-binding ":l" ":d")) :major)
  (setq checkdoc-force-docstrings-flag nil)
  (setq checkdoc-spellcheck-documentation-flag t))

(use-package solidity-mode
  :ensure t
  :mode ("\\.sol$'" . solidity-mode)
  :config
  (setq solidity-comment-style 'slash))

(use-package solidity-flycheck
  :ensure t
  :hook (solidity-mode . tychoish/solidity-flycheck-setup)
  :commands (solidity-flycheck)
  :config
  (defun tychoish/solidity-flycheck-setup ()
    (setq-local solidity-flycheck-solc-checker-active t))

  (add-to-list 'flycheck-checkers 'solidity-flycheck))

(use-package slime
  :load-path "~/quicklisp/dists/quicklisp/software/slime-v2.31/"
  :after (f lisp-mode)
  :delight
  (lisp-mode "lisp")
  (slime-mode "sl")
  (slime-autodoc-mode "")
  :mode ("\\.lisp" . lisp-mode)
  :bind ("C-c d c" . hyperspec-lookup)
  :commands (slime slime-connect)
  :config
  (setq ls-lisp-dirs-first t)
  (setq inferior-lisp-program "sbcl"))

(use-package comint
  :defer t
  :bind (:map comint-mode-map
              ("M-n" . comint-next-input)
              ("M-p" . comint-previous-input)
              ([down] . comint-next-matching-input-from-input)
              ([up] . comint-previous-matching-input-from-input))
  :commands (comint-mode comint-run)
  :config
  (setq ansi-color-for-comint-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; programming adjacent tools

(use-package shell-pop
  :ensure t
  :bind (("<f9>" . shell-pop))
  :commands (shell-pop)
  :config
  (setq shell-pop-universal-key "<f9>")
  (setq shell-pop-window-position "top")
  (setq shell-pop-window-size 25)
  (setq shell-pop-window-size 25)
  (setq shell-pop-autocd-to-working-dir t)
  (setq shell-pop-cleanup-buffer-at-process-exit t))

(use-package journalctl-mode
  :ensure t
  :bind (("C-c t j" . journalctl)))

(use-package docker
  :ensure t
  :commands (docker)
  :bind ("C-c C-d" . docker))

(use-package lpr
  :ensure t
  :commands (lpr-region lpr-buffer)
  :config
  (setq lpr-add-switches "-T ''"))

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
  (setq flycheck-keymap-prefix (kbd "C-c f"))
  ;; the order of the following 3 operations is important.
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map)
  (setq flycheck-keymap-prefix (kbd "C-c f"))

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
  :config
  (add-to-list 'flycheck-checkers 'golangci-lint)
  (setq flycheck-go-vet-shadow t)
  (setq flycheck-go-build-install-deps t)
  (setq flycheck-golangci-lint-fast t)
  (setq flycheck-golangci-lint-tests t))

(use-package compile
  :functions (tychoish-uniq-compile-buffer)
  :bind (("C-c t c" . tychoish-compile-project-build)
         ("C-c t l" . tychoish-compile-project-golang-lint)
         ("C-c C-t c" . compile)
	 :map compilation-mode-map
	 ("C" . compile))
  :commands (compile
             tychoish-compile-project-build
             tychoish-compile-project-golang-lint
             tychoish-compile-project-super-lint
             tychoish-compile-project-build-tests)
  :config
  (setq-default compilation-save-buffers-predicate #'tychoish/guess-compilation-root)

  (defun tychoish/guess-compilation-root ()
    (if-let* ((project-root (projectile-project-root)))
        project-root
      default-directory))

  (defun compile-add-error-syntax (name regexp file line &optional col level)
    "Register new compilation error syntax."
    (add-to-list 'compilation-error-regexp-alist-alist (list name regexp file line col level))
    (add-to-list 'compilation-error-regexp-alist name))

  (compile-add-error-syntax 'rust-pretty-logfile "^\s+ at \\(.*\\):\\([0-9]+\\)" 1 2)

  (defun colorize-compilation-buffer ()
    (ansi-color-apply-on-region compilation-filter-start (point)))

  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

  (defun tychoish-compile-project-build ()
    (interactive)
    (tychoish-compile-project "build" "time make -k build"))

  (defun tychoish-compile-project-build-tests ()
    (interactive)
    (tychoish-compile-project "build-test" "go test ./... -run=NOOP"))

  (defun tychoish-compile-project-golang-lint ()
    (interactive)
    (tychoish-compile-project "lint" "golangci-lint run --allow-parallel-runners"))

  (defun tychoish-compile-gotests ()
    (interactive)
    (tychoish-compile-project "go-test" "go list -f '{{ if (or .TestGoFiles .XTestGoFiles) }}{{ .ImportPath }}{{ end }}' ./... | xargs -t go test -race -v"))

  (defun tychoish-compile-project-super-lint ()
    (interactive)
    (let* ((project-directory (if (eq "" (projectile-project-root))
                                  (default-directory)
                                (projectile-project-root)))
           (options (list "VALIDATE_YAML=true"
                          "VALIDATE_OPENAPI=true"
                          "VALIDATE_MD=true"
                          "MARKDOWN_CONFIG_FILE=.markdownlint.yml"
                          "VALIDATE_ALL_CODEBASE=true"
                          "LINTER_RULES_PATH=."
                          "RUN_LOCAL=true"))
           (optstr (format "-e %s" (s-join " -e " options)))
           (command-string (format "docker run %s -v %s:/tmp/lint github/super-linter" optstr project-directory)))
      (tychoish-compile-project "super-lint" command-string)))

  (defun tychoish-compile-project (name cmd)
    (let* ((project-directory (if (eq "" (projectile-project-root))
                                  (default-directory)
                                (projectile-project-root)))
           (project-name (if (eq "" (projectile-project-name))
                             (file-name-nondirectory (s-chop-suffix "/" project-directory))
                           (projectile-project-name)))
           (project-compile-buffer (concat "*" project-name "-" name "*")))
      (if (get-buffer project-compile-buffer)
          (switch-to-buffer-other-window (get-buffer project-compile-buffer))
        (progn
          (let ((default-directory project-directory))
            (compile cmd))
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
    (setq grammarly-on- message-function-list (remove 'flycheck-grammarly--on-message 'grammarly-on-open-function-list))
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

(use-package rust-compile
  :after (rustic-mode))

(use-package rust-playground
  :ensure t
  :commands (rust-playground rust-playground-run-command))

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

(use-package virtualenvwrapper
  :ensure t
  :commands (venv-workon venv-deactivate venv-initialize-eshell venv-initialize-interactive-shells))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; language server protocol (lsp) [eglot] + treesitter
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eglot
  :ensure nil
  :defines (eglot-mode-map)
  :hook (((js-mode
           js-ts-mode
           typescript-ts-mode
	   go-ts-mode
	   go-mod-ts-mode
	   yaml-mode
	   yaml-ts-mode
	   rust-mode
	   rust-ts-mode
	   rustic-mode
	   python-mode
	   python-ts-mode
	   go-mode
	   go-mod-mode
           bash-ts-mode
           bash-mode
           sh-mode) . #'eglot-ensure))
  :bind (("C-c l l s" . eglot)
         ("C-c l l l" . eglot-list-connections)
         ("C-c l l g" . eglot-forget-pending-continuations)
         :map eglot-mode-map
         ("C-c l l r" . eglot-reconnect)
         ("C-c l l k" . eglot-shutdown)
         ("C-c l r" . eglot-rename)
         ("C-c l m" . imenu)
         ("C-c l f" . eglot-format)
         ("C-c l a" . eglot-code-actions)
         ("C-c l o" . eglot-code-action-organize-imports)
       ; ("C-c l i" . eglot-code-action-inline)
         ("C-c l e" . eglot-code-action-extract)
         ("C-c l w" . eglot-code-action-rewrite)
         ("C-c l q" . eglot-code-action-quickfix)
         ("C-c l c" . eglot-call-type-hierarchy)
         ("C-c l t" . eglot-show-type-hierarchy))
  :init
  (add-hook 'eglot-managed-mode-hook #'tychoish/eglot-ensure-hook)
  (defun tychoish/eglot-ensure-hook ()
    (setq-local eldoc-docuemntation-stratedgy 'eldoc-documentation-compose-eagerly)
    ;; toggle it on and off so that the left-fringe isn't weird.
    (flycheck-eglot-mode -1)
    (flycheck-eglot-mode 1))
  :config
  (setq eglot-menu-string "eg")
  (when (and (featurep 'helm) helm-mode
	     (not (or (featurep 'vertico)
		      vertico-mode)))
    (bind-keys :map eglot-mode-map
               ("C-c l m" . helm-imenu)))

  (setq eglot-autoshutdown t)
  (setq eglot-extend-to-xref t)

  (add-to-list 'eglot-stay-out-of 'flymake)
  (add-to-list 'eglot-stay-out-of 'company)

  (add-hook 'before-save-hook 'eglot-organize-imports)
  (add-hook 'before-save-hook 'eglot-format-for-hook)

  (add-to-list 'eglot-server-programs
               `((go-mode go-dot-mod-mode go-dot-work-mode go-ts-mode go-mod-ts-mode)
                 . ,(eglot-alternatives
                     `(("gopls-systemd" ,(format "-remote=unix;/run/user/%d/gopls.socket" (user-uid)))
                       ("gopls-auto" "-remote=auto")
                       ("gopls")))))

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

(use-package xref
  :bind (("M-." . xref-find-definitions)
         ("C-c l c" . xref-find-references)
         ("C-c l d" . xref-find-definitions)
         ("C-c l p" . xref-go-back)
         ("C-c l n" . xref-go-forward)
         ("C-c l o" . xref-find-definitions-other-window)))

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
         ("\\.sol\\'" . solidity-ts-mode)
         ("\\.json\\'" . json-ts-mode)
         ("\\.toml\\'" . toml-ts-mode)
         ;; -- c/c++
         ("\\.cpp\\'" . c++-ts-mode)
         ("\\.cc\\'" . c++-ts-mode)
         ("\\.hh\\'" . c++-ts-mode)
         ("\\.cxx\\'" . c++-ts-mode)
         ("\\.h\\'" . c-or-c++-ts-mode)
         ("\\.c\\'" . c-ts-mode)
         ("CMakeLists.txt" . cmake-ts-mode)
         ;; -- jvm
         ("\\.java\\'" . java-ts-mode)
         ("\\.scala\\'" . scala-ts-mode)
         ("\\.sc\\'" . scala-ts-mode)
         ("\\.kt\\'" . kotlin-ts-mode)
         ("\\.kts\\'" . kotlin-ts-mode)
         ("\\.kdl\\'" . kdl-ts-mode))
  :init
  (add-hook 'js-ts-mode-hook (create-run-hooks-function-for js-mode))
  (add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode))
  (add-to-list 'major-mode-remap-alist '(jav-mode . js-ts-mode))
  (add-to-list 'major-mode-remap-alist '(css-mode . css-ts-mode))
  (add-to-list 'major-mode-remap-alist '(js-json-mode . json-ts-mode))

  (add-hook 'c++-ts-mode-hook (create-run-hooks-function-for c++-mode))
  (add-hook 'c-ts-mode-hook (create-run-hooks-function-for c-mode))
  (add-to-list 'major-mode-remap-alist '(c-mode . c-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-mode))
  (add-to-list 'major-mode-remap-alist '(c-or-c++-ts-mode . c-or-c++-mode))
  :config
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
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (java "https://github.com/tree-sitter/tree-sitter-java")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (kotlin "https://github.com/fwcd/tree-sitter-kotlin")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (regex "https://github.com/tree-sitter/tree-sitter-regex")
          (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (scala "https://github.com/tree-sitter/tree-sitter-scala")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (kdl "https://github.com/amaanq/tree-sitter-kdl")
          (solidity "https://github.com/JoranHonig/tree-sitter-solidity")
          (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

  (defun tychoish-background-rebuild-treesit-bindings ()
    (interactive)
    (async-start
     `(lambda ()
        ,(async-inject-variables "treesit-language-source-alist")
        (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)))
     (lambda (result)
       (message "rebuilt treesit grammars for %s" result)))))

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
  :bind (("C-c r a g" . gptel)
         ("C-c r a r" . gptel-rewrite)
         :map gptel-mode-map
         ("C-c r a m c" . tychoish-gptel-copilot)
         ("C-c r a m C" . tychoish-gptel-copilot-default)
         ("C-c r a m g" . tychoish-gptel-gemini)
         ("C-c r a m G" . tychoish-gptel-copilot-default))
  :commands gptel
  :config
  (setq tychoish--gemini-backend (gptel-make-gemini "gemini" :key google-gemini-key :stream t))
  (setq tychoish--gemini-model 'gemini-2.5-pro-preview-06-05)
  (setq tychoish--copilot-backend (gptel-make-gh-copilot "copilot"))
  (setq tychoish--copilot-model 'claude-3.5-sonnet)

  (defun tychoish-gptel-copilot ()
    (interactive)
    (setq-local gptel-model tychoish--copilot-model)
    (setq-local gptel-backend tychoish--copilot-backend))

  (defun tychoish-gptel-copilot-default ()
    (interactive)
    (setq-default gptel-model tychoish--copilot-model)
    (setq-default gptel-backend tychoish--copilot-backend))

  (defun tychoish-gptel-gemini ()
    (interactive)
    (setq-local gptel-model tychoish--gemini-model)
    (setq-local gptel-backend tychoish--gemini-backend))

  (defun tychoish-gptel-gemini-default ()
    (interactive)
    (setq-default gptel-model tychoish--gemini-model)
    (setq-default gptel-backend tychoish--gemini-backend))

  (tychoish-gptel-gemini-default)
  (require 'gptel-integrations))

(use-package gptel-aibo
  :ensure t
  :bind (("C-c r a w" . gptel-aibo-summon))
  :commands (gptel-aibo-summon gptel-aibo))

(use-package mcp
  :ensure t
  :commands (mcp-hub-start-all-servers)
  :config
  (add-to-list 'mcp-hub-servers '("time" . (:command "uvx" :args ("mcp-server-time"))))
  (add-to-list 'mcp-hub-servers '("fetch" . (:command "uvx" :args ("mcp-server-fetch"))))
  (add-to-list 'mcp-hub-servers '("git" . (:command "uvx" :args ("mcp-server-git"))))
  (require 'mcp-hub))

(use-package copilot-chat
  :ensure t
  :bind (("C-c r c m" . copilot-chat-transient)
         ("C-c r c t" . copilot-chat))
  :config
  (setq copilot-chat-frontend 'markdown)
  (setq copilot-chat-follow t)
  (setq copilot-chat-markdown-prompt "##"))

(use-package copilot
  :ensure t
  :bind (("C-c r c s" . copilot-complete)
         ("C-c r c g" . copilot-clear-overlay)
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

(provide 'tychoish-core)
;;; tychoish-core.el ends here
