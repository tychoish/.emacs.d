;;; tychoish-core -- contains use-package forms for managing configuration.

;;; Commentary:

;; Provides my collection of use-package forms and related
;; configuration for loading and configuring all Emacs packages.

;; This configuration optimizes for lazy-loading so that configuration
;; only loads when called directly or a mode is activated.

;;; Code:

(use-package async
  :ensure t
  :commands (async-start async-start-process)
  :after (dired)
  :config
  (async-bytecomp-package-mode t)
  (dired-async-mode 1))

(use-package dired
  :ensure nil
  :config
  (define-key dired-mode-map (kbd "r") 'wdired-change-to-wdired-mode))

(use-package auto-package-update
  :commands (auto-package-update-maybe auto-package-update-now)
  :ensure t
  :config
  (setq auto-package-update-hide-results t
        auto-package-update-interval 9001)
  (auto-package-update-maybe))

(use-package use-package-ensure-system-package
  :ensure t
  :after (use-package))

(use-package eldoc
  :defer t
  :ensure t
  :diminish
  :commands (eldoc-mode kill-eldoc-and-help-buffers)
  :bind (("C-c d e" . eldoc-doc-buffer)
         ("C-c d q" . kill-eldoc-and-help-buffers))
  :config
  (defun kill-eldoc-and-help-buffers ()
    "Kills all eldoc and help buffers"
    (interactive)
    (kill-matching-buffers "\*Help\\*\\|*eldoc.*\\*" nil t))

  (defun jump-to-elisp-help ()
    (interactive)
   (apropos-docum (symbol-name (intern-soft (thing-at-point 'symbol))))))

(use-package diminish
  :ensure t
  :commands (diminish))

(use-package delight
  :ensure t
  :commands (delight))

(use-package autorevert
  :commands (auto-revert-mode)
  :ensure t
  :init
  (defalias 'rb 'revert-buffer)
  (defalias 'revert 'revert-buffer)
  :config
  (diminish 'auto-revert-mode)
  (setq auto-revert-verbose nil)
  (setq auto-revert-avoid-polling t)
  (setq auto-revert-interval 60))

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
  :commands (f-exists? f-join f-glob f-expand))

(use-package s
  :ensure t
  :commands (s-contains?))

(use-package dash
  :ensure t
  :commands (--remove))

(use-package anzu
  :ensure t
  :diminish
  :commands (anzu-query-replace anzu-query-replace-regexp global-anzu-mode anzu-mode)
  :hook ((isearch-mode) . anzu-mode)
  :bind (("C-c q r" . anzu-query-replace)
         ("C-c q x" . anzu-query-replace-regexp))
  :config
  (setq query-replace-highlight t)
  (setq search-highlight t)
  (defalias 'srr 'string-replace-regexp)
  (defalias 'sr 'string-replace)
  (defalias 'qrr 'anzu-query-replace-regexp)
  (defalias 'qr 'anzu-query-replace))

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
                   ;; '(vc-mode vc-mode)
                   "%M"
                   global-mode-string
                   ""
                   mode-line-modes)))

  (defun tychoish-setup-modeline ()
    (interactive)
    (doom-modeline-def-segment misc-info
      '("" mode-line-misc-info))

    (doom-modeline-mode 1)

    (delight 'emacs-lisp-mode "el")
    (delight 'fundamental-mode "fund")

    (diminish 'abbrev-mode)

    (diminish 'auto-fill-function "afm")
    (diminish 'overwrite-mode "om")
    (diminish 'refill-mode "rf")
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
  (add-to-list 'mode-line-misc-info (format "[%s]" tychoish-emacs-identifier)))

(use-package esup
  :ensure t
  :commands (esup))

(use-package modus-themes
  :ensure t
  :defer t
  :init
  (setq modus-themes-deuteranopia t)
  (setq modus-themes-common-palette-overrides
        '((border-mode-line-active bg-mode-line-active)
          (border-mode-line-inactive bg-mode-line-inactive))))

;; (use-package modus-themes-exporter
;;   :after modus-themes
;;   :commands (modus-themes-exporter-export))

(use-package winum
  :ensure t
  :after (doom-modeline)
  :commands (winum-select-window-by-number winum-mode)
  :bind (("C-x w n" . winum-select-window-by-number))
  :config
  (setq winum-auto-setup-mode-line nil)
  (setq winum-scope 'frame-local)
  (winum-mode 1))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :after (helm)
  :commands (which-key-mode)
  :config
  (setq which-key-idle-delay .4)
  (setq which-key-idle-secondary-delay 0.2)
  (which-key-setup-minibuffer))

(use-package helm
  :ensure t
  :after (tychoish-bootstrap)
  :diminish
  :commands (helm-mode)
  :bind (
         ;; most interesting helm menus are under one prefix
         ("C-c h a" . helm-apropos)
         ("C-c h d" . helm-info)
         ("C-c h i" . helm-imenu)
         ("C-c h k" . helm-regist)
         ("C-c h l" . helm-locate)
         ("C-c h m" . helm-man-woman)
         ("C-c h o" . helm-occur)
         ("C-c h p" . helm-browse-project)
         ("C-c h r" . helm-recentf)
         ("C-c h r" . helm-top)
         ("C-c h t" . helm-etags-select)
         ("C-c h g" . helm-google-suggest)
         ("C-c h w" . helm-buffers-list)
         ("C-c h y" . helm-show-kill-ring)

         ;; defined elsewhered :
         ;; ("C-c h c" . helm-company)
         ;; ("C-c h b" . helm-make-projectile)
         ;; ("C-c h n" . helm-make-projectile)
         ;; ("C-c h f" . helm-flycheck)
         ;; ("C-c h e" . helm-flyspell-correct)
         ;; ("C-c h s" . helm-swoop)

         ;; helm-native developer operations
         ("C-x r h" . helm-register)
         ("M-y" . helm-show-kill-ring)

         ;; helm alternatives for common standard operations
         ("C-x C-f" . helm-find-files)
         ("C-x f" . helm-for-files)
         ("C-x C-d" . helm-browse-project)
         ("C-x d" . helm-browse-project)

         ;; change buffers; mini is
         ("C-x b" . helm-mini)
         ("C-x C-b" . helm-mini)

         ;; command interface
         ("M-x" . helm-M-x)
         ("C-x m" . helm-M-x)
         ("C-x C-m" . helm-M-x)

         :map helm-map
         ("TAB" . helm-execute-persistent-action)
         ("C-j" . helm-select-action))
  :config
  (set-face-attribute 'helm-source-header nil :height 0.98 :family "Source Code Pro" :weight 'semibold :background 'unspecified)
  (helm-autoresize-mode 1)
  (helm-mode 1)

  (setq history-delete-duplicates t)
  (setq history-length 250)

  (setq helm-M-x-fuzzy-match t)
  (setq helm-move-to-line-cycle-in-source nil)
  (setq helm-autoresize-max-height 40)
  (setq helm-autoresize-min-height 20)
  (setq helm-autoresize-mode nil)
  (setq helm-c-adaptive-history-file (tychoish-get-config-file-path "helm-c-adaptive-history"))
  (setq helm-c-adaptive-sorting t)
  (setq helm-c-source-kill-ring '())
  (setq helm-candidate-number-limit 250)
  (setq helm-case-fold-search t)
  (setq helm-display-header-line nil)
  (setq helm-ff-cache-mode-max-idle-time 300)
  (setq helm-ff-keep-cached-candidates "local")
  (setq helm-ff-keep-cached-candidates nil)
  (setq helm-ff-refresh-cache-delay 300)
  (setq helm-grep-ag-command "rg --color=always --colors 'match:fg:black' --colors 'match:bg:yellow' --smart-case --no-heading --line-number %s %s %s")
  (setq helm-grep-ag-pipe-cmd-switches '("--colors 'match:fg:black'" "--colors 'match:bg:yellow'"))
  (setq helm-input-idle-delay 0)
  (setq helm-man-or-woman-function 'woman)
  (setq helm-split-window-in-side-p t))

(use-package helm-company
  :ensure t
  :after (helm company)
  :bind (("C-c h c" . helm-company))
  :commands (helm-company)
  :init
  (define-key company-mode-map (kbd "C-,") 'helm-company)
  (define-key company-active-map (kbd "C-,") 'helm-company))

(use-package helm-flycheck
  :ensure t
  :after (helm flycheck)
  :commands (helm-flycheck)
  :bind (("C-c f d" . helm-flycheck)
         ("C-c h f" . 'helm-flycheck)))

(use-package helm-make
  :ensure t
  :bind (("C-c h b" . helm-make-projectile)
         ("C-c h n" . helm-make))
  :config
  (setq helm-make-named-buffer t)
  (setq helm-make-fuzzy-matching t)
  (setq helm-make-cache-targets t)
  (setq helm-make-do-save t)
  (setq helm-make-sort-targets t))

(use-package helm-swoop
  :ensure t
  :bind (("M-m" . helm-swoop)
         ("C-c M-s" . helm-multi-swoop)
         ("C-x M-s" . helm-multi-swoop-all)
         ("C-c o s" . helm-multi-swoop-org)
         ("C-c h s" . helm-swoop)
         ("M-s" . helm-swoop)
         ("M-M" . helm-swoop-back-to-last-point))
  :init
  (bind-key "M-m" 'helm-swoop-from-isearch isearch-mode-map)
  :config
  (setq helm-swoop-split-with-multiple-windows t)
  (setq helm-swoop-split-direction 'split-window-vertically))

(use-package helm-eww
  :ensure t
  :bind (("C-c w o" . helm-eww)))

(use-package helm-projectile
  :ensure t
  :after (projectile helm)
  :bind (("M-p" . helm-projectile)
         ("C-c s a" . helm-projectile-ag)
         ("C-c s r" . helm-projectile-rg)
         ("C-c s g" . helm-projectile-grep))
  :config
  (helm-projectile-on))

(use-package helm-c-yasnippet
  :ensure t
  :after (yasnippet helm)
  :init
  (setq helm-yas-space-match-any-greedy t))

(use-package helm-rg
  :ensure t
  :bind (("C-c r s" . helm-rg)
         ("C-c r r" . helm-projectile-rg))
  :ensure-system-package ((rg . ripgrep))
  :config
  (set-face-attribute 'helm-rg-error-message nil :foreground "pink4" :background nil :weight 'normal)
  (set-face-attribute 'helm-rg-active-arg-face nil :foreground "olive drab")
  (set-face-attribute 'helm-rg-base-rg-cmd-face nil :foreground "dim gray")
  (set-face-attribute 'helm-rg-directory-cmd-face nil :foreground "brown")
  (set-face-attribute 'helm-rg-directory-header-face nil :foreground nil :weight 'extra-bold)
  (set-face-attribute 'helm-rg-extra-arg-face nil :foreground "yellow4")
  (set-face-attribute 'helm-rg-file-match-face nil :foreground "#088")
  (set-face-attribute 'helm-rg-inactive-arg-face nil :foreground "dim gray")
  (set-face-attribute 'helm-rg-title-face nil :foreground "purple" :weight 'bold))

(use-package ripgrep
  :ensure t
  :commands (projectile-ripgrep ripgrep-regexp)
  :bind (("C-c r g" . tychoish-rg)
         ("C-c r p" . tychoish-rg-repo)
         ("C-c r m" . tychoish-find-merges))
  :init
  (setenv "RIPGREP_CONFIG_PATH" (f-expand "~/.ripgreprc"))
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

(use-package eww
  :ensure t
  :bind (("C-c w d" . browse-url-generic)
         ("C-c w e" . browse-url)
         ("C-c w f" . browse-url-firefox)
         ("C-c w c" . browse-url-chrome)
         ("C-c w g" . eww-search-words))
  :init
  (setq browse-url-browser-function 'eww-browse-url)
  (setq browse-url-generic-program "chrom")
  (setq shr-color-visible-luminance-min 80)
  (setq shr-use-colors nil)
  (setq shr-use-fonts nil)
  :config
  (setq eww-search-prefix "https://www.google.com/search?q="))

(use-package compile
  :functions (tychoish-uniq-compile-buffer)
  :commands (compile
             tychoish-compile-project-build
             tychoish-compile-project-golang-lint
             tychoish-compile-project-super-lint
             tychoish-compile-project-build-tests)
  :bind (("C-c t c" . tychoish-compile-project-build)
         ("C-c t l" . tychoish-compile-project-golang-lint)
         ("C-c C-t c" . compile))
  :config
  (defun compile-add-error-syntax (name regexp file line &optional col level)
    "Register new compilation error syntax."
    (add-to-list 'compilation-error-regexp-alist-alist (list name regexp file line col level))
    (add-to-list 'compilation-error-regexp-alist name))

  (compile-add-error-syntax 'rust-pretty-logfile "^\s+ at \\(.*\\):\\([0-9]+\\)" 1 2)

  (setq compilation-ask-about-save nil)
  (setq compilation-scroll-output t)

  (defun colorize-compilation-buffer ()
    (ansi-color-apply-on-region compilation-filter-start (point)))

  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

  (define-key compilation-mode-map (kbd "C") 'compile)

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

(use-package wgrep
  :ensure t
  :bind (:map grep-mode-map
         ("r" . wgrep-change-to-wgrep-mode))
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
                                 c-ts-mode
                                 c-mode
                                 cc-mode
                                 cc-ts-mode
                                 eww-mode
                                 go-ts-mode
                                 special-mode)))

(use-package writeroom-mode
  :ensure t
  :bind (("C-c t i" . writeroom-mode))
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
  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

  (setq session-save-print-spec '(t nil 40000)))

(use-package winner
  :ensure t
  :commands (winner-mode winner-undo winner-redo)
  :hook ((fundamental-mode) . winner-mode))

(use-package desktop
  :ensure t
  :commands (desktop-save-mode desktop-read tychoish-save-desktop)
  :after (tychoish-bootstrap)
  :config
  (setq desktop-load-locked-desktop t)
  (setq desktop-dirname user-emacs-directory)
  (setq desktop-base-file-name (tychoish-get-config-file-prefix "desktop-file"))
  (setq desktop-base-lock-name (convert-standard-filename (tychoish-get-config-file-prefix (format "desktop-lock-%d" (emacs-pid)))))

  ;; use session-save to save the desktop manually
  (defun tychoish-save-desktop ()
    "Save an emacs session... sometimes"
    (interactive)

    (when (> 50 (random 100))
        (desktop-save desktop-dirname)
        (desktop-read)))

  (add-hook 'after-save-hook 'tychoish-save-desktop)
  (setq ad-redefinition-action 'accept)
  (defun emacs-process-p (pid)
    "If pid is the process ID of an emacs process, return t, else
nil. Also returns nil if pid is nil."
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
                "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS\\|"
                "\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
                "\\)$"))

  (setq desktop-files-not-to-save
        (concat "\\(\\`/[^/:]*:\\|(ftp)\\'\\)" ;; default
                "^/usr/lib/go/.*\\|"
                "^/usr/lib/rustlib/.*\\|"
                "^/home.+go/pkg/mod\\|"
                "^/home.+\\.cargo"))

  (add-to-list 'desktop-globals-to-save 'register-alist)
  (add-to-list 'desktop-globals-to-save 'file-name-history)
  (add-to-list 'desktop-modes-not-to-save 'dired-mode)
  (add-to-list 'desktop-modes-not-to-save 'Info-mode)
  (add-to-list 'desktop-modes-not-to-save 'org-mode)
  (add-to-list 'desktop-modes-not-to-save 'eww-mode)
  (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
  (add-to-list 'desktop-modes-not-to-save 'fundamental-mode))

(use-package recentf
  :ensure t
  :commands (recentf-mode recentf-open)
  :after (helm)
  :bind (("C-c h r" . helm-recentf))
  :config
  (setq recentf-auto-cleanup 'never)
  (setq recentf-keep '(file-remote-p file-readable-p))
  (setq recentf-max-menu-items 100)
  (setq recentf-save-file (tychoish-get-config-file-path "recentf")))

(use-package magit
  :ensure t
  :commands (magit-toplevel)
  :bind (("C-x g s" . magit-status)
         ("C-x g f" . magit-branch)
         ("C-x g b" . magit-blame))
  :init
  (setq version-control t)
  (setq vc-follow-symlinks t)
  (setq vc-handled-backends nil)

  :config
  (put 'magit-diff-edit-hunk-commit 'disabled nil)
  (setq magit-auto-revert-mode nil)
  (add-to-list 'magit-status-sections-hook 'magit-insert-modules t)
  (setq magit-module-sections-nested nil))

(use-package forge
  :ensure t
  :after (magit magithub)
  :defer t)

(use-package vc
  :defer t
  :init
  ;; Disable VC entirely
  (setq vc-handled-backends ())
  ;; Don't ask me again
  (setq vc-follow-symlinks t))

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
  (setq yas-prompt-functions '(helm-yas-complete yas-dropdown-prompt yas-ido-prompt))

  (diminish 'yas-minor-mode "ys")
  (add-hook 'org-mode-hook (lambda ()
                             (make-local-variable 'yas-trigger-key)
                             (setq yas-trigger-key [tab])
                             (define-key yas-keymap [tab] 'yas-next-field)))
  (yas-reload-all))


(use-package helm-c-yasnippet
  :ensure t
  :after yasnippet
  :bind (("C-c s y" . 'helm-yas-complete)
         ("C-c C-y" . 'helm-yas-complete)))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

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
                           company-emoji
                           company-yasnippet

                           company-etags
                           company-wordfreq
                           company-elisp
                           company-files
                           company-cmake))

  (global-company-mode))

(use-package company-emoji
  :ensure t
  :after company)

(use-package company-wordfreq
  :ensure t
  :after company
  :autoload company-wordfreq)

(use-package flycheck
  :ensure t
  :diminish (flycheck-mode . "fc")
  :bind (("C-c f f" . flycheck-mode))
  :init
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11")))
  :config
  ;; the order of the following 3 operations is important.
  (define-key flycheck-mode-map flycheck-keymap-prefix nil)
  (setq flycheck-keymap-prefix (kbd "C-c f"))
  (define-key flycheck-mode-map flycheck-keymap-prefix flycheck-command-map)

  (setq flycheck-check-syntax-automatically '(save new-line idle-change idle-buffer-switch))
  (setq flycheck-idle-change-delay 1)
  (setq flycheck-idle-buffer-switch-delay 1)
  (setq flycheck-checker-error-threshold nil)
  (setq flycheck-display-errors-delay 0.5)
  (setq flycheck-flake8-maximum-line-length 100))

(use-package flycheck-golangci-lint
  :ensure t
  :after (go-ts-mode flycheck)
  :config
  (setq flycheck-go-vet-shadow t)
  (setq flycheck-go-build-install-deps t)
  (setq flycheck-go-vet-print-functions t)
  (setq flycheck-golangci-lint-fast t)
  (setq flycheck-golangci-lint-tests t)
  (flycheck-golangci-lint-setup))

(use-package go-ts-mode
  :ensure nil
  :delight "go"
  :mode (("\\.go$" . go-ts-mode)
         ("\\.go" . go-ts-mode)
         ("\\.go\\'" . go-ts-mode))
  :init
  (unless (getenv "GOPATH")
    (setenv "GOPATH" (expand-file-name "~/go")))

  (setq local-go-bin (concat (getenv "GOPATH") "/bin"))
  (setq exec-path (cons local-go-bin exec-path))
  (setenv "PATH" (format "%s:%s" (getenv "PATH") local-go-bin ))
  (add-to-list 'exec-path local-go-bin)

  :config
  (add-hook 'go-ts-mode-hook 'flycheck-mode)
  (add-hook 'go-ts-mode-hook
            (lambda ()
              (setq-local comment-auto-fill-only-comments t)
              (auto-fill-mode 1))))

(use-package cargo
  :ensure t
  :after (rustic)
  :config
  (setq cargo-process--command-fmt "fmt --all")
  (add-hook 'rust-ts-mode-hook 'cargo-minor-mode))

(use-package rustic
  :ensure t
  :delight "rs"
  :mode "\\.rs$'"
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  (setq rust-format-on-save t)
  (setq rustic-lsp-client 'eglot)

  (defun rk/rustic-mode-hook ()
    ;; so that run C-c C-c C-r works without having to confirm, but don't try to
    ;; save rust buffers that are not file visiting. Once
    ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
    ;; no longer be necessary.
    (when buffer-file-name
      (setq-local buffer-save-without-query t)))

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(use-package rust-compile
  :after (rustic))

(use-package rust-playground
  :ensure t
  :commands (rust-playground rust-playground-run-command))

(use-package protobuf-mode
  :ensure t
  :mode "\\.proto$'")

(use-package cmake-project
  :ensure t
  :after (cmake-ts-mode f c-mode)
  :init
  (defun tychoish-cmake-project-p ()
    (let* ((project-directory (if (eq "" (projectile-project-root))
                                  (default-directory)
                                (projectile-project-root))))
           (f-exists? (f-join project-directory "CMakeLists.txt"))))

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
              (eq major-mode 'c-mode)
              (eq major-mode 'c-ts-mode)
              (eq major-mode 'c++-ts-mode))
      (clang-format-buffer)))

  (add-hook 'before-save-hook 'clang-format-before-save)
  :config
  (setq clang-format-style "Google"))

(use-package cpputils-cmake
  :ensure t
  :after (c++-mode c-mode)
  ;; :bind (("C-c C-c C-g" . (lambda ()(interactive) (gud-gdb (concat "gdb --fullname " (cppcm-get-exe-path-current-buffer))))))
  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (if (derived-mode-p 'c-mode 'c++-mode)
                  (cppcm-reload-all)
                )))
  (add-hook 'c90-mode-hook (lambda () (cppcm-reload-all))))

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

(use-package python-ts-mode
  :ensure nil
  :delight "py"
  :after (tychoish-editing)
  :bind (:map python-ts-mode-map
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
  (add-hook 'python-ts-mode-hook (lambda ()
                                (define-key python-ts-mode-map "'" 'tychoish-electric-pair)
                                (set-fill-column 100)))

  (setq python-indent-offset 4)

  (if (eq system-type 'darwin)
      (setq py-python-command "/usr/bin/python"))
  (if (eq system-type 'gnu/linux)
      (setq py-python-command "/usr/bin/python2"))

  (font-lock-add-keywords 'python-ts-mode (font-lock-show-tabs))
  (font-lock-add-keywords 'python-ts-mode (font-lock-width-keyword 100)))

(use-package virtualenvwrapper
  :ensure t
  :after (python-ts-mode)
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell))

(use-package eshell
  :commands (eshell)
  :after (helm)
  :init
  (add-hook 'eshell-mode-hook
            (lambda ()
              (eshell-cmpl-initialize)
              (define-key eshell-mode-map (kbd "M-p") 'helm-eshell-history)
              (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
              (define-key eshell-mode-map (kbd "M-s f") 'helm-eshell-prompts-all)
              (define-key eshell-mode-map (kbd "M-r") 'helm-eshell-history))))

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

(use-package jinja2-mode
  :ensure t
  :mode "\\.jinja\\'")


(use-package ninja-mode
  :ensure t
  :mode "\\.ninja\\'")

(use-package slime
  :ensure t
  :after (f tychoish-bootstrap)
  :commands (slime)
  :bind (("C-c d c" . hyperspec-lookup))
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

  (load-quicklisp-file "slimes-helper.el")
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
  :ensure t
  :mode (("\\.org$" . org-mode))
  :after (s)
  :delight "org"
  :commands (tychoish-org-add-file-capture-templates
             org-save-all-org-buffers)
  :bind (("C-c o a" . org-agenda)
         ("C-c o t a" . org-agenda)
         ("C-c o l s" . org-store-link)
         ("C-c o l i" . org-insert-link)
         ("C-c o j" . helm-org-capture-templates)
         ("C-c o c" . org-capture)
         ("C-c o h a" . helm-org-agenda-files-headings)
         ("C-c o k l" . org-capture-goto-last-stored)
         ("C-c o k t" . org-capture-goto-target)
         ("C-c o k w" . org-capture-refile)
         ("C-c o l a" . org-annotate-file))
  :init
  (add-hook 'org-mode-hook 'tychoish--add-toc-org-hook)
  (add-hook 'org-mode-hook 'flycheck-mode)
  (add-hook 'org-mode-hook 'auto-fill-mode)

  (setq org-directory (concat local-notes-directory "/org"))
  (setq org-agenda-files (list org-directory))
  (defun tychoish-org-date-now ()
    (interactive)
    (format-time-string "<%Y-%02m-%02d %02H:%02M:%02S %Z>" (time-stamp)))

  (defvar org-odt-data-dir "~/.emacs.d/org/etc")

  (defun tychoish-org-mark-done-and-archive ()
    "mark done and move to completed archive sibling"
    (interactive)
    (org-todo 'done)
    (let ((org-archive-sibling-heading "Completed"))
      (org-archive-to-archive-sibling)))

  (defun org-set-weekday-of-timestamp ()
    "Check if cursor is within a timestamp and compute weekday from numeric date"
    (interactive)
    (when (org-at-timestamp-p t)
      (org-timestamp-change 0 'year)
      t))

  (defun tychoish-org-reset-capture-templates ()
    (interactive)
    (setq org-capture-templates '(("n" "notes")
                                  ("j" "journal")
                                  ("t" "tasks")
                                  ("r" "routines"))))

  (defun tychoish-org--add-routines (prefix-key name orgfile-path)
    (add-to-list 'org-capture-templates `(,(concat "r" prefix-key) ,name))
    (dolist (ival-pair '(("1d" . "Daily")
                         ("1w" . "Weekly")
                         ("4w" . "Monthly")
                         ("12w" . "Quarterly")
                         ("21w" . "Half Yearly")
                         ("52w" . "Yearly")))
      (let* ((interval (car ival-pair))
             (heading (cdr ival-pair))
             (interval-prefix (downcase (substring heading 0 1)))
             (lower-heading (downcase heading))
             (menu-name (concat name lower-heading "routine"))
             (template (concat "* %^{Title}\nSCHEDULED: <%(org-read-date nil nil \"++" interval "\") ++" interval ">\n%?")))
      (dolist (prefix (list (concat prefix-key "r" interval-prefix) (concat "r" prefix-key interval-prefix)))
        (add-to-list 'org-capture-templates `(,prefix ,(concat name " " lower-heading " routine")
                                              entry (file+olp ,orgfile-path "Loops" heading)
                                              ,template
                                              :prepend t
                                              :kill-buffer t
                                              :empty-lines-after 1))))))

  :config
  (tychoish-org-reset-capture-templates)
  (defun tychoish-org-add-file-capture-templates (name &rest args)
    "Defines a set of capture mode templates for adding notes and tasks to a file."
    (let ((org-filename (concat org-directory "/" (make-filename-slug name) ".org"))
          (prefix-key (or (plist-get args :prefix) ""))
          (should-add-routines (plist-get args :routines)))

      (when (s-contains? prefix-key "tjnr")
        (error "org-capture prefix key '%s' for '%s' contains well-known prefix" prefix-key org-filename))

      (when prefix-key
        (add-to-list 'org-capture-templates `(,prefix-key ,name)))

      (when should-add-routines
        (tychoish-org--add-routines prefix-key name org-filename))

      (add-to-list 'org-capture-templates `(,prefix-key ,name))

      ;; journal, for date related content
      (dolist (prefix (list (concat prefix-key "j") (concat "j" prefix-key)))
        (add-to-list 'org-capture-templates `(,prefix ,(concat name " journal")
                                              entry (file+datetree ,org-filename "Journal")
                                              "* <%<%Y-%m-%d %H:%M>>\n%?"
                                              :prepend nil
                                              :kill-buffer t
                                              :empty-lines-after 1)))

      (add-to-list 'org-capture-templates `(,(concat prefix-key "n") ,(concat name " notes")))
      (dolist (prefix (list (concat "n" prefix-key) (concat prefix-key "nn")))
        (add-to-list 'org-capture-templates `(,prefix ,(concat name " basic notes")
                                                     entry (file+headline ,org-filename "Inbox")
                                                     "* %?"
                                                     :prepend t
                                                     :kill-buffer t
                                                     :empty-lines-after  1)))
      (add-to-list 'org-capture-templates `(,(concat prefix-key "nl") ,(concat name " notes (org-link)")
                                            entry (file+headline ,org-filename "Inbox")
                                            "* %?\n%a"
                                            :prepend t
                                            :kill-buffer t
                                            :empty-lines-after 1))
      (add-to-list 'org-capture-templates `(,(concat prefix-key "nk") ,(concat name " notes (kill buffer)")
                                            entry (file+headline ,org-filename "Inbox")
                                            "* %?\n%c"
                                            :prepend t
                                            :kill-buffer t
                                            :empty-lines-after 1))

      (add-to-list 'org-capture-templates `(,(concat prefix-key "t") ,(concat name " tasks")))
      (dolist (prefix (list (concat "t" prefix-key) (concat prefix-key "tt")))
        (add-to-list 'org-capture-templates `(,prefix ,(concat name " basic tasks")
                                              entry (file+headline ,org-filename "Tasks")
                                              "* TODO %?"
                                              :prepend t
                                              :kill-buffer t
                                              :empty-lines-after 0)))
      (add-to-list 'org-capture-templates `(,(concat prefix-key "tl") ,(concat name " tasks (org-link)")
                                            entry (file+headline ,org-filename "Tasks")
                                            "* TODO %?\n%a"
                                            :prepend t
                                            :kill-buffer t
                                            :empty-lines-after 0))
      (add-to-list 'org-capture-templates `(,(concat prefix-key "tk") ,(concat name " tasks (kill buffer)")
                                            entry (file+headline ,org-filename "Tasks")
                                            "* TODO %?\n%c"
                                            :prepend t
                                            :kill-buffer t
                                            :empty-lines-after 0))))
  (define-key org-mode-map (kbd "C-c o w") 'org-refile)
  (define-key org-mode-map (kbd "C-c o l o") 'org-open-link-from-string)
  (define-key org-mode-map (kbd "C-c o l a o") 'org-agenda-open-link)
  (define-key org-mode-map (kbd "C-c o t") 'org-set-tags-command)
  (define-key org-mode-map (kbd "C-c o p") 'org-insert-property-drawer)
  (define-key org-mode-map (kbd "C-c o d n") 'tychoish-org-date-now)
  (define-key org-mode-map (kbd "C-c o a a") 'org-agenda)
  (define-key org-mode-map (kbd "C-c o a s") 'org-archive-to-archive-sibling)
  (define-key org-mode-map (kbd "C-c o a e") 'org-cycle-force-archived)
  (define-key org-mode-map (kbd "C-c o a d") 'tychoish-org-mark-done-and-archive)
  (define-key org-mode-map (kbd "C-c o a f") 'org-archive-set-tag)
  (define-key org-mode-map (kbd "C-c o n") 'org-narrow-to-subtree)
  (define-key org-mode-map (kbd "C-c o b t") 'org-ctags-create-tags)
  (define-key org-mode-map (kbd "C-c o h c") 'helm-capture-templates)
  (define-key org-mode-map (kbd "C-c o h p") 'helm-org-parent-headings)
  (define-key org-mode-map (kbd "C-c o h b") 'helm-org-in-buffer-headings)
  (define-key org-mode-map (kbd "M-TAB") 'org-cycle)
  (define-key org-mode-map (kbd "C-M-TAB") 'org-cycle-force-archived)
  (define-key org-mode-map (kbd "C-c o r c") 'org-bibtex-create)
  (define-key org-mode-map (kbd "C-c o r r") 'org-bibtex-create-in-current-entry)
  (define-key org-mode-map (kbd "C-c o r k") 'org-bibtex-export-to-kill-ring)
  (define-key org-mode-map (kbd "C-c o r v v") 'org-bibtex-check)
  (define-key org-mode-map (kbd "C-c o r v a") 'org-bibtex-check-all)
  (define-key org-mode-map (kbd "C-c o r s") 'org-bibtex-search)
  (define-key org-mode-map (kbd "C-c o r e") 'org-bibtex)
  (define-key org-mode-map (kbd "C-c C-p") 'set-mark-command)

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
  (diminish 'org-capture-mode)

  (add-hook 'org-ctrl-c-ctrl-c-hook 'org-set-weekday-of-timestamp)
  (add-hook 'org-agenda-mode-hook 'revbufs)
  (add-hook 'org-shiftup-final-hook 'windmove-up)
  (add-hook 'org-shiftleft-final-hook 'windmove-left)
  (add-hook 'org-shiftdown-final-hook 'windmove-down)
  (add-hook 'org-shiftright-final-hook 'windmove-right)

  (setq auto-mode-alist (cons '("\\.org$" . org-mode) auto-mode-alist))
  (setq org-archive-location (concat org-directory "/archive/%s::datetree/"))
  (setq org-default-notes-file (concat org-directory "/organizer.org"))
  (setq org-annotate-file-storage-file (concat org-directory "/annotations.org"))
  (setq org-agenda-files (cl-remove-duplicates (append org-agenda-files user-org-directories)))

  (org-load-modules-maybe t)
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
        '(("b" "Backlog" tags "+backlog|+inbox-ITEM=\"Inbox\"|TODO=BLOCKED")
          ("c" "Super view"
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

  (setq org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d!)")
                            (sequence "BLOCKED(s)" "BACKLOG(b)" "INPROGRESS(p)" "|" "GONEAWAY(g@)" "INCOMPLETE(i@)")))

  (setq org-todo-keyword-faces '(("TODO" . org-warning)
                                 ("INPROGRESS" . "orange")
                                 ("INCOMPLETE" . "orange")
                                 ("SCHEDULED" . "green")
                                 ("BACKLOG" . (:foreground "orange" :weight bold))
                                 ("PROJECT" . (:foreground "blue" :weight bold))))

  (setq org-tag-alist '((:startgroup . nil)
                          ("inbox" . ?i)
                          ("backlog" . ?b)
                        (:endgroup . nil)
                        (:startgroup . nil)
                          ("@desk" . ?d)
                          ("@personal" . ?p)
                          ("@work" . ?w)
                        (:endgroup . nil)))

  (setq org-CUA-compatible t)
  (setq org-replace-disputed-keys t)
  (setq org-agenda-block-separator nil)
  (setq org-agenda-columns-add-appointments-to-effort-sum t)
  (setq org-agenda-compact-blocks t)
  (setq org-agenda-default-appointment-duration 60)
  (setq org-agenda-inhibit-startup nil)
  (setq org-agenda-mouse-1-follows-link t)
  (setq org-agenda-skip-deadline-if-done t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
  (setq org-agenda-skip-unavailable-files t)
  (setq org-agenda-start-on-weekday nil)
  (setq org-enforce-todo-checkbox-dependencies t)
  (setq org-enforce-todo-dependencies t)
  (setq org-fast-tag-selection-include-todo t)
  (setq org-fontify-done-headline t)
  (setq org-footnote-auto-label nil)
  (setq org-footnote-define-inline nil)
  (setq org-footnote-section nil)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-provide-todo-statistics t)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  (setq org-refile-targets '((org-agenda-files :maxlevel . 4)))
  (setq org-log-into-drawer t)
  (setq org-refile-use-outline-path 'file)
  (setq org-return-follows-link t)
  (setq org-reverse-note-order t)
  (setq org-startup-folded "content")
  (setq org-startup-indented nil)
  (setq org-tags-exclude-from-inheritance '("project"))
  (setq org-track-ordered-property-with-tag t)
  (setq org-use-fast-tag-selection t)
  (setq org-use-fast-todo-selection t)
  (setq org-use-speed-commands (lambda () (and (looking-at org-outline-regexp) (looking-back "^\**"))))
  (setq org-agenda-skip-scheduled-if-done t))

;; (use-package org-mu4e
;; :ensure t
;;   :after (org mu4e))

(use-package helm-org
  :ensure t
  :after (org helm)
  :commands (helm-org-capture-templates
             helm-org-in-buffer-heddings
             helm-org-agenda-files-headings)
  :config
  (add-to-list 'helm-completing-read-handlers-alist '(org-capture . helm-org-completing-read-tags))
  (add-to-list 'helm-completing-read-handlers-alist '(org-set-tags . helm-org-completing-read-tags)))

(use-package org-contrib
  :ensure t
  :after org)

(use-package toc-org
  :ensure t
  :after org
  :commands (toc-org-insert-toc)
  :autoload (tychoish--add-toc-org-hook)
  :config
  (defun tychoish--add-toc-org-op () (save-excursion (toc-org-insert-toc)))
  (defun tychoish--add-toc-org-hook () (add-hook 'write-contents-functions 'tychoish--add-toc-org-op nil t)))

(use-package ox-gist
  :ensure t
  :after ox
  :commands (org-gist-export-private-gist org-gist-export-to-public-gist)
  :bind (:map org-mode-map
         ("C-c o e g p" . org-gist-export-to-private-gist)
         ("C-c o e g g" . org-gist-export-to-public-gist))
  :config
  (defun org-gist-export-private-gist ()
    (interactive)
    (org-gist-export-to-gist))
  (defun org-gist-export-public-gist ()
    (interactive)
    (org-gist-export-to-gist 'public)))

(use-package ox-rst
  :ensure t
  :after ox
  :commands (org-rst-export-to-rst org-rst-export-as-rst)
  :config
  (setq org-rst-headline-underline-characters (list ?= ?- ?~ ?' ?^ ?`)))

(use-package ox-leanpub
  :ensure t
  :after ox
  :config
  (require 'ox-leanpub-markua)
  (org-leanpub-book-setup-menu-markua))

(use-package ox-hugo
  :ensure t
  :after ox)

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
         ("C-x h" . help)
         ("C-x C-h" . help)
         ("C-c f C-0" . opacity-reset)
         ("C-c t t d" . disable-theme)
         ("<f5>" . xterm-mouse-mode-toggle)
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
  (setq native-comp-jit-compilation t)

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
  (setq kill-buffer-query-functions nil)

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
  ;; (set-face-attribute 'header-line nil :background nil :weight 'bold)
  (add-to-list 'term-file-aliases '("alacritty" . "xterm"))
  (set-fontset-font t 'emoji '("Noto Color Emoji" . "iso10646-1") nil 'prepend)

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
  (setq qbookmark-default-file (tychoish-get-config-file-path "bookmarks")))

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
             tychoish-create-note-file
             tychoish-blog-open-drafts-dired
             make-filename-slug)
  :bind (("C-c t b m" . tychoish-blog-insert-date)
         ("C-c t b p" . tychoish-blog-publish-post)
         ("C-c t b n" . tychoish-blog-create-post)
         ("C-c t b C-p" . tychoish-blog-push)
         ("C-c t b d" . tychoish-blog-open-drafts-dired))
  :config
  (setq tychoish-blog-path (expand-file-name "~/src/blog")))

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

  (setq select-enable-primary t)
  (setq select-enable-clipboard t)

  (global-set-key (kbd "<mouse-2>") 'clipboard-yank)

  (global-set-key (kbd "s-x") 'clipboard-kill-region) ;;cut
  (global-set-key (kbd "s-c") 'clipboard-kill-ring-save) ;;copy
  (global-set-key (kbd "s-v") 'clipboard-yank) ;;paste
  (global-set-key (kbd "C-z") 'undo)

  (global-unset-key (kbd "C-x C-u"))
  (global-set-key (kbd "C-x C-u t") 'upcase-initials-region)
  (global-set-key (kbd "C-x C-u r") 'upcase-region)
  (global-set-key (kbd "C-x C-u w") 'upcase-word)
  (global-set-key (kbd "C-c d k") 'delete-region)

  (global-set-key (kbd "C-x l") 'goto-line)
  (global-set-key (kbd "C-c C-f") 'set-fill-column)
  (global-set-key (kbd "C-c C-p") 'set-mark-command)
  (global-set-key (kbd "C-c c") 'comment-region)
  (global-set-key (kbd "C-c i") 'indent-region)

  (global-set-key (kbd "M-<SPC>") 'set-mark-command)
  (global-set-key (kbd "C-h") 'backward-kill-word)
  (global-set-key (kbd "C-x C-x") 'exchange-point-and-mark)
  (global-set-key (kbd "M-<SPC>") 'set-mark-command))

(use-package git-grep
  :ensure t
  :bind (("C-c g g" . git-grep)
         ("C-c g r" . git-grep-repo))
  :init
  (global-set-key (kbd "C-c g f") 'find-grep)
  (global-set-key (kbd "C-c C-o") 'occur)
  (define-key isearch-mode-map (kbd "C-o") 'isearch-occur))

(use-package whitespace
  :ensure t
  :bind (("C-c C-w" . whitespace-cleanup))
  :config
  (setq whitespace-style '(face trailing tabs spaces lines newline missing-newline-at-eof
                           empty space-mark tab-mark newline-mark)))

(use-package tramp
  :ensure t
  :commands (sshra ssh-reagent)
  :config
  (setq tramp-default-method "ssh")

  (defun ssh-reagent ()
    (interactive)
    (let* ((sshdir (car (directory-files "/tmp" nil "ssh-*")))
           (agent (car (directory-files (concat "/tmp/" sshdir) nil "agent.*"))))
      (setenv "SSH_AUTH_SOCK" (concat "/tmp/" sshdir "/" agent)))
    (message "Attached to SSH Session"))

  (defalias 'sshra 'ssh-reagent))

(use-package docker
  :ensure t
  :commands (docker)
  :bind ("C-c C-d" . docker))

(use-package lpr
  :ensure t
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
  :ensure t
  :bind (("M-j" . windmove-down)
          ("M-k" . windmove-up)
          ("M-h" . windmove-left)
          ("M-l" . windmove-right)
          ("M-J" . (lambda () (interactive) (enlarge-window 1)))
          ("M-K" . (lambda () (interactive) (enlarge-window -1)))
          ("M-h" . (lambda () (interactive) (enlarge-window 1 t)))
          ("M-l" . (lambda () (interactive) (enlarge-window -1 t))))
  :config
  (windmove-default-keybindings))

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
  (setq message-interactive t)
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
             mu4e
             mu4e-compose-new
             mu4e-update-mail-and-index
             mu4e-headers-jump-to-maildir
             mu4e-headers-search-bookmark
             tychoish-set-up-email)
  :init
  (setq mail-header-separator "--------------------------")
  (setq mail-imenu-generic-expression
        '(("Subject"  "^Subject: *\\(.*\\)" 1)
          ("Cc"     "^C[Cc]: *\\(.*\\)" 1)
          ("To"     "^To: *\\(.*\\)" 1)
          ("From"  "^From: *\\(.*\\)" 1)))

  (setq mu4e-compose-complete-addresses t)
  (setq mu4e-compose-complete-only-after "2015-01-01")
  (setq mu4e-compose-dont-reply-to-self t)
  (setq mu4e-compose-format-flowed nil)
  (setq mu4e-compose-keep-self-cc nil)
  (setq mu4e-compose-signature t)
  (setq mu4e-drafts-folder "/drafts")
  (setq mu4e-headers-include-related nil)
  (setq mu4e-headers-results-limit 1000)
  (setq mu4e-maildir-shortcuts nil)
  (setq mu4e-reply-to-address user-mail-address)
  (setq mu4e-sent-folder "/sent")
  (setq mu4e-trash-folder "/trash")
  (setq mu4e-user-agent-string nil)
  (setq mu4e-view-show-images t)
  (setq mu4e~maildir-list nil)

  (setq completion-ignore-case t)
  (setq compose-mail-user-agent-warnings nil)
  (setq mail-signature t)
  (setq mail-specify-envelope-from t)
  (setq mail-user-agent 'mu4e-user-agent)
  (setq mc-gpg-user-id (getenv "GPG_KEY_ID"))
  (setq mml-secure-openpgp-sign-with-sender t)
  (setq sendmail-program "msmtp")
  (setq smtpmail-queue-mail nil)

  (defun tychoish-mu4e-compose-mode-hook-op ()
    "My settings for message composition."
    (flush-lines "^\\(> \n\\)*> -- \n\\(\n?> .*\\)*") ;;; Kills quoted sigs.
    (set-buffer-modified-p nil) ;;; We haven't changed the buffer, haven't we? *g*
    (message-goto-body) ;;; Jumps to the beginning of the mail text
    (setq make-backup-files nil) ;;; No backups necessary.
    (use-hard-newlines 1 'always)
    (auto-fill-mode 1)
    (visual-line-mode 0)
    (visual-fill-column-mode 0)
    (setq-local message-fill-column 66)
    (setq-local set-fill-column 66)
    (flyspell-mode 1))

  (add-hook 'mu4e-compose-mode-hook 'tychoish-mu4e-compose-mode-hook-op)

  (defun tychoish-add-standard-mail-bookmarks ()
    "add a standard/generic list of bookmarks"
    (add-to-list 'mu4e-bookmarks '("mime:image/*" "Messages with images" ?p))
    (add-to-list 'mu4e-bookmarks '("date:today..now" "Today's messages" ?t))
    (add-to-list 'mu4e-bookmarks '("date:7d..now" "This Week's messages" ?w))
    (add-to-list 'mu4e-bookmarks '("m:/inbox" "Inbox (all)" ?i))
    (add-to-list 'mu4e-bookmarks '("m:/rss.*  AND flag:unread" "RSS Unread" ?r))
    (add-to-list 'mu4e-bookmarks '("m:/inbox OR flag:unread AND NOT (flag:trashed OR m:/sent OR m:/trash)" "all unread message" ?a))
    (add-to-list 'mu4e-bookmarks '("flag:unread AND NOT flag:trashed AND NOT m:/rss.*" "Unread messages (no RSS)" ?u))
    (add-to-list 'mu4e-bookmarks '("m:/inbox OR flag:unread AND NOT (m:/rss.* OR m:/sent OR flag:trashed OR m:/trash)"
                                   "to read/process queue" ?q))
    (add-to-list 'mu4e-bookmarks '("(m:/inbox OR m:/prof) AND flag:unread" "unread primary queues to file"?f))
    (add-to-list 'mu4e-bookmarks '("(NOT m:/inbox AND NOT m:/prof) AND flag:unread" "all sorted email" ?s)))


  :config
  (defun tychoish-set-up-email (maildir name address)
    (setq message-directory maildir)
    (setq mu4e-maildir maildir)
    (setq mu4e-mu-home (f-join maildir ".mu"))

    (setq mu4e-bookmarks nil)
    (tychoish-add-standard-mail-bookmarks)

    (setq user-mail-address address)
    (setq user-full-name name)
    (setq mu4e-compose-reply-to-address address)
    (setq smtpmail-queue-dir (f-join maildir "queue/cur"))
    (setq message-signature-file (f-join maildir "tools" "signatures" address))
    (setq mail-host-address (s-replace-regexp ".*@" "" address))
    (setq message-sendmail-extra-arguments `("-a" ,address))
    (setq message-auto-save-directory (f-join mu4e-maildir "drafts"))
    (tychoish-change-email-body user-full-name address)
    (message (format "mail: configured address [%s]" address)))

  (defun tychoish-change-email-body (name address)
    "change an email address on an extant mail buffer"
    (when (equal major-mode 'mu4e-compose-mode)
      (goto-char (point-min))
      (let ((new-from (concat "From: " name " <" address ">")))
        (while (re-search-forward "^From:.*$" nil t 1)
          (replace-match new-from nil nil)))))

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
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
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

  (add-hook 'markdown-mode-hook 'turn-on-auto-fill)
  (add-hook 'markdown-mode-hook 'flycheck-mode)
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

(use-package helm-flyspell
  :ensure t
  :bind (("M-$" . helm-flyspell-correct)
         ("C-c h e" . helm-flyspell-correct)))

(use-package flyspell-correct-helm
  :ensure t
  :after (flyspell)
  :init
  (setq flyspell-correct-interface #'flyspell-correct-helm))

(use-package flyspell
  :ensure t
  :defer t
  :diminish (flyspell-mode . "fs")
  :commands (flyspell-mode flyspell-prog-mode flyspell-correct-wrapper)
  :init
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  (add-hook 'text-mode-hook 'flyspell-mode)
  :bind (("M-;" . flyspell-correct-wrapper)
         ("C-c C-;" . flyspell-correct-wrapper)
         ("C-c ;" . flyspell-correct-wrapper))
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
  (add-hook 'rst-mode-hook 'flycheck-mode)
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

(use-package flycheck-aspell
  :ensure t
  :after flycheck
  :config
  (flycheck-aspell-define-checker "org" "Org" ("--add-filter" "url") (org-mode))
  (flycheck-aspell-define-checker "rst" "reStructuredText" ("--add-filter" "url") (rst-mode))

  (add-to-list 'flycheck-checkers 'org-aspell-dynamic)
  (add-to-list 'flycheck-checkers 'rst-aspell-dynamic)
  (add-to-list 'flycheck-checkers 'markdown-aspell-dynamic)
  (add-to-list 'flycheck-checkers 'c-aspell-dynamic)
  (add-to-list 'flycheck-checkers 'mail-aspell-dynamic))

(use-package flycheck-vale
  :ensure t
  :after (flycheck)
  :ensure-system-package vale
  ;; vale is the aur package "vale-bin" but maybe its more portable to
  ;; try and use "go get" or "go install".
  :config
  (flycheck-vale-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; irc (erc) configuration
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package alert
  :ensure t
  :functions (alert)
  :commands (alert)
  :config
  (setq alert-log-messages nil)
  (cond
   ((eq system-type 'darwin)
    (setq alert-default-style 'osx-notifier))
   ((executable-find "notify-send")
    (setq alert-default-style 'libnotify))
   ((eq system-type 'gnu/linux)
    (setq alert-default-style 'notifications))
   (t (setq alert-default-style 'message)))

  (message (format "%s alerts configured" alert-default-style)))

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
                             (make-local-variable 'erc-fill-column)
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
                      list
                      match
                      menu
                      move-to-prompt
                      netsplit
                      networks
                      noncommands
                      readonly
                      ring
                      spelling
                      track))


  (setq erc-ignore-list '("*@*facebook" "&bitlbee"))
  (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                    "324" "329" "332" "333" "353" "477"))
  (setq erc-hide-list '("MODE" "JOIN" "PART"))

  (setq erc-current-nick-highlight-type 'nick)
  (setq erc-insert-timestamp-function 'erc-insert-timestamp-left)
  (setq erc-keywords '("\\berc[-a-z]*\\b" "\\bemms[-a-z]*\\b"))

  (setq erc-timestamp-format "[%H:%M] ")
  (setq erc-fill-prefix "        ")
  (setq erc-fill-column 80)

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
            (lambda ()
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
    "When t, disable setting =mode-line-process= with the connection status")

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
    "Switch to ERC buffer using IDO to choose which one, or start ERC
if not already started."
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

(use-package telega
  :ensure t
  :commands (telega)
  :bind-keymap (("C-c v" . telega-prefix-map))
  :config
  (set-fontset-font t 'unicode "Symbola" nil 'append)

  (require 'telega-mnz)
  (require 'telega-alert)
  (telega-notifications-mode 1)
  (telega-alert-mode 1)
  (telega-mode-line-mode 1)
  (global-telega-mnz-mode 1)
  (diminish 'telega-mnz-mode)

  (setq telega-debug nil)

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

  (setq telega-use-images t)
  (setq telega-chat-fill-column 72)
  (setq telega-root-fill-column 72)
  (setq telega-chat-button-width 24)
  (setq telega-chat-input-markups '("markdown2"))
  (setq telega-server-libs-prefix "/usr/lib")
  (setq telega-completing-read-function 'helm--completing-read-default)
  (setq telega-use-tracking-for '(or unmuted mention))
  (setq telega-markdown2-backquotes-as-precode t)

  (define-key telega-prefix-map (kbd "f") telega-chatbuf-fastnav-map)
  (define-key telega-prefix-map (kbd "F") 'telega-buffer-file-send)
  (define-key telega-prefix-map (kbd "r") 'telega-root-fastnav-map)
  (define-key telega-prefix-map (kbd "v") 'telega)

  (add-hook 'telega-chat-mode-hook 'visual-line-mode)
  (add-hook 'telega-chat-mode-hook 'flyspell-mode))

(use-package tracking
  :ensure t
  :after (telega erc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; System Configuration
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package pkgbuild-mode
  :ensure t
  :mode ("/PKGBUILD$"))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; language server protocol code
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure)
  :hook ((python-ts-mode
          go-ts-mode
          rust-ts-mode
          js-ts-mode
          typescript-ts-mode
          bash-ts-mode sh-mode) . eglot-ensure)
  :bind (("C-c l l s" . eglot)
         ("C-c l l r" . eglot-reconnect)
         ("C-c l l k" . eglot-shutdown)
         ("C-c l l l" . eglot-list-connections)
         ("C-c l r" . eglot-rename)
         ("C-c l f" . eglot-format)
         ("C-c l m" . helm-imenu)
         ("C-c l a" . eglot-code-actions))
  :config
  (setq eglot-ignored-server-capabilities '(:inlayHintProvider))
  (setq eglot-autoshutdown t)

  (defun eglot-organize-imports ()
    (interactive)
    (eglot-code-actions nil nil "source.organizeImports" t))

  (add-hook 'before-save-hook 'eglot-organize-imports)
  (add-hook 'before-save-hook 'eglot-format-buffer)

  (setq-default eglot-workspace-configuration
                '((:pylsp . (:configurationSources ["flake8"]
                             :plugins (:pycodestyle (:enabled nil)
                                       :black (:enabled t)
                                       :mccabe (:enabled nil)
                                       :flake8 (:enabled t)))))))

(use-package helm-xref
  :ensure t
  :bind (("M-." . xref-find-definitions)
         ("C-c l c" . xref-find-references)
         ("C-c l d" . xref-find-definitions)
         ("C-c l p" . xref-go-back)
         ("C-c l n" . xref-go-forward)
         ("C-c l o" . xref-find-definitions-other-window)))

(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :config
  (setq flycheck-eglot-exclusive nil)
  (setq flycheck-eglot-enable-diagnostic-tags)
  (global-flycheck-eglot-mode 1))

(use-package treesit
  :ensure nil
  :init
  (setq c++-ts-mode-hook 'c++-mode-hook)
  (setq c-ts-mode-hook 'c-mode-hook)
  (setq js-ts-mode-hook 'js-mode-hook)

  (setq major-mode-remap-alist
        '((js-mode . js-ts-mode)
          (js-json-mode . json-ts-mode)
          (c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (c-or-c++-mode . c-or-c++-ts-mode)
          (css-mode . css-ts-mode)
          (python-mode . python-ts-mode)))

  (add-to-list 'auto-mode-alist '("\\.sh\\'" . bash-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.bash\\'" . bash-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.bashrc\\'" . bash-ts-mode))

  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . css-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))

  (add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.cxx\\'" . c++-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c-or-c++-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.java\\'" . java-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.sc\\'" . scala-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.kt\\'" . kotlin-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.kts\\'" . kotlin-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.kdl\\'" . kdl-ts-mode))

  (add-to-list 'auto-mode-alist '("go.mod" . go-mod-ts-mode))
  (add-to-list 'auto-mode-alist '("Cargo.lock" . toml-ts-mode))

  (add-to-list 'auto-mode-alist '("Dockerfile" . dockerfile-ts-mode))
  (add-to-list 'auto-mode-alist '("CMakeLists.txt" . cmake-ts-mode))

  (font-lock-add-keywords 'c-mode (font-lock-width-keyword 100))
  (font-lock-add-keywords 'c-ts-mode (font-lock-width-keyword 100))
  (font-lock-add-keywords 'c++-mode (font-lock-width-keyword 100))
  (font-lock-add-keywords 'c++-ts-mode (font-lock-width-keyword 100))
  (add-hook 'c++-mode-hook (lambda () (setq show-trailing-whitespace t)))
  :config
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
       (message "rebuilding treesit grammars %s" result)))))

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
(provide 'tychoish-core)
;;; tychoish-core.el ends here
