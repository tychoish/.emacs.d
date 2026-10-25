;;; setup-core.el -- contains all major use-package forms -*- lexical-binding: t; -*- no-byte-compile: t; -*-

;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Provides my collection of use-package forms and related
;; configuration for loading and configuring all Emacs packages.

;; This configuration optimizes for lazy-loading so that configuration
;; only loads when called directly or a mode is activated.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'generator))
(require 'cl-lib)
(require 'generator)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; INTERNAL

(use-package hud
  :ensure nil
  :defer t
  :commands (hud-dispatch hud-select))

(use-package eglox
  :ensure nil
  :defer t
  :commands (eglox-mode
	     eglox-global-mode
	     eglox-ensure
	     eglox-cleanup-stale-connections
	     eglox-update-workspace
	     eglox-code-action-inline
	     eglox-format-for-hook
	     eglox-organize-imports-for-hook))

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
	     ad:org-agenda-redo
	     orgx-link-github-line-support
	     orgx-open-custom-id-slug-fallback)
  :init
  (with-eval-after-load 'ol
    (require 'orgx))
  (keymap-set orgx-global-map "a" #'orgx-agenda-view)
  (keymap-set orgx-global-map "r" #'orgx-agenda-files-reload)
  (keymap-set orgx-global-map "f" #'orgx-agenda-files-open)
  (keymap-set orgx-global-map "j" #'orgx-capture)
  (keymap-set orgx-global-map "u" (cons "untagged-in-file" #'orgx-agenda-untagged-in-file))
  (keymap-set orgx-global-map "/" #'orgx-agenda-for-file)

  (add-hook 'org-mode-hook #'orgx-minor-mode-turn-on)
  (add-hook 'org-agenda-mode-hook #'orgx-agenda-minor-mode-turn-on)
  (add-one-shot-hook
   :name "org-install-aux-packages"
   :hook 'org-mode-hook
   :operation #'orgx--install-auxiliary-packages)
  :config
  (font-lock-add-keywords 'org-mode tk-font-lock-keywords 'append)
  (orgx--setup-standard-capture-templates)
  (keymap-set orgx-global-map "c" (cons "org-capture-commands" orgx-minor-mode-capture-map))
  (advice-add 'org-agenda :before #'ad:org-agenda--open-files)
  (advice-add 'org-agenda-redo :around #'ad:org-agenda-redo))

(require 'setup-project)

(use-package setup-mail
  :ensure nil
  :defer t
  :commands (tychoish-mail-select-account
	     tychoish-define-mail-account
	     tychoish-mail-add-refile-rule)
  :init
  (keymap-set hud-mail-map "a" #'tychoish-mail-select-account))

(use-package daemons-dash
  :ensure nil
  :commands (daemons-dash daemons-dash-dispatch)
  :init
  (keymap-set hud-core-map "s" #'daemons-dash)
  :config
  (require 'daemons-dash-config nil t))

(use-package magit-dash
  :ensure t
  :defer t
  :init
  (keymap-set hud-magit-map "d" #'magit-dash-open)
  (keymap-set hud-magit-map "o" #'magit-dash-open-repo)
  (keymap-set hud-magit-map "g" #'magit-dash-gh-menu)
  (keymap-set hud-magit-map "w" #'magit-dash-worktree-dispatch)
  (keymap-set hud-core-map "d" #'magit-dash-open)
  (keymap-set hud-core-map "g" #'magit-dash-open)
  :config
  (setq magit-dash-gh-prune-cache-dir (sprite-state-path "magit-dash-gh-prune"))
  (setq magit-dash-gh-prune-pr-limit 50)
  (setq magit-dash-show-discovered-submodules nil)
  (setq magit-dash-render-branch-name-as-basename t)
  (setq magit-dash-which-key-abbreviate-commands 'strip)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CORE PACKAGES

(use-package delight
  :ensure t
  :defer (not (gui-p))
  :demand (gui-p)
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
  (delight 'markdown-mode "mdwn")
  (delight 'emojify-mode)
  (delight 'nerd-icons-dired-mode nil 'nerd-icons-dired)
  (delight 'nerd-icons-xref-mode nil 'nerd-icons-xref)
  (delight 'nerd-icons-completion-mode nil 'nerd-icons-completion)
  (delight 'nerd-icons-dired-mode "")
  (delight 'builder-mode "")
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
  (delight 'auto-revert-mode nil 'autorevert)

  (delight 'async-bytecomp-package-mode "" 'async-bytecomp)
  (delight 'dired-async-mode "" 'dired-async)

  (delight 'agent-shell-completion-mode nil 'agent-shell-completion)
  (delight 'agent-shell-ui-mode nil 'agent-shell-ui)

  (delight 'go-ts-mode "go.ts")
  (delight 'go-mod-ts-mode "go.mod.ts")
  (delight 'go-mode "go")
  (delight 'rust-mode "rs")
  (delight 'rustic-mode "rs(x)")
  (delight 'python-ts-mode "py.ts")
  (delight 'python-mode "py")
  (delight 'lisp-mode "lisp")
  (delight 'slime-mode "sl")
  (delight 'slime-autodoc-mode "")

  (delight 'denote-sequence-hierarchy-mode "Hierarchy" 'denote-sequence)
  (delight 'outline-minor-mode nil 'outline)
  (delight 'cursor-sensor-mode nil 'cursor-sensor)
  (delight 'agent-shell-notifications-mode nil 'agent-shell-notifications)
  (delight 'terraform-mode "tf" 'terraform-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Project / Repository Tools

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; grep/search

(use-package ripgrep
  :ensure t
  :defines (hud-ecclectic-rg-map)
  :defer t
  :init
  ;; "C-c g r"
  (keymap-set hud-ecclectic-rg-map "x" #'deadgrep)
  (keymap-set hud-ecclectic-rg-map "d" #'find-ripgrep)
  (keymap-set hud-ecclectic-rg-map "c" #'find-ripgrep-compile)
  (keymap-set hud-ecclectic-rg-map "m" #'find-merge-conflicts)
  (keymap-set hud-ecclectic-grep-project-map "p" #'find-ripgrep-project)
  (defvar ripgrep-regexp-history nil)

  ;; find-ripgrep -- compilation buffer wrappers

  (cl-defun ripgrep-compile (&key regexp directory buffer-name)
    (let ((compilation-buffer-name-function (lambda (&optional _) (or buffer-name (format "*%s-rg*" (projectile-project-name))))))
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
    (interactive)
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
     :directory (projectile-project-root)
     :regexp (find-ripgrep--resolve-regexp
	      :directory (projectile-project-root))))

  (defun find-merge-conflicts ()
    "Use ripgrep to identify all merge conflict artifacts"
    (interactive)
    (let ((root (projectile-project-root)))
      (ripgrep-compile
       :regexp "^(=======$|<<<<<<<|>>>>>>>)"
       :directory root
       :buffer-name (format "*%s-merge-conflicts*"
                            (file-name-nondirectory (directory-file-name root))))))
  :config
  (setenv "RIPGREP_CONFIG_PATH" (expand-file-name "~/.ripgreprc")))

(use-package wgrep
  :ensure t
  :defer t
  :commands (wgrep-change-to-wgrep-mode)
  :after (grep)
  :init
  (keymap-set grep-mode-map "w" #'wgrep-change-to-wgrep-mode)
  :config
  (setq wgrep-enable-key "w"))

(use-package anzu
  :ensure t
  :defer t
  :init
  (keymap-set isearch-mode-map "C-o" #'isearch-occur)
  (add-hook 'isearch-mode-hook #'anzu-mode)
  (setq anzu-cons-mode-line-p nil)
  :config
  (seq-do #'make-variable-buffer-local
          '(anzu--total-matched anzu--current-position anzu--state
            anzu--cached-count anzu--cached-positions anzu--last-command
            anzu--last-isearch-string anzu--overflow-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Completion & Selection Suite (Minaid)

(require 'minaid)

(use-package revbufs
  :ensure t
  :defer t
  :init
  (keymap-set global-map "C-x x a" #'revbufs)
  :config
  (keymap-set revbufs-mode-map "C-k" #'revbufs-kill)
  (keymap-set revbufs-mode-map "q" #'bury-buffer)

  (defalias 'revbufs-kill
    (kmacro "C-f C-f C-f C-k")))

(use-package popper
  :ensure t
  :commands (popper-cycle popper-toggle popper-kill-latest-popup)
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
  :defer t
  :commands (magit-toplevel)
  :config
  (setq vc-follow-symlinks t)
  (setq version-control t)
  (setq magit-module-sections-nested t)
  (put 'magit-diff-edit-hunk-commit 'disabled nil)
  (add-hook 'magit-status-sections-hook 'magit-insert-worktrees t)
  (add-hook 'magit-status-sections-hook 'magit-insert-modules t)

  (defvar-keymap magit-command-mode-map) ;; "/"
  (keymap-set magit-mode-map "/" '(magit-command-mode-map . "magit-command"))
  (keymap-set magit-command-mode-map "x" '(execute-extended-magit-command . "magit-commands"))
  (keymap-set magit-command-mode-map "m" '(execute-extended-magit-command . "(s)merge-commands"))

  (keymap-set magit-status-mode-map "C-n" #'next-line)
  (keymap-set magit-status-mode-map "C-p" #'previous-line)

  (when (fboundp 'hud-modeline-set-segment-action)
    (hud-modeline-set-segment-action 'vc #'magit-dispatch))

  (with-eval-after-load 'magit-gh
    (unless (condition-case nil
                (transient-get-suffix 'magit-gh "g")
              (error nil))
      (transient-append-suffix 'magit-gh '(-1)
        ["magit-dash"
         ("g" "magit-dash: prune/CI logs/PR comments/gh auth" magit-dash-gh-menu)]))))

(use-package git-link
  :ensure t
  :defer t
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
  :defer t
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

(use-package tracking
  :ensure t
  :after (:any telega erc)
  :defer t
  :config
  (setq tracking-max-mode-line-entries 3))

(use-package telega
  :ensure t
  :defines (telega-chat-mode-hook)
  :commands (telega-chat-mode
	     telega-extras-switch-to-root
	     telega-extras-force-kill
	     telega-extras-disconnect)
  :init
  (hud-bind-deferred-keymap hud-mode-map "C-c v" 'telega 'telega-prefix-map "telega")
  (hud-bind-deferred-keymap hud-mode-map "C-c n" 'telega 'telega-prefix-map "telega")
  :config
  (add-hook 'telega-chat-mode-hook #'minaid-corfu-text-mode-setup)

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
  (setq telega-extras-enable-chatbuf-idle-timers t
        telega-extras-chatbuf-bury-idle-time 1200
        telega-extras-chatbuf-kill-idle-time 14400)
  (setq telega-chat-auto-fill-mode-lighter "")
  (telega-mode-line-mode 1)
  (telega-alert-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; writing (english) configuration

(use-package denote
  :ensure t
  :defer t
  :commands (denote-dired denote-directories)
  :config
  (setq denote-dash-default-filter '(not "journal"))
  (setq denote-review-insert-after "date")
  (when (default-boundp 'denote-directory)
    (setq denote-directory '()))
  (add-to-list 'denote-directory (file-name-concat (or local-notes-directory (expand-file-name "~/notes")) "denote"))
  (setq denote-file-type 'org)
  (setq denote-known-keywords '("project" "reference" "journal" "idea" "writing" "migration" "agent" "plan" "singing"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-save-buffers t)
  (setq denote-prompts '(title keywords file-type))
  (setq denote-journal-title-format nil)
  (setq denote-journal-directory (file-name-concat local-notes-directory "denote" "journal"))
  (setq denote-sequence-scheme 'alphanumeric)
  (setq denote-explore-network-directory (file-name-concat (or local-notes-directory (expand-file-name "~/notes")) "explore"))
  (setq denote-explore-network-format "graphviz")
  (setq denote-explore-network-graphviz-filetype "svg")
  (add-to-list 'display-buffer-alist
               '("\\`\\*denote-sequence-hierarchy "
                 (display-buffer-reuse-window display-buffer-pop-up-window)))
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'denote--title-history)
    (add-to-list 'savehist-additional-variables 'denote--keywords-history))

  (setq denote-rename-buffer-function #'denote-dash-rename-buffer)
  (denote-rename-buffer-mode 1)

  (defun ad:denote-link-ol-get-heading--org-only (fn &rest args)
    "Only call FN in `org-mode' buffers.
`denote-link-ol-store' calls this unconditionally, even from
`markdown-mode' buffers, which makes `org-get-heading' warn via
`org-element-at-point' about running outside an Org buffer."
    (when (derived-mode-p 'org-mode)
      (apply fn args)))
  (advice-add 'denote-link-ol-get-heading :around
              #'ad:denote-link-ol-get-heading--org-only)

  (setq denote-link-description-format #'denote-dash-link-description-format))

(use-package consult-notes
  :ensure t
  :defer t
  :config
  (consult-notes-denote-mode 1)

  (defun ad:consult-notes--single-directory-string (fn &rest args)
    "Let-bind `denote-directory' to a single string for the duration of FN."
    (let ((denote-directory (denote-directories-get-common-root)))
      (apply fn args)))
  (advice-add 'consult-notes :around #'ad:consult-notes--single-directory-string)
  (advice-add 'consult-notes-search-in-all-notes :around #'ad:consult-notes--single-directory-string))

(use-package markdown-mode
  :ensure t
  :mode ("\\.mdwn" "\\.md" "\\.markdown" "\\.txt")
  :commands (tychoish/markdown-align-tables
             tychoish/markdown-align-tables-in-file
             tychoish/markdown-align-tables-dired)
  :config
  (font-lock-add-keywords 'markdown-mode tk-font-lock-keywords 'append)
  (font-lock-add-keywords 'gfm-mode tk-font-lock-keywords 'append)
  (defalias 'markdown-indent-code (kmacro "SPC SPC SPC SPC SPC C-a C-n"))
  (add-hook 'markdown-mode-hook 'turn-off-auto-fill)
  (defun tychoish/markdown-setup-imenu ()
    (setq imenu-generic-expression markdown-imenu-generic-expression))
  (add-hook 'markdown-mode-hook #'tychoish/markdown-setup-imenu)

  (defun tychoish/markdown-disable-colon-electric-indent ()
    "Prevent `:' from triggering `markdown-indent-line', which
resets point to column 0 outside of definition-list context."
    (setq-local electric-indent-chars (remq ?: electric-indent-chars)))
  (add-hook 'markdown-mode-hook #'tychoish/markdown-disable-colon-electric-indent)

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

(use-package flyspell
  :ensure t
  :defer t
  :init
  (defun tychoish--flyspell-run-in-buffer (buf fn)
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (if (eq fn #'flyspell-prog-mode)
            (funcall fn)
          (funcall fn 1)))))

  (defun tychoish--flyspell-enable-idle (fn)
    "Enable FN in the current buffer once Emacs is idle."
    (run-with-idle-timer 0.2 nil #'tychoish--flyspell-run-in-buffer (current-buffer) fn))

  (defun tychoish--flyspell-mode-idle ()
    "Enable `flyspell-mode' in the current buffer once Emacs is idle."
    (tychoish--flyspell-enable-idle #'flyspell-mode))

  (defun tychoish--flyspell-prog-mode-idle ()
    "Enable `flyspell-prog-mode' in the current buffer once Emacs is idle."
    (tychoish--flyspell-enable-idle #'flyspell-prog-mode))

  (keymap-set hud-mode-map "C-c [" #'flyspell-correct-next)
  (keymap-set hud-mode-map "C-c ]" #'flyspell-correct-previous)
  (keymap-set hud-mode-map "M-$" #'flyspell-correct-at-point)
  (keymap-set hud-mode-map "C-;" #'flyspell-correct-previous)

  (with-eval-after-load 'minaid
    (keymap-set minaid-consult-map "f" #'consult-flyspell))

  (add-hook 'prog-mode-hook #'tychoish--flyspell-prog-mode-idle)
  (add-hook 'text-mode-hook #'tychoish--flyspell-mode-idle)
  (add-hook 'telega-chat-mode-hook #'tychoish--flyspell-mode-idle)
  :config
  (defun consult-flyspell--round-trip ()
    (flyspell-correct-at-point)
    (consult-flyspell))

  (defvar tychoish--ispell-cache-file (sprite-state-path "ispell-aspell-cache.el")
    "Cached results of aspell's dictionary auto-detection probe.")

  (defvar tychoish--ispell-cache-vars
    '(ispell-last-program-name ispell-library-directory ispell-really-aspell
      ispell-really-hunspell ispell-really-enchant ispell-encoding8-command
      ispell-dictionary-alist ispell-aspell-dictionary-alist
      ispell--aspell-found-dictionaries ispell-aspell-dict-dir ispell-aspell-data-dir)
    "Ispell/aspell state variables to persist across sessions.")

  (defun tychoish--ispell-cache-params-around (orig-fn &rest args)
    "Skip aspell's dictionary-detection subprocess calls using a cached result.
`ispell-set-spellchecker-params' shells out to `aspell' several times (a
version check, `dicts', and `config dict-dir'/`data-dir') the first time
flyspell activates in a session; since flyspell is hooked to
`prog-mode-hook'/`text-mode-hook', that happens within 0.2s of the first
buffer. Aspell's installed dictionaries don't change between restarts on
the same machine, so load a cached result when present, and write one out
after the first real (uncached) probe."
    (when (file-exists-p tychoish--ispell-cache-file)
      (load tychoish--ispell-cache-file nil t))
    (let ((already-cached (equal ispell-last-program-name ispell-program-name)))
      (apply orig-fn args)
      (unless already-cached
        (with-temp-file tychoish--ispell-cache-file
          (insert ";; -*- lexical-binding: t; -*-\n")
          (seq-do (lambda (var) (insert (format "(setq %s '%S)\n" var (symbol-value var))))
                  tychoish--ispell-cache-vars)))))

  (advice-add 'ispell-set-spellchecker-params :around #'tychoish--ispell-cache-params-around)

  (defun tychoish/ispell-clear-aspell-cache ()
    "Delete the cached aspell dictionary-detection result.
Use after upgrading aspell or its dictionaries, since the cache would
otherwise keep replaying stale detection results."
    (interactive)
    (when (file-exists-p tychoish--ispell-cache-file)
      (delete-file tychoish--ispell-cache-file))
    (setq ispell-last-program-name nil))

  (setq ispell-list-command "list")
  (setq consult-flyspell-select-function 'consult-flyspell--round-trip)
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
  (setq flyspell-min-buffer-size (* flyspell-guess-size flyspell-guess-slots))
  (setq flyspell-guess-indicator nil))

(use-package whitespace
  :ensure nil
  :defer t
  :commands (toggle-local-whitespace-cleanup)
  :init
  (defun bootstrap-set-up-show-whitespace ()
    (setq-local show-trailing-whitespace t))

  (keymap-set hud-mode-map "C-c C-w" #'whitespace-cleanup)
  (keymap-set hud-whitespace-map "c" #'whitespace-cleanup)
  (keymap-set hud-whitespace-map "t" #'toggle-local-whitespace-cleanup)

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
  :config
  (add-hook 'yaml-mode-hook #'yaml-pro-mode)
  (add-hook 'yaml-mode-hook (hud-set-tab-width 2)))

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

  (add-to-list 'major-mode-remap-alist '((go-mode . go-ts-mode)))
  (add-to-list 'major-mode-remap-alist '((go-mod-mode . go-mod-ts-mode)))
  :config
  (require 'project)
  (add-hook 'project-find-functions #'project-find-go-module)

  (keymap-set go-ts-mode-map "C-c C-t c" #'builder-go-coverage-and-convert)

  (defun tychoish/go-mode-setup ()
    (setq-local tab-width 8)
    (setq-local fill-column 100)
    (setq-local compilation-error-screen-columns nil)
    (setq-local flycheck-disabled-checkers '(go-unconvert go-errcheck go-staticcheck go-vet go-build go-test go-gofmt golangci-lint))
    (when (fboundp 'cov-mode)
      (cov-mode 1)))

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
  :defer t
  :after (rust-mode flycheck)
  :mode (("\\.rs\\'" . rustic-mode)
	 ("Cargo.lock" . toml-ts-mode))
  :commands (rust-resolve-fmt-path)
  :config
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

  (when (executable-find "rustup")
    (setq rustic-rustfmt-args "+nightly"))

  (add-to-list 'flycheck-checkers 'rustic-clippy))

(use-package python
  :ensure nil
  :mode (("\\.py$" . python-ts-mode))
  :init
  (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
  :config
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

(use-package terraform-mode
  :ensure t
  :mode ("\\.tf" "\\.tfvars" "\\.tfvars.example")
  :config
  (setq terraform-format-on-save t)
  (setq terraform-indent-level 2))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; build/compilation support

(use-package flycheck
  :ensure t
  :defer t
  :defines (flycheck-checkers)
  :commands (flycheck-disable-checker)
  :init
  (keymap-set hud-mode-map "M-g f" #'flycheck-mode)
  (with-eval-after-load 'minaid
    (keymap-set minaid-consult-map "c" #'consult-flycheck))
  (keymap-set hud-mode-map "C-c f f" #'flycheck-mode)
  :config
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

  (keymap-set flycheck-command-map ";" #'consult-flycheck)

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
  (setq flycheck-go-vet-shadow t)
  (setq flycheck-go-build-install-deps nil)
  (setq flycheck-flake8-maximum-line-length 100))

(use-package compile
  :defer t
  :defines (compilation-mode-map)
  :commands (builder-compile)
  :init
  (keymap-set hud-core-map "c" #'builder-compile)
  :config
  (defun tychoish-compile-project-super-lint ()
    (interactive)
    (let* ((project-directory (projectile-project-root))
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

  (setq-default compilation-save-buffers-predicate #'projectile-project-root))

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
  :commands (tychoish/flycheck-vale-enable tychoish/flycheck-vale-disable)
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

(use-package cov
  :ensure t
  :defer t
  :commands (cov-mode cov-turn-on cov-turn-off)
  :config
  (setq cov-coverage-mode 'both)
  (setq cov-lcov-file-name "coverage/lcov.info")
  (setq cov-coverage-file-paths '("cover" "coverage" ".")))


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
  (setq dape-cwd-function #'projectile-project-root))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ROBOTS (AI) Integration
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package gptel
  :ensure t
  :defer t
  :functions (gptel-make-anthropic gptel-make-gh-copilot gptel-make-gemini)
  :config
  (defvar gemini-api-key nil)
  (defvar anthropic-api-key nil)
  (defvar openai-api-key nil)

  (cl-defmacro make-gptel-set-up-backend-functions (&key name model backend key api-key)
    (declare (indent defun) (debug (def-form form)))

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
           (or (message "[gptel] set backend to %s for the local buffer" ,name)))

	 (defun ,default-function-symbol ()
           ,(format "Set the default LLM backend for the current session to `%s'" model)
           (interactive)
           (setq-default gptel-model ,model)
           ,(when api-key
              `(setq-default gptel-api-key (lambda () ,api-key)))
           (setq-default gptel-backend ,backend)
           (or (message "[gptel] set default backend to %s" ,name)))

	 (keymap-set gptel-mode-map ,(format "C-c r a m %s" (upcase key)) #',default-function-symbol)
	 (keymap-set gptel-mode-map ,(format "C-c r a m %s" (downcase key)) #',local-function-symbol)
	 (keymap-set hud-robot-gptel-set-default-model-map ,(downcase key) #',default-function-symbol))))

  (keymap-set gptel-mode-map "C-c m" #'gptel-menu)

  (setq gptel-include-reasoning 'ignore)

  (make-gptel-set-up-backend-functions
   :name "sonnet-5"
   :key "s"
   :model 'claude-sonnet-5
   :backend (gptel-make-anthropic "claude-sonnet" :key anthropic-api-key :stream t)
   :api-key anthropic-api-key)

  (make-gptel-set-up-backend-functions
   :name "gemini-flash"
   :key "f"
   :model 'gemini-flash-latest
   :backend (gptel-make-gemini "gemini" :key gemini-api-key :stream t))

  (make-gptel-set-up-backend-functions
   :name "gemini-flash-preview"
   :key "f"
   :model 'gemini-3-flash-preview
   :backend (gptel-make-gemini "gemini" :key gemini-api-key :stream t))

  (make-gptel-set-up-backend-functions
   :name "gemini-flash-lite"
   :key "l"
   :model 'gemini-flash-lite-latest
   :backend (gptel-make-gemini "gemini" :key gemini-api-key :stream t))

  (make-gptel-set-up-backend-functions
   :name "copilot"
   :key "c"
   :model 'gpt-5-mini
   :backend (gptel-make-gh-copilot "copilot"))

  (make-gptel-set-up-backend-functions
   :name "gpt-5"
   :key "5"
   :model 'gpt-5
   :backend (gptel-make-openai "openai" :key openai-api-key)
   :api-key openai-api-key)

  (make-gptel-set-up-backend-functions
   :name "gpt-5.6-terra"
   :key "m"
   :backend (gptel-make-openai "openai" :key openai-api-key)
   :model 'gpt-5.6-terra
   :api-key openai-api-key)

  (gptel-set-backend-default-gemini-flash-preview)
  (require 'gptel-integrations))

(use-package gptel-mcp
  :after (gptel mcpkit)
  :commands (gptel-mcp-mode
             gptel-mcp-setup-session
             gptel-mcp-register-all-services
             gptel-mcp-enable
             gptel-mcp-disable)
  :config
  (setq gptel-mcp-default-services '(emacs denote))
  (gptel-mcp-mode 1))

(use-package agent-shell
  :ensure t
  :defer t
  :commands (agent-shell-resolve-permissions)
  :init
  (keymap-set hud-robot-agent-shell-map "o" #'agent-shell)
  (keymap-set hud-robot-agent-shell-map "t" #'agent-shell-toggle)
  (keymap-set hud-robot-agent-shell-map "b" #'agent-shell-switch-buffer)
  (keymap-set hud-robot-agent-shell-map "n" #'agent-shell-new-shell)
  (keymap-set hud-robot-agent-shell-map "e" #'agent-shell-new-temp-shell)
  (keymap-set hud-robot-agent-shell-map "w" #'agent-shell-new-worktree-shell)
  (keymap-set hud-robot-agent-shell-map "v" #'tychoish/agent-shell-toggle-terse-output)
  (keymap-set hud-robot-agent-shell-map "p" #'agent-shell-workflow-select)
  (keymap-set hud-robot-agent-shell-map "m" #'agent-shell-workflow-dispatch-menu)
  (keymap-set hud-robot-agent-shell-map "a" #'agent-shell-menu-select-action)
  (keymap-set hud-robot-agent-shell-send-map "d" #'agent-shell-send-dwim)
  (keymap-set hud-robot-agent-shell-send-map "r" #'agent-shell-send-region)
  (keymap-set hud-robot-agent-shell-send-map "f" #'agent-shell-send-file)
  (keymap-set hud-robot-agent-shell-send-map "t" #'agent-shell-send-file-to)
  (keymap-set hud-robot-agent-shell-send-map "b" #'agent-shell-menu-send-buffer)
  (keymap-set hud-robot-agent-shell-send-map "p" #'agent-shell-menu-send-file)
  (keymap-set hud-robot-agent-shell-send-map "y" #'agent-shell-yank-dwim)
  (make-read-extended-command-for-prefix "agent-shell"
    :bind-map hud-robot-agent-shell-map
    :bind-key "x")
  (with-eval-after-load 'which-key
    (push '((nil . "^agent-shell-") . (nil . "")) which-key-replacement-alist))

  (declare-function tychoish/agent-shell-antigravity-bootstrap "setup-core")

  (defun tychoish/agent-shell-antigravity-ensure-binary (&rest _)
    "Ensure Antigravity ACP server binary is installed synchronously before agent starts."
    (require 'agent-shell)
    (tychoish/agent-shell-antigravity-bootstrap nil :sync))

  (advice-add 'agent-shell-antigravity-start-agent :before
              #'tychoish/agent-shell-antigravity-ensure-binary)

  (add-lazy-init
   :name "<bootstrap> agent-shell antigravity"
   :delay 60
   :operation (lambda ()
                (require 'agent-shell)
                (tychoish/agent-shell-antigravity-bootstrap)))
  :config
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
	   "CLAUDE_CODE_DISABLE_EXPLORE_PLAN_AGENTS" "1"
	   "CLAUDE_AGENT_SDK_DISABLE_BUILTIN_AGENTS" "1"
	   "ENABLE_CLAUDEAI_MCP_SERVERS" "false"
           :inherit-env t))

    (setq agent-shell-omp-environment
	  (agent-shell-make-environment-variables
	   "ENABLE_CLAUDEAI_MCP_SERVERS" "false"
	   "CLAUDE_CODE_DISABLE_EXPLORE_PLAN_AGENTS" "1"
	   "CLAUDE_AGENT_SDK_DISABLE_BUILTIN_AGENTS" "1"
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
  (setq agent-shell-activity-group-expand-by-default t)
  (setq agent-shell-thought-process-expand-by-default t)
  (setq agent-shell-tool-use-expand-by-default nil)
  (setq agent-shell-user-message-expand-by-default nil)
  (require 'agent-shell-menu)
  (setq agent-shell-buffer-name-format #'agent-shell-menu-format-buffer-name)

  (defvar shell-maker--busy)

  (defun ad:agent-shell-recover-stuck-push-end (orig-fn &rest args)
    "Call ORIG-FN with ARGS, then recover a stuck busy state.

Works around a bug in `agent-shell-experimental--on-session-push-end'.
When a session_push_end notification arrives but its matching
session/push request is no longer in :active-requests (a stale or
duplicate notification), the function silently no-ops and never calls
its :on-finished callback. `shell-maker-finish-output' is the only place
that clears `shell-maker--busy' for ACP sessions, and it is invoked
exclusively from that callback, so `shell-maker--busy' is left t forever
with no user-visible failure."
    (apply orig-fn args)
    (when shell-maker--busy
      (message "agent-shell: recovering stuck busy state after session_push_end")
      (setq shell-maker--busy nil)
      (when-let* ((on-finished (plist-get args :on-finished)))
        (funcall on-finished))))
  (advice-add 'agent-shell-experimental--on-session-push-end
              :around #'ad:agent-shell-recover-stuck-push-end)

  (defun tychoish/agent-shell--force-clear-busy (shell-buffer request)
    "Force SHELL-BUFFER back to ready if REQUEST never got a response.
Some ACP agents accept `session/cancel' but never answer the
in-flight `session/prompt' request it targets. `shell-maker-finish-output'
is the only place that clears `shell-maker--busy', so without this the
shell stays wedged until the connection is restarted."
    (when (buffer-live-p shell-buffer)
      (with-current-buffer shell-buffer
        (when (and shell-maker--busy
                   (member request (map-elt agent-shell--state :active-requests)))
          (message "agent-shell: forcing recovery after interrupt got no response")
          (map-put! agent-shell--state :active-requests
                    (seq-remove (lambda (r) (equal r request))
                                (map-elt agent-shell--state :active-requests)))
          (setq shell-maker--busy nil)
          (shell-maker-finish-output :config shell-maker--config :success nil)))))

  (defun ad:agent-shell-interrupt-recover-if-unanswered (orig-fn &rest args)
    "Call ORIG-FN with ARGS, then arm a fallback for an unanswered cancel.
See `tychoish/agent-shell--force-clear-busy'."
    (let ((shell-buffer (current-buffer))
          (request (car (map-elt agent-shell--state :active-requests))))
      (apply orig-fn args)
      (when (and request (map-nested-elt agent-shell--state '(:session :id)))
        (run-with-timer 5 nil #'tychoish/agent-shell--force-clear-busy
                         shell-buffer request))))
  (advice-add 'agent-shell-interrupt
              :around #'ad:agent-shell-interrupt-recover-if-unanswered)

  (defun ad:acp-route-incoming-message-isolate-errors (orig-fn &rest args)
    "Call ORIG-FN with ARGS, isolating errors from response handling."
    (condition-case err
        (apply orig-fn args)
      (error
       (message "acp: error routing incoming message, isolated to avoid wedging the drain loop: %S" err)
       nil)))

  (advice-add 'acp--route-incoming-message :around #'ad:acp-route-incoming-message-isolate-errors)

  (defun tychoish/agent-shell--config-installed-p (config)
    "Return non-nil if CONFIG's agent executable is installed on the local system."
    (when-let* ((cm (map-elt config :client-maker))
                (client (ignore-errors (funcall cm (current-buffer))))
                (cmd-spec (map-elt client :command))
                (cmd (if (consp cmd-spec) (car cmd-spec) cmd-spec))
                ((stringp cmd))
                ((not (string-empty-p cmd))))
      (executable-find cmd t)))

  (defun tychoish/agent-shell-installed-configs ()
    "Return default agent configs whose CLI executable exists on `exec-path'."
    (seq-filter #'tychoish/agent-shell--config-installed-p
                (mapcar (lambda (e) (if (functionp e) (funcall e) e))
                        (agent-shell-default-agent-config-makers))))

  (setq agent-shell-agent-configs #'tychoish/agent-shell-installed-configs)

  (defun ad:agent-shell-insert-to-shell-buffer-advance-point (orig-fn &rest args)
    "Call ORIG-FN with ARGS, ensuring point and window-point advance to `point-max'."
    (prog1 (apply orig-fn args)
      (when-let* ((shell-buffer (plist-get args :shell-buffer))
                  ((buffer-live-p shell-buffer)))
        (with-current-buffer shell-buffer
          (goto-char (point-max))
          (when-let* ((win (get-buffer-window shell-buffer t)))
            (set-window-point win (point-max)))))))

  (advice-add 'agent-shell--insert-to-shell-buffer :around #'ad:agent-shell-insert-to-shell-buffer-advance-point)

  (declare-function agent-shell-ui--group-header-range "agent-shell-ui")
  (declare-function agent-shell-ui--block-range "agent-shell-ui")

  (cl-defun ad:agent-shell-ui--group-children-bounded (&key group-qualified-id)
    "Reimplementation of `agent-shell-ui--group-children' that cannot spin forever."
    (when-let* ((header (agent-shell-ui--group-header-range group-qualified-id)))
      (save-mark-and-excursion
        (let ((children '())
              (pos (map-elt header :end)))
          (catch 'done
            (while t
              (goto-char pos)
              (skip-chars-forward " \t\n")
              (when (eobp) (throw 'done nil))
              (let ((state (get-text-property (point) 'agent-shell-ui-state)))
                (unless (and state (equal (map-elt state :group-id) group-qualified-id))
                  (throw 'done nil))
                (let* ((block (agent-shell-ui--block-range :position (point)))
                       (end (map-elt block :end)))
                  ;; Bail instead of spinning forever when the block
                  ;; doesn't advance past `pos'.
                  (when (or (null end) (<= end pos))
                    (throw 'done nil))
                  (push (list (cons :qualified-id (map-elt state :qualified-id))
                              (cons :start (map-elt block :start))
                              (cons :end end))
                        children)
                  (setq pos end)))))
          (nreverse children)))))
  (advice-add 'agent-shell-ui--group-children
              :override #'ad:agent-shell-ui--group-children-bounded)

  (setq agent-shell-anthropic-authentication (agent-shell-anthropic-make-authentication :login t))
  (setq agent-shell-anthropic-default-session-mode-id "auto")
  (setq agent-shell-pi-acp-command '("npx" "-y" "pi-acp"))
  (setq agent-shell-cursor-acp-command '("cursor-agent" "acp"))

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
  (add-hook 'agent-shell-mode-hook #'ansi-color-for-comint-mode-on)

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
    (unless (and-let* ((agent-name (map-nested-elt agent-shell--state '(:agent-config :mode-line-name)))
                       ((equal agent-name "Claude"))
                       (title (map-nested-elt agent-shell--state '(:session :title)))
                       ((stringp title))
                       ((not (string-empty-p title)))))
      (funcall orig-fn event)))

  (advice-add 'agent-shell--refresh-session-title :around
              #'ad:agent-shell--refresh-session-title)

  ;; MCPKit dynamic session integration
  (defun agent-shell-mcpkit-active-url ()
    "Return HTTP URL for current Emacs session's active MCP server, or nil."
    (when-let* ((port (or (and (fboundp 'mcpkit-active-port) (mcpkit-active-port))
                          (and (fboundp 'bootstrap-init-mcp-services)
                               (progn (bootstrap-init-mcp-services) (mcpkit-active-port)))
                          (and (fboundp 'mcpkit-start-all-services)
                               (mcpkit-start-all-services)))))
      (if (and (fboundp 'mcpkit-proxy-active-port)
               (mcpkit-proxy-active-port)
               (bound-and-true-p sprite-instance-id))
          (format "http://127.0.0.1:%d/sprite/%s/mcp"
                  (mcpkit-proxy-active-port)
                  sprite-instance-id)
        (format "http://127.0.0.1:%d/mcp" port))))

  (defun agent-shell-mcpkit-server-spec ()
    "Return normalized ACP McpServer specification for current session's MCPKit."
    (when-let* ((url (agent-shell-mcpkit-active-url)))
      `((name . "emacs")
        (type . "http")
        (url . ,url)
        (headers . ()))))

  (setq agent-shell-mcp-servers
        (list (lambda () (agent-shell-mcpkit-server-spec))))

  (defun ad:agent-shell--mcp-servers-filter-nil (orig-fn &rest args)
    "Filter out null server entries produced when dynamic MCP resolvers yield nil."
    (when-let* ((servers (apply orig-fn args)))
      (let ((filtered (seq-filter (lambda (s) (map-elt s 'name)) servers)))
        (when (> (length filtered) 0)
          (apply #'vector (append filtered nil))))))

  (advice-add 'agent-shell--mcp-servers :around #'ad:agent-shell--mcp-servers-filter-nil)

  (defun agent-shell-mcpkit-environment-variables ()
    "Return environment variables exposing current Emacs session MCPKit info."
    (when-let* ((port (ignore-errors (and (fboundp 'mcpkit-active-port) (mcpkit-active-port)))))
      (list (format "MCPKIT_PORT=%d" port)
            (format "EMACS_MCP_PORT=%d" port)
            (format "EMACS_MCP_URL=http://127.0.0.1:%d/mcp" port)
            (format "EMACS_SPRITE_ID=%s" (or (bound-and-true-p sprite-instance-id) (system-name)))
            (format "EMACS_SERVER_NAME=%s" (or (bound-and-true-p server-name) "server")))))

  (defun ad:agent-shell--make-acp-client-inject-mcpkit-env (orig-fn &rest args)
    "Inject current session's MCPKit environment variables into ACP client."
    (let* ((env (plist-get args :environment-variables))
           (extra (agent-shell-mcpkit-environment-variables)))
      (if extra
          (apply orig-fn (plist-put (copy-sequence args) :environment-variables (append extra env)))
        (apply orig-fn args))))

  (advice-add 'agent-shell--make-acp-client :around
              #'ad:agent-shell--make-acp-client-inject-mcpkit-env)

  (defun tychoish/agent-shell-antigravity--do-bootstrap (install-dir platform-sym bin-path)
    "Download and install the Antigravity ACP server binary to INSTALL-DIR."
    (require 'url)
    (require 'json)
    (require 'map)
    (make-directory install-dir t)
    (let* ((reg-url "https://raw.githubusercontent.com/agentclientprotocol/registry/main/antigravity-acp/agent.json")
           (reg (with-temp-buffer
                  (url-insert-file-contents reg-url)
                  (json-parse-buffer :object-type 'alist)))
           (archive-url (map-nested-elt reg (list 'distribution 'binary platform-sym 'archive)))
           (zip (expand-file-name "server.zip" install-dir)))
      (url-copy-file archive-url zip t)
      (call-process "unzip" nil nil nil "-o" "-q" zip "-d" install-dir)
      (delete-file zip)
      (set-file-modes bin-path #o755)
      bin-path))

  (defcustom agent-shell-antigravity-bootstrap-timeout 300
    "Timeout in seconds waiting for Antigravity ACP server bootstrap on a sprite."
    :type 'integer
    :group 'agent-shell)

  (defvar tychoish/agent-shell-antigravity--future nil
    "In-flight `sprite-future' for Antigravity ACP server bootstrap, or nil.")

  (defun tychoish/agent-shell-antigravity-bootstrap (&optional force sync)
    "Download and install the latest Antigravity ACP server binary via sprite.
When FORCE is non-nil, re-download even if already present.
When SYNC is non-nil, wait synchronously for completion; otherwise dispatch
asynchronously via `sprite-future-then'."
    (interactive "P")
    (require 'sprite-future)
    (require 'agent-shell)
    (let* ((arch (pcase (car (split-string system-configuration "-"))
                   ((or "x86_64" "amd64") "x86_64")
                   ((or "aarch64" "arm64") "aarch64")
                   (a a)))
           (os (pcase system-type ('darwin "darwin") ('windows-nt "windows") (_ "linux")))
           (platform (format "%s-%s" os arch))
           (platform-sym (intern platform))
           (install-dir (agent-shell-cache-dir "antigravity" platform))
           (bin-name (file-name-nondirectory (car agent-shell-antigravity-acp-command)))
           (bin-path (expand-file-name bin-name install-dir)))
      (if (and (not force) (file-executable-p bin-path))
          (progn
            (setq agent-shell-antigravity-acp-command
                  (cons bin-path (cdr agent-shell-antigravity-acp-command)))
            (message "Antigravity: using ACP server at %s" bin-path)
            (when (called-interactively-p 'interactive)
              (when (fboundp 'alert)
                (alert (format "ACP server ready at %s" bin-path)
                       :title "Antigravity Bootstrap")))
            bin-path)
        ;; Check if a bootstrap future is already pending
        (if (and tychoish/agent-shell-antigravity--future
                 (sprite-future-pending-p tychoish/agent-shell-antigravity--future))
            (if sync
                (progn
                  (message "Antigravity: waiting for in-flight bootstrap...")
                  (let ((path (sprite-future-wait tychoish/agent-shell-antigravity--future
                                                  :timeout agent-shell-antigravity-bootstrap-timeout)))
                    (setq tychoish/agent-shell-antigravity--future nil)
                    (if (and path (file-executable-p path))
                        (progn
                          (setq agent-shell-antigravity-acp-command
                                (cons path (cdr agent-shell-antigravity-acp-command)))
                          path)
                      (error "Antigravity: in-flight bootstrap failed or timed out"))))
              (message "Antigravity: bootstrap already in flight...")
              tychoish/agent-shell-antigravity--future)
          (message "Antigravity: bootstrapping server for %s via sprite..." platform)
          (let ((future (condition-case err
                            (sprite-future-eval
                             (sprite-name (sprite-get-or-create-next :timeout 60))
                             `(progn
                                (require 'agent-shell)
                                (tychoish/agent-shell-antigravity--do-bootstrap ,install-dir ',platform-sym ,bin-path)))
                          (error
                           (message "Antigravity: failed to dispatch bootstrap to sprite: %S" err)
                           (when (fboundp 'alert)
                             (alert (format "Bootstrap dispatch failed: %S" err)
                                    :title "Antigravity Bootstrap"))
                           nil))))
            (when future
              (setq tychoish/agent-shell-antigravity--future future)
              (if sync
                  (condition-case err
                      (let ((path (sprite-future-wait future :timeout agent-shell-antigravity-bootstrap-timeout)))
                        (setq tychoish/agent-shell-antigravity--future nil)
                        (when (or (not path) (sprite-future-rejected-p future))
                          (error "sprite bootstrap failed or timed out"))
                        (setq agent-shell-antigravity-acp-command
                              (cons path (cdr agent-shell-antigravity-acp-command)))
                        (message "Antigravity: using ACP server at %s" path)
                        (when (fboundp 'alert)
                          (alert (format "ACP server ready at %s" path)
                                 :title "Antigravity Bootstrap"))
                        path)
                    (error
                     (setq tychoish/agent-shell-antigravity--future nil)
                     (message "Antigravity: bootstrap failed: %S" err)
                     (when (fboundp 'alert)
                       (alert (format "Bootstrap failed: %S" err)
                              :title "Antigravity Bootstrap"))
                     nil))
                (sprite-future-then
                 future
                 (lambda (path)
                   (setq tychoish/agent-shell-antigravity--future nil)
                   (if (and path (file-exists-p path))
                       (progn
                         (set-file-modes path #o755)
                         (setq agent-shell-antigravity-acp-command
                               (cons path (cdr agent-shell-antigravity-acp-command)))
                         (message "Antigravity: using ACP server at %s" path)
                         (when (fboundp 'alert)
                           (alert (format "ACP server ready at %s" path)
                                  :title "Antigravity Bootstrap")))
                     (message "Antigravity: bootstrap completed but binary missing: %S" path)
                     (when (fboundp 'alert)
                       (alert (format "Bootstrap failed: binary missing %S" path)
                              :title "Antigravity Bootstrap"))))
                 (lambda (err)
                   (setq tychoish/agent-shell-antigravity--future nil)
                   (message "Antigravity: bootstrap failed: %S" err)
                   (when (fboundp 'alert)
                     (alert (format "Bootstrap failed: %S" err)
                            :title "Antigravity Bootstrap"))))
                future)))))))

  (tychoish/agent-shell--apply-environment))

(use-package agent-shell-queue
  :ensure t
  :after agent-shell
  :defer t
  :commands (agent-shell-queue-item-menu
             agent-shell-queue-buffer-open
             agent-shell-queue-capture)
  :config
  (defvar-keymap hud-robot-agent-shell-map)
  (setq agent-shell-queue-write-log-enabled t)
  (require 'agent-shell-queue-org)
  (setq agent-shell-queue-serialization-format 'org)
  (setq agent-shell-queue-default-pause-delay 30.0)
  (setq agent-shell-queue-stall-timeout 600)
  ;; Route agent-shell's built-in busy-prompt queue through agent-shell-queue
  ;; for persistence, introspection, and pause/resume instead of the
  ;; in-memory ring `agent-shell-prompt-queue' uses on its own.
  (agent-shell-queue-overload-mode 1)
  (keymap-set agent-shell-queue-mode-map "C-c j" '(hud-robot-agent-shell-map . "robot-agent-shell"))
  (keymap-set hud-robot-agent-shell-map "q" #'agent-shell-queue-buffer-open)
  (keymap-set hud-robot-agent-shell-map "/" #'agent-shell-queue-capture)
  (keymap-unset agent-shell-mode-map "<spc>")

  (with-eval-after-load 'agent-shell-menu
    (agent-shell-menu-mode-key "?" agent-shell-menu-dispatch))

  (agent-shell-menu-mode-key "p" agent-shell-menu-resolve-permission)
  (agent-shell-menu-mode-key "/" agent-shell-prompt-queue)
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
                ((eq agent-shell-queue-serialization-format 'org) "org")
                (t "el"))))
      (expand-file-name (concat "queue." ext)
                        (sprite-state-path "agent-shell"))))

  (defun tychoish--agent-shell-queue-archive-file ()
    "Archive file under the per-instance agent-shell state directory."
    (expand-file-name "queue-archive.jsonl"
                      (sprite-state-path "agent-shell")))

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

(use-package agent-shell-notifications
  :ensure t
  :defer t
  :after (agent-shell alert)
  :commands (agent-shell-notifications-mode)
  :init
  (add-hook 'agent-shell-mode-hook #'agent-shell-notifications-mode)
  (add-hook 'agent-shell-viewport-edit-mode-hook #'agent-shell-notifications-mode)
  (add-hook 'agent-shell-viewport-edit-view-hook #'agent-shell-notifications-mode)
  :config
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


;;; MCP Services (mcpkit)

(use-package mcpkit
  :commands (mcpkit-start-all-services
             mcpkit-stop-all-services
             mcpkit-restart-all-services
             mcpkit-services-status
             mcpkit-allocate-free-port))

(provide 'setup-core)
;;; setup-core.el ends here
