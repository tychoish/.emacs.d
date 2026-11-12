;;; setup-core.el -- contains all major use-package forms -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Package hooks, with-eval-after-load forms, and mode integration glue
;; for packages configured outside use-package.

;; This configuration optimizes for lazy-loading so that configuration
;; only loads when called directly or a mode is activated.

;;; Code:

(require 'cl-lib)

(require 'cmdr-macros)
(require 'cmdr-keys)
(require 'cmdr-spec)

(declare-function electric-pair-default-inhibit "elec-pair")
(declare-function electric-pair-conservative-inhibit "elec-pair")

(defvar denote-directory)
(defvar magit-mode-map)
(defvar magit-status-mode-map)
(defvar magit-module-sections-nested)
(defvar popper-reference-buffers)
(defvar popper-echo-dispatch-keys)
(defvar popper-display-control)
(defvar sprite-mode-map-prefix)
(defvar alert-default-style)
(defvar alert-fade-time)
(defvar alert-libnotify-command)
(defvar alert-log-messages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLI flags

(require 'clix)

(register-clix-flag slow-op-timing
  :flag "--with-slow-op-timing" :doc "Enable slow operation timing." :consume nil
  :handler (message "[op]: enabling time reporting"))

(register-clix-flag sprite-id
  :flag "--id" :doc "Specify Emacs instance ID for sprite."
  :var sprite-cli-instance-id)

(register-clix-flag org-exec
  :flag "--org-exec" :doc "Execute an Org file's Babel blocks."
  :action (progn (require 'org) (require 'ob) (require 'builder-tasks)
                 (builder-org-babel-execute-file clix-arg-org-exec))
  :final t)

(register-clix-flag org-exec-dir
  :flag "--org-exec-dir" :doc "Execute Org Babel blocks in a directory."
  :action (progn (require 'org) (require 'ob) (require 'builder-tasks)
                 (builder-org-babel-execute-directory clix-arg-org-exec-dir))
  :final t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cmdr keybindings & specifications

(defvar tychoish-core-map (cmdr-keys-prefix cmdr-mode-map "C-c t" "core")
  "The cmdr core prefix map under C-c t.")

(setq sprite-mode-map-prefix (cons "z" 'tychoish-core-map))

(cmdr-keys-prefix cmdr-mode-map "C-c t t" "theme")
(cmdr-keys-set cmdr-mode-map "C-c t t r" #'disable-all-themes)
(cmdr-keys-set cmdr-mode-map "C-c t t i" #'toggle-hud-modeline-icons)
(cmdr-keys-set cmdr-mode-map "C-c t t s" #'toggle-hud-modeline-show-buffer-size)
(cmdr-keys-set cmdr-mode-map "C-c t s" #'daemons-dash)
(cmdr-keys-set cmdr-mode-map "C-c h s" #'eglox-describe-symbol-dwim)
(cmdr-keys-set cmdr-mode-map "C-c m a" #'tychoish-mail-select-account)
(cmdr-keys-set cmdr-mode-map "C-x C-b s" #'projectile-save-project-buffers-picker)

(cmdr-register-spec
  :name mail-accounts
  :probe tychoish-mail-select-account
  :commands ((cmdr-spec-command :category mu4e
                                :command tychoish-mail-select-account
                                :display-name "mail-select-account"
                                :description "select mail account"
                                :transient-key "ma")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; core emacs -- global settings and options

(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)

(setq jit-lock-defer-time 0.2)

(setq confirm-kill-processes nil)
(setq confirm-nonexistent-file-or-buffer nil)
(setq select-enable-clipboard nil)

(setq auto-revert-verbose nil)
(setq auto-revert-avoid-polling t)
(setq auto-revert-interval 60)
(setq backup-by-copying t)
(setq delete-old-versions t)

(setq vc-follow-symlinks t)
(setq version-control t)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

(setq-default cursor-in-non-selected-windows nil)
(setq scroll-conservatively 25)
(setq scroll-preserve-screen-position t)

(setq ring-bell-function #'ignore)
(setq-default indicate-empty-lines t)
(setq-default truncate-lines t)
(setq-default indent-tabs-mode nil) ; (setq tab-width 4)
(setq shell-command-dont-erase-buffer 'end-last-out)

(setq switch-to-prev-buffer-skip 'visible)
(setq split-height-threshold 100)
(setq window-sides-vertical t)

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'list-threads 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; platform/os -- system configuration and integration

;;; Direct terminal file aliases
(add-to-list 'term-file-aliases '("alacritty" . "xterm"))
(add-to-list 'term-file-aliases '("ghostty" . "xterm-ghostty"))

(when (eq system-type 'darwin)
  (setq read-process-output-max (* 64 1024))
  (setq ns-function-modifier 'hyper)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))

(when (eq system-type 'gnu/linux)
  (setq read-process-output-max (* 1024 1024)))

(advice-add 'display-startup-echo-area-message :override #'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; third-party -- configuration

(setq savehist-coding-system 'utf-8-emacs)
(setq desktop-buffers-not-to-save
      (concat "\\` \\|\\("
              "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS\\|"
              "\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
              "\\)$"))

(setq desktop-files-not-to-save
      (concat "\\(\\`/[^/:]*:\\|(ftp)\\'\\)\\|"
              "^/usr/lib/go/.*\\|"
              "^/usr/lib/rustlib/.*\\|"
              "^/home.+go/pkg/mod\\|"
              "^/home.+\\.cargo"))

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

(setq whitespace-style '(face trailing tabs spaces lines newline missing-newline-at-eof empty space-mark tab-mark newline-mark))

(setq alert-log-messages nil)
(setq alert-fade-time 15)
(setq tracking-max-mode-line-entries 3)
(setq docker-terminal-backend 'eat)
(setq inferior-lisp-program "sbcl")

(setq which-key-idle-delay 0.25)
(setq which-key-idle-secondary-delay 0.125)
(setq which-key-lighter "")

(setq emojify-emoji-styles '(ascii unicode github))
(setq emojify-display-style 'unicode)
(setq modus-themes-deuteranopia t)
(setq modus-themes-common-palette-overrides
        '((border-mode-line-active bg-mode-line-active)
          (border-mode-line-inactive bg-mode-line-inactive)
          (message-separator bg-main)))

(setq ollama-tailnet-fallback-strategy :auto-local)

(setq denote-dash-default-filter '(not "journal"))
(setq denote-review-insert-after "date")
(setq denote-known-keywords '("project" "reference" "journal" "idea" "writing" "agent" "plan"))
(setq denote-save-buffers t)
(setq denote-prompts '(title keywords file-type))
(setq denote-journal-title-format nil)
(setq denote-sequence-scheme 'alphanumeric)
(setq denote-explore-network-format "graphviz")
(setq denote-link-description-format #'denote-dash-link-description-format)
(setq denote-rename-buffer-function #'denote-dash-rename-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Core & UI systems

(with-eval-after-load 'elec-pair
  (add-to-list 'electric-pair-pairs '(?< . ?>))
  (add-to-list 'electric-pair-pairs '(?{ . ?}))
  (add-to-list 'electric-pair-pairs '(?\[ . ?\])))

(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables 'denote--title-history)
  (add-to-list 'savehist-additional-variables 'denote--keywords-history)
  (add-to-list 'savehist-additional-variables 'compile-history))

(with-eval-after-load 'desktop
  (add-to-list 'desktop-modes-not-to-save 'dired-mode)
  (add-to-list 'desktop-modes-not-to-save 'Info-mode)
  (add-to-list 'desktop-modes-not-to-save 'org-mode)
  (add-to-list 'desktop-modes-not-to-save 'eww-mode)
  (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
  (add-to-list 'desktop-modes-not-to-save 'fundamental-mode))

(with-eval-after-load 'warnings
  (add-to-list 'warning-suppress-log-types '(frameset))
  (add-to-list 'warning-suppress-types '(files missing-lexbind-cookie)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shells & communication

(with-eval-after-load 'comint
  (keymap-set comint-mode-map "M-n" #'comint-next-input)
  (keymap-set comint-mode-map "M-p" #'comint-previous-input)
  (keymap-set comint-mode-map "<down>" #'comint-next-matching-input-from-input)
  (keymap-set comint-mode-map "<up>" #'comint-previous-matching-input-from-input))

(with-eval-after-load 'agent-shell
  (require 'setup-agent-shell))

(with-eval-after-load 'erc
  (require 'tracking))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Search & navigation

(keymap-set isearch-mode-map "C-o" #'isearch-occur)

(with-eval-after-load 'dired
  (keymap-set dired-mode-map "w" #'wdired-change-to-wdired-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Build & system tools

;; cmdr-ecosystem installs bindings; these configure package-specific menus.
(with-eval-after-load 'elpaish
  (cmdr-transient-insert-suffix-once 'elpaish-menu '(-1 0) '("x" "extended elpaish commands" execute-extended-elpaish-command)))

(with-eval-after-load 'docker
  (cmdr-transient-insert-suffix-once 'docker '(-1 0) '("m" "emacs docker commands" execute-extended-docker-command)))

(with-eval-after-load 'ollama-tailnet
  (cmdr-transient-insert-suffix-once 'ollama-transient-menu '(-1 0) '("x" "emacs ollama commands" execute-extended-ollama-command)))

(with-eval-after-load 'magit-gh
  (cmdr-transient-insert-suffix-once 'magit-gh '(-1) ["magit-dash" ("g" "magit-dash: prune/CI logs/PR comments/gh auth" magit-dash-gh-menu)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Git & VCS

(with-eval-after-load 'magit
  (setq magit-module-sections-nested t)
  (put 'magit-diff-edit-hunk-commit 'disabled nil)
  (add-hook 'magit-status-sections-hook 'magit-insert-worktrees t)
  (add-hook 'magit-status-sections-hook 'magit-insert-modules t)

  (defvar-keymap magit-command-mode-map) ;; "/"
  (keymap-set magit-mode-map "/" (cons "magit-command" magit-command-mode-map))
  (keymap-set magit-status-mode-map "C-n" #'next-line)
  (keymap-set magit-status-mode-map "C-p" #'previous-line)
  (keymap-set magit-command-mode-map "x" (cons "magit-commands" #'execute-extended-magit-command))
  (keymap-set magit-command-mode-map "m" (cons "(s)merge-commands" #'execute-extended-magit-command))

  (when (fboundp 'hud-modeline-set-segment-action)
    (hud-modeline-set-segment-action 'vc #'magit-dispatch)))

(with-eval-after-load 'git-link
  (defun ad:git-link--new-copy-to-clipboard (link)
    "Also copy LINK to the system clipboard."
    (with-temp-buffer
      (insert link)
      (clipboard-kill-ring-save (point-min) (point-max))))

  (advice-add 'git-link--new :after #'ad:git-link--new-copy-to-clipboard))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Denote & notes

(with-eval-after-load 'denote
  (add-to-list 'display-buffer-alist '("\\`\\*denote-sequence-hierarchy" (display-buffer-reuse-window display-buffer-pop-up-window)))

  (defun ad:denote-link-ol-get-heading--org-only (fn &rest args)
    "Only call FN in `org-mode' buffers."
    (when (derived-mode-p 'org-mode)
      (apply fn args)))

  (advice-add 'denote-link-ol-get-heading :around #'ad:denote-link-ol-get-heading--org-only))

(with-eval-after-load 'denote-dash
  (keymap-set denote-dash-org-map "r" #'orgx-denote-migrate-subtree)
  (make-read-extended-command-for-prefix "denote"
    :bind-map denote-dash-global-map
    :bind-key "x"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; AI & LLM integration

(with-eval-after-load 'gptel
  (keymap-set gptel-mode-map "C-c m" #'gptel-menu)
  (setq gptel-include-reasoning 'ignore)
  (setq-default gptel-backend (gptel-make-openai-oauth "codex"))
  (setq-default gptel-model 'gpt-6.1-terra)
  (gptel-make-gh-copilot "gh-copilot"))

(provide 'setup-core)
;;; setup-core.el ends here
