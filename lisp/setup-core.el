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
(require 'setup-modes)

(declare-function sprite-state-path "sprite-state")
(declare-function electric-pair-default-inhibit "elec-pair")
(declare-function electric-pair-conservative-inhibit "elec-pair")
(declare-function arch-enable-desktop-notifications "arch")

(defvar denote-directory)
(defvar magit-mode-map)
(defvar magit-status-mode-map)
(defvar magit-module-sections-nested)
(defvar popper-reference-buffers)
(defvar popper-echo-dispatch-keys)
(defvar popper-display-control)
(defvar cov-coverage-mode)
(defvar cov-lcov-file-name)
(defvar cov-coverage-file-paths)
(defvar dape-key-prefix)
(defvar dape-buffer-window-arrangement)
(defvar dape-cwd-function)
(defvar rustic-use-rust-save-some-buffers)
(defvar rustic-clippy-arguments)
(defvar rustic-rustfmt-args)
(defvar sprite-mode-map-prefix)
(defvar alert-default-style)
(defvar alert-fade-time)
(defvar alert-libnotify-command)
(defvar alert-log-messages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLI flags

(eval-when-compile
  (require 'clix))

(register-clix-flag slow-op-timing
  :flag "--with-slow-op-timing" :doc "Enable slow operation timing." :consume nil
  :handler (message "[op]: enabling time reporting"))

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

(autoload 'eglox-describe-symbol-dwim "eglox" nil t)
(autoload 'projectile-save-project-buffers-picker "setup-project" nil t)

(defvar tychoish-core-map (cmdr-keys-prefix cmdr-mode-map "C-c t" "core")
  "The cmdr core prefix map under C-c t.")

(setq sprite-mode-map-prefix (cons "z" 'tychoish-core-map))

(advice-add 'set-frame-font :around #'cmdr-set-frame-font-interactive)

(cmdr-keys-prefix cmdr-mode-map "C-c t t" "theme")
(cmdr-keys-set cmdr-mode-map "C-c t t r" #'disable-all-themes)
(cmdr-keys-set cmdr-mode-map "C-c t t i" #'toggle-hud-modeline-icons)
(cmdr-keys-set cmdr-mode-map "C-c t t s" #'toggle-hud-modeline-show-buffer-size)
(cmdr-keys-set cmdr-mode-map "C-c t p" #'toggle-electric-pair-inhibition)
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
;;; Core & UI systems

(defvar electric-pair-inhibition nil)
(defvar electric-pair-eagerness t)

(create-toggle-functions electric-pair-inhibition)

(defun setup-electric-pair-inhibition (char)
  (if electric-pair-inhibition
      nil
    (if electric-pair-eagerness
        (electric-pair-default-inhibit char)
      (electric-pair-conservative-inhibit char))))

(with-eval-after-load 'elec-pair
  (setq electric-indent-chars '(?\n ?:))
  (setq electric-pair-inhibit-predicate #'setup-electric-pair-inhibition)
  (add-to-list 'electric-pair-pairs '(?< . ?>)))

(with-eval-after-load 'savehist
  (setq savehist-coding-system 'utf-8-emacs)
  (add-to-list 'savehist-additional-variables 'compile-history))

(with-eval-after-load 'desktop
  (add-to-list 'desktop-globals-to-save 'register-alist)
  (add-to-list 'desktop-globals-to-save 'file-name-history)
  (add-to-list 'desktop-modes-not-to-save 'dired-mode)
  (add-to-list 'desktop-modes-not-to-save 'Info-mode)
  (add-to-list 'desktop-modes-not-to-save 'org-mode)
  (add-to-list 'desktop-modes-not-to-save 'eww-mode)
  (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
  (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

  (setq desktop-buffers-not-to-save
        (concat "\\("
                "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS\\|"
                "\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
                "\\)$"))

  (setq desktop-files-not-to-save
        (concat "\\(\\`/[^/:]*:\\|(ftp)\'\\)"
                "^/usr/lib/go/.*\\|"
                "^/usr/lib/rustlib/.*\\|"
                "^/home.+go/pkg/mod\\|"
                "^/home.+\\.cargo")))

(with-eval-after-load 'popper
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
  (setq popper-display-control nil))

(with-eval-after-load 'whitespace
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

(with-eval-after-load 'warnings
  (add-to-list 'warning-suppress-log-types '(frameset))
  (add-to-list 'warning-suppress-types '(files missing-lexbind-cookie)))

(with-eval-after-load 'which-key
  (setq which-key-idle-delay 0.25
        which-key-idle-secondary-delay 0.125
        which-key-lighter ""))

(with-eval-after-load 'modus-themes
  (setq modus-themes-deuteranopia t
        modus-themes-common-palette-overrides
        '((border-mode-line-active bg-mode-line-active)
          (border-mode-line-inactive bg-mode-line-inactive)
          (message-separator bg-main))))

(defun tychoish-alert-default-style ()
  "Determine the preferred `alert' style for the current environment."
  (cond
   ((eq system-type 'darwin)
    (if (display-graphic-p) 'osx-notifier 'notifier))
   (alert-libnotify-command
    'libnotify)
   ((and (eq system-type 'gnu/linux) (getenv "DBUS_SESSION_BUS_ADDRESS"))
    'notifications)
   (t
    'message)))

(defun tychoish-alert-darwin-server-init ()
  "Switch `alert-default-style' to `osx-notifier' on first GUI client frame."
  (when (display-graphic-p)
    (setq alert-default-style 'osx-notifier)
    (remove-hook 'server-after-make-frame-hook #'tychoish-alert-darwin-server-init)))

(with-eval-after-load 'alert
  (setq alert-log-messages nil
        alert-fade-time 15
        alert-default-style (tychoish-alert-default-style))

  (when (and (eq system-type 'darwin) (daemonp))
    (add-hook 'server-after-make-frame-hook #'tychoish-alert-darwin-server-init)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Icons & display

(with-eval-after-load 'nerd-icons
  (add-to-list 'nerd-icons-mode-icon-alist
               '(agent-shell-queue-item-view-mode nerd-icons-codicon "nf-cod-checklist" :face nerd-icons-green))

  (defun ad:nerd-icons-icon-for-buffer-safe (orig &rest args)
    "Return empty string instead of signaling for an unresolvable buffer icon."
    (condition-case nil
	(apply orig args)
      (error "")))

  (advice-add 'nerd-icons-icon-for-buffer :around #'ad:nerd-icons-icon-for-buffer-safe))

(with-eval-after-load 'nerd-icons-completion
  (cl-defmethod nerd-icons-completion-get-icon :around (cand (_cat (eql buffer)))
    "Skip icon lookup when CAND names a killed buffer."
    (if (buffer-live-p (get-buffer cand))
        (cl-call-next-method)
      "")))

(with-eval-after-load 'emojify
  (setq emojify-emoji-styles '(ascii unicode github)
        emojify-display-style 'unicode
        emojify-point-entered-behaviour 'echo))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shells & communication

(with-eval-after-load 'comint
  (keymap-set comint-mode-map "M-n" #'comint-next-input)
  (keymap-set comint-mode-map "M-p" #'comint-previous-input)
  (keymap-set comint-mode-map "<down>" #'comint-next-matching-input-from-input)
  (keymap-set comint-mode-map "<up>" #'comint-previous-matching-input-from-input))

(with-eval-after-load 'eshell
  (with-eval-after-load "em-cmpl"
    (add-hook 'eshell-mode-hook #'eshell-cmpl-initialize)))

(with-eval-after-load 'agent-shell
  (require 'setup-agent-shell))

(with-eval-after-load 'erc
  (require 'tracking))

(with-eval-after-load 'tracking
  (setq tracking-max-mode-line-entries 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Search & navigation

(keymap-set isearch-mode-map "C-o" #'isearch-occur)

(with-eval-after-load 'dired
  (setq ls-lisp-dirs-first t)
  (keymap-set dired-mode-map "w" #'wdired-change-to-wdired-mode))

(with-eval-after-load 'dabbrev
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'authinfo-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(with-eval-after-load 'anzu
  (setq anzu-cons-mode-line-p nil)
  (seq-do #'make-variable-buffer-local
          '(anzu--total-matched anzu--current-position anzu--state
            anzu--cached-count anzu--cached-positions anzu--last-command
            anzu--last-isearch-string anzu--overflow-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Build & system tools

(with-eval-after-load 'builder-core
  (require 'builder-tasks))

(with-eval-after-load 'arch
  (arch-enable-desktop-notifications)
  (run-with-idle-timer 2 nil #'arch--populate-cache))

(with-eval-after-load 'elpaish
  (cmdr-transient-insert-suffix-once 'elpaish-menu '(-1 0) '("x" "extended elpaish commands" execute-extended-elpaish-command)))

(with-eval-after-load 'docker
  (setq docker-terminal-backend 'eat)
  (cmdr-transient-insert-suffix-once 'docker '(-1 0) '("m" "emacs docker commands" execute-extended-docker-command)))

(with-eval-after-load 'slime
  (setq inferior-lisp-program "sbcl"))

(with-eval-after-load 'cov
  (setq cov-coverage-mode 'both
        cov-lcov-file-name "coverage/lcov.info"
        cov-coverage-file-paths '("cover" "coverage" ".")))

(with-eval-after-load 'dape
  (add-hook 'kill-emacs-hook #'dape-breakpoint-save)
  (add-hook 'dape-start-hook #'save-all-buffers)
  (setq dape-key-prefix (kbd "C-c C-d")
        dape-buffer-window-arrangement 'right
        dape-cwd-function #'projectile-project-root))

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

(with-eval-after-load 'magit-dash
  (setq magit-dash-gh-prune-cache-dir (sprite-state-path "magit-dash-gh-prune")))

(with-eval-after-load 'magit-gh
  (cmdr-transient-insert-suffix-once 'magit-gh '(-1)
    ["magit-dash"
     ("g" "magit-dash: prune/CI logs/PR comments/gh auth" magit-dash-gh-menu)]))

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
  (when (default-boundp 'denote-directory)
    (setq denote-directory '()))
  (add-to-list 'denote-directory (file-name-concat (or local-notes-directory (expand-file-name "~/notes")) "denote"))
  (setq denote-journal-directory (file-name-concat (or local-notes-directory (expand-file-name "~/notes")) "denote" "journal")
        denote-explore-network-directory (file-name-concat (or local-notes-directory (expand-file-name "~/notes")) "explore")
        denote-dash-default-filter '(not "journal")
        denote-review-insert-after "date"
        denote-known-keywords '("project" "reference" "journal" "idea" "writing" "agent" "plan")
        denote-file-type 'org
        denote-infer-keywords t
        denote-sort-keywords t
        denote-save-buffers t
        denote-prompts '(title keywords file-type)
        denote-journal-title-format nil
        denote-sequence-scheme 'alphanumeric
        denote-explore-network-format "graphviz"
        denote-explore-network-graphviz-filetype "svg")
  (add-to-list 'display-buffer-alist
	       '("\\`\\*denote-sequence-hierarchy " 
                 (display-buffer-reuse-window display-buffer-pop-up-window)))
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'denote--title-history)
    (add-to-list 'savehist-additional-variables 'denote--keywords-history))

  (setq denote-rename-buffer-function #'denote-dash-rename-buffer)
  (denote-rename-buffer-mode 1)

  (defun ad:denote-link-ol-get-heading--org-only (fn &rest args)
    "Only call FN in `org-mode' buffers."
    (when (derived-mode-p 'org-mode)
      (apply fn args)))
  (advice-add 'denote-link-ol-get-heading :around
	      #'ad:denote-link-ol-get-heading--org-only)

  (setq denote-link-description-format #'denote-dash-link-description-format))

(with-eval-after-load 'denote-dash
  (keymap-set denote-dash-org-map "r" #'orgx-denote-migrate-subtree)
  (make-read-extended-command-for-prefix "denote"
    :bind-map denote-dash-global-map
    :bind-key "x"))

(with-eval-after-load 'denote-sync
  (require 'denote-sync-notion)
  (require 'denote-sync-gdocs))

(with-eval-after-load 'consult-notes
  (consult-notes-denote-mode 1)

  (defun ad:consult-notes--single-directory-string (fn &rest args)
    "Let-bind `denote-directory' to a single string for the duration of FN."
    (let ((denote-directory (denote-directories-get-common-root)))
      (apply fn args)))
  (advice-add 'consult-notes :around #'ad:consult-notes--single-directory-string)
  (advice-add 'consult-notes-search-in-all-notes :around #'ad:consult-notes--single-directory-string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Programming language modes

(with-eval-after-load 'python
  (keymap-set python-base-mode-map "M-<left>" #'python-indent-shift-left)
  (keymap-set python-base-mode-map "M-<right>" #'python-indent-shift-right)
  (font-lock-add-keywords 'python-ts-mode (overmark-font-lock-show-tabs)))

(with-eval-after-load 'rust-mode
  (setq rust-mode-treesitter-derive t))

(with-eval-after-load 'rustic
  (setq rustic-lsp-client 'eglot
        rustic-format-trigger 'on-save
        rustic-cargo-clippy-trigger-fix 'on-compile
        rustic-use-rust-save-some-buffers t
        rustic-clippy-arguments "--all --all-features -- --deny warnings")
  (when (executable-find "rustup")
    (setq rustic-rustfmt-args "+nightly")))

(with-eval-after-load 'go-ts-mode
  (keymap-set go-ts-mode-map "C-c C-t c" #'builder-go-coverage-and-convert))

(with-eval-after-load 'terraform-mode
  (setq terraform-format-on-save t
        terraform-indent-level 2))

(with-eval-after-load 'web-mode
  (setq web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        js-indent-level 2
        web-mode-enable-auto-pairing t
        web-mode-enable-auto-expanding t
        web-mode-enable-css-colorization t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; AI & LLM integration

(with-eval-after-load 'ollama-tailnet
  (setq ollama-tailnet-fallback-strategy :auto-local)
  (ollama-tailnet-setup-laptop-presets)
  (cmdr-transient-insert-suffix-once 'ollama-transient-menu '(-1 0) '("x" "emacs ollama commands" execute-extended-ollama-command)))

(with-eval-after-load 'gptel
  (defvar gemini-api-key nil)
  (defvar anthropic-api-key nil)
  (defvar openai-api-key nil)

  (cl-defun make-gptel-set-up-backend-functions (&key name model backend key api-key)
    "Register buffer-local and session-default backend switchers for MODEL as NAME."
    (declare (indent defun))
    (let ((local-function-symbol (intern (format "gptel-set-backend-%s" name)))
          (default-function-symbol (intern (format "gptel-set-backend-default-%s" name))))
      (defalias local-function-symbol
        (lambda ()
          (interactive)
          (setq-local gptel-model model)
          (when api-key
            (setq-local gptel-api-key (if (functionp api-key) api-key (lambda () api-key))))
          (setq-local gptel-backend backend)
          (message "[gptel] set backend to %s for the local buffer" name))
        (format "Set LLM backend for the current buffer to `%s'" model))

      (defalias default-function-symbol
        (lambda ()
          (interactive)
          (setq-default gptel-model model)
          (when api-key
            (setq-default gptel-api-key (if (functionp api-key) api-key (lambda () api-key))))
          (setq-default gptel-backend backend)
          (message "[gptel] set default backend to %s" name))
        (format "Set the default LLM backend for the current session to `%s'" model))

      (keymap-set gptel-mode-map (format "C-c r a m %s" (upcase key)) default-function-symbol)
      (keymap-set gptel-mode-map (format "C-c r a m %s" (downcase key)) local-function-symbol)
      (cmdr-keys-prefix cmdr-mode-map "C-c r g m" "gptel-set-default-model")
      (cmdr-keys-set cmdr-mode-map (format "C-c r g m %s" (downcase key)) default-function-symbol)))

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
   :name "copilot"
   :key "c"
   :model 'gpt-5-mini
   :backend (gptel-make-gh-copilot "copilot"))

  (make-gptel-set-up-backend-functions
   :name "gpt-5.6-terra"
   :key "m"
   :backend (gptel-make-openai "openai" :key openai-api-key)
   :model 'gpt-5.6-terra
   :api-key openai-api-key)

  (gptel-set-backend-default-gemini-flash)
  (require 'gptel-integrations)
  (gptel-mcp-mode 1))

(provide 'setup-core)
;;; setup-core.el ends here
