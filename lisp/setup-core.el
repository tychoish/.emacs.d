;;; setup-core.el -- contains all major use-package forms -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Provides my collection of use-package forms and related
;; configuration for loading and configuring all Emacs packages.

;; This configuration optimizes for lazy-loading so that configuration
;; only loads when called directly or a mode is activated.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; INTERNAL

(eval-when-compile
  (require 'xtd-macro))

(require 'hud-mode)

(defvar local-notes-directory nil
  "Directory where notes (org, roam, deft, etc.) are stored.")

(defvar user-org-directories nil
  "Additional directories that may contain org files.")

(autoload 'hud-dispatch "hud" nil t)
(autoload 'hud-select "hud" nil t)
(autoload 'eglox-mode "eglox")

(autoload 'tychoish-mail-select-account "setup-mail" nil t)
(autoload 'tychoish-define-mail-account "setup-mail")
(autoload 'tychoish-mail-add-refile-rule "setup-mail")

(autoload 'orgx-capture "orgx" nil t)
(autoload 'orgx-agenda-view "orgx" nil t)
(autoload 'orgx-agenda-files-open "orgx" nil t)
(autoload 'orgx-minor-mode-turn-on "orgx")
(autoload 'orgx-agenda-minor-mode-turn-on "orgx")
(autoload 'orgx-agenda-files-reload "orgx" nil t)
(autoload 'orgx-agenda-untagged-in-file "orgx" nil t)
(autoload 'orgx-agenda-for-file "orgx" nil t)

(autoload 'daemons-dash "daemons-dash" nil t)
(autoload 'daemons-dash-dispatch "daemons-dash" nil t)

(use-package magit-dash
  :ensure t
  :defer t
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
  (delight 'slime-autodoc-mode nil)
  (delight 'minaid-mode nil 'minaid)

  (delight 'denote-sequence-hierarchy-mode "Hierarchy" 'denote-sequence)
  (delight 'outline-minor-mode nil 'outline)
  (delight 'cursor-sensor-mode nil 'cursor-sensor)
  (delight 'agent-shell-notifications-mode nil 'agent-shell-notifications)
  (delight 'terraform-mode "tf" 'terraform-mode))

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

  (require 'tracking)
  (require 'telega-alert)
  (require 'telega-extras)
  (telega-extras-setup)
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
  (setq denote-journal-directory (file-name-concat (or local-notes-directory (expand-file-name "~/notes")) "denote" "journal"))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; build/compilation support

(use-package flycheck
  :ensure t
  :defer t
  :defines (flycheck-checkers)
  :commands (flycheck-disable-checker)
  :init
  (with-eval-after-load 'minaid
    (keymap-set minaid-consult-map "c" #'consult-flycheck))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ROBOTS (AI) Integration

(use-package gptel
  :ensure t
  :defer t
  :functions (gptel-make-anthropic gptel-make-gh-copilot gptel-make-gemini)
  :config
  (defvar gemini-api-key nil)
  (defvar anthropic-api-key nil)
  (defvar openai-api-key nil)

  (eval-when-compile
    (require 'cl-lib))

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
