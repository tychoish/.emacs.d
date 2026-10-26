;;; setup-hooks.el --- Package hooks and after-load glue -*- lexical-binding: t; -*-

;;; Commentary:
;; Package hooks, with-eval-after-load forms, and mode integration glue
;; for packages configured outside use-package.

;;; Code:

(require 'cl-lib)
(require 'xtd-macro)

(autoload 'hud-transient-insert-suffix-once "hud")

(with-eval-after-load 'elec-pair
  (setq electric-pair-inhibit-predicate #'bootstrap-electric-pair-inhibition)
  (add-to-list 'electric-pair-pairs '(?< . ?>)))

(with-eval-after-load 'transient
  (setq transient-values-file (file-name-concat user-emacs-directory sprite--conf-state-directory (sprite-state-file-prefix "transient-values.el"))))

(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook #'nerd-icons-dired-mode)
  (keymap-set dired-mode-map "w" #'wdired-change-to-wdired-mode))

(with-eval-after-load 'recentf
  (setq recentf-save-file (sprite-state-path "recentf.el")))

(with-eval-after-load 'warnings
  (add-to-list 'warning-suppress-log-types '(frameset)))

(with-eval-after-load 'emojify
  (setq emojify-emoji-styles '(ascii unicode github))
  (setq emojify-display-style 'unicode)
  (setq emojify-point-entered-behaviour 'echo))

(with-eval-after-load 'erc
  (require 'tracking))

(with-eval-after-load 'tracking
  (setq tracking-max-mode-line-entries 3))

(with-eval-after-load 'shell-maker
  (defalias 'shell-maker-map 'shell-maker-major-mode-map)
  (setq shell-maker-root-path (sprite-state-path "shell-maker")))

(with-eval-after-load 'eshell
  (setq eshell-history-file-name (file-name-concat
				  user-emacs-directory
				  sprite--conf-state-directory
				  (sprite-state-file-prefix "eshell")))
  (with-eval-after-load "em-cmpl"
    (add-hook 'eshell-mode 'eshell-cmpl-initialize)))

(with-eval-after-load 'comint
  (defun colorize-shell ()
    "Apply ANSI colors to the agent-shell buffer."
    (interactive)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))

  (add-hook 'comint-mode-hook 'ansi-color-for-comint-mode-on)
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  (add-hook 'shell-mode-hook #'minaid-corfu-prog-mode-setup)
  (keymap-set comint-mode-map "M-n" #'comint-next-input)
  (keymap-set comint-mode-map "M-p" #'comint-previous-input)
  (keymap-set comint-mode-map "<down>" #'comint-next-matching-input-from-input)
  (keymap-set comint-mode-map "<up>" #'comint-previous-matching-input-from-input))

(with-eval-after-load 'dabbrev
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'authinfo-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(add-hook 'abbrev-mode-hook #'bootstrap-load-abbrev-files)
(add-hook 'auto-save-mode-hook #'bootstrap-set-up-auto-save)
(add-hook 'nerd-icons-completion-mode-hook #'nerd-icons-xref-mode)

(with-eval-after-load 'whitespace
  ;; TODO(): are the following settings material?
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


(defun tempel-setup-capf ()
  (setq-local completion-at-point-functions
              (cons #'tempel-complete completion-at-point-functions)))

(defun tempel-open-custom-file ()
  (interactive)
  (find-file (expand-file-name "templates" user-emacs-directory)))

(add-hook 'conf-mode-hook #'tempel-setup-capf)
(add-hook 'prog-mode-hook #'tempel-setup-capf)
(add-hook 'text-mode-hook #'tempel-setup-capf)

(with-eval-after-load 'tempel
  (setq tempel-path (expand-file-name "templates" user-emacs-directory)))

(with-eval-after-load 'nerd-icons
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)

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
    "Skip icon lookup when CAND names a killed buffer.
`get-buffer' returns the buffer object even when dead, and the primary
method has no liveness check, so it errors in `set-buffer' once a
completion candidate outlives its buffer."
    (if (buffer-live-p (get-buffer cand))
        (cl-call-next-method)
      "")))

(with-eval-after-load 'marginalia
  (add-to-list 'marginalia-command-categories '(consult-completion-in-region . imenu)))

(with-eval-after-load 'smerge-mode
  (keymap-set hud-smerge-map "r" #'smerge-kill-and-vc-next-conflict)

  (defun smerge-kill-and-vc-next-conflict ()
    "Kill the current conflict option and move to the next conflict."
    (interactive)
    (smerge-kill-current)
    (smerge-vc-next-conflict)))

(with-eval-after-load 'docker
  (setq docker-terminal-backend 'eat)
  (hud-transient-insert-suffix-once 'docker '(-1 0) '("m" "emacs docker commands" execute-extended-docker-command)))

(with-eval-after-load 'denote-dash
  (keymap-set denote-dash-org-map "r" #'orgx-migrate-subtree-to-denote)
  (make-read-extended-command-for-prefix "denote"
    :bind-map denote-dash-global-map
    :bind-key "x"))

(with-eval-after-load 'flycheck-eglot
  (setq-default flycheck-eglot-exclusive nil)
  (add-to-list 'flycheck-checkers 'eglot-check)
  (setq flycheck-eglot-enable-diagnostic-tags nil)
  (flycheck-add-next-checker 'eglot-check 'go-gofmt))

(with-eval-after-load 'denote-sync
  (require 'denote-sync-notion)
  (require 'denote-sync-gdocs))

(with-eval-after-load 'arch
  (defun arch-alert-after-install (pkg)
    (alert (format "Installed package %s" (arch-pkg-name pkg))
           :title "Arch Package Manager"))

  (defun arch-alert-after-upgrade (pkg)
    (alert (format "Upgraded package %s" (arch-pkg-name pkg))
           :title "Arch Package Manager"))

  (defun arch-alert-after-remove (pkg)
    (alert (format "Removed package %s" (arch-pkg-name pkg))
           :title "Arch Package Manager"))

  (defun arch-alert-after-upgrade-all ()
    (alert "System upgrade completed"
           :title "Arch Package Manager"))

  (add-hook 'arch-after-install-hook #'arch-alert-after-install)
  (add-hook 'arch-after-upgrade-hook #'arch-alert-after-upgrade)
  (add-hook 'arch-after-remove-hook #'arch-alert-after-remove)
  (add-hook 'arch-after-upgrade-all-hook #'arch-alert-after-upgrade-all)
  (run-with-idle-timer 2 nil #'arch--populate-cache))

(with-eval-after-load 'slime
  (make-read-extended-command-for-prefix "slime"
    :key-alias "slime-commands"
    :bind-map hud-ide-map
    :bind-key "s")
  (setq ls-lisp-dirs-first t)
  (setq inferior-lisp-program "sbcl"))

(with-eval-after-load 'builder-core
  (require 'builder-tasks))

(with-eval-after-load 'ollama-tailnet
  (ollama-tailnet-setup-laptop-presets)
  (hud-transient-insert-suffix-once 'ollama-transient-menu '(-1 0) '("x" "emacs ollama commands" execute-extended-ollama-command)))

(with-eval-after-load 'elpaish
  (hud-transient-insert-suffix-once 'elpaish-menu '(-1 0) '("x" "extended elpaish commands" execute-extended-elpaish-command)))

(with-eval-after-load 'treesit
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

(with-eval-after-load 'terraform-mode
  (setq terraform-format-on-save t)
  (setq terraform-indent-level 2))

(with-eval-after-load 'web-mode
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq js-indent-level 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-expanding t)
  (setq web-mode-enable-css-colorization t))

(with-eval-after-load 'markdown-mode
  (font-lock-add-keywords 'markdown-mode tk-font-lock-keywords 'append)
  (font-lock-add-keywords 'gfm-mode tk-font-lock-keywords 'append)
  (defalias 'markdown-indent-code (kmacro "SPC SPC SPC SPC SPC C-a C-n"))
  (add-hook 'markdown-mode-hook #'turn-off-auto-fill)
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
	  ("h1"	    "^# \\(.*\\)$" 1)
	  ("h2"	    "^## \\(.*\\)$" 1)
	  ("h3"	    "^### \\(.*\\)$" 1)
	  ("h4"	    "^#### \\(.*\\)$" 1)
	  ("h5"	    "^##### \\(.*\\)$" 1)
	  ("h6"	    "^###### \\(.*\\)$" 1)
	  ("fn"	    "^\\[^\\(.*\\)\\]" 1)))

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

(with-eval-after-load 'yaml-mode
  (add-hook 'yaml-mode-hook #'yaml-pro-mode)
  (add-hook 'yaml-mode-hook (hud-set-tab-width 2)))

(with-eval-after-load 'go-ts-mode
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

  (add-hook 'go-ts-mode-hook #'tychoish/go-mode-setup)
  (add-hook 'go-mode-hook #'tychoish/go-mode-setup)

  (let ((current-path (getenv "PATH"))
	(gopath (getenv "GOPATH")))
    (unless gopath
      (setq gopath (setenv "GOPATH" (expand-file-name "~/go"))))
    (let ((local-go-bin (expand-file-name (concat gopath "/bin"))))
      (add-to-list 'exec-path local-go-bin)
      (unless (string-search local-go-bin current-path)
        (setenv "PATH" (format "%s:%s" current-path local-go-bin))))))

(with-eval-after-load 'rustic
  (defun rustic-mode-auto-save-hook ()
    "Enable auto-saving in rustic-mode buffers."
    (when buffer-file-name
      (setq-local compilation-ask-about-save nil)))
  (add-hook 'rustic-mode-hook #'rustic-mode-auto-save-hook)

  (setq rust-mode-treesitter-derive t)
  (setq rustic-lsp-client 'eglot)
  (setq rustic-format-trigger 'on-save)
  (setq rustic-cargo-clippy-trigger-fix 'on-compile)
  (setq rustic-use-rust-save-some-buffers t)
  (setq rustic-clippy-arguments "--all --all-features -- --deny warnings")

  (when (executable-find "rustup")
    (setq rustic-rustfmt-args "+nightly"))

  (with-eval-after-load 'compile
  (require 'rust-compile nil t)
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
      (builder-compile-project "super-lint" command-string))))

  (with-eval-after-load 'flycheck
    (add-to-list 'flycheck-checkers 'rustic-clippy)))

(with-eval-after-load 'python
  (defun tychoish/python-setup ()
    (setq-local python-indent-offset 4)
    (setq-local tab-width 4)
    (setq-local fill-column 100))
  (add-hook 'python-ts-mode-hook #'tychoish/python-setup)

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

  (keymap-set python-ts-mode-map "M-<left>" #'balle-python-shift-left)
  (keymap-set python-ts-mode-map "M-<right>" #'balle-python-shift-right)
  (when (fboundp 'font-lock-show-tabs)
    (font-lock-add-keywords 'python-ts-mode (font-lock-show-tabs)))
  (when (fboundp 'font-lock-width-keyword)
    (font-lock-add-keywords 'python-ts-mode (font-lock-width-keyword 100))))

;;; Search / isearch / anzu
(keymap-set isearch-mode-map "C-o" #'isearch-occur)
(add-hook 'isearch-mode-hook #'anzu-mode)
(setq anzu-cons-mode-line-p nil)

(with-eval-after-load 'anzu
  (seq-do #'make-variable-buffer-local
          '(anzu--total-matched anzu--current-position anzu--state
            anzu--cached-count anzu--cached-positions anzu--last-command
            anzu--last-isearch-string anzu--overflow-p)))

;;; revbufs
(with-eval-after-load 'revbufs
  (defalias 'revbufs-kill (kmacro "C-f C-f C-f C-k"))
  (keymap-set revbufs-mode-map "C-k" #'revbufs-kill)
  (keymap-set revbufs-mode-map "q" #'bury-buffer))

;;; popper
(with-eval-after-load 'popper
  (add-hook 'popper-mode-hook #'popper-echo-mode)
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

;;; git-link
(with-eval-after-load 'git-link
  (defun ad:git-link--new-copy-to-clipboard (link)
    "Also copy LINK to the system clipboard.
`select-enable-clipboard' is nil, so `kill-new' (called by
`git-link--new') only reaches the Emacs kill-ring, not the OS
clipboard."
    (with-temp-buffer
      (insert link)
      (clipboard-kill-ring-save (point-min) (point-max))))

  (advice-add 'git-link--new :after #'ad:git-link--new-copy-to-clipboard))

;;; consult-notes
(with-eval-after-load 'consult-notes
  (consult-notes-denote-mode 1)

  (defun ad:consult-notes--single-directory-string (fn &rest args)
    "Let-bind `denote-directory' to a single string for the duration of FN."
    (let ((denote-directory (denote-directories-get-common-root)))
      (apply fn args)))
  (advice-add 'consult-notes :around #'ad:consult-notes--single-directory-string)
  (advice-add 'consult-notes-search-in-all-notes :around #'ad:consult-notes--single-directory-string))

;;; cov
(with-eval-after-load 'cov
  (setq cov-coverage-mode 'both)
  (setq cov-lcov-file-name "coverage/lcov.info")
  (setq cov-coverage-file-paths '("cover" "coverage" ".")))

;;; dape
(with-eval-after-load 'dape
  (add-hook 'kill-emacs-hook #'dape-breakpoint-save)
  (add-hook 'dape-start-hook #'save-all-buffers)
  (setq dape-key-prefix (kbd "C-c C-d"))
  (setq dape-buffer-window-arrangement 'right)
  (setq dape-cwd-function #'projectile-project-root))

(with-eval-after-load 'magit
  (setq magit-module-sections-nested t)
  (put 'magit-diff-edit-hunk-commit 'disabled nil)
  (add-hook 'magit-status-sections-hook 'magit-insert-worktrees t)
  (add-hook 'magit-status-sections-hook 'magit-insert-modules t)

  (defvar-keymap magit-command-mode-map) ;; "/"
  (keymap-set magit-mode-map "/" '(magit-command-mode-map . "magit-command"))
  (keymap-set magit-status-mode-map "C-n" #'next-line)
  (keymap-set magit-status-mode-map "C-p" #'previous-line)
  (keymap-set magit-command-mode-map "x" '(execute-extended-magit-command . "magit-commands"))
  (keymap-set magit-command-mode-map "m" '(execute-extended-magit-command . "(s)merge-commands"))

  (when (fboundp 'hud-modeline-set-segment-action)
    (hud-modeline-set-segment-action 'vc #'magit-dispatch)))

(with-eval-after-load 'magit-gh
  (unless (condition-case nil
              (transient-get-suffix 'magit-gh "g")
            (error nil))
    (transient-append-suffix 'magit-gh '(-1)
      ["magit-dash"
       ("g" "magit-dash: prune/CI logs/PR comments/gh auth" magit-dash-gh-menu)])))

(provide 'setup-hooks)
;;; setup-hooks.el ends here
