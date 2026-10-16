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
  (add-hook 'shell-mode-hook 'tychoish/corfu-prog-mode-setup)
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

(with-eval-after-load 'slime
  (make-read-extended-command-for-prefix "slime"
    :key-alias "slime-commands"
    :bind-map hud-ide-map
    :bind-key "s")
  (setq ls-lisp-dirs-first t)
  (setq inferior-lisp-program "sbcl"))

(provide 'setup-hooks)
;;; setup-hooks.el ends here
