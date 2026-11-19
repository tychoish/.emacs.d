;;; setup-core.el --- Core settings and package integration -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Package hooks, with-eval-after-load forms, and mode integration glue
;; for packages configured outside use-package.

;; This configuration optimizes for lazy-loading so that configuration
;; only loads when called directly or a mode is activated.

;;; Code:

(require 'cmdr-spec)
(require 'clix)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CLI flags

(register-clix-flag
 slow-op-timing
 :flag "--with-slow-op-timing"
 :doc "Enable slow operation timing."
 :consume nil
 :handler (message "[op]: enabling time reporting"))

(register-clix-flag
 sprite-id
 :flag "--id"
 :doc "Specify Emacs instance ID for sprite."
 :var sprite-cli-instance-id)

(register-clix-flag
 org-exec
 :flag "--org-exec"
 :doc "Execute an Org file's Babel blocks."
 :action (progn (require 'org) (require 'ob) (require 'builder-tasks)
                (builder-org-babel-execute-file clix-arg-org-exec))
 :final t)

(register-clix-flag
 org-exec-dir
 :flag "--org-exec-dir"
 :doc "Execute Org Babel blocks in a directory."
 :action (progn (require 'org) (require 'ob) (require 'builder-tasks)
                (builder-org-babel-execute-directory clix-arg-org-exec-dir))
 :final t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cmdr keybindings & specifications

(cmdr-register-spec
  :name mail-accounts
  :probe mailx-select-account
  :prefixes (("C-c m" . "mail"))
  :bindings (("C-c m a" . mailx-select-account))
  :commands ((cmdr-spec-command :category mu4e
                                :command mailx-select-account
                                :display-name "mail-select-account"
                                :description "select mail account"
                                :transient-key "ma"))
  :keymap-bindings
  ((message message-mode-map
    :hook message-mode-hook
    :bindings (("M-q" . ignore)))
   (mu4e-compose mu4e-compose-minor-mode-map
    :hook mu4e-compose-mode-hook
    :bindings (("R" . mailx-compose-reply)
               ("r" . mu4e-headers-mark-for-read)
               ("<remap> <mu4e-context-switch>" . mu4e-compose-context-switch)))
   (mu4e-headers mu4e-headers-mode-map
    :hook mu4e-headers-mode-hook
    :bindings (("C-r" . mailx-compose-reply)
               ("o" . mu4e-headers-mark-for-unread)
               ("u" . mu4e-headers-mark-for-unread)
               (";" . mu4e-mark-resolve-deferred-marks)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; core emacs -- global settings and options

(setq confirm-kill-processes nil)
(setq confirm-nonexistent-file-or-buffer nil)
(setq select-enable-clipboard nil)

(setq auto-revert-verbose nil)
(setq auto-revert-avoid-polling t)
(setq auto-revert-interval 60)
(setq backup-by-copying t) ;; sprite-state
(setq delete-old-versions t) ;; sprite-state

(setq vc-follow-symlinks t) ;; magit-dash
(setq version-control t) ;; magit-dash
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)

(setq-default indicate-empty-lines t)
(setq-default truncate-lines t)
(setq-default indent-tabs-mode nil) ; (setq tab-width 4)
(setq shell-command-dont-erase-buffer 'end-last-out)

(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'list-timers 'disabled nil)
(put 'list-threads 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(advice-add 'display-startup-echo-area-message :override #'ignore)

(add-to-list 'term-file-aliases '("alacritty" . "xterm"))
(add-to-list 'term-file-aliases '("ghostty" . "xterm-ghostty"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; third-party -- configuration

(setq savehist-coding-system 'utf-8-emacs)

(setq popper-reference-buffers
      '(arch-install-mode
        orgx-capture-shell-mode
        "\\*Messages\\*"
        "\\*Warnings\\*"
        "\\*Backtrace\\*"
        "\\*Compile-Log\\*"
        "\\*compilation\\*"
        "\\*grep\\*"
        "\\*xref\\*"
        "\\*Flymake diagnostics.*\\*"
        "\\*ert.*\\*"
        ert-results-mode
        help-mode
        compilation-mode))

(setq popper-echo-dispatch-keys '("C-1" "C-2" "C-3" "C-4" "C-5" "C-6" "C-7" "C-8" "C-9"))
(setq popper-display-control nil)
(setq whitespace-style '(face trailing tabs spaces lines newline missing-newline-at-eof empty space-mark tab-mark newline-mark))
(setq tracking-max-mode-line-entries 3)

(setq inferior-lisp-program "sbcl")
(setq emojify-display-style 'unicode)
(setq ollama-tailnet-fallback-strategy :auto-local)
(setq docker-terminal-backend 'eat)

(setq denote-dash-default-filter '(not "journal"))
(setq denote-known-keywords '("project" "reference" "journal" "idea" "writing" "agent" "plan"))
(setq denote-save-buffers t)
(setq denote-prompts '(title keywords file-type))
(setq denote-journal-title-format nil)
(setq denote-sequence-scheme 'alphanumeric)
(setq denote-explore-network-format "graphviz")
(setq denote-review-insert-after "date")

(setq agent-shell-dot-subdir-function 'bootstrap-agent-shell-state-path)
(setq agent-shell-header-style 'text)
(setq agent-shell-anthropic-default-session-mode-id "auto")
(setq agent-shell-display-action
      '((display-buffer-reuse-window display-buffer-use-some-window)
        (reusable-frames . t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Late Package Configuration

(with-eval-after-load 'elec-pair
  (add-to-list 'electric-pair-pairs '(?< . ?>))
  (add-to-list 'electric-pair-pairs '(?{ . ?}))
  (add-to-list 'electric-pair-pairs '(?\[ . ?\])))

(with-eval-after-load 'warnings
  (add-to-list 'warning-suppress-log-types '(frameset))
  (add-to-list 'warning-suppress-types '(files missing-lexbind-cookie)))

(with-eval-after-load 'gptel
  (setq gptel-include-reasoning 'ignore)
  (setq-default gptel-backend (gptel-make-openai-oauth "codex"))
  (setq-default gptel-model 'gpt-6.1-terra)
  (gptel-make-gh-copilot "gh-copilot"))

(provide 'setup-core)
;;; setup-core.el ends here
