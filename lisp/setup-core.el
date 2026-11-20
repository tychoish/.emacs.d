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
  :name mailx
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
;;; third-party -- configuration

(setq emojify-display-style 'unicode)
(setq afp-fill-comments-only-mode-list '(prog-mode))

(setq denote-dash-default-filter '(not "journal"))
(setq denote-known-keywords '("project" "reference" "journal" "idea" "writing" "agent" "plan"))

;; thinking of dropping journal/explore/review
(setq denote-journal-title-format nil)
(setq denote-explore-network-format "graphviz")
(setq denote-review-insert-after "date")

(setq agent-shell-header-style 'text)
(setq agent-shell-anthropic-default-session-mode-id "auto")

(provide 'setup-core)
;;; setup-core.el ends here
