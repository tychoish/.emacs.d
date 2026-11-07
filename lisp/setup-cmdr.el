;;; setup-cmdr.el --- Personal command menu registrations -*- lexical-binding: t; -*-

;;; Commentary:
;; Register the configuration's commands with the standalone cmdr package.

;;; Code:

(require 'cmdr)
(require 'cmdr-keys)
(require 'cmdr-macros)
(require 'seq)

(autoload 'cmdr-defaults-install "cmdr-defaults")
(autoload 'cmdr-ecosystem-install "cmdr-ecosystem")

(defvar hud-modeline-icons)
(defvar hud-modeline-show-buffer-size)
(defvar electric-pair-inhibition)
(defvar electric-pair-eagerness)

(make-read-extended-command-for-prefix "elpaish")
(make-read-extended-command-for-prefix "agent-shell")
(make-read-extended-command-for-prefix "gptel")
(make-read-extended-command-for-prefix "gptel-set-backend")
(make-read-extended-command-for-prefix "ollama")
(make-read-extended-command-for-prefix "ollama-tailnet")
(make-read-extended-command-for-prefix "tailscale")

(create-toggle-functions hud-modeline-icons)
(create-toggle-functions hud-modeline-show-buffer-size)
(create-toggle-functions slow-op-reporting)
(create-toggle-functions electric-pair-inhibition)
(create-toggle-functions electric-pair-eagerness)

;; Keymaps
(defvar setup-cmdr-core-map (cmdr-keys-prefix cmdr-mode-map "C-c t" "core")
  "The cmdr core prefix map under C-c t.")
(defvar setup-cmdr-blogging-map (cmdr-keys-prefix cmdr-mode-map "C-c t b" "blogging")
  "The cmdr blogging prefix map under C-c t b.")
(defvar setup-cmdr-theme-map (cmdr-keys-prefix cmdr-mode-map "C-c t t" "theme")
  "The cmdr theme prefix map under C-c t t.")
(defvar setup-cmdr-whitespace-map (cmdr-keys-prefix cmdr-mode-map "C-c t w" "whitespace")
  "The cmdr whitespace prefix map under C-c t w.")
(defvar setup-cmdr-opacity-map (cmdr-keys-prefix cmdr-mode-map "C-c f o" "opacity")
  "The cmdr opacity prefix map under C-c f o.")
(defvar setup-cmdr-mail-map (cmdr-keys-prefix cmdr-mode-map "C-c m" "mail")
  "The cmdr mail prefix map under C-c m.")
(defvar setup-cmdr-robot-map (cmdr-keys-prefix cmdr-mode-map "C-c r" "robot")
  "The cmdr robot prefix map under C-c r.")
(defvar setup-cmdr-robot-agent-shell-map (cmdr-keys-prefix cmdr-mode-map "C-c r s" "agent-shell")
  "The cmdr agent-shell prefix map under C-c r s.")
(defvar setup-cmdr-robot-agent-shell-send-map (cmdr-keys-prefix cmdr-mode-map "C-c r s s" "send")
  "The cmdr agent-shell send prefix map under C-c r s s.")
(defvar setup-cmdr-robot-gptel-map (cmdr-keys-prefix cmdr-mode-map "C-c r g" "gptel")
  "The cmdr gptel prefix map under C-c r g.")
(defvar setup-cmdr-robot-gptel-set-default-model-map
  (cmdr-keys-prefix cmdr-mode-map "C-c r g m" "gptel-set-default-model")
  "The cmdr gptel default model prefix map under C-c r g m.")
(defvar setup-cmdr-robot-ollama-map (cmdr-keys-prefix cmdr-mode-map "C-c r o" "ollama")
  "The cmdr ollama prefix map under C-c r o.")
(defvar setup-cmdr-robot-ollama-tailnet-map (cmdr-keys-prefix cmdr-mode-map "C-c r o t" "tailnet")
  "The cmdr tailnet prefix map under C-c r o t.")
(defvar setup-cmdr-robot-tailscale-map (cmdr-keys-prefix cmdr-mode-map "C-c r n" "network")
  "The cmdr tailscale network prefix map under C-c r n.")
(defvar setup-cmdr-magit-map (cmdr-keys-prefix cmdr-mode-map "C-x g" "magit")
  "The cmdr magit prefix map under C-x g.")
(defvar setup-cmdr-smerge-map (cmdr-keys-prefix cmdr-mode-map "C-x g m" "smerge")
  "The cmdr smerge prefix map under C-x g m.")

;; Backward-compatibility aliases
(defvaralias 'cmdr-core-map 'setup-cmdr-core-map)
(defvaralias 'hud-core-map 'setup-cmdr-core-map)
(defvaralias 'cmdr-blogging-map 'setup-cmdr-blogging-map)
(defvaralias 'hud-blogging-map 'setup-cmdr-blogging-map)
(defvaralias 'hud-theme-map 'setup-cmdr-theme-map)
(defvaralias 'hud-whitespace-map 'setup-cmdr-whitespace-map)
(defvaralias 'hud-display-opacity-map 'setup-cmdr-opacity-map)
(defvaralias 'hud-mail-map 'setup-cmdr-mail-map)
(defvaralias 'hud-robot-map 'setup-cmdr-robot-map)
(defvaralias 'hud-robot-agent-shell-map 'setup-cmdr-robot-agent-shell-map)
(defvaralias 'hud-robot-agent-shell-send-map 'setup-cmdr-robot-agent-shell-send-map)
(defvaralias 'hud-robot-gptel-map 'setup-cmdr-robot-gptel-map)
(defvaralias 'cmdr-robot-gptel-set-default-model-map 'setup-cmdr-robot-gptel-set-default-model-map)
(defvaralias 'hud-robot-gptel-set-default-model-map 'setup-cmdr-robot-gptel-set-default-model-map)
(defvaralias 'hud-robot-ollama-map 'setup-cmdr-robot-ollama-map)
(defvaralias 'hud-robot-ollama-tailnet-map 'setup-cmdr-robot-ollama-tailnet-map)
(defvaralias 'hud-robot-tailscale-map 'setup-cmdr-robot-tailscale-map)
(defvaralias 'hud-magit-map 'setup-cmdr-magit-map)
(defvaralias 'hud-smerge-map 'setup-cmdr-smerge-map)
(defvaralias 'hud-mode-map 'cmdr-mode-map)
(defalias 'hud-mode #'cmdr-mode)

(defun setup-cmdr-install-bindings ()
  "Install generic cmdr plugins and this configuration's bindings."
  (cmdr-defaults-install)
  (cmdr-ecosystem-install)
  (seq-do (lambda (prefix)
            (cmdr-keys-prefix cmdr-mode-map (car prefix) (cdr prefix)))
          '(("C-c t b" . "blogging") ("C-c t t" . "theme")
            ("C-c t w" . "whitespace") ("C-c m" . "mail")
            ("C-c r" . "robot") ("C-c r s" . "agent-shell")
            ("C-c r s s" . "send") ("C-c r g" . "gptel")
            ("C-c r g m" . "gptel-set-default-model")
            ("C-c r o" . "ollama") ("C-c r o t" . "tailnet")
            ("C-c r n" . "network") ("C-x g" . "magit")
            ("C-x g m" . "smerge")))
  (seq-do (lambda (binding)
            (cmdr-keys-set cmdr-mode-map (car binding) (cdr binding)))
          '(("C-c t k" . execute-extended-clipboard-command)
            ("C-c t p" . toggle-electric-pair-inhibition)
            ("C-c t e" . execute-extended-elpaish-command)
            ("C-c t j" . journalctl) ("C-c t s" . daemons-dash)
            ("C-c t d" . magit-dash-open) ("C-c t g" . magit-dash-open)
            ("C-c t c" . builder-compile) ("C-c t r" . sardis-run)
            ("C-c t b m" . insert-current-date)
            ("C-c t b d" . bootstrap-blog-open-drafts-dired)
            ("C-c t b n" . bootstrap-blog-create-post)
            ("C-c t b p" . bootstrap-blog-publish-post)
            ("C-c t t r" . disable-all-themes)
            ("C-c t t d" . bootstrap-load-dark-theme)
            ("C-c t t l" . bootstrap-load-light-theme)
            ("C-c t t i" . toggle-hud-modeline-icons)
            ("C-c t t s" . toggle-hud-modeline-show-buffer-size)
            ("C-c t w c" . whitespace-cleanup)
            ("C-c t w t" . toggle-local-whitespace-cleanup)
            ("C-c h s" . eglox-describe-symbol-dwim)
            ("C-c h q" . kill-eldoc-and-help-buffers)
            ("C-x C-b s" . projectile-save-project-buffers-picker)
            ("C-x g d" . magit-dash-open) ("C-x g o" . magit-dash-open-repo)
            ("C-x g g" . magit-dash-gh-menu) ("C-x g w" . magit-dash-worktree-dispatch)
            ("C-x g m r" . smerge-kill-and-vc-next-conflict)
            ("C-c m m" . mu4e) ("C-c m d" . mu4e-search-maildir)
            ("C-c m b" . mu4e-search-bookmark) ("C-c m c" . mu4e-compose-new)
            ("C-c m a" . tychoish-mail-select-account)
            ("C-c r p" . agent-shell-workflow-select)
            ("C-c r m" . agent-shell-workflow-dispatch-menu)
            ("C-c r s o" . agent-shell) ("C-c r s t" . agent-shell-toggle)
            ("C-c r s b" . agent-shell-switch-buffer) ("C-c r s n" . agent-shell-new-shell)
            ("C-c r s e" . agent-shell-new-temp-shell)
            ("C-c r s w" . agent-shell-new-worktree-shell)
            ("C-c r s p" . agent-shell-workflow-select)
            ("C-c r s m" . agent-shell-workflow-dispatch-menu)
            ("C-c r s a" . agent-shell-menu-select-action)
            ("C-c r s q" . agent-shell-queue-buffer-open)
            ("C-c r s /" . agent-shell-queue-capture)
            ("C-c r s v" . tychoish/agent-shell-toggle-terse-output)
            ("C-c r s x" . execute-extended-agent-shell-command)
            ("C-c r s s d" . agent-shell-send-dwim)
            ("C-c r s s r" . agent-shell-send-region)
            ("C-c r s s s" . agent-shell-send-region-to)
            ("C-c r s s f" . agent-shell-send-file)
            ("C-c r s s t" . agent-shell-send-file-to)
            ("C-c r s s b" . agent-shell-menu-send-buffer)
            ("C-c r s s p" . agent-shell-menu-send-file)
            ("C-c r s s y" . agent-shell-yank-dwim)
            ("C-c r g g" . gptel) ("C-c r g r" . gptel-rewrite)
            ("C-c r g a" . gptel-agent) ("C-c r g w" . gptel-aibo-summon)
            ("C-c r g b" . execute-extended-gptel-set-backend-command)
            ("C-c r g x" . execute-extended-gptel-command)
            ("C-c r o o" . ollama-transient-menu) ("C-c r o s" . ollama-status)
            ("C-c r o l" . ollama-list-models) ("C-c r o p" . ollama-pull-model)
            ("C-c r o d" . ollama-delete-model) ("C-c r o c" . ollama-copy-model)
            ("C-c r o i" . ollama-show-model) ("C-c r o k" . ollama-check-server)
            ("C-c r o r" . ollama-status-refresh)
            ("C-c r o x" . execute-extended-ollama-command)
            ("C-c r o t s" . ollama-tailnet-status)
            ("C-c r o t d" . ollama-tailnet-discover-hosts)
            ("C-c r o t p" . ollama-tailnet-pull-model)
            ("C-c r o t r" . ollama-tailnet-service-restart)
            ("C-c r o t t" . ollama-tailnet-service-status)
            ("C-c r o t b" . ollama-tailnet-set-gptel-backend)
            ("C-c r o t l" . ollama-tailnet-models)
            ("C-c r o t S" . ollama-tailnet-search-model)
            ("C-c r o t x" . execute-extended-ollama-tailnet-command)
            ("C-c r n s" . tailscale-status) ("C-c r n c" . tailscale-connect)
            ("C-c r n d" . tailscale-disconnect) ("C-c r n y" . tailscale-copy-ip)
            ("C-c r n f" . tailscale-file-send)
            ("C-c r n x" . execute-extended-tailscale-command))))

(defun setup-cmdr-register-commands ()
  "Register personal commands without clearing other cmdr entries."
  (cmdr-register-command
   :category 'denote
   :command 'denote-dash
   :description "denote list"
   :transient-key "dv")

  (cmdr-register-command
   :category 'denote
   :command 'denote-dash-dispatch
   :description "denote dispatch"
   :transient-key "dd")

  (cmdr-register-command
   :category 'denote
   :command 'denote
   :description "new note"
   :transient-key "dn")

  (cmdr-register-command
   :category 'denote
   :command 'denote-journal-new-entry
   :description "new journal entry"
   :transient-key "dj")

  (cmdr-register-command
   :category 'denote
   :command 'denote-sequence-view-hierarchy
   :description "denote sequence hierarchy"
   :transient-key "ds")

  (cmdr-register-command
   :category 'magit-dash
   :command 'magit-dash-open
   :description "repo dashboard"
   :transient-key "rd")

  (cmdr-register-command
   :category 'magit-dash
   :command 'magit-dash-open-repo
   :description "magit open repo"
   :transient-key "rr")

  (cmdr-register-command
   :category 'magit-dash
   :command 'magit-dash-sync-all
   :description "sync all dashboard repos"
   :transient-key "rs")

  (cmdr-register-command
   :category 'magit-dash
   :command 'magit-dash-sync-repo
   :description "sync one dashboard repo"
   :transient-key "ru")

  (cmdr-register-command
   :category 'agent-shell
   :command 'agent-shell-new-shell
   :description "new agent shell"
   :transient-key "an")

  (cmdr-register-command
   :category 'agent-shell
   :command 'agent-shell-menu-new-shell-in-dir
   :description "new agent shell (dir)"
   :transient-key "ad")

  (cmdr-register-command
   :category 'agent-shell
   :command 'agent-shell-switch-buffer
   :transient-key "ab"
   :description "switch agent shell buffer")

  (cmdr-register-command
   :category 'agent-shell
   :command 'agent-shell-queue-buffer-open
   :description "open queue"
   :transient-key "aq")

  (cmdr-register-command
   :category 'agent-shell
   :command 'agent-shell-workflow-select
   :description "workflows"
   :transient-key "ap")
  (cmdr-register-command
   :category 'network
   :command 'tailscale-status
   :description "tailscale status"
   :transient-key "ns")

  (cmdr-register-command
   :category 'network
   :command 'tailscale-copy-ip
   :description "copy node IP"
   :transient-key "ny")

  (cmdr-register-command
   :category 'network
   :command 'tailscale-file-send
   :description "send file via Taildrop"
   :transient-key "nf")

  (cmdr-register-command
   :category 'mu4e
   :command 'mu4e-compose-mail
   :description "compose new email"
   :transient-key "mc")

  (cmdr-register-command
   :category 'mu4e
   :command 'mu4e
   :description "open mu4e dashboard"
   :transient-key "mm")

  (cmdr-register-command
   :category 'mu4e
   :command 'tychoish-mail-select-account
   :display-name "mail-select-account"
   :description "select mail account"
   :transient-key "ma")

  ;; Row 2

  (cmdr-register-command
   :row 2
   :category 'org-mode
   :command 'orgx-capture
   :description "org-capture with template"
   :transient-key "oc")

  (cmdr-register-command
   :row 2
   :category 'org-mode
   :command 'org-agenda
   :description "org-agenda view dispatch"
   :transient-key "oa")

  (cmdr-register-command
   :row 2
   :category 'org-mode
   :command 'orgx-find-heading
   :description "jump to org heading"
   :transient-key "oh")

  (cmdr-register-command
   :row 2
   :category 'sprite
   :command 'sprite-list
   :description "sprite dashboard"
   :transient-key "sd")

  (cmdr-register-command
   :row 2
   :category 'sprite
   :command 'sprite-create
   :description "spawn sprite"
   :transient-key "ss")

  (cmdr-register-command
   :row 2
   :category 'sprite
   :command 'sprite-open-frame
   :description "open frame in sprite"
   :transient-key "sf")

  (cmdr-register-command
   :row 2
   :category 'arch
   :command 'arch-find-package
   :description "arch package info"
   :transient-key "li")

  (cmdr-register-command
   :row 2
   :category 'arch
   :command 'arch-list
   :description "arch package list"
   :transient-key "ll"))

(provide 'setup-cmdr)
;;; setup-cmdr.el ends here
