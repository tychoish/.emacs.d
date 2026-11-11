;;; test-setup-core.el --- ERT tests for setup-core.el -*- lexical-binding: t; no-byte-compile: t; -*-

;; Run inside a live Emacs session with full config loaded:
;;   M-x ert RET t RET
;; or filtered:
;;   (ert "^tychoish-core/")

(require 'ert)
(require 'test-helper)
(require 'cl-lib)
(require 'cmdr-keys)
(defvar hud-ecclectic-grep-project-map (make-sparse-keymap))
(require 'setup-core)
(require 'eglox-project)
(require 'builder)

(defvar modus-themes-deuteranopia)
(defvar modus-themes-common-palette-overrides)

(ert-deftest tychoish-core/modus-theme-configuration-loads ()
  "Evaluate the deferred Modus configuration as on first theme load."
  (let ((modus-themes-deuteranopia nil)
        (modus-themes-common-palette-overrides nil))
    (with-temp-buffer
      (insert-file-contents (expand-file-name "lisp/setup-core.el" test-helper-root))
      (goto-char (point-min))
      (search-forward "(with-eval-after-load 'modus-themes")
      (goto-char (match-beginning 0))
      (eval `(progn ,@(cddr (read (current-buffer)))) t))
    (should modus-themes-deuteranopia)
    (should (equal modus-themes-common-palette-overrides
                   '((border-mode-line-active bg-mode-line-active)
                     (border-mode-line-inactive bg-mode-line-inactive)
                     (message-separator bg-main))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-buffer-name-format lambda

;; The buffer-name-format lambda lives in tychoish-core's use-package agent-shell
;; :config block.  Reproduce it here so these tests are self-contained.
(defun tychoish-core-test--agent-shell-format (agent-name project-name)
  "Reference implementation of the tychoish buffer-name-format logic."
  (let* ((raw (string-trim project-name))
         (base (file-name-nondirectory (directory-file-name raw)))
         (stripped (replace-regexp-in-string "\\`[./]+" "" base))
         (slug (downcase (replace-regexp-in-string "\\s-+" "-"
                                                   (if (string-empty-p stripped) base stripped)))))
    (format "*%s-%s*"
            (car (split-string (downcase (string-trim agent-name))))
            slug)))

(ert-deftest tychoish-core/agent-shell-buffer-name-format-basic ()
  (should (equal "*claude-my-project*"
                 (tychoish-core-test--agent-shell-format "Claude" "my-project"))))

(ert-deftest tychoish-core/agent-shell-buffer-name-format-uses-first-word-of-agent ()
  (should (equal "*claude-my-project*"
                 (tychoish-core-test--agent-shell-format "Claude Sonnet" "my-project"))))

(ert-deftest tychoish-core/agent-shell-buffer-name-format-lowercases-agent ()
  (should (equal "*claude-project*"
                 (tychoish-core-test--agent-shell-format "CLAUDE" "project"))))

(ert-deftest tychoish-core/agent-shell-buffer-name-format-extracts-basename-from-path ()
  (should (equal "*claude-my-project*"
                 (tychoish-core-test--agent-shell-format "Claude" "/home/user/my-project"))))

(ert-deftest tychoish-core/agent-shell-buffer-name-format-strips-trailing-slash ()
  (should (equal "*claude-my-project*"
                 (tychoish-core-test--agent-shell-format "Claude" "/home/user/my-project/"))))

(ert-deftest tychoish-core/agent-shell-buffer-name-format-slugifies-spaces ()
  (should (equal "*claude-my-project*"
                 (tychoish-core-test--agent-shell-format "Claude" "my project"))))

(ert-deftest tychoish-core/agent-shell-buffer-name-format-strips-leading-dots ()
  (should (equal "*claude-hidden*"
                 (tychoish-core-test--agent-shell-format "Claude" ".hidden"))))

(ert-deftest tychoish-core/agent-shell-buffer-name-format-trims-whitespace-from-agent ()
  (should (equal "*claude-project*"
                 (tychoish-core-test--agent-shell-format "  Claude  " "project"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; nerd-icons mode registration

(defconst tychoish-core-test--registered-modes
  '(agent-shell-queue-item-view-mode)
  "Modes explicitly registered in nerd-icons-mode-icon-alist by this config.")

(ert-deftest tychoish-core/nerd-icons-registered-modes-resolve ()
  "Every mode we register in nerd-icons-mode-icon-alist resolves without error."
  (require 'setup-agent-shell)
  (require 'nerd-icons)
  (seq-do (lambda (mode)
            (should-not
             (condition-case err
                 (progn (nerd-icons-icon-for-mode mode) nil)
               (error err))))
          tychoish-core-test--registered-modes))

(ert-deftest tychoish-core/nerd-icons-icon-for-buffer-degrades-gracefully ()
  "Icon lookup returns a string even when given an unregistered mode."
  (require 'nerd-icons)
  (with-temp-buffer
    (setq major-mode 'tychoish-core-test--nonexistent-mode-xyz)
    (should (stringp (nerd-icons-icon-for-buffer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; python-ts-mode buffer-local setup hook

(ert-deftest tychoish-core/python-ts-mode-setup-runs-on-hook ()
  "Python setup hooks must be registered on `python-ts-mode-hook'.

Regression test: verifies that buffer-local setup hooks are attached to
`python-ts-mode-hook' when `python' is loaded."
  (require 'eglox)
  (eglox-global-mode 1)
  (should (memq #'eglox--python-setup python-ts-mode-hook)))

(ert-deftest tychoish-core/compilation-read-command ()
  "Test that `builder-compilation-read-command' processes `builder--read-command'
correctly and reads from the minibuffer using the candidate's command."
  (let* ((c (make-builder-candidate :command "make -j8" :name "build"))
         (mock-results (cons c nil)))
    (cl-letf (((symbol-function 'builder--read-command)
               (lambda (_cmd) mock-results))
              ((symbol-function 'read-from-minibuffer)
               (lambda (prompt val &rest _rest)
                 (should (equal "edit command => " prompt))
                 (should (equal "make -j8" val))
                 val)))
      (should (equal "make -j8" (builder-compilation-read-command "make"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tychoish-mail account definition and immediate activation

(ert-deftest tychoish-core/mail-account-definition-activates-default ()
  "Test that `tychoish-define-mail-account' activates a default account in an idle timer after definition."
  (require 'setup-mail)
  (let ((cmdr-mode-map (make-sparse-keymap))
        (tychoish-mail-accounts-table (make-hash-table :test #'equal))
        (tychoish-mail-account-current nil)
        (user-mail-address nil)
        (user-full-name nil)
        (tychoish-mail-activation-delay 0.0))
    (setq mu4e-get-mail-command nil)
    (cl-letf (((symbol-function 'mu4e) (lambda (&rest _args) nil)))
      (let ((before (copy-sequence timer-idle-list)))
        (tychoish-define-mail-account
         :name "Test User"
         :id "test-acc"
         :address "test@example.com"
         :key "x"
         :maildir "/tmp/test-mail"
         :default t
         :command "true")
        (should (eq (keymap-lookup cmdr-mode-map "C-c m x")
                    #'tychoish-mail-test-acc))
        (should-not tychoish-mail-account-current)
        (thread-last timer-idle-list
                     (seq-remove (lambda (tm) (memq tm before)))
                     (seq-do #'timer-event-handler)))
      (should (equal tychoish-mail-account-current "tychoish-mail-test-acc"))
      (should (equal user-mail-address "test@example.com"))
      (should (equal user-full-name "Test User"))
      (should (equal mu4e-get-mail-command "true")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Projectile yielding / incremental operations

(ert-deftest tychoish-core/projectile-enable-all-buffers-yielding ()
  "Test that `projectile-enable-all-buffers` enables projectile-mode in live buffers."
  (defvar hud-ecclectic-grep-project-map (make-sparse-keymap))
  (require 'projectile)
  (require 'gen)
  (let ((b1 (generate-new-buffer "test-proj-1"))
        (b2 (generate-new-buffer "test-proj-2"))
        (done-count nil))
    (unwind-protect
        (progn
          (with-current-buffer b1 (projectile-mode -1))
          (with-current-buffer b2 (projectile-mode -1))
          (cl-letf (((symbol-function 'buffer-list) (lambda (&optional _frame) (list b1 b2))))
            (projectile-enable-all-buffers
             :delay 0.0
             :on-done (lambda (cnt) (setq done-count cnt)))
            (let ((timeout 3.0)
                  (start (float-time)))
              (while (and (null done-count) (< (- (float-time) start) timeout))
                (sit-for 0.02)))
            (should (equal done-count 2))
            (with-current-buffer b1 (should projectile-mode))
            (with-current-buffer b2 (should projectile-mode))))
      (kill-buffer b1)
      (kill-buffer b2))))

(ert-deftest tychoish-core/projectile-disable-all-buffers-yielding ()
  "Test that `projectile-disable-all-buffers` disables projectile-mode in live buffers."
  (defvar hud-ecclectic-grep-project-map (make-sparse-keymap))
  (require 'projectile)
  (require 'gen)
  (let ((b1 (generate-new-buffer "test-proj-1"))
        (b2 (generate-new-buffer "test-proj-2"))
        (done-count nil))
    (unwind-protect
        (progn
          (with-current-buffer b1 (projectile-mode 1))
          (with-current-buffer b2 (projectile-mode 1))
          (cl-letf (((symbol-function 'buffer-list) (lambda (&optional _frame) (list b1 b2))))
            (projectile-disable-all-buffers
             :delay 0.0
             :on-done (lambda (cnt) (setq done-count cnt)))
            (let ((timeout 3.0)
                  (start (float-time)))
              (while (and (null done-count) (< (- (float-time) start) timeout))
                (sit-for 0.02)))
            (should (equal done-count 2))
            (with-current-buffer b1 (should-not projectile-mode))
            (with-current-buffer b2 (should-not projectile-mode))))
      (kill-buffer b1)
      (kill-buffer b2))))

(ert-deftest tychoish-core/projectile-load-project-buffers-yielding ()
  "Test that `projectile-load-project-buffers` loads project files incrementally."
  (defvar hud-ecclectic-grep-project-map (make-sparse-keymap))
  (require 'projectile)
  (require 'gen)
  (let* ((loaded-files nil)
         (done-loaded nil))
    (cl-letf (((symbol-function 'projectile-project-root) (lambda (&optional _) "/fake/project/"))
              ((symbol-function 'projectile-project-files) (lambda (_r) (list "a.txt" "b.txt")))
              ((symbol-function 'projectile-project-name) (lambda (&optional _) "fake"))
              ((symbol-function 'file-regular-p) (lambda (_) t))
              ((symbol-function 'find-buffer-visiting) (lambda (_) nil))
              ((symbol-function 'find-file-noselect) (lambda (f) (push f loaded-files) (generate-new-buffer " *dummy*"))))
      (projectile-load-project-buffers
       "/fake/project/"
       :delay 0.0
       :on-done (lambda (cnt) (setq done-loaded cnt)))
      (let ((timeout 3.0)
            (start (float-time)))
        (while (and (null done-loaded) (< (- (float-time) start) timeout))
          (sit-for 0.02)))
      (should (equal done-loaded 2))
      (should (equal (length loaded-files) 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell MCPKit session integration

(ert-deftest tychoish-core/antigravity-config-available-before-install ()
  "Allow selecting Antigravity before its executable has been installed."
  (require 'agent-shell)
  (should (tychoish/agent-shell--config-installed-p '((:identifier . antigravity)))))

(ert-deftest tychoish-core/agent-shell-mcpkit-active-url ()
  "Test active URL generation for direct and sprite-proxy MCP endpoints."
  (require 'agent-shell)
  (require 'server)
  ;; Direct port
  (cl-letf (((symbol-function 'mcpkit-active-port) (lambda () 8765))
            ((symbol-function 'mcpkit-proxy-active-port) (lambda () nil)))
    (let ((sprite-instance-id nil))
      (should (equal "http://127.0.0.1:8765/mcp" (agent-shell-mcpkit-active-url)))))
  ;; Proxy routing
  (cl-letf (((symbol-function 'mcpkit-active-port) (lambda () 8765))
            ((symbol-function 'mcpkit-proxy-active-port) (lambda () 8760)))
    (let ((sprite-instance-id "work"))
      (should (equal "http://127.0.0.1:8760/sprite/work/mcp" (agent-shell-mcpkit-active-url))))))

(ert-deftest tychoish-core/agent-shell-mcpkit-server-spec ()
  "Test ACP server spec construction."
  (require 'agent-shell)
  (require 'server)
  (cl-letf (((symbol-function 'agent-shell-mcpkit-active-url) (lambda () "http://127.0.0.1:8765/mcp")))
    (should (equal '((name . "emacs")
                     (type . "http")
                     (url . "http://127.0.0.1:8765/mcp")
                     (headers . ()))
                   (agent-shell-mcpkit-server-spec)))))

(ert-deftest tychoish-core/agent-shell-mcpkit-environment-variables ()
  "Test environment variable list generation."
  (require 'agent-shell)
  (require 'server)
  (cl-letf (((symbol-function 'mcpkit-active-port) (lambda () 8765)))
    (let ((sprite-instance-id "primary")
          (server-name "test-server"))
      (let ((env (agent-shell-mcpkit-environment-variables)))
        (should (member "MCPKIT_PORT=8765" env))
        (should (member "EMACS_MCP_PORT=8765" env))
        (should (member "EMACS_MCP_URL=http://127.0.0.1:8765/mcp" env))
        (should (member "EMACS_SPRITE_ID=primary" env))
        (should (member "EMACS_SERVER_NAME=test-server" env))))))

(ert-deftest tychoish-core/agent-shell-mcp-servers-dynamic-resolution ()
  "Test that `agent-shell--mcp-servers' evaluates dynamically and filters nil."
  (require 'agent-shell)
  (require 'server)
  ;; Active server returns normalized vector
  (cl-letf (((symbol-function 'agent-shell-mcpkit-server-spec)
             (lambda () '((name . "emacs") (type . "http") (url . "http://127.0.0.1:8765/mcp")))))
    (let ((agent-shell-mcp-servers (list (lambda () (agent-shell-mcpkit-server-spec)))))
      (should (equal [((name . "emacs") (type . "http") (url . "http://127.0.0.1:8765/mcp") (headers . []))]
                     (agent-shell--mcp-servers)))))
  ;; Inactive server returns nil
  (cl-letf (((symbol-function 'agent-shell-mcpkit-server-spec) (lambda () nil)))
    (let ((agent-shell-mcp-servers (list (lambda () (agent-shell-mcpkit-server-spec)))))
      (should-not (agent-shell--mcp-servers)))))

(ert-deftest tychoish-core/agent-shell-acp-client-injects-mcpkit-env ()
  "Test that `agent-shell--make-acp-client' advice injects MCPKit env vars."
  (require 'agent-shell)
  (require 'server)
  (cl-letf (((symbol-function 'agent-shell-mcpkit-environment-variables)
             (lambda () '("MCPKIT_PORT=8765" "EMACS_MCP_URL=http://127.0.0.1:8765/mcp"))))
    (let ((client (agent-shell--make-acp-client
                   :command "echo"
                   :command-params '("test")
                   :environment-variables '("FOO=bar"))))
      (let ((env (map-elt client :environment-variables)))
        (should (member "MCPKIT_PORT=8765" env))
        (should (member "EMACS_MCP_URL=http://127.0.0.1:8765/mcp" env))
        (should (member "FOO=bar" env))))))

(ert-deftest tychoish-core/gptel-mcp-session-setup ()
  "Test that `gptel-mcp-setup-session` configures buffer-local MCPKit tools."
  (require 'gptel)
  (require 'gptel-mcp)
  (let ((gptel-tools nil)
        (gptel-use-tools nil)
        (mcpkit-registry nil)
        (gptel--known-tools nil)
        (gptel-mcp-default-services '(core_test)))
    (mcpkit-define-service 'core_test :description "core test service")
    (mcpkit-register-tool 'core_test_tool 'core_test
      :description "core test tool"
      (identity "test-result"))
    (with-temp-buffer
      (gptel-mcp-setup-session (current-buffer))
      (should gptel-use-tools)
      (should (= (length gptel-tools) 1))
      (should (equal (gptel-tool-name (car gptel-tools)) "core_test_tool")))))

(ert-deftest tychoish-core/gptel-default-and-buffer-isolation ()
  "The configured default is OAuth and buffer selections remain independent."
  (require 'gptel)
  (should (gptel-openai-oauth-p (default-value 'gptel-backend)))
  (should (eq (default-value 'gptel-model) 'gpt-6.1-terra))
  (should (gptel-get-backend "gh-copilot"))
  (should (memq 'gpt-6.1-terra (gptel-backend-models (default-value 'gptel-backend))))
  (with-temp-buffer
    (setq-local gptel-model 'local-test-model)
    (with-temp-buffer
      (should (eq gptel-model 'gpt-6.1-terra)))
    (should (eq gptel-model 'local-test-model))))

(ert-deftest tychoish-core/agent-shell-queue-menu-bindings ()
  "Test that `agent-shell-queue' configures `agent-shell-menu' mode keys without error."
  (require 'agent-shell)
  (require 'agent-shell-queue)
  (require 'agent-shell-menu)
  (agent-shell-menu-install)
  (should (fboundp 'agent-shell-menu-dispatch))
  (should (equal (lookup-key agent-shell-mode-map (kbd "m"))
                 'agent-shell-menu-output-key-m))
  (should (equal (lookup-key agent-shell-mode-map (kbd "?"))
                 'agent-shell-menu-output-key-?)))

(ert-deftest tychoish-core/annotated-agent-shell-prefix-bindings ()
  (require 'setup-agent-shell)
  (require 'agent-shell-queue)
  (let ((agent-shell-map (cmdr-keys-prefix cmdr-mode-map "C-c r s" "agent-shell")))
    (should (eq (keymap-lookup agent-shell-mode-map "C-c j") agent-shell-map))
    (should (eq (keymap-lookup agent-shell-queue-mode-map "C-c j") agent-shell-map))))

(ert-deftest tychoish-core/annotated-magit-prefix-bindings ()
  (require 'magit)
  (should (eq (keymap-lookup magit-mode-map "/") magit-command-mode-map))
  (should (eq (keymap-lookup magit-command-mode-map "x") #'execute-extended-magit-command))
  (should (eq (keymap-lookup magit-command-mode-map "m") #'execute-extended-magit-command)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; alert default style resolution

(ert-deftest tychoish-core/alert-default-style-darwin-graphic ()
  "On macOS with a graphic display, alert defaults to osx-notifier."
  (cl-letf (((symbol-value 'system-type) 'darwin)
            ((symbol-function 'display-graphic-p) (lambda (&optional _) t)))
    (should (eq (tychoish-alert-default-style) 'osx-notifier))))

(ert-deftest tychoish-core/alert-default-style-darwin-terminal ()
  "On macOS in a terminal or headless session, alert defaults to notifier."
  (cl-letf (((symbol-value 'system-type) 'darwin)
            ((symbol-function 'display-graphic-p) (lambda (&optional _) nil)))
    (should (eq (tychoish-alert-default-style) 'notifier))))

(ert-deftest tychoish-core/alert-default-style-linux-dbus ()
  "On Linux, prefer libnotify even when a DBus session bus exists."
  (cl-letf (((symbol-value 'system-type) 'gnu/linux)
            (alert-libnotify-command "/usr/bin/notify-send")
            ((symbol-function 'getenv)
             (lambda (var) (when (equal var "DBUS_SESSION_BUS_ADDRESS") "unix:path=/tmp/dbus"))))
    (should (eq (tychoish-alert-default-style) 'libnotify))))

(ert-deftest tychoish-core/alert-default-style-linux-dbus-fallback ()
  "Use DBus notifications when notify-send is unavailable."
  (cl-letf (((symbol-value 'system-type) 'gnu/linux)
            (alert-libnotify-command nil)
            ((symbol-function 'getenv) (lambda (_) "unix:path=/tmp/dbus")))
    (should (eq (tychoish-alert-default-style) 'notifications))))

(ert-deftest tychoish-core/alert-default-style-linux-libnotify ()
  "On Linux without DBus but with alert-libnotify-command, alert defaults to libnotify."
  (cl-letf (((symbol-value 'system-type) 'gnu/linux)
            ((symbol-function 'getenv) (lambda (_) nil))
            (alert-libnotify-command "/usr/bin/notify-send"))
    (should (eq (tychoish-alert-default-style) 'libnotify))))

(ert-deftest tychoish-core/alert-default-style-fallback-message ()
  "Fallback alert style defaults to message when no desktop notifier is present."
  (cl-letf (((symbol-value 'system-type) 'berkeley-unix)
            ((symbol-function 'getenv) (lambda (_) nil))
            (alert-libnotify-command nil))
    (should (eq (tychoish-alert-default-style) 'message))))

(ert-deftest tychoish-core/alert-darwin-server-init-switches-and-removes-hook ()
  "When a GUI frame connects to a macOS daemon, switch style and detach hook."
  (let ((alert-default-style 'notifier)
        (server-after-make-frame-hook '(tychoish-alert-darwin-server-init)))
    ;; In a terminal client frame, style does not change and hook remains
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _) nil)))
      (tychoish-alert-darwin-server-init)
      (should (eq alert-default-style 'notifier))
      (should (memq #'tychoish-alert-darwin-server-init server-after-make-frame-hook)))
    ;; In a GUI client frame, style updates to osx-notifier and hook is removed
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _) t)))
      (tychoish-alert-darwin-server-init)
      (should (eq alert-default-style 'osx-notifier))
      (should-not (memq #'tychoish-alert-darwin-server-init server-after-make-frame-hook)))))

(defvar argi)

(ert-deftest tychoish-core/clix-sprite-id-flag ()
  "The --id CLI flag sets `sprite-cli-instance-id'."
  (let ((sprite-cli-instance-id nil)
        (argi "--id=my-instance"))
    (should (cli/sprite-id))
    (should (equal sprite-cli-instance-id "my-instance")))
  (let ((sprite-cli-instance-id nil)
        (argi "--id")
        (command-line-args-left '("other-instance")))
    (should (cli/sprite-id))
    (should (equal sprite-cli-instance-id "other-instance"))
    (should (null command-line-args-left))))

(provide 'test-setup-core)
;;; test-setup-core.el ends here
