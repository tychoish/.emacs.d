;;; test-setup-core.el --- ERT tests for setup-core.el -*- lexical-binding: t; no-byte-compile: t; -*-

;; Run inside a live Emacs session with full config loaded:
;;   M-x ert RET t RET
;; or filtered:
;;   (ert "^tychoish-core/")

(require 'ert)
(require 'test-helper)
(require 'cl-lib)
(require 'hud-mode)
(defvar hud-mode-map (make-sparse-keymap))
(defvar hud-ecclectic-grep-project-map (make-sparse-keymap))
(require 'setup-core)
(require 'builder)

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
  (require 'nerd-icons)
  (seq-do (lambda (mode)
            (should-not
             (condition-case err
                 (progn (nerd-icons-icon-for-mode mode) nil)
               (error err))))
          tychoish-core-test--registered-modes))

(ert-deftest tychoish-core/nerd-icons-icon-for-buffer-degrades-gracefully ()
  "The safe advice returns a string even when given an unregistered mode."
  (require 'nerd-icons)
  (with-temp-buffer
    (setq major-mode 'tychoish-core-test--nonexistent-mode-xyz)
    (should (stringp (nerd-icons-icon-for-buffer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; python-ts-mode buffer-local setup hook

(ert-deftest tychoish-core/python-ts-mode-setup-runs-on-hook ()
  "`tychoish/python-setup' must be registered on the actual mode hook.

Regression test: this was previously wired to the symbol `python-ts-mode'
\(the mode function itself, which is never a hook variable\) instead of
`python-ts-mode-hook', so the setup never ran.  Also previously the
use-package block was named `python-ts-mode', but that symbol is never
`provide'd -- `python-ts-mode' is defined inside python.el, which only
provides the feature `python' -- so `with-eval-after-load' never fired
from a plain `require'.  The block is now named `python' to match the
feature it actually depends on, so a plain `require' triggers it."
  (require 'python)
  (should (memq 'tychoish/python-setup python-ts-mode-hook)))

(ert-deftest tychoish-core/compilation-read-command ()
  "Test that `builder-compilation-read-command' processes `builder--read-command'
correctly and reads from the minibuffer using the candidate's command."
  (let* ((c (make-builder-candidate :command "make -j8" :name "build"))
         (mock-results (cons c nil)))
    (cl-letf (((symbol-function 'builder--read-command)
               (lambda (_cmd) mock-results))
              ((symbol-function 'read-from-minibuffer)
               (lambda (prompt val)
                 (should (equal "edit command => " prompt))
                 (should (equal "make -j8" val))
                 val)))
      (should (equal "make -j8" (builder-compilation-read-command "make"))))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tychoish-mail account definition and immediate activation

(ert-deftest tychoish-core/mail-account-definition-activates-default ()
  "Test that `tychoish-define-mail-account' activates a default account immediately upon definition."
  (require 'setup-mail)
  (let ((hud-mail-map (make-sparse-keymap))
        (tychoish-mail-accounts-table (make-hash-table :test #'equal))
        (tychoish-mail-account-current nil)
        (user-mail-address nil)
        (user-full-name nil))
    (setq mu4e-get-mail-command nil)
    (cl-letf (((symbol-function 'mu4e) (lambda (&rest _args) nil)))
      (tychoish-define-mail-account
       :name "Test User"
       :id "test-acc"
       :address "test@example.com"
       :key "x"
       :maildir "/tmp/test-mail"
       :default t
       :command "true")
      (should (equal tychoish-mail-account-current "tychoish-mail-test-acc"))
      (should (equal user-mail-address "test@example.com"))
      (should (equal user-full-name "Test User"))
      (should (equal mu4e-get-mail-command "true")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Projectile yielding / incremental operations

(ert-deftest tychoish-core/projectile-enable-all-buffers-yielding ()
  "Test that `projectile-enable-all-buffers` enables projectile-mode in live buffers."
  (require 'hud-mode nil t)
  (defvar hud-mode-map (make-sparse-keymap))
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
  (require 'hud-mode nil t)
  (defvar hud-mode-map (make-sparse-keymap))
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
  (require 'hud-mode nil t)
  (defvar hud-mode-map (make-sparse-keymap))
  (defvar hud-ecclectic-grep-project-map (make-sparse-keymap))
  (require 'projectile)
  (require 'gen)
  (let* ((loaded-files nil)
         (done-loaded nil))
    (cl-letf (((symbol-function 'projectile-project-root) (lambda (&optional _) "/fake/project/"))
              ((symbol-function 'projectile-project-files) (lambda (_r) (list "a.txt" "b.txt")))
              ((symbol-function 'projectile-project-name) (lambda () "fake"))
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

(provide 'test-setup-core)
;;; test-setup-core.el ends here
