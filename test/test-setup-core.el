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
(defvar mu4e-get-mail-command)
(defvar mu4e-mu-home)
(defvar mu4e--started)
(defvar mu4e--server-indexing)
(defvar mu4e--update-buffer)

(ert-deftest tychoish-core/modus-theme-configuration-loads ()
  "The loaded configuration sets the Modus palette options."
  (should modus-themes-deuteranopia)
  (should (equal modus-themes-common-palette-overrides
                 '((border-mode-line-active bg-mode-line-active)
                   (border-mode-line-inactive bg-mode-line-inactive)
                   (message-separator bg-main)))))

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
  (should (tychoish--agent-shell--config-installed-p '((:identifier . antigravity)))))

(ert-deftest tychoish-core/agent-shell-mcpkit-bridge-startup ()
  "The startup batch enables the MCPKit bridge."
  (require 'bootstrap)
  (should (memq 'mcpkit-agent-shell-mode sprite-startup-operations)))

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

(ert-deftest tychoish-core/refile-rule-generator-preserves-keywords ()
  "Generated rules should match the selected message field."
  (require 'setup-mail)
  (seq-do
   (lambda (field)
     (let ((kill-ring nil)
           (answers (list "example" "/archive/test")))
       (cl-letf (((symbol-function 'annotated-completing-read)
                  (lambda (&rest _) field))
                 ((symbol-function 'read-string)
                  (lambda (&rest _) (pop answers)))
                 ((symbol-function 'tychoish-mail--field-display-value)
                  (lambda (&rest _) "example"))
                 ((symbol-function 'mu4e-message-field)
                  (lambda (msg key) (plist-get msg key)))
                 ((symbol-function 'mu4e-contact-email) #'identity))
         (tychoish-mail-generate-refile-rule '(:subject "example"))
         (let ((rule (read (current-kill 0))))
           (should (eq (car rule) 'tychoish-mail-add-refile-rule))
           (should (string-match-p (regexp-quote field) (prin1-to-string rule)))
           (should-not (string-match-p "::" (prin1-to-string rule)))
           (should (nth 2 (cadr rule)))
           (let ((tychoish-mail--refile-rules nil)
                 (key (intern field)))
             (eval rule t)
             (let ((fn (car tychoish-mail--refile-rules)))
               (should (equal "/archive/test"
                              (funcall fn (list key (if (eq key :from)
                                                        '("example") "example")))))
               (should-not (funcall fn (list key (if (eq key :from)
                                                     '("other") "other"))))))))))
   '(":from" ":subject")))

(ert-deftest tychoish-core/desktop-exclusions-include-remote-and-local-paths ()
  "Desktop exclusions independently match remote and dependency paths."
  (require 'desktop)
  (should (string-match-p desktop-files-not-to-save "/ssh:example:/tmp/file"))
  (should (string-match-p desktop-files-not-to-save "/usr/lib/go/src/file.go"))
  (should (string-match-p desktop-files-not-to-save "/home/user/go/pkg/mod/pkg/file.go"))
  (should-not (string-match-p desktop-files-not-to-save "/home/user/src/file.go"))
  (should (string-match-p desktop-buffers-not-to-save " *temporary*")))

(ert-deftest tychoish-core/agent-shell-terse-environment-follows-toggle ()
  "The persona is supplied only when terse output is enabled."
  (require 'agent-shell)
  (let ((agent-shell-anthropic-claude-environment nil)
        (agent-shell-omp-environment nil)
        (process-environment '("CLAUDE_PERSONA=inherited"))
        (tychoish/agent-shell-terse-output t))
    (tychoish--agent-shell--apply-environment)
    (should (seq-some (lambda (e) (string-prefix-p "CLAUDE_PERSONA=" e))
                      agent-shell-anthropic-claude-environment))
    (setq tychoish/agent-shell-terse-output nil)
    (tychoish--agent-shell--apply-environment)
    (should-not (seq-some (lambda (e) (string-prefix-p "CLAUDE_PERSONA=" e))
                          agent-shell-anthropic-claude-environment))))

(ert-deftest tychoish-core/interrupt-recovery-requires-confirmation ()
  "Declining interruption must not cancel a request or arm recovery."
  (require 'agent-shell)
  (with-temp-buffer
    (setq major-mode 'agent-shell-mode)
    (let ((agent-shell--state '((:session . ((:id . "session")))
                                (:active-requests . (request))))
          (confirmed nil)
          (called nil)
          (armed nil))
      (cl-letf (((symbol-function 'agent-shell-interrupt-confirmed-p)
                 (lambda () confirmed))
                ((symbol-function 'run-with-timer)
                 (lambda (&rest _) (setq armed t))))
        (ad:agent-shell-interrupt-recover-if-unanswered
         (lambda (&rest _) (setq called t)))
        (should-not called)
        (should-not armed)
        (setq confirmed t)
        (ad:agent-shell-interrupt-recover-if-unanswered
         (lambda (&rest args) (setq called args)))
        (should (equal called '(t)))
        (should armed)))))

(ert-deftest tychoish-core/mail-unsubscribe-decodes-mailto ()
  "The standard mailto handler decodes address, subject and body."
  (require 'setup-mail)
  (require 'url-mailto)
  (with-temp-buffer
    (message-mode)
    (let ((url-mail-command 'compose-mail)
          (url-request-data nil))
      (cl-letf (((symbol-function 'mu4e-fetch-field)
                 (lambda (&rest _) "<mailto:list%40example.org?subject=Please%20remove&body=Stop%20mail>"))
                ((symbol-function 'compose-mail)
                 (lambda (&rest _)
                   (erase-buffer)
                   (insert "To: \nSubject: \n" mail-header-separator "\n"))))
        (tychoish-mail-unsubscribe 'message)
        (should (string-match-p "list@example.org" (buffer-string)))
        (should (string-match-p "Please remove" (buffer-string)))
        (should (string-match-p "Stop mail" (buffer-string)))))))

(ert-deftest tychoish-core/agent-shell-insert-without-focus-preserves-point ()
  "Inserting without focus leaves the shell's point undisturbed."
  (require 'agent-shell)
  (with-temp-buffer
    (insert "existing")
    (goto-char 1)
    (setq-local agent-shell--state '((:session . ((:id . "session")))))
    (cl-letf (((symbol-function 'shell-maker-busy) (lambda () nil)))
      (agent-shell--insert-to-shell-buffer
       :shell-buffer (current-buffer) :text "new" :no-focus t)
      (should (= (point) 1))
      (should (string-suffix-p "new" (buffer-string))))))

(defvar tychoish-core-test--interrupt-before-called nil)

(defun tychoish-core-test--interrupt-before (&rest _)
  "Record whether before advice runs."
  (setq tychoish-core-test--interrupt-before-called t))

(ert-deftest tychoish-core/interrupt-decline-skips-later-before-advice ()
  "Declining confirmation must not run later-installed queue advice."
  (require 'agent-shell)
  (with-temp-buffer
    (setq major-mode 'agent-shell-mode)
    (setq-local agent-shell--state '((:session . ((:id . "session")))))
    (let ((tychoish-core-test--interrupt-before-called nil))
      (unwind-protect
          (progn
            (advice-add 'agent-shell-interrupt :before #'tychoish-core-test--interrupt-before)
            (cl-letf (((symbol-function 'agent-shell-interrupt-confirmed-p) (lambda () nil)))
              (agent-shell-interrupt))
            (should-not tychoish-core-test--interrupt-before-called))
        (advice-remove 'agent-shell-interrupt #'tychoish-core-test--interrupt-before)))))

(ert-deftest tychoish-core/mail-header-separator-roundtrip ()
  "Compose and draft header delimiters use dashes and can be removed."
  (require 'setup-mail)
  (skip-unless (or (featurep 'mu4e-draft) (locate-library "mu4e-draft")))
  (require 'mu4e-draft)
  (let ((expected "--------------------------"))
    (should (equal (substring-no-properties mail-header-separator) expected))
    (should (equal (substring-no-properties mu4e--header-separator) expected))
    (with-temp-buffer
      (insert "Subject: Example\n\nBody\n")
      (mu4e--delimit-headers)
      (should (string-match-p (concat "^" (regexp-quote expected) "$") (buffer-string)))
      (mu4e--delimit-headers t)
      (should (equal (buffer-string) "Subject: Example\n\nBody\n"))
      (mu4e--delimit-headers)
      (mail-sendmail-undelimit-header)
      (should (equal (buffer-string) "Subject: Example\n\nBody\n")))))

(provide 'test-setup-core)
;;; test-setup-core.el ends here
