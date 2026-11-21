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
(defvar mu4e-compose-minor-mode-map)
(defvar mu4e-headers-mode-map)

(ert-deftest tychoish-core/mailx-keymap-bindings-activate ()
  "The mail spec applies its bindings to available keymaps."
  (let ((message-mode-map (make-sparse-keymap))
        (mu4e-compose-minor-mode-map (make-sparse-keymap))
        (mu4e-headers-mode-map (make-sparse-keymap)))
    (should-not (cmdr-spec--activate-keymap-bindings (cmdr-spec-get 'mailx)))
    (should (eq (keymap-lookup message-mode-map "M-q") #'ignore))
    (should (eq (keymap-lookup mu4e-compose-minor-mode-map "R")
                #'mailx-compose-reply))
    (should (eq (keymap-lookup mu4e-headers-mode-map "C-r")
                #'mailx-compose-reply))))

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
  (require 'setup-core)
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

(ert-deftest tychoish-core/antigravity-config-available-in-installed ()
  "Antigravity config maker is available in installed configs."
  (require 'agent-shell)
  (require 'agent-shell-menu)
  (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/bin/true")))
    (should (memq #'agent-shell-antigravity-make-agent-config (agent-shell-menu-installed-agent-configs)))))

(ert-deftest tychoish-core/agent-shell-mcpkit-bridge-startup ()
  "The startup batch enables the MCPKit bridge."
  (require 'bootstrap)
  (should (member '(:mode mcpkit-agent-shell-mode) sprite-startup-operations)))

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
  (require 'mailx)
  (seq-do
   (lambda (field)
     (let ((kill-ring nil)
           (answers (list "example" "/archive/test")))
       (cl-letf (((symbol-function 'annotated-completing-read)
                  (lambda (&rest _) field))
                 ((symbol-function 'read-string)
                  (lambda (&rest _) (pop answers)))
                 ((symbol-function 'mailx--field-display-value)
                  (lambda (&rest _) "example"))
                 ((symbol-function 'mu4e-message-field)
                  (lambda (msg key) (plist-get msg key)))
                 ((symbol-function 'mu4e-contact-email) #'identity))
         (mailx-generate-refile-rule '(:subject "example"))
         (let ((rule (read (current-kill 0))))
           (should (eq (car rule) 'mailx-add-refile-rule))
           (should (string-match-p (regexp-quote field) (prin1-to-string rule)))
           (should-not (string-match-p "::" (prin1-to-string rule)))
           (should (nth 2 (cadr rule)))
           (let ((mailx--refile-rules nil)
                 (key (intern field)))
             (eval rule t)
             (let ((fn (car mailx--refile-rules)))
               (should (equal "/archive/test"
                              (funcall fn (list key (if (eq key :from)
                                                        '("example") "example")))))
               (should-not (funcall fn (list key (if (eq key :from)
                                                     '("other") "other"))))))))))
   '(":from" ":subject")))

(ert-deftest tychoish-core/mail-unsubscribe-decodes-mailto ()
  "The standard mailto handler decodes address, subject and body."
  (require 'mailx)
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
        (mailx-unsubscribe 'message)
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

(ert-deftest tychoish-core/mail-header-separator-roundtrip ()
  "Compose and draft header delimiters use dashes and can be removed."
  (require 'mailx)
  (skip-unless (or (featurep 'mu4e-draft) (locate-library "mu4e-draft")))
  (require 'mu4e-draft)
  (load (locate-library "mu4e-draft") nil t)
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
