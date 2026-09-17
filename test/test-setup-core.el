;;; test-setup-core.el --- ERT tests for setup-core.el -*- lexical-binding: t; no-byte-compile: t; -*-

;; Run inside a live Emacs session with full config loaded:
;;   M-x ert RET t RET
;; or filtered:
;;   (ert "^tychoish-core/")

(require 'ert)
(require 'test-helper)
(require 'cl-lib)
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
  "Test that `tychoish-compilation-read-command' processes `builder--read-command'
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
      (should (equal "make -j8" (tychoish-compilation-read-command "make"))))))
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
;;; eglot hooks & eglot-tempel integration

(ert-deftest tychoish-core/eglot-managed-mode-hook-functions-bound ()
  (dolist (fn eglot-managed-mode-hook)
    (should (fboundp fn))))

(ert-deftest tychoish-core/eglot-tempel-mode-not-on-hook ()
  "Ensure `eglot-tempel-mode' is not on `eglot-managed-mode-hook'.
`eglot-tempel-mode' is a global mode enabled in its `:config' block;
putting it on `eglot-managed-mode-hook' causes a reconnect loop on
buffer transitions."
  (should-not (memq 'eglot-tempel-mode eglot-managed-mode-hook))
  (should-not (memq 'tychoish/eglot-tempel-enable eglot-managed-mode-hook)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; eglot/gopls stale-connection and workspace-configuration regressions
;;
;; 2026-09-17: a crash/reconnect storm against a large Go monorepo traced to
;; three distinct bugs, each covered below:
;;   1. `tychoish/eglot-prune-duplicate-servers' shut down duplicate servers
;;      via raw `jsonrpc-shutdown' without marking them as a deliberate
;;      shutdown, so core Eglot's `eglot--on-shutdown' treated the pruning
;;      itself as an unexpected crash and auto-reconnected, feeding a
;;      self-sustaining loop.
;;   2. A now-removed `eglot-register-capability' advice deferred replying
;;      to a server's `client/registerCapability' request for up to two
;;      minutes while waiting for Projectile's cache to warm -- a protocol
;;      stall that any LSP server (not just gopls) can read as a broken
;;      connection. Fixed by declining the `workspace/didChangeWatchedFiles'
;;      dynamic-registration capability outright.
;;   3. `tychoish/eglot-default-server-configuration' had pylsp's `:plugins'
;;      and yaml's `:format' each missing a level of parens, so their
;;      "value" was a bare keyword (e.g. `:black') instead of a nested
;;      plist. Eglot bundles every section into one JSON blob on every
;;      connect (regardless of which server is connecting), so this raised
;;      a real `(wrong-type-argument json-value-p :black)' error on every
;;      single connection attempt, disrupting the handshake.

(defmacro tychoish-core-test--with-fake-eglot-server (var &rest body)
  "Bind VAR to a minimal live `eglot-lsp-server' instance for BODY.
`eglot-lsp-server' initialization requires a real process (it calls
`process-put' on its `:process' slot) and a real project (some capability
helpers call `project-root' on it), so this spawns an inert process and a
transient project and cleans both up afterward."
  (declare (indent 1))
  `(let* ((tychoish-core-test--proc (start-process "tychoish-core-test" nil "cat"))
          (,var (make-instance 'eglot-lsp-server :name "test"
                                :process tychoish-core-test--proc)))
     (setf (eglot--project ,var) (cons 'transient temporary-file-directory))
     (unwind-protect
         (progn ,@body)
       (delete-process tychoish-core-test--proc))))

(ert-deftest tychoish-core/eglot-prune-duplicate-servers-marks-shutdown-requested ()
  "Regression test: pruning duplicates must not look like a crash to Eglot.
Without marking `eglot--shutdown-requested', core Eglot's `eglot--on-shutdown'
auto-reconnects any server it doesn't believe was shut down on purpose,
turning routine dedup into an infinite reconnect loop."
  (tychoish-core-test--with-fake-eglot-server keep
    (tychoish-core-test--with-fake-eglot-server dup
      (let* ((project 'test-project)
             (eglot--servers-by-project (make-hash-table :test #'equal))
             (shutdown-calls nil))
        (puthash project (list keep dup) eglot--servers-by-project)
        (cl-letf (((symbol-function 'jsonrpc-shutdown)
                   (lambda (server &optional _cleanup) (push server shutdown-calls))))
          (tychoish/eglot-prune-duplicate-servers))
        (should (equal (list dup) shutdown-calls))
        (should (eglot--shutdown-requested dup))
        (should-not (eglot--shutdown-requested keep))
        (should (equal (list keep) (gethash project eglot--servers-by-project)))))))

(ert-deftest tychoish-core/eglot-declines-didChangeWatchedFiles-dynamic-registration ()
  "Regression test: never advertise `workspace/didChangeWatchedFiles'
dynamic registration. A prior version instead deferred the reply to a
server's registration request for up to two minutes while Projectile's
cache warmed, which several LSP servers (including gopls) treated as a
dead connection."
  (tychoish-core-test--with-fake-eglot-server server
    (let ((caps (eglot-client-capabilities server)))
      (should (equal '(:dynamicRegistration :json-false)
                     (plist-get (plist-get caps :workspace) :didChangeWatchedFiles))))))

(ert-deftest tychoish-core/eglot-activate-editing-mode-throttled ()
  "Regression test: connecting to a project with many already-open matching
buffers must not activate all of them at once. `eglot--maybe-activate-editing-mode'
runs in a synchronous sweep over every open buffer at connect time, and each
activation signals `textDocument/didOpen' plus diagnostics -- fine for a
few buffers, but a burst can overwhelm a heavily-loaded server."
  (let* ((tychoish/eglot-activate-recent nil)
         (tychoish/eglot-activate-queue nil)
         (tychoish/eglot-activate-timer nil)
         (call-count 0)
         (total (* 2 tychoish/eglot-activate-burst-limit))
         (buffers (mapcar (lambda (n) (generate-new-buffer (format "eglot-throttle-test-%d" n)))
                           (number-sequence 1 total))))
    (unwind-protect
        (progn
          (seq-do (lambda (buf)
                    (with-current-buffer buf
                      (ad:eglot--maybe-activate-editing-mode-throttle
                       (lambda () (cl-incf call-count)))))
                  buffers)
          (should (= call-count tychoish/eglot-activate-burst-limit))
          (should (= (- total tychoish/eglot-activate-burst-limit)
                     (length tychoish/eglot-activate-queue)))
          (should tychoish/eglot-activate-timer))
      (when (timerp tychoish/eglot-activate-timer)
        (cancel-timer tychoish/eglot-activate-timer))
      (seq-do #'kill-buffer buffers))))

(ert-deftest tychoish-core/eglot-activate-queue-drains-in-batches ()
  "Regression test: queued buffers actually get processed, not stranded."
  (let* ((tychoish/eglot-activate-recent nil)
         (tychoish/eglot-activate-queue nil)
         (tychoish/eglot-activate-timer nil)
         (activated nil)
         (total (* 2 tychoish/eglot-activate-burst-limit))
         (buffers (mapcar (lambda (n) (generate-new-buffer (format "eglot-drain-test-%d" n)))
                           (number-sequence 1 total))))
    (unwind-protect
        (cl-letf (((symbol-function 'eglot--maybe-activate-editing-mode)
                   (lambda () (push (current-buffer) activated))))
          (setq tychoish/eglot-activate-queue (copy-sequence buffers))
          (tychoish/eglot-activate-process-queue)
          (should (= tychoish/eglot-activate-burst-limit (length activated)))
          (should (= (- total tychoish/eglot-activate-burst-limit)
                     (length tychoish/eglot-activate-queue)))
          (tychoish/eglot-activate-process-queue)
          (should (= total (length activated)))
          (should (null tychoish/eglot-activate-queue)))
      (when (timerp tychoish/eglot-activate-timer)
        (cancel-timer tychoish/eglot-activate-timer))
      (seq-do #'kill-buffer buffers))))

(ert-deftest tychoish-core/eglot-activate-prioritizes-visible-buffer ()
  "Regression test: a buffer stuck in the throttle queue must activate
immediately once it's actually visible, rather than waiting out the full
drain cycle just because it missed the initial connect's activation burst."
  (let* ((tychoish/eglot-activate-recent nil)
         (buf (generate-new-buffer "eglot-visible-test"))
         (other (generate-new-buffer "eglot-visible-test-other"))
         (tychoish/eglot-activate-queue (list buf other))
         (activated nil))
    (unwind-protect
        (cl-letf (((symbol-function 'eglot--maybe-activate-editing-mode)
                   (lambda () (push (current-buffer) activated))))
          (tychoish/eglot-activate-prioritize-visible (selected-window))
          (should-not activated)
          (with-current-buffer buf
            (tychoish/eglot-activate-prioritize-visible
             (progn (set-window-buffer (selected-window) buf) (selected-window))))
          (should (equal (list buf) activated))
          (should-not (memq buf tychoish/eglot-activate-queue))
          (should (memq other tychoish/eglot-activate-queue)))
      (seq-do #'kill-buffer (list buf other)))))

(defun tychoish-core-test--flatten-workspace-configuration (val)
  "Reproduce `eglot--workspace-configuration-plist's flattening of VAL.
Duplicated here (rather than calling the real function) because that
function requires a live, connected SERVER to resolve a project root;
this is the exact same destructuring logic applied to our own data."
  (or (and (consp (car val))
           (cl-loop for (section . v) in val
                    collect (if (keywordp section) section (intern (format ":%s" section)))
                    collect v))
      val))

(ert-deftest tychoish-core/eglot-default-server-configuration-encodes-cleanly ()
  "Regression test: every section in `tychoish/eglot-default-server-configuration'
must encode to valid JSON. A missing level of parens around a section's
settings (e.g. `:plugins' or `:format') silently turns that key's value
into a bare keyword, which `jsonrpc--json-encode' rejects -- and since
Eglot bundles every section into one blob on every connect, this broke
every server's connection, not just the misconfigured one."
  (should (stringp
           (jsonrpc--json-encode
            (tychoish-core-test--flatten-workspace-configuration
             tychoish/eglot-default-server-configuration)))))

(ert-deftest tychoish-core/eglot-default-server-configuration-nesting ()
  "Regression test: pylsp's `:plugins' and yaml's `:format' must be nested
plists, not a bare keyword sitting where a plist is expected."
  (let* ((flat (tychoish-core-test--flatten-workspace-configuration
                tychoish/eglot-default-server-configuration))
         (pylsp (plist-get flat :pylsp))
         (yaml (plist-get flat :yaml)))
    (when pylsp
      (should (listp (plist-get pylsp :plugins)))
      (should-not (keywordp (plist-get pylsp :plugins))))
    (when yaml
      (should (listp (plist-get yaml :format)))
      (should-not (keywordp (plist-get yaml :format))))))

(provide 'test-setup-core)
;;; test-setup-core.el ends here
