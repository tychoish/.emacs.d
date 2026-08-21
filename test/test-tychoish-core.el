;;; test-tychoish-core.el --- ERT tests for tychoish-core.el -*- lexical-binding: t; no-byte-compile: t; -*-

;; Run inside a live Emacs session with full config loaded:
;;   M-x ert RET t RET
;; or filtered:
;;   (ert "^tychoish-core/")

(require 'ert)
(require 'test-helper)
(require 'cl-lib)
(require 'tychoish-core)
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
  (require 'tychoish-mail)
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

(ert-deftest tychoish-core/eglot-tempel-mode-on-hook ()
  "The hook must call a fixed-arg enable wrapper, not the bare mode
function: `eglot-managed-mode-hook' runs with no arguments on both
connect and disconnect, so calling `eglot-tempel-mode' directly would
toggle it (and reconnect the server) on every managed buffer
transition."
  (should (memq 'tychoish/eglot-tempel-enable eglot-managed-mode-hook))
  (should-not (memq 'eglot-tempel-mode eglot-managed-mode-hook)))

(provide 'test-tychoish-core)
;;; test-tychoish-core.el ends here
