;;; test-clix.el --- ERT tests for clix.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Unit tests for `register-clix-flag' macro and CLI flag handling.

;;; Code:

(require 'ert)
(require 'test-helper)
(require 'cl-lib)
(require 'clix)

(defvar argi)
(defvar command-line-args-left)
(defvar test-hook nil)

(ert-deftest clix-test/register-flag-consumes-argument ()
  "Test that `register-clix-flag' with :consume t sets clix-arg-<name> and pops command-line-args-left."
  (let ((command-line-functions nil))
    (register-clix-flag test-consume
      :flag "--test-consume"
      :doc "Test consuming flag."
      :consume t)

    (should (boundp 'clix-arg-test-consume))
    (should (fboundp 'cli/test-consume))
    (should (memq 'cli/test-consume command-line-functions))

    (let ((argi "--test-consume")
          (command-line-args-left '("my-value" "extra-arg")))
      (should (cli/test-consume))
      (should (equal clix-arg-test-consume "my-value"))
      (should (equal command-line-args-left '("extra-arg"))))))

(ert-deftest clix-test/register-flag-no-consume ()
  "Test that `register-clix-flag' with :consume nil does not consume from command-line-args-left."
  (let ((command-line-functions nil)
        (triggered nil))
    (register-clix-flag test-no-consume
      :flag "--test-toggle"
      :doc "Test toggle flag."
      :consume nil
      :handler (setq triggered t))

    (should (fboundp 'cli/test-no-consume))
    (should-not (boundp 'clix-arg-test-no-consume))

    (let ((argi "--test-toggle")
          (command-line-args-left '("keep-this")))
      (should (cli/test-no-consume))
      (should (eq triggered t))
      (should (equal command-line-args-left '("keep-this"))))))

(ert-deftest clix-test/register-flag-custom-var ()
  "Test that `register-clix-flag' with explicit :var sets the custom symbol."
  (let ((command-line-functions nil))
    (register-clix-flag test-custom-var
      :flag "--test-custom"
      :doc "Test custom variable flag."
      :var my-custom-cli-var
      :consume t)

    (should (boundp 'my-custom-cli-var))

    (let ((argi "--test-custom")
          (command-line-args-left '("custom-val")))
      (should (cli/test-custom-var))
      (should (equal my-custom-cli-var "custom-val"))
      (should (null command-line-args-left)))))

(ert-deftest clix-test/register-flag-immediate-action ()
  "Test that :action executes immediately when :hook is nil."
  (let ((command-line-functions nil)
        (action-executed nil))
    (register-clix-flag test-immediate
      :flag "--test-imm"
      :doc "Test immediate action."
      :consume t
      :hook nil
      :action (setq action-executed (format "ran with %s" clix-arg-test-immediate)))

    (let ((argi "--test-imm")
          (command-line-args-left '("payload")))
      (should (cli/test-immediate))
      (should (equal action-executed "ran with payload")))))

(ert-deftest clix-test/register-flag-hooked-action ()
  "Test that :action registers on hook when :hook is specified."
  (let ((command-line-functions nil)
        (test-hook nil)
        (action-executed nil))
    (register-clix-flag test-hooked
      :flag "--test-hook"
      :doc "Test hooked action."
      :consume t
      :hook 'test-hook
      :action (setq action-executed clix-arg-test-hooked))

    (should (fboundp 'cli/test-hooked-run))
    (should (memq 'cli/test-hooked-run test-hook))

    (let ((argi "--test-hook")
          (command-line-args-left '("delayed-payload")))
      (should (cli/test-hooked))
      (should-not action-executed)
      (run-hooks 'test-hook)
      (should (equal action-executed "delayed-payload")))))

(ert-deftest clix-test/unmatched-flag-returns-nil ()
  "Test that a flag returns nil when argi does not match."
  (register-clix-flag test-unmatched
    :flag "--test-unmatched"
    :consume nil)
  (let ((argi "--other-flag")
        (command-line-args-left nil))
    (should-not (cli/test-unmatched))))

(ert-deftest clix-test/exact-and-attached-values ()
  (register-clix-flag test-exact :flag "--test-exact" :doc "Test value.")
  (let ((argi "--test-exact-other") (command-line-args-left '("keep")))
    (should-not (cli/test-exact))
    (should (equal command-line-args-left '("keep"))))
  (let ((argi "--test-exact=a=b") (command-line-args-left '("keep")))
    (should (cli/test-exact))
    (should (equal clix-arg-test-exact "a=b"))
    (should (equal command-line-args-left '("keep"))))
  (let ((argi "--test-exact") (command-line-args-left nil))
    (should-error (cli/test-exact) :type 'user-error)))

(ert-deftest clix-test/final-and-hooked-switch ()
  (let ((test-hook nil) (command-line-functions nil) (exits nil) (ran nil))
    (register-clix-flag test-final :flag "--test-final" :consume nil
      :hook 'test-hook :action (setq ran t) :final ran)
    (cl-letf (((symbol-function 'kill-emacs) (lambda (code &rest _) (push code exits))))
      (run-hooks 'test-hook)
      (should-not ran)
      (let ((argi "--test-final")) (should (cli/test-final)))
      (run-hooks 'test-hook)
      (should ran)
      (should (equal exits '(0)))
      (run-hooks 'test-hook)
      (should (equal exits '(0))))))

(ert-deftest clix-test/help-and-subcommands ()
  (let ((output (with-output-to-string (clix-help))))
    (should (string-match-p (regexp-quote "--clix-help") output))
    (should-not (string-match-p "^  --help
" output))
    (should (string-match-p (regexp-quote "--org-exec VALUE") output)))
  (let ((argi "help") (called nil))
    (cl-letf (((symbol-function 'kill-emacs) (lambda (&rest _) (setq called t))))
      (with-output-to-string (should (cli/help-command)))
      (should called))))

(provide 'test-clix)
;;; test-clix.el ends here
