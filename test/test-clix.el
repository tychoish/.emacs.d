;;; test-clix.el --- ERT tests for clix.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Unit tests for `register-clix-flag' macro and CLI flag handling.

;;; Code:

(require 'ert)
(require 'test-helper)
(require 'cl-lib)
(require 'clix)

(defvar argi)
(defvar argv)
(defvar test-hook nil)

(ert-deftest clix-test/register-flag-consumes-argument ()
  "Test that `register-clix-flag' with :consume t sets clix-arg-<name> and pops argv."
  (let ((command-line-functions nil))
    (register-clix-flag test-consume
      :flag "--test-consume"
      :doc "Test consuming flag."
      :consume t)

    (should (boundp 'clix-arg-test-consume))
    (should (fboundp 'cli/test-consume))
    (should (memq 'cli/test-consume command-line-functions))

    (let ((argi "--test-consume")
          (argv '("my-value" "extra-arg")))
      (should (cli/test-consume))
      (should (equal clix-arg-test-consume "my-value"))
      (should (equal argv '("extra-arg"))))))

(ert-deftest clix-test/register-flag-no-consume ()
  "Test that `register-clix-flag' with :consume nil does not consume from argv."
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
          (argv '("keep-this")))
      (should (cli/test-no-consume))
      (should (eq triggered t))
      (should (equal argv '("keep-this"))))))

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
          (argv '("custom-val")))
      (should (cli/test-custom-var))
      (should (equal my-custom-cli-var "custom-val"))
      (should (null argv)))))

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
          (argv '("payload")))
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
          (argv '("delayed-payload")))
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
        (argv nil))
    (should-not (cli/test-unmatched))))

(provide 'test-clix)
;;; test-clix.el ends here
