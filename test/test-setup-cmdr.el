;;; test-setup-cmdr.el --- Tests for personal cmdr registrations -*- lexical-binding: t; -*-

;;; Commentary:
;; Verify personal commands are registered without discarding other entries.

;;; Code:

(require 'ert)
(require 'cmdr)
(require 'setup-cmdr)

(ert-deftest setup-cmdr-registers-personal-commands-idempotently ()
  "Register the menu twice while preserving independently added commands."
  (let ((cmdr-command-table nil))
    (cmdr-register-command :category 'external :command 'ignore :transient-key "zz")
    (setup-cmdr-register-commands)
    (let ((count (length (cmdr--flat-entries))))
      (should (= count 29))
      (should (assq 'denote cmdr-command-table))
      (should (assq 'external cmdr-command-table))
      (should (eq 2 (cmdr-command-row (cadr (assq 'org-mode cmdr-command-table)))))
      (setup-cmdr-register-commands)
      (should (= count (length (cmdr--flat-entries)))))))

(ert-deftest setup-cmdr-installs-personal-bindings-idempotently ()
  "Install generic plugins and personal keys without replacing other keys."
  (require 'cmdr-keys)
  (let ((cmdr-mode-map (make-sparse-keymap)))
    (setup-cmdr-install-bindings)
    (should (eq (keymap-lookup cmdr-mode-map "C-c t s") #'daemons-dash))
    (should (eq (keymap-lookup cmdr-mode-map "C-c t e") #'execute-extended-elpaish-command))
    (should (eq (keymap-lookup cmdr-mode-map "C-c r s n") #'agent-shell-new-shell))
    (should (eq (keymap-lookup cmdr-mode-map "C-x .") #'cmdr-menu))
    (let ((snapshot (copy-tree cmdr-mode-map)))
      (setup-cmdr-install-bindings)
      (should (equal snapshot cmdr-mode-map)))))

(ert-deftest setup-cmdr-personal-command-generators ()
  "Personal extended commands remain available without HUD."
  (should (commandp #'execute-extended-elpaish-command))
  (should (commandp #'execute-extended-agent-shell-command))
  (should (commandp #'execute-extended-gptel-set-backend-command))
  (should (read-extended-command-for-gptel-prefix-p 'gptel-rewrite nil))
  (should-not (read-extended-command-for-gptel-prefix-p 'other-command nil)))

(provide 'test-setup-cmdr)
;;; test-setup-cmdr.el ends here
