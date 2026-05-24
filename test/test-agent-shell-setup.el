;;; test-agent-shell-setup.el --- ERT tests for agent-shell-setup -*- lexical-binding: t -*-

;; Run inside a live Emacs session with full config loaded:
;;   M-x ert RET t RET
;; or filtered:
;;   (ert "^agent-shell-setup/")

(require 'ert)
(require 'test-helper)
(require 'agent-shell-setup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-buffer-name-format lambda

(ert-deftest agent-shell-setup/buffer-name-format-basic ()
  (should (equal "*claude-my-project*"
                 (funcall agent-shell-buffer-name-format "Claude" "my-project"))))

(ert-deftest agent-shell-setup/buffer-name-format-uses-first-word-of-agent ()
  (should (equal "*claude-my-project*"
                 (funcall agent-shell-buffer-name-format "Claude Sonnet" "my-project"))))

(ert-deftest agent-shell-setup/buffer-name-format-lowercases-agent ()
  (should (equal "*claude-project*"
                 (funcall agent-shell-buffer-name-format "CLAUDE" "project"))))

(ert-deftest agent-shell-setup/buffer-name-format-extracts-basename-from-path ()
  (should (equal "*claude-my-project*"
                 (funcall agent-shell-buffer-name-format "Claude" "/home/user/my-project"))))

(ert-deftest agent-shell-setup/buffer-name-format-strips-trailing-slash ()
  (should (equal "*claude-my-project*"
                 (funcall agent-shell-buffer-name-format "Claude" "/home/user/my-project/"))))

(ert-deftest agent-shell-setup/buffer-name-format-slugifies-spaces ()
  (should (equal "*claude-my-project*"
                 (funcall agent-shell-buffer-name-format "Claude" "my project"))))

(ert-deftest agent-shell-setup/buffer-name-format-strips-leading-dots ()
  (should (equal "*claude-hidden*"
                 (funcall agent-shell-buffer-name-format "Claude" ".hidden"))))

(ert-deftest agent-shell-setup/buffer-name-format-trims-whitespace-from-agent ()
  (should (equal "*claude-project*"
                 (funcall agent-shell-buffer-name-format "  Claude  " "project"))))

(provide 'test-agent-shell-setup)
;;; test-agent-shell-setup.el ends here
