;;; test-agent-shell-setup.el --- ERT tests for agent-shell buffer-name formatting -*- lexical-binding: t -*-

;; Run inside a live Emacs session with full config loaded:
;;   M-x ert RET t RET
;; or filtered:
;;   (ert "^agent-shell-setup/")

(require 'ert)
(require 'test-helper)

;; The buffer-name-format lambda lives in tychoish-core's use-package agent-shell
;; :config block.  Reproduce it here so these tests are self-contained.
(defun agent-shell-setup-test--format (agent-name project-name)
  "Reference implementation of the tychoish buffer-name-format logic."
  (let* ((raw (string-trim project-name))
         (base (file-name-nondirectory (directory-file-name raw)))
         (stripped (replace-regexp-in-string "\\`[./]+" "" base))
         (slug (downcase (replace-regexp-in-string "\\s-+" "-"
                                                   (if (string-empty-p stripped) base stripped)))))
    (format "*%s-%s*"
            (car (split-string (downcase (string-trim agent-name))))
            slug)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-buffer-name-format lambda

(ert-deftest agent-shell-setup/buffer-name-format-basic ()
  (should (equal "*claude-my-project*"
                 (agent-shell-setup-test--format "Claude" "my-project"))))

(ert-deftest agent-shell-setup/buffer-name-format-uses-first-word-of-agent ()
  (should (equal "*claude-my-project*"
                 (agent-shell-setup-test--format "Claude Sonnet" "my-project"))))

(ert-deftest agent-shell-setup/buffer-name-format-lowercases-agent ()
  (should (equal "*claude-project*"
                 (agent-shell-setup-test--format "CLAUDE" "project"))))

(ert-deftest agent-shell-setup/buffer-name-format-extracts-basename-from-path ()
  (should (equal "*claude-my-project*"
                 (agent-shell-setup-test--format "Claude" "/home/user/my-project"))))

(ert-deftest agent-shell-setup/buffer-name-format-strips-trailing-slash ()
  (should (equal "*claude-my-project*"
                 (agent-shell-setup-test--format "Claude" "/home/user/my-project/"))))

(ert-deftest agent-shell-setup/buffer-name-format-slugifies-spaces ()
  (should (equal "*claude-my-project*"
                 (agent-shell-setup-test--format "Claude" "my project"))))

(ert-deftest agent-shell-setup/buffer-name-format-strips-leading-dots ()
  (should (equal "*claude-hidden*"
                 (agent-shell-setup-test--format "Claude" ".hidden"))))

(ert-deftest agent-shell-setup/buffer-name-format-trims-whitespace-from-agent ()
  (should (equal "*claude-project*"
                 (agent-shell-setup-test--format "  Claude  " "project"))))

(provide 'test-agent-shell-setup)
;;; test-agent-shell-setup.el ends here
