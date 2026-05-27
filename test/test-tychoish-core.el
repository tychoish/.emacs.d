;;; test-tychoish-core.el --- ERT tests for tychoish-core.el -*- lexical-binding: t -*-

;; Run inside a live Emacs session with full config loaded:
;;   M-x ert RET t RET
;; or filtered:
;;   (ert "^tychoish-core/")

(require 'ert)
(require 'test-helper)

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
;;; agent-shell-menu :config integration
;;
;; These tests verify that agent-shell-mode-key wrappers are created by
;; the agent-shell-menu :config block, not the agent-shell :config block.
;; Before the fix, agent-shell :config called agent-shell-mode-key before
;; agent-shell-menu was loaded, making the macro void and aborting config.

(ert-deftest tychoish-core/agent-shell-mode-key-functions-defined ()
  "Verify agent-shell-mode-key wrapper functions were created by agent-shell-menu."
  (require 'agent-shell)
  (should (fboundp 'agent-shell-output-key-?))
  (should (fboundp 'agent-shell-output-key-p))
  (should (fboundp 'agent-shell-output-key-a))
  (should (fboundp 'agent-shell-output-key-b))
  (should (fboundp 'agent-shell-output-key-TAB))
  (should (fboundp 'agent-shell-output-key-q)))

(ert-deftest tychoish-core/agent-shell-header-style-configured ()
  "Verify agent-shell-header-style is set; confirms agent-shell :config ran to completion."
  (require 'agent-shell)
  (should (boundp 'agent-shell-header-style))
  (should (eq agent-shell-header-style 'text)))

(ert-deftest tychoish-core/agent-shell-buffer-name-format-is-function ()
  "Verify agent-shell-buffer-name-format is a callable; confirms agent-shell :config ran to completion."
  (require 'agent-shell)
  (should (boundp 'agent-shell-buffer-name-format))
  (should (functionp agent-shell-buffer-name-format)))

(provide 'test-tychoish-core)
;;; test-tychoish-core.el ends here
