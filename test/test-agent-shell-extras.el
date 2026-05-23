;;; test-agent-shell-extras.el --- ERT tests for agent-shell-extras -*- lexical-binding: t -*-

;; Run inside a live Emacs session with the full config loaded:
;;   M-x ert RET t RET
;; or filtered:
;;   (ert "^agent-shell-extras/")
;;
;; Batch run:
;;   emacs --batch -L ~/.emacs.d/lisp \
;;     --eval '(progn (setq package-user-dir "~/.emacs.d/elpa") (package-initialize))' \
;;     -l ~/.emacs.d/test/test-helper.el \
;;     -l ~/.emacs.d/test/test-agent-shell-extras.el \
;;     --eval '(ert-run-tests-batch-and-exit "agent-shell-extras/")'

(require 'ert)
(require 'cl-lib)
(require 'test-helper)
(require 'agent-shell-extras)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-buffer-name-format lambda

(ert-deftest agent-shell-extras/buffer-name-format-basic ()
  (should (equal "*claude-my-project*"
                 (funcall agent-shell-buffer-name-format "Claude" "my-project"))))

(ert-deftest agent-shell-extras/buffer-name-format-uses-first-word-of-agent ()
  (should (equal "*claude-my-project*"
                 (funcall agent-shell-buffer-name-format "Claude Sonnet" "my-project"))))

(ert-deftest agent-shell-extras/buffer-name-format-lowercases-agent ()
  (should (equal "*claude-project*"
                 (funcall agent-shell-buffer-name-format "CLAUDE" "project"))))

(ert-deftest agent-shell-extras/buffer-name-format-extracts-basename-from-path ()
  (should (equal "*claude-my-project*"
                 (funcall agent-shell-buffer-name-format "Claude" "/home/user/my-project"))))

(ert-deftest agent-shell-extras/buffer-name-format-strips-trailing-slash ()
  (should (equal "*claude-my-project*"
                 (funcall agent-shell-buffer-name-format "Claude" "/home/user/my-project/"))))

(ert-deftest agent-shell-extras/buffer-name-format-slugifies-spaces ()
  (should (equal "*claude-my-project*"
                 (funcall agent-shell-buffer-name-format "Claude" "my project"))))

(ert-deftest agent-shell-extras/buffer-name-format-strips-leading-dots ()
  (should (equal "*claude-hidden*"
                 (funcall agent-shell-buffer-name-format "Claude" ".hidden"))))

(ert-deftest agent-shell-extras/buffer-name-format-trims-whitespace-from-agent ()
  (should (equal "*claude-project*"
                 (funcall agent-shell-buffer-name-format "  Claude  " "project"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell--format-age

(ert-deftest agent-shell-extras/format-age-seconds ()
  (should (equal "30s" (agent-shell--format-age (seconds-to-time 30)))))

(ert-deftest agent-shell-extras/format-age-one-second ()
  (should (equal "1s" (agent-shell--format-age (seconds-to-time 1)))))

(ert-deftest agent-shell-extras/format-age-boundary-59s ()
  (should (equal "59s" (agent-shell--format-age (seconds-to-time 59)))))

(ert-deftest agent-shell-extras/format-age-minutes ()
  (should (equal "5m" (agent-shell--format-age (seconds-to-time 300)))))

(ert-deftest agent-shell-extras/format-age-boundary-60s-is-one-minute ()
  (should (equal "1m" (agent-shell--format-age (seconds-to-time 60)))))

(ert-deftest agent-shell-extras/format-age-boundary-3599s-is-59m ()
  (should (equal "59m" (agent-shell--format-age (seconds-to-time 3599)))))

(ert-deftest agent-shell-extras/format-age-hours ()
  (should (equal "2h" (agent-shell--format-age (seconds-to-time 7200)))))

(ert-deftest agent-shell-extras/format-age-boundary-3600s-is-one-hour ()
  (should (equal "1h" (agent-shell--format-age (seconds-to-time 3600)))))

(ert-deftest agent-shell-extras/format-age-days ()
  (should (equal "3d" (agent-shell--format-age (seconds-to-time (* 3 86400))))))

(ert-deftest agent-shell-extras/format-age-boundary-86400s-is-one-day ()
  (should (equal "1d" (agent-shell--format-age (seconds-to-time 86400)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell--block-category

(ert-deftest agent-shell-extras/block-category-thinking ()
  (should (equal "thinking"
                 (agent-shell--block-category "msg_abc123_agent_thought_chunk"))))

(ert-deftest agent-shell-extras/block-category-agent-message ()
  (should (equal "agent message"
                 (agent-shell--block-category "turn_1_agent_message_chunk"))))

(ert-deftest agent-shell-extras/block-category-user-message ()
  (should (equal "user message"
                 (agent-shell--block-category "turn_1_user_message_chunk"))))

(ert-deftest agent-shell-extras/block-category-plan ()
  (should (equal "plan"
                 (agent-shell--block-category "session-plan"))))

(ert-deftest agent-shell-extras/block-category-plan-any-prefix ()
  (should (equal "plan"
                 (agent-shell--block-category "some-other-thing-plan"))))

(ert-deftest agent-shell-extras/block-category-session-info ()
  (should (equal "session info"
                 (agent-shell--block-category "bootstrapping-intro"))))

(ert-deftest agent-shell-extras/block-category-tool-call-default ()
  (should (equal "tool call"
                 (agent-shell--block-category "tool_use_xyz"))))

(ert-deftest agent-shell-extras/block-category-tool-call-unknown-id ()
  (should (equal "tool call"
                 (agent-shell--block-category "some-random-id-123"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell--permission-buttons

(ert-deftest agent-shell-extras/permission-buttons-empty-buffer ()
  (with-temp-buffer
    (should (null (agent-shell--permission-buttons)))))

(ert-deftest agent-shell-extras/permission-buttons-no-permission-property ()
  (with-temp-buffer
    (insert "some text without button property")
    (should (null (agent-shell--permission-buttons)))))

(ert-deftest agent-shell-extras/permission-buttons-finds-single-button ()
  (with-temp-buffer
    (insert "[ Allow ]")
    (put-text-property 1 10 'button 'permission)
    (let ((buttons (agent-shell--permission-buttons)))
      (should (= 1 (length buttons)))
      (should (equal "Allow" (caar buttons))))))

(ert-deftest agent-shell-extras/permission-buttons-trims-brackets-and-whitespace ()
  (with-temp-buffer
    (insert "[  Deny  ]")
    (put-text-property 1 11 'button 'permission)
    (let ((buttons (agent-shell--permission-buttons)))
      (should (equal "Deny" (caar buttons))))))

(ert-deftest agent-shell-extras/permission-buttons-returns-position ()
  (with-temp-buffer
    (insert "[ Allow ]")
    (put-text-property 1 10 'button 'permission)
    (let ((buttons (agent-shell--permission-buttons)))
      (should (= 1 (cdar buttons))))))

(ert-deftest agent-shell-extras/permission-buttons-finds-multiple-buttons ()
  (with-temp-buffer
    (insert "[ Allow ]\n[ Deny ]")
    (put-text-property 1 10 'button 'permission)
    (put-text-property 11 19 'button 'permission)
    (let ((buttons (agent-shell--permission-buttons)))
      (should (= 2 (length buttons)))
      (should (equal "Allow" (caar buttons)))
      (should (equal "Deny" (caadr buttons))))))

(ert-deftest agent-shell-extras/permission-buttons-ignores-non-permission-button ()
  (with-temp-buffer
    (insert "[ Other ]")
    (put-text-property 1 10 'button 'other-value)
    (should (null (agent-shell--permission-buttons)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-extras--pick-buffer

(ert-deftest agent-shell-extras/pick-buffer-passes-alist-not-buffer-list ()
  "Regression: passing (agent-shell-buffers) raw to annotated-completing-read
produced 'Each alist entry must be a cons cell; got: #<buffer ...>'."
  (let* ((mock-buf (generate-new-buffer "*mock-agent-pick*"))
         (captured nil))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell-buffers) (lambda () (list mock-buf)))
                  ((symbol-function 'agent-shell--buffer-annotation) (lambda (_) "ann"))
                  ((symbol-function 'annotated-completing-read)
                   (lambda (table &rest _)
                     (setq captured table)
                     (buffer-name mock-buf))))
          (agent-shell-extras--pick-buffer "test: ")
          (should (listp captured))
          (should (= 1 (length captured)))
          (should (consp (car captured)))
          (should (equal (buffer-name mock-buf) (caar captured)))
          (should (equal "ann" (cdar captured))))
      (kill-buffer mock-buf))))

(ert-deftest agent-shell-extras/pick-buffer-errors-when-no-buffers ()
  (cl-letf (((symbol-function 'agent-shell-buffers) (lambda () nil)))
    (should-error (agent-shell-extras--pick-buffer "test: ") :type 'user-error)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-extras--same-project-buffers

(ert-deftest agent-shell-extras/same-project-buffers-empty-when-no-agent-buffers ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'agent-shell-buffers) (lambda () nil)))
      (should (null (agent-shell-extras--same-project-buffers))))))

(ert-deftest agent-shell-extras/same-project-buffers-excludes-current-buffer ()
  (with-temp-buffer
    (let ((cur (current-buffer)))
      (cl-letf (((symbol-function 'agent-shell-buffers) (lambda () (list cur))))
        (should (null (agent-shell-extras--same-project-buffers)))))))

(ert-deftest agent-shell-extras/same-project-buffers-includes-same-dir-buffer ()
  (let ((other (generate-new-buffer "*mock-agent*")))
    (unwind-protect
        (with-temp-buffer
          (setq-local default-directory "/tmp/test-proj/")
          (with-current-buffer other
            (setq-local default-directory "/tmp/test-proj/"))
          (cl-letf (((symbol-function 'agent-shell-buffers) (lambda () (list other))))
            (should (memq other (agent-shell-extras--same-project-buffers)))))
      (kill-buffer other))))

(ert-deftest agent-shell-extras/same-project-buffers-excludes-different-dir-buffer ()
  (let ((other (generate-new-buffer "*mock-agent*")))
    (unwind-protect
        (with-temp-buffer
          (setq-local default-directory "/tmp/proj-a/")
          (with-current-buffer other
            (setq-local default-directory "/tmp/proj-b/"))
          (cl-letf (((symbol-function 'agent-shell-buffers) (lambda () (list other))))
            (should (null (agent-shell-extras--same-project-buffers)))))
      (kill-buffer other))))

(ert-deftest agent-shell-extras/same-project-buffers-returns-only-matching ()
  (let ((match (generate-new-buffer "*mock-match*"))
        (other (generate-new-buffer "*mock-other*")))
    (unwind-protect
        (with-temp-buffer
          (setq-local default-directory "/tmp/my-proj/")
          (with-current-buffer match
            (setq-local default-directory "/tmp/my-proj/"))
          (with-current-buffer other
            (setq-local default-directory "/tmp/different/"))
          (cl-letf (((symbol-function 'agent-shell-buffers)
                     (lambda () (list match other))))
            (let ((result (agent-shell-extras--same-project-buffers)))
              (should (memq match result))
              (should-not (memq other result)))))
      (kill-buffer match)
      (kill-buffer other))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transient menu key integrity

(ert-deftest agent-shell-extras/global-menu-no-key-prefix-conflicts ()
  "No key in agent-shell-global-menu is a strict prefix of another key."
  (let* ((keys (transient-test/collect-keys 'agent-shell-global-menu))
         (conflicts (transient-test/key-prefix-conflicts keys)))
    (should (null conflicts))))

(ert-deftest agent-shell-extras/global-menu-no-duplicate-keys ()
  "No key appears more than once in agent-shell-global-menu."
  (let* ((keys (transient-test/collect-keys 'agent-shell-global-menu))
         (dups (transient-test/duplicate-keys keys)))
    (should (null dups))))

(ert-deftest agent-shell-extras/session-menu-no-key-prefix-conflicts ()
  "No key in agent-shell-session-menu is a strict prefix of another key."
  (let* ((keys (transient-test/collect-keys 'agent-shell-session-menu))
         (conflicts (transient-test/key-prefix-conflicts keys)))
    (should (null conflicts))))

(ert-deftest agent-shell-extras/session-menu-no-duplicate-keys ()
  "No key appears more than once in agent-shell-session-menu."
  (let* ((keys (transient-test/collect-keys 'agent-shell-session-menu))
         (dups (transient-test/duplicate-keys keys)))
    (should (null dups))))

(provide 'test-agent-shell-extras)
;;; test-agent-shell-extras.el ends here
