;;; test-agent-shell-menu.el --- ERT tests for agent-shell-menu -*- lexical-binding: t -*-

;; Run inside a live Emacs session with full config loaded:
;;   M-x ert RET t RET
;; or filtered:
;;   (ert "^agent-shell-menu/")

(require 'ert)
(require 'cl-lib)
(require 'test-helper)
(require 'agent-shell-menu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell--block-category

(ert-deftest agent-shell-menu/block-category-thinking ()
  (should (equal "thinking"
                 (agent-shell--block-category "msg_abc123_agent_thought_chunk"))))

(ert-deftest agent-shell-menu/block-category-agent-message ()
  (should (equal "agent message"
                 (agent-shell--block-category "turn_1_agent_message_chunk"))))

(ert-deftest agent-shell-menu/block-category-user-message ()
  (should (equal "user message"
                 (agent-shell--block-category "turn_1_user_message_chunk"))))

(ert-deftest agent-shell-menu/block-category-plan ()
  (should (equal "plan"
                 (agent-shell--block-category "session-plan"))))

(ert-deftest agent-shell-menu/block-category-plan-any-prefix ()
  (should (equal "plan"
                 (agent-shell--block-category "some-other-thing-plan"))))

(ert-deftest agent-shell-menu/block-category-session-info ()
  (should (equal "session info"
                 (agent-shell--block-category "bootstrapping-intro"))))

(ert-deftest agent-shell-menu/block-category-tool-call-default ()
  (should (equal "tool call"
                 (agent-shell--block-category "tool_use_xyz"))))

(ert-deftest agent-shell-menu/block-category-tool-call-unknown-id ()
  (should (equal "tool call"
                 (agent-shell--block-category "some-random-id-123"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell--permission-buttons

(ert-deftest agent-shell-menu/permission-buttons-empty-buffer ()
  (with-temp-buffer
    (should (null (agent-shell--permission-buttons)))))

(ert-deftest agent-shell-menu/permission-buttons-no-permission-property ()
  (with-temp-buffer
    (insert "some text without button property")
    (should (null (agent-shell--permission-buttons)))))

(ert-deftest agent-shell-menu/permission-buttons-finds-single-button ()
  (with-temp-buffer
    (insert "[ Allow ]")
    (put-text-property 1 10 'button 'permission)
    (let ((buttons (agent-shell--permission-buttons)))
      (should (= 1 (length buttons)))
      (should (equal "Allow" (caar buttons))))))

(ert-deftest agent-shell-menu/permission-buttons-trims-brackets-and-whitespace ()
  (with-temp-buffer
    (insert "[  Deny  ]")
    (put-text-property 1 11 'button 'permission)
    (let ((buttons (agent-shell--permission-buttons)))
      (should (equal "Deny" (caar buttons))))))

(ert-deftest agent-shell-menu/permission-buttons-returns-position ()
  (with-temp-buffer
    (insert "[ Allow ]")
    (put-text-property 1 10 'button 'permission)
    (let ((buttons (agent-shell--permission-buttons)))
      (should (= 1 (cdar buttons))))))

(ert-deftest agent-shell-menu/permission-buttons-finds-multiple-buttons ()
  (with-temp-buffer
    (insert "[ Allow ]\n[ Deny ]")
    (put-text-property 1 10 'button 'permission)
    (put-text-property 11 19 'button 'permission)
    (let ((buttons (agent-shell--permission-buttons)))
      (should (= 2 (length buttons)))
      (should (equal "Allow" (caar buttons)))
      (should (equal "Deny" (caadr buttons))))))

(ert-deftest agent-shell-menu/permission-buttons-ignores-non-permission-button ()
  (with-temp-buffer
    (insert "[ Other ]")
    (put-text-property 1 10 'button 'other-value)
    (should (null (agent-shell--permission-buttons)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell--permission-button-action

(ert-deftest agent-shell-menu/permission-button-action-invokes-command ()
  "The lambda returned by permission-button-action calls the button's RET command."
  (let* ((invoked nil)
         (cmd (lambda () (interactive) (setq invoked t))))
    (with-temp-buffer
      (insert "x")
      (put-text-property 1 2 'keymap
                         (let ((m (make-sparse-keymap)))
                           (define-key m (kbd "RET") cmd)
                           m))
      (let ((action (agent-shell--permission-button-action 1)))
        (call-interactively action)))
    (should invoked)))

(ert-deftest agent-shell-menu/permission-button-action-ignores-non-command ()
  "Regression: when lookup-key returns a non-commandp value (e.g. \"\"), the
lambda must silently do nothing instead of signalling
Wrong type argument: commandp, \"\"."
  (with-temp-buffer
    (insert "x")
    (put-text-property 1 2 'keymap
                       (let ((m (make-sparse-keymap)))
                         (define-key m (kbd "RET") "")
                         m))
    (let ((action (agent-shell--permission-button-action 1)))
      (should-not (condition-case err
                      (progn (call-interactively action) nil)
                    (error err))))))

(ert-deftest agent-shell-menu/resolve-permission-uses-buttons-for-lookup ()
  "Regression: agent-shell-resolve-permission must bind buttons before using it
in assoc — previously buttons was a free variable causing pos to always be nil."
  (let (invoked-pos)
    (with-temp-buffer
      (insert (make-string 50 ?x))
      (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) t))
                ((symbol-function 'agent-shell--permission-pending-p) (lambda () t))
                ((symbol-function 'agent-shell--permission-buttons)
                 (lambda () (list (cons "Allow" 1))))
                ((symbol-function 'annotated-completing-read)
                 (lambda (_table &rest _) "Allow"))
                ((symbol-function 'agent-shell--permission-action-at)
                 (lambda (pos) (setq invoked-pos pos) #'ignore))
                ((symbol-function 'call-interactively) #'ignore))
        (agent-shell-resolve-permission)))
    (should (equal 1 invoked-pos))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-select-action

(ert-deftest agent-shell-menu/select-action-calls-command-not-docstring ()
  "Regression: selecting a permission option must invoke the command, not the
doc-string.  The old pipeline converted (label . cmd) to (label . doc-string)
before the assoc lookup, so `call-interactively' received \"\" and signalled
Wrong type argument: commandp, \"\"."
  (let* ((invoked nil)
	 (action (lambda () (interactive) (setq invoked t)))
	 (captured-table nil))
    (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) t))
	      ((symbol-function 'agent-shell--permission-pending-p) (lambda () t))
	      ((symbol-function 'agent-shell--permission-buttons)
	       (lambda () (list (cons "Allow" 1))))
	      ((symbol-function 'agent-shell--permission-button-action)
	       (lambda (_pos) action))
	      ((symbol-function 'annotated-completing-read)
	       (lambda (table &rest _)
		 (setq captured-table table)
		 "permission: Allow")))
      (agent-shell-select-action))
    (should invoked)
    (should (equal "" (cdr (assoc "permission: Allow" captured-table))))))

(ert-deftest agent-shell-menu/select-action-permission-entries-before-actions ()
  "Permission entries appear before regular action entries in the display table."
  (let (captured-table)
    (cl-letf (((symbol-function 'derived-mode-p) (lambda (&rest _) t))
	      ((symbol-function 'agent-shell--permission-pending-p) (lambda () t))
	      ((symbol-function 'agent-shell--permission-buttons)
	       (lambda () (list (cons "Allow" 1))))
	      ((symbol-function 'agent-shell--permission-button-action)
	       (lambda (_pos) (lambda () (interactive))))
	      ((symbol-function 'annotated-completing-read)
	       (lambda (table &rest _)
		 (setq captured-table table)
		 (caar table))))
      (agent-shell-select-action))
    (should (string-prefix-p "permission:" (caar captured-table)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-extras--pick-buffer

(ert-deftest agent-shell-menu/pick-buffer-passes-alist-not-buffer-list ()
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

(ert-deftest agent-shell-menu/pick-buffer-errors-when-no-buffers ()
  (cl-letf (((symbol-function 'agent-shell-buffers) (lambda () nil)))
    (should-error (agent-shell-extras--pick-buffer "test: ") :type 'user-error)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; agent-shell-extras--same-project-buffers

(ert-deftest agent-shell-menu/same-project-buffers-empty-when-no-agent-buffers ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'agent-shell-buffers) (lambda () nil)))
      (should (null (agent-shell-extras--same-project-buffers))))))

(ert-deftest agent-shell-menu/same-project-buffers-excludes-current-buffer ()
  (with-temp-buffer
    (let ((cur (current-buffer)))
      (cl-letf (((symbol-function 'agent-shell-buffers) (lambda () (list cur))))
        (should (null (agent-shell-extras--same-project-buffers)))))))

(ert-deftest agent-shell-menu/same-project-buffers-includes-same-dir-buffer ()
  (let ((other (generate-new-buffer "*mock-agent*")))
    (unwind-protect
        (with-temp-buffer
          (setq-local default-directory "/tmp/test-proj/")
          (with-current-buffer other
            (setq-local default-directory "/tmp/test-proj/"))
          (cl-letf (((symbol-function 'agent-shell-buffers) (lambda () (list other))))
            (should (memq other (agent-shell-extras--same-project-buffers)))))
      (kill-buffer other))))

(ert-deftest agent-shell-menu/same-project-buffers-excludes-different-dir-buffer ()
  (let ((other (generate-new-buffer "*mock-agent*")))
    (unwind-protect
        (with-temp-buffer
          (setq-local default-directory "/tmp/proj-a/")
          (with-current-buffer other
            (setq-local default-directory "/tmp/proj-b/"))
          (cl-letf (((symbol-function 'agent-shell-buffers) (lambda () (list other))))
            (should (null (agent-shell-extras--same-project-buffers)))))
      (kill-buffer other))))

(ert-deftest agent-shell-menu/same-project-buffers-returns-only-matching ()
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
;;; agent-shell--session-permission-suffixes

(ert-deftest agent-shell-menu/session-permission-suffixes-empty-when-no-pending ()
  "No suffixes generated when no permission is pending."
  (cl-letf (((symbol-function 'agent-shell--session-shell-buffer) (lambda () nil)))
    (should (null (agent-shell--session-permission-suffixes nil)))))

(ert-deftest agent-shell-menu/session-permission-suffixes-one-per-button ()
  "One transient suffix is produced per pending permission button."
  (let ((shell (generate-new-buffer " *mock-shell*")))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell--session-shell-buffer)
                   (lambda () shell))
                  ((symbol-function 'agent-shell--permission-buttons)
                   (lambda () (list (cons "Allow" 10) (cons "Deny" 20))))
                  ((symbol-function 'agent-shell--permission-action-at)
                   (lambda (_pos) #'ignore)))
          (let ((suffixes (agent-shell--session-permission-suffixes nil)))
            (should (= 2 (length suffixes)))
            (should (equal "1" (transient--suffix-key (nth 0 suffixes))))
            (should (equal "2" (transient--suffix-key (nth 1 suffixes))))))
      (kill-buffer shell))))

(ert-deftest agent-shell-menu/session-permission-suffixes-labels-include-button-text ()
  "Each suffix description includes the permission button label."
  (let ((shell (generate-new-buffer " *mock-shell-2*")))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell--session-shell-buffer)
                   (lambda () shell))
                  ((symbol-function 'agent-shell--permission-buttons)
                   (lambda () (list (cons "Allow for session" 10))))
                  ((symbol-function 'agent-shell--permission-action-at)
                   (lambda (_pos) #'ignore)))
          (let* ((suffix (car (agent-shell--session-permission-suffixes nil)))
                 (desc (plist-get (cdr suffix) :description)))
            (should (string-match-p "Allow for session" desc))))
      (kill-buffer shell))))

(ert-deftest agent-shell-menu/session-permission-action-runs-in-shell-buffer ()
  "The action lambda activates the button in the shell buffer regardless of
which buffer is current when the action is invoked."
  (let* ((shell (generate-new-buffer " *mock-shell-3*"))
         (invoked-in nil))
    (unwind-protect
        (progn
          (with-current-buffer shell
            (insert "x")
            (put-text-property 1 2
                               'keymap
                               (let ((m (make-sparse-keymap)))
                                 (define-key m (kbd "RET")
                                   (lambda () (interactive)
                                     (setq invoked-in (current-buffer))))
                                 m)))
          (let ((action (agent-shell--session-permission-button-action shell 1)))
            (with-temp-buffer
              (call-interactively action)))
          (should (eq shell invoked-in)))
      (kill-buffer shell))))

(ert-deftest agent-shell-menu/session-permission-pending-p-uses-shell-buffer ()
  "The predicate checks the shell buffer, not the current buffer."
  (let ((shell (generate-new-buffer " *mock-shell-4*")))
    (unwind-protect
        (cl-letf (((symbol-function 'agent-shell--session-shell-buffer)
                   (lambda () shell))
                  ((symbol-function 'agent-shell--permission-pending-p)
                   (lambda (&rest _) t)))
          (should (agent-shell--session-permission-pending-p)))
      (kill-buffer shell))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transient menu key integrity

(ert-deftest agent-shell-menu/global-menu-no-key-prefix-conflicts ()
  "No key in agent-shell-global-menu is a strict prefix of another key."
  (let* ((keys (transient-test/collect-keys 'agent-shell-global-menu))
         (conflicts (transient-test/key-prefix-conflicts keys)))
    (should (null conflicts))))

(ert-deftest agent-shell-menu/global-menu-no-duplicate-keys ()
  "No key appears more than once in agent-shell-global-menu."
  (let* ((keys (transient-test/collect-keys 'agent-shell-global-menu))
         (dups (transient-test/duplicate-keys keys)))
    (should (null dups))))

(ert-deftest agent-shell-menu/session-menu-no-key-prefix-conflicts ()
  "No key in agent-shell-session-menu is a strict prefix of another key."
  (let* ((keys (transient-test/collect-keys 'agent-shell-session-menu))
         (conflicts (transient-test/key-prefix-conflicts keys)))
    (should (null conflicts))))

(ert-deftest agent-shell-menu/session-menu-no-duplicate-keys ()
  "No key appears more than once in agent-shell-session-menu."
  (let* ((keys (transient-test/collect-keys 'agent-shell-session-menu))
         (dups (transient-test/duplicate-keys keys)))
    (should (null dups))))

(provide 'test-agent-shell-menu)
;;; test-agent-shell-menu.el ends here
