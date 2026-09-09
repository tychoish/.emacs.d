;;; mcpkit-ask-test.el --- ERT tests for mcpkit-ask.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((lisp-dir (expand-file-name "../lisp" (file-name-directory (or load-file-name buffer-file-name))))
      (ws-dir (expand-file-name "../elpa/web-server-20210708.2242" (file-name-directory (or load-file-name buffer-file-name)))))
  (unless (member lisp-dir load-path)
    (push lisp-dir load-path))
  (unless (member ws-dir load-path)
    (push ws-dir load-path)))

(require 'mcpkit)
(require 'mcpkit-ask)

(ert-deftest mcpkit-ask-test/register-tools ()
  "Test registering `agent-shell-ask` tools on `agent-shell-ask` service."
  (let ((mcpkit-registry nil))
    (mcpkit-register-ask-tools)
    (let ((svc (mcpkit-get-service 'agent-shell-ask)))
      (should (mcpkit-service-p svc))
      (should (eq (mcpkit-service-name svc) 'agent-shell-ask))
      (should (= (mcpkit-service-port svc) 8766))
      (let ((tools (mcpkit-service-tools svc)))
        (should (gethash "ask_user" tools))
        (should (gethash "poll_question" tools))
        (should (gethash "get_next_question" tools))
        (should (gethash "list_pending_questions" tools))
        (should (gethash "cancel_question" tools))))))

(provide 'mcpkit-ask-test)
;;; mcpkit-ask-test.el ends here
