;;; mcpkit-ask.el --- Agent Shell Ask MCP service integration for mcpkit -*- lexical-binding: t; -*-

;; Author: tycho garen
;; Keywords: tools, mcp, agent-shell

;;; Commentary:
;;
;; Provides `mcpkit-register-ask-tools` to expose `agent-shell-ask`
;; human-in-the-loop question queue functionality as an MCP service via mcpkit.

;;; Code:

(require 'cl-lib)
(require 'mcpkit)

(declare-function agent-shell-ask-create "agent-shell-ask")
(declare-function agent-shell-ask-question-id "agent-shell-ask")
(declare-function agent-shell-ask-question-status "agent-shell-ask")
(declare-function agent-shell-ask-get "agent-shell-ask")
(declare-function agent-shell-ask-question-response "agent-shell-ask")
(declare-function agent-shell-ask-cursor-next "agent-shell-ask")
(declare-function agent-shell-ask-question-prompt "agent-shell-ask")
(declare-function agent-shell-ask-question-kind "agent-shell-ask")
(declare-function agent-shell-ask-question-options "agent-shell-ask")
(declare-function agent-shell-ask-list-pending "agent-shell-ask")
(declare-function agent-shell-ask-cancel "agent-shell-ask")

;;;###autoload
(defun mcpkit-register-ask-tools ()
  "Register `agent-shell-ask` tools on dedicated `agent-shell-ask` service."
  (when (fboundp 'mcpkit-register-tool)
    (let ((svc (or (mcpkit-get-service 'agent-shell-ask)
                   (mcpkit-define-service 'agent-shell-ask
                     :port 8766
                     :description "Human-in-the-Loop Question Queue Service"))))
      (mcpkit-register-tool ask_user svc
        :description "Ask human operator a question and wait for asynchronous response."
        :input-schema '(:type "object"
                        :properties (:prompt (:type "string" :description "Prompt text shown to user")
                                     :kind (:type "string" :description "Question type: single-choice, multi-choice, text, boolean, file")
                                     :options (:type "array" :items (:type "string") :description "List of choices")
                                     :target_shell (:type "string" :description "Target shell buffer name"))
                        :required ["prompt"])
        (lambda (args)
          (when (featurep 'agent-shell-ask)
            (let* ((prompt (plist-get args :prompt))
                   (kind-str (or (plist-get args :kind) "single-choice"))
                   (kind (intern kind-str))
                   (options (plist-get args :options))
                   (target (plist-get args :target_shell))
                   (q (agent-shell-ask-create :prompt prompt :kind kind :options options :target-shell target)))
              (list :question_id (agent-shell-ask-question-id q)
                    :status (symbol-name (agent-shell-ask-question-status q)))))))

      (mcpkit-register-tool poll_question svc
        :description "Poll the status and response of a question by ID."
        :input-schema '(:type "object"
                        :properties (:question_id (:type "string" :description "Question ID to poll"))
                        :required ["question_id"])
        (lambda (args)
          (when (featurep 'agent-shell-ask)
            (let* ((qid (plist-get args :question_id))
                   (q (agent-shell-ask-get qid)))
              (if (null q)
                  (list :error (format "Question %s not found" qid))
                (list :question_id qid
                      :status (symbol-name (agent-shell-ask-question-status q))
                      :response (agent-shell-ask-question-response q)))))))

      (mcpkit-register-tool get_next_question svc
        :description "Get next pending human question using cursor iteration."
        :input-schema '(:type "object"
                        :properties (:cursor_id (:type "string" :description "Cursor ID string")
                                     :target_shell (:type "string" :description "Target shell buffer filter")))
        (lambda (args)
          (when (featurep 'agent-shell-ask)
            (let* ((cid (plist-get args :cursor_id))
                   (target (plist-get args :target_shell))
                   (q (agent-shell-ask-cursor-next cid target)))
              (if (null q)
                  (list :status "none_pending")
                (list :question_id (agent-shell-ask-question-id q)
                      :prompt (agent-shell-ask-question-prompt q)
                      :kind (symbol-name (agent-shell-ask-question-kind q))
                      :options (agent-shell-ask-question-options q)
                      :status (symbol-name (agent-shell-ask-question-status q))))))))

      (mcpkit-register-tool list_pending_questions svc
        :description "List all pending questions."
        :input-schema '(:type "object" :properties ())
        (lambda (_args)
          (when (featurep 'agent-shell-ask)
            (mapcar (lambda (q)
                      (list :question_id (agent-shell-ask-question-id q)
                            :prompt (agent-shell-ask-question-prompt q)
                            :kind (symbol-name (agent-shell-ask-question-kind q))
                            :status (symbol-name (agent-shell-ask-question-status q))))
                    (agent-shell-ask-list-pending)))))

      (mcpkit-register-tool cancel_question svc
        :description "Cancel a pending question."
        :input-schema '(:type "object"
                        :properties (:question_id (:type "string" :description "Question ID to cancel"))
                        :required ["question_id"])
        (lambda (args)
          (when (featurep 'agent-shell-ask)
            (let* ((qid (plist-get args :question_id))
                   (q (agent-shell-ask-cancel qid "Cancelled via MCP")))
              (if q
                  (list :question_id qid :status "cancelled")
                (list :error (format "Question %s not found" qid))))))))))

(provide 'mcpkit-ask)
;;; mcpkit-ask.el ends here
