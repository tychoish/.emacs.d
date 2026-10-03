;;; asq-mcp.el --- Agent Shell Queue (asq) MCP service integration for mcpkit -*- lexical-binding: t; -*-

;; Author: Tychoish
;; Keywords: tools, mcp, agent-shell, queue

;;; Commentary:
;;
;; Provides Model Context Protocol (MCP) tools for interacting with the
;; Agent Shell Queue (asq) via mcpkit.
;;
;; Exposes queue inspection, item fetch, enqueue, reenqueue, interject,
;; and status mutations (cancel, defer, finish).
;;
;; Tools are registered at top-level on the `asq' service upon loading.
;; Services are started only when explicitly requested via `mcpkit-start-service'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'mcpkit)
(require 'agent-shell-queue nil t)

(defgroup asq-mcp nil
  "Agent Shell Queue (asq) MCP service integration."
  :group 'mcpkit
  :prefix "asq-mcp-")

(defcustom asq-mcp-port 8765
  "Default TCP port for the asq MCP service."
  :type 'integer
  :group 'asq-mcp)

;;; Service Definition & Top-Level Tool Registration

(defvar asq-mcp-service
  (or (mcpkit-get-service 'asq)
      (mcpkit-define-service 'asq
        :port asq-mcp-port
        :description "Agent Shell Queue (asq) Management and Inspection"))
  "The asq `mcpkit-service' instance.")

(defun asq-mcp--ensure-queue ()
  "Ensure `agent-shell-queue' is loaded."
  (unless (fboundp 'agent-shell-queue-get-item-by-id)
    (require 'agent-shell-queue))
  (when (fboundp 'agent-shell-queue--ensure-loaded)
    (agent-shell-queue--ensure-loaded)))

;; 1. asq_list_items
(mcpkit-register-tool 'asq_list_items 'asq
  :description "List queue items across all buffers or for a specific buffer, optionally filtered by status."
  :input-schema '(:type "object"
                  :properties (:buffer (:type "string" :description "Optional buffer name filter")
                               :status (:type "string" :description "Optional status filter (e.g. active, running, done, blocked.skip, blocked.task, aborted)"))
                  :required [])
  (asq-mcp--ensure-queue)
  (let* ((target-buf (plist-get args :buffer))
         (status-str (plist-get args :status))
         (status-sym (when (and status-str (not (string-empty-p status-str)))
                       (intern status-str)))
         (results nil))
    (seq-do (lambda (bucket)
              (let ((buf-name (car bucket)))
                (when (or (null target-buf) (equal target-buf buf-name))
                  (seq-do (lambda (item)
                            (let ((item-status (agent-shell-queue-item-status item)))
                              (when (or (null status-sym) (eq item-status status-sym))
                                (push (list :id (agent-shell-queue-item-id item)
                                            :buffer buf-name
                                            :status (symbol-name item-status)
                                            :kind (symbol-name (or (agent-shell-queue-item-kind item) 'prompt))
                                            :prompt (agent-shell-queue-item-args item)
                                            :background (if (agent-shell-queue-item-background item) t :json-false)
                                            :reenqueued_from (or (agent-shell-queue-item-reenqueued-from item) "")
                                            :has_children (if (agent-shell-queue-item-reenqueued-as item) t :json-false))
                                      results))))
                          (cdr bucket)))))
            (agent-shell-queue-store-items agent-shell-queue--store))
    (list :count (length results)
          :items (vconcat (nreverse results)))))

;; 2. asq_get_item
(mcpkit-register-tool 'asq_get_item 'asq
  :description "Retrieve details of a specific queue item by ID, including prompt, response, lineage, and interjection data."
  :input-schema '(:type "object"
                  :properties (:id (:type "string" :description "Queue item identifier"))
                  :required ["id"])
  (asq-mcp--ensure-queue)
  (let* ((id (plist-get args :id))
         (pair (agent-shell-queue-get-item-by-id id)))
    (if (not pair)
        (list :found :json-false :id id :error (format "No item found with id %s" id))
      (let* ((buf-name (car pair))
             (item (cdr pair)))
        (list :found t
              :id id
              :buffer buf-name
              :status (symbol-name (agent-shell-queue-item-status item))
              :kind (symbol-name (or (agent-shell-queue-item-kind item) 'prompt))
              :background (if (agent-shell-queue-item-background item) t :json-false)
              :prompt (or (agent-shell-queue-item-args item) "")
              :response (or (agent-shell-queue-item-response item) "")
              :reenqueued_from (or (agent-shell-queue-item-reenqueued-from item) "")
              :reenqueued_as (vconcat (or (agent-shell-queue-item-reenqueued-as item) []))
              :interjection_prompt (or (agent-shell-queue-item-interjection-prompt item) "")
              :interjection_result (or (agent-shell-queue-item-interjection-result item) ""))))))

;; 3. asq_enqueue
(mcpkit-register-tool 'asq_enqueue 'asq
  :description "Enqueue a prompt in an agent-shell buffer, optionally linking as a follow-up to an existing item."
  :input-schema '(:type "object"
                  :properties (:prompt (:type "string" :description "Task prompt text to execute")
                               :buffer (:type "string" :description "Target agent-shell buffer name")
                               :from_id (:type "string" :description "Optional parent item ID this follows up on")
                               :background (:type "boolean" :description "Run in background when non-nil"))
                  :required ["prompt" "buffer"])
  (asq-mcp--ensure-queue)
  (let* ((prompt (plist-get args :prompt))
         (buf-name (plist-get args :buffer))
         (from-id (plist-get args :from_id))
         (background (plist-get args :background))
         (buf (or (get-buffer buf-name)
                  (user-error "No buffer named %s" buf-name)))
         (item (agent-shell-queue-add prompt buf))
         (new-id (agent-shell-queue-item-id item)))
    (when background
      (setf (agent-shell-queue-item-background item) t))
    (when (and from-id (not (string-empty-p from-id)))
      (setf (agent-shell-queue-item-reenqueued-from item) from-id)
      (when-let* ((old-pair (agent-shell-queue-get-item-by-id from-id))
                  (old-item (cdr old-pair)))
        (setf (agent-shell-queue-item-reenqueued-as old-item)
              (append (agent-shell-queue-item-reenqueued-as old-item) (list new-id)))))
    (agent-shell-queue--save)
    (list :status "ok"
          :id new-id
          :buffer buf-name
          :from_id (or from-id ""))))

;; 4. asq_reenqueue
(mcpkit-register-tool 'asq_reenqueue 'asq
  :description "Re-enqueue a done or aborted item as a new active item in the same buffer."
  :input-schema '(:type "object"
                  :properties (:id (:type "string" :description "ID of the completed or aborted item to re-enqueue"))
                  :required ["id"])
  (asq-mcp--ensure-queue)
  (let* ((id (plist-get args :id))
         (pair (agent-shell-queue-get-item-by-id id)))
    (if (not pair)
        (list :status "error" :error (format "No item found with id %s" id))
      (condition-case err
          (let ((new-id (agent-shell-queue-reenqueue id)))
            (list :status "ok" :original_id id :new_id new-id))
        (error (list :status "error" :error (error-message-string err)))))))

;; 5. asq_interject
(mcpkit-register-tool 'asq_interject 'asq
  :description "Interrupt the currently running queue task or specified item and inject an interjection prompt."
  :input-schema '(:type "object"
                  :properties (:prompt (:type "string" :description "Interjection guidance or instruction")
                               :id (:type "string" :description "Optional running item ID (defaults to currently running item)"))
                  :required ["prompt"])
  (asq-mcp--ensure-queue)
  (let* ((prompt (plist-get args :prompt))
         (id (plist-get args :id))
         (running-pair (if (and id (not (string-empty-p id)))
                           (agent-shell-queue-get-item-by-id id)
                         (let ((found nil))
                           (catch 'done
                             (seq-do (lambda (bucket)
                                       (seq-do (lambda (item)
                                                 (when (memq (agent-shell-queue-item-status item) '(running interjecting))
                                                   (setq found (cons (car bucket) item))
                                                   (throw 'done nil)))
                                               (cdr bucket)))
                                     (agent-shell-queue-store-items agent-shell-queue--store)))
                           found))))
    (if (not running-pair)
        (list :status "error" :error "No running queue item to interject")
      (let* ((buf-name (car running-pair))
             (item (cdr running-pair)))
        (setf (agent-shell-queue-item-interjection-prompt item) prompt)
        (when-let* ((buf (get-buffer buf-name)))
          (with-current-buffer buf
            (when (fboundp 'agent-shell-interrupt)
              (ignore-errors (agent-shell-interrupt)))
            (when (fboundp 'comint-send-input)
              (insert prompt)
              (comint-send-input))))
        (agent-shell-queue--save)
        (list :status "ok"
              :id (agent-shell-queue-item-id item)
              :buffer buf-name
              :interjected_prompt prompt)))))

;; 6. asq_cancel
(mcpkit-register-tool 'asq_cancel 'asq
  :description "Cancel an item by ID: aborts if running, or removes from queue if pending."
  :input-schema '(:type "object"
                  :properties (:id (:type "string" :description "ID of item to cancel or abort"))
                  :required ["id"])
  (asq-mcp--ensure-queue)
  (let* ((id (plist-get args :id))
         (pair (agent-shell-queue-get-item-by-id id)))
    (if (not pair)
        (list :status "error" :error (format "No item found with id %s" id))
      (let* ((buf-name (car pair))
             (item (cdr pair))
             (status (agent-shell-queue-item-status item)))
        (if (eq status 'running)
            (progn
              (when-let* ((buf (get-buffer buf-name))
                          (_ (buffer-live-p buf)))
                (with-current-buffer buf
                  (when (fboundp 'agent-shell-interrupt)
                    (agent-shell-interrupt))))
              (setf (agent-shell-queue-item-status item) 'aborted)
              (setf (agent-shell-queue-item-outcome item) 'canceled)
              (agent-shell-queue--insert-resume-task buf-name item)
              (agent-shell-queue--pause-and-save buf-name)
              (list :status "ok" :id id :action "aborted" :buffer buf-name))
          (agent-shell-queue-remove id)
          (list :status "ok" :id id :action "removed" :buffer buf-name))))))

;; 7. asq_defer
(mcpkit-register-tool 'asq_defer 'asq
  :description "Toggle queue item between active and blocked/skip."
  :input-schema '(:type "object"
                  :properties (:id (:type "string" :description "Queue item identifier"))
                  :required ["id"])
  (asq-mcp--ensure-queue)
  (let* ((id (plist-get args :id))
         (pair (agent-shell-queue-get-item-by-id id)))
    (if (not pair)
        (list :status "error" :error (format "No item found with id %s" id))
      (progn
        (agent-shell-queue-defer id)
        (let* ((updated (agent-shell-queue-get-item-by-id id))
               (new-status (when updated (agent-shell-queue-item-status (cdr updated)))))
          (list :status "ok" :id id :new_status (symbol-name (or new-status 'unknown))))))))

;; 8. asq_finish
(mcpkit-register-tool 'asq_finish 'asq
  :description "Mark queue item as done without dispatching through LLM, optionally recording response text."
  :input-schema '(:type "object"
                  :properties (:id (:type "string" :description "Queue item identifier")
                               :response (:type "string" :description "Optional final response or resolution text"))
                  :required ["id"])
  (asq-mcp--ensure-queue)
  (let* ((id (plist-get args :id))
         (resp (plist-get args :response))
         (pair (agent-shell-queue-get-item-by-id id)))
    (if (not pair)
        (list :status "error" :error (format "No item found with id %s" id))
      (let ((item (cdr pair)))
        (when (and resp (not (string-empty-p resp)))
          (setf (agent-shell-queue-item-response item) resp))
        (condition-case err
            (progn
              (agent-shell-queue-mark-done id)
              (list :status "ok" :id id :status "done"))
          (error
           (list :status "error" :error (error-message-string err))))))))

;;;###autoload
(defun asq-mcp-register ()
  "Ensure `asq-mcp-service' is registered in `mcpkit-registry' and return it."
  (interactive)
  (unless (mcpkit-get-service 'asq)
    (mcpkit-register-service asq-mcp-service))
  asq-mcp-service)

(provide 'asq-mcp)
;;; asq-mcp.el ends here
