;;; mcpkit.el --- MCP service framework for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; mcpkit.el is a registration-based framework for defining Model Context
;; Protocol (MCP) services and tools in Emacs and exposing them over HTTP
;; JSON-RPC 2.0 via web-server.el.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'map)
(require 'seq)
(require 'web-server)

;;; Default Encoders and Decoders

(defun mcpkit--default-encode (result)
  "Encode RESULT into an MCP content array."
  (cond
   ((and (listp result) (plist-get result :type))
    (list result))
   ((and (listp result) (seq-every-p (lambda (item) (and (listp item) (plist-get item :type))) result))
    result)
   ((stringp result)
    (list (list :type "text" :text result)))
   (t
    (list (list :type "text" :text (format "%S" result))))))

;;; Core Structs

(cl-defstruct (mcpkit-tool
               (:constructor make-mcpkit-tool)
               (:conc-name mcpkit-tool-))
  "Structure representing a tool registered on an MCP service."
  (name "" :type string :documentation "Tool name string.")
  (description "" :type string :documentation "Human-readable description of the tool.")
  (input-schema nil :type list :documentation "Plist JSON Schema literal for input arguments.")
  (handler nil :documentation "Tool handler function (lambda (args done) ...).")
  (decode #'identity :type function :documentation "Function to decode raw arguments plist before calling handler.")
  (encode #'mcpkit--default-encode :type function :documentation "Function to encode handler return value into content array."))

(cl-defstruct (mcpkit-service
               (:constructor make-mcpkit-service--raw)
               (:conc-name mcpkit-service-))
  "Structure representing an MCP service definition."
  (name nil :type symbol :documentation "Symbol naming the service.")
  (port 8765 :type integer :documentation "TCP port to listen on.")
  (description "" :type string :documentation "Description of the service.")
  (tools (make-hash-table :test #'equal) :documentation "Hash table mapping tool name strings to `mcpkit-tool' structs.")
  (server nil :documentation "Live `ws-server' struct instance, or nil when inactive.")
  (log-buffer-name nil :type (or null string) :documentation "Name of the log buffer for this service."))

;;; Registry

(defvar mcpkit-registry nil
  "Alist mapping service name symbols to `mcpkit-service' instances.")

(defun mcpkit-register-service (service &optional name)
  "Register SERVICE in `mcpkit-registry' under NAME.
NAME defaults to the service's name symbol."
  (let ((key (or name (mcpkit-service-name service))))
    (setf (map-elt mcpkit-registry key) service)))

(cl-defun mcpkit-define-service (name &key (port 8765) (description ""))
  "Define a new service NAME and register it in `mcpkit-registry'."
  (let ((service (make-mcpkit-service--raw
                  :name name
                  :port port
                  :description description
                  :log-buffer-name (format "*mcpkit-%s*" name))))
    (mcpkit-register-service service name)
    service))

(defun mcpkit-get-service (name-or-service)
  "Retrieve registered service by NAME (symbol or string).
Alternatively returns NAME-OR-SERVICE if already a SERVICE instance."
  (cond
   ((mcpkit-service-p name-or-service) name-or-service)
   ((symbolp name-or-service) (map-elt mcpkit-registry name-or-service))
   ((stringp name-or-service) (map-elt mcpkit-registry (intern name-or-service)))))

(defun mcpkit-remove-service (name-or-service)
  "Remove service by NAME or instance from `mcpkit-registry'."
  (when-let* ((svc (mcpkit-get-service name-or-service)))
    (setq mcpkit-registry (map-delete mcpkit-registry (mcpkit-service-name svc)))))

;;; Tool Registration

(defun mcpkit--register-tool (service-or-name tool-name-sym description input-schema decode encode _async handler-fn)
  "Register tool TOOL-NAME-SYM on SERVICE-OR-NAME with HANDLER-FN."
  (let ((service (mcpkit-get-service service-or-name)))
    (unless service
      (user-error "Service `%s' not found" service-or-name))
    (let* ((name-str (cond
                      ((symbolp tool-name-sym) (symbol-name tool-name-sym))
                      ((stringp tool-name-sym) tool-name-sym)
                      ((and (listp tool-name-sym) (eq (car tool-name-sym) 'quote))
                       (symbol-name (cadr tool-name-sym)))
                      (t (format "%s" tool-name-sym))))
           (tool (make-mcpkit-tool
                  :name name-str
                  :description (or description "")
                  :input-schema (or input-schema '(:type "object" :properties () :required []))
                  :decode (or decode #'identity)
                  :encode (or encode #'mcpkit--default-encode)
                  :handler handler-fn)))
      (setf (map-elt (mcpkit-service-tools service) name-str) tool)
      tool)))

(cl-defmacro mcpkit-register-tool (name service-or-name &rest rest &key description input-schema decode encode async &allow-other-keys)
  "Register a tool NAME on SERVICE-OR-NAME with BODY.
DESCRIPTION is a human-readable docstring.
INPUT-SCHEMA is a JSON schema plist.
DECODE is a function to decode arguments.
ENCODE is a function to encode results.
ASYNC if non-nil means BODY handles calling DONE asynchronously.
BODY is the tool implementation, with ARGS (and DONE if ASYNC) bound."
  (declare (indent 2) (doc-string 3))
  (let* ((tool-sym (if (and (listp name) (eq (car name) 'quote))
                       (cadr name)
                     name))
         (body rest))
    (while (and body (keywordp (car body)))
      (setq body (cddr body)))
    (when (and (stringp (car body)) (cdr body) (not description))
      (setq description (car body)
            body (cdr body)))
    (let ((handler
           (if async
               `(lambda (args done)
                  (condition-case err
                      (progn ,@body)
                    (error (funcall done (error-message-string err) nil))))
             `(lambda (args done)
                (condition-case err
                    (let ((res (progn ,@body)))
                      (funcall done nil res))
                  (error (funcall done (error-message-string err) nil)))))))
      `(mcpkit--register-tool
        ,service-or-name
        ',tool-sym
        ,description
        ,input-schema
        ,decode
        ,encode
        ,async
        ,handler))))

;;; JSON-RPC 2.0 Codec & Error Constants

(defconst mcpkit-error-parse-error -32700
  "Invalid JSON was received by the server.")

(defconst mcpkit-error-invalid-request -32600
  "The JSON sent is not a valid Request object.")

(defconst mcpkit-error-method-not-found -32601
  "The method does not exist / is not available.")

(defconst mcpkit-error-invalid-params -32602
  "Invalid method parameter(s).")

(defconst mcpkit-error-internal-error -32603
  "Internal JSON-RPC error.")

(defun mcpkit--parse-request (json-string)
  "Parse raw JSON-RPC request JSON-STRING into a plist."
  (json-parse-string json-string :object-type 'plist :false-object nil))

(defun mcpkit--make-error-response (id code message &optional data)
  "Construct a JSON-RPC 2.0 error response plist for ID, CODE, MESSAGE, and DATA."
  (let ((err (if data
                 (list :code code :message message :data data)
               (list :code code :message message))))
    (list :jsonrpc "2.0" :id id :error err)))

(defun mcpkit--make-success-response (id result-plist)
  "Construct a JSON-RPC 2.0 success response plist for ID and RESULT-PLIST."
  (list :jsonrpc "2.0" :id id :result result-plist))

(defun mcpkit--serialize-response (response-plist)
  "Serialize RESPONSE-PLIST to a JSON string."
  (json-serialize response-plist :false-object :json-false :null-object nil))

;;; Tool Resolution

(defun mcpkit--service-tools-normalized (svc policy)
  "Return list of `mcpkit-tool' structs from SVC.
Tools are namespaced with service prefix if POLICY is \\='namespace."
  (let (tools)
    (when (mcpkit-service-p svc)
      (maphash
       (lambda (tname tool)
         (let ((exposed-name (if (eq policy 'namespace)
                                 (format "%s__%s" (mcpkit-service-name svc) tname)
                               tname)))
           (if (equal exposed-name (mcpkit-tool-name tool))
               (push tool tools)
             (let ((cloned (copy-mcpkit-tool tool)))
               (setf (mcpkit-tool-name cloned) exposed-name)
               (push cloned tools)))))
       (mcpkit-service-tools svc)))
    (nreverse tools)))

(defun mcpkit--normalize-tools (tools-or-services)
  "Return a list of `mcpkit-tool' structs from TOOLS-OR-SERVICES."
  (cond
   ((null tools-or-services) nil)
   ((hash-table-p tools-or-services)
    (let (tools)
      (maphash (lambda (_k tool) (push tool tools)) tools-or-services)
      (nreverse tools)))
   ((mcpkit-service-p tools-or-services)
    (mcpkit--service-tools-normalized tools-or-services 'error))
   ((and (listp tools-or-services) (seq-every-p #'mcpkit-tool-p tools-or-services))
    tools-or-services)
   ((listp tools-or-services)
    (let (tools)
      (seq-do
       (lambda (item)
         (cond
          ((mcpkit-tool-p item) (push item tools))
          ((consp item)
           (seq-do (lambda (t-item) (push t-item tools))
                   (mcpkit--service-tools-normalized (mcpkit-get-service (car item)) (cdr item))))
          (t
           (when-let* ((svc (mcpkit-get-service item)))
             (seq-do (lambda (t-item) (push t-item tools))
                     (mcpkit--service-tools-normalized svc 'error))))))
       tools-or-services)
      (nreverse tools)))
   (t nil)))

(defun mcpkit--find-tool (name active-services)
  "Find `mcpkit-tool' with NAME string in ACTIVE-SERVICES."
  (seq-find (lambda (tool) (equal (mcpkit-tool-name tool) name))
            (mcpkit--normalize-tools active-services)))

;;; MCP Protocol Handlers

(defun mcpkit--handle-initialize (id &optional _params)
  "Handle MCP initialize request for ID and return response plist."
  (mcpkit--make-success-response
   id
   (list :protocolVersion "2024-11-05"
         :capabilities (list :tools (list :listChanged :json-false))
         :serverInfo (list :name "mcpkit" :version "0.1.0"))))

(defun mcpkit--handle-tools-list (id active-tools)
  "Handle MCP tools/list request for ID and ACTIVE-TOOLS."
  (let* ((tools (mcpkit--normalize-tools active-tools))
         (descriptors (vconcat
                       (seq-map (lambda (tool)
                                  (list :name (mcpkit-tool-name tool)
                                        :description (mcpkit-tool-description tool)
                                        :inputSchema (or (mcpkit-tool-input-schema tool)
                                                         '(:type "object"))))
                                tools))))
    (mcpkit--make-success-response id (list :tools descriptors))))

(defun mcpkit--finish-tool-call (id tool tool-name result start-time done-callback)
  "Complete tool call for ID and TOOL with RESULT, invoking DONE-CALLBACK."
  (let ((dur (* (- (float-time) start-time) 1000.0)))
    (condition-case enc-err
        (let* ((encode-fn (or (mcpkit-tool-encode tool) #'mcpkit--default-encode))
               (encoded (funcall encode-fn result))
               (content-list (cond
                              ((vectorp encoded) (append encoded nil))
                              ((and (listp encoded) (plist-get encoded :type)) (list encoded))
                              ((listp encoded) encoded)
                              (t (list encoded))))
               (content-vec (vconcat content-list))
               (result-plist (list :content content-vec :isError :json-false))
               (resp (mcpkit--make-success-response id result-plist)))
          (funcall done-callback 'success resp tool-name dur nil))
      (error
       (let* ((msg (format "Encode error: %s" (error-message-string enc-err)))
              (resp (mcpkit--make-error-response id mcpkit-error-internal-error msg)))
         (funcall done-callback 'error resp tool-name dur msg))))))

(defun mcpkit--handle-tools-call (id params active-services done-callback &optional start-time)
  "Handle MCP tools/call request for ID and PARAMS on ACTIVE-SERVICES.
Invoke DONE-CALLBACK with status, response-plist, tool-name, dur, msg."
  (let* ((start (or start-time (float-time)))
         (tool-name (plist-get params :name))
         (arguments (plist-get params :arguments))
         (tool (when (stringp tool-name) (mcpkit--find-tool tool-name active-services))))
    (cond
     ((or (null tool-name) (not (stringp tool-name)))
      (let* ((dur (* (- (float-time) start) 1000.0))
             (msg "Invalid params: missing or invalid tool name")
             (resp (mcpkit--make-error-response id mcpkit-error-invalid-params msg)))
        (funcall done-callback 'error resp tool-name dur msg)))
     ((null tool)
      (let* ((dur (* (- (float-time) start) 1000.0))
             (msg (format "Tool not found: %s" tool-name))
             (resp (mcpkit--make-error-response id mcpkit-error-invalid-params msg)))
        (funcall done-callback 'error resp tool-name dur msg)))
     (t
      (condition-case decode-err
          (let* ((decode-fn (or (mcpkit-tool-decode tool) #'identity))
                 (decoded-args (funcall decode-fn (or arguments nil)))
                 (called nil)
                 (tool-done
                  (lambda (&rest cb-args)
                    (unless called
                      (setq called t)
                      (let ((dur (* (- (float-time) start) 1000.0)))
                        (cond
                         ((and (car cb-args) (memq (car cb-args) '(success error)))
                          (apply done-callback cb-args))
                         ((and (>= (length cb-args) 2) (car cb-args))
                          (let* ((err-val (car cb-args))
                                 (msg (if (stringp err-val) err-val (format "%s" err-val)))
                                 (resp (mcpkit--make-error-response id mcpkit-error-internal-error msg)))
                            (funcall done-callback 'error resp tool-name dur msg)))
                         ((>= (length cb-args) 2)
                          (mcpkit--finish-tool-call id tool tool-name (cadr cb-args) start done-callback))
                         ((= (length cb-args) 1)
                          (mcpkit--finish-tool-call id tool tool-name (car cb-args) start done-callback))
                         (t
                          (mcpkit--finish-tool-call id tool tool-name nil start done-callback))))))))
            (condition-case handler-err
                (funcall (mcpkit-tool-handler tool) decoded-args tool-done)
              (error
               (unless called
                 (setq called t)
                 (let* ((dur (* (- (float-time) start) 1000.0))
                        (msg (error-message-string handler-err))
                        (resp (mcpkit--make-error-response id mcpkit-error-internal-error msg)))
                   (funcall done-callback 'error resp tool-name dur msg))))))
        (error
         (let* ((dur (* (- (float-time) start) 1000.0))
                (msg (format "Decode error: %s" (error-message-string decode-err)))
                (resp (mcpkit--make-error-response id mcpkit-error-invalid-params msg)))
           (funcall done-callback 'error resp tool-name dur msg))))))))

(defun mcpkit--format-payload (payload)
  "Format PAYLOAD for log output."
  (cond
   ((null payload) "")
   ((stringp payload) payload)
   (t
    (condition-case nil
        (mcpkit--serialize-response payload)
      (error (format "%S" payload))))))

(defun mcpkit--invoke-done-callback (callback status resp tool-name dur msg &optional req resp-pl)
  "Invoke CALLBACK with STATUS, RESP, TOOL-NAME, DUR, MSG, and REQ, RESP-PL."
  (let ((max-args (cdr (func-arity callback))))
    (if (and (numberp max-args) (<= max-args 5))
        (funcall callback status resp tool-name dur msg)
      (funcall callback status resp tool-name dur msg req resp-pl))))

(defun mcpkit--handle-request-payload (body-string active-services done-callback)
  "Parse BODY-STRING and dispatch JSON-RPC request for ACTIVE-SERVICES.
Invoke DONE-CALLBACK with status, resp, tool-name, dur, msg, req, resp-pl."
  (let ((start-time (float-time)))
    (condition-case parse-err
        (let* ((request (mcpkit--parse-request body-string))
               (id (plist-get request :id))
               (method (plist-get request :method))
               (params (plist-get request :params))
               (inner-done
                (lambda (status resp tool-name dur msg &optional req resp-pl)
                  (mcpkit--invoke-done-callback
                   done-callback
                   status resp tool-name dur msg
                   (or req body-string)
                   (or resp-pl resp)))))
          (cond
           ((equal method "initialize")
            (let* ((dur (* (- (float-time) start-time) 1000.0))
                   (resp (mcpkit--handle-initialize id params)))
              (funcall inner-done 'success resp nil dur nil)))
           ((equal method "tools/list")
            (let* ((dur (* (- (float-time) start-time) 1000.0))
                   (resp (mcpkit--handle-tools-list id active-services)))
              (funcall inner-done 'success resp nil dur nil)))
           ((equal method "tools/call")
            (mcpkit--handle-tools-call id params active-services inner-done start-time))
           (t
            (let* ((dur (* (- (float-time) start-time) 1000.0))
                   (msg (format "Method not found: %s" method))
                   (resp (mcpkit--make-error-response id mcpkit-error-method-not-found msg)))
              (funcall inner-done 'error resp nil dur msg)))))
      (error
       (let* ((dur (* (- (float-time) start-time) 1000.0))
              (msg (format "Parse error: %s" (error-message-string parse-err)))
              (resp (mcpkit--make-error-response nil mcpkit-error-parse-error msg)))
         (mcpkit--invoke-done-callback done-callback 'error resp nil dur msg body-string resp))))))

;;; Transport & Server Lifecycle

(defgroup mcpkit nil
  "Model Context Protocol service framework."
  :group 'tools
  :prefix "mcpkit-")

(defvar mcpkit--active-services nil
  "List of active service cons cells (SERVICE . ON-COLLISION).")

(defvar mcpkit--active-server nil
  "The shared live `ws-server' instance.")

(defcustom mcpkit-log-verbose nil
  "When non-nil, log full decoded arguments and encoded results in `mcpkit--log`."
  :type 'boolean
  :group 'mcpkit)

(defun mcpkit--service-active-p (svc)
  "Return non-nil if SVC is currently active."
  (and (mcpkit-service-p svc)
       (seq-some (lambda (entry) (eq (car entry) svc)) mcpkit--active-services)))

(defun mcpkit--log (status tool-name duration-ms err-msg &optional req-payload resp-payload)
  "Log request outcome STATUS, TOOL-NAME, DURATION-MS, and ERR-MSG.
REQ-PAYLOAD and RESP-PAYLOAD are optional request and response payloads logged
when `mcpkit-log-verbose' is non-nil."
  (let* ((timestamp (format-time-string "%Y-%m-%d %H:%M:%S.%3N"))
         (status-str (if (member status '(success ok "success" "OK")) "OK" "ERROR"))
         (tool-str (if (and tool-name (not (string-empty-p (format "%s" tool-name))))
                       (format "%s" tool-name)
                     "-"))
         (dur-val (or duration-ms 0.0))
         (err-str (if (and err-msg (not (string-empty-p (format "%s" err-msg))))
                      (format " [error: %s]" err-msg)
                    ""))
         (req-str (if (and mcpkit-log-verbose req-payload)
                      (format " [req: %s]" (mcpkit--format-payload req-payload))
                    ""))
         (resp-str (if (and mcpkit-log-verbose resp-payload)
                       (format " [resp: %s]" (mcpkit--format-payload resp-payload))
                     ""))
         (log-line (format "[%s] %s %s %.2fms%s%s%s\n"
                           timestamp status-str tool-str dur-val err-str req-str resp-str))
         (active-buf-names (seq-filter
                            #'identity
                            (seq-map (lambda (entry)
                                       (let ((svc (car entry)))
                                         (and (mcpkit-service-p svc)
                                              (mcpkit-service-log-buffer-name svc))))
                                     mcpkit--active-services)))
         (buf-names (delete-dups (or active-buf-names (list "*mcpkit-server*")))))
    (seq-do
     (lambda (buf-name)
       (with-current-buffer (get-buffer-create buf-name)
         (goto-char (point-max))
         (insert log-line)))
     buf-names)))

(defun mcpkit--handle-request (request)
  "HTTP request handler called by `web-server' for REQUEST."
  (let ((proc (oref request process))
        (body (oref request body)))
    (mcpkit--handle-request-payload
     body
     mcpkit--active-services
     (lambda (status response-payload tool-name duration-ms err-msg &optional req-payload resp-payload)
       (let* ((json-str (if (stringp response-payload)
                            response-payload
                          (mcpkit--serialize-response response-payload)))
              (bytes (string-bytes json-str)))
         (ws-response-header proc 200
                             '("Content-Type" . "application/json; charset=utf-8")
                             (cons "Content-Length" (number-to-string bytes)))
         (process-send-string proc json-str)
         (mcpkit--log status tool-name duration-ms err-msg (or req-payload body) (or resp-payload json-str)))))))

(cl-defun mcpkit-start-service (name-or-service &key (on-collision 'namespace) port)
  "Start or merge SERVICE in `mcpkit--active-services'.
ON-COLLISION specifies how tool name collisions are handled:
\\='namespace (default) prefixes tools with `<service-name>__'.
\\='error signals `user-error' if an un-namespaced tool name collides."
  (interactive
   (list (intern (completing-read "Start MCP service: "
                                  (seq-map (lambda (e) (symbol-name (car e)))
                                           mcpkit-registry)))))
  (let ((svc (mcpkit-get-service name-or-service)))
    (unless svc
      (user-error "No service found for '%s'" name-or-service))
    (unless (mcpkit--service-active-p svc)
      (when (eq on-collision 'error)
        (let ((existing-tools (seq-map #'mcpkit-tool-name (mcpkit--normalize-tools mcpkit--active-services))))
          (maphash
           (lambda (tname _tool)
             (when (member tname existing-tools)
               (user-error "Tool name collision for '%s' when starting service '%s'"
                           tname (mcpkit-service-name svc))))
           (mcpkit-service-tools svc))))
      (push (cons svc on-collision) mcpkit--active-services))
    (unless mcpkit--active-server
      (let* ((listen-port (or port (mcpkit-service-port svc) 8765))
             (log-buf (or (mcpkit-service-log-buffer-name svc) "*mcpkit-server*"))
             (srv (ws-start (list (cons (lambda (_req) t) #'mcpkit--handle-request))
                            listen-port
                            log-buf
                            :host 'local)))
        (setq mcpkit--active-server srv)))
    (setf (mcpkit-service-server svc) mcpkit--active-server)
    svc))

(defun mcpkit-stop-service (name-or-service)
  "Stop SERVICE by removing it from `mcpkit--active-services'.
If no active services remain, close the underlying `ws-server' socket."
  (interactive
   (list (intern (completing-read "Stop MCP service: "
                                  (seq-map (lambda (e) (symbol-name (mcpkit-service-name (car e))))
                                           mcpkit--active-services)))))
  (let ((svc (mcpkit-get-service name-or-service)))
    (when (and svc (mcpkit--service-active-p svc))
      (setq mcpkit--active-services
            (seq-remove (lambda (entry) (eq (car entry) svc)) mcpkit--active-services))
      (setf (mcpkit-service-server svc) nil)
      (when (and (null mcpkit--active-services) mcpkit--active-server)
        (ignore-errors (ws-stop mcpkit--active-server))
        (setq mcpkit--active-server nil)))
    svc))

;;; Tabulated List Mode

(defun mcpkit--service-list-entries ()
  "Generate entries list for `mcpkit-service-list-mode'."
  (seq-map
   (lambda (entry)
     (let* ((name-sym (car entry))
            (svc (cdr entry))
            (status-str (if (mcpkit--service-active-p svc) "active" "inactive"))
            (port-str (number-to-string (mcpkit-service-port svc)))
            (tools-str (number-to-string (hash-table-count (mcpkit-service-tools svc))))
            (desc (or (mcpkit-service-description svc) "")))
       (list name-sym
             (vector (symbol-name name-sym)
                     status-str
                     port-str
                     tools-str
                     desc))))
   mcpkit-registry))

(defvar-keymap mcpkit-service-list-mode-map
  :doc "Keymap for `mcpkit-service-list-mode'."
  :parent tabulated-list-mode-map
  "g" #'mcpkit-service-list-refresh
  "s" #'mcpkit-service-list-start-at-point
  "k" #'mcpkit-service-list-stop-at-point)

(define-derived-mode mcpkit-service-list-mode tabulated-list-mode "MCP-Services"
  "Major mode for browsing and managing MCP services.

\\{mcpkit-service-list-mode-map}"
  (setq tabulated-list-format
        [("Service" 20 t)
         ("Status" 10 t)
         ("Port" 8 (lambda (a b) (< (string-to-number (aref (cadr a) 2))
                                    (string-to-number (aref (cadr b) 2)))))
         ("Tools" 8 (lambda (a b) (< (string-to-number (aref (cadr a) 3))
                                     (string-to-number (aref (cadr b) 3)))))
         ("Description" 30 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Service" . nil))
  (tabulated-list-init-header))

(defun mcpkit-service-list-refresh ()
  "Refresh the MCP service list buffer."
  (interactive)
  (when (derived-mode-p 'mcpkit-service-list-mode)
    (setq tabulated-list-entries (mcpkit--service-list-entries))
    (tabulated-list-print t)))

(defun mcpkit-service-list-start-at-point ()
  "Start the MCP service at point."
  (interactive)
  (when-let* ((id (tabulated-list-get-id)))
    (mcpkit-start-service id)
    (mcpkit-service-list-refresh)
    (message "Started service `%s'" id)))

(defun mcpkit-service-list-stop-at-point ()
  "Stop the MCP service at point."
  (interactive)
  (when-let* ((id (tabulated-list-get-id)))
    (mcpkit-stop-service id)
    (mcpkit-service-list-refresh)
    (message "Stopped service `%s'" id)))

(defun mcpkit-list-services ()
  "Display a tabulated list of registered MCP services."
  (interactive)
  (let ((buf (get-buffer-create "*mcpkit-services*")))
    (with-current-buffer buf
      (mcpkit-service-list-mode)
      (setq tabulated-list-entries (mcpkit--service-list-entries))
      (tabulated-list-print t))
    (display-buffer buf)
    buf))

(provide 'mcpkit)
;;; mcpkit.el ends here
