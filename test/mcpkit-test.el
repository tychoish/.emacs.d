;;; mcpkit-test.el --- ERT tests for mcpkit.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'map)

(let ((lisp-dir (expand-file-name "../lisp" (file-name-directory (or load-file-name buffer-file-name))))
      (ws-dir (expand-file-name "../elpa/web-server-20210708.2242" (file-name-directory (or load-file-name buffer-file-name)))))
  (unless (member lisp-dir load-path)
    (push lisp-dir load-path))
  (unless (member ws-dir load-path)
    (push ws-dir load-path)))
(require 'mcpkit)

(ert-deftest mcpkit-test/core-registry ()
  "Test defining, retrieving, and removing services in `mcpkit-registry'."
  (let ((mcpkit-registry nil))
    ;; Define service
    (let ((service (mcpkit-define-service 'test-service :port 9000 :description "Test service")))
      (should (mcpkit-service-p service))
      (should (eq (mcpkit-service-name service) 'test-service))
      (should (= (mcpkit-service-port service) 9000))
      (should (equal (mcpkit-service-description service) "Test service"))

      ;; Retrieve service by symbol, string, or instance
      (should (eq (mcpkit-get-service 'test-service) service))
      (should (eq (mcpkit-get-service "test-service") service))
      (should (eq (mcpkit-get-service service) service))

      ;; Define another service under same name replaces it
      (let ((new-service (mcpkit-define-service 'test-service :port 9001)))
        (should (eq (mcpkit-get-service 'test-service) new-service))
        (should-not (eq (mcpkit-get-service 'test-service) service)))

      ;; Remove service
      (mcpkit-remove-service 'test-service)
      (should-not (mcpkit-get-service 'test-service))

      ;; Removing non-existent service is a no-op
      (should-not (mcpkit-remove-service 'unknown-service)))))

(ert-deftest mcpkit-test/registration-macro ()
  "Test `mcpkit-register-tool' and `mcpkit--register-tool' functionality."
  (let ((mcpkit-registry nil))
    ;; Error when service is not registered
    (should-error (mcpkit--register-tool 'non-existent 'tool1 "desc" nil nil nil nil #'ignore)
                  :type 'user-error)
    (should-error (mcpkit-register-tool tool1 'non-existent "desc" (+ 1 2))
                  :type 'user-error)

    (let ((service (mcpkit-define-service 'test-service)))
      ;; 1. Synchronous tool registration with defaults
      (mcpkit-register-tool echo-tool 'test-service
        :description "Echo message"
        (plist-get args :msg))

      (let ((tool (map-elt (mcpkit-service-tools service) "echo-tool")))
        (should (mcpkit-tool-p tool))
        (should (equal (mcpkit-tool-name tool) "echo-tool"))
        (should (equal (mcpkit-tool-description tool) "Echo message"))
        (should (equal (mcpkit-tool-input-schema tool)
                       '(:type "object" :properties () :required [])))
        (should (eq (mcpkit-tool-decode tool) #'identity))
        (should (eq (mcpkit-tool-encode tool) #'mcpkit--default-encode))

        ;; Invoking sync handler success
        (let (err-res val-res)
          (funcall (mcpkit-tool-handler tool)
                   '(:msg "hello")
                   (lambda (err res) (setq err-res err val-res res)))
          (should-not err-res)
          (should (equal val-res "hello"))))

      ;; Sync handler error catching
      (mcpkit-register-tool fail-tool 'test-service
        :description "Always fails"
        (error "Something went wrong"))

      (let ((tool (map-elt (mcpkit-service-tools service) "fail-tool"))
            err-res val-res)
        (funcall (mcpkit-tool-handler tool)
                 nil
                 (lambda (err res) (setq err-res err val-res res)))
        (should (stringp err-res))
        (should (string-match-p "Something went wrong" err-res))
        (should-not val-res))

      ;; 2. Asynchronous tool registration
      (mcpkit-register-tool async-tool 'test-service
        :description "Async computation"
        :async t
        (let ((val (* (plist-get args :x) 2)))
          (funcall done nil val)))

      (let ((tool (map-elt (mcpkit-service-tools service) "async-tool"))
            err-res val-res)
        (should (mcpkit-tool-p tool))
        (funcall (mcpkit-tool-handler tool)
                 '(:x 21)
                 (lambda (err res) (setq err-res err val-res res)))
        (should-not err-res)
        (should (= val-res 42)))

      ;; Async handler synchronous error catching
      (mcpkit-register-tool async-fail-tool 'test-service
        :description "Async with sync error"
        :async t
        (error "Async exploded"))

      (let ((tool (map-elt (mcpkit-service-tools service) "async-fail-tool"))
            err-res val-res)
        (funcall (mcpkit-tool-handler tool)
                 nil
                 (lambda (err res) (setq err-res err val-res res)))
        (should (stringp err-res))
        (should (string-match-p "Async exploded" err-res))
        (should-not val-res))

      ;; 3. Custom schema, decode, and encode functions
      (let* ((custom-schema '(:type "object" :properties (:num (:type "integer")) :required ["num"]))
             (custom-decode (lambda (plist) (list :num (* (plist-get plist :num) 10))))
             (custom-encode (lambda (res) (list (list :type "text" :text (format "Result=%d" res))))))
        (mcpkit-register-tool custom-tool 'test-service
          :description "Custom codec tool"
          :input-schema custom-schema
          :decode custom-decode
          :encode custom-encode
          (plist-get args :num))

        (let ((tool (map-elt (mcpkit-service-tools service) "custom-tool")))
          (should (equal (mcpkit-tool-input-schema tool) custom-schema))
          (should (eq (mcpkit-tool-decode tool) custom-decode))
          (should (eq (mcpkit-tool-encode tool) custom-encode))

          (let* ((raw-args '(:num 5))
                 (decoded (funcall (mcpkit-tool-decode tool) raw-args)))
            (should (equal decoded '(:num 50)))
            (let (res-val)
              (funcall (mcpkit-tool-handler tool) decoded (lambda (_err res) (setq res-val res)))
              (should (= res-val 50))
              (let ((encoded (funcall (mcpkit-tool-encode tool) res-val)))
                (should (equal encoded '((:type "text" :text "Result=50")))))))))

      ;; 4. Registration using service instance variable directly
      (let ((svc (mcpkit-define-service 'var-service)))
        (mcpkit-register-tool instance-tool svc
          :description "Registered via instance"
          "ok")
        (should (mcpkit-tool-p (map-elt (mcpkit-service-tools svc) "instance-tool"))))

      ;; 5. Quoted tool name and string service name
      (mcpkit-register-tool 'quoted-tool "test-service"
        "Docstring description"
        "result-from-quoted")
      (let ((tool (map-elt (mcpkit-service-tools service) "quoted-tool")))
        (should (mcpkit-tool-p tool))
        (should (equal (mcpkit-tool-name tool) "quoted-tool"))
        (should (equal (mcpkit-tool-description tool) "Docstring description"))))))

(ert-deftest mcpkit-test/jsonrpc-codec ()
  "Test JSON-RPC 2.0 codec, MCP envelopes, protocol handlers, and error codes."
  (let ((mcpkit-registry nil))
    ;; 1. Codec helpers
    ;; Parse request
    (let ((req (mcpkit--parse-request "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"test\",\"params\":{\"a\":false}}")))
      (should (equal (plist-get req :jsonrpc) "2.0"))
      (should (equal (plist-get req :id) 1))
      (should (equal (plist-get req :method) "test"))
      (should (null (plist-get (plist-get req :params) :a))))

    ;; Make success response
    (let ((resp (mcpkit--make-success-response 42 '(:status "ok"))))
      (should (equal (plist-get resp :jsonrpc) "2.0"))
      (should (equal (plist-get resp :id) 42))
      (should (equal (plist-get resp :result) '(:status "ok"))))

    ;; Make error response
    (let ((resp1 (mcpkit--make-error-response 42 -32601 "Method not found"))
          (resp2 (mcpkit--make-error-response nil -32700 "Parse error" '(:detail "invalid"))))
      (should (equal (plist-get resp1 :jsonrpc) "2.0"))
      (should (equal (plist-get resp1 :id) 42))
      (should (equal (plist-get (plist-get resp1 :error) :code) -32601))
      (should (equal (plist-get (plist-get resp1 :error) :message) "Method not found"))
      (should-not (plist-get (plist-get resp1 :error) :data))

      (should (null (plist-get resp2 :id)))
      (should (equal (plist-get (plist-get resp2 :error) :code) -32700))
      (should (equal (plist-get (plist-get resp2 :error) :data) '(:detail "invalid"))))

    ;; Serialization
    (let* ((resp (mcpkit--make-success-response 1 (list :flag :json-false :count 10)))
           (json (mcpkit--serialize-response resp)))
      (should (stringp json))
      (should (string-match-p "\"flag\":false" json))
      (should (string-match-p "\"count\":10" json)))

    ;; Setup service and tools for MCP protocol testing
    (let ((service (mcpkit-define-service 'test-service :port 8000 :description "Test service")))
      ;; Register an echo tool
      (mcpkit-register-tool echo 'test-service
        :description "Echo input message"
        :input-schema '(:type "object" :properties (:msg (:type "string")) :required ["msg"])
        (format "Echo: %s" (plist-get args :msg)))

      ;; Register a tool with custom decode
      (mcpkit-register-tool double-num 'test-service
        :description "Double a number"
        :input-schema '(:type "object" :properties (:num (:type "integer")))
        :decode (lambda (args)
                  (let ((num (plist-get args :num)))
                    (unless (numberp num)
                      (error "Parameter `num` must be a number"))
                    (list :num num)))
        (* 2 (plist-get args :num)))

      ;; Register an async tool
      (mcpkit-register-tool async-hello 'test-service
        :description "Async hello tool"
        :async t
        (funcall done nil (format "Hello, %s!" (plist-get args :name))))

      ;; Register a failing tool (for testing -32603 internal error)
      (mcpkit-register-tool failing-tool 'test-service
        :description "A tool that throws an error"
        (error "Internal failure in tool"))

      ;; 2. Test initialize method
      (let* ((init-json "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{}}")
             (cb-called nil)
             (cb-status nil)
             (cb-resp nil)
             (cb-dur nil)
             (cb-err nil))
        (mcpkit--handle-request-payload
         init-json
         service
         (lambda (status resp tool-name dur err)
           (setq cb-called t
                 cb-status status
                 cb-resp resp
                 cb-dur dur
                 cb-err err)))
        (should cb-called)
        (should (eq cb-status 'success))
        (should-not cb-err)
        (should (equal (plist-get cb-resp :jsonrpc) "2.0"))
        (should (equal (plist-get cb-resp :id) 1))
        (let ((result (plist-get cb-resp :result)))
          (should (equal (plist-get result :protocolVersion) "2024-11-05"))
          (should (equal (plist-get (plist-get result :capabilities) :tools)
                         '(:listChanged :json-false)))
          (should (equal (plist-get (plist-get result :serverInfo) :name) "mcpkit"))
          (should (equal (plist-get (plist-get result :serverInfo) :version) "0.1.0"))))

      ;; 3. Test tools/list method
      (let* ((list-json "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/list\"}")
             (cb-called nil)
             (cb-status nil)
             (cb-resp nil))
        (mcpkit--handle-request-payload
         list-json
         service
         (lambda (status resp _tool _dur _err)
           (setq cb-called t
                 cb-status status
                 cb-resp resp)))
        (should cb-called)
        (should (eq cb-status 'success))
        (should (equal (plist-get cb-resp :id) 2))
        (let* ((result (plist-get cb-resp :result))
               (tools-vec (plist-get result :tools)))
          (should (vectorp tools-vec))
          (should (= (length tools-vec) 4))
          ;; Find echo tool descriptor
          (let ((echo-desc (seq-find (lambda (d) (equal (plist-get d :name) "echo"))
                                     (append tools-vec nil))))
            (should echo-desc)
            (should (equal (plist-get echo-desc :description) "Echo input message"))
            (should (equal (plist-get echo-desc :inputSchema)
                           '(:type "object" :properties (:msg (:type "string")) :required ["msg"]))))))

      ;; 4. Test tools/call - Success (synchronous)
      (let* ((call-json "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"tools/call\",\"params\":{\"name\":\"echo\",\"arguments\":{\"msg\":\"world\"}}}")
             (cb-called nil)
             (cb-status nil)
             (cb-resp nil)
             (cb-tool nil)
             (cb-err nil))
        (mcpkit--handle-request-payload
         call-json
         service
         (lambda (status resp tool-name _dur err)
           (setq cb-called t
                 cb-status status
                 cb-resp resp
                 cb-tool tool-name
                 cb-err err)))
        (should cb-called)
        (should (eq cb-status 'success))
        (should-not cb-err)
        (should (equal cb-tool "echo"))
        (should (equal (plist-get cb-resp :id) 3))
        (let* ((result (plist-get cb-resp :result))
               (content (plist-get result :content)))
          (should (equal (plist-get result :isError) :json-false))
          (should (vectorp content))
          (should (equal (aref content 0) '(:type "text" :text "Echo: world")))))

      ;; Test tools/call - Success (async)
      (let* ((call-json "{\"jsonrpc\":\"2.0\",\"id\":4,\"method\":\"tools/call\",\"params\":{\"name\":\"async-hello\",\"arguments\":{\"name\":\"Alice\"}}}")
             (cb-called nil)
             (cb-status nil)
             (cb-resp nil)
             (cb-tool nil))
        (mcpkit--handle-request-payload
         call-json
         service
         (lambda (status resp tool-name _dur _err)
           (setq cb-called t
                 cb-status status
                 cb-resp resp
                 cb-tool tool-name)))
        (should cb-called)
        (should (eq cb-status 'success))
        (should (equal cb-tool "async-hello"))
        (let* ((result (plist-get cb-resp :result))
               (content (plist-get result :content)))
          (should (equal (plist-get result :isError) :json-false))
          (should (equal (aref content 0) '(:type "text" :text "Hello, Alice!")))))

      ;; 5. Test Error -32700 (Parse error)
      (let* ((bad-json "{not valid json")
             (cb-called nil)
             (cb-status nil)
             (cb-resp nil)
             (cb-err nil))
        (mcpkit--handle-request-payload
         bad-json
         service
         (lambda (status resp _tool _dur err)
           (setq cb-called t
                 cb-status status
                 cb-resp resp
                 cb-err err)))
        (should cb-called)
        (should (eq cb-status 'error))
        (should (stringp cb-err))
        (should (null (plist-get cb-resp :id)))
        (let ((err-obj (plist-get cb-resp :error)))
          (should (= (plist-get err-obj :code) -32700))
          (should (stringp (plist-get err-obj :message)))))

      ;; 6. Test Error -32601 (Method not found)
      (let* ((unknown-json "{\"jsonrpc\":\"2.0\",\"id\":5,\"method\":\"unknown/method\",\"params\":{}}")
             (cb-called nil)
             (cb-status nil)
             (cb-resp nil)
             (cb-err nil))
        (mcpkit--handle-request-payload
         unknown-json
         service
         (lambda (status resp _tool _dur err)
           (setq cb-called t
                 cb-status status
                 cb-resp resp
                 cb-err err)))
        (should cb-called)
        (should (eq cb-status 'error))
        (should (string-match-p "unknown/method" cb-err))
        (should (equal (plist-get cb-resp :id) 5))
        (let ((err-obj (plist-get cb-resp :error)))
          (should (= (plist-get err-obj :code) -32601))
          (should (string-match-p "Method not found" (plist-get err-obj :message)))))

      ;; 7. Test Error -32602 (Invalid params: tool not found, missing name, or decode error)
      ;; 7a. Tool not found
      (let* ((missing-tool-json "{\"jsonrpc\":\"2.0\",\"id\":6,\"method\":\"tools/call\",\"params\":{\"name\":\"nonexistent\",\"arguments\":{}}}")
             (cb-called nil)
             (cb-status nil)
             (cb-resp nil)
             (cb-err nil))
        (mcpkit--handle-request-payload
         missing-tool-json
         service
         (lambda (status resp _tool _dur err)
           (setq cb-called t
                 cb-status status
                 cb-resp resp
                 cb-err err)))
        (should cb-called)
        (should (eq cb-status 'error))
        (should (string-match-p "Tool not found" cb-err))
        (should (equal (plist-get cb-resp :id) 6))
        (let ((err-obj (plist-get cb-resp :error)))
          (should (= (plist-get err-obj :code) -32602))
          (should (string-match-p "Tool not found" (plist-get err-obj :message)))))

      ;; 7b. Missing tool name
      (let* ((missing-name-json "{\"jsonrpc\":\"2.0\",\"id\":7,\"method\":\"tools/call\",\"params\":{\"arguments\":{}}}")
             (cb-called nil)
             (cb-status nil)
             (cb-resp nil))
        (mcpkit--handle-request-payload
         missing-name-json
         service
         (lambda (status resp _tool _dur _err)
           (setq cb-called t
                 cb-status status
                 cb-resp resp)))
        (should cb-called)
        (should (eq cb-status 'error))
        (should (equal (plist-get cb-resp :id) 7))
        (let ((err-obj (plist-get cb-resp :error)))
          (should (= (plist-get err-obj :code) -32602))))

      ;; 7c. Decode error
      (let* ((decode-err-json "{\"jsonrpc\":\"2.0\",\"id\":8,\"method\":\"tools/call\",\"params\":{\"name\":\"double-num\",\"arguments\":{\"num\":\"not-a-number\"}}}")
             (cb-called nil)
             (cb-status nil)
             (cb-resp nil)
             (cb-err nil))
        (mcpkit--handle-request-payload
         decode-err-json
         service
         (lambda (status resp _tool _dur err)
           (setq cb-called t
                 cb-status status
                 cb-resp resp
                 cb-err err)))
        (should (string-match-p "Parameter.*num.*must be a number" cb-err))
        (should (eq cb-status 'error))
        (should (equal (plist-get cb-resp :id) 8))
        (let ((err-obj (plist-get cb-resp :error)))
          (should (= (plist-get err-obj :code) -32602))))

      ;; 8. Test Error -32603 (Internal error from tool handler)
      (let* ((fail-json "{\"jsonrpc\":\"2.0\",\"id\":9,\"method\":\"tools/call\",\"params\":{\"name\":\"failing-tool\",\"arguments\":{}}}")
             (cb-called nil)
             (cb-status nil)
             (cb-resp nil)
             (cb-err nil))
        (mcpkit--handle-request-payload
         fail-json
         service
         (lambda (status resp _tool _dur err)
           (setq cb-called t
                 cb-status status
                 cb-resp resp
                 cb-err err)))
        (should cb-called)
        (should (eq cb-status 'error))
        (should (string-match-p "Internal failure in tool" cb-err))
        (should (equal (plist-get cb-resp :id) 9))
        (let ((err-obj (plist-get cb-resp :error)))
          (should (= (plist-get err-obj :code) -32603))
          (should (string-match-p "Internal failure in tool" (plist-get err-obj :message))))))))

(ert-deftest mcpkit-test/transport-wiring ()
  "Test service starting, stopping, merging, and collision handling."
  (let ((mcpkit-registry nil)
        (mcpkit--active-services nil)
        (mcpkit--active-server nil)
        (ws-started-count 0)
        (ws-stopped-count 0))
    (cl-letf (((symbol-function 'ws-start)
               (lambda (handlers port log-buffer &rest _args)
                 (cl-incf ws-started-count)
                 (make-instance 'ws-server :handlers handlers :port port)))
              ((symbol-function 'ws-stop)
               (lambda (_server)
                 (cl-incf ws-stopped-count))))
      (let ((svc1 (mcpkit-define-service 'svc1 :port 8001))
            (svc2 (mcpkit-define-service 'svc2 :port 8002)))
        (mcpkit-register-tool toolA 'svc1 :description "Tool A" (list "A"))
        (mcpkit-register-tool toolB 'svc2 :description "Tool B" (list "B"))
        (mcpkit-register-tool toolA 'svc2 :description "Tool A in Svc2" (list "A2"))

        ;; Start svc1
        (mcpkit-start-service 'svc1)
        (should (= ws-started-count 1))
        (should (mcpkit-service-server svc1))

        ;; Merging svc2 with default on-collision 'namespace succeeds
        (mcpkit-start-service 'svc2 :on-collision 'namespace)
        (should (= ws-started-count 1))

        ;; Check normalized tools
        (let ((tools (mcpkit--normalize-tools mcpkit--active-services)))
          (should (seq-find (lambda (t-item) (equal (mcpkit-tool-name t-item) "svc1__toolA")) tools))
          (should (seq-find (lambda (t-item) (equal (mcpkit-tool-name t-item) "svc2__toolA")) tools))
          (should (seq-find (lambda (t-item) (equal (mcpkit-tool-name t-item) "svc2__toolB")) tools)))

        ;; Stopping svc1 leaves svc2 and listener active
        (mcpkit-stop-service 'svc1)
        (should-not (mcpkit-service-server svc1))
        (should (= ws-stopped-count 0))

        ;; Stopping svc2 closes listener
        (mcpkit-stop-service 'svc2)
        (should (= ws-stopped-count 1))
        (should-not mcpkit--active-server)

        ;; Test on-collision 'error
        (mcpkit-start-service 'svc1 :on-collision 'error)
        (should-error (mcpkit-start-service 'svc2 :on-collision 'error) :type 'user-error)))))
(ert-deftest mcpkit-test/interactive-commands ()
  "Test interactive specs and functionality of `mcpkit-list-services', `mcpkit-start-service', `mcpkit-stop-service'."
  (let ((mcpkit-registry nil)
        (mcpkit--active-services nil)
        (mcpkit--active-server nil))
    ;; 1. Check interactive forms exist
    (should (interactive-form #'mcpkit-start-service))
    (should (interactive-form #'mcpkit-stop-service))
    (should (interactive-form #'mcpkit-list-services))

    ;; 2. Empty registry listing
    (let ((buf (mcpkit-list-services)))
      (should (buffer-live-p buf))
      (with-current-buffer buf
        (should (derived-mode-p 'mcpkit-service-list-mode))
        (should (derived-mode-p 'tabulated-list-mode))
        (should-not tabulated-list-entries)))

    ;; 3. Registry with services and tools
    (let ((svc1 (mcpkit-define-service 'alpha-svc :port 8001 :description "Alpha service description"))
          (svc2 (mcpkit-define-service 'beta-svc :port 8002 :description "Beta service description")))
      (mcpkit-register-tool tool1 'alpha-svc :description "Tool 1" (list "1"))
      (mcpkit-register-tool tool2 'alpha-svc :description "Tool 2" (list "2"))
      (mcpkit-register-tool tool3 'beta-svc :description "Tool 3" (list "3"))

      ;; Mock ws-start and ws-stop for interactive calls
      (cl-letf (((symbol-function 'ws-start)
                 (lambda (handlers port _log-buffer &rest _args)
                   (make-instance 'ws-server :handlers handlers :port port)))
                ((symbol-function 'ws-stop) #'ignore)
                ((symbol-function 'completing-read)
                 (lambda (prompt choices &rest _args)
                   (cond
                    ((string-match-p "Start" prompt) "alpha-svc")
                    ((string-match-p "Stop" prompt) "alpha-svc")
                    (t (if (consp choices) (car choices) "alpha-svc"))))))
        ;; Call mcpkit-start-service interactively
        (call-interactively #'mcpkit-start-service)
        (should (mcpkit--service-active-p svc1))

        ;; Check list-services with active/inactive services
        (let ((buf (mcpkit-list-services)))
          (with-current-buffer buf
            (should (derived-mode-p 'mcpkit-service-list-mode))
            (should (= (length tabulated-list-entries) 2))

            ;; Test point actions
            (goto-char (point-min))
            (mcpkit-service-list-stop-at-point)
            (should-not (mcpkit--service-active-p svc1))

            (mcpkit-service-list-start-at-point)
            (should (mcpkit--service-active-p svc1))))

        ;; Call mcpkit-stop-service interactively
        (call-interactively #'mcpkit-stop-service)
        (call-interactively #'mcpkit-stop-service)
        (should-not (mcpkit--service-active-p svc1))))))
(ert-deftest mcpkit-test/request-logging ()
  "Test request logging formatting, buffer writing, timing, and verbose toggle."
  (let ((mcpkit-registry nil)
        (mcpkit--active-services nil)
        (mcpkit--active-server nil)
        (mcpkit-log-verbose nil))
    ;; 1. Direct mcpkit--log with default buffer (*mcpkit-server*)
    (let ((buf (get-buffer-create "*mcpkit-server*")))
      (with-current-buffer buf (erase-buffer))
      (mcpkit--log 'success "my-tool" 12.345 nil)
      (with-current-buffer buf
        (let ((log-text (buffer-string)))
          ;; Timestamp format [YYYY-MM-DD HH:MM:SS.NNN]
          (should (string-match-p "\\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\.[0-9]\\{3\\}\\]" log-text))
          ;; Status OK, tool my-tool, duration 12.35ms
          (should (string-match-p "OK my-tool 12\\.35ms" log-text))
          ;; No error or payload
          (should-not (string-match-p "\\[error:" log-text))
          (should-not (string-match-p "\\[req:" log-text))
          (should-not (string-match-p "\\[resp:" log-text)))))

    ;; 2. Log with error and nil tool name
    (let ((buf (get-buffer-create "*mcpkit-server*")))
      (with-current-buffer buf (erase-buffer))
      (mcpkit--log 'error nil 5.0 "Something went wrong")
      (with-current-buffer buf
        (let ((log-text (buffer-string)))
          (should (string-match-p "ERROR - 5\\.00ms \\[error: Something went wrong\\]" log-text)))))

    ;; 3. Verbose logging toggle
    (let ((buf (get-buffer-create "*mcpkit-server*")))
      (with-current-buffer buf (erase-buffer))
      (let ((mcpkit-log-verbose nil))
        (mcpkit--log 'success "echo" 1.5 nil "{\"input\":\"test\"}" '(:result "ok"))
        (with-current-buffer buf
          (let ((log-text (buffer-string)))
            (should-not (string-match-p "\\[req:" log-text))
            (should-not (string-match-p "\\[resp:" log-text)))))
      (with-current-buffer buf (erase-buffer))
      (let ((mcpkit-log-verbose t))
        (mcpkit--log 'success "echo" 1.5 nil "{\"input\":\"test\"}" '(:result "ok"))
        (with-current-buffer buf
          (let ((log-text (buffer-string)))
            (should (string-match-p "\\[req: {\"input\":\"test\"}\\]" log-text))
            (should (string-match-p "\\[resp: .*\"result\":\"ok\".*\\]" log-text))))))

    ;; 4. Active service specific log buffer
    (let* ((svc (mcpkit-define-service 'log-test-svc :port 8080 :description "Log test"))
           (custom-buf (get-buffer-create "*mcpkit-log-test-svc*")))
      (with-current-buffer custom-buf (erase-buffer))
      (setq mcpkit--active-services (list (cons svc 'namespace)))
      (mcpkit--log 'success "test-op" 42.0 nil)
      (with-current-buffer custom-buf
        (should (string-match-p "OK test-op 42\\.00ms" (buffer-string)))))

    ;; 5. End-to-end handle-request with logging
    (let* ((svc (mcpkit-define-service 'req-svc :port 8081))
           (svc-buf (get-buffer-create "*mcpkit-req-svc*"))
           (mcpkit-log-verbose t))
      (with-current-buffer svc-buf (erase-buffer))
      (mcpkit-register-tool ping 'req-svc
        :description "Ping tool"
        "pong")
      (setq mcpkit--active-services (list (cons svc 'namespace)))
      ;; Mock request object
      (let* ((req-body "{\"jsonrpc\":\"2.0\",\"id\":99,\"method\":\"tools/call\",\"params\":{\"name\":\"req-svc__ping\",\"arguments\":{}}}")
             (sent-headers nil)
             (sent-body nil)
             (mock-proc (make-process :name "mock-proc" :buffer nil :command '("cat"))))
        (unwind-protect
            (cl-letf (((symbol-function 'ws-response-header)
                       (lambda (_proc _code &rest headers)
                         (setq sent-headers headers)))
                      ((symbol-function 'process-send-string)
                       (lambda (_proc str)
                         (setq sent-body str))))
              (let ((fake-req (make-instance 'ws-request :process mock-proc :body req-body)))
                (mcpkit--handle-request fake-req)
                (should sent-body)
                (should (string-match-p "\"pong\"" sent-body))
                ;; Verify log was written to svc-buf
                (with-current-buffer svc-buf
                  (let ((log-text (buffer-string)))
                    (should (string-match-p "OK req-svc__ping" log-text))
                    (should (string-match-p "\\[req: .*\\]" log-text))
                    (should (string-match-p "\\[resp: .*\\]" log-text))))))
          (delete-process mock-proc))))))

(provide 'mcpkit-test)
;;; mcpkit-test.el ends here

