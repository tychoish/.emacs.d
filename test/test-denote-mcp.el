;;; test-denote-mcp.el --- Tests for denote-mcp and mcpkit-emacs -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'test-helper)

(require 'mcpkit)
(require 'denote-mcp)
(require 'mcpkit-emacs)

(defmacro test-mcpkit--with-temp-denote (&rest body)
  "Execute BODY in a sandbox `denote-directory'."
  (declare (indent 0))
  `(let* ((temp-dir (make-temp-file "denote-test-" t))
          (denote-directory temp-dir)
          (mcpkit-registry nil)
          (mcpkit--active-services nil)
          (mcpkit--active-server nil))
     (unwind-protect
         (progn ,@body)
       (when (file-directory-p temp-dir)
         (delete-directory temp-dir t)))))

(ert-deftest test-denote-mcp/registration ()
  "Test that all denote tools are registered on the denote service."
  (let ((mcpkit-registry nil))
    (let ((svc (denote-mcp-register)))
      (should (mcpkit-service-p svc))
      (should (eq (mcpkit-service-name svc) 'denote))
      (let ((tools (mcpkit-service-tools svc)))
        (should (gethash "denote_find" tools))
        (should (gethash "denote_find_by_slug" tools))
        (should (gethash "denote_find_most_recent" tools))
        (should (gethash "denote_get_metadata" tools))
        (should (gethash "denote_read_note" tools))
        (should (gethash "denote_create_note" tools))
        (should (gethash "denote_append_body" tools))
        (should (gethash "denote_rename" tools))
        (should (gethash "denote_mark_executed" tools))
        (should (gethash "denote_sync_frontmatter" tools))
        (should (gethash "denote_seq_get_next" tools))
        (should (gethash "denote_seq_set" tools))
        (should (gethash "denote_seq_graft" tools))
        (should (gethash "denote_seq_reparent" tools))
        (should (gethash "denote_seq_tree" tools))
        (should (gethash "denote_verify" tools))
        (should (gethash "denote_link_string" tools))
        (should (gethash "denote_insert_dblock" tools))
        (should (gethash "denote_redate" tools))))))

(ert-deftest test-mcpkit-emacs/registration ()
  "Test that core Emacs tools are registered on the emacs service."
  (let ((mcpkit-registry nil))
    (let ((svc (mcpkit-emacs-register)))
      (should (mcpkit-service-p svc))
      (should (eq (mcpkit-service-name svc) 'emacs))
      (let ((tools (mcpkit-service-tools svc)))
        (should (gethash "emacs_server_status" tools))
        (should (gethash "emacs_get_buffer" tools))
        (should (gethash "emacs_eval" tools))))))

(ert-deftest test-denote-mcp/crud-workflow ()
  "Test creating a note, reading it, appending body, and reading metadata."
  (test-mcpkit--with-temp-denote
    (let ((svc (denote-mcp-register)))
      ;; Create note
      (let* ((create-tool (gethash "denote_create_note" (mcpkit-service-tools svc)))
             (res (funcall (mcpkit-tool-handler create-tool)
                           (list :title "Test Note Alpha"
                                 :keywords '("agent" "plan")
                                 :sequence "3a1"
                                 :content "* Introduction\nFirst section.")
                           (lambda (_status r) r))))
        (should (plist-get res :path))
        (should (equal (plist-get res :title) "Test Note Alpha"))
        (should (file-exists-p (plist-get res :path)))

        ;; Read note
        (let* ((read-tool (gethash "denote_read_note" (mcpkit-service-tools svc)))
               (read-res (funcall (mcpkit-tool-handler read-tool)
                                  (list :file_or_slug "test-note-alpha")
                                  (lambda (_status r) r))))
          (should (string-search "First section." (plist-get read-res :content))))

        ;; Append body
        (let* ((append-tool (gethash "denote_append_body" (mcpkit-service-tools svc)))
               (app-res (funcall (mcpkit-tool-handler append-tool)
                                 (list :file_or_slug "test-note-alpha"
                                       :content "* Tasks\n- [ ] Task 1")
                                 (lambda (_status r) r))))
          (should (> (plist-get app-res :bytes_appended) 0)))

        ;; Verify appended content
        (let* ((read-tool (gethash "denote_read_note" (mcpkit-service-tools svc)))
               (read-res2 (funcall (mcpkit-tool-handler read-tool)
                                   (list :file_or_slug "test-note-alpha")
                                   (lambda (_status r) r))))
          (should (string-search "Task 1" (plist-get read-res2 :content))))

        ;; Check link string
        (let* ((link-tool (gethash "denote_link_string" (mcpkit-service-tools svc)))
               (link-res (funcall (mcpkit-tool-handler link-tool)
                                  (list :target_slug "test-note-alpha")
                                  (lambda (_status r) r))))
          (should (string-prefix-p "[[denote:" (plist-get link-res :link)))
          (should (string-search "Test Note Alpha" (plist-get link-res :link))))

        ;; Mark executed
        (let* ((mark-tool (gethash "denote_mark_executed" (mcpkit-service-tools svc)))
               (mark-res (funcall (mcpkit-tool-handler mark-tool)
                                  (list :file_or_slug "test-note-alpha")
                                  (lambda (_status r) r))))
          (should (equal (plist-get mark-res :status) "marked"))
          (should (member "x" (plist-get mark-res :keywords))))))))

(ert-deftest test-mcpkit-emacs/eval-and-buffer ()
  "Test evaluation and buffer inspection."
  (let ((mcpkit-registry nil))
    (let ((svc (mcpkit-emacs-register)))
      ;; Eval tool
      (let* ((eval-tool (gethash "emacs_eval" (mcpkit-service-tools svc)))
             (res (funcall (mcpkit-tool-handler eval-tool)
                           (list :expression "(+ 40 2)")
                           (lambda (_status r) r))))
        (should (equal (plist-get res :status) "success"))
        (should (equal (plist-get res :result) "42")))

      ;; Get buffer
      (with-current-buffer (get-buffer-create "*test-mcpkit-buf*")
        (insert "Buffer test payload")
        (let* ((buf-tool (gethash "emacs_get_buffer" (mcpkit-service-tools svc)))
               (res (funcall (mcpkit-tool-handler buf-tool)
                             (list :buffer_or_file "*test-mcpkit-buf*")
                             (lambda (_status r) r))))
          (should (plist-get res :found))
          (should (equal (plist-get res :name) "*test-mcpkit-buf*"))
          (should (string-search "Buffer test payload" (plist-get res :content))))
        (kill-buffer "*test-mcpkit-buf*")))))

(ert-deftest test-denote-mcp/jsonrpc-payload ()
  "Test JSON-RPC 2.0 dispatch over `mcpkit--handle-request-payload'."
  (test-mcpkit--with-temp-denote
    (let* ((svc (denote-mcp-register))
           (active-services (list (cons svc 'error))))
      ;; Create a note directly first
      (let ((create-tool (gethash "denote_create_note" (mcpkit-service-tools svc))))
        (funcall (mcpkit-tool-handler create-tool)
                 (list :title "Payload Test Note" :keywords '("test"))
                 (lambda (_status r) r)))

      ;; Test initialize request
      (let* ((init-req "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{}}")
             init-resp)
        (mcpkit--handle-request-payload
         init-req
         active-services
         (lambda (_status resp &rest _)
           (setq init-resp resp)))
        (should (equal (plist-get init-resp :id) 1))
        (should (plist-get (plist-get init-resp :result) :capabilities)))

      ;; Test tools/list request
      (let* ((list-req "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"tools/list\",\"params\":{}}")
             list-resp)
        (mcpkit--handle-request-payload
         list-req
         active-services
         (lambda (_status resp &rest _)
           (setq list-resp resp)))
        (should (equal (plist-get list-resp :id) 2))
        (let ((tools (plist-get (plist-get list-resp :result) :tools)))
          (should (seq-find (lambda (t-desc) (equal (plist-get t-desc :name) "denote_find")) tools))))

      ;; Test tools/call request for denote_find_by_slug
      (let* ((call-req "{\"jsonrpc\":\"2.0\",\"id\":3,\"method\":\"tools/call\",\"params\":{\"name\":\"denote_find_by_slug\",\"arguments\":{\"slug\":\"payload-test\"}}}")
             call-resp)
        (mcpkit--handle-request-payload
         call-req
         active-services
         (lambda (_status resp &rest _)
           (setq call-resp resp)))
        (should (equal (plist-get call-resp :id) 3))
        (let* ((res (plist-get call-resp :result))
               (content (plist-get res :content)))
          (should (vectorp content))
          (should (> (length content) 0)))))))

(provide 'test-denote-mcp)
;;; test-denote-mcp.el ends here
