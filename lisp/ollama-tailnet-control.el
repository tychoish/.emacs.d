;;; ollama-tailnet-control.el --- Orchestration and control interface for ollama-tailnet -*- lexical-binding: t; -*-

;; Author: Tychoish
;; Keywords: hypermedia, tools, ai

;;; Commentary:
;; Interactive commands, systemd service lifecycle control, and REST API
;; integration (pulling models, inspecting process status) for Ollama hosts.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'ollama-tailnet-vars)
(require 'ollama-tailnet-gptel)

(defgroup ollama-tailnet-control nil
  "Control interface for ollama-tailnet."
  :group 'ollama-tailnet)

(defun ollama-tailnet--systemctl-cmd (host-struct &rest args)
  "Execute systemctl command with ARGS for HOST-STRUCT.
Returns a list of command arguments for `make-process'."
  (let ((address (ollama-tailnet-host-address host-struct))
        (local-p (ollama-tailnet-host-local-p host-struct)))
    (if local-p
        (cons "systemctl" (cons "--user" args))
      (let ((hostname (car (split-string address ":"))))
        (append (list "ssh" hostname "systemctl" "--user") args)))))

(defun ollama-tailnet-service-restart (host-name)
  "Restart systemd ollama.service on HOST-NAME."
  (interactive
   (list (completing-read "Restart Ollama service on host: "
                          (mapcar (lambda (h) (symbol-name (ollama-tailnet-host-name h)))
                                  (ollama-tailnet-list-hosts)))))
  (let ((host-struct (ollama-tailnet-get-host host-name)))
    (unless host-struct (error "Unknown host: %s" host-name))
    (let* ((cmd (ollama-tailnet--systemctl-cmd host-struct "restart" "ollama"))
           (buf (get-buffer-create "*ollama-tailnet-systemd*")))
      (message "[ollama-tailnet] Restarting ollama service on %s..." host-name)
      (make-process
       :name (format "ollama-restart-%s" host-name)
       :buffer buf
       :command cmd
       :sentinel
       (lambda (proc event)
         (when (memq (process-status proc) '(exit signal))
           (if (= (process-exit-status proc) 0)
               (message "[ollama-tailnet] Successfully restarted ollama service on %s" host-name)
             (message "[ollama-tailnet] Failed to restart service on %s: %s" host-name (string-trim event)))))))))

(defun ollama-tailnet-service-status (host-name)
  "Check systemd ollama.service status on HOST-NAME and display output."
  (interactive
   (list (completing-read "Check Ollama service status on host: "
                          (mapcar (lambda (h) (symbol-name (ollama-tailnet-host-name h)))
                                  (ollama-tailnet-list-hosts)))))
  (let ((host-struct (ollama-tailnet-get-host host-name)))
    (unless host-struct (error "Unknown host: %s" host-name))
    (let* ((cmd (ollama-tailnet--systemctl-cmd host-struct "status" "ollama"))
           (buf (get-buffer-create (format "*ollama-status-%s*" host-name))))
      (with-current-buffer buf
        (read-only-mode -1)
        (erase-buffer))
      (make-process
       :name (format "ollama-status-%s" host-name)
       :buffer buf
       :command cmd
       :sentinel
       (lambda (_proc _event)
         (display-buffer buf))))))

(defun ollama-tailnet-pull-model (host-name model-name)
  "Pull MODEL-NAME on HOST-NAME with live progress reporting."
  (interactive
   (let* ((hosts (ollama-tailnet-list-hosts))
          (h-picked (completing-read "Host: "
                                     (mapcar (lambda (h) (symbol-name (ollama-tailnet-host-name h))) hosts)))
          (m-picked (read-string "Model to pull (e.g. gemma4:32b): ")))
     (list h-picked m-picked)))
  (let ((host-struct (ollama-tailnet-get-host host-name)))
    (unless host-struct (error "Unknown host: %s" host-name))
    (let* ((addr (ollama-tailnet-host-address host-struct))
           (url (format "http://%s/api/pull" addr))
           (payload (json-encode `((name . ,model-name) (stream . t))))
           (url-request-method "POST")
           (url-request-extra-headers '(("Content-Type" . "application/json")))
           (url-request-data payload))
      (message "[ollama-tailnet] Starting pull of %s on %s..." model-name host-name)
      (url-retrieve
       url
       (lambda (status)
         (let ((err (plist-get status :error)))
           (if err
               (message "[ollama-tailnet] Pull failed for %s on %s: %S" model-name host-name err)
             (message "[ollama-tailnet] Finished pull of %s on %s" model-name host-name)
             (let ((cur-mods (ollama-tailnet-host-models host-struct)))
               (setf (ollama-tailnet-host-models host-struct)
                     (delete-dups (cons model-name cur-mods)))))))
       nil t t))))

(defun ollama-tailnet-status ()
  "Display status summary buffer for all registered tailnet Ollama hosts."
  (interactive)
  (let ((buf (get-buffer-create "*ollama-tailnet-status*"))
        (hosts (ollama-tailnet-list-hosts)))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (insert "= Ollama Tailnet Status Summary =\n\n")
      (if (null hosts)
          (insert "No tailnet hosts registered.\n")
        (dolist (h hosts)
          (let ((reachable (ollama-tailnet-host-reachable-p h)))
            (insert (format "[%s] %s\n"
                            (ollama-tailnet-host-name h)
                            (if reachable "(online)" "(offline/unreachable)")))
            (insert (format "  Address:       %s\n" (ollama-tailnet-host-address h)))
            (insert (format "  Local Host:    %s\n" (if (ollama-tailnet-host-local-p h) "Yes" "No")))
            (insert (format "  Default Model: %s\n" (ollama-tailnet-host-default-model h)))
            (insert (format "  Models:        %s\n\n" (string-join (ollama-tailnet-host-models h) ", ")))))))
    (display-buffer buf)))

(provide 'ollama-tailnet-control)
;;; ollama-tailnet-control.el ends here
