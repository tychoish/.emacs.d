;;; ollama-tailnet-gptel.el --- Gptel integration for ollama-tailnet -*- lexical-binding: t; -*-

;; Author: Tychoish
;; Keywords: hypermedia, tools, ai

;;; Commentary:
;; Host-default model spec resolution, gptel backend generation,
;; backend caching, and host probing logic for ollama-tailnet.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'gptel)
(require 'ollama-tailnet-vars)

(defvar ollama-tailnet-backend-cache (make-hash-table :test 'equal)
  "Cache of gptel backend instances keyed by host address string.")

(defun ollama-tailnet-clear-backend-cache ()
  "Clear the gptel backend cache."
  (clrhash ollama-tailnet-backend-cache))

(defun ollama-tailnet-resolve-model (host-model-spec)
  "Resolve HOST-MODEL-SPEC string or symbol (e.g. \"derrida/default\", \"derrida/gemma4:32b\", or 'derrida/default)
to a cons cell `(HOST-STRUCT . MODEL-NAME)'.

If spec is a simple host name (e.g. \"derrida\"), model defaults to host's `default-model'.
If host is omitted or not registered, applies `ollama-tailnet-fallback-strategy'."
  (let* ((spec-str (cond
                    ((symbolp host-model-spec) (symbol-name host-model-spec))
                    ((stringp host-model-spec) (string-trim host-model-spec))
                    (t (error "Invalid host-model spec type: %S" host-model-spec))))
         (parts (split-string spec-str "/" t))
         host-sym model-str host-struct)
    (cond
     ((= (length parts) 2)
      (setq host-sym (intern (car parts)))
      (setq model-str (cadr parts)))
     ((= (length parts) 1)
      (let ((maybe-host (ollama-tailnet-get-host (car parts))))
        (if maybe-host
            (progn
              (setq host-struct maybe-host)
              (setq host-sym (ollama-tailnet-host-name host-struct))
              (setq model-str "default"))
          (setq model-str (car parts))
          (setq host-sym nil))))
     (t (error "Invalid host-model spec format: %s" host-model-spec)))

    (when (and host-sym (not host-struct))
      (setq host-struct (ollama-tailnet-get-host host-sym)))

    ;; Host resolution fallback when host-struct is still nil
    (unless host-struct
      (pcase ollama-tailnet-fallback-strategy
        (:prompt
         (let ((hosts (ollama-tailnet-list-hosts)))
           (unless hosts
             (error "No tailnet hosts registered in ollama-tailnet"))
           (let* ((choices (mapcar (lambda (h) (symbol-name (ollama-tailnet-host-name h))) hosts))
                  (picked (completing-read (format "Host for spec '%s' unreachable/unregistered, select host: " spec-str)
                                          choices nil t)))
             (setq host-struct (ollama-tailnet-get-host picked)))))
        (:auto-local
         (setq host-struct (or (cl-find-if #'ollama-tailnet-host-local-p (ollama-tailnet-list-hosts))
                               (car (ollama-tailnet-list-hosts))))
         (unless host-struct
           (error "No local or default tailnet host registered")))
        (:error
         (error "Tailnet host '%s' not found or unreachable" (or host-sym spec-str)))))

    (when (or (null model-str) (string-equal model-str "default") (string-empty-p model-str))
      (setq model-str (ollama-tailnet-host-default-model host-struct)))

    (cons host-struct model-str)))

(defun ollama-tailnet-get-gptel-backend (host-model-spec)
  "Return a gptel backend struct corresponding to HOST-MODEL-SPEC.
HOST-MODEL-SPEC can be a string (e.g. \"derrida/default\") or symbol (e.g. 'derrida/default)."
  (pcase-let ((`(,host-struct . ,model-name) (ollama-tailnet-resolve-model host-model-spec)))
    (let* ((host-sym (ollama-tailnet-host-name host-struct))
           (host-addr (ollama-tailnet-host-address host-struct))
           (models-list (mapcar #'intern (delete-dups (cons model-name (ollama-tailnet-host-models host-struct)))))
           (backend-name (format "Ollama-%s" host-sym))
           (cached-backend (gethash host-addr ollama-tailnet-backend-cache)))
      (if cached-backend
          cached-backend
        (let ((backend (gptel-make-ollama backend-name
                         :host host-addr
                         :models models-list
                         :stream t)))
          (puthash host-addr backend ollama-tailnet-backend-cache)
          backend)))))

(defun ollama-tailnet-set-gptel-backend (host-model-spec &optional local)
  "Set `gptel-backend' and `gptel-model' for HOST-MODEL-SPEC.
If LOCAL is non-nil, set buffer-locally; otherwise set globally."
  (interactive
   (list (completing-read "Host/Model spec: "
                          (mapcan (lambda (h)
                                    (let ((h-name (symbol-name (ollama-tailnet-host-name h))))
                                      (cons (format "%s/default" h-name)
                                            (mapcar (lambda (m) (format "%s/%s" h-name m))
                                                    (ollama-tailnet-host-models h)))))
                                  (ollama-tailnet-list-hosts)))
         current-prefix-arg))
  (pcase-let ((`(,host-struct . ,model-name) (ollama-tailnet-resolve-model host-model-spec)))
    (let ((backend (ollama-tailnet-get-gptel-backend host-model-spec)))
      (if local
          (progn
            (setq-local gptel-backend backend)
            (setq-local gptel-model (intern model-name))
            (message "[ollama-tailnet] Set local buffer backend to %s (%s)"
                     (ollama-tailnet-host-name host-struct) model-name))
        (setq-default gptel-backend backend)
        (setq-default gptel-model (intern model-name))
        (message "[ollama-tailnet] Set default backend to %s (%s)"
                 (ollama-tailnet-host-name host-struct) model-name)))))

(defun ollama-tailnet-probe-host-default (host-name &optional callback)
  "Query remote Ollama /api/tags endpoint for HOST-NAME asynchronously.
Updates host models list and default-model if available.
Invokes CALLBACK with (HOST-STRUCT SUCCESS-P) when complete."
  (let ((host-struct (ollama-tailnet-get-host host-name)))
    (unless host-struct
      (error "Host %s is not registered" host-name))
    (let* ((addr (ollama-tailnet-host-address host-struct))
           (url (format "http://%s/api/tags" addr))
           (url-request-method "GET")
           (url-request-extra-headers '(("Content-Type" . "application/json"))))
      (url-retrieve
       url
       (lambda (status)
         (let ((err (plist-get status :error))
               (success nil))
           (unless err
             (goto-char (point-min))
             (when (re-search-forward "\r?\n\r?\n" nil t)
               (ignore-errors
                 (let* ((json-object-type 'plist)
                        (json-array-type 'list)
                        (data (json-read))
                        (models-data (plist-get data :models))
                        (model-names (mapcar (lambda (m) (plist-get m :name)) models-data)))
                   (when model-names
                     (setf (ollama-tailnet-host-models host-struct) model-names)
                     (setq success t))))))
           (when callback
             (funcall callback host-struct success))))
       nil t t))))

(provide 'ollama-tailnet-gptel)
;;; ollama-tailnet-gptel.el ends here
