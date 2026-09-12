;;; ollama-tailnet-models.el --- Model management dashboard for ollama-tailnet -*- lexical-binding: t; -*-

;; Author: Tychoish
;; Keywords: hypermedia, tools, ai, tailscale

;;; Commentary:
;; A tabulated-list dashboard of Ollama models installed across every
;; registered tailnet host, mirroring `arch.el's browsing/search/upgrade UX:
;; one list across hosts, pull/delete/upgrade actions, an upstream-digest
;; based "upgradeable" check, and library search.
;;
;; "Upgradeable" has no local equivalent to a package version number — an
;; Ollama tag is mutable, so the only way to know if a pulled tag is stale is
;; to compare its local manifest digest against the digest the registry
;; serves for that tag right now.  `ollama-tailnet-remote-manifest-digest'
;; does that via an unauthenticated HEAD request against the same
;; `registry.ollama.ai' distribution-spec endpoint the `ollama' CLI itself
;; talks to; it only works for unnamespaced official library models.
;;
;; Library search has no documented JSON API, so `ollama-tailnet-search-library'
;; is a best-effort scrape of `ollama.com/search'; if that markup changes the
;; search may return nothing, but pulling a model by name never depended on
;; search succeeding.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'transient)
(require 'annotated-completing-read)
(require 'ollama-tailnet-vars)
(require 'ollama-tailnet-control)

;;; HTTP helpers

(defun ollama-tailnet--request-json (host-struct method path &optional payload)
  "Synchronously request METHOD PATH on HOST-STRUCT, JSON-encoding PAYLOAD.
Returns the parsed JSON body as an alist, or nil for an empty body.
Signals an error on an unreachable host or a non-2xx HTTP status."
  (let* ((url-request-method method)
         (url-request-extra-headers (and payload '(("Content-Type" . "application/json"))))
         (url-request-data (and payload (encode-coding-string (json-encode payload) 'utf-8)))
         (url (format "http://%s%s" (ollama-tailnet-host-address host-struct) path))
         (buf (url-retrieve-synchronously url t t 10)))
    (unless buf
      (error "ollama-tailnet: no response from %s" url))
    (unwind-protect
        (with-current-buffer buf
          (let* ((status (bound-and-true-p url-http-response-status))
                 (body-start (if (bound-and-true-p url-http-end-of-headers)
                                 (marker-position url-http-end-of-headers)
                               (point-min)))
                 (body (string-trim (buffer-substring-no-properties body-start (point-max)))))
            (when (and status (>= status 400))
              (error "ollama-tailnet: HTTP %d from %s: %s" status url body))
            (unless (string-empty-p body)
              (json-parse-string body :object-type 'alist :array-type 'list))))
      (kill-buffer buf))))

(defun ollama-tailnet--split-model-name (model-name)
  "Split MODEL-NAME \"name[:tag]\" into (NAME . TAG); TAG defaults to \"latest\"."
  (if (string-match "\\`\\([^:]+\\):\\(.+\\)\\'" model-name)
      (cons (match-string 1 model-name) (match-string 2 model-name))
    (cons model-name "latest")))

(defun ollama-tailnet-remote-manifest-digest (model-name)
  "Return the upstream registry manifest digest for MODEL-NAME.
Only supported for unnamespaced official library models (no \"/\" in the
name, e.g. \"llama3:8b\"); namespaced or third-party models signal a
user-error since their registry path cannot be inferred."
  (let* ((parts (ollama-tailnet--split-model-name model-name))
         (name (car parts))
         (tag (cdr parts)))
    (when (string-match-p "/" name)
      (user-error "Remote digest check only supports official library models, not %S" model-name))
    (let* ((url (format "https://registry.ollama.ai/v2/library/%s/manifests/%s" name tag))
           (url-request-method "HEAD")
           (buf (url-retrieve-synchronously url t t 10)))
      (unless buf
        (error "ollama-tailnet: no response from registry for %s" model-name))
      (unwind-protect
          (with-current-buffer buf
            (goto-char (point-min))
            (let ((end (if (bound-and-true-p url-http-end-of-headers)
                           url-http-end-of-headers
                         (point-max))))
              (if (re-search-forward "^ollama-content-digest:[ \t]*\\([0-9a-fA-F]+\\)" end t)
                  (concat "sha256:" (downcase (match-string 1)))
                (error "ollama-tailnet: no digest header in registry response for %s" model-name))))
        (kill-buffer buf)))))

;;; Model data model

(cl-defstruct (ollama-tailnet-model (:constructor ollama-tailnet-model--make) (:copier nil))
  "A model installed on an Ollama tailnet host."
  host           ; symbol — tailnet host name
  name           ; string, e.g. "llama3:8b"
  size           ; number, bytes
  modified       ; string, ISO8601 timestamp from /api/tags
  digest         ; string, "sha256:<hex>"
  remote-digest  ; string or nil — populated by `ollama-tailnet-check-model-update'
  upgradeable-p) ; nil, t, or the symbol `unknown'

(defun ollama-tailnet--parse-tags-response (data host-name)
  "Parse an `/api/tags' JSON alist DATA into a list of
`ollama-tailnet-model' for HOST-NAME."
  (seq-map
   (lambda (m)
     (ollama-tailnet-model--make
      :host host-name
      :name (alist-get 'name m)
      :size (alist-get 'size m)
      :modified (alist-get 'modified_at m)
      :digest (alist-get 'digest m)))
   (alist-get 'models data)))

(defun ollama-tailnet-list-models-for-host (host-struct)
  "Return a list of `ollama-tailnet-model' installed on HOST-STRUCT."
  (ollama-tailnet--parse-tags-response
   (ollama-tailnet--request-json host-struct "GET" "/api/tags")
   (ollama-tailnet-host-name host-struct)))

(defun ollama-tailnet-delete-model-on-host (host-struct model-name)
  "Delete MODEL-NAME from HOST-STRUCT via `/api/delete'."
  (ollama-tailnet--request-json host-struct "DELETE" "/api/delete" `((model . ,model-name)))
  t)

(defun ollama-tailnet--normalize-digest (digest)
  "Strip a leading \"sha256:\" prefix from DIGEST, if present."
  (and digest (if (string-prefix-p "sha256:" digest) (substring digest 7) digest)))

(defun ollama-tailnet--digests-differ-p (local remote)
  "Return non-nil if LOCAL and REMOTE digests differ once normalized."
  (and local remote
       (not (equal (ollama-tailnet--normalize-digest local)
                   (ollama-tailnet--normalize-digest remote)))))

(defun ollama-tailnet-check-model-update (model)
  "Query the upstream registry for MODEL and set its `upgradeable-p' slot.
Sets it to the symbol `unknown' rather than signaling on failure (e.g. a
namespaced model, or a network error), since this runs from an interactive
list buffer where a partial failure should just render as unknown."
  (condition-case _err
      (let ((remote (ollama-tailnet-remote-manifest-digest (ollama-tailnet-model-name model))))
        (setf (ollama-tailnet-model-remote-digest model) remote)
        (setf (ollama-tailnet-model-upgradeable-p model)
              (ollama-tailnet--digests-differ-p (ollama-tailnet-model-digest model) remote)))
    (error (setf (ollama-tailnet-model-upgradeable-p model) 'unknown)))
  model)

;;; Library search

(defun ollama-tailnet--parse-library-search (html)
  "Parse `ollama.com/search' result HTML into an alist of (name . summary).
Best-effort scrape of ollama.com's search page markup."
  (let (results (seen (make-hash-table :test #'equal)))
    (with-temp-buffer
      (insert html)
      (goto-char (point-min))
      (while (re-search-forward "href=\"/library/\\([a-zA-Z0-9_.:-]+\\)\"" nil t)
        (let ((name (match-string 1))
              (desc ""))
          (unless (map-elt seen name)
            (setf (map-elt seen name) t)
            (save-excursion
              (when (re-search-forward "<p class=\"max-w-lg[^\"]*\">\\([^<]*\\)</p>" nil t)
                (setq desc (string-trim (match-string 1)))))
            (push (cons name desc) results)))))
    (nreverse results)))

(defun ollama-tailnet-search-library (query)
  "Search the public Ollama model library for QUERY.
Returns an alist of (name . summary)."
  (let* ((url (format "https://ollama.com/search?q=%s" (url-hexify-string query)))
         (url-request-extra-headers '(("User-Agent" . "Mozilla/5.0")))
         (buf (url-retrieve-synchronously url t t 10)))
    (unless buf
      (user-error "ollama-tailnet: could not reach ollama.com"))
    (unwind-protect
        (with-current-buffer buf
          (let ((body-start (if (bound-and-true-p url-http-end-of-headers)
                                (marker-position url-http-end-of-headers)
                              (point-min))))
            (ollama-tailnet--parse-library-search
             (buffer-substring-no-properties body-start (point-max)))))
      (kill-buffer buf))))

;;;###autoload
(defun ollama-tailnet-search-model ()
  "ACR-search the public Ollama library and pull the selected model."
  (interactive)
  (let* ((query (read-string "Search Ollama library: "))
         (results (or (ollama-tailnet-search-library query)
                      (user-error "No results for %S" query)))
         (name (annotated-completing-read
                results
                :prompt (format "Library [%s]: " query)
                :require-match t
                :category 'ollama-tailnet-library-package)))
    (when name
      (let* ((tag (let ((s (read-string (format "Tag for %s (default latest): " name))))
                   (if (string-empty-p s) "latest" s)))
             (host-name (completing-read "Pull to host: "
                                         (seq-map (lambda (h) (symbol-name (ollama-tailnet-host-name h)))
                                                  (ollama-tailnet-list-hosts)))))
        (ollama-tailnet-pull-model host-name (format "%s:%s" name tag))))))

;;; Tabulated list view

(defconst ollama-tailnet--models-buffer-name "*ollama-tailnet-models*"
  "Name of the ollama-tailnet models dashboard buffer.")

(defvar-local ollama-tailnet--models-all-entries nil
  "Full tabulated-list entries for the current ollama-tailnet models buffer.")

(defun ollama-tailnet--format-size (bytes)
  "Format BYTES as a short human-readable size string."
  (cond
   ((not (numberp bytes)) "-")
   ((>= bytes 1e9) (format "%.1f GB" (/ bytes 1e9)))
   ((>= bytes 1e6) (format "%.1f MB" (/ bytes 1e6)))
   ((>= bytes 1e3) (format "%.1f KB" (/ bytes 1e3)))
   (t (format "%d B" bytes))))

(defun ollama-tailnet--format-modified (timestamp)
  "Format ollama's ISO8601 TIMESTAMP as \"YYYY-MM-DD HH:MM\", or \"-\" on failure."
  (condition-case nil
      (format-time-string "%Y-%m-%d %H:%M" (date-to-time timestamp))
    (error "-")))

(defun ollama-tailnet--status-string (model)
  "Return a propertized status string for MODEL's `upgradeable-p' slot."
  (pcase (ollama-tailnet-model-upgradeable-p model)
    ('t (propertize "update avail" 'face 'warning))
    ('unknown (propertize "unknown" 'face 'shadow))
    (_ (propertize "up to date" 'face 'success))))

(defun ollama-tailnet--build-entry (model)
  "Build a tabulated-list entry for `ollama-tailnet-model' MODEL."
  (list model
        (vector
         (symbol-name (ollama-tailnet-model-host model))
         (ollama-tailnet-model-name model)
         (ollama-tailnet--format-size (ollama-tailnet-model-size model))
         (ollama-tailnet--format-modified (ollama-tailnet-model-modified model))
         (ollama-tailnet--status-string model))))

(defvar ollama-tailnet-models-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    map)
  "Keymap for `ollama-tailnet-models-mode'.")

(define-key ollama-tailnet-models-mode-map (kbd "p")   #'ollama-tailnet-models-pull)
(define-key ollama-tailnet-models-mode-map (kbd "u")   #'ollama-tailnet-models-upgrade)
(define-key ollama-tailnet-models-mode-map (kbd "d")   #'ollama-tailnet-models-delete)
(define-key ollama-tailnet-models-mode-map (kbd "c")   #'ollama-tailnet-models-check-update)
(define-key ollama-tailnet-models-mode-map (kbd "C")   #'ollama-tailnet-models-check-update-all)
(define-key ollama-tailnet-models-mode-map (kbd "s")   #'ollama-tailnet-search-model)
(define-key ollama-tailnet-models-mode-map (kbd "g")   #'ollama-tailnet-models-refresh)
(define-key ollama-tailnet-models-mode-map (kbd "RET") #'ollama-tailnet-models-show-info)
(define-key ollama-tailnet-models-mode-map (kbd "?")   #'ollama-tailnet-models-menu)
(define-key ollama-tailnet-models-mode-map (kbd "q")   #'quit-window)

(define-derived-mode ollama-tailnet-models-mode tabulated-list-mode "Ollama-Tailnet"
  "Major mode for browsing and managing Ollama models across tailnet hosts.

Columns: Host | Model | Size | Modified | Status
  Status: up to date / update avail / unknown (until checked via `c')

\\{ollama-tailnet-models-mode-map}"
  (setq tabulated-list-format
        (vector
         '("Host"     12 t)
         '("Model"    30 t)
         '("Size"     10 t)
         '("Modified" 17 t)
         '("Status"   14 t)))
  (setq tabulated-list-sort-key '("Host" . nil))
  (tabulated-list-init-header))

(defun ollama-tailnet--list-models-for-reachable-hosts ()
  "Return the concatenated model list from every reachable tailnet host.
Unreachable hosts, and hosts that error while listing, are skipped with a
message rather than aborting the whole refresh."
  (thread-last (ollama-tailnet-list-hosts)
    (seq-filter (lambda (host)
                  (or (ollama-tailnet-host-reachable-p host)
                      (progn (message "ollama-tailnet: host %s unreachable, skipping"
                                      (ollama-tailnet-host-name host))
                             nil))))
    (seq-mapcat (lambda (host)
                  (condition-case err
                      (ollama-tailnet-list-models-for-host host)
                    (error (message "ollama-tailnet: could not list models on %s: %s"
                                    (ollama-tailnet-host-name host) (error-message-string err))
                           nil))))))

(defun ollama-tailnet-models-refresh ()
  "Refresh the model list across all registered, reachable tailnet hosts."
  (interactive)
  (let ((models (ollama-tailnet--list-models-for-reachable-hosts)))
    (setq ollama-tailnet--models-all-entries (seq-map #'ollama-tailnet--build-entry models))
    (setq tabulated-list-entries ollama-tailnet--models-all-entries)
    (tabulated-list-print t)))

;;; Actions

(defun ollama-tailnet--model-at-point ()
  "Return the `ollama-tailnet-model' at point or signal a user error."
  (or (tabulated-list-get-id)
      (user-error "No model at point")))

(defun ollama-tailnet--host-struct-for-model (model)
  "Return the registered host struct for MODEL's host, or signal an error."
  (or (ollama-tailnet-get-host (ollama-tailnet-model-host model))
      (error "ollama-tailnet: unknown host %s" (ollama-tailnet-model-host model))))

(defun ollama-tailnet-models-pull ()
  "Pull a new model, defaulting the host prompt to the row at point."
  (interactive)
  (let* ((model (ignore-errors (tabulated-list-get-id)))
         (hosts (seq-map (lambda (h) (symbol-name (ollama-tailnet-host-name h)))
                         (ollama-tailnet-list-hosts)))
         (host-name (completing-read "Host: " hosts nil nil
                                     (and model (symbol-name (ollama-tailnet-model-host model)))))
         (model-name (read-string "Model to pull (e.g. llama3:8b): ")))
    (ollama-tailnet-pull-model host-name model-name)))

(defun ollama-tailnet-models-upgrade ()
  "Re-pull the model at point on its host, refreshing it to the latest digest."
  (interactive)
  (let ((model (ollama-tailnet--model-at-point)))
    (ollama-tailnet-pull-model (symbol-name (ollama-tailnet-model-host model))
                               (ollama-tailnet-model-name model))))

(defun ollama-tailnet-models-delete ()
  "Delete the model at point from its host."
  (interactive)
  (let* ((model (ollama-tailnet--model-at-point))
         (host (ollama-tailnet--host-struct-for-model model))
         (name (ollama-tailnet-model-name model)))
    (when (yes-or-no-p (format "Delete %s on %s? " name (ollama-tailnet-model-host model)))
      (ollama-tailnet-delete-model-on-host host name)
      (ollama-tailnet-models-refresh)
      (message "ollama-tailnet: deleted %s on %s" name (ollama-tailnet-model-host model)))))

(defun ollama-tailnet-models-check-update ()
  "Check whether the model at point has a newer digest upstream."
  (interactive)
  (let ((model (ollama-tailnet--model-at-point)))
    (ollama-tailnet-check-model-update model)
    (tabulated-list-set-col 4 (ollama-tailnet--status-string model) t)
    (message "ollama-tailnet: %s %s" (ollama-tailnet-model-name model)
             (pcase (ollama-tailnet-model-upgradeable-p model)
               ('t "has an update available")
               ('unknown "could not be checked (namespaced model or network error)")
               (_ "is up to date")))))

(defun ollama-tailnet-models-check-update-all ()
  "Check every listed model for an available upgrade."
  (interactive)
  (seq-do (lambda (e) (ollama-tailnet-check-model-update (car e))) tabulated-list-entries)
  (tabulated-list-print t)
  (message "ollama-tailnet: update check complete"))

(defun ollama-tailnet-models-show-info ()
  "Show raw `/api/show' details for the model at point."
  (interactive)
  (let* ((model (ollama-tailnet--model-at-point))
         (host (ollama-tailnet--host-struct-for-model model))
         (name (ollama-tailnet-model-name model))
         (data (ollama-tailnet--request-json host "POST" "/api/show" `((model . ,name))))
         (buf (get-buffer-create (format "*ollama-tailnet-model<%s/%s>*"
                                         (ollama-tailnet-model-host model) name))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (emacs-lisp-mode)
        (insert (format ";; %s on %s\n\n" name (ollama-tailnet-model-host model)))
        (insert (pp-to-string data))
        (goto-char (point-min))
        (font-lock-ensure)
        (setq buffer-read-only t)))
    (pop-to-buffer buf)))

;;; Transient menu

(transient-define-prefix ollama-tailnet-models-menu ()
  "Actions for the ollama-tailnet models dashboard."
  [["Model"
    ("p" "Pull new model"    ollama-tailnet-models-pull)
    ("u" "Upgrade (re-pull)" ollama-tailnet-models-upgrade)
    ("d" "Delete"            ollama-tailnet-models-delete)
    ("c" "Check for update"  ollama-tailnet-models-check-update)
    ("C" "Check all"         ollama-tailnet-models-check-update-all)]
   ["View"
    ("RET" "Show info"      ollama-tailnet-models-show-info)
    ("s"   "Search library" ollama-tailnet-search-model)
    ("g"   "Refresh"        ollama-tailnet-models-refresh)]
   ["Quit"
    ("q" "Quit" quit-window)]])

;;;###autoload
(defun ollama-tailnet-models ()
  "Open the model dashboard across all registered tailnet hosts."
  (interactive)
  (let ((buf (get-buffer-create ollama-tailnet--models-buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'ollama-tailnet-models-mode)
        (ollama-tailnet-models-mode))
      (ollama-tailnet-models-refresh))
    (pop-to-buffer buf)))

(provide 'ollama-tailnet-models)
;;; ollama-tailnet-models.el ends here
