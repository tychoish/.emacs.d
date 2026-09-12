;;; daemons-dash.el --- System Daemon Dashboard -*- lexical-binding: t; -*-

;; Author: Tycho Garen
;; Keywords: convenience, processes, services, systemd, docker, sprite, ollama
;; Package-Requires: ((emacs "29.1") (transient "0.4.0"))

;;; Commentary:
;; Unified system service and daemon management dashboard for Emacs.
;; Aggregates multiple service providers (systemd system/user, sprite daemons,
;; docker containers, ollama LLM models) into a single tabulated-list view
;; (`*daemons-dash*`). Provides one-key status operations, context-aware log
;; viewing, interactive filtering/narrowing by provider and status, and
;; integration with `daemons-dash-config.el` for declarative state verification.

;;; Code:

(require 'seq)
(require 'map)
(require 'subr-x)
(require 'cl-lib)
(require 'tabulated-list)
(require 'transient)
(require 'url)
(require 'json)

;; Soft dependencies / forward declarations
(declare-function journalctl--run "journalctl-mode" (transient-opts &optional chunk))
(declare-function docker-container-logs-action "docker-container" (action args))
(declare-function docker-container-read-name "docker-container" ())
(declare-function sprite--registry-all "sprite" ())
(declare-function sprite-name "sprite" (sprite))
(declare-function sprite-status "sprite" (sprite))
(declare-function sprite-pid "sprite" (sprite))
(declare-function sprite-resolve-list "sprite" ())
(declare-function sprite--running-p "sprite" (name))
(declare-function sprite-start-time "sprite" (sprite))
(declare-function sprite--format-uptime "sprite" (seconds))
(declare-function sprite-stop "sprite" (name))
(declare-function sprite-restart "sprite" (name))
(declare-function sprite-open-log "sprite" (name))
(declare-function sprite--log-buffer-name "sprite" (name))

;;; Data Structures

(cl-defstruct (daemons-dash-provider (:constructor daemons-dash-provider--make) (:copier nil))
  "Struct representing a daemons-dash service provider backend."
  name        ; symbol, e.g. 'systemd-user
  label       ; string display name, e.g. "systemd-user"
  list-fn     ; (fn) -> list of `daemons-dash-item'
  start-fn    ; (fn item-id)
  stop-fn     ; (fn item-id)
  restart-fn  ; (fn item-id)
  enable-fn   ; (fn item-id)
  disable-fn  ; (fn item-id)
  logs-fn     ; (fn item-id)
  inspect-fn  ; (fn item-id)
  )

(cl-defstruct (daemons-dash-item (:constructor daemons-dash-item--make) (:copier nil))
  "Struct representing a single service/daemon entry in the dashboard."
  id              ; string unique identifier, e.g. "systemd-user:odem.service"
  name            ; display name string, e.g. "odem.service"
  provider        ; provider symbol, e.g. 'systemd-user
  status          ; symbol or string, e.g. 'active, 'inactive, 'failed, 'running
  details         ; string description (PID, uptime, ports, VRAM)
  config-status   ; symbol: 'match, 'desync, or 'untracked
  expected-status ; symbol or string: expected status when config desyncs
  raw-data        ; plist or underlying data object
  )

;;; Provider Registry

(defvar daemons-dash-providers (make-hash-table :test #'equal)
  "Global registry of daemons-dash provider structs keyed by provider symbol.")

(defun daemons-dash-register-provider (provider)
  "Register PROVIDER struct in `daemons-dash-providers'."
  (unless (daemons-dash-provider-p provider)
    (error "Invalid provider struct: %S" provider))
  (setf (map-elt daemons-dash-providers (daemons-dash-provider-name provider)) provider))

(defun daemons-dash-get-provider (name)
  "Return provider struct registered under symbol NAME, or nil."
  (map-elt daemons-dash-providers name))

;;; Customization Options & Group

(defgroup daemons-dash nil
  "System Daemon Dashboard and Elisp Daemon Registry."
  :group 'tools
  :prefix "daemons-dash-")

(defcustom daemons-dash-enabled-providers '(systemd-user systemd-system sprite docker ollama)
  "List of provider symbols enabled in the dashboard."
  :type '(repeat symbol)
  :group 'daemons-dash)

(defcustom daemons-dash-hidden-providers nil
  "List of provider symbols currently hidden from the dashboard view."
  :type '(repeat symbol)
  :group 'daemons-dash)

(defcustom daemons-dash-hidden-states nil
  "List of state symbols (e.g. 'active, 'inactive, 'failed) hidden from the dashboard view."
  :type '(repeat symbol)
  :group 'daemons-dash)

(defcustom daemons-dash-systemd-show-only nil
  "List of systemd unit names to explicitly include in systemd-system provider.
When non-nil, only systemd units whose names match items in this list are shown."
  :type '(repeat string)
  :group 'daemons-dash)

(defcustom daemons-dash-systemd-show-all-excluding nil
  "List of systemd unit names to exclude from systemd-system provider."
  :type '(repeat string)
  :group 'daemons-dash)

(defcustom daemons-dash-systemd-show-matching-fn #'daemons-dash-user-enabled-service-p
  "Predicate function for systemd-system service inclusion.
Called with unit name string or plist; returns non-nil to include."
  :type 'function
  :group 'daemons-dash)

(defcustom daemons-dash-ollama-host "http://localhost:11434"
  "Base URL for the Ollama local API server."
  :type 'string
  :group 'daemons-dash)

(defcustom daemons-dash-ollama-timeout 2
  "Timeout in seconds for Ollama API requests."
  :type 'integer
  :group 'daemons-dash)

;;; Faces

(defface daemons-dash-face-active
  '((t :inherit success :weight bold))
  "Face for active or running daemons."
  :group 'daemons-dash)

(defface daemons-dash-face-inactive
  '((t :inherit shadow))
  "Face for inactive or stopped daemons."
  :group 'daemons-dash)

(defface daemons-dash-face-failed
  '((t :inherit error :weight bold))
  "Face for failed daemons."
  :group 'daemons-dash)

(defface daemons-dash-face-match
  '((t :inherit success))
  "Face for MATCH configuration status."
  :group 'daemons-dash)

(defface daemons-dash-face-desync
  '((t :inherit warning :weight bold))
  "Face for DESYNC configuration status."
  :group 'daemons-dash)

(defface daemons-dash-face-untracked
  '((t :inherit shadow :slant italic))
  "Face for UNTRACKED configuration status."
  :group 'daemons-dash)

(defface daemons-dash-face-provider
  '((t :inherit font-lock-builtin-face))
  "Face for provider column."
  :group 'daemons-dash)

(defface daemons-dash-face-name
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for daemon name column."
  :group 'daemons-dash)

;;; Systemd Filtering Predicates

(defun daemons-dash-user-enabled-service-p (unit-info)
  "Return non-nil if UNIT-INFO represents a user-enabled or user-relevant system service.
UNIT-INFO can be a unit name string or a plist with :name and :unit-file-state."
  (let ((unit-name (if (plistp unit-info) (plist-get unit-info :name) unit-info))
        (state (when (plistp unit-info) (plist-get unit-info :unit-file-state))))
    (and (stringp unit-name)
         (not (member unit-name daemons-dash-systemd-show-all-excluding))
         (or (null daemons-dash-systemd-show-only)
             (member unit-name daemons-dash-systemd-show-only))
         (or (and state (member state '("enabled" "enabled-runtime" "static" "generated" "indirect")))
             (not (string-match-p "^\\(sys-\\|dev-\\|run-\\|proc-\\|user-\\)" unit-name))))))

;;; Helper Utilities

(defun daemons-dash--run-command (args)
  "Execute ARGS command list synchronously and return standard output string."
  (with-temp-buffer
    (let ((exit-code (apply #'call-process (car args) nil t nil (cdr args))))
      (if (zerop exit-code)
          (buffer-string)
        (buffer-string)))))

(defun daemons-dash--show-journalctl (&rest args)
  "Display journalctl output for ARGS.
Uses `journalctl--run' if available, otherwise executes journalctl directly."
  (if (fboundp 'journalctl--run)
      (journalctl--run args)
    (let ((buf (get-buffer-create (format "*journalctl:%s*" (string-join args " ")))))
      (pop-to-buffer buf)
      (compilation-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (apply #'call-process "journalctl" nil t nil (append args '("-n" "100" "--no-pager")))))))

(defun daemons-dash--parse-systemd-units (output provider-sym)
  "Parse systemctl list-units OUTPUT lines into `daemons-dash-item' structs.
PROVIDER-SYM is 'systemd-user or 'systemd-system."
  (let ((items nil))
    (dolist (line (split-string output "\n" t))
      (let ((trimmed (string-trim line)))
        ;; Remove bullet indicator if present
        (when (string-prefix-p "●" trimmed)
          (setq trimmed (string-trim (substring trimmed 1))))
        (let ((parts (split-string trimmed "[ \t]+" t)))
          (when (>= (length parts) 4)
            (let* ((unit (nth 0 parts))
                   (load (nth 1 parts))
                   (active (nth 2 parts))
                   (sub (nth 3 parts))
                   (desc (string-join (nthcdr 4 parts) " "))
                   (status (cond
                            ((string-equal active "active") 'active)
                            ((string-equal active "failed") 'failed)
                            (t 'inactive)))
                   (details (format "%s (%s/%s) %s" sub load active desc)))
              (push (daemons-dash-item--make
                     :id (format "%s:%s" provider-sym unit)
                     :name unit
                     :provider provider-sym
                     :status status
                     :details details
                     :config-status 'untracked
                     :raw-data (list :name unit :load load :active active :sub sub :desc desc))
                    items))))))
    (nreverse items)))

;;; Provider Implementations

;; 1. systemd-user
(defun daemons-dash-systemd-user-list ()
  "Fetch user systemd services, timers, and sockets."
  (if (executable-find "systemctl")
      (let ((output (daemons-dash--run-command
                     '("systemctl" "--user" "list-units" "--type=service,timer,socket" "--all" "--no-legend" "--no-pager"))))
        (daemons-dash--parse-systemd-units output 'systemd-user))
    (list (daemons-dash-item--make
           :id "systemd-user:unavailable"
           :name "systemd-user"
           :provider 'systemd-user
           :status 'inactive
           :details "systemctl executable not found"
           :config-status 'untracked))))

(defun daemons-dash-systemd-user-start (id)
  (let ((unit (cadr (split-string id ":"))))
    (daemons-dash--run-command (list "systemctl" "--user" "start" unit))))

(defun daemons-dash-systemd-user-stop (id)
  (let ((unit (cadr (split-string id ":"))))
    (daemons-dash--run-command (list "systemctl" "--user" "stop" unit))))

(defun daemons-dash-systemd-user-restart (id)
  (let ((unit (cadr (split-string id ":"))))
    (daemons-dash--run-command (list "systemctl" "--user" "restart" unit))))

(defun daemons-dash-systemd-user-enable (id)
  (let ((unit (cadr (split-string id ":"))))
    (daemons-dash--run-command (list "systemctl" "--user" "enable" unit))))

(defun daemons-dash-systemd-user-disable (id)
  (let ((unit (cadr (split-string id ":"))))
    (daemons-dash--run-command (list "systemctl" "--user" "disable" unit))))

(defun daemons-dash-systemd-user-logs (id)
  "View logs for user systemd unit ID."
  (let ((unit (cadr (split-string id ":"))))
    (require 'journalctl-mode nil t)
    (daemons-dash--show-journalctl (format "--user-unit=%s" unit))))

(daemons-dash-register-provider
 (daemons-dash-provider--make
  :name 'systemd-user
  :label "systemd-user"
  :list-fn #'daemons-dash-systemd-user-list
  :start-fn #'daemons-dash-systemd-user-start
  :stop-fn #'daemons-dash-systemd-user-stop
  :restart-fn #'daemons-dash-systemd-user-restart
  :enable-fn #'daemons-dash-systemd-user-enable
  :disable-fn #'daemons-dash-systemd-user-disable
  :logs-fn #'daemons-dash-systemd-user-logs
  :inspect-fn #'daemons-dash-systemd-user-logs))

;; 2. systemd-system
(defun daemons-dash-systemd-system-list ()
  "Fetch system systemd services filtered by inclusion predicate."
  (if (executable-find "systemctl")
      (let* ((output (daemons-dash--run-command
                      '("systemctl" "list-units" "--type=service,timer,socket" "--all" "--no-legend" "--no-pager")))
             (all-items (daemons-dash--parse-systemd-units output 'systemd-system)))
        (if daemons-dash-systemd-show-matching-fn
            (seq-filter (lambda (item)
                          (funcall daemons-dash-systemd-show-matching-fn (daemons-dash-item-name item)))
                        all-items)
          all-items))
    (list (daemons-dash-item--make
           :id "systemd-system:unavailable"
           :name "systemd-system"
           :provider 'systemd-system
           :status 'inactive
           :details "systemctl executable not found"
           :config-status 'untracked))))

(defun daemons-dash-systemd-system-start (id)
  (let ((unit (cadr (split-string id ":"))))
    (daemons-dash--run-command (list "sudo" "systemctl" "start" unit))))

(defun daemons-dash-systemd-system-stop (id)
  (let ((unit (cadr (split-string id ":"))))
    (daemons-dash--run-command (list "sudo" "systemctl" "stop" unit))))

(defun daemons-dash-systemd-system-restart (id)
  (let ((unit (cadr (split-string id ":"))))
    (daemons-dash--run-command (list "sudo" "systemctl" "restart" unit))))

(defun daemons-dash-systemd-system-enable (id)
  (let ((unit (cadr (split-string id ":"))))
    (daemons-dash--run-command (list "sudo" "systemctl" "enable" unit))))

(defun daemons-dash-systemd-system-disable (id)
  (let ((unit (cadr (split-string id ":"))))
    (daemons-dash--run-command (list "sudo" "systemctl" "disable" unit))))

(defun daemons-dash-systemd-system-logs (id)
  "View logs for system systemd unit ID."
  (let ((unit (cadr (split-string id ":"))))
    (require 'journalctl-mode nil t)
    (daemons-dash--show-journalctl (format "--unit=%s" unit))))

(daemons-dash-register-provider
 (daemons-dash-provider--make
  :name 'systemd-system
  :label "systemd-system"
  :list-fn #'daemons-dash-systemd-system-list
  :start-fn #'daemons-dash-systemd-system-start
  :stop-fn #'daemons-dash-systemd-system-stop
  :restart-fn #'daemons-dash-systemd-system-restart
  :enable-fn #'daemons-dash-systemd-system-enable
  :disable-fn #'daemons-dash-systemd-system-disable
  :logs-fn #'daemons-dash-systemd-system-logs
  :inspect-fn #'daemons-dash-systemd-system-logs))

;; 3. sprite
(defun daemons-dash-sprite-list ()
  "Fetch subordinate Emacs daemons from `sprite.el'."
  (when (require 'sprite nil t)
    (let ((sprites (ignore-errors (sprite-resolve-list)))
          (items nil))
      (dolist (s sprites)
        (let* ((name (sprite-name s))
               (running (ignore-errors (sprite--running-p name)))
               (status (if running 'active 'inactive))
               (start-time (sprite-start-time s))
               (uptime (if (and start-time (fboundp 'sprite--format-uptime))
                           (sprite--format-uptime (float-time (time-since start-time)))
                         "unknown"))
               (details (format "running %s, uptime %s" (if running "yes" "no") uptime)))
          (push (daemons-dash-item--make
                 :id (format "sprite:%s" name)
                 :name name
                 :provider 'sprite
                 :status status
                 :details details
                 :config-status 'untracked
                 :raw-data s)
                items)))
      (nreverse items))))

(defun daemons-dash-sprite-stop (id)
  (let ((name (cadr (split-string id ":"))))
    (when (fboundp 'sprite-stop)
      (sprite-stop name))))

(defun daemons-dash-sprite-restart (id)
  (let ((name (cadr (split-string id ":"))))
    (when (fboundp 'sprite-restart)
      (sprite-restart name))))

(defun daemons-dash-sprite-logs (id)
  (let ((name (cadr (split-string id ":"))))
    (cond
     ((fboundp 'sprite-open-log)
      (sprite-open-log name))
     ((and (fboundp 'sprite--log-buffer-name)
           (get-buffer (sprite--log-buffer-name name)))
      (pop-to-buffer (sprite--log-buffer-name name)))
     (t (message "No log buffer available for sprite %s" name)))))

(daemons-dash-register-provider
 (daemons-dash-provider--make
  :name 'sprite
  :label "sprite"
  :list-fn #'daemons-dash-sprite-list
  :start-fn #'ignore
  :stop-fn #'daemons-dash-sprite-stop
  :restart-fn #'daemons-dash-sprite-restart
  :enable-fn #'ignore
  :disable-fn #'ignore
  :logs-fn #'daemons-dash-sprite-logs
  :inspect-fn #'daemons-dash-sprite-logs))

;; 4. docker
(defun daemons-dash-docker-list ()
  "Fetch Docker containers via CLI or docker.el."
  (if (executable-find "docker")
      (condition-case err
          (let ((output (daemons-dash--run-command
                         '("docker" "container" "ls" "-a" "--format" "{{.ID}}\t{{.Names}}\t{{.Status}}\t{{.Ports}}")))
                (items nil))
            (dolist (line (split-string output "\n" t))
              (let ((parts (split-string line "\t" t)))
                (when (>= (length parts) 3)
                  (let* ((cid (nth 0 parts))
                         (cname (nth 1 parts))
                         (cstatus (nth 2 parts))
                         (cports (or (nth 3 parts) ""))
                         (status (if (string-prefix-p "Up" cstatus) 'active 'inactive))
                         (details (format "%s %s" cstatus (if (string-empty-p cports) "" (concat "ports " cports)))))
                    (push (daemons-dash-item--make
                           :id (format "docker:%s" cname)
                           :name cname
                           :provider 'docker
                           :status status
                           :details details
                           :config-status 'untracked
                           :raw-data (list :id cid :name cname :status cstatus :ports cports))
                          items)))))
            (nreverse items))
        (error
         (list (daemons-dash-item--make
                :id "docker:inactive"
                :name "docker"
                :provider 'docker
                :status 'inactive
                :details (format "Docker daemon inactive (%s)" (error-message-string err))
                :config-status 'untracked))))
    (list (daemons-dash-item--make
           :id "docker:unavailable"
           :name "docker"
           :provider 'docker
           :status 'inactive
           :details "docker executable not found"
           :config-status 'untracked))))

(defun daemons-dash-docker-start (id)
  (let ((cname (cadr (split-string id ":"))))
    (daemons-dash--run-command (list "docker" "start" cname))))

(defun daemons-dash-docker-stop (id)
  (let ((cname (cadr (split-string id ":"))))
    (daemons-dash--run-command (list "docker" "stop" cname))))

(defun daemons-dash-docker-restart (id)
  (let ((cname (cadr (split-string id ":"))))
    (daemons-dash--run-command (list "docker" "restart" cname))))

(defun daemons-dash-docker-logs (id)
  (let ((cname (cadr (split-string id ":"))))
    (let ((buf (get-buffer-create (format "*docker-logs:%s*" cname))))
      (pop-to-buffer buf)
      (compilation-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (call-process "docker" nil t nil "logs" "--tail" "200" cname)))))

(daemons-dash-register-provider
 (daemons-dash-provider--make
  :name 'docker
  :label "docker"
  :list-fn #'daemons-dash-docker-list
  :start-fn #'daemons-dash-docker-start
  :stop-fn #'daemons-dash-docker-stop
  :restart-fn #'daemons-dash-docker-restart
  :enable-fn #'ignore
  :disable-fn #'ignore
  :logs-fn #'daemons-dash-docker-logs
  :inspect-fn #'daemons-dash-docker-logs))

;; 5. ollama
(defun daemons-dash-ollama-list ()
  "Fetch active Ollama models via local HTTP API (`/api/ps`)."
  (let ((url (concat daemons-dash-ollama-host "/api/ps"))
        (url-request-method "GET")
        (url-show-status nil))
    (condition-case nil
        (with-current-buffer (url-retrieve-synchronously url t t daemons-dash-ollama-timeout)
          (goto-char (point-min))
          (if (re-search-forward "\n\n" nil t)
              (let* ((json-object-type 'plist)
                     (json-array-type 'list)
                     (data (json-read))
                     (models (plist-get data :models))
                     (items nil))
                (kill-buffer)
                (dolist (m models)
                  (let* ((name (or (plist-get m :name) (plist-get m :model)))
                         (size (or (plist-get m :size) 0))
                         (vram (or (plist-get m :size_vram) 0))
                         (vram-mb (/ vram (* 1024 1024)))
                         (details (format "VRAM %dMB size %dMB" vram-mb (/ size (* 1024 1024)))))
                    (push (daemons-dash-item--make
                           :id (format "ollama:%s" name)
                           :name name
                           :provider 'ollama
                           :status 'active
                           :details details
                           :config-status 'untracked
                           :raw-data m)
                          items)))
                (or (nreverse items)
                    (list (daemons-dash-item--make
                           :id "ollama:idle"
                           :name "ollama"
                           :provider 'ollama
                           :status 'inactive
                           :details "Ollama server running (no models loaded in VRAM)"
                           :config-status 'untracked))))
            (kill-buffer)
            (list (daemons-dash-item--make
                   :id "ollama:inactive"
                   :name "ollama"
                   :provider 'ollama
                   :status 'inactive
                   :details "Ollama server inactive"
                   :config-status 'untracked))))
      (error
       (list (daemons-dash-item--make
              :id "ollama:inactive"
              :name "ollama"
              :provider 'ollama
              :status 'inactive
              :details "Ollama server inactive"
              :config-status 'untracked))))))

(defun daemons-dash-ollama-logs (_id)
  (daemons-dash-systemd-system-logs "systemd-system:ollama.service"))

(daemons-dash-register-provider
 (daemons-dash-provider--make
  :name 'ollama
  :label "ollama"
  :list-fn #'daemons-dash-ollama-list
  :start-fn #'ignore
  :stop-fn #'ignore
  :restart-fn #'ignore
  :enable-fn #'ignore
  :disable-fn #'ignore
  :logs-fn #'daemons-dash-ollama-logs
  :inspect-fn #'daemons-dash-ollama-logs))

;;; Data Fetching & Aggregation

(declare-function daemons-dash-config-annotate-entries "daemons-dash-config" (entries))

(defun daemons-dash-fetch-all ()
  "Gather `daemons-dash-item' structs from all `daemons-dash-enabled-providers'.
Annotates entries with configuration status if `daemons-dash-config' is loaded."
  (let ((all-items nil))
    (dolist (provider-sym daemons-dash-enabled-providers)
      (when-let* ((provider (daemons-dash-get-provider provider-sym))
                  (fn (daemons-dash-provider-list-fn provider)))
        (setq all-items (append all-items (funcall fn)))))
    (if (fboundp 'daemons-dash-config-annotate-entries)
        (daemons-dash-config-annotate-entries all-items)
      all-items)))

;;; Filtering & Narrowing

(defun daemons-dash--item-visible-p (item)
  "Return non-nil if ITEM should be visible under current filters."
  (let ((provider (daemons-dash-item-provider item))
        (status (daemons-dash-item-status item)))
    (not (or (memq provider daemons-dash-hidden-providers)
             (memq status daemons-dash-hidden-states)
             (and (memq status '(active running))
                  (memq 'active daemons-dash-hidden-states))
             (and (memq status '(inactive stopped))
                  (memq 'inactive daemons-dash-hidden-states))))))

(defun daemons-dash-toggle-provider-filter (provider)
  "Toggle visibility of daemons from PROVIDER."
  (interactive
   (let ((providers (delete-dups
                     (append daemons-dash-enabled-providers
                             (hash-table-keys daemons-dash-providers)))))
     (list (intern (completing-read "Toggle provider: "
                                    (mapcar #'symbol-name providers)
                                    nil t)))))
  (if (memq provider daemons-dash-hidden-providers)
      (setq daemons-dash-hidden-providers (delq provider daemons-dash-hidden-providers))
    (push provider daemons-dash-hidden-providers))
  (daemons-dash-refresh))

(defun daemons-dash-toggle-state-filter (state)
  "Toggle visibility of daemons with STATE ('active, 'inactive, 'failed)."
  (interactive
   (list (intern (completing-read "Toggle state: "
                                  '("active" "inactive" "failed")
                                  nil t))))
  (if (memq state daemons-dash-hidden-states)
      (setq daemons-dash-hidden-states (delq state daemons-dash-hidden-states))
    (push state daemons-dash-hidden-states))
  (daemons-dash-refresh))

(defun daemons-dash-filter-reset ()
  "Reset all filters and display all daemons."
  (interactive)
  (setq daemons-dash-hidden-providers nil
        daemons-dash-hidden-states nil)
  (daemons-dash-refresh)
  (message "daemons-dash: all filters cleared"))

(defun daemons-dash-filter-toggle-systemd-user ()
  "Toggle visibility of systemd-user daemons."
  (interactive)
  (daemons-dash-toggle-provider-filter 'systemd-user))

(defun daemons-dash-filter-toggle-systemd-system ()
  "Toggle visibility of systemd-system daemons."
  (interactive)
  (daemons-dash-toggle-provider-filter 'systemd-system))

(defun daemons-dash-filter-toggle-sprite ()
  "Toggle visibility of sprite daemons."
  (interactive)
  (daemons-dash-toggle-provider-filter 'sprite))

(defun daemons-dash-filter-toggle-docker ()
  "Toggle visibility of docker daemons."
  (interactive)
  (daemons-dash-toggle-provider-filter 'docker))

(defun daemons-dash-filter-toggle-ollama ()
  "Toggle visibility of ollama daemons."
  (interactive)
  (daemons-dash-toggle-provider-filter 'ollama))

(defun daemons-dash-filter-toggle-active ()
  "Toggle visibility of active/running daemons."
  (interactive)
  (daemons-dash-toggle-state-filter 'active))

(defun daemons-dash-filter-toggle-inactive ()
  "Toggle visibility of inactive/stopped daemons."
  (interactive)
  (daemons-dash-toggle-state-filter 'inactive))

(defun daemons-dash-filter-toggle-failed ()
  "Toggle visibility of failed daemons."
  (interactive)
  (daemons-dash-toggle-state-filter 'failed))

;;; Tabulated List Rendering

(defun daemons-dash--format-status (status)
  "Format STATUS symbol/string into propertized text."
  (let ((str (symbol-name status)))
    (pcase status
      ('active (propertize str 'face 'daemons-dash-face-active))
      ('running (propertize str 'face 'daemons-dash-face-active))
      ('failed (propertize str 'face 'daemons-dash-face-failed))
      ('inactive (propertize str 'face 'daemons-dash-face-inactive))
      (_ (propertize str 'face 'daemons-dash-face-inactive)))))

(defun daemons-dash--format-config-status (item)
  "Format `daemons-dash-item' configuration status and expected state."
  (let ((cs (daemons-dash-item-config-status item))
        (expected (daemons-dash-item-expected-status item)))
    (pcase cs
      ('match (propertize "MATCH" 'face 'daemons-dash-face-match))
      ('desync (propertize (format "DESYNC (exp %s)" (or expected "active"))
                           'face 'daemons-dash-face-desync))
      (_ (propertize "UNTRACKED" 'face 'daemons-dash-face-untracked)))))

(defun daemons-dash--build-entry (item)
  "Convert `daemons-dash-item' ITEM into a `tabulated-list' entry."
  (list item
        (vector
         (propertize (daemons-dash-item-name item) 'face 'daemons-dash-face-name)
         (propertize (symbol-name (daemons-dash-item-provider item)) 'face 'daemons-dash-face-provider)
         (daemons-dash--format-status (daemons-dash-item-status item))
         (or (daemons-dash-item-details item) "")
         (daemons-dash--format-config-status item))))

;;; Tabulated List Mode & Keymaps

(defvar daemons-dash-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'daemons-dash-inspect)
    (define-key map (kbd "l")   #'daemons-dash-view-logs)
    (define-key map (kbd "s")   #'daemons-dash-start)
    (define-key map (kbd "k")   #'daemons-dash-stop)
    (define-key map (kbd "r")   #'daemons-dash-restart)
    (define-key map (kbd "e")   #'daemons-dash-enable)
    (define-key map (kbd "d")   #'daemons-dash-disable)
    (define-key map (kbd "f")   #'daemons-dash-filter-menu)
        (define-key map (kbd "v")   #'daemons-dash-verify-config)
    (define-key map (kbd "g")   #'daemons-dash-refresh)
    (define-key map (kbd "q")   #'quit-window)
    (define-key map (kbd "?")   #'daemons-dash-dispatch)
    (define-key map (kbd "m")   #'daemons-dash-dispatch)
    map)
  "Keymap for `daemons-dash-mode'.")

(define-derived-mode daemons-dash-mode tabulated-list-mode "daemons-dash"
  "Major mode for displaying system services and background daemons.

\\{daemons-dash-mode-map}"
  (make-local-variable 'daemons-dash-hidden-providers)
  (make-local-variable 'daemons-dash-hidden-states)
  (setq tabulated-list-format
        [("Name" 30 t)
         ("Provider" 15 t)
         ("Status" 12 t)
         ("Details" 35 t)
         ("Config" 25 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("Name" . nil))
  (tabulated-list-init-header))

;;; Interactive Actions

(defun daemons-dash--item-at-point ()
  "Return `daemons-dash-item' at point or signal user-error."
  (or (tabulated-list-get-id)
      (user-error "No daemon item at point")))

(defun daemons-dash-refresh ()
  "Refresh the daemons dashboard buffer."
  (interactive)
  (let* ((items (daemons-dash-fetch-all))
         (filtered (seq-filter #'daemons-dash--item-visible-p items)))
    (setq tabulated-list-entries (mapcar #'daemons-dash--build-entry filtered))
    (tabulated-list-print t)
    (let ((hidden-info nil))
      (when daemons-dash-hidden-providers
        (push (format "providers hidden: %s"
                      (mapconcat #'symbol-name daemons-dash-hidden-providers ","))
              hidden-info))
      (when daemons-dash-hidden-states
        (push (format "states hidden: %s"
                      (mapconcat #'symbol-name daemons-dash-hidden-states ","))
              hidden-info))
      (if hidden-info
          (message "daemons-dash: %d items shown (%d hidden: %s)"
                   (length filtered)
                   (- (length items) (length filtered))
                   (string-join (nreverse hidden-info) "; "))
        (message "daemons-dash: %d items" (length items))))))

(defun daemons-dash-start ()
  "Start the daemon at point."
  (interactive)
  (let* ((item (daemons-dash--item-at-point))
         (provider (daemons-dash-get-provider (daemons-dash-item-provider item))))
    (when-let* ((fn (daemons-dash-provider-start-fn provider)))
      (funcall fn (daemons-dash-item-id item))
      (message "Started %s (%s)" (daemons-dash-item-name item) (daemons-dash-item-provider item))
      (daemons-dash-refresh))))

(defun daemons-dash-stop ()
  "Stop the daemon at point."
  (interactive)
  (let* ((item (daemons-dash--item-at-point))
         (provider (daemons-dash-get-provider (daemons-dash-item-provider item))))
    (when-let* ((fn (daemons-dash-provider-stop-fn provider)))
      (funcall fn (daemons-dash-item-id item))
      (message "Stopped %s (%s)" (daemons-dash-item-name item) (daemons-dash-item-provider item))
      (daemons-dash-refresh))))

(defun daemons-dash-restart ()
  "Restart the daemon at point."
  (interactive)
  (let* ((item (daemons-dash--item-at-point))
         (provider (daemons-dash-get-provider (daemons-dash-item-provider item))))
    (when-let* ((fn (daemons-dash-provider-restart-fn provider)))
      (funcall fn (daemons-dash-item-id item))
      (message "Restarted %s (%s)" (daemons-dash-item-name item) (daemons-dash-item-provider item))
      (daemons-dash-refresh))))

(defun daemons-dash-enable ()
  "Enable the daemon at point."
  (interactive)
  (let* ((item (daemons-dash--item-at-point))
         (provider (daemons-dash-get-provider (daemons-dash-item-provider item))))
    (when-let* ((fn (daemons-dash-provider-enable-fn provider)))
      (funcall fn (daemons-dash-item-id item))
      (message "Enabled %s (%s)" (daemons-dash-item-name item) (daemons-dash-item-provider item))
      (daemons-dash-refresh))))

(defun daemons-dash-disable ()
  "Disable the daemon at point."
  (interactive)
  (let* ((item (daemons-dash--item-at-point))
         (provider (daemons-dash-get-provider (daemons-dash-item-provider item))))
    (when-let* ((fn (daemons-dash-provider-disable-fn provider)))
      (funcall fn (daemons-dash-item-id item))
      (message "Disabled %s (%s)" (daemons-dash-item-name item) (daemons-dash-item-provider item))
      (daemons-dash-refresh))))

(defun daemons-dash-view-logs ()
  "View logs for the daemon at point."
  (interactive)
  (let* ((item (daemons-dash--item-at-point))
         (provider (daemons-dash-get-provider (daemons-dash-item-provider item))))
    (if-let* ((fn (daemons-dash-provider-logs-fn provider)))
        (funcall fn (daemons-dash-item-id item))
      (message "No log handler available for provider %s" (daemons-dash-item-provider item)))))

(defun daemons-dash-inspect ()
  "Inspect details for the daemon at point."
  (interactive)
  (let* ((item (daemons-dash--item-at-point))
         (provider (daemons-dash-get-provider (daemons-dash-item-provider item))))
    (if-let* ((fn (daemons-dash-provider-inspect-fn provider)))
        (funcall fn (daemons-dash-item-id item))
      (daemons-dash-view-logs))))

(defun daemons-dash-verify-config ()
  "Trigger declarative config verification and refresh dashboard."
  (interactive)
  (require 'daemons-dash-config nil t)
  (daemons-dash-refresh)
  (message "daemons-dash: declarative config verification updated"))

;;; Transient Menus

(transient-define-prefix daemons-dash-filter-menu ()
  "Transient menu for filtering and narrowing the daemons dashboard."
  :info-manual "(daemons-dash) Filtering"
  ["Filter by Provider"
   ("pu" daemons-dash-filter-toggle-systemd-user
    :description (lambda () (format "systemd-user     [%s]"
                                    (if (memq 'systemd-user daemons-dash-hidden-providers) "HIDDEN" "SHOWN")))
    :transient t)
   ("ps" daemons-dash-filter-toggle-systemd-system
    :description (lambda () (format "systemd-system   [%s]"
                                    (if (memq 'systemd-system daemons-dash-hidden-providers) "HIDDEN" "SHOWN")))
    :transient t)
   ("pe" daemons-dash-filter-toggle-sprite
    :description (lambda () (format "sprite           [%s]"
                                    (if (memq 'sprite daemons-dash-hidden-providers) "HIDDEN" "SHOWN")))
    :transient t)
   ("pd" daemons-dash-filter-toggle-docker
    :description (lambda () (format "docker           [%s]"
                                    (if (memq 'docker daemons-dash-hidden-providers) "HIDDEN" "SHOWN")))
    :transient t)
   ("po" daemons-dash-filter-toggle-ollama
    :description (lambda () (format "ollama           [%s]"
                                    (if (memq 'ollama daemons-dash-hidden-providers) "HIDDEN" "SHOWN")))
    :transient t)
   ("pp" "Choose provider..." daemons-dash-toggle-provider-filter :transient t)]
  ["Filter by State"
   ("sa" daemons-dash-filter-toggle-active
    :description (lambda () (format "active / running [%s]"
                                    (if (memq 'active daemons-dash-hidden-states) "HIDDEN" "SHOWN")))
    :transient t)
   ("si" daemons-dash-filter-toggle-inactive
    :description (lambda () (format "inactive / stop  [%s]"
                                    (if (memq 'inactive daemons-dash-hidden-states) "HIDDEN" "SHOWN")))
    :transient t)
   ("sf" daemons-dash-filter-toggle-failed
    :description (lambda () (format "failed           [%s]"
                                    (if (memq 'failed daemons-dash-hidden-states) "HIDDEN" "SHOWN")))
    :transient t)
   ("ss" "Choose state..." daemons-dash-toggle-state-filter :transient t)]
  ["Filter Controls"
   ("ra" "Reset all filters (show all)" daemons-dash-filter-reset :transient t)
   ("g"  "Refresh view"                 daemons-dash-refresh :transient t)
   ("q"  "Quit filter menu"             transient-quit-one)])

(transient-define-prefix daemons-dash-dispatch ()
  "Transient menu for daemons-dash operations."
  [["Actions"
    ("s" "Start daemon"      daemons-dash-start)
    ("k" "Stop daemon"       daemons-dash-stop)
    ("r" "Restart daemon"    daemons-dash-restart)
    ("e" "Enable daemon"     daemons-dash-enable)
    ("d" "Disable daemon"    daemons-dash-disable)]
   ["Inspection & Config"
    ("l" "View logs"         daemons-dash-view-logs)
    ("in" "Inspect details" daemons-dash-inspect)
    ("v" "Verify config"     daemons-dash-verify-config)
    ("g" "Refresh"           daemons-dash-refresh)]
   ["Filters & Narrowing"
    ("f"  "Filter menu..."   daemons-dash-filter-menu)
    ("fa" "Toggle active"    daemons-dash-filter-toggle-active :transient t)
    ("fi" "Toggle inactive"  daemons-dash-filter-toggle-inactive :transient t)
    ("ff" "Toggle failed"    daemons-dash-filter-toggle-failed :transient t)
    ("fr" "Reset filters"    daemons-dash-filter-reset :transient t)]
   ["Other"
    ("q" "Quit"              quit-window)]])

;;; Autoloaded Main Entrypoint

;;;###autoload
(defun daemons-dash ()
  "Open the unified system service and daemon dashboard buffer."
  (interactive)
  (let ((buf (get-buffer-create "*daemons-dash*")))
    (with-current-buffer buf
      (daemons-dash-mode)
      (daemons-dash-refresh))
    (pop-to-buffer buf)))

(provide 'daemons-dash)
;;; daemons-dash.el ends here
