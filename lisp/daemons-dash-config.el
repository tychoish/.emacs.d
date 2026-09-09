;;; daemons-dash-config.el --- Elisp Daemon Registry & Configuration Engine -*- lexical-binding: t; -*-

;; Author: Tycho Garen
;; Keywords: convenience, processes, services, configuration, systemd, yaml
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; Declarative Elisp daemon registration, configuration, state verification,
;; and YAML translation layer for `daemons-dash`.
;;
;; Defines native Elisp registration macros (`daemons-dash-define-daemon`,
;; `daemons-dash-register-service`) to declare expected daemon states, startup
;; options, and provider specs directly in Elisp.
;;
;; Compares live dashboard entries against registered Elisp specifications
;; (annotating rows with `MATCH` or `DESYNC`) and provides YAML export/import
;; functions for external interoperability.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'map)
(require 'subr-x)
(require 'daemons-dash)

;;; Registry Storage

(defvar daemons-dash-config-registry (make-hash-table :test #'equal)
  "Hash table mapping daemon/service name string or ID to configuration plist.")

(defun daemons-dash-config-clear ()
  "Clear all registered daemon declarations from `daemons-dash-config-registry'."
  (clrhash daemons-dash-config-registry))

;;; Service Registration Functions & Macros

(defun daemons-dash-register-service (name &rest props)
  "Register daemon declaration for NAME with PROPERTIES plist.
NAME can be a string or symbol (e.g. \"odem.service\" or 'odem.service).
PROPS is a key-value plist supported keys:
  :provider        Symbol ('systemd-user, 'systemd-system, 'docker, 'sprite, 'ollama)
  :expected-status Symbol ('active, 'running, 'inactive, 'stopped) - default 'active
  :doc             String documentation
  :auto-restart    Boolean
  :ports           List of port strings/numbers
  :command         String exec command
  :unit            String unit file name (defaults to NAME if systemd)"
  (let* ((name-str (if (symbolp name) (symbol-name name) name))
         (plist (if (and (= (length props) 1) (listp (car props)))
                    (car props)
                  props))
         (provider (or (plist-get plist :provider) 'systemd-user))
         (expected (or (plist-get plist :expected-status) 'active))
         (unit (or (plist-get plist :unit) name-str))
         (entry (list :name name-str
                      :provider provider
                      :expected-status expected
                      :unit unit
                      :doc (plist-get plist :doc)
                      :auto-restart (plist-get plist :auto-restart)
                      :ports (plist-get plist :ports)
                      :command (plist-get plist :command)
                      :raw-props plist)))
    (setf (map-elt daemons-dash-config-registry name-str) entry)
    (setf (map-elt daemons-dash-config-registry unit) entry)
    entry))

(defmacro daemons-dash-define-daemon (name &rest props)
  "Declaratively define a daemon configuration in Elisp.
NAME is an unquoted symbol or string naming the daemon/service.
PROPS are key-value properties passed to `daemons-dash-register-service'.

Example:
  (daemons-dash-define-daemon odem.service
    :provider systemd-user
    :expected-status active
    :doc \"odem Telegram bot service\")"
  (declare (indent 1))
  (let ((name-val (if (symbolp name) (symbol-name name) name))
        (quoted-props nil))
    (while props
      (let ((key (pop props))
            (val (pop props)))
        (push key quoted-props)
        (push (if (and (symbolp val) (not (booleanp val)) (not (keywordp val)))
                  `',val
                val)
              quoted-props)))
    `(daemons-dash-register-service ,name-val ,@(nreverse quoted-props))))

;;; Declarative State Verification

(defun daemons-dash-config-verify (item)
  "Verify `daemons-dash-item' ITEM against `daemons-dash-config-registry'.
Sets `daemons-dash-item-config-status' to 'match, 'desync, or 'untracked.
Returns the updated ITEM struct."
  (let* ((name (daemons-dash-item-name item))
         (id (daemons-dash-item-id item))
         (live-status (daemons-dash-item-status item))
         (spec (or (map-elt daemons-dash-config-registry name)
                   (map-elt daemons-dash-config-registry id))))
    (if spec
        (let ((expected (plist-get spec :expected-status)))
          (setf (daemons-dash-item-expected-status item) expected)
          (if (daemons-dash-config--status-matches-p live-status expected)
              (setf (daemons-dash-item-config-status item) 'match)
            (setf (daemons-dash-item-config-status item) 'desync)))
      (setf (daemons-dash-item-config-status item) 'untracked)
      (setf (daemons-dash-item-expected-status item) nil))
    item))

(defun daemons-dash-config--status-matches-p (live expected)
  "Return non-nil if LIVE status matches EXPECTED status symbol."
  (cond
   ((eq live expected) t)
   ((and (memq live '(active running)) (memq expected '(active running))) t)
   ((and (memq live '(inactive stopped)) (memq expected '(inactive stopped))) t)
   (t nil)))

(defun daemons-dash-config-annotate-entries (items)
  "Annotate list of `daemons-dash-item' structs in ITEMS with configuration status."
  (mapcar #'daemons-dash-config-verify items))

;;; YAML Export & Import Translation Layer

(defun daemons-dash-config--spec-to-alist (spec)
  "Convert a daemon configuration SPEC plist into a YAML-encodeable alist."
  (let ((alist nil))
    (push (cons 'name (plist-get spec :name)) alist)
    (push (cons 'provider (symbol-name (plist-get spec :provider))) alist)
    (push (cons 'expected_status (symbol-name (plist-get spec :expected-status))) alist)
    (when-let* ((doc (plist-get spec :doc)))
      (push (cons 'doc doc) alist))
    (when (plist-get spec :auto-restart)
      (push (cons 'auto_restart t) alist))
    (when-let* ((ports (plist-get spec :ports)))
      (push (cons 'ports (if (listp ports) (vconcat ports) ports)) alist))
    (when-let* ((cmd (plist-get spec :command)))
      (push (cons 'command cmd) alist))
    (nreverse alist)))

(defun daemons-dash-config-export-yaml (&optional file)
  "Export all registered daemon declarations to a YAML string or FILE.
Encodes Lisp data structures to YAML using `yaml-encode'.
Returns the YAML string."
  (let ((services nil)
        (seen-names (make-hash-table :test #'equal)))
    (maphash
     (lambda (_key spec)
       (let ((name (plist-get spec :name)))
         (unless (gethash name seen-names)
           (puthash name t seen-names)
           (push (daemons-dash-config--spec-to-alist spec) services))))
     daemons-dash-config-registry)
    (let* ((services-vec (vconcat (nreverse services)))
           (root `((daemons . ((services . ,services-vec)))))
           (yaml-str (if (fboundp 'yaml-encode)
                         (yaml-encode root)
                       (error "yaml-encode is not available"))))
      (when file
        (with-temp-file file
          (insert yaml-str)))
      yaml-str)))

(defun daemons-dash-config-import-yaml (source)
  "Import daemon declarations from SOURCE (file path or YAML string).
Populates `daemons-dash-config-registry' with parsed declarations."
  (let ((content (if (and (stringp source) (file-exists-p source))
                     (with-temp-buffer
                       (insert-file-contents source)
                       (buffer-string))
                   source)))
    (if (fboundp 'yaml-parse-string)
        (let* ((parsed (yaml-parse-string content :object-type 'plist :sequence-type 'list))
               (daemons (plist-get parsed :daemons))
               (services (plist-get daemons :services)))
          (dolist (s services)
            (let ((name (plist-get s :name))
                  (provider (intern (or (plist-get s :provider) "systemd-user")))
                  (expected (intern (or (plist-get s :expected_status) "active")))
                  (doc (plist-get s :doc))
                  (auto-restart (plist-get s :auto_restart))
                  (ports (plist-get s :ports))
                  (command (plist-get s :command)))
              (when name
                (daemons-dash-register-service
                 name
                 :provider provider
                 :expected-status expected
                 :doc doc
                 :auto-restart auto-restart
                 :ports ports
                 :command command)))))
      ;; Fallback simple regexp parser for YAML services if yaml-parse-string absent
      (daemons-dash-config--import-yaml-fallback content))))

(defun daemons-dash-config--import-yaml-fallback (yaml-str)
  "Fallback regexp YAML parser for daemon services when `yaml-parse-string' is unavailable."
  (let ((lines (split-string yaml-str "\n"))
        (current-name nil)
        (current-provider 'systemd-user)
        (current-expected 'active)
        (current-doc nil))
    (dolist (line lines)
      (cond
       ((string-match "^\\s-+- name:\\s-*\"?\\([^\"]+\\)\"?" line)
        (when current-name
          (daemons-dash-register-service current-name :provider current-provider :expected-status current-expected :doc current-doc))
        (setq current-name (match-string 1 line))
        (setq current-provider 'systemd-user)
        (setq current-expected 'active)
        (setq current-doc nil))
       ((string-match "^\\s-+provider:\\s-*\\([a-zA-Z0-9-]+\\)" line)
        (setq current-provider (intern (match-string 1 line))))
       ((string-match "^\\s-+expected_status:\\s-*\\([a-zA-Z0-9-]+\\)" line)
        (setq current-expected (intern (match-string 1 line))))
       ((string-match "^\\s-+doc:\\s-*\"?\\([^\"]+\\)\"?" line)
        (setq current-doc (match-string 1 line)))))
    (when current-name
      (daemons-dash-register-service current-name :provider current-provider :expected-status current-expected :doc current-doc))))

(provide 'daemons-dash-config)
;;; daemons-dash-config.el ends here
