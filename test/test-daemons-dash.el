;;; test-daemons-dash.el --- Tests for daemons-dash -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; ERT unit tests for daemons-dash and daemons-dash-config.

;;; Code:

(require 'ert)
(require 'map)
(load (expand-file-name "daemons-dash"
                        (expand-file-name "../lisp" (file-name-directory (or load-file-name buffer-file-name)))))
(load (expand-file-name "daemons-dash-config"
                        (expand-file-name "../lisp" (file-name-directory (or load-file-name buffer-file-name)))))

;;; Provider Registry Tests

(ert-deftest daemons-dash-test-provider-registry ()
  "Registering a provider struct makes it retrievable by symbol."
  (let ((daemons-dash-providers (make-hash-table :test #'equal))
        (p (daemons-dash-provider--make :name 'test-provider :label "Test")))
    (daemons-dash-register-provider p)
    (should (eq (daemons-dash-get-provider 'test-provider) p))))

(ert-deftest daemons-dash-test-item-struct ()
  "Constructing daemons-dash-item yields correct fields."
  (let ((item (daemons-dash-item--make
               :id "systemd-user:odem.service"
               :name "odem.service"
               :provider 'systemd-user
               :status 'active
               :details "PID 123"
               :config-status 'untracked)))
    (should (equal (daemons-dash-item-id item) "systemd-user:odem.service"))
    (should (equal (daemons-dash-item-name item) "odem.service"))
    (should (eq (daemons-dash-item-provider item) 'systemd-user))
    (should (eq (daemons-dash-item-status item) 'active))
    (should (eq (daemons-dash-item-config-status item) 'untracked))))

;;; Systemd Filtering Predicate Tests

(ert-deftest daemons-dash-test-systemd-show-matching ()
  "Test systemd service inclusion/exclusion logic."
  (let ((daemons-dash-systemd-show-all-excluding '("bad.service"))
        (daemons-dash-systemd-show-only nil))
    (should (daemons-dash-user-enabled-service-p "odem.service"))
    (should-not (daemons-dash-user-enabled-service-p "bad.service"))
    (should-not (daemons-dash-user-enabled-service-p "sys-devices-foo.device"))))

(ert-deftest daemons-dash-test-systemd-show-only-override ()
  "Test show-only restriction overrides default matching."
  (let ((daemons-dash-systemd-show-only '("specific.service"))
        (daemons-dash-systemd-show-all-excluding nil))
    (should (daemons-dash-user-enabled-service-p "specific.service"))
    (should-not (daemons-dash-user-enabled-service-p "other.service"))))

;;; Elisp Configuration & Matching Verification Tests

(ert-deftest daemons-dash-test-config-registration ()
  "Registering daemon declarations populates daemons-dash-config-registry."
  (let ((daemons-dash-config-registry (make-hash-table :test #'equal)))
    (daemons-dash-define-daemon "test-daemon.service"
      :provider systemd-user
      :expected-status active
      :doc "Test daemon service")
    (let ((spec (map-elt daemons-dash-config-registry "test-daemon.service")))
      (should spec)
      (should (equal (plist-get spec :name) "test-daemon.service"))
      (should (eq (plist-get spec :provider) 'systemd-user))
      (should (eq (plist-get spec :expected-status) 'active))
      (should (equal (plist-get spec :doc) "Test daemon service")))))

(ert-deftest daemons-dash-test-config-verify-match ()
  "Verifying a live active item against active spec sets status to match."
  (let ((daemons-dash-config-registry (make-hash-table :test #'equal))
        (item (daemons-dash-item--make
               :id "systemd-user:odem.service"
               :name "odem.service"
               :provider 'systemd-user
               :status 'active)))
    (daemons-dash-register-service "odem.service" :expected-status 'active)
    (daemons-dash-config-verify item)
    (should (eq (daemons-dash-item-config-status item) 'match))))

(ert-deftest daemons-dash-test-config-verify-desync ()
  "Verifying a live failed item against expected active spec sets status to desync."
  (let ((daemons-dash-config-registry (make-hash-table :test #'equal))
        (item (daemons-dash-item--make
               :id "systemd-user:dunst.service"
               :name "dunst.service"
               :provider 'systemd-user
               :status 'failed)))
    (daemons-dash-register-service "dunst.service" :expected-status 'active)
    (daemons-dash-config-verify item)
    (should (eq (daemons-dash-item-config-status item) 'desync))
    (should (eq (daemons-dash-item-expected-status item) 'active))))

(ert-deftest daemons-dash-test-config-verify-untracked ()
  "Unregistered items are annotated as untracked."
  (let ((daemons-dash-config-registry (make-hash-table :test #'equal))
        (item (daemons-dash-item--make
               :id "systemd-user:unknown.service"
               :name "unknown.service"
               :provider 'systemd-user
               :status 'active)))
    (daemons-dash-config-verify item)
    (should (eq (daemons-dash-item-config-status item) 'untracked))
    (should (null (daemons-dash-item-expected-status item)))))

;;; YAML Translation Layer Tests

(ert-deftest daemons-dash-test-yaml-export-import ()
  "Exporting declarations to YAML and re-importing restores registry entries."
  (let ((daemons-dash-config-registry (make-hash-table :test #'equal)))
    (daemons-dash-register-service "odem.service" :provider 'systemd-user :expected-status 'active :doc "odem bot")
    (daemons-dash-register-service "postgres-dev" :provider 'docker :expected-status 'running :doc "dev db")
    (let ((yaml-text (daemons-dash-config-export-yaml)))
      (should (string-match-p "odem.service" yaml-text))
      (should (string-match-p "postgres-dev" yaml-text))
      ;; Clear and import
      (setq daemons-dash-config-registry (make-hash-table :test #'equal))
      (daemons-dash-config-import-yaml yaml-text)
      (should (map-elt daemons-dash-config-registry "odem.service"))
      (should (map-elt daemons-dash-config-registry "postgres-dev")))))

;;; Provider Fallback Tests

(ert-deftest daemons-dash-test-ollama-inactive-fallback ()
  "Ollama provider handles connection error gracefully."
  (let ((daemons-dash-ollama-host "http://127.0.0.1:59999") ; dead port
        (daemons-dash-ollama-timeout 1))
    (let ((items (daemons-dash-ollama-list)))
      (should (= (length items) 1))
      (let ((item (car items)))
        (should (eq (daemons-dash-item-provider item) 'ollama))
        (should (eq (daemons-dash-item-status item) 'inactive))
        (should (string-match-p "inactive" (daemons-dash-item-details item)))))))

(provide 'test-daemons-dash)
;;; test-daemons-dash.el ends here
