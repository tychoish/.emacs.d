;;; test-daemons-dash.el --- Tests for daemons-dash -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Comprehensive ERT unit tests for daemons-dash and daemons-dash-config.

;;; Code:

(require 'ert)
(require 'map)
(require 'cl-lib)
(load (expand-file-name "daemons-dash"
                        (expand-file-name "../lisp" (file-name-directory (or load-file-name buffer-file-name)))))
(load (expand-file-name "daemons-dash-config"
                        (expand-file-name "../lisp" (file-name-directory (or load-file-name buffer-file-name)))))

;;; 1. Provider Registry & Structs

(ert-deftest daemons-dash-test-provider-registry ()
  "Registering a provider struct makes it retrievable by symbol."
  (let ((daemons-dash-providers (make-hash-table :test #'equal))
        (p (daemons-dash-provider--make :name 'test-provider :label "Test")))
    (daemons-dash-register-provider p)
    (should (eq (daemons-dash-get-provider 'test-provider) p))
    (should-error (daemons-dash-register-provider 'not-a-struct))))

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
    (should (equal (daemons-dash-item-details item) "PID 123"))
    (should (eq (daemons-dash-item-config-status item) 'untracked))))

;;; 2. Helper Utilities

(ert-deftest daemons-dash-test-id-target ()
  "Test extraction of target name from provider:target id format."
  (should (equal (daemons-dash--id-target "systemd-user:odem.service") "odem.service"))
  (should (equal (daemons-dash--id-target "docker:my-container") "my-container"))
  (should (equal (daemons-dash--id-target "ollama:llama3:latest") "llama3:latest"))
  (should (equal (daemons-dash--id-target "no-colon-identifier") "no-colon-identifier")))

(ert-deftest daemons-dash-test-run-command ()
  "Test synchronous command execution helper."
  (let ((output (daemons-dash--run-command '("sh" "-c" "echo daemons-dash-test"))))
    (should (string-match-p "daemons-dash-test" output))))

(ert-deftest daemons-dash-test-systemd-parse-lines ()
  "Test parsing systemd list-units output lines."
  (let* ((line-bullet "● failed.service loaded failed failed Failed Service")
         (line-active " active.service loaded active running Active Service")
         (line-dead   " inactive.service loaded inactive dead Dead Service")
         (line-short  " short line")
         (item-bullet (daemons-dash--parse-systemd-line line-bullet 'systemd-user))
         (item-active (daemons-dash--parse-systemd-line line-active 'systemd-system))
         (item-dead   (daemons-dash--parse-systemd-line line-dead 'systemd-user))
         (item-short  (daemons-dash--parse-systemd-line line-short 'systemd-user)))
    (should (equal (daemons-dash-item-name item-bullet) "failed.service"))
    (should (eq (daemons-dash-item-status item-bullet) 'failed))
    (should (eq (daemons-dash-item-provider item-bullet) 'systemd-user))

    (should (equal (daemons-dash-item-name item-active) "active.service"))
    (should (eq (daemons-dash-item-status item-active) 'active))
    (should (eq (daemons-dash-item-provider item-active) 'systemd-system))

    (should (equal (daemons-dash-item-name item-dead) "inactive.service"))
    (should (eq (daemons-dash-item-status item-dead) 'inactive))

    (should (null item-short))

    ;; Multiple lines via parse-systemd-units
    (let ((items (daemons-dash--parse-systemd-units
                  (format "%s\n%s\n%s\n" line-bullet line-active line-dead)
                  'systemd-user)))
      (should (= (length items) 3)))))

;;; 3. Systemd Filtering Predicates

(ert-deftest daemons-dash-test-systemd-show-matching ()
  "Test systemd service inclusion/exclusion logic."
  (let ((daemons-dash-systemd-show-all-excluding '("bad.service"))
        (daemons-dash-systemd-show-only nil))
    (should (daemons-dash-user-enabled-service-p "odem.service"))
    (should-not (daemons-dash-user-enabled-service-p "bad.service"))
    (should-not (daemons-dash-user-enabled-service-p "sys-devices-foo.device"))
    ;; Plist input
    (should (daemons-dash-user-enabled-service-p '(:name "custom.service" :unit-file-state "enabled")))
    (should-not (daemons-dash-user-enabled-service-p '(:name "bad.service" :unit-file-state "enabled")))))

(ert-deftest daemons-dash-test-systemd-show-only-override ()
  "Test show-only restriction overrides default matching."
  (let ((daemons-dash-systemd-show-only '("specific.service"))
        (daemons-dash-systemd-show-all-excluding nil))
    (should (daemons-dash-user-enabled-service-p "specific.service"))
    (should-not (daemons-dash-user-enabled-service-p "other.service"))))

;;; 4. Formatters and Rendering

(ert-deftest daemons-dash-test-format-status ()
  "Test formatting of various status symbols and string inputs."
  (let ((active-res (daemons-dash--format-status 'active))
        (running-res (daemons-dash--format-status 'running))
        (failed-res (daemons-dash--format-status 'failed))
        (inactive-res (daemons-dash--format-status 'inactive))
        (other-res (daemons-dash--format-status 'unknown)))
    (should (equal (get-text-property 0 'face active-res) 'daemons-dash-face-active))
    (should (equal (get-text-property 0 'face running-res) 'daemons-dash-face-active))
    (should (equal (get-text-property 0 'face failed-res) 'daemons-dash-face-failed))
    (should (equal (get-text-property 0 'face inactive-res) 'daemons-dash-face-inactive))
    (should (equal (get-text-property 0 'face other-res) 'daemons-dash-face-inactive))))

(ert-deftest daemons-dash-test-format-config-status ()
  "Test formatting of configuration status annotations."
  (let ((match-item (daemons-dash-item--make :config-status 'match))
        (desync-item (daemons-dash-item--make :config-status 'desync :expected-status 'active))
        (untracked-item (daemons-dash-item--make :config-status 'untracked)))
    (should (string-match-p "MATCH" (daemons-dash--format-config-status match-item)))
    (should (string-match-p "DESYNC" (daemons-dash--format-config-status desync-item)))
    (should (string-match-p "exp active" (daemons-dash--format-config-status desync-item)))
    (should (string-match-p "UNTRACKED" (daemons-dash--format-config-status untracked-item)))))

(ert-deftest daemons-dash-test-build-entry ()
  "Test converting daemons-dash-item into tabulated-list row vector."
  (let* ((item (daemons-dash-item--make
                :id "systemd-user:test.service"
                :name "test.service"
                :provider 'systemd-user
                :status 'active
                :details "running ok"
                :config-status 'match))
         (entry (daemons-dash--build-entry item)))
    (should (eq (car entry) item))
    (let ((vec (cadr entry)))
      (should (vectorp vec))
      (should (= (length vec) 5))
      (should (string-match-p "test.service" (aref vec 0)))
      (should (string-match-p "systemd-user" (aref vec 1)))
      (should (string-match-p "active" (aref vec 2)))
      (should (equal (aref vec 3) "running ok"))
      (should (string-match-p "MATCH" (aref vec 4))))))

;;; 5. Filtering & Narrowing Tests

(ert-deftest daemons-dash-test-filtering-providers ()
  "Disabling/hiding a provider filters its items out of the visible list."
  (let ((item-user (daemons-dash-item--make :id "systemd-user:a" :name "a" :provider 'systemd-user :status 'active))
        (item-sys (daemons-dash-item--make :id "systemd-system:b" :name "b" :provider 'systemd-system :status 'active))
        (item-docker (daemons-dash-item--make :id "docker:c" :name "c" :provider 'docker :status 'active))
        (daemons-dash-hidden-providers nil)
        (daemons-dash-hidden-states nil))
    (should (daemons-dash--item-visible-p item-user))
    (should (daemons-dash--item-visible-p item-sys))
    (should (daemons-dash--item-visible-p item-docker))

    (setq daemons-dash-hidden-providers '(systemd-user docker))
    (should-not (daemons-dash--item-visible-p item-user))
    (should (daemons-dash--item-visible-p item-sys))
    (should-not (daemons-dash--item-visible-p item-docker))))

(ert-deftest daemons-dash-test-filtering-states ()
  "Disabling/hiding a state filters items in that state out of the visible list."
  (let ((item-active (daemons-dash-item--make :id "systemd-user:a" :name "a" :provider 'systemd-user :status 'active))
        (item-running (daemons-dash-item--make :id "docker:b" :name "b" :provider 'docker :status 'running))
        (item-inactive (daemons-dash-item--make :id "systemd-user:c" :name "c" :provider 'systemd-user :status 'inactive))
        (item-failed (daemons-dash-item--make :id "systemd-user:d" :name "d" :provider 'systemd-user :status 'failed))
        (daemons-dash-hidden-providers nil)
        (daemons-dash-hidden-states nil))
    ;; Hide inactive
    (setq daemons-dash-hidden-states '(inactive))
    (should (daemons-dash--item-visible-p item-active))
    (should (daemons-dash--item-visible-p item-running))
    (should-not (daemons-dash--item-visible-p item-inactive))
    (should (daemons-dash--item-visible-p item-failed))

    ;; Hide active (also matches running)
    (setq daemons-dash-hidden-states '(active))
    (should-not (daemons-dash--item-visible-p item-active))
    (should-not (daemons-dash--item-visible-p item-running))
    (should (daemons-dash--item-visible-p item-inactive))
    (should (daemons-dash--item-visible-p item-failed))

    ;; Hide failed
    (setq daemons-dash-hidden-states '(failed))
    (should (daemons-dash--item-visible-p item-active))
    (should-not (daemons-dash--item-visible-p item-failed))))

(ert-deftest daemons-dash-test-filter-toggles-and-reset ()
  "Test filter toggle functions and filter reset."
  (with-temp-buffer
    (daemons-dash-mode)
    (setq-local daemons-dash-hidden-providers nil)
    (setq-local daemons-dash-hidden-states nil)
    ;; Provider toggles
    (daemons-dash-filter-toggle-systemd-user)
    (should (memq 'systemd-user daemons-dash-hidden-providers))
    (daemons-dash-filter-toggle-systemd-user)
    (should-not (memq 'systemd-user daemons-dash-hidden-providers))

    (daemons-dash-filter-toggle-systemd-system)
    (should (memq 'systemd-system daemons-dash-hidden-providers))

    (daemons-dash-filter-toggle-docker)
    (should (memq 'docker daemons-dash-hidden-providers))

    (daemons-dash-filter-toggle-sprite)
    (should (memq 'sprite daemons-dash-hidden-providers))

    (daemons-dash-filter-toggle-ollama)
    (should (memq 'ollama daemons-dash-hidden-providers))

    ;; State toggles
    (daemons-dash-filter-toggle-active)
    (should (memq 'active daemons-dash-hidden-states))

    (daemons-dash-filter-toggle-inactive)
    (should (memq 'inactive daemons-dash-hidden-states))

    (daemons-dash-filter-toggle-failed)
    (should (memq 'failed daemons-dash-hidden-states))

    ;; Reset
    (daemons-dash-filter-reset)
    (should (null daemons-dash-hidden-providers))
    (should (null daemons-dash-hidden-states))))

;;; 6. Interactive Actions & Dispatching

(ert-deftest daemons-dash-test-item-at-point-error-when-empty ()
  "When point is not on a daemon entry, daemons-dash--item-at-point raises user-error."
  (with-temp-buffer
    (daemons-dash-mode)
    (should-error (daemons-dash--item-at-point) :type 'user-error)))

(ert-deftest daemons-dash-test-action-dispatchers ()
  "Test start, stop, restart, enable, disable action dispatching on item at point."
  (let* ((started-id nil)
         (stopped-id nil)
         (restarted-id nil)
         (enabled-id nil)
         (disabled-id nil)
         (item (daemons-dash-item--make
                :id "mock-p:my-daemon"
                :name "my-daemon"
                :provider 'mock-p
                :status 'active))
         (mock-provider
          (daemons-dash-provider--make
           :name 'mock-p
           :label "mock"
           :list-fn (lambda () (list item))
           :start-fn (lambda (id) (setq started-id id))
           :stop-fn (lambda (id) (setq stopped-id id))
           :restart-fn (lambda (id) (setq restarted-id id))
           :enable-fn (lambda (id) (setq enabled-id id))
           :disable-fn (lambda (id) (setq disabled-id id)))))
    (let ((daemons-dash-providers (make-hash-table :test #'equal))
          (daemons-dash-enabled-providers '(mock-p)))
      (daemons-dash-register-provider mock-provider)
      (with-temp-buffer
        (daemons-dash-mode)
        (daemons-dash-refresh)
        (goto-char (point-min))

        (daemons-dash-start)
        (should (equal started-id "mock-p:my-daemon"))

        (daemons-dash-stop)
        (should (equal stopped-id "mock-p:my-daemon"))

        (daemons-dash-restart)
        (should (equal restarted-id "mock-p:my-daemon"))

        (daemons-dash-enable)
        (should (equal enabled-id "mock-p:my-daemon"))

        (daemons-dash-disable)
        (should (equal disabled-id "mock-p:my-daemon"))))))

(ert-deftest daemons-dash-test-action-unsupported ()
  "Executing an unsupported action on a provider messages without raising error."
  (let* ((mock-provider
          (daemons-dash-provider--make
           :name 'mock-noaction
           :label "mock"
           :list-fn #'ignore
           :start-fn nil))
         (item (daemons-dash-item--make
                :id "mock-noaction:test"
                :name "test"
                :provider 'mock-noaction
                :status 'active)))
    (let ((daemons-dash-providers (make-hash-table :test #'equal)))
      (daemons-dash-register-provider mock-provider)
      (with-temp-buffer
        (daemons-dash-mode)
        (setq tabulated-list-entries (list (daemons-dash--build-entry item)))
        (tabulated-list-print t)
        (goto-char (point-min))
        ;; Should not raise error
        (daemons-dash-start)))))

(ert-deftest daemons-dash-test-inspect-safe-invocation ()
  "Selecting an item to inspect or view logs does not signal wrong-number-of-arguments."
  (require 'journalctl-mode nil t)
  (let ((item (daemons-dash-item--make
               :id "systemd-user:odem.service"
               :name "odem.service"
               :provider 'systemd-user
               :status 'active))
        (journalctl-called nil))
    (cl-letf (((symbol-function 'journalctl--run)
               (lambda (opts &optional _chunk)
                 (setq journalctl-called opts))))
      (with-temp-buffer
        (daemons-dash-mode)
        (setq tabulated-list-entries (list (daemons-dash--build-entry item)))
        (tabulated-list-print t)
        (goto-char (point-min))
        (daemons-dash-inspect)
        (should (equal journalctl-called '("--user-unit=odem.service")))))))

(ert-deftest daemons-dash-test-view-logs-fallback ()
  "Viewing logs when inspect-fn is nil calls logs-fn."
  (let* ((logs-called nil)
         (mock-provider
          (daemons-dash-provider--make
           :name 'mock-logs
           :label "mock"
           :list-fn #'ignore
           :logs-fn (lambda (id) (setq logs-called id))
           :inspect-fn nil))
         (item (daemons-dash-item--make
                :id "mock-logs:daemon"
                :name "daemon"
                :provider 'mock-logs
                :status 'active)))
    (let ((daemons-dash-providers (make-hash-table :test #'equal)))
      (daemons-dash-register-provider mock-provider)
      (with-temp-buffer
        (daemons-dash-mode)
        (setq tabulated-list-entries (list (daemons-dash--build-entry item)))
        (tabulated-list-print t)
        (goto-char (point-min))
        (daemons-dash-inspect)
        (should (equal logs-called "mock-logs:daemon"))))))

;;; 7. Data Aggregation & Error Resilience

(ert-deftest daemons-dash-test-fetch-all-aggregation ()
  "daemons-dash-fetch-all combines items across enabled providers."
  (let* ((p1 (daemons-dash-provider--make
              :name 'mock1
              :list-fn (lambda () (list (daemons-dash-item--make :id "mock1:1" :name "1" :provider 'mock1 :status 'active)))))
         (p2 (daemons-dash-provider--make
              :name 'mock2
              :list-fn (lambda () (list (daemons-dash-item--make :id "mock2:2" :name "2" :provider 'mock2 :status 'active)))))
         (daemons-dash-providers (make-hash-table :test #'equal))
         (daemons-dash-enabled-providers '(mock1 mock2)))
    (daemons-dash-register-provider p1)
    (daemons-dash-register-provider p2)
    (let ((items (daemons-dash-fetch-all)))
      (should (= (length items) 2)))))

(ert-deftest daemons-dash-test-fetch-all-error-resilience ()
  "daemons-dash-fetch-all catches errors in a failing provider without failing completely."
  (let* ((p-fail (daemons-dash-provider--make
                  :name 'mock-fail
                  :list-fn (lambda () (error "backend failure"))))
         (p-ok (daemons-dash-provider--make
                :name 'mock-ok
                :list-fn (lambda () (list (daemons-dash-item--make :id "mock-ok:1" :name "1" :provider 'mock-ok :status 'active)))))
         (daemons-dash-providers (make-hash-table :test #'equal))
         (daemons-dash-enabled-providers '(mock-fail mock-ok)))
    (daemons-dash-register-provider p-fail)
    (daemons-dash-register-provider p-ok)
    (let ((items (daemons-dash-fetch-all)))
      (should (= (length items) 1))
      (should (equal (daemons-dash-item-name (car items)) "1")))))

;;; 8. Provider Specific Tests (Systemd, Docker, Sprite, Ollama)

(ert-deftest daemons-dash-test-systemd-unavailable ()
  "When systemctl executable is missing, providers return unavailable fallback."
  (cl-letf (((symbol-function 'executable-find)
             (lambda (cmd) (if (equal cmd "systemctl") nil "/bin/true"))))
    (let ((user-items (daemons-dash-systemd-user-list))
          (sys-items (daemons-dash-systemd-system-list)))
      (should (= (length user-items) 1))
      (should (string-match-p "unavailable" (daemons-dash-item-id (car user-items))))
      (should (= (length sys-items) 1))
      (should (string-match-p "unavailable" (daemons-dash-item-id (car sys-items)))))))

(ert-deftest daemons-dash-test-systemd-actions-exec ()
  "Systemd action functions invoke systemctl with appropriate arguments."
  (let ((commands nil))
    (cl-letf (((symbol-function 'daemons-dash--run-command)
               (lambda (cmd) (push cmd commands) "")))
      (daemons-dash-systemd-user-start "systemd-user:odem.service")
      (should (equal (car commands) '("systemctl" "--user" "start" "odem.service")))

      (daemons-dash-systemd-user-stop "systemd-user:odem.service")
      (should (equal (car commands) '("systemctl" "--user" "stop" "odem.service")))

      (daemons-dash-systemd-user-restart "systemd-user:odem.service")
      (should (equal (car commands) '("systemctl" "--user" "restart" "odem.service")))

      (daemons-dash-systemd-user-enable "systemd-user:odem.service")
      (should (equal (car commands) '("systemctl" "--user" "enable" "odem.service")))

      (daemons-dash-systemd-user-disable "systemd-user:odem.service")
      (should (equal (car commands) '("systemctl" "--user" "disable" "odem.service")))

      (daemons-dash-systemd-system-start "systemd-system:nginx.service")
      (should (equal (car commands) '("sudo" "systemctl" "start" "nginx.service")))

      (daemons-dash-systemd-system-stop "systemd-system:nginx.service")
      (should (equal (car commands) '("sudo" "systemctl" "stop" "nginx.service"))))))

(ert-deftest daemons-dash-test-docker-parse-and-list ()
  "Docker line parser extracts container name, status, and ports."
  (let* ((line-up "abc1234\tmy-app\tUp 3 hours\t0.0.0.0:8080->80/tcp")
         (line-down "def5678\tdb\tExited (0) 2 hours ago\t")
         (item-up (daemons-dash-docker--parse-line line-up))
         (item-down (daemons-dash-docker--parse-line line-down)))
    (should (equal (daemons-dash-item-name item-up) "my-app"))
    (should (eq (daemons-dash-item-status item-up) 'active))
    (should (string-match-p "8080" (daemons-dash-item-details item-up)))

    (should (equal (daemons-dash-item-name item-down) "db"))
    (should (eq (daemons-dash-item-status item-down) 'inactive))

    ;; Test docker-list with mock CLI output
    (cl-letf (((symbol-function 'executable-find) (lambda (_) "/usr/bin/docker"))
              ((symbol-function 'daemons-dash--run-command)
               (lambda (_) (format "%s\n%s\n" line-up line-down))))
      (let ((items (daemons-dash-docker-list)))
        (should (= (length items) 2))))))

(ert-deftest daemons-dash-test-docker-error-and-missing ()
  "Docker list handles missing executable and daemon error gracefully."
  (cl-letf (((symbol-function 'executable-find) (lambda (_) nil)))
    (let ((items (daemons-dash-docker-list)))
      (should (= (length items) 1))
      (should (string-match-p "unavailable" (daemons-dash-item-id (car items))))))

  (cl-letf (((symbol-function 'executable-find) (lambda (_) "/usr/bin/docker"))
            ((symbol-function 'daemons-dash--run-command)
             (lambda (_) (error "Docker daemon socket not found"))))
    (let ((items (daemons-dash-docker-list)))
      (should (= (length items) 1))
      (should (string-match-p "inactive" (daemons-dash-item-id (car items))))
      (should (string-match-p "inactive" (daemons-dash-item-details (car items)))))))

(ert-deftest daemons-dash-test-docker-actions ()
  "Docker action commands invoke docker start, stop, restart."
  (let ((commands nil))
    (cl-letf (((symbol-function 'daemons-dash--run-command)
               (lambda (cmd) (push cmd commands) "")))
      (daemons-dash-docker-start "docker:my-container")
      (should (equal (car commands) '("docker" "start" "my-container")))

      (daemons-dash-docker-stop "docker:my-container")
      (should (equal (car commands) '("docker" "stop" "my-container")))

      (daemons-dash-docker-restart "docker:my-container")
      (should (equal (car commands) '("docker" "restart" "my-container"))))))

(ert-deftest daemons-dash-test-sprite-provider ()
  "Sprite provider list and actions function correctly with mock sprite records."
  (require 'sprite nil t)
  (let ((mock-sprite (if (fboundp 'sprite--make)
                         (sprite--make :name "test-sprite")
                       (record 'sprite "test-sprite"))))
    (cl-letf (((symbol-function 'sprite-resolve-list)
               (lambda () (list mock-sprite)))
              ((symbol-function 'sprite--running-p)
               (lambda (_) t))
              ((symbol-function 'sprite-start-time)
               (lambda (_) nil)))
      (let ((items (daemons-dash-sprite-list)))
        (should (= (length items) 1))
        (should (equal (daemons-dash-item-name (car items)) "test-sprite"))
        (should (eq (daemons-dash-item-status (car items)) (quote active)))))))

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

(ert-deftest daemons-dash-test-ollama-model-parsing ()
  "Ollama model plist is properly converted to daemons-dash-item."
  (let* ((model-plist '(:name "llama3:8b" :size 4661224676 :size_vram 4661224676))
         (item (daemons-dash-ollama--model-to-item model-plist)))
    (should (equal (daemons-dash-item-id item) "ollama:llama3:8b"))
    (should (equal (daemons-dash-item-name item) "llama3:8b"))
    (should (eq (daemons-dash-item-provider item) 'ollama))
    (should (eq (daemons-dash-item-status item) 'active))
    (should (string-match-p "VRAM" (daemons-dash-item-details item)))))

;;; 9. Declarative Configuration & Verification

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

(ert-deftest daemons-dash-test-config-clear-and-errors ()
  "daemons-dash-config-clear empties registry; invalid name raises error."
  (let ((daemons-dash-config-registry (make-hash-table :test #'equal)))
    (daemons-dash-register-service "test.service" :provider 'systemd-user)
    (should (map-elt daemons-dash-config-registry "test.service"))
    (daemons-dash-config-clear)
    (should-not (map-elt daemons-dash-config-registry "test.service"))
    ;; Invalid name types
    (should-error (daemons-dash-register-service 12345))))

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

(ert-deftest daemons-dash-test-config-verify-equivalent-statuses ()
  "active/running and inactive/stopped are considered matching."
  (let ((daemons-dash-config-registry (make-hash-table :test #'equal))
        (item-run (daemons-dash-item--make :name "app" :provider 'docker :status 'running))
        (item-stop (daemons-dash-item--make :name "worker" :provider 'systemd-user :status 'stopped)))
    (daemons-dash-register-service "app" :expected-status 'active)
    (daemons-dash-register-service "worker" :expected-status 'inactive)
    (daemons-dash-config-verify item-run)
    (daemons-dash-config-verify item-stop)
    (should (eq (daemons-dash-item-config-status item-run) 'match))
    (should (eq (daemons-dash-item-config-status item-stop) 'match))))

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

(ert-deftest daemons-dash-test-config-annotate-entries ()
  "daemons-dash-config-annotate-entries verifies every item in list."
  (let ((daemons-dash-config-registry (make-hash-table :test #'equal))
        (items (list (daemons-dash-item--make :name "s1" :status 'active)
                     (daemons-dash-item--make :name "s2" :status 'failed))))
    (daemons-dash-register-service "s1" :expected-status 'active)
    (daemons-dash-config-annotate-entries items)
    (should (eq (daemons-dash-item-config-status (nth 0 items)) 'match))
    (should (eq (daemons-dash-item-config-status (nth 1 items)) 'untracked))))

;;; 10. YAML Translation Layer

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

(ert-deftest daemons-dash-test-yaml-export-to-file ()
  "Exporting declarations to a file writes YAML content to disk."
  (let ((daemons-dash-config-registry (make-hash-table :test #'equal))
        (tmp (make-temp-file "daemons-test-" nil ".yaml")))
    (unwind-protect
        (progn
          (daemons-dash-register-service "file-service" :provider 'systemd-user :expected-status 'active)
          (daemons-dash-config-export-yaml tmp)
          (should (file-exists-p tmp))
          (with-temp-buffer
            (insert-file-contents tmp)
            (should (string-match-p "file-service" (buffer-string)))))
      (delete-file tmp))))

(ert-deftest daemons-dash-test-yaml-fallback-import-detailed ()
  "Fallback regexp YAML parser handles ports, command, auto_restart, and doc."
  (let ((daemons-dash-config-registry (make-hash-table :test #'equal))
        (yaml-str (concat
                   "daemons:\n"
                   "  services:\n"
                   "    - name: \"custom-daemon\"\n"
                   "      provider: docker\n"
                   "      expected_status: running\n"
                   "      doc: \"A custom daemon test\"\n"
                   "      auto_restart: true\n"
                   "      command: \"./start.sh\"\n"
                   "      ports:\n"
                   "        - 8080\n"
                   "        - 8443\n")))
    (daemons-dash-config--import-yaml-fallback yaml-str)
    (let ((spec (map-elt daemons-dash-config-registry "custom-daemon")))
      (should spec)
      (should (equal (plist-get spec :name) "custom-daemon"))
      (should (eq (plist-get spec :provider) 'docker))
      (should (eq (plist-get spec :expected-status) 'running))
      (should (equal (plist-get spec :doc) "A custom daemon test"))
      (should (equal (plist-get spec :command) "./start.sh"))
      (should (plist-get spec :auto-restart))
      (should (equal (plist-get spec :ports) '("8080" "8443"))))))

(ert-deftest daemons-dash-test-yaml-fallback-export ()
  "daemons-dash-config--export-yaml-fallback outputs well-formed YAML."
  (let* ((specs (list (list :name "test-svc"
                            :provider 'systemd-user
                            :expected-status 'active
                            :doc "Doc string"
                            :auto-restart t
                            :ports '("9000")
                            :command "run.sh")))
         (yaml (daemons-dash-config--export-yaml-fallback specs)))
    (should (string-match-p "test-svc" yaml))
    (should (string-match-p "systemd-user" yaml))
    (should (string-match-p "active" yaml))
    (should (string-match-p "Doc string" yaml))
    (should (string-match-p "auto_restart: true" yaml))
    (should (string-match-p "9000" yaml))
    (should (string-match-p "run.sh" yaml))))

;;; 11. Mode & Transient Menu Validation

(ert-deftest daemons-dash-test-mode-setup ()
  "daemons-dash-mode configures tabulated-list and local filter variables."
  (with-temp-buffer
    (daemons-dash-mode)
    (should (derived-mode-p 'tabulated-list-mode))
    (should (equal tabulated-list-sort-key '("Name" . nil)))
    (should (local-variable-p 'daemons-dash-hidden-providers))
    (should (local-variable-p 'daemons-dash-hidden-states))))

(ert-deftest daemons-dash-test-transient-menus-no-conflicts ()
  "Verify transient prefix menus have no duplicate keys or prefix conflicts."
  (when (fboundp 'transient-test/collect-keys)
    ;; Filter menu
    (let* ((filter-keys (transient-test/collect-keys 'daemons-dash-filter-menu))
           (dups (transient-test/duplicate-keys filter-keys))
           (conflicts (transient-test/key-prefix-conflicts filter-keys)))
      (should (null dups))
      (should (null conflicts)))
    ;; Dispatch menu
    (let* ((dispatch-keys (transient-test/collect-keys 'daemons-dash-dispatch))
           (dups (transient-test/duplicate-keys dispatch-keys))
           (conflicts (transient-test/key-prefix-conflicts dispatch-keys)))
      (should (null dups))
      (should (null conflicts)))))

(ert-deftest daemons-dash-test-entrypoint ()
  "Main interactive entrypoint opens buffer in daemons-dash-mode."
  (cl-letf (((symbol-function 'daemons-dash-refresh) #'ignore)
            ((symbol-function 'pop-to-buffer) #'ignore))
    (let ((buf (daemons-dash)))
      (should (bufferp buf))
      (with-current-buffer buf
        (should (eq major-mode 'daemons-dash-mode)))
      (kill-buffer buf))))

(provide 'test-daemons-dash)
;;; test-daemons-dash.el ends here
