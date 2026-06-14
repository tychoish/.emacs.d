;;; test-sprite-session.el --- ERT tests for sprite-session.el -*- lexical-binding: t -*-

;; Run inside a live Emacs session:
;;   (ert "^sprite-session/")
;;
;; Batch run:
;;   emacs --batch -L ~/.emacs.d/lisp \
;;     -l ~/.emacs.d/test/test-sprite-session.el \
;;     --eval '(ert-run-tests-batch-and-exit "sprite-session/")'

;;; Commentary:
;;
;; Unit tests for sprite-session.el.  DBus functions and system-type are
;; mocked via cl-letf so no live DBus connection or real idle timers are
;; required.  The with-clean-state macro isolates all mutable var state.
;;

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'sprite-session)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Test helpers

(defmacro sprite-session-test/with-clean-state (&rest body)
  "Run BODY with isolated sprite-session hook and timer state.
Cancels any timer created during BODY on exit."
  (declare (indent 0))
  `(let ((sprite-session-idle-hook nil)
         (sprite-session--idle-timer nil)
         (sprite-session--logind-signal nil))
     (unwind-protect
         (progn ,@body)
       (when (timerp sprite-session--idle-timer)
         (cancel-timer sprite-session--idle-timer)))))

(defmacro sprite-session-test/capture-messages (&rest body)
  "Execute BODY and return the list of strings passed to `message' in order."
  (declare (indent 0))
  (let ((msgs (gensym "msgs")))
    `(let (,msgs)
       (cl-letf (((symbol-function 'message)
                  (lambda (fmt &rest args)
                    (push (apply #'format fmt args) ,msgs))))
         ,@body)
       (nreverse ,msgs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sprite-session-start/stop-idle-timer

(ert-deftest sprite-session/start-idle-timer-creates-timer ()
  "`sprite-session-start-idle-timer' stores a live timer in the state variable."
  (sprite-session-test/with-clean-state
    (sprite-session-start-idle-timer)
    (should (timerp sprite-session--idle-timer))))

(ert-deftest sprite-session/stop-idle-timer-cancels-and-nils ()
  "`sprite-session-stop-idle-timer' cancels the timer and sets the var to nil."
  (sprite-session-test/with-clean-state
    (sprite-session-start-idle-timer)
    (sprite-session-stop-idle-timer)
    (should (null sprite-session--idle-timer))))

(ert-deftest sprite-session/stop-idle-timer-noop-when-nil ()
  "`sprite-session-stop-idle-timer' does not error when timer is already nil."
  (sprite-session-test/with-clean-state
    (sprite-session-stop-idle-timer)))

(ert-deftest sprite-session/start-idle-timer-replaces-existing ()
  "Calling start twice replaces the first timer with a new one."
  (sprite-session-test/with-clean-state
    (sprite-session-start-idle-timer)
    (let ((first sprite-session--idle-timer))
      (sprite-session-start-idle-timer)
      (should (timerp sprite-session--idle-timer))
      (should-not (eq first sprite-session--idle-timer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sprite-session-sync-idle-timer

(ert-deftest sprite-session/sync-starts-timer-when-hook-nonempty ()
  "`sprite-session-sync-idle-timer' starts the timer when the hook has members."
  (sprite-session-test/with-clean-state
    (cl-letf (((symbol-function 'message) #'ignore))
      (add-hook 'sprite-session-idle-hook #'ignore)
      (sprite-session-sync-idle-timer)
      (should (timerp sprite-session--idle-timer)))))

(ert-deftest sprite-session/sync-stops-timer-when-hook-empty ()
  "`sprite-session-sync-idle-timer' stops the timer when the hook is empty."
  (sprite-session-test/with-clean-state
    (cl-letf (((symbol-function 'message) #'ignore))
      (sprite-session-start-idle-timer)
      (sprite-session-sync-idle-timer)
      (should (null sprite-session--idle-timer)))))

(ert-deftest sprite-session/sync-does-not-restart-running-timer ()
  "`sprite-session-sync-idle-timer' leaves an already-running timer untouched."
  (sprite-session-test/with-clean-state
    (cl-letf (((symbol-function 'message) #'ignore))
      (add-hook 'sprite-session-idle-hook #'ignore)
      (sprite-session-start-idle-timer)
      (let ((first sprite-session--idle-timer))
        (sprite-session-sync-idle-timer)
        (should (eq first sprite-session--idle-timer))))))

(ert-deftest sprite-session/sync-noop-when-hook-empty-and-timer-nil ()
  "`sprite-session-sync-idle-timer' does not error when hook empty and timer nil."
  (sprite-session-test/with-clean-state
    (cl-letf (((symbol-function 'message) #'ignore))
      (sprite-session-sync-idle-timer))))

(ert-deftest sprite-session/sync-logs-running-state ()
  "`sprite-session-sync-idle-timer' logs \"running\" when the timer is active."
  (sprite-session-test/with-clean-state
    (add-hook 'sprite-session-idle-hook #'ignore)
    (let ((msgs (sprite-session-test/capture-messages
                  (sprite-session-sync-idle-timer))))
      (should (= 1 (length msgs)))
      (should (string-match-p "running" (car msgs))))))

(ert-deftest sprite-session/sync-logs-stopped-state ()
  "`sprite-session-sync-idle-timer' logs \"stopped\" when the hook is empty."
  (sprite-session-test/with-clean-state
    (let ((msgs (sprite-session-test/capture-messages
                  (sprite-session-sync-idle-timer))))
      (should (= 1 (length msgs)))
      (should (string-match-p "stopped" (car msgs))))))

(ert-deftest sprite-session/sync-logs-registered-op-count ()
  "`sprite-session-sync-idle-timer' includes the count of registered ops."
  (sprite-session-test/with-clean-state
    (add-hook 'sprite-session-idle-hook #'ignore)
    (add-hook 'sprite-session-idle-hook #'identity)
    (let ((msgs (sprite-session-test/capture-messages
                  (sprite-session-sync-idle-timer))))
      (should (string-match-p "2" (car msgs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sprite-session-add-on-idle

(ert-deftest sprite-session/add-on-idle-adds-fn-to-hook ()
  "`sprite-session-add-on-idle' adds fn to `sprite-session-idle-hook'."
  (sprite-session-test/with-clean-state
    (cl-letf (((symbol-function 'message) #'ignore))
      (sprite-session-add-on-idle #'ignore)
      (should (memq #'ignore sprite-session-idle-hook)))))

(ert-deftest sprite-session/add-on-idle-starts-timer ()
  "`sprite-session-add-on-idle' starts the idle timer."
  (sprite-session-test/with-clean-state
    (cl-letf (((symbol-function 'message) #'ignore))
      (sprite-session-add-on-idle #'ignore)
      (should (timerp sprite-session--idle-timer)))))

(ert-deftest sprite-session/add-on-idle-logs-registration ()
  "`sprite-session-add-on-idle' logs \"registered\" with the function name."
  (sprite-session-test/with-clean-state
    (let ((msgs (sprite-session-test/capture-messages
                  (sprite-session-add-on-idle #'ignore))))
      (should (string-match-p "registered" (car msgs)))
      (should (string-match-p "ignore" (car msgs))))))

(ert-deftest sprite-session/add-on-idle-emits-two-messages ()
  "`sprite-session-add-on-idle' emits one registration line then one sync line."
  (sprite-session-test/with-clean-state
    (let ((msgs (sprite-session-test/capture-messages
                  (sprite-session-add-on-idle #'ignore))))
      (should (= 2 (length msgs))))))

(ert-deftest sprite-session/add-on-idle-multiple-fns-all-on-hook ()
  "Multiple distinct fns can be added; all appear in the hook."
  (sprite-session-test/with-clean-state
    (cl-letf (((symbol-function 'message) #'ignore))
      (sprite-session-add-on-idle #'ignore)
      (sprite-session-add-on-idle #'identity)
      (should (memq #'ignore sprite-session-idle-hook))
      (should (memq #'identity sprite-session-idle-hook)))))

(ert-deftest sprite-session/add-on-idle-multiple-fns-timer-live ()
  "Timer stays running when multiple fns are registered."
  (sprite-session-test/with-clean-state
    (cl-letf (((symbol-function 'message) #'ignore))
      (sprite-session-add-on-idle #'ignore)
      (sprite-session-add-on-idle #'identity)
      (should (timerp sprite-session--idle-timer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sprite-session-remove-on-idle

(ert-deftest sprite-session/remove-on-idle-removes-fn-from-hook ()
  "`sprite-session-remove-on-idle' removes fn from `sprite-session-idle-hook'."
  (sprite-session-test/with-clean-state
    (cl-letf (((symbol-function 'message) #'ignore))
      (sprite-session-add-on-idle #'ignore)
      (sprite-session-remove-on-idle #'ignore)
      (should-not (memq #'ignore sprite-session-idle-hook)))))

(ert-deftest sprite-session/remove-on-idle-stops-timer-when-hook-empties ()
  "`sprite-session-remove-on-idle' stops the timer when the hook becomes empty."
  (sprite-session-test/with-clean-state
    (cl-letf (((symbol-function 'message) #'ignore))
      (sprite-session-add-on-idle #'ignore)
      (sprite-session-remove-on-idle #'ignore)
      (should (null sprite-session--idle-timer)))))

(ert-deftest sprite-session/remove-on-idle-keeps-timer-when-others-remain ()
  "Timer stays running after removing one fn when other fns remain."
  (sprite-session-test/with-clean-state
    (cl-letf (((symbol-function 'message) #'ignore))
      (sprite-session-add-on-idle #'ignore)
      (sprite-session-add-on-idle #'identity)
      (sprite-session-remove-on-idle #'ignore)
      (should (timerp sprite-session--idle-timer)))))

(ert-deftest sprite-session/remove-on-idle-logs-deregistration ()
  "`sprite-session-remove-on-idle' logs \"deregistered\" with the function name."
  (sprite-session-test/with-clean-state
    (cl-letf (((symbol-function 'message) #'ignore))
      (sprite-session-add-on-idle #'ignore))
    (let ((msgs (sprite-session-test/capture-messages
                  (sprite-session-remove-on-idle #'ignore))))
      (should (string-match-p "deregistered" (car msgs)))
      (should (string-match-p "ignore" (car msgs))))))

(ert-deftest sprite-session/remove-on-idle-emits-two-messages ()
  "`sprite-session-remove-on-idle' emits one deregistration then one sync line."
  (sprite-session-test/with-clean-state
    (cl-letf (((symbol-function 'message) #'ignore))
      (sprite-session-add-on-idle #'ignore))
    (let ((msgs (sprite-session-test/capture-messages
                  (sprite-session-remove-on-idle #'ignore))))
      (should (= 2 (length msgs))))))

(ert-deftest sprite-session/remove-nonexistent-fn-is-safe ()
  "Removing a fn that was never registered does not error."
  (sprite-session-test/with-clean-state
    (cl-letf (((symbol-function 'message) #'ignore))
      (sprite-session-remove-on-idle #'ignore))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; systemd-logind: availability predicate

(ert-deftest sprite-session/logind-unavailable-on-non-linux ()
  "`sprite-session--logind-available-p' returns nil on non-Linux systems."
  (let ((system-type 'darwin))
    (should-not (sprite-session--logind-available-p))))

(ert-deftest sprite-session/logind-unavailable-without-dbus ()
  "`sprite-session--logind-available-p' returns nil when dbus-register-signal is unbound."
  (let ((system-type 'gnu/linux))
    (cl-letf (((symbol-function 'fboundp)
               (lambda (sym)
                 (if (eq sym 'dbus-register-signal) nil (fboundp sym)))))
      (should-not (sprite-session--logind-available-p)))))

(ert-deftest sprite-session/logind-available-on-linux-with-dbus ()
  "`sprite-session--logind-available-p' returns non-nil on Linux with dbus."
  (let ((system-type 'gnu/linux))
    (cl-letf (((symbol-function 'fboundp)
               (lambda (sym)
                 (if (eq sym 'dbus-register-signal) t (fboundp sym)))))
      (should (sprite-session--logind-available-p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; systemd-logind: start/stop watch

(ert-deftest sprite-session/start-logind-watch-calls-dbus-register ()
  "`sprite-session-start-logind-watch' calls `dbus-register-signal' on Linux."
  (sprite-session-test/with-clean-state
    (let ((dbus-called nil)
          (system-type 'gnu/linux))
      (cl-letf (((symbol-function 'fboundp)
                 (lambda (sym)
                   (if (eq sym 'dbus-register-signal) t (fboundp sym))))
                ((symbol-function 'dbus-register-signal)
                 (lambda (&rest _) (setq dbus-called t) 'fake-signal)))
        (sprite-session-start-logind-watch)
        (should dbus-called)
        (should (eq 'fake-signal sprite-session--logind-signal))))))

(ert-deftest sprite-session/start-logind-watch-noop-on-non-linux ()
  "`sprite-session-start-logind-watch' does not touch dbus on non-Linux."
  (sprite-session-test/with-clean-state
    (let ((dbus-called nil)
          (system-type 'darwin))
      (cl-letf (((symbol-function 'dbus-register-signal)
                 (lambda (&rest _) (setq dbus-called t))))
        (sprite-session-start-logind-watch)
        (should-not dbus-called)
        (should (null sprite-session--logind-signal))))))

(ert-deftest sprite-session/stop-logind-watch-unregisters-signal ()
  "`sprite-session-stop-logind-watch' calls `dbus-unregister-object' with the stored signal."
  (sprite-session-test/with-clean-state
    (let (received)
      (setq sprite-session--logind-signal 'fake-signal)
      (cl-letf (((symbol-function 'dbus-unregister-object)
                 (lambda (obj) (setq received obj))))
        (sprite-session-stop-logind-watch)
        (should (eq 'fake-signal received))
        (should (null sprite-session--logind-signal))))))

(ert-deftest sprite-session/stop-logind-watch-noop-when-not-watching ()
  "`sprite-session-stop-logind-watch' does not error when signal var is nil."
  (sprite-session-test/with-clean-state
    (sprite-session-stop-logind-watch)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sprite-session--on-prepare-for-sleep

(ert-deftest sprite-session/on-prepare-for-sleep-runs-hook-when-sleeping ()
  "`sprite-session--on-prepare-for-sleep' runs the before-sleep hook when arg is t."
  (let (run-hooks-arg)
    (cl-letf (((symbol-function 'run-hooks)
               (lambda (hook) (setq run-hooks-arg hook))))
      (sprite-session--on-prepare-for-sleep t)
      (should (eq 'sprite-session-before-sleep-hook run-hooks-arg)))))

(ert-deftest sprite-session/on-prepare-for-sleep-skips-hook-when-waking ()
  "`sprite-session--on-prepare-for-sleep' does not run the hook on wake (arg nil)."
  (let (run-hooks-arg)
    (cl-letf (((symbol-function 'run-hooks)
               (lambda (hook) (setq run-hooks-arg hook))))
      (sprite-session--on-prepare-for-sleep nil)
      (should-not run-hooks-arg))))

(provide 'test-sprite-session)
;;; test-sprite-session.el ends here
