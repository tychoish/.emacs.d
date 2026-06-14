;;; sprite-session.el --- Generic session lifecycle hooks for Emacs instances -*- lexical-binding: t; -*-

;;; Commentary:
;; Idle timer and systemd-logind sleep hooks that application packages can
;; register against.  Decouples trigger mechanisms (Emacs idleness, system
;; sleep) from application-level responses.
;;
;; Usage: call `sprite-session-start-idle-timer' and/or
;; `sprite-session-start-logind-watch' from your package's setup function,
;; add functions to `sprite-session-idle-hook' and/or
;; `sprite-session-before-sleep-hook', then stop the timers in teardown.

;;; Code:

(require 'dbus nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs idle hook

(defcustom sprite-session-idle-timeout 3600
  "Seconds of Emacs idle before `sprite-session-idle-hook' fires."
  :type 'integer
  :group 'sprite)

(defvar sprite-session-idle-hook nil
  "Hook run when Emacs has been idle for `sprite-session-idle-timeout' seconds.
Each function is called with no arguments.")

(defvar sprite-session--idle-timer nil
  "Timer that runs `sprite-session-idle-hook' after extended Emacs idle.")

(defun sprite-session-start-idle-timer ()
  "Start a repeating idle timer that runs `sprite-session-idle-hook'.
Cancels any existing timer first."
  (sprite-session-stop-idle-timer)
  (setq sprite-session--idle-timer
        (run-with-idle-timer sprite-session-idle-timeout t
                             #'run-hooks 'sprite-session-idle-hook)))

(defun sprite-session-stop-idle-timer ()
  "Cancel the idle timer."
  (when sprite-session--idle-timer
    (cancel-timer sprite-session--idle-timer)
    (setq sprite-session--idle-timer nil)))

(defun sprite-session-sync-idle-timer ()
  "Start or stop the idle timer to match `sprite-session-idle-hook' membership.
Starts the timer when the hook is non-nil; stops it when empty.
Always logs: sprite-session: idle timer <running|stopped> (<N> registered ops)"
  (if sprite-session-idle-hook
      (unless sprite-session--idle-timer
        (sprite-session-start-idle-timer))
    (when sprite-session--idle-timer
      (sprite-session-stop-idle-timer)))
  (message "sprite-session: idle timer %s (%d registered ops)"
           (if sprite-session--idle-timer "running" "stopped")
           (length sprite-session-idle-hook)))

(defun sprite-session-add-on-idle (fn)
  "Add FN to `sprite-session-idle-hook' and start the timer if needed.
Logs the registration and delegates to `sprite-session-sync-idle-timer'."
  (message "sprite-session: registered idle op: %s" (symbol-name fn))
  (add-hook 'sprite-session-idle-hook fn)
  (sprite-session-sync-idle-timer))

(defun sprite-session-remove-on-idle (fn)
  "Remove FN from `sprite-session-idle-hook' and stop the timer if the hook is empty.
Logs the deregistration and delegates to `sprite-session-sync-idle-timer'."
  (message "sprite-session: deregistered idle op: %s" (symbol-name fn))
  (remove-hook 'sprite-session-idle-hook fn)
  (sprite-session-sync-idle-timer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; systemd-logind PrepareForSleep hook

(defvar sprite-session-before-sleep-hook nil
  "Hook run before the system suspends or hibernates.
Each function is called with no arguments.  Only fires on Linux systems
with DBus support and systemd-logind.")

(defvar sprite-session--logind-signal nil
  "DBus registration object for the logind PrepareForSleep signal.")

(defun sprite-session--logind-available-p ()
  "Return non-nil when systemd-logind DBus integration is usable."
  (and (fboundp 'dbus-register-signal)
       (eq system-type 'gnu/linux)))

(defun sprite-session--on-prepare-for-sleep (going-to-sleep)
  "Run `sprite-session-before-sleep-hook' when GOING-TO-SLEEP is non-nil."
  (when going-to-sleep
    (run-hooks 'sprite-session-before-sleep-hook)))

(defun sprite-session-start-logind-watch ()
  "Register a DBus signal to run `sprite-session-before-sleep-hook' on sleep."
  (when (sprite-session--logind-available-p)
    (setq sprite-session--logind-signal
          (dbus-register-signal
           :system
           "org.freedesktop.login1"
           "/org/freedesktop/login1"
           "org.freedesktop.login1.Manager"
           "PrepareForSleep"
           #'sprite-session--on-prepare-for-sleep))))

(defun sprite-session-stop-logind-watch ()
  "Unregister the logind PrepareForSleep DBus signal."
  (when sprite-session--logind-signal
    (dbus-unregister-object sprite-session--logind-signal)
    (setq sprite-session--logind-signal nil)))

(provide 'sprite-session)
;;; sprite-session.el ends here
