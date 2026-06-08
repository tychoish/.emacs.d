;;; telega-extras.el --- Idle-disconnect and lifecycle extras for telega -*- lexical-binding: t; -*-

;;; Commentary:
;; Idle-disconnect timer, systemd-logind sleep integration, buffer
;; management, and notification filtering for the telega Telegram client.
;;
;; Load order: required from use-package telega :config block, after telega
;; is loaded.  All setup runs via `telega-extras-setup' on `telega-ready-hook'
;; so nothing here races against telega's own load-time hooks.

;;; Code:

(require 'sprite)
(require 'alert)
(require 'tychoish-bootstrap)
(require 'telega)
(require 'telega-server)
(require 'telega-chat)
(require 'telega-msg)
(require 'telega-root)
(require 'dbus nil t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Idle-disconnect timer

(defvar telega-extras--idle-timer nil
  "Timer that disconnects telega after extended Emacs idle.")

(defun telega-extras-disconnect ()
  "Soft-disconnect from Telegram: stop the server, keep buffers.
Calls `telega-server-kill', which stops the tdlib subprocess and fires
`telega-kill-hook' but leaves the root and chat buffers alive.
Reconnect with \\[telega]."
  (interactive)
  (telega-server-kill))

(defun telega-extras--idle-disconnect ()
  "Disconnect from Telegram when Emacs idle threshold is reached."
  (when (telega-server-live-p)
    (message "telega-extras: idle disconnect")
    (telega-extras-disconnect)))

(defun telega-extras-start-idle-timer ()
  "Start a repeating idle timer to disconnect telega after 1 hour of idle."
  (telega-extras-stop-idle-timer)
  (setq telega-extras--idle-timer
        (run-with-idle-timer 3600 t #'telega-extras--idle-disconnect)))

(defun telega-extras-stop-idle-timer ()
  "Cancel the idle disconnect timer."
  (when telega-extras--idle-timer
    (cancel-timer telega-extras--idle-timer)
    (setq telega-extras--idle-timer nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; systemd-logind PrepareForSleep integration
;;
;; On Linux with DBus support, Emacs can subscribe to the logind Manager
;; PrepareForSleep signal.  This fires (with arg t) just before the system
;; suspends or hibernates, giving us a chance to disconnect before the
;; network disappears.  This is complementary to the idle timer: the idle
;; timer handles "left laptop open but walked away"; logind handles
;; "closed the lid".

(defvar telega-extras--logind-signal nil
  "DBus registration object for the logind PrepareForSleep signal.")

(defun telega-extras--logind-available-p ()
  "Return non-nil when systemd-logind DBus integration is usable."
  (and (fboundp 'dbus-register-signal)
       (eq system-type 'gnu/linux)))

(defun telega-extras--on-prepare-for-sleep (going-to-sleep)
  "Disconnect telega before sleep when GOING-TO-SLEEP is non-nil."
  (when (and going-to-sleep (telega-server-live-p))
    (message "telega-extras: disconnecting before sleep/suspend")
    (telega-extras-disconnect)))

(defun telega-extras-start-logind-watch ()
  "Register a DBus signal to disconnect telega on system sleep or suspend."
  (when (telega-extras--logind-available-p)
    (setq telega-extras--logind-signal
          (dbus-register-signal
           :system
           "org.freedesktop.login1"
           "/org/freedesktop/login1"
           "org.freedesktop.login1.Manager"
           "PrepareForSleep"
           #'telega-extras--on-prepare-for-sleep))))

(defun telega-extras-stop-logind-watch ()
  "Unregister the logind PrepareForSleep DBus signal."
  (when telega-extras--logind-signal
    (dbus-unregister-object telega-extras--logind-signal)
    (setq telega-extras--logind-signal nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Confirmation helper

(defun telega-extras--confirm (prompt)
  "Prompt with PROMPT offering yes/no/abort choices.
Returns t to proceed, nil to skip, or signals `user-error' on abort.
Intended for use with `called-interactively-p' guards so that
non-interactive callers never see a prompt."
  (pcase (read-answer prompt
                      '(("yes" ?y "proceed")
                        ("no" ?n "do nothing")
                        ("abort" ?q "signal an error and stop")))
    ("yes" t)
    ("no" nil)
    (_ (user-error "Aborted"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Buffer management

(defun telega-extras-bury-chat-buffers ()
  "Replace all visible telega chat buffers with the root buffer."
  (interactive)
  (when-let* ((root (get-buffer telega-root-buffer-name))
              (chat-windows
               (thread-last (frame-list)
                 (seq-mapcat #'window-list)
                 (seq-filter (lambda (w)
                               (with-current-buffer (window-buffer w)
                                 (derived-mode-p 'telega-chat-mode))))
		 (mapc (lambda (w)
			 (bury-buffer (window-buffer w))
			 (set-window-buffer w root))))))

    (alert (format "buried %d telega-chat-%s"
                   (length chat-windows)
		   (s-plural-for chat-windows "buffer" "buffers"))
           :title (format "emacs.%s.telega" (sprite-instance-name))
           :persistent t)))

(defun telega-extras-kill-chat-buffers ()
  "Kill all live telega-chat-mode buffers, prompting when interactive."
  (interactive)
  (when-let* ((bufs (thread-last (buffer-list)
                      (seq-filter (lambda (b)
                                    (with-current-buffer b
                                      (derived-mode-p 'telega-chat-mode))))))
              (_ (or (not (called-interactively-p 'interactive))
                     (telega-extras--confirm
                      (format "Kill %d telega chat buffer%s? "
                              (length bufs)
                              (if (= 1 (length bufs)) "" "s"))))))
    (mapc #'kill-buffer bufs)))

(defun telega-extras-force-kill ()
  "Kill Telega without confirmation."
  (interactive)
  (telega-kill t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Root buffer as default for new frames

(defun telega-extras-switch-to-root (&optional _frame)
  "Switch to the Telega Root buffer, falling back to a sensible buffer."
  (interactive)
  (switch-to-buffer
   (or (get-buffer telega-root-buffer-name)
       (last-buffer)
       (get-buffer-create tychoish-fallback-buffer-name))))

(defun telega-extras-make-root-default ()
  "Make `telega-extras-switch-to-root' the default buffer for new frames."
  (add-hook 'after-make-frame-functions #'telega-extras-switch-to-root)
  (add-hook 'server-after-make-frame-hook #'telega-extras-switch-to-root)
  (setq initial-buffer-choice #'telega-extras-switch-to-root))

(defun telega-extras-remove-root-default ()
  "Undo the effect of `telega-extras-make-root-default'."
  (remove-hook 'after-make-frame-functions #'telega-extras-switch-to-root)
  (remove-hook 'server-after-make-frame-hook #'telega-extras-switch-to-root)
  (setq initial-buffer-choice nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Root buffer navigation

(defun telega-extras-root-cycle-next (chat)
  "Expand forum CHAT or cycle point to the next important/unread chat."
  (interactive (list (telega-chat-at (point))))
  (if (telega-chat-match-p chat 'is-forum)
      (telega-chat-button-toggle-view chat)
    (ignore-errors
      (telega-root-next-important (point))
      (telega-root-next-mention (point))
      (telega-root-next-reaction (point))
      (telega-root-next-unread (point)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Notification filtering

(defun telega-extras--notify-skip-own (orig-fn msg)
  "Skip desktop notification when MSG was sent by self; otherwise call ORIG-FN."
  (unless (telega-msg-match-p msg '(sender me))
    (funcall orig-fn msg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Debug toggle

(defun telega-extras-toggle-debug ()
  "Toggle `telega-debug'."
  (interactive)
  (setq telega-debug (not telega-debug))
  (message "telega-debug %s" (if telega-debug "enabled" "disabled")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Override: disable per-chat folder grouping
;;
;; Returning nil from telega-chat-folders disables the per-chat folder
;; annotations used by telega's grouping views, keeping the root view flat.

(defun telega-chat-folders (_chat)
  "Return nil to disable per-chat folder grouping."
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Lifecycle entry points

(defun telega-extras-setup ()
  "Set up telega extras.  Called on `telega-ready-hook'."
  (telega-extras-make-root-default)
  (telega-extras-start-idle-timer)
  (telega-extras-start-logind-watch)
  (advice-add 'telega-notifications-msg-notify-p
              :around #'telega-extras--notify-skip-own))

(defun telega-extras-teardown ()
  "Tear down telega extras.  Called on `telega-kill-hook'.
Prompts for confirmation when called interactively."
  (interactive)
  (when (or (not (called-interactively-p 'interactive))
            (telega-extras--confirm "Tear down telega extras? "))
    (telega-extras-remove-root-default)
    (telega-extras-stop-idle-timer)
    (telega-extras-stop-logind-watch)
    (advice-remove 'telega-notifications-msg-notify-p
                   #'telega-extras--notify-skip-own)))

(add-hook 'telega-ready-hook #'telega-extras-setup)
(add-hook 'telega-kill-hook #'telega-extras-teardown)

(provide 'telega-extras)
;;; telega-extras.el ends here
