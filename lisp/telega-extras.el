;;; telega-extras.el --- Configurable idle actions and lifecycle extras for telega -*- lexical-binding: t; -*-

;;; Commentary:
;; Configurable idle actions, systemd-logind sleep integration, buffer
;; management, and notification filtering for the telega Telegram client.
;;
;; Trigger mechanisms (idle timer, logind PrepareForSleep) are handled by
;; `sprite-session'; this file registers telega-specific responses on those
;; hooks and provides per-instance configuration helpers for personal.el.
;;
;; Load order: required from use-package telega :config block, after telega
;; is loaded.  All setup runs via `telega-extras-setup' on `telega-ready-hook'
;; so nothing here races against telega's own load-time hooks.

;;; Code:

(require 'sprite)
(require 'sprite-session)
(require 'alert)
(require 'bootstrap)

(declare-function resolve-plural-form "xtdlib")

(require 'telega)
(require 'telega-server)
(require 'telega-chat)
(require 'telega-msg)
(require 'telega-root)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Configurable idle action

(defcustom telega-extras-idle-action 'switch-to-root
  "Action taken when `sprite-session-idle-hook' fires while telega is live.
Possible values:
  `switch-to-root'     — switch the current frame to the telega root buffer
  `bury-chat-buffers'  — bury visible chat windows and replace with root
  `kill-chat-buffers'  — kill all telega-chat-mode buffers
  `disconnect'         — stop the telega server (soft disconnect)"
  :type '(choice (const :tag "Switch to root buffer" switch-to-root)
                 (const :tag "Bury chat buffers" bury-chat-buffers)
                 (const :tag "Kill chat buffers" kill-chat-buffers)
                 (const :tag "Disconnect from Telegram" disconnect))
  :group 'telega)

(defun telega-extras--on-idle ()
  "Perform `telega-extras-idle-action' when the session idle hook fires."
  (when (telega-server-live-p)
    (message "telega-extras: idle action: %s" telega-extras-idle-action)
    (pcase telega-extras-idle-action
      ('switch-to-root (telega-extras-switch-to-root))
      ('bury-chat-buffers (telega-extras-bury-chat-buffers))
      ('kill-chat-buffers (telega-extras-kill-chat-buffers))
      ('disconnect (telega-extras-disconnect)))))

(defun telega-extras-start-idle-timer ()
  "Register `telega-extras--on-idle' on the session idle hook and start the timer if needed."
  (sprite-session-add-on-idle #'telega-extras--on-idle))

(defun telega-extras-stop-idle-timer ()
  "Remove `telega-extras--on-idle' from the session idle hook and stop the timer if empty."
  (sprite-session-remove-on-idle #'telega-extras--on-idle))

(defun telega-extras-disconnect ()
  "Disconnect from Telegram.  No-op when the server is not live."
  (interactive)
  (when (telega-server-live-p)
    (telega-server-kill)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; systemd-logind PrepareForSleep integration
;;
;; Sleep always disconnects regardless of `telega-extras-idle-action': once
;; the lid closes the network is gone, so any lighter action would leave
;; telega in a broken state.

(defun telega-extras--logind-available-p ()
  "Return non-nil when systemd-logind DBus integration is available."
  (and (eq system-type 'gnu/linux)
       (fboundp 'dbus-register-signal)))

(defvar telega-extras--was-live-before-sleep nil
  "Non-nil when telega was connected at the last system sleep.
Used by `telega-extras--on-after-sleep' to decide whether to reconnect.")

(defvar telega-extras--sleeping nil
  "Non-nil while the system is asleep after a sleep-triggered disconnect.
Prevents `telega-extras-teardown' from unregistering the logind watch,
which must remain active to catch the wake signal.")

(defun telega-extras--on-prepare-for-sleep (going-to-sleep)
  "Disconnect telega before system sleep.
GOING-TO-SLEEP is t when entering sleep, nil on wake."
  (when going-to-sleep
    (telega-extras-disconnect)))

(defun telega-extras--on-before-sleep ()
  "Disconnect telega before system sleep or suspend."
  (setq telega-extras--was-live-before-sleep (telega-server-live-p))
  (when telega-extras--was-live-before-sleep
    (message "telega-extras: disconnecting before sleep/suspend")
    (setq telega-extras--sleeping t)
    (telega-extras-disconnect)))

(defun telega-extras--on-after-sleep ()
  "Reconnect telega after system wake if it was live before sleep."
  (setq telega-extras--sleeping nil)
  (when telega-extras--was-live-before-sleep
    (setq telega-extras--was-live-before-sleep nil)
    (message "telega-extras: scheduling reconnect after wake")
    (run-with-timer 3 nil #'telega)))

(defun telega-extras-start-logind-watch ()
  "Register sleep/wake handlers on session hooks and start watch."
  (add-hook 'sprite-session-before-sleep-hook #'telega-extras--on-before-sleep)
  (add-hook 'sprite-session-after-sleep-hook #'telega-extras--on-after-sleep)
  (sprite-session-start-logind-watch))

(defun telega-extras-stop-logind-watch ()
  "Remove sleep/wake handlers from session hooks and stop watch."
  (remove-hook 'sprite-session-before-sleep-hook #'telega-extras--on-before-sleep)
  (remove-hook 'sprite-session-after-sleep-hook #'telega-extras--on-after-sleep)
  (sprite-session-stop-logind-watch))

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
		   (resolve-plural-form (length chat-windows) "buffer" "buffers"))
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
       (get-buffer-create bootstrap-fallback-buffer-name))))

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

(defun telega-extras-root-default-for-instances (instance-ids)
  "Enable telega root as the default buffer on new frames for INSTANCE-IDS.
Call this from personal.el before telega loads.  When the running Emacs
instance (per `sprite-instance-name') is a member of INSTANCE-IDS,
`telega-extras-make-root-default' is registered on `telega-ready-hook'
and undone on `telega-kill-hook'.
Example: (telega-extras-root-default-for-instances \\='(\"telega\" \"primary\"))"
  (when (member (sprite-instance-name) instance-ids)
    (add-hook 'telega-ready-hook #'telega-extras-make-root-default)
    (add-hook 'telega-kill-hook #'telega-extras-remove-root-default)))

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
    (unless telega-extras--sleeping
      (telega-extras-stop-logind-watch))
    (advice-remove 'telega-notifications-msg-notify-p
                   #'telega-extras--notify-skip-own)))

(add-hook 'telega-ready-hook #'telega-extras-setup)
(add-hook 'telega-kill-hook #'telega-extras-teardown)

(provide 'telega-extras)
;;; telega-extras.el ends here
