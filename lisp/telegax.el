;;; telegax.el --- Configurable idle actions and lifecycle extras for telega -*- lexical-binding: t; -*-

;;; Commentary:
;; Configurable idle actions, systemd-logind sleep integration, buffer
;; management, notification filtering and the static configuration for the
;; telega Telegram client.
;;
;; Trigger mechanisms (idle timer, logind PrepareForSleep) are handled by
;; `sprite-system'; this file registers telega-specific responses on those
;; hooks and provides per-instance configuration helpers for personal.el.
;;
;; Settings and key bindings are applied by `telegax-mode'.  The global
;; `telegax-global-mode' (enabled at startup) anchors the telega prefix
;; keys without loading telega; the first use enables `telegax-mode', which
;; loads telega and manages the lifecycle hooks, timers and advice.

;;; Code:

(require 'sprite)
(require 'sprite-system)
(require 'alert)
(require 'map)

;; external: xtdlib and minaid
(declare-function resolve-plural-form "xtdlib")
(declare-function minaid-corfu-text-mode-setup "minaid")

;; external: telega (loaded by `telegax-mode')
(declare-function telega "telega")
(declare-function telega-kill "telega")
(declare-function telega-server-live-p "telega-server")
(declare-function telega-server-kill "telega-server")
(declare-function telega-chat-auto-fill-mode "telega-chat")
(declare-function telega-chat-at "telega-chat")
(declare-function telega-chat-match-p "telega-chat")
(declare-function telega-chat-button-toggle-view "telega-chat")
(declare-function telega-msg-match-p "telega-msg")
(declare-function telega-root-next-important "telega-root")
(declare-function telega-root-next-mention "telega-root")
(declare-function telega-root-next-reaction "telega-root")
(declare-function telega-root-next-unread "telega-root")
(declare-function telega-mode-line-mode "telega-modeline")
(declare-function telega-alert-mode "telega-alert")
(defvar telega-debug)
(defvar telega-root-buffer-name)
(defvar telega-prefix-map)
(defvar telega-root-mode-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Configurable idle action

(defcustom telegax-idle-action 'switch-to-root
  "Action taken when `sprite-system-idle-hook' fires while telega is live.
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

(defun telegax--chatbuf-idle-timers-custom-set (symbol value)
  "Set SYMBOL to VALUE and update the chatbuf idle timers if needed."
  (set-default symbol value)
  (when (and (bound-and-true-p telegax-mode)
             (telega-server-live-p))
    (telegax-start-chatbuf-idle-timers)))

(defcustom telegax-enable-chatbuf-idle-timers t
  "When non-nil, enable idle timers for burying and killing chat buffers.
These timers run when their respective timeouts are specified in
`telegax-chatbuf-bury-idle-time' and
`telegax-chatbuf-kill-idle-time'."
  :type 'boolean
  :set #'telegax--chatbuf-idle-timers-custom-set
  :group 'telega)

(defcustom telegax-chatbuf-bury-idle-time 1200
  "Inactivity time in seconds after which a telega chat buffer is buried.
Only has effect when `telegax-enable-chatbuf-idle-timers' is non-nil."
  :type '(choice (const :tag "Never" nil)
                 (integer :tag "Seconds"))
  :set #'telegax--chatbuf-idle-timers-custom-set
  :group 'telega)

(defcustom telegax-chatbuf-kill-idle-time 14400
  "Inactivity time in seconds after which a telega chat buffer is killed.
Only has effect when `telegax-enable-chatbuf-idle-timers' is non-nil."
  :type '(choice (const :tag "Never" nil)
                 (integer :tag "Seconds"))
  :set #'telegax--chatbuf-idle-timers-custom-set
  :group 'telega)

(defvar telegax-chatbuf-bury-idle-timer nil
  "Idle timer for burying telega chat buffers.")

(defvar telegax-chatbuf-kill-idle-timer nil
  "Idle timer for killing telega chat buffers.")

(defun telegax-start-chatbuf-idle-timers ()
  "Start the chat buffer idle timers if enabled and timeouts are specified."
  (telegax-stop-chatbuf-idle-timers)
  (when telegax-enable-chatbuf-idle-timers
    (when (and (numberp telegax-chatbuf-bury-idle-time)
               (> telegax-chatbuf-bury-idle-time 0))
      (setq telegax-chatbuf-bury-idle-timer
            (run-with-idle-timer telegax-chatbuf-bury-idle-time t
                                 #'telegax-bury-chat-buffers)))
    (when (and (numberp telegax-chatbuf-kill-idle-time)
               (> telegax-chatbuf-kill-idle-time 0))
      (setq telegax-chatbuf-kill-idle-timer
            (run-with-idle-timer telegax-chatbuf-kill-idle-time t
                                 #'telegax-kill-chat-buffers)))))

(defun telegax-stop-chatbuf-idle-timers ()
  "Stop and clear the chat buffer idle timers."
  (when telegax-chatbuf-bury-idle-timer
    (cancel-timer telegax-chatbuf-bury-idle-timer)
    (setq telegax-chatbuf-bury-idle-timer nil))
  (when telegax-chatbuf-kill-idle-timer
    (cancel-timer telegax-chatbuf-kill-idle-timer)
    (setq telegax-chatbuf-kill-idle-timer nil)))

(defun telegax--on-idle ()
  "Perform `telegax-idle-action' when the session idle hook fires."
  (when (telega-server-live-p)
    (message "telegax: idle action: %s" telegax-idle-action)
    (pcase telegax-idle-action
      ('switch-to-root (telegax-switch-to-root))
      ('bury-chat-buffers (telegax-bury-chat-buffers))
      ('kill-chat-buffers (telegax-kill-chat-buffers))
      ('disconnect (telegax-disconnect)))))

(defun telegax-start-idle-timer ()
  "Register the idle handler, starting the session timer if needed."
  (sprite-system-add-on-idle #'telegax--on-idle))

(defun telegax-stop-idle-timer ()
  "Remove the idle handler, stopping the session timer if empty."
  (sprite-system-remove-on-idle #'telegax--on-idle))

;;;###autoload
(defun telegax-disconnect ()
  "Disconnect from Telegram.  No-op when the server is not live."
  (interactive)
  (when (telega-server-live-p)
    (telega-server-kill)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; systemd-logind PrepareForSleep integration
;;
;; Sleep always disconnects regardless of `telegax-idle-action': once
;; the lid closes the network is gone, so any lighter action would leave
;; telega in a broken state.

(defun telegax--logind-available-p ()
  "Return non-nil when systemd-logind DBus integration is available."
  (and (eq system-type 'gnu/linux)
       (featurep 'dbusbind)
       (fboundp 'dbus-register-signal)))

(defvar telegax--was-live-before-sleep nil
  "Non-nil when telega was connected at the last system sleep.
Used by `telegax--on-after-sleep' to decide whether to reconnect.")

(defvar telegax--reconnect-timer nil "Pending reconnect after wake, if any.")

(defvar telegax--sleeping nil
  "Non-nil while the system is asleep after a sleep-triggered disconnect.
Prevents `telegax-teardown' from unregistering the logind watch,
which must remain active to catch the wake signal.")

(defun telegax--on-prepare-for-sleep (going-to-sleep)
  "Disconnect telega before system sleep.
GOING-TO-SLEEP is t when entering sleep, nil on wake."
  (when going-to-sleep
    (telegax-disconnect)))

(defun telegax--on-before-sleep ()
  "Disconnect before sleep and bury chat buffers if enabled."
  (setq telegax--was-live-before-sleep (telega-server-live-p))
  (when telegax--was-live-before-sleep
    (message "telegax: disconnecting before sleep/suspend")
    (setq telegax--sleeping t)
    (telegax-disconnect))
  (when telegax-enable-chatbuf-idle-timers
    (telegax-bury-chat-buffers)))

(defun telegax--on-after-sleep ()
  "Reconnect telega after system wake if it was live before sleep."
  (setq telegax--sleeping nil)
  (when telegax--was-live-before-sleep
    (setq telegax--was-live-before-sleep nil)
    (message "telegax: scheduling reconnect after wake")
    (setq telegax--reconnect-timer (run-with-timer 3 nil #'telegax--reconnect))))

(defun telegax-start-logind-watch ()
  "Register sleep/wake handlers on session hooks and start watch."
  (when (telegax--logind-available-p)
    (add-hook 'sprite-system-before-sleep-hook #'telegax--on-before-sleep)
    (add-hook 'sprite-system-after-sleep-hook #'telegax--on-after-sleep)
    (sprite-system-start-logind-watch)))

(defun telegax-stop-logind-watch ()
  "Remove sleep/wake handlers from session hooks and stop watch."
  (remove-hook 'sprite-system-before-sleep-hook #'telegax--on-before-sleep)
  (remove-hook 'sprite-system-after-sleep-hook #'telegax--on-after-sleep)
  (when (and (null sprite-system-before-sleep-hook)
             (null sprite-system-after-sleep-hook))
    (sprite-system-stop-logind-watch)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Confirmation helper

(defun telegax--confirm (prompt)
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

(defvar-keymap telegax-buffer-management-map
  :name "telega-buffer-management"
  :doc "Buffer management keymap under telega prefix C-c v d.")

(keymap-set telegax-buffer-management-map "h" #'telegax-bury-chat-buffers)
(keymap-set telegax-buffer-management-map "k" #'telegax-kill-chat-buffers)

(defun telegax-bury-chat-buffers ()
  "Bury all live telega chat buffers and replace visible ones with the root buffer."
  (interactive)
  (let* ((root (or (get-buffer telega-root-buffer-name)
                   (get-scratch-buffer-create)))
         (chat-bufs (seq-filter (lambda (b)
                                  (with-current-buffer b
                                    (derived-mode-p 'telega-chat-mode)))
                                (buffer-list)))
         (chat-windows
          (thread-last (frame-list)
            (seq-mapcat #'window-list)
            (seq-filter (lambda (w)
                          (with-current-buffer (window-buffer w)
                            (derived-mode-p 'telega-chat-mode)))))))
    (when (or chat-windows chat-bufs)
      (mapc (lambda (w)
              (set-window-buffer w root))
            chat-windows)
      (mapc #'bury-buffer chat-bufs)
      (alert (format "buried %d telega-chat-%s"
                     (length chat-bufs)
                     (resolve-plural-form (length chat-bufs) "buffer" "buffers"))
             :title (format "emacs.%s.telega" (sprite-instance-name))
             :persistent t))))

(defun telegax-kill-chat-buffers ()
  "Kill all live telega-chat-mode buffers, prompting when interactive."
  (interactive)
  (when-let* ((bufs (thread-last (buffer-list)
                      (seq-filter (lambda (b)
                                    (with-current-buffer b
                                      (derived-mode-p 'telega-chat-mode))))))
              (_ (or (not (called-interactively-p 'interactive))
                     (telegax--confirm
                      (format "Kill %d telega chat buffer%s? "
                              (length bufs)
                              (if (= 1 (length bufs)) "" "s"))))))
    (mapc #'kill-buffer bufs)))

;;;###autoload
(defun telegax-force-kill ()
  "Kill Telega without confirmation."
  (interactive)
  (telega-kill t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Root buffer as default for new frames

;;;###autoload
(defun telegax-switch-to-root (&optional _frame)
  "Switch to the Telega Root buffer, falling back to a sensible buffer."
  (interactive)
  (switch-to-buffer
   (or (get-buffer telega-root-buffer-name)
       (last-buffer)
       (get-scratch-buffer-create))))

(defvar telegax--previous-initial-buffer-choice nil
  "Initial buffer choice before enabling Telega as the default.")

(defun telegax-make-root-default ()
  "Make `telegax-switch-to-root' the default buffer for new frames."
  (add-hook 'after-make-frame-functions #'telegax-switch-to-root)
  (add-hook 'server-after-make-frame-hook #'telegax-switch-to-root)
  (unless (eq initial-buffer-choice #'telegax-switch-to-root)
    (setq telegax--previous-initial-buffer-choice initial-buffer-choice))
  (setq initial-buffer-choice #'telegax-switch-to-root))

(defun telegax-remove-root-default ()
  "Undo the effect of `telegax-make-root-default'."
  (remove-hook 'after-make-frame-functions #'telegax-switch-to-root)
  (remove-hook 'server-after-make-frame-hook #'telegax-switch-to-root)
  (when (eq initial-buffer-choice #'telegax-switch-to-root)
    (setq initial-buffer-choice telegax--previous-initial-buffer-choice)))

(defun telegax-root-default-for-instances (instance-ids)
  "Enable telega root as the default buffer on new frames for INSTANCE-IDS.
Call this from personal.el before telega loads.  When the running Emacs
instance (per `sprite-instance-name') is a member of INSTANCE-IDS,
`telegax-make-root-default' is registered on `telega-ready-hook'
and undone on `telega-kill-hook'.
Example: (telegax-root-default-for-instances \\='(\"telega\" \"primary\"))"
  (when (member (sprite-instance-name) instance-ids)
    (add-hook 'telega-ready-hook #'telegax-make-root-default)
    (add-hook 'telega-kill-hook #'telegax-remove-root-default)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Root buffer navigation

(defun telegax-root-cycle-next (chat)
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

(defun telegax--notify-skip-own (orig-fn msg)
  "Skip desktop notification when MSG was sent by self; otherwise call ORIG-FN."
  (unless (telega-msg-match-p msg '(sender me))
    (funcall orig-fn msg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Debug toggle

(defun telegax-toggle-debug ()
  "Toggle `telega-debug'."
  (interactive)
  (setq telega-debug (not telega-debug))
  (message "telega-debug %s" (if telega-debug "enabled" "disabled")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Command filter

;;;###autoload
(defun telegax-execute-command ()
  "Run `execute-extended-command' offering only commands prefixed \"telega\"."
  (interactive)
  (let ((read-extended-command-predicate
         (lambda (command _buffer)
           (string-prefix-p "telega" (symbol-name command)))))
    (with-suppressed-warnings ((interactive-only execute-extended-command))
      (execute-extended-command nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Configuration, applied when `telegax-mode' is enabled

(defun telegax--hide-chat-folders (_chat)
  "Suppress folder annotations for CHAT."
  nil)

(defconst telegax-settings
  `((telega-server-libs-prefix . ,(if (eq system-type 'darwin) "/opt/homebrew" "/usr"))
    (telega-emoji-use-images . t)
    (telega-use-images . t)
    (telega-chat-input-markups . ("markdown2"))
    (telega-use-tracking-for . (or unmuted mention unread))
    (telega-markdown2-backquotes-as-precode . t)
    (telega-debug . nil)
    (telega-chat--display-buffer-action
     . ((display-buffer-reuse-window display-buffer-use-some-window)))
    (telega-chat-auto-fill-mode-lighter . "")
    (telega-root-view-grouping-folders . t)
    (telega-folder-icons-alist . nil)
    (telega-root-view-grouping-other-chats . nil)
    (telega-root-view-grouping-alist . (("open" . has-chatbuf)
                                        ("personal" chat-list "personal")
                                        ("groups" type basicgroup supergroup)))
    (telega-filter-custom-show-folders . nil)
    (telega-filter-button-width . (.2 12 16))
    (telega-filters-custom . (("main" main)
                              ("groups" type basicgroup supergroup)
                              ("broadcast" type channel)
                              ("archive" archive)))
    (telega-root-view-top-categories . (("Users" . 10) ("Groups" . 10)
                                        ("Channels" . 10) ("Bots" . 10)
                                        ("InlineBots" . 10) ("Calls" . 10)))
    (telega-chat-folders-exclude . ("unknowns" "InlineBots" "Calls" "Personal"))
    (telega-chat-folders-insexp . telega-folders-insert-default))
  "Telega variable settings applied when `telegax-mode' is enabled.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Lifecycle

(defun telegax-chat-setup ()
  "Set up completion and filling in a telega chat buffer."
  (minaid-corfu-text-mode-setup)
  (telega-chat-auto-fill-mode 1))

(defun telegax--reconnect ()
  "Reconnect to Telegram after waking."
  (setq telegax--reconnect-timer nil)
  (telega))

(defun telegax-setup ()
  "Start the Telega lifecycle integrations."
  (telegax-start-idle-timer)
  (telegax-start-logind-watch)
  (telegax-start-chatbuf-idle-timers)
  (advice-add 'telega-notifications-msg-notify-p
              :around #'telegax--notify-skip-own))

(defun telegax-teardown ()
  "Stop the Telega lifecycle integrations."
  (interactive)
  (telegax-remove-root-default)
  (telegax-stop-idle-timer)
  (telegax-stop-chatbuf-idle-timers)
  (unless telegax--sleeping (telegax-stop-logind-watch))
  (advice-remove 'telega-notifications-msg-notify-p #'telegax--notify-skip-own))

(defun telegax-on-ready ()
  "Start lifecycle integrations when Telega is ready."
  (telegax-setup))

(defun telegax-on-kill ()
  "Stop lifecycle integrations when Telega disconnects, except during sleep."
  (unless telegax--sleeping
    (telegax-teardown)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Modes

(defvar-keymap telegax-global-mode-map
  :doc "Anchors for `telega-prefix-map' while `telegax-global-mode' is on.")

(defun telegax-prefix-dispatch ()
  "Enable `telegax-mode', then replay the keys that invoked this command.
Stands in for `telega-prefix-map' until telega has been loaded."
  (interactive)
  (telegax-mode 1)
  (setq unread-command-events
        (append (listify-key-sequence (this-command-keys-vector))
                unread-command-events)))

(keymap-set telegax-global-mode-map "C-c v" #'telegax-prefix-dispatch)
(keymap-set telegax-global-mode-map "C-c n" #'telegax-prefix-dispatch)

;;;###autoload
(define-minor-mode telegax-global-mode
  "Bind the telega prefix keys, loading telega on first use."
  :global t
  :group 'telega
  :lighter nil
  :keymap telegax-global-mode-map)

;;;###autoload
(define-minor-mode telegax-mode
  "Configure telega and manage its chat, idle, sleep and notification integrations."
  :global t
  :group 'telega
  :lighter nil
  (require 'telega)
  (require 'telega-server)
  (if telegax-mode
      (progn
        (require 'tracking)
        (require 'telega-alert)
        (map-do #'set telegax-settings)
        (when (eq system-type 'darwin)
          (setq-default alert-default-style 'osx-notifier))
        (advice-add 'telega-chat-folders :override #'telegax--hide-chat-folders)
        (keymap-set telegax-global-mode-map "C-c v" (cons "telega" telega-prefix-map))
        (keymap-set telegax-global-mode-map "C-c n" (cons "telega" telega-prefix-map))
        (keymap-set telega-prefix-map "d" (cons "buffers" telegax-buffer-management-map))
        (keymap-set telega-prefix-map "x" (cons "telega-commands" #'telegax-execute-command))
        (keymap-set telega-root-mode-map "<tab>" #'telegax-root-cycle-next)
        (keymap-set telega-root-mode-map "h" #'telegax-bury-chat-buffers)
        (keymap-set telega-root-mode-map "C-k" #'telegax-kill-chat-buffers)
        (keymap-set telega-root-mode-map "d" #'telegax-disconnect)
        (add-hook 'telega-chat-mode-hook #'telegax-chat-setup)
        (add-hook 'telega-ready-hook #'telegax-on-ready)
        (add-hook 'telega-kill-hook #'telegax-on-kill)
        (telega-mode-line-mode 1)
        (telega-alert-mode 1)
        (when (telega-server-live-p)
          (telegax-setup)))
    (telega-alert-mode -1)
    (telega-mode-line-mode -1)
    (advice-remove 'telega-chat-folders #'telegax--hide-chat-folders)
    (keymap-set telegax-global-mode-map "C-c v" #'telegax-prefix-dispatch)
    (keymap-set telegax-global-mode-map "C-c n" #'telegax-prefix-dispatch)
    (keymap-unset telega-prefix-map "d" t)
    (keymap-unset telega-prefix-map "x" t)
    (keymap-unset telega-root-mode-map "<tab>" t)
    (keymap-unset telega-root-mode-map "h" t)
    (keymap-unset telega-root-mode-map "C-k" t)
    (keymap-unset telega-root-mode-map "d" t)
    (remove-hook 'telega-chat-mode-hook #'telegax-chat-setup)
    (remove-hook 'telega-ready-hook #'telegax-on-ready)
    (remove-hook 'telega-kill-hook #'telegax-on-kill)
    (setq telegax--sleeping nil
          telegax--was-live-before-sleep nil)
    (when telegax--reconnect-timer
      (cancel-timer telegax--reconnect-timer)
      (setq telegax--reconnect-timer nil))
    (telegax-teardown)))

(provide 'telegax)
;;; telegax.el ends here
