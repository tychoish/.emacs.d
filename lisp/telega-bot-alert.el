;;; telega-bot-alert.el --- alert.el backend that delivers via telega-bot -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Registers a `telega-bot' output target for `alert.el': an alert style
;; that delivers notifications as Telegram messages through one or more
;; configured (bot, chat-id) targets. Multiple targets can be registered
;; at once, each combining a bot instance/name with the user or chat id
;; that should receive the alert.
;;
;; Usage:
;;
;;   (require 'telega-bot-alert)
;;
;;   (telega-bot-alert-register-target
;;    :name "personal"
;;    :bot 'my-bot
;;    :chat-id 12345678)
;;
;;   (telega-bot-alert-register-target
;;    :name "ops-channel"
;;    :bot 'ops-bot
;;    :chat-id -100123456789)
;;
;;   ;; Send to every registered target:
;;   (alert "Build finished" :style 'telega-bot :title "CI")
;;
;;   ;; Send to specific targets only:
;;   (alert "Deploy failed" :style 'telega-bot :title "CI"
;;          :data '(:targets ("ops-channel")))
;;
;;   ;; Or via the convenience wrapper:
;;   (telega-bot-alert "Deploy failed" :title "CI" :targets '("ops-channel"))

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'alert)
(require 'telega-bot)

(cl-defstruct (telega-bot-alert-target
               (:constructor telega-bot-alert--make-target))
  "A single telega-bot delivery target for alert.el notifications."
  (name nil :type string :documentation "Unique name identifying this target.")
  (bot nil :documentation "Bot name/symbol/instance registered via `telega-bot'.")
  (chat-id nil :documentation "Telegram chat id notifications are delivered to.")
  (thread-id nil :documentation "Optional forum thread id."))

(defvar telega-bot-alert-targets nil
  "Alist of registered `telega-bot-alert-target' instances, keyed by name.")

(defvar telega-bot-alert-default-targets nil
  "List of target names used when an alert does not request specific targets.
When nil, alerts without an explicit `:targets' selection are delivered to
every target in `telega-bot-alert-targets'.")

(cl-defun telega-bot-alert-register-target (&key name bot chat-id thread-id)
  "Register a telega-bot alert target NAME delivering to BOT/CHAT-ID/THREAD-ID."
  (unless (and name bot chat-id)
    (error "telega-bot-alert-register-target requires :name, :bot and :chat-id"))
  (let ((target (telega-bot-alert--make-target
                 :name name :bot bot :chat-id chat-id :thread-id thread-id)))
    (setf (map-elt telega-bot-alert-targets name) target)
    target))

(defun telega-bot-alert-unregister-target (name)
  "Remove the telega-bot alert target NAME."
  (setq telega-bot-alert-targets (map-delete telega-bot-alert-targets name)))

(defun telega-bot-alert-get-target (name)
  "Return the registered telega-bot alert target NAME, or nil."
  (map-elt telega-bot-alert-targets name))

(defun telega-bot-alert--target-names (info)
  "Resolve the target names an alert described by INFO should be sent to."
  (or (map-elt (map-elt info :data) :targets)
      telega-bot-alert-default-targets
      (map-keys telega-bot-alert-targets)))

(defun telega-bot-alert--format-message (info)
  "Format the Telegram text for an alert described by INFO."
  (let ((title (map-elt info :title))
        (message (map-elt info :message)))
    (if title
        (format "*%s*\n%s" title message)
      message)))

(defun telega-bot-alert--notify (info)
  "Deliver alert.el notification INFO to all resolved telega-bot targets."
  (let ((text (telega-bot-alert--format-message info)))
    (seq-do
     (lambda (name)
       (when-let* ((target (telega-bot-alert-get-target name)))
         (telega-bot-send-response text
                                   :bot (telega-bot-alert-target-bot target)
                                   :chat-id (telega-bot-alert-target-chat-id target)
                                   :thread-id (telega-bot-alert-target-thread-id target))))
     (telega-bot-alert--target-names info))))

(alert-define-style 'telega-bot :title "Deliver via telega-bot"
                    :notifier #'telega-bot-alert--notify)

(cl-defun telega-bot-alert (message &key title severity category targets)
  "Send MESSAGE as a telega-bot alert, optionally restricted to TARGETS."
  (alert message
        :title title
        :severity (or severity 'normal)
        :category category
        :style 'telega-bot
        :data (when targets (list :targets targets))))

(provide 'telega-bot-alert)
;;; telega-bot-alert.el ends here
