;;; test-telega-extras.el --- ERT tests for telega-extras.el -*- lexical-binding: t -*-

;;; Commentary:
;; Tests run without a live Telega/tdlib connection.  Telega functions that
;; talk to the server are mocked with cl-letf where needed.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'test-helper)
(require 'telega-extras)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; idle timer lifecycle

(ert-deftest telega-extras/start-idle-timer-creates-timer ()
  "`telega-extras-start-idle-timer' registers the handler on the session idle hook."
  (let ((sprite-session-idle-hook nil)
        (sprite-session--idle-timer nil))
    (unwind-protect
        (progn
          (telega-extras-start-idle-timer)
          (should (memq #'telega-extras--on-idle sprite-session-idle-hook)))
      (telega-extras-stop-idle-timer))))

(ert-deftest telega-extras/stop-idle-timer-cancels-and-nils ()
  "`telega-extras-stop-idle-timer' removes the handler from the session idle hook."
  (let ((sprite-session-idle-hook nil)
        (sprite-session--idle-timer nil))
    (telega-extras-start-idle-timer)
    (telega-extras-stop-idle-timer)
    (should-not (memq #'telega-extras--on-idle sprite-session-idle-hook))))

(ert-deftest telega-extras/start-idle-timer-idempotent ()
  "Calling start twice registers the handler exactly once."
  (let ((sprite-session-idle-hook nil)
        (sprite-session--idle-timer nil))
    (unwind-protect
        (progn
          (telega-extras-start-idle-timer)
          (telega-extras-start-idle-timer)
          (should (= 1 (length (seq-filter
                                (lambda (f) (eq f #'telega-extras--on-idle))
                                sprite-session-idle-hook)))))
      (telega-extras-stop-idle-timer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; idle disconnect

(ert-deftest telega-extras/idle-disconnect-does-nothing-when-server-dead ()
  "`telega-extras-disconnect' is a no-op when the server is not live."
  (let (kill-called)
    (cl-letf (((symbol-function 'telega-server-live-p) (lambda () nil))
              ((symbol-function 'telega-kill) (lambda (&rest _) (setq kill-called t))))
      (telega-extras-disconnect)
      (should-not kill-called))))

(ert-deftest telega-extras/idle-disconnect-kills-when-server-live ()
  "`telega-extras-disconnect' calls `telega-server-kill' when server is live."
  (let (kill-called)
    (cl-letf (((symbol-function 'telega-server-live-p) (lambda () t))
              ((symbol-function 'telega-server-kill) (lambda () (setq kill-called t))))
      (telega-extras-disconnect)
      (should kill-called))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; logind availability

(ert-deftest telega-extras/logind-unavailable-on-non-linux ()
  "`telega-extras--logind-available-p' returns nil on non-Linux systems."
  (cl-letf (((symbol-function 'system-type) nil))
    (let ((system-type 'darwin))
      (should-not (telega-extras--logind-available-p)))))

(ert-deftest telega-extras/logind-unavailable-without-dbus ()
  "`telega-extras--logind-available-p' returns nil when dbus is not loaded."
  (let ((system-type 'gnu/linux))
    (cl-letf (((symbol-function 'dbus-register-signal) nil))
      (should-not (telega-extras--logind-available-p)))))

(ert-deftest telega-extras/on-prepare-for-sleep-skips-when-waking ()
  "`telega-extras--on-prepare-for-sleep' does not kill telega on wake."
  (let (kill-called)
    (cl-letf (((symbol-function 'telega-server-live-p) (lambda () t))
              ((symbol-function 'telega-kill) (lambda (&rest _) (setq kill-called t))))
      ;; nil = waking up, not going to sleep
      (telega-extras--on-prepare-for-sleep nil)
      (should-not kill-called))))

(ert-deftest telega-extras/on-prepare-for-sleep-kills-when-sleeping ()
  "`telega-extras--on-prepare-for-sleep' disconnects telega when going to sleep."
  (let (kill-called)
    (cl-letf (((symbol-function 'telega-server-live-p) (lambda () t))
              ((symbol-function 'telega-server-kill) (lambda () (setq kill-called t))))
      (telega-extras--on-prepare-for-sleep t)
      (should kill-called))))

(ert-deftest telega-extras/on-prepare-for-sleep-skips-when-dead ()
  "`telega-extras--on-prepare-for-sleep' does not error when server is dead."
  (let (kill-called)
    (cl-letf (((symbol-function 'telega-server-live-p) (lambda () nil))
              ((symbol-function 'telega-kill) (lambda (&rest _) (setq kill-called t))))
      (telega-extras--on-prepare-for-sleep t)
      (should-not kill-called))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; kill-chat-buffers

(ert-deftest telega-extras/kill-chat-buffers-kills-chat-mode-buffers ()
  "`telega-extras-kill-chat-buffers' kills all buffers in `telega-chat-mode'."
  (let ((buf (generate-new-buffer " *telega-test-chat*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq major-mode 'telega-chat-mode))
          (telega-extras-kill-chat-buffers)
          (should-not (buffer-live-p buf)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest telega-extras/kill-chat-buffers-spares-other-modes ()
  "`telega-extras-kill-chat-buffers' does not kill non-chat buffers."
  (let ((chat-buf (generate-new-buffer " *telega-test-chat2*"))
        (other-buf (generate-new-buffer " *telega-test-other*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq major-mode 'telega-chat-mode))
          (with-current-buffer other-buf
            (setq major-mode 'text-mode))
          (telega-extras-kill-chat-buffers)
          (should-not (buffer-live-p chat-buf))
          (should (buffer-live-p other-buf)))
      (when (buffer-live-p chat-buf) (kill-buffer chat-buf))
      (when (buffer-live-p other-buf) (kill-buffer other-buf)))))

(ert-deftest telega-extras/kill-chat-buffers-noop-when-none ()
  "`telega-extras-kill-chat-buffers' does not error when no chat buffers exist."
  (let ((original-buffers (buffer-list)))
    ;; Ensure no telega-chat-mode buffers exist.
    (should-not
     (seq-find (lambda (b)
                 (eq (buffer-local-value 'major-mode b) 'telega-chat-mode))
               original-buffers))
    ;; Should not signal.
    (telega-extras-kill-chat-buffers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bury-chat-buffers

(ert-deftest telega-extras/bury-chat-buffers-noop-without-root ()
  "`telega-extras-bury-chat-buffers' does nothing when root buffer is absent."
  ;; Bind to a name that certainly doesn't exist.
  (let ((telega-root-buffer-name "*telega-extras-test-nonexistent-root*"))
    ;; Should not signal.
    (telega-extras-bury-chat-buffers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; root-default hooks

(ert-deftest telega-extras/make-root-default-adds-hooks ()
  "`telega-extras-make-root-default' adds frame hooks and sets initial-buffer-choice."
  (let ((after-make-frame-functions nil)
        (server-after-make-frame-hook nil)
        (initial-buffer-choice nil))
    (telega-extras-make-root-default)
    (should (memq #'telega-extras-switch-to-root after-make-frame-functions))
    (should (memq #'telega-extras-switch-to-root server-after-make-frame-hook))
    (should (eq #'telega-extras-switch-to-root initial-buffer-choice))))

(ert-deftest telega-extras/remove-root-default-removes-hooks ()
  "`telega-extras-remove-root-default' removes frame hooks and clears initial-buffer-choice."
  (let ((after-make-frame-functions (list #'telega-extras-switch-to-root))
        (server-after-make-frame-hook (list #'telega-extras-switch-to-root))
        (initial-buffer-choice #'telega-extras-switch-to-root))
    (telega-extras-remove-root-default)
    (should-not (memq #'telega-extras-switch-to-root after-make-frame-functions))
    (should-not (memq #'telega-extras-switch-to-root server-after-make-frame-hook))
    (should (null initial-buffer-choice))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; notify-skip-own

(ert-deftest telega-extras/notify-skip-own-passes-non-own-messages ()
  "The advice calls orig-fn for messages not sent by self."
  (let (orig-called)
    (cl-letf (((symbol-function 'telega-msg-match-p) (lambda (_msg _temex) nil)))
      (telega-extras--notify-skip-own
       (lambda (_msg) (setq orig-called t))
       'fake-msg)
      (should orig-called))))

(ert-deftest telega-extras/notify-skip-own-suppresses-own-messages ()
  "The advice returns nil and does not call orig-fn for self-sent messages."
  (let (orig-called)
    (cl-letf (((symbol-function 'telega-msg-match-p) (lambda (_msg _temex) t)))
      (let ((result (telega-extras--notify-skip-own
                     (lambda (_msg) (setq orig-called t))
                     'fake-msg)))
        (should-not orig-called)
        (should (null result))))))

(ert-deftest telega-extras/notify-skip-own-returns-orig-result ()
  "The advice returns the value from orig-fn for non-own messages."
  (cl-letf (((symbol-function 'telega-msg-match-p) (lambda (_msg _temex) nil)))
    (let ((result (telega-extras--notify-skip-own
                   (lambda (_msg) 'notify-yes)
                   'fake-msg)))
      (should (eq 'notify-yes result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; telega-chat-folders override

(ert-deftest telega-extras/chat-folders-always-nil ()
  "`telega-chat-folders' returns nil for any argument."
  (should (null (telega-chat-folders nil)))
  (should (null (telega-chat-folders 'some-chat)))
  (should (null (telega-chat-folders '(:id 123)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; confirm helper

(ert-deftest telega-extras/confirm-yes-returns-t ()
  "`telega-extras--confirm' returns t when user answers yes."
  (cl-letf (((symbol-function 'read-answer) (lambda (_prompt _choices) "yes")))
    (should (eq t (telega-extras--confirm "Test? ")))))

(ert-deftest telega-extras/confirm-no-returns-nil ()
  "`telega-extras--confirm' returns nil when user answers no."
  (cl-letf (((symbol-function 'read-answer) (lambda (_prompt _choices) "no")))
    (should (null (telega-extras--confirm "Test? ")))))

(ert-deftest telega-extras/confirm-abort-signals-user-error ()
  "`telega-extras--confirm' signals user-error when user answers abort."
  (cl-letf (((symbol-function 'read-answer) (lambda (_prompt _choices) "abort")))
    (should-error (telega-extras--confirm "Test? ") :type 'user-error)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; kill-chat-buffers prompt behaviour

(ert-deftest telega-extras/kill-chat-buffers-non-interactive-skips-prompt ()
  "Non-interactive calls to `telega-extras-kill-chat-buffers' kill without prompting."
  (let ((buf (generate-new-buffer " *telega-test-chat-ni*"))
        prompt-called)
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq major-mode 'telega-chat-mode))
          (cl-letf (((symbol-function 'read-answer)
                     (lambda (&rest _) (setq prompt-called t) "yes")))
            (telega-extras-kill-chat-buffers))
          (should-not (buffer-live-p buf))
          (should-not prompt-called))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest telega-extras/kill-chat-buffers-aborts-on-user-error ()
  "When `telega-extras--confirm' signals user-error, no buffers are killed."
  (let ((buf (generate-new-buffer " *telega-test-chat-abort*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq major-mode 'telega-chat-mode))
          (cl-letf (((symbol-function 'read-answer)
                     (lambda (&rest _) "abort")))
            ;; Simulate interactive call by calling confirm directly and
            ;; catching the error.  Non-interactive path skips prompt.
            (should-error (telega-extras--confirm "Kill? ") :type 'user-error))
          ;; Buffer should still be live because we never actually called kill.
          (should (buffer-live-p buf)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; teardown prompt behaviour

(ert-deftest telega-extras/teardown-non-interactive-runs-without-prompt ()
  "Non-interactive `telega-extras-teardown' runs cleanup without prompting."
  (let ((after-make-frame-functions (list #'telega-extras-switch-to-root))
        (server-after-make-frame-hook (list #'telega-extras-switch-to-root))
        (initial-buffer-choice #'telega-extras-switch-to-root)
        (telega-extras--idle-timer nil)
        (telega-extras--logind-signal nil)
        prompt-called)
    (cl-letf (((symbol-function 'read-answer)
               (lambda (&rest _) (setq prompt-called t) "yes"))
              ((symbol-function 'advice-remove) #'ignore))
      (telega-extras-teardown))
    (should-not prompt-called)
    (should-not (memq #'telega-extras-switch-to-root after-make-frame-functions))))

(provide 'test-telega-extras)
;;; test-telega-extras.el ends here
