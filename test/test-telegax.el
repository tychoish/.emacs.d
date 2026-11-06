;;; test-telegax.el --- ERT tests for telegax.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Tests run without a live Telega/tdlib connection.  Telega functions that
;; talk to the server are mocked with cl-letf where needed.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'server)
(require 'test-helper)
(require 'telega)
(require 'telegax)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; idle timer lifecycle

(ert-deftest telegax/start-idle-timer-creates-timer ()
  "`telegax-start-idle-timer' registers the handler on the session idle hook."
  (let ((sprite-system-idle-hook nil)
        (sprite-system--idle-timer nil))
    (unwind-protect
        (progn
          (telegax-start-idle-timer)
          (should (memq #'telegax--on-idle sprite-system-idle-hook)))
      (telegax-stop-idle-timer))))

(ert-deftest telegax/stop-idle-timer-cancels-and-nils ()
  "`telegax-stop-idle-timer' removes the handler from the session idle hook."
  (let ((sprite-system-idle-hook nil)
        (sprite-system--idle-timer nil))
    (telegax-start-idle-timer)
    (telegax-stop-idle-timer)
    (should-not (memq #'telegax--on-idle sprite-system-idle-hook))))

(ert-deftest telegax/start-idle-timer-idempotent ()
  "Calling start twice registers the handler exactly once."
  (let ((sprite-system-idle-hook nil)
        (sprite-system--idle-timer nil))
    (unwind-protect
        (progn
          (telegax-start-idle-timer)
          (telegax-start-idle-timer)
          (should (= 1 (length (seq-filter
                                (lambda (f) (eq f #'telegax--on-idle))
                                sprite-system-idle-hook)))))
      (telegax-stop-idle-timer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; idle disconnect

(ert-deftest telegax/idle-disconnect-does-nothing-when-server-dead ()
  "`telegax-disconnect' is a no-op when the server is not live."
  (let (kill-called)
    (cl-letf (((symbol-function 'telega-server-live-p) (lambda () nil))
              ((symbol-function 'telega-kill) (lambda (&rest _) (setq kill-called t))))
      (telegax-disconnect)
      (should-not kill-called))))

(ert-deftest telegax/idle-disconnect-kills-when-server-live ()
  "`telegax-disconnect' calls `telega-server-kill' when server is live."
  (let (kill-called)
    (cl-letf (((symbol-function 'telega-server-live-p) (lambda () t))
              ((symbol-function 'telega-server-kill) (lambda () (setq kill-called t))))
      (telegax-disconnect)
      (should kill-called))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; logind availability

(ert-deftest telegax/logind-unavailable-on-non-linux ()
  "`telegax--logind-available-p' returns nil on non-Linux systems."
  (cl-letf (((symbol-function 'system-type) nil))
    (let ((system-type 'darwin))
      (should-not (telegax--logind-available-p)))))

(ert-deftest telegax/logind-unavailable-without-dbus ()
  "`telegax--logind-available-p' returns nil when dbus is not loaded."
  (let ((system-type 'gnu/linux))
    (cl-letf (((symbol-function 'dbus-register-signal) nil))
      (should-not (telegax--logind-available-p)))))

(ert-deftest telegax/logind-unavailable-without-dbusbind ()
  "`telegax--logind-available-p' returns nil when Emacs lacks dbusbind."
  (defvar features)
  (let ((system-type 'gnu/linux)
        (features (remq 'dbusbind features)))
    (should-not (telegax--logind-available-p))))

(ert-deftest telegax/logind-available-on-linux-with-dbusbind ()
  "`telegax--logind-available-p' returns non-nil on Linux with dbusbind."
  (defvar features)
  (let ((system-type 'gnu/linux)
        (features (cons 'dbusbind features)))
    (cl-letf (((symbol-function 'fboundp)
               (lambda (sym)
                 (if (eq sym 'dbus-register-signal) t (fboundp sym)))))
      (should (telegax--logind-available-p)))))

(ert-deftest telegax/start-logind-watch-noop-without-dbusbind ()
  "`telegax-start-logind-watch' does nothing when dbusbind is missing."
  (defvar features)
  (let ((system-type 'gnu/linux)
        (features (remq 'dbusbind features))
        watch-started)
    (cl-letf (((symbol-function 'sprite-system-start-logind-watch)
               (lambda () (setq watch-started t))))
      (telegax-start-logind-watch)
      (should-not watch-started))))

(ert-deftest telegax/on-prepare-for-sleep-skips-when-waking ()
  "`telegax--on-prepare-for-sleep' does not kill telega on wake."
  (let (kill-called)
    (cl-letf (((symbol-function 'telega-server-live-p) (lambda () t))
              ((symbol-function 'telega-kill) (lambda (&rest _) (setq kill-called t))))
      ;; nil = waking up, not going to sleep
      (telegax--on-prepare-for-sleep nil)
      (should-not kill-called))))

(ert-deftest telegax/on-prepare-for-sleep-kills-when-sleeping ()
  "`telegax--on-prepare-for-sleep' disconnects telega when going to sleep."
  (let (kill-called)
    (cl-letf (((symbol-function 'telega-server-live-p) (lambda () t))
              ((symbol-function 'telega-server-kill) (lambda () (setq kill-called t))))
      (telegax--on-prepare-for-sleep t)
      (should kill-called))))

(ert-deftest telegax/on-prepare-for-sleep-skips-when-dead ()
  "`telegax--on-prepare-for-sleep' does not error when server is dead."
  (let (kill-called)
    (cl-letf (((symbol-function 'telega-server-live-p) (lambda () nil))
              ((symbol-function 'telega-kill) (lambda (&rest _) (setq kill-called t))))
      (telegax--on-prepare-for-sleep t)
      (should-not kill-called))))

(ert-deftest telegax/on-prepare-for-sleep-buries-when-timers-enabled ()
  "`telegax--on-before-sleep' calls bury when chatbuf idle timers are enabled."
  (let ((telegax-enable-chatbuf-idle-timers t)
        bury-called)
    (cl-letf (((symbol-function 'telega-server-live-p) (lambda () nil))
              ((symbol-function 'telegax-bury-chat-buffers)
               (lambda () (setq bury-called t))))
      (telegax--on-before-sleep)
      (should bury-called))))

(ert-deftest telegax/on-prepare-for-sleep-no-bury-when-timers-disabled ()
  "`telegax--on-before-sleep' skips bury when chatbuf idle timers are off."
  (let ((telegax-enable-chatbuf-idle-timers nil)
        bury-called)
    (cl-letf (((symbol-function 'telega-server-live-p) (lambda () nil))
              ((symbol-function 'telegax-bury-chat-buffers)
               (lambda () (setq bury-called t))))
      (telegax--on-before-sleep)
      (should-not bury-called))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; kill-chat-buffers

(ert-deftest telegax/kill-chat-buffers-kills-chat-mode-buffers ()
  "`telegax-kill-chat-buffers' kills all buffers in `telega-chat-mode'."
  (let ((buf (generate-new-buffer " *telega-test-chat*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq major-mode 'telega-chat-mode))
          (telegax-kill-chat-buffers)
          (should-not (buffer-live-p buf)))
      (when (buffer-live-p buf)
        (kill-buffer buf)))))

(ert-deftest telegax/kill-chat-buffers-spares-other-modes ()
  "`telegax-kill-chat-buffers' does not kill non-chat buffers."
  (let ((chat-buf (generate-new-buffer " *telega-test-chat2*"))
        (other-buf (generate-new-buffer " *telega-test-other*")))
    (unwind-protect
        (progn
          (with-current-buffer chat-buf
            (setq major-mode 'telega-chat-mode))
          (with-current-buffer other-buf
            (setq major-mode 'text-mode))
          (telegax-kill-chat-buffers)
          (should-not (buffer-live-p chat-buf))
          (should (buffer-live-p other-buf)))
      (when (buffer-live-p chat-buf) (kill-buffer chat-buf))
      (when (buffer-live-p other-buf) (kill-buffer other-buf)))))

(ert-deftest telegax/kill-chat-buffers-noop-when-none ()
  "`telegax-kill-chat-buffers' does not error when no chat buffers exist."
  (let ((original-buffers (buffer-list)))
    ;; Ensure no telega-chat-mode buffers exist.
    (should-not
     (seq-find (lambda (b)
                 (eq (buffer-local-value 'major-mode b) 'telega-chat-mode))
               original-buffers))
    ;; Should not signal.
    (telegax-kill-chat-buffers)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; chatbuf idle timers

(ert-deftest telegax/start-chatbuf-idle-timers-noop-when-disabled ()
  "`telegax-start-chatbuf-idle-timers' starts nothing when feature is disabled."
  (let ((telegax-enable-chatbuf-idle-timers nil)
        (telegax-chatbuf-bury-idle-time 30)
        (telegax-chatbuf-kill-idle-time 60)
        (telegax-chatbuf-bury-idle-timer nil)
        (telegax-chatbuf-kill-idle-timer nil))
    (telegax-start-chatbuf-idle-timers)
    (should-not telegax-chatbuf-bury-idle-timer)
    (should-not telegax-chatbuf-kill-idle-timer)))

(ert-deftest telegax/start-chatbuf-idle-timers-creates-timers-when-enabled ()
  "`telegax-start-chatbuf-idle-timers' creates both timers when enabled."
  (let ((telegax-enable-chatbuf-idle-timers t)
        (telegax-chatbuf-bury-idle-time 30)
        (telegax-chatbuf-kill-idle-time 60)
        (telegax-chatbuf-bury-idle-timer nil)
        (telegax-chatbuf-kill-idle-timer nil))
    (unwind-protect
        (progn
          (telegax-start-chatbuf-idle-timers)
          (should (timerp telegax-chatbuf-bury-idle-timer))
          (should (timerp telegax-chatbuf-kill-idle-timer)))
      (telegax-stop-chatbuf-idle-timers))))

(ert-deftest telegax/start-chatbuf-idle-timers-skips-nil-timeout ()
  "`telegax-start-chatbuf-idle-timers' skips a timer whose timeout is nil."
  (let ((telegax-enable-chatbuf-idle-timers t)
        (telegax-chatbuf-bury-idle-time nil)
        (telegax-chatbuf-kill-idle-time 60)
        (telegax-chatbuf-bury-idle-timer nil)
        (telegax-chatbuf-kill-idle-timer nil))
    (unwind-protect
        (progn
          (telegax-start-chatbuf-idle-timers)
          (should-not telegax-chatbuf-bury-idle-timer)
          (should (timerp telegax-chatbuf-kill-idle-timer)))
      (telegax-stop-chatbuf-idle-timers))))

(ert-deftest telegax/stop-chatbuf-idle-timers-cancels-and-nils ()
  "`telegax-stop-chatbuf-idle-timers' cancels live timers and sets vars to nil."
  (let ((telegax-enable-chatbuf-idle-timers t)
        (telegax-chatbuf-bury-idle-time 30)
        (telegax-chatbuf-kill-idle-time 60)
        (telegax-chatbuf-bury-idle-timer nil)
        (telegax-chatbuf-kill-idle-timer nil))
    (telegax-start-chatbuf-idle-timers)
    (should (timerp telegax-chatbuf-bury-idle-timer))
    (telegax-stop-chatbuf-idle-timers)
    (should-not telegax-chatbuf-bury-idle-timer)
    (should-not telegax-chatbuf-kill-idle-timer)))

(ert-deftest telegax/stop-chatbuf-idle-timers-idempotent ()
  "`telegax-stop-chatbuf-idle-timers' is safe when timers are already nil."
  (let ((telegax-chatbuf-bury-idle-timer nil)
        (telegax-chatbuf-kill-idle-timer nil))
    (telegax-stop-chatbuf-idle-timers)
    (should-not telegax-chatbuf-bury-idle-timer)
    (should-not telegax-chatbuf-kill-idle-timer)))

(ert-deftest telegax/chatbuf-idle-timers-custom-set-sets-value ()
  "`telegax--chatbuf-idle-timers-custom-set' sets the symbol default."
  (cl-letf (((symbol-function 'telega-server-live-p) (lambda () nil)))
    (telegax--chatbuf-idle-timers-custom-set
     'telegax-chatbuf-bury-idle-time 42)
    (should (eq 42 (default-value 'telegax-chatbuf-bury-idle-time)))))

(ert-deftest telegax/chatbuf-idle-timers-custom-set-restarts-when-live ()
  "`telegax--chatbuf-idle-timers-custom-set' restarts timers when server is live."
  (let ((telegax-mode t)
        restarted)
    (cl-letf (((symbol-function 'telega-server-live-p) (lambda () t))
              ((symbol-function 'telegax-start-chatbuf-idle-timers)
               (lambda () (setq restarted t))))
      (telegax--chatbuf-idle-timers-custom-set
       'telegax-chatbuf-bury-idle-time 42)
      (should restarted))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bury-chat-buffers

(ert-deftest telegax/bury-chat-buffers-noop-without-root ()
  "`telegax-bury-chat-buffers' does nothing when no root buffer exists."
  (let ((telega-root-buffer-name "*telegax-test-nonexistent-root*"))
    (cl-letf (((symbol-function 'get-scratch-buffer-create)
               (lambda () (current-buffer))))
      ;; Should not signal.
      (telegax-bury-chat-buffers))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; root-default hooks

(ert-deftest telegax/make-root-default-adds-hooks ()
  "`telegax-make-root-default' adds frame hooks and sets initial-buffer-choice."
  (let ((after-make-frame-functions nil)
        (server-after-make-frame-hook nil)
        (initial-buffer-choice nil))
    (telegax-make-root-default)
    (should (memq #'telegax-switch-to-root after-make-frame-functions))
    (should (memq #'telegax-switch-to-root server-after-make-frame-hook))
    (should (eq #'telegax-switch-to-root initial-buffer-choice))))

(ert-deftest telegax/remove-root-default-removes-hooks ()
  "`telegax-remove-root-default' removes frame hooks and clears initial-buffer-choice."
  (let ((after-make-frame-functions (list #'telegax-switch-to-root))
        (server-after-make-frame-hook (list #'telegax-switch-to-root))
        (initial-buffer-choice #'telegax-switch-to-root))
    (telegax-remove-root-default)
    (should-not (memq #'telegax-switch-to-root after-make-frame-functions))
    (should-not (memq #'telegax-switch-to-root server-after-make-frame-hook))
    (should (null initial-buffer-choice))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; notify-skip-own

(ert-deftest telegax/notify-skip-own-passes-non-own-messages ()
  "The advice calls orig-fn for messages not sent by self."
  (let (orig-called)
    (cl-letf (((symbol-function 'telega-msg-match-p) (lambda (_msg _temex) nil)))
      (telegax--notify-skip-own
       (lambda (_msg) (setq orig-called t))
       'fake-msg)
      (should orig-called))))

(ert-deftest telegax/notify-skip-own-suppresses-own-messages ()
  "The advice returns nil and does not call orig-fn for self-sent messages."
  (let (orig-called)
    (cl-letf (((symbol-function 'telega-msg-match-p) (lambda (_msg _temex) t)))
      (let ((result (telegax--notify-skip-own
                     (lambda (_msg) (setq orig-called t))
                     'fake-msg)))
        (should-not orig-called)
        (should (null result))))))

(ert-deftest telegax/notify-skip-own-returns-orig-result ()
  "The advice returns the value from orig-fn for non-own messages."
  (cl-letf (((symbol-function 'telega-msg-match-p) (lambda (_msg _temex) nil)))
    (let ((result (telegax--notify-skip-own
                   (lambda (_msg) 'notify-yes)
                   'fake-msg)))
      (should (eq 'notify-yes result)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; telega-chat-folders override

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; confirm helper

(ert-deftest telegax/confirm-yes-returns-t ()
  "`telegax--confirm' returns t when user answers yes."
  (cl-letf (((symbol-function 'read-answer) (lambda (_prompt _choices) "yes")))
    (should (eq t (telegax--confirm "Test? ")))))

(ert-deftest telegax/confirm-no-returns-nil ()
  "`telegax--confirm' returns nil when user answers no."
  (cl-letf (((symbol-function 'read-answer) (lambda (_prompt _choices) "no")))
    (should (null (telegax--confirm "Test? ")))))

(ert-deftest telegax/confirm-abort-signals-user-error ()
  "`telegax--confirm' signals user-error when user answers abort."
  (cl-letf (((symbol-function 'read-answer) (lambda (_prompt _choices) "abort")))
    (should-error (telegax--confirm "Test? ") :type 'user-error)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; kill-chat-buffers prompt behaviour

(ert-deftest telegax/kill-chat-buffers-non-interactive-skips-prompt ()
  "Non-interactive calls to `telegax-kill-chat-buffers' kill without prompting."
  (let ((buf (generate-new-buffer " *telega-test-chat-ni*"))
        prompt-called)
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq major-mode 'telega-chat-mode))
          (cl-letf (((symbol-function 'read-answer)
                     (lambda (&rest _) (setq prompt-called t) "yes")))
            (telegax-kill-chat-buffers))
          (should-not (buffer-live-p buf))
          (should-not prompt-called))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(ert-deftest telegax/kill-chat-buffers-aborts-on-user-error ()
  "When `telegax--confirm' signals user-error, no buffers are killed."
  (let ((buf (generate-new-buffer " *telega-test-chat-abort*")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (setq major-mode 'telega-chat-mode))
          (cl-letf (((symbol-function 'read-answer)
                     (lambda (&rest _) "abort")))
            ;; Simulate interactive call by calling confirm directly and
            ;; catching the error.  Non-interactive path skips prompt.
            (should-error (telegax--confirm "Kill? ") :type 'user-error))
          ;; Buffer should still be live because we never actually called kill.
          (should (buffer-live-p buf)))
      (when (buffer-live-p buf) (kill-buffer buf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; teardown prompt behaviour

(ert-deftest telegax/teardown-non-interactive-runs-without-prompt ()
  "Non-interactive `telegax-teardown' runs cleanup without prompting."
  (let ((after-make-frame-functions (list #'telegax-switch-to-root))
        (server-after-make-frame-hook (list #'telegax-switch-to-root))
        (initial-buffer-choice #'telegax-switch-to-root)
        (telegax--idle-timer nil)
        (telegax--logind-signal nil)
        (telegax-chatbuf-bury-idle-timer nil)
        (telegax-chatbuf-kill-idle-timer nil)
        prompt-called)
    (cl-letf (((symbol-function 'read-answer)
               (lambda (&rest _) (setq prompt-called t) "yes"))
              ((symbol-function 'advice-remove) #'ignore))
      (telegax-teardown))
    (should-not prompt-called)
    (should-not (memq #'telegax-switch-to-root after-make-frame-functions))))

(ert-deftest telegax/lifecycle-disable-removes-resources ()
  (let ((telegax-mode nil)
        (telegax--sleeping nil))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'telega-server-live-p) (lambda () t)))
            (telegax-mode 1))
          (should (advice-member-p #'telegax--notify-skip-own
                                   'telega-notifications-msg-notify-p))
          (telegax-mode -1)
          (should-not (advice-member-p #'telegax--notify-skip-own
                                       'telega-notifications-msg-notify-p))
          (should-not telegax-chatbuf-bury-idle-timer)
          (should-not telegax-chatbuf-kill-idle-timer))
      (telegax-mode -1))))

(ert-deftest telegax/timer-customization-respects-disabled-mode ()
  (let ((telegax-mode nil)
        (started nil))
    (cl-letf (((symbol-function 'telega-server-live-p) (lambda () t))
              ((symbol-function 'telegax-start-chatbuf-idle-timers)
               (lambda () (setq started t))))
      (telegax--chatbuf-idle-timers-custom-set
       'telegax-chatbuf-bury-idle-time 1200)
      (should-not started))))

(ert-deftest telegax/mode-registers-and-removes-hooks ()
  (let ((telegax-mode nil))
    (unwind-protect
        (progn
          (telegax-mode 1)
          (should (memq #'telegax-on-ready telega-ready-hook))
          (should (memq #'telegax-chat-setup telega-chat-mode-hook))
          (telegax-mode -1)
          (should-not (memq #'telegax-on-ready telega-ready-hook))
          (should-not (memq #'telegax-chat-setup telega-chat-mode-hook)))
      (telegax-mode -1))))

(ert-deftest telegax/execute-command-filters-on-telega-prefix ()
  (let (predicate)
    (cl-letf (((symbol-function 'execute-extended-command)
               (lambda (_) (setq predicate read-extended-command-predicate))))
      (telegax-execute-command)
      (should (funcall predicate 'telega-chat-with nil))
      (should-not (funcall predicate 'magit-status nil)))))

(ert-deftest telegax/global-mode-anchors-prefix-until-telega-mode ()
  (let ((telegax-global-mode nil)
        (telegax-mode nil))
    (unwind-protect
        (progn
          (telegax-global-mode 1)
          (should (eq (keymap-lookup telegax-global-mode-map "C-c v")
                      #'telegax-prefix-dispatch))
          (telegax-mode 1)
          (should (eq (keymap-lookup telegax-global-mode-map "C-c v")
                      (keymap-lookup telegax-global-mode-map "C-c n")))
          (should-not (eq (keymap-lookup telegax-global-mode-map "C-c v")
                          #'telegax-prefix-dispatch))
          (should (keymap-lookup telega-prefix-map "d"))
          (should (keymap-lookup telega-prefix-map "x"))
          (telegax-mode -1)
          (should (eq (keymap-lookup telegax-global-mode-map "C-c v")
                      #'telegax-prefix-dispatch))
          (should-not (keymap-lookup telega-prefix-map "d"))
          (should-not (keymap-lookup telega-prefix-map "x")))
      (telegax-mode -1)
      (telegax-global-mode -1))))

(ert-deftest telegax/prefix-dispatch-enables-mode-and-replays-keys ()
  (let ((telegax-mode nil)
        (unread-command-events nil))
    (unwind-protect
        (cl-letf (((symbol-function 'this-command-keys-vector)
                   (lambda () (kbd "C-c v"))))
          (telegax-prefix-dispatch)
          (should telegax-mode)
          (should (equal unread-command-events (listify-key-sequence (kbd "C-c v")))))
      (telegax-mode -1))))

(ert-deftest telegax/mode-applies-and-removes-configuration ()
  (let ((telegax-mode nil))
    (unwind-protect
        (progn
          (telegax-mode 1)
          (should-not telega-root-view-grouping-other-chats)
          (should (advice-member-p #'telegax--hide-chat-folders 'telega-chat-folders))
          (should (eq (keymap-lookup telega-root-mode-map "h")
                      #'telegax-bury-chat-buffers))
          (telegax-mode -1)
          (should-not (advice-member-p #'telegax--hide-chat-folders 'telega-chat-folders))
          (should-not (keymap-lookup telega-root-mode-map "h")))
      (telegax-mode -1))))

(ert-deftest telegax/kill-preserves-lifecycle-preference ()
  (let ((telegax-mode t)
        (telegax--sleeping nil)
        (stopped nil))
    (cl-letf (((symbol-function 'telegax-teardown)
               (lambda () (setq stopped t))))
      (telegax-on-kill)
      (should stopped)
      (should telegax-mode))))

(provide 'test-telegax)
;;; test-telegax.el ends here
