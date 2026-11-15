;;; tychoish-mail-server.el --- Mu server lifecycle for mail contexts -*- lexical-binding: t; -*-

;;; Commentary:
;; Start, stop, and switch mu databases while tracking server readiness.

;;; Code:

(require 'seq)

(autoload 'mu4e "mu4e" nil t)
(autoload 'mu4e-quit "mu4e" nil t)
(declare-function mu4e-running-p "mu4e-server")

(defvar mu4e-mu-home)
(defvar mu4e--started)
(defvar mu4e--server-indexing)
(defvar mu4e--update-buffer)
(defvar mu4e-confirm-quit)
(defvar mu4e-context-policy)
(defvar mu4e--server-process)
(defvar mu4e-pong-func)

(defvar tychoish-mail--server-home nil
  "Database home of the current mu server, or nil when not running.")

(defvar tychoish-mail--transitioning nil
  "Non-nil while starting a replacement mu server.")

(defvar tychoish-mail--stopping-process nil
  "Mu server process still exiting after mu4e detached from it.")

(advice-add 'mu4e--server-start :before #'tychoish-mail--await-stopped-server)
(advice-add 'mu4e--server-start :after #'tychoish-mail--record-server-home)

(defun tychoish-mail--record-server-home (&rest _args)
  "Remember the home used to start the running mu server."
  (when (mu4e-running-p)
    (setq tychoish-mail--server-home mu4e-mu-home)))

(defun tychoish-mail--await-server-ready ()
  "Wait until the running mu server has completed its startup handler."
  (unless mu4e--started
    (let* ((ready nil)
           (deadline (+ (float-time) 10))
           (handler mu4e-pong-func)
           (wrapper (lambda (info)
                      (funcall handler info)
                      (setq ready t))))
      (unwind-protect
          (progn
            (setq mu4e-pong-func wrapper)
            (seq-some (lambda (_attempt)
                        (or ready
                            (not (mu4e-running-p))
                            (>= (float-time) deadline)
                            (progn (accept-process-output mu4e--server-process 0.1)
                                   nil)))
                      (number-sequence 1 100)))
        (when (eq mu4e-pong-func wrapper)
          (setq mu4e-pong-func handler)))
      (unless (and ready (mu4e-running-p))
        (error "Mu server did not become ready for %s" mu4e-mu-home)))))

(defun tychoish-mail-server-ensure-ready ()
  "Start the configured mu server if necessary and wait until it is ready."
  (unless (mu4e-running-p)
    (mu4e 'background))
  (tychoish-mail--await-server-ready))

(defun tychoish-mail--await-stopped-server (&rest _args)
  "Wait for a detached mu server to exit before starting another."
  (when tychoish-mail--stopping-process
    (let ((deadline (+ (float-time) 5)))
      (seq-some (lambda (_attempt)
                  (or (not (process-live-p tychoish-mail--stopping-process))
                      (>= (float-time) deadline)
                      (progn (accept-process-output tychoish-mail--stopping-process 0.1)
                             nil)))
                (number-sequence 1 50000)))
    (when (process-live-p tychoish-mail--stopping-process)
      (error "Previous mu server did not exit; try again after it stops"))
    (setq tychoish-mail--stopping-process nil)))

(defun tychoish-mail--stop-server ()
  "Stop mu4e and wait until its detached process exits."
  (let ((process mu4e--server-process)
        (mu4e-confirm-quit nil))
    (setq tychoish-mail--stopping-process process)
    (unwind-protect
        (mu4e-quit)
      ;; A process still attached to mu4e was not detached by the quit.
      (when (and process (eq mu4e--server-process process))
        (setq tychoish-mail--stopping-process nil)))
    (when (mu4e-running-p)
      (error "Failed to stop the previous mu server"))
    (setq tychoish-mail--server-home nil)
    (tychoish-mail--await-stopped-server)))

(defun tychoish-mail-server--ensure-no-drafts ()
  "Refuse a database change while any mu4e draft remains open."
  (when-let* ((draft (seq-find (lambda (buffer)
                                (with-current-buffer buffer
                                  (derived-mode-p 'mu4e-compose-mode)))
                              (buffer-list))))
    (user-error "Save and close mail draft %s before switching databases"
                (buffer-name draft))))

(defun tychoish-mail-server-switch-database (home)
  "Prepare the mu database at HOME before entering its context.
Open drafts keep their original paths and are guarded during save and send."
  (tychoish-mail--await-stopped-server)
  (let* ((tychoish-mail--transitioning t)
         (running (and (featurep 'mu4e) (mu4e-running-p)))
         (old-home (if running
                       (or tychoish-mail--server-home mu4e-mu-home)
                     mu4e-mu-home)))
    (unless (equal old-home home)
      (tychoish-mail-server--ensure-no-drafts)
      (when (or mu4e--server-indexing
                (and (buffer-live-p mu4e--update-buffer)
                     (process-live-p (get-buffer-process mu4e--update-buffer))))
        (user-error "Cannot switch mail databases while an update is running"))
      (when running
        (tychoish-mail--stop-server)
        (tychoish-mail-server--ensure-no-drafts))
      (setq tychoish-mail--server-home nil)
      (unless running
        (setq mu4e--started nil)))
    (setq mu4e-mu-home home)
    (when (and running (not (equal old-home home)))
      ;; Startup completes on pong, not when `mu4e' returns.
      (condition-case err
          (let ((mu4e-context-policy nil))
            (tychoish-mail-server-ensure-ready)
            (setq tychoish-mail--server-home home))
        ((error quit)
         (unwind-protect
             (when (mu4e-running-p)
               (tychoish-mail--stop-server))
           (setq mu4e--started nil)
           (if (mu4e-running-p)
               (progn
                 (setq tychoish-mail--server-home home)
                 (setq mu4e-mu-home home))
             (setq tychoish-mail--server-home nil)
             (setq mu4e-mu-home old-home)))
         (signal (car err) (cdr err)))))))

(provide 'tychoish-mail-server)
;;; tychoish-mail-server.el ends here
