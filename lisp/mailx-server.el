;;; mailx-server.el --- Mu server lifecycle for mail contexts -*- lexical-binding: t; -*-

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

(defvar mailx--transitioning nil
  "Non-nil while a database switch is stopping the old mu server.
Stop waits for process output, so timers or filters can re-enter mail
commands; entry points that would start a server refuse while this is set.")

(defvar mailx--stopping-process nil
  "Mu server process still exiting after mu4e detached from it.")

(advice-add 'mu4e--server-start :before #'mailx--await-stopped-server)
(advice-add 'mu4e--server-queries :before-while #'mailx--server-running-p)
(advice-add 'mu4e-update-index :before #'mailx--start-for-index)

(defun mailx--start-for-index (&rest _args)
  "Initialize mu4e before an index update starts a stopped server."
  (when mailx--transitioning
    (user-error "Wait for the mail database switch before indexing"))
  (unless (mu4e-running-p)
    (mu4e 'background)))

(defun mailx--server-running-p (&rest _args)
  "Return non-nil when mu4e's server is running."
  (and (fboundp 'mu4e-running-p) (mu4e-running-p)))

(defun mailx-server-current-home ()
  "Return the database home of the running server, or the configured home."
  (if (and (mu4e-running-p) (processp mu4e--server-process))
      (when-let* ((arg (seq-find (lambda (item) (string-prefix-p "--muhome=" item))
                                 (process-command mu4e--server-process))))
        (substring arg (length "--muhome=")))
    mu4e-mu-home))

(defun mailx--await-server-ready ()
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

(defun mailx-server-ensure-ready ()
  "Start the configured mu server if necessary and wait until it is ready."
  (unless (mu4e-running-p)
    (mu4e 'background))
  (mailx--await-server-ready))

(defun mailx--await-stopped-server (&rest _args)
  "Wait for a detached mu server to exit before starting another."
  (when mailx--stopping-process
    (let ((deadline (+ (float-time) 5)))
      (seq-some (lambda (_attempt)
                  (or (not (process-live-p mailx--stopping-process))
                      (>= (float-time) deadline)
                      (progn (accept-process-output mailx--stopping-process 0.1)
                             nil)))
                (number-sequence 1 50000)))
    (when (process-live-p mailx--stopping-process)
      (error "Previous mu server did not exit; try again after it stops"))
    (setq mailx--stopping-process nil)))

(defun mailx--stop-server ()
  "Stop mu4e and wait until its detached process exits."
  (let ((process mu4e--server-process)
        (mu4e-confirm-quit nil))
    (setq mailx--stopping-process process)
    (unwind-protect
        (mu4e-quit)
      ;; A process still attached to mu4e was not detached by the quit.
      (when (and process (eq mu4e--server-process process))
        (setq mailx--stopping-process nil)))
    (when (mu4e-running-p)
      (error "Failed to stop the previous mu server"))
    (mailx--await-stopped-server)))

(defun mailx-server--ensure-no-drafts ()
  "Refuse a database change while any mu4e draft remains open."
  (when-let* ((draft (seq-find (lambda (buffer)
                                (with-current-buffer buffer
                                  (derived-mode-p 'mu4e-compose-mode)))
                              (buffer-list))))
    (user-error "Save and close mail draft %s before switching databases"
                (buffer-name draft))))

(defun mailx-server-switch-database (home)
  "Select database HOME, stopping the current mu server when necessary.
The server remains stopped until mu4e is next started."
  (mailx--await-stopped-server)
  (let* ((mailx--transitioning t)
         (running (and (featurep 'mu4e) (mu4e-running-p)))
         (old-home (mailx-server-current-home)))
    (unless (equal old-home home)
      (mailx-server--ensure-no-drafts)
      (when (or mu4e--server-indexing
                (and (buffer-live-p mu4e--update-buffer)
                     (process-live-p (get-buffer-process mu4e--update-buffer))))
        (user-error "Cannot switch mail databases while an update is running"))
      (when running
        (mailx--stop-server)
        (mailx-server--ensure-no-drafts))
      (setq mu4e--started nil))
    (setq mu4e-mu-home home)))

(provide 'mailx-server)
;;; mailx-server.el ends here
