;;; tychoish-backup -- configuration for backup files

;;; Commentary:

;; This file contains configuration and setup for Emacs' auto-save
;; backup directory configuration, and was inspired both by a desire
;; to package more parts of the unique configuration as proper
;; packages, and also in response to this report
;; <https://github.com/tychoish/.emacs.d/issues/3>, which notes that
;; the directory has broader permissions that required.

;;; Code:

(defvar tychoish-backup-directory (expand-file-name (concat user-emacs-directory "backups/"))
  "Defines a location for auto-save backup files.
This value is used to set both 'auto-save-filename-transforms'
and 'backup-directory-alist' by the `tychoish-set-backup-directory'
function which is called during startup.  Use `tychoish-set-backup-directory'
to change the value of this variable.")

(setq backup-by-copying t)
(setq make-backup-files t)

(defun tychoish-set-backup-directory (path)
  "Set the backup directory to PATH and configure appropriately."
  (setq tychoish-backup-directory path)
  (setq auto-save-file-name-transforms `((".*" ,tychoish-backup-directory t)))
  (add-to-list 'backup-directory-alist (cons "." tychoish-backup-directory))

  (unless (file-exists-p tychoish-backup-directory)
    (make-directory tychoish-backup-directory))
  (chmod tychoish-backup-directory #o700))

(tychoish-set-backup-directory tychoish-backup-directory)

(provide 'tychoish-backup)
;;; tychoish-backup.el ends here
