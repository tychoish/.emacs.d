;;; tychoish-setuputils.el --- utilities used during setup

;; Author: tychoish
;; Maintainer: tychoish
;; Version: 1.0-pre
;; Package-Requires: ()
;; Homepage: https://github.com/tychoish/.eamcs.d
;; Keywords: emacs startup dotemacs config

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package holds the functions I want to call directly during
;; start up, as well as functions useful for building configuration
;; that don't make sense to define elsewhere.  As this file needs to
;; load during startup my goal is to keep it as short as possible, and
;; acknowledging that the collection of functionality might need to be
;; slightly more eclectic than many other packages.

;;; Code:

(defvar tychoish-backup-directory (expand-file-name (concat user-emacs-directory "backups/"))
  "Defines a location for auto-save backup files.
This value is used to set both 'auto-save-filename-transforms'
and 'backup-directory-alist' by the `tychoish-set-backup-directory'
function which is called during startup.  Use `tychoish-set-backup-directory'
to change the value of this variable.")

(defun tychoish-set-backup-directory (path)
  "Set the backup directory to PATH and configure appropriately."
  (setq tychoish-backup-directory path)
  (setq auto-save-file-name-transforms `((".*" ,tychoish-backup-directory t)))
  (add-to-list 'backup-directory-alist (cons "." tychoish-backup-directory))

  (unless (file-exists-p tychoish-backup-directory)
    (make-directory tychoish-backup-directory))
  (chmod tychoish-backup-directory #o700))

(defmacro with-timer (name &rest body)
  "Report on NAME and the time taken to execute BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%s: %.06fs" ,name (float-time (time-since time)))))

(defmacro with-slow-op-timer (name threshold &rest body)
  "Send a message the BODY operation of NAME takes longer to execute than the THRESHOLD."
  `(let ((time (current-time)))
     ,@body
     (tychoish--threshold-logger ,threshold (time-to-seconds (time-since time)) ,name)))

(defun tychoish--threshold-logger (threshold duration name)
  "Send a message with the DURATION and NAME when the duration is over the THRESHOLD."
  (when (>  duration threshold)
    (message "%s: %.06fs" name duration)))

(defun gui-p ()
  "Return t when the current session is or may be a GUI session."
  (when (or (daemonp) (window-system))
    t))

(defun default-string (default input)
  "Return the DEFAULT value if the INPUT is empty or nil."
  (cond
   ((string-equal default input)
    default)
   ((eq input nil)
    default)
   ((string-equal input "")
    default)
   (t
    input)))

(defun tychoish-setup-global-modes ()
  "Set up useful global modes, for use at the end of the setup process."
  (fringe-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (delete-selection-mode 1)
  (show-paren-mode t)
  (transient-mark-mode 1)
  (column-number-mode t)
  (electric-pair-mode 1)

  (when (gui-p)
    (which-key-mode 1))

  (when (daemonp)
    (let ((gc-cons-threshold 800000))
      (session-initialize)
      (recentf-mode 1)
      (desktop-save-mode 1))))

(defun tychoish-setup-font (name number)
  (interactive "sName: \nNSize: ")
  (let ((new-font-name (concat name "-" (number-to-string number))))
    (set-face-attribute 'default nil :font new-font-name)
    (add-to-list 'default-frame-alist (cons 'font new-font-name))))

(defun tychoish-get-config-file-prefix (name)
  "Build a config file basename, for NAME.
This combines the host name and the dameon name."
  (format "%s-%s-%s" (system-name) (default-string "generic" (daemonp)) name))

(defun tychoish-get-config-file-path (name)
  "Return an absolute path for NAME in the configuration directory.
The is unique to the system and daemon instance."
  (concat (expand-file-name user-emacs-directory) (tychoish-get-config-file-prefix name)))

(defun tychoish-setup-user-local-config ()
  "Ensure that all config files in the '~/.emacs.d/user' are loaded."
  (let ((dirname (concat (expand-file-name user-emacs-directory) "user")))
    (when (file-accessible-directory-p dirname)
      (add-to-list 'load-path dirname)

      (mapc (lambda (fn)
	      (when (string-match-p "\\.el$" fn)
		(with-slow-op-timer (format "loading user config [%s]" fn) 0.10
		 (require (intern (string-remove-suffix ".el" fn))))))
	    (directory-files dirname))) t))

(defun display-startup-echo-area-message ()
  "Called during setup, intentially a noop, which omit the message."
  nil)

(provide 'tychoish-setuputils)
;;; tychoish-setuputils.el ends here
