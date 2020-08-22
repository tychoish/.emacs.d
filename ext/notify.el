;;; notify.el --- notification front-end

;; Copyright (C) 2008  Mark A. Hershberger

;; Original Author: Mark A. Hershberger <mhersberger@intrahealth.org>
;; Modified by Andrey Kotlarski <m00naticus@gmail.com>
;; Modified by Andrew Gwozdziewycz <git@apgwoz.com>
;; Modified by Aidan Gauland <aidalgol@no8wireless.co.nz> October 2011
;; Keywords: extensions, convenience, lisp

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This provides a single function, `notify', that will produce a notify
;; pop-up via D-Bus, libnotify, simple message or growl.
;; To use, just put (autoload 'notify "notify" "Notify TITLE, BODY.")
;;  in your init file.  You may override default chosen notification
;;  method by assigning `notify-method' to one of 'notify-via-dbus
;; 'notify-via-libnotify or 'notify-via-message
;;; Code:

(defvar notify-defaults (list :app "Emacs" :icon "emacs" :timeout 5000
			      :urgency "normal"
			      :category "emacs.message")
  "Notification settings' defaults.
May be overridden with key-value additional arguments to `notify'.")
(defvar notify-last-notification '(0 0 0) "Time of last notification.")
(defvar notify-method 'notify-via-libnotify
  "Notification method among, 'notify-via-libnotify, 'notify-via-message or 'notify-via-growl")

;; determine notification method unless already set
;; prefer libnotify > growl > message

;; tychoish modified to remove dbus and add sardis

(cond
 ((executable-find "notify-send")
  (setq notify-method 'notify-via-libnotify))
 ((executable-find "sardis")
  (setq notify-method 'notify-via-sardis))
 ((executable-find "growlnotify")
  (setq notify-method 'notify-via-growl))
 (t
  (setq notify-method 'notify-via-message)))

(defun notify-via-sardis (title body)
  "Notify TITLE BODY with a sardis message"
  (call-process "sardis" nil 0 nil
		"--level" "debug" "notify" "send" title body))

(defun notify-via-libnotify (title body)
  "Notify with TITLE, BODY via `libnotify'."
  (call-process "notify-send" nil 0 nil
		title body "-t"
		(number-to-string (get 'notify-defaults :timeout))
		"-i" (get 'notify-defaults :icon)
		"-u" (get 'notify-defaults :urgency)
		"-c" (get 'notify-defaults :category)))

(defun notify-via-message (title body)
  "Notify TITLE, BODY with a simple message."
  (message "%s: %s" title body))

(defun notify-via-growl (title body)
  "Notify TITLE, BODY with a growl"
  (call-process "growlnotify" nil 0 nil
		"-a" (get 'notify-defaults :app)
		"-t" (notify-via-growl-stringify title)
		"-m" (notify-via-growl-stringify body)))

(defun notify-via-sardis (title body)
  "Notify TITLE BODY with a sardis message"
  (call-process "sardis" nil 0 t
		"xmpp" "send" title body))

(defun notify-via-growl-stringify (thing)
  (cond ((null thing) "")
	((stringp thing) thing)
	(t (format "%s" thing))))

(defun keywords-to-properties (symbol args &optional defaults)
  "Add to SYMBOL's property list key-values from ARGS and DEFAULTS."
  (when (consp defaults)
    (keywords-to-properties symbol defaults))
  (while args
    (put symbol (car args) (cadr args))
    (setq args (cddr args))))

;;;###autoload
(defun notify (title body &rest args)
  "Notify TITLE, BODY via `notify-method'. ARGS may be amongst
:timeout, :icon, :urgency, :app and :category."
  ;; get defaults
  (or (eq notify-method 'notify-via-message)
      (keywords-to-properties 'notify-defaults args
			      notify-defaults))
  ;; call relevant notification
    (funcall notify-method title body))

;;;###autoload
(defun notify-send (text)
  "send a notification from this emacs instance"
  (let ((source (if (daemonp)
		    (concat "emacs-" (daemonp))
		  "emacs")))
    (message (concat "notify: " text))
    (notify source text)))

(provide 'notify)
;;; notify.el ends here
