;;; early-init.el --- GC suppression, native-comp, frame defaults -*- lexical-binding: t; -*-

;;; Commentary:
;; Macro utilities for startup optimization (with-gc-suppressed, with-slow-op-timer, etc.),
;; native compilation settings, and early frame configuration.

;;; Code:
(defmacro with-gc-suppressed (&rest body)
  `(let ((gc-cons-threshold 800000000000000)
         (gc-cons-percentage 1.0))
     (prog1
	 (progn ,@body)
       (garbage-collect))))

(defmacro with-file-name-handler-disabled (&rest body)
  `(let ((file-name-handler-alist nil))
     ,@body))

(defvar slow-op-reporting (and (member "--with-slow-op-timing" command-line-args) t)
  "When non-nil, `with-slow-op-timer' logs any operation that exceeds `slow-op-threshold'.
Off by default; pass --with-slow-op-timing on the command line to enable it.
Detected here against `command-line-args' -- rather than via
`command-line-functions' in init.el, where the flag is normally handled --
because `command-line-1' only dispatches `command-line-functions' after
`after-init-hook' has already run.  By then init.el and every module it
requires (bootstrap, tychoish-core, tychoish-mail, orgx, user/*.el)
have already loaded once with reporting off, so every `with-slow-op-timer'
call in the main synchronous init path would go unmeasured.")

(defvar slow-op-threshold 0.0001
  "Minimum duration in seconds for `with-slow-op-timer' to emit a log message.")

(defmacro with-slow-op-timer (name &rest body)
  "Send a message the BODY operation of NAME takes longer to execute than a hardcoded threshold."
  (declare (indent defun) (debug t))
  `(if (not slow-op-reporting)
       (progn ,@body)
     (let* ((inhibit-message t)
	    (time (current-time))
	    (return-value (progn ,@body))
	    (duration (time-to-seconds (time-since time))))
       (when (> duration slow-op-threshold)
	 (message "[op]: %s: %.06fs" ,name duration))
       return-value)))

(setq package-enable-at-startup nil)

(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)

(when (string-match "NATIVE_COMP" system-configuration-features)
  (setcar native-comp-eln-load-path (expand-file-name "~/.cache/emacs/eln/"))
  (setq native-comp-async-report-warnings-errors nil)
  (setq native-comp-jit-compilation t)
  (setq native-compile-prune-cache t))

(provide 'early-init)
;;; early-init.el ends here
