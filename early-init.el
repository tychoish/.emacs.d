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

(defvar slow-op-reporting nil
  "When non-nil, `with-slow-op-timer' logs any operation that exceeds `slow-op-threshold'.
Always enabled at 500ms so genuinely blocking startup work is surfaced without needing
a special flag.  Set to nil in user/*.el to suppress reporting on a specific machine.")

(defvar slow-op-threshold 0.001
  "Minimum duration in seconds for `with-slow-op-timer' to emit a log message.
Set to 0.1 (100ms) to surface meaningfully slow operations without noise.")

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
