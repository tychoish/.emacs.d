;; -*- lexical-binding: t; -*-
(defmacro with-gc-suppressed (&rest body)
  `(let ((gc-cons-threshold 800000000000000)
         (gc-cons-percentage 1.0))
     (prog1
	 (progn ,@body)
       (garbage-collect))))

(defmacro with-file-name-handler-disabled (&rest body)
  `(let ((file-name-handler-alist nil))
     ,@body))

(defvar slow-op-reporting debug-on-error
  "A toggle that, when enabled is supports more verbose timing reporting.
Turns `with-slow-op-timer' from a noop to reporting on the duration of enclosed operations.")

(defvar slow-op-threshold 0.005
  "Threshold in seconds, or fractions thereof. Controls the behavior of `with-slow-op-timer'. Any operation below this threshold (faster) are ignored. Use this to control verbosity.")

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
  (setq native-comp-async-report-warnings-errors 'silent)
  (setq native-comp-jit-compilation t)
  (setq native-compile-prune-cache t))
