;;; early-init.el --- GC suppression, native-comp, frame defaults -*- lexical-binding: t; -*-

;;; Commentary:
;; Macro utilities for startup optimization (with-gc-suppressed, with-slow-op-timer, etc.),
;; native compilation settings, and early frame configuration.

;;; Code:

(defvar bootstrap--gc-idle-timer nil
  "Debounced idle timer for running deferred garbage collection.")

(defun bootstrap-schedule-idle-gc (&optional delay)
  "Schedule garbage collection to run DELAY seconds after Emacs becomes idle."
  (unless noninteractive
    (when (timerp bootstrap--gc-idle-timer)
      (cancel-timer bootstrap--gc-idle-timer))
    (setq bootstrap--gc-idle-timer
          (run-with-idle-timer (or delay 16) nil
                               (lambda ()
                                 (setq bootstrap--gc-idle-timer nil)
                                 (garbage-collect))))))

(defmacro with-gc-suppressed (&rest body)
  "Execute BODY with garbage collection temporarily suppressed.
Defers cleanup to an idle timer after execution completes."
  `(let ((gc-cons-threshold 8000000000000000)
         (file-name-handler-alist nil)
         (gc-cons-percentage 1.0))
     (prog1
	 (progn ,@body)
       (bootstrap-schedule-idle-gc 8))))

(defvar slow-op-reporting (and (member "--with-slow-op-timing" command-line-args) t)
  "When non-nil, `with-slow-op-timer' logs any operation that exceeds
`slow-op-threshold'.")

(defvar slow-op-threshold 0.00001
  "Minimum duration in seconds for `with-slow-op-timer' to emit a log message.")

(setq sprite-slow-op-reporting slow-op-reporting)
(setq sprite-slow-op-threshold slow-op-threshold)

(defmacro with-slow-op-timer (name &rest body)
  "Report NAME when the runtime of BODY exceeds `slow-op-threshold'."
  (declare (indent defun) (debug t))
  `(if (not slow-op-reporting)
       (progn ,@body)
     (let* ((time (current-time))
	    (return-value (let ((inhibit-message t)) ,@body))
	    (duration (time-to-seconds (time-since time))))
       (when (> duration slow-op-threshold)
	 (let ((inhibit-message t))
	   (message "[op]: %s: %.06fs" ,name duration)))
       return-value)))

(setq package-quickstart t)
(setq package-enable-at-startup nil)
(setq use-package-expand-minimally t)
(setq load-prefer-newer t)

(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(alpha . 95) default-frame-alist)
(push '(font . "Source Code Pro-13") default-frame-alist)

(when (string-match "NATIVE_COMP" system-configuration-features)
  (setcar native-comp-eln-load-path (expand-file-name "~/.cache/emacs/eln/"))
  (setq native-comp-async-report-warnings-errors nil)
  (setq native-comp-jit-compilation nil)
  (setq native-compile-prune-cache t))

(provide 'early-init)
;;; early-init.el ends here
