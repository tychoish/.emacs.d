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

(unless (fboundp 'logx-time)
  (defmacro logx-time (spec &rest body)
    "Timing shim when `logx' is not yet loaded, delegating to `logx' if loaded later.
Measures execution time when `slow-op-reporting' is non-nil."
    (declare (indent 1) (debug (sexp body)))
    `(if (not slow-op-reporting)
         (progn ,@body)
       (let* ((thresh ,(if (and (consp spec) (keywordp (car spec)))
                           (or (plist-get spec :threshold) 'slow-op-threshold)
                         'slow-op-threshold))
              (start (float-time))
              (err-val nil)
              (result nil))
         (unwind-protect
             (condition-case err
                 (setq result (let ((inhibit-message t)) ,@body))
               (error
                (setq err-val err)
                (signal (car err) (cdr err))))
           (let ((duration (- (float-time) start)))
             (when (>= duration (or thresh 0.0))
               (let* ((name ,(cond
                              ((stringp spec) spec)
                              ((and (consp spec) (eq (car spec) 'quote)) (format "%s" (cadr spec)))
                              ((and (consp spec) (keywordp (car spec)))
                               (or (plist-get spec :msg) (plist-get spec :name) "op"))
                              ((symbolp spec) (symbol-name spec))
                              (t (format "%s" spec))))
                      (dur-ms (* duration 1000.0)))
                 (if (featurep 'logx)
                     (let ((ev-data (list :duration_ms dur-ms
                                          :duration_s duration)))
                       (when err-val
                         (setq ev-data (append ev-data (list :status 'error
                                                             :error (error-message-string err-val)))))
                       (logx--emit-event 'info
                                         ,(if (and (consp spec) (keywordp (car spec)))
                                              (or (plist-get spec :cat) ''op)
                                            ''op)
                                         (format "%s in %.2fms" name dur-ms)
                                         nil
                                         ev-data))
                   (let ((inhibit-message t))
                     (message "[op]: %s: %.06fs" name duration)))))))
         result))))

(unless (fboundp 'with-logx-time)
  (defmacro with-logx-time (spec &rest body)
    "Fallback alias for `logx-time' when `logx' is not yet loaded."
    (declare (indent 1) (debug (sexp body)))
    `(logx-time ,spec ,@body)))

(defmacro with-slow-op-timer (name &rest body)
  "Report NAME when the runtime of BODY exceeds `slow-op-threshold'.
Delegates to `logx-time' when `slow-op-reporting' is enabled."
  (declare (indent defun) (debug t))
  `(if (not slow-op-reporting)
       (progn ,@body)
     (logx-time (:cat 'op :msg ,name :threshold slow-op-threshold)
       ,@body)))

(setq package-quickstart t)
(setq package-enable-at-startup nil)
(setq use-package-expand-minimally t)
(setq load-prefer-newer t)

(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(alpha . 95) default-frame-alist)

(when (string-match "NATIVE_COMP" system-configuration-features)
  (setcar native-comp-eln-load-path (expand-file-name "~/.cache/emacs/eln/"))
  (setq native-comp-async-report-warnings-errors nil)
  (setq native-comp-jit-compilation nil)
  (setq native-compile-prune-cache t))

(provide 'early-init)
;;; early-init.el ends here
