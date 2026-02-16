(defmacro with-gc-suppressed (&rest body)
  `(progn
     (let ((gc-cons-threshold 800000000000000))
       ,@body)
     (let ((garbage-collection-messages t))
       (garbage-collect))))

(defmacro with-file-name-handler-disabled (&rest body)
  `(let ((file-name-handler-alist nil))
     ,@body))

(defvar tychoish/slow-op-reporting (or debug-on-error init-file-debug))
(defvar tychoish/slow-op-threshold 0.01)

(defmacro with-slow-op-timer (name &rest body)
  "Send a message the BODY operation of NAME takes longer to execute than a hardcoded threshold."
  `(let* ((inhibit-message t)
	  (time (current-time))
	  (return-value (progn ,@body))
	  (duration (time-to-seconds (time-since time))))
     (when (and tychoish/slow-op-reporting (> duration tychoish/slow-op-threshold))
       (message "[op]: %s: %.06fs" ,name duration))
     return-value))

(defun cli/time-reporting ()
  (when (string-prefix-p "--with-slow-op-timing" argi)
    (message "[op]: enabling time reporting")
    (setq tychoish/slow-op-reporting t)))

(add-to-list 'command-line-functions 'cli/time-reporting)

(setcar native-comp-eln-load-path (expand-file-name "~/.cache/emacs/eln/"))

(when (string-match "NATIVE_COMP" system-configuration-features)
  (setq native-comp-jit-compilation t)
  (setq native-compile-prune-cache t))
