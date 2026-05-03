(defmacro with-gc-suppressed (&rest body)
  `(let ((gc-cons-threshold 800000000000000)
         (gc-cons-percentage 1.0))
     (prog1 (progn ,@body)
       (garbage-collect))))

(defmacro with-file-name-handler-disabled (&rest body)
  `(let ((file-name-handler-alist nil))
     ,@body))

(setcar native-comp-eln-load-path (expand-file-name "~/.cache/emacs/eln/"))

(when (string-match "NATIVE_COMP" system-configuration-features)
  (setq native-comp-jit-compilation t)
  (setq native-compile-prune-cache t))
