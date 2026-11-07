(require 'use-package-core)
(defun ad:use-package-statistics-convert--higher-precision-time (result)
  "Reformat the duration column in RESULT with higher precision.
`use-package-statistics-convert' only reports two decimal places;
this widens it to four for finer-grained startup profiling."
  (let* ((package (car result))
	 (statistics (map-elt use-package-statistics package)))
    (setf (aref (cadr result) 3)
	  (format "%.4f" (use-package-statistics-time statistics)))
    result))

(advice-add #'use-package-statistics-convert :filter-return
	    #'ad:use-package-statistics-convert--higher-precision-time)

(let ((pos (member :autoload use-package-keywords)))
  (if pos
      (setcdr pos (cons :ensure-lazy (cdr pos)))
    (add-to-list 'use-package-keywords :ensure-lazy t)))

(defun ad:use-package-handler/:ensure-skip-lazy (orig-fn name-symbol keyword ensure rest state)
  "Skip standard ensure if :ensure-lazy is present in the REST arguments."
  (if (plist-get rest :ensure-lazy)
      (use-package-process-keywords name-symbol rest state)
    (funcall orig-fn name-symbol keyword ensure rest state)))

(advice-add 'use-package-handler/:ensure :around #'ad:use-package-handler/:ensure-skip-lazy)

(defun use-package-handler/:ensure-lazy (name-symbol _keyword _args rest state)
  (let* ((body (use-package-process-keywords name-symbol rest state))
         (commands (plist-get state :commands)))
    (if (null commands)
        ;; Fallback to standard check if no autoload commands are specified
        `(progn
           (unless (package-installed-p ',name-symbol)
             (package-install ',name-symbol))
           (require ',name-symbol)
           ,@body)
      ;; Generate wrapper autoloads that install-then-invoke
      (let ((wrapper-defs
             (mapcar
              (lambda (cmd)
                `(defun ,cmd (&rest args)
                   ,(format "On-demand wrapper to install and call `%s'." cmd)
                   (interactive)
                   (unless (package-installed-p ',name-symbol)
                     (message "Installing %s on-demand..." ',name-symbol)
                     (package-install ',name-symbol))
                   (fmakunbound ',cmd)
                   (load (symbol-name ',name-symbol) nil t)
                   (apply ',cmd args)))
              commands)))
        `(progn
           ,@wrapper-defs
           ,@body)))))
