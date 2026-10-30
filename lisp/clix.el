;;; clix.el --- Command-line interface extensions for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; CLI flag registration macro and standard handlers for tychoish Emacs.
;;
;; Provides `register-clix-flag' for declaring command-line switches handled
;; via `command-line-functions'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defvar argi nil)
(defvar argv nil)

(declare-function builder-org-babel-execute-file "builder-tasks")
(declare-function builder-org-babel-execute-directory "builder-tasks")

(cl-defmacro register-clix-flag (name &key flag doc var (consume t) handler action hook)
  "Register a command-line flag NAME.

NAME is a base symbol used to derive handler function `cli/<name>'
and variable `clix-arg-<name>' (if VAR is nil and CONSUME is non-nil).

Key arguments:
:flag     The command-line switch string.  Defaults to \"--<name>\".
:doc      Documentation string for the flag and variable.
:var      Symbol to store the flag's argument.  Defaults to `clix-arg-<name>'.
:consume  Whether this flag consumes an argument from `argv'.  Defaults to t.
:handler  Form executed when flag is parsed in `command-line-functions'.
:action   Form executed to perform the flag's action.
:hook     Hook on which to run ACTION.  If nil (default), runs ACTION
          immediately during parsing in `command-line-functions'."
  (let* ((name-str (symbol-name name))
         (fn-name (intern (format "cli/%s" name-str)))
         (run-fn-name (intern (format "cli/%s-run" name-str)))
         (flag-str (or flag (format "--%s" name-str)))
         (var-sym (cond
                   ((and var (not (eq var t))) var)
                   (consume (intern (format "clix-arg-%s" name-str)))
                   (t nil)))
         (hook-sym (cond
                    ((null hook) nil)
                    ((and (consp hook) (eq (car hook) 'quote)) (cadr hook))
                    (t hook))))
    `(progn
       ,@(when var-sym
           `((defvar ,var-sym nil ,doc)))

       (defun ,fn-name ()
         ,doc
         (when (string-prefix-p ,flag-str argi)
           ,@(when consume
               `((setq ,var-sym (pop argv))))
           ,@(when handler
               (list handler))
           ,@(when (and action (null hook-sym))
               (list action))
           t))

       ,@(when (and action hook-sym)
           `((defun ,run-fn-name ()
               (when ,(or var-sym t)
                 ,action))
             (add-hook ',hook-sym #',run-fn-name)))

       (add-to-list 'command-line-functions #',fn-name))))

;;; Standard Flags

(register-clix-flag slow-op-timing
  :flag "--with-slow-op-timing"
  :doc "Acknowledge slow-op timing command-line flag."
  :consume nil
  :handler (message "[op]: enabling time reporting"))

(register-clix-flag org-exec
  :flag "--org-exec"
  :doc "Org file to execute via `builder-org-babel-execute-file'; set by --org-exec."
  :action (progn
            (require 'org nil t)
            (require 'ob nil t)
            (require 'builder-tasks nil t)
            (builder-org-babel-execute-file clix-arg-org-exec)
            (kill-emacs 0)))

(register-clix-flag org-exec-dir
  :flag "--org-exec-dir"
  :doc "Directory to execute all org babel blocks in; set by --org-exec-dir."
  :action (progn
            (require 'org nil t)
            (require 'ob nil t)
            (require 'builder-tasks nil t)
            (builder-org-babel-execute-directory clix-arg-org-exec-dir)
            (kill-emacs 0)))

(provide 'clix)
;;; clix.el ends here
