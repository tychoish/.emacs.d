;;; clix.el --- Command-line interface extensions for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; Register options and subcommands handled by `command-line-functions'.
;; Emacs handles --help before loading the init file; use --clix-help or help
;; for clix's help during normal startup.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defvar argi)
(defvar command-line-args-left)
(defvar clix--entries nil "Registered entries for `clix-help'.")

(declare-function builder-org-babel-execute-file "builder-tasks")
(declare-function builder-org-babel-execute-directory "builder-tasks")

(cl-defmacro register-clix-flag (name &key flag doc var (consume t) handler action hook final)
  "Register command-line entry NAME.
FLAG defaults to --NAME; a FLAG without a leading dash is a subcommand.
DOC describes the entry.  With CONSUME (default t), store its value in VAR,
or in `clix-arg-NAME' when VAR is omitted.  Options accept both --FLAG VALUE
and --FLAG=VALUE.  HANDLER runs during parsing.  ACTION runs immediately, or
on HOOK if specified.  FINAL is evaluated after ACTION and, if non-nil,
exits Emacs with status zero.  An error in ACTION prevents the exit."
  (let* ((name-str (symbol-name name))
         (fn-name (intern (format "cli/%s" name-str)))
         (run-fn-name (intern (format "cli/%s-run" name-str)))
         (seen-sym (intern (format "clix--%s-seen" name-str)))
         (flag-str (or flag (format "--%s" name-str)))
         (var-sym (cond ((and var (not (eq var t))) var)
                        (consume (intern (format "clix-arg-%s" name-str)))))
         (hook-sym (if (and (consp hook) (eq (car hook) 'quote))
                       (cadr hook)
                     hook)))
    `(progn
       (setq clix--entries (assq-delete-all ',name clix--entries))
       (push (list ',name ,flag-str ,doc ,consume) clix--entries)
       ,@(when (and hook-sym (or action final)) `((defvar ,seen-sym nil)))
       ,@(when var-sym `((defvar ,var-sym nil ,doc)))
       (defun ,fn-name ()
         ,doc
         (when (or (equal argi ,flag-str)
                   ,@(when (and consume (string-prefix-p "--" flag-str))
                       `((string-prefix-p (concat ,flag-str "=") argi))))
           ,@(when consume
               `((setq ,var-sym
                       (if (equal argi ,flag-str)
                           (or (pop command-line-args-left)
                               (user-error "%s requires a value" ,flag-str))
                         (substring argi (1+ (length ,flag-str)))))))
           ,@(when (and hook-sym (or action final)) `((setq ,seen-sym t)))
           ,@(when handler (list handler))
           ,@(when (and action (null hook-sym)) (list action))
           ,@(when (and final (null hook-sym)) `((when ,final (kill-emacs 0))))
           t))
       ,@(when (and (or action final) hook-sym)
           `((defun ,run-fn-name ()
               (when ,seen-sym
                 (setq ,seen-sym nil)
                 ,@(when action (list action))
                 ,@(when final `((when ,final (kill-emacs 0))))))
             (add-hook ',hook-sym #',run-fn-name)))
       (add-to-list 'command-line-functions #',fn-name))))

(defun clix-help ()
  "Print registered clix options and subcommands."
  (princ "Clix options and subcommands:\n")
  (seq-do (lambda (entry)
            (princ (format "  %s%s\n      %s\n" (nth 1 entry)
                           (if (nth 3 entry) " VALUE" "")
                           (or (nth 2 entry) ""))))
          (reverse clix--entries)))

(register-clix-flag help
  :flag "--clix-help" :doc "Show clix help." :consume nil
  :action (clix-help) :final t)
(register-clix-flag help-command
  :flag "help" :doc "Show clix help." :consume nil
  :action (clix-help) :final t)
(register-clix-flag slow-op-timing
  :flag "--with-slow-op-timing" :doc "Enable slow operation timing." :consume nil
  :handler (message "[op]: enabling time reporting"))
(register-clix-flag org-exec
  :flag "--org-exec" :doc "Execute an Org file's Babel blocks."
  :action (progn (require 'org) (require 'ob) (require 'builder-tasks)
                 (builder-org-babel-execute-file clix-arg-org-exec))
  :final t)
(register-clix-flag org-exec-dir
  :flag "--org-exec-dir" :doc "Execute Org Babel blocks in a directory."
  :action (progn (require 'org) (require 'ob) (require 'builder-tasks)
                 (builder-org-babel-execute-directory clix-arg-org-exec-dir))
  :final t)

(provide 'clix)
;;; clix.el ends here
