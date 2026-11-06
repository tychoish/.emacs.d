;;; test-eglox-ide.el --- ERT tests for eglox standalone commands and ide map -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Does not require test-helper, cmdr, xtd-macro, or hud-mode: run with
;;   emacs --batch -Q -L lisp -l test/test-eglox-ide.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'seq)
(require 'map)
(require 'eglox)

(ert-deftest eglox-ide/loads-standalone ()
  (should (featurep 'eglox))
  (should-not (featurep 'hud-mode))
  (should-not (featurep 'cmdr))
  (should-not (featurep 'xtd-macro)))

(ert-deftest eglox-ide/map-lookup-every-key ()
  (seq-do (lambda (pair)
            (should (eq (keymap-lookup eglox-ide-map (car pair)) (cdr pair))))
          '(("m" . imenu)
            ("c" . xref-find-references)
            ("d" . xref-find-definitions)
            ("p" . xref-go-back)
            ("n" . xref-go-forward)
            ("o" . xref-find-definitions-other-window)
            ("x" . execute-extended-xref-command)
            ("s" . execute-extended-slime-command))))

(ert-deftest eglox-ide/attached-to-command-map ()
  (should (eq (keymap-lookup eglox-command-map "x") eglox-ide-map))
  (should (eq (keymap-lookup eglox-command-map "x d") #'xref-find-definitions))
  (should (eq (keymap-lookup eglox-command-map "e") #'execute-extended-eglot-command)))

(defun eglox-ide-test--duplicate-keys (map)
  "Return keys bound more than once directly in MAP."
  (let (keys dups)
    (map-keymap (lambda (key _) (if (memq key keys) (push key dups) (push key keys))) map)
    dups))

(ert-deftest eglox-ide/no-key-collisions ()
  (seq-do (lambda (map)
            (should-not (eglox-ide-test--duplicate-keys (symbol-value map)))
            (map-keymap (lambda (key _)
                          (should (characterp key))
                          (should (= key (downcase key))))
                        (symbol-value map)))
          '(eglox-ide-map eglox-command-map eglox-global-map)))

(ert-deftest eglox-ide/extended-command-filters-by-prefix ()
  (dolist (spec '((execute-extended-eglot-command read-extended-command-for-eglot-prefix-p "eglot")
                  (execute-extended-xref-command read-extended-command-for-xref-prefix-p "xref")
                  (execute-extended-slime-command read-extended-command-for-slime-prefix-p "slime")))
    (let ((pred (nth 1 spec))
          (prefix (nth 2 spec))
          seen)
      (should (funcall pred (intern (concat prefix "-foo")) nil))
      (should-not (funcall pred 'other-command nil))
      (should-not (funcall pred (intern (concat "x" prefix)) nil))
      (cl-letf (((symbol-function 'execute-extended-command)
                 (lambda (_arg) (setq seen read-extended-command-predicate))))
        (call-interactively (nth 0 spec)))
      (should (eq seen pred)))))

(defmacro eglox-ide-test--with-describe-stubs (slime eglot &rest body)
  "Run BODY with stubs; SLIME and EGLOT are the connected states."
  (declare (indent 2))
  `(let (called)
     (cl-letf (((symbol-function 'slime-describe-symbol) (lambda () (interactive) (push 'slime called)))
               ((symbol-function 'slime-connected-p) (lambda () ,slime))
               ((symbol-function 'eglot-current-server) (lambda () ,eglot))
               ((symbol-function 'consult-eglot-symbols) (lambda () (push 'eglot called)))
               ((symbol-function 'describe-symbol) (lambda (&rest _) (interactive) (push 'describe called))))
       ,@body)
     called))

(ert-deftest eglox-ide/describe-dispatch ()
  (should (equal '(slime) (eglox-ide-test--with-describe-stubs t t (eglox-describe-symbol-dwim nil))))
  (should (equal '(eglot) (eglox-ide-test--with-describe-stubs nil t (eglox-describe-symbol-dwim nil))))
  (should (equal '(describe) (eglox-ide-test--with-describe-stubs nil nil (eglox-describe-symbol-dwim nil))))
  (should (equal '(describe) (eglox-ide-test--with-describe-stubs t t (eglox-describe-symbol-dwim '(4))))))

(provide 'test-eglox-ide)
;;; test-eglox-ide.el ends here
