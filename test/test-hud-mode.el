;;; test-hud-mode.el --- Tests for hud-mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Regression tests for hud-mode minibuffer behavior.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'hud-mode)

(ert-deftest hud-mode-test/minibuffer-quit-once ()
  "Quit through the standard minibuffer command exactly once."
  (let ((calls nil))
    (cl-letf (((symbol-function 'minibuffer-selected-window) (lambda () t))
              ((symbol-function 'abort-minibuffers)
               (lambda () (push 'abort calls)))
              ((symbol-function 'minibuffer-keyboard-quit)
               (lambda () (push 'quit calls)))
              ((symbol-function 'move-beginning-of-line) #'ignore)
              ((symbol-function 'kill-line) #'ignore))
      (hud-super-abort-minibuffers))
    (should (equal calls '(quit)))))

(ert-deftest hud-mode-test/lazy-init-form-runs-only-on-timer ()
  "A form is evaluated when the idle callback runs, with lexical bindings."
  (let ((callback nil)
        (result nil)
        (value 42))
    (cl-letf (((symbol-function 'run-with-idle-timer)
               (lambda (delay repeat function &rest _args)
                 (should (= delay 3))
                 (should-not repeat)
                 (setq callback function))))
      (add-lazy-init :name "form" :delay 3 :form (setq result value))
      (should-not result)
      (funcall callback)
      (should (= result 42)))))

(ert-deftest hud-mode-test/lazy-init-operation-remains-supported ()
  "Operation functions are still invoked by the idle callback."
  (let ((callback nil)
        (called nil))
    (cl-letf (((symbol-function 'run-with-idle-timer)
               (lambda (_delay _repeat function &rest _args)
                 (setq callback function))))
      (add-lazy-init :name "operation" :operation (lambda () (setq called t)))
      (should-not called)
      (funcall callback)
      (should called))))

(ert-deftest hud-mode-test/lazy-init-validates-input ()
  "Require a name and an operation or form, giving the form precedence."
  (should-error (macroexpand-1 '(add-lazy-init :form (message "x"))) :type 'user-error)
  (should-error (macroexpand-1 '(add-lazy-init :name "x")) :type 'user-error)
  (let ((callback nil)
        (result nil))
    (cl-letf (((symbol-function 'run-with-idle-timer)
               (lambda (_delay _repeat function &rest _args)
                 (setq callback function))))
      (add-lazy-init :name "both" :form (setq result 'form)
                     :operation (lambda () (setq result 'operation)))
      (funcall callback)
      (should (eq result 'form)))))

(provide 'test-hud-mode)
;;; test-hud-mode.el ends here
