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

(provide 'test-hud-mode)
;;; test-hud-mode.el ends here
