;;; test-telega-bot-alert.el --- ERT tests for telega-bot-alert.el -*- lexical-binding: t; no-byte-compile: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'test-helper)
(require 'telega-bot-alert)

(ert-deftest telega-bot-alert/register-and-unregister-target ()
  "Test target registration and lookup by name."
  (let ((telega-bot-alert-targets nil))
    (telega-bot-alert-register-target :name "personal" :bot 'some-bot :chat-id 111)
    (should (telega-bot-alert-get-target "personal"))
    (should (equal (telega-bot-alert-target-chat-id (telega-bot-alert-get-target "personal")) 111))

    (telega-bot-alert-unregister-target "personal")
    (should-not (telega-bot-alert-get-target "personal"))))

(ert-deftest telega-bot-alert/delivers-to-all-registered-targets-by-default ()
  "Test that an alert with no explicit targets fans out to every registered target."
  (let ((telega-bot-alert-targets nil)
        (telega-bot-alert-default-targets nil)
        (sent nil))
    (telega-bot-alert-register-target :name "a" :bot 'bot-a :chat-id 1)
    (telega-bot-alert-register-target :name "b" :bot 'bot-b :chat-id 2)
    (cl-letf (((symbol-function 'telega-bot-send-response)
               (lambda (text &rest args)
                 (push (list text (map-elt args :bot) (map-elt args :chat-id)) sent))))
      (alert "Build finished" :style 'telega-bot :title "CI"))
    (should (= (length sent) 2))
    (should (seq-find (lambda (call) (equal (cadr call) 'bot-a)) sent))
    (should (seq-find (lambda (call) (equal (cadr call) 'bot-b)) sent))
    (should (seq-every-p (lambda (call) (string-match-p "\\*CI\\*" (car call))) sent))))

(ert-deftest telega-bot-alert/delivers-only-to-requested-targets ()
  "Test that `:data (:targets ...)' restricts delivery to the named targets."
  (let ((telega-bot-alert-targets nil)
        (telega-bot-alert-default-targets nil)
        (sent nil))
    (telega-bot-alert-register-target :name "a" :bot 'bot-a :chat-id 1)
    (telega-bot-alert-register-target :name "b" :bot 'bot-b :chat-id 2)
    (cl-letf (((symbol-function 'telega-bot-send-response)
               (lambda (text &rest args)
                 (push (list text (map-elt args :bot) (map-elt args :chat-id)) sent))))
      (telega-bot-alert "Deploy failed" :title "CI" :targets '("b")))
    (should (= (length sent) 1))
    (should (equal (cadr (car sent)) 'bot-b))))

(provide 'test-telega-bot-alert)
;;; test-telega-bot-alert.el ends here
