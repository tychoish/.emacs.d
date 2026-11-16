;;; test-mailx-edge.el --- Mail edge-case tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Edge cases for refile rules, unsubscribe parsing, display values and the
;; lazily installed cmdr keybindings.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'map)
(require 'cmdr-spec)
(require 'setup-core)
(require 'mailx)
(require 'mu4e)
(require 'mu4e-compose)
(require 'mu4e-headers)

(ert-deftest mailx-edge/refile-folder-defaults-to-archive ()
  (let ((mailx--refile-rules nil))
    (should (equal "/archive" (mailx-refile-folder '(:subject "x"))))))

(ert-deftest mailx-edge/refile-rules-first-match-wins-and-nil-falls-through ()
  (let ((mailx--refile-rules nil))
    (mailx-add-refile-rule (lambda (_) nil))
    (mailx-add-refile-rule (lambda (_) "/a"))
    (mailx-add-refile-rule (lambda (_) "/b"))
    (should (equal "/a" (mailx-refile-folder nil)))))

(ert-deftest mailx-edge/add-refile-rule-is-idempotent ()
  (let ((mailx--refile-rules nil)
        (rule (lambda (_) "/x")))
    (mailx-add-refile-rule rule)
    (mailx-add-refile-rule rule)
    (should (= 1 (length mailx--refile-rules)))))

(ert-deftest mailx-edge/field-display-value-variants ()
  (cl-letf (((symbol-function 'mu4e-message-field)
             (lambda (_msg field)
               (pcase field
                 (:none nil)
                 (:str "hello")
                 (:num 42)
                 (:people '((:name "Ann" :email "ann@x.org") (:email "bob@x.org")))))))
    (should (equal "(none)" (mailx--field-display-value nil :none)))
    (should (equal "hello" (mailx--field-display-value nil :str)))
    (should (equal "42" (mailx--field-display-value nil :num)))
    (should (equal "Ann, bob@x.org" (mailx--field-display-value nil :people)))))

(defun mailx-edge--unsubscribe (header)
  "Run `mailx-unsubscribe' against HEADER."
  (cl-letf (((symbol-function 'mu4e-fetch-field) (lambda (&rest _) header)))
    (mailx-unsubscribe 'msg)))

(ert-deftest mailx-edge/unsubscribe-missing-header ()
  (should-error (mailx-edge--unsubscribe nil) :type 'user-error))

(ert-deftest mailx-edge/unsubscribe-header-without-uris ()
  (should-error (mailx-edge--unsubscribe "no brackets here") :type 'user-error))

(ert-deftest mailx-edge/unsubscribe-unknown-scheme ()
  (should-error (mailx-edge--unsubscribe "<ftp://example.org/x>") :type 'user-error))

(ert-deftest mailx-edge/unsubscribe-https-browses ()
  (let (visited)
    (cl-letf (((symbol-function 'browse-url) (lambda (u &rest _) (setq visited u))))
      (mailx-edge--unsubscribe "<https://example.org/u>"))
    (should (equal "https://example.org/u" visited))))

(ert-deftest mailx-edge/unsubscribe-multiple-prompts ()
  (let (visited)
    (cl-letf (((symbol-function 'browse-url) (lambda (u &rest _) (setq visited u)))
              ((symbol-function 'completing-read) (lambda (_p uris &rest _) (cadr uris))))
      (mailx-edge--unsubscribe "<mailto:a@x.org>, <https://example.org/two>"))
    (should (equal "https://example.org/two" visited))))

(ert-deftest mailx-edge/lazy-keybindings-installed-by-spec ()
  (cmdr-spec-install 'mail-accounts)
  (should (eq 'mailx-compose-reply (keymap-lookup mu4e-compose-minor-mode-map "R")))
  (should (eq 'mu4e-headers-mark-for-unread (keymap-lookup mu4e-headers-mode-map "u")))
  (should (eq 'ignore (keymap-lookup message-mode-map "M-q"))))

(provide 'test-mailx-edge)
;;; test-mailx-edge.el ends here
