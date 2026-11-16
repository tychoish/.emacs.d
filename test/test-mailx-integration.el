;;; test-mail-integration.el --- Isolated mu4e integration test -*- lexical-binding: t; -*-

;;; Commentary:
;; Run in a throwaway batch Emacs, never in the user's mail session.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'mailx)
(require 'mu4e)
(require 'mu4e-compose)

(ert-deftest mailx-integration/reopen-indexed-draft ()
  "Reopen a real indexed draft and enforce its database ownership."
  (skip-unless (and noninteractive (executable-find "mu")
                    (not (mu4e-running-p))))
  (let* ((root (make-temp-file "mail-integration-" t))
         (maildir (expand-file-name "mail" root))
         (home (expand-file-name ".mu" maildir))
         (other-maildir (expand-file-name "other" root))
         (other-home (expand-file-name ".mu" other-maildir))
         (file (expand-file-name "drafts/cur/integration:2,DS" maildir))
         (mu4e-mu-home home)
         (mu4e-contexts nil)
         (mu4e-context-policy nil)
         (mu4e-compose-context-policy nil)
         (mu4e-notification-support nil)
         (mu4e-modeline-support nil)
         (mu4e--context-current nil)
         draft first-draft second-draft)
    (unwind-protect
        (progn
          (dolist (directory (list maildir other-maildir))
            (dolist (folder '("drafts" "sent" "trash"))
              (dolist (part '("cur" "new" "tmp"))
                (make-directory (expand-file-name (concat folder "/" part) directory) t))))
          (make-directory (expand-file-name "tools/signatures" maildir) t)
          (with-temp-file (expand-file-name "tools/signatures/alpha@example.org" maildir)
            (insert "Alpha signature\n"))
          (make-directory (expand-file-name "tools/signatures" other-maildir) t)
          (with-temp-file (expand-file-name "tools/signatures/beta@example.org" other-maildir)
            (insert "Beta signature\n"))
          (with-temp-file file
            (insert "From: Alpha <alpha@example.org>\n"
                    "To: recipient@example.org\n"
                    "Subject: indexed integration draft\n"
                    "Date: Sun, 15 Nov 2026 12:00:00 +0000\n"
                    "Message-ID: <mail-integration@example.org>\n\n"
                    "Draft body\n"))
          (should (zerop (call-process "mu" nil nil nil "init" "--muhome" home
                                      "--maildir" maildir "--my-address" "alpha@example.org")))
          (should (zerop (call-process "mu" nil nil nil "init" "--muhome" other-home
                                      "--maildir" other-maildir "--my-address" "beta@example.org")))
          (should (zerop (call-process "mu" nil nil nil "index" "--muhome" home)))
          (with-temp-buffer
            (should (zerop (call-process "mu" nil t nil "find" "--muhome" home
                                        "--fields=l" "subject:indexed integration draft")))
            (should (string-match-p (regexp-quote file) (buffer-string))))
          (setq mu4e-contexts
                (list (mailx-make-context
                       :name "alpha" :full-name "Alpha" :address "alpha@example.org"
                       :maildir maildir :command "fetch-alpha")
                      (mailx-make-context
                       :name "beta" :full-name "Beta" :address "beta@example.org"
                       :maildir other-maildir :command "fetch-beta")
                      (mailx-make-context
                       :name "alpha-two" :full-name "Alpha Two" :address "alpha2@example.org"
                       :maildir maildir :command "fetch-alpha")))
          (cl-letf (((symbol-function 'cmdr-frame-record-home-frame) #'ignore)
                    ((symbol-function 'turn-off-hard-wrap) #'ignore)
                    ((symbol-function 'mu4e-message-at-point)
                     (lambda (&rest _) (list :path file :flags '(draft)))))
            (mu4e-context-switch nil "alpha")
            ;; Creating the first draft starts mu4e from a cold session and
            ;; selects beta while the session's active identity is alpha.
            (let ((mu4e-compose-context-policy 'ask))
              (cl-letf (((symbol-function 'mu4e--context-ask-user)
                         (lambda (&rest _) (cadr mu4e-contexts))))
                (setq first-draft (mu4e-compose-new "one@example.org" "First"))))
            (let ((mu4e-mu-home other-home))
              (should (equal (mailx-server-current-home) home))
              (with-current-buffer first-draft
                (mailx--guard-draft-database)))
            (with-current-buffer first-draft
              (should (equal user-mail-address "beta@example.org"))
              (mu4e-compose-context-switch nil "alpha")
              (mu4e-compose-context-switch nil "beta"))
            (with-temp-buffer
              (setq second-draft (mu4e-compose-new "two@example.org" "Second"))
              (should (equal user-mail-address "alpha@example.org"))
              (should (equal message-directory maildir))
              (should (equal message-auto-save-directory (expand-file-name "drafts" maildir)))
              (should (equal mu4e-get-mail-command "fetch-alpha")))
            (with-current-buffer second-draft
              (should (equal user-mail-address "alpha@example.org")))
            (with-current-buffer first-draft
              (should (equal user-mail-address "beta@example.org")))
            (with-temp-buffer
              (mu4e-context-switch nil "alpha-two")
              (should (equal mu4e-mu-home home))
              (mu4e-context-switch nil "alpha"))
            (with-temp-buffer (setq draft (mu4e-compose-edit)))
            (with-current-buffer draft
              (mu4e-compose-context-switch nil "beta")
              (should (equal user-mail-address "beta@example.org"))
              (should (equal message-directory maildir))
              (should (equal message-auto-save-directory (expand-file-name "drafts" maildir)))
              (should (equal mu4e-get-mail-command "fetch-alpha"))
              (goto-char (point-max))
              (insert "Updated\n")
              (save-buffer))
            (with-temp-buffer
              (should-error (mu4e-context-switch nil "beta") :type 'user-error)
              (should (equal mu4e-mu-home home)))
            ;; These are disposable test drafts.  Real drafts must be saved
            ;; or discarded by the user before they are closed.
            (dolist (buffer (list draft first-draft second-draft))
              (when (buffer-live-p buffer)
                (with-current-buffer buffer (set-buffer-modified-p nil))
                (kill-buffer buffer)))
            (with-temp-buffer
              (mu4e-context-switch nil "beta")
              (should-not (mu4e-running-p))
              (should (equal mu4e-mu-home other-home))
              (should (equal message-directory other-maildir))
              (should (equal mu4e-get-mail-command "fetch-beta")))
            ;; Indexing is a public entry point even when mu4e was previously
            ;; initialized and the database switch left its server stopped.
            (mu4e-update-index)
            (mailx-server-ensure-ready)
            (should (mu4e-running-p))
            (should mu4e--started)
            (let ((deadline (+ (float-time) 10)))
              (while (and mu4e--server-indexing (< (float-time) deadline))
                (accept-process-output mu4e--server-process 0.1)))
            (should-not mu4e--server-indexing)
            (with-temp-buffer
              (setq first-draft (mu4e-compose-new "after@example.org" "After index")))
            (with-current-buffer first-draft
              (should (equal user-mail-address "beta@example.org"))
              (set-buffer-modified-p nil))
            (kill-buffer first-draft)
            (with-temp-buffer
              (setq buffer-file-name file)
              (should-error (mailx--guard-draft-database) :type 'user-error))
            (with-temp-buffer (mu4e-context-switch nil "alpha"))
            (should-not (mu4e-running-p))
            (should (equal (mailx-server-current-home) home))
            (mu4e)
            (mailx-server-ensure-ready)
            (should (get-buffer mu4e-main-buffer-name))))
      (dolist (buffer (list draft first-draft second-draft))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (unwind-protect
          (when (mu4e-running-p)
            (let ((process mu4e--server-process)
                  (deadline (+ (float-time) 5))
                  (mu4e-confirm-quit nil))
              (mu4e-quit)
              (while (and (process-live-p process) (< (float-time) deadline))
                (accept-process-output process 0.1))
              (when (process-live-p process)
                (delete-process process))))
        (delete-directory root t)))))

(provide 'test-mail-integration)
;;; test-mail-integration.el ends here
