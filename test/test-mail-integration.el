;;; test-mail-integration.el --- Isolated mu4e integration test -*- lexical-binding: t; -*-

;;; Commentary:
;; Run in a throwaway batch Emacs, never in the user's mail session.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'setup-mail)
(require 'mu4e)
(require 'mu4e-compose)

(ert-deftest tychoish-mail-integration/reopen-indexed-draft ()
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
         (tychoish-mail--server-home nil)
         (mu4e--context-current nil)
         draft)
    (unwind-protect
        (progn
          (dolist (directory (list maildir other-maildir))
            (dolist (folder '("drafts" "sent" "trash"))
              (dolist (part '("cur" "new" "tmp"))
                (make-directory (expand-file-name (concat folder "/" part) directory) t))))
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
                (list (tychoish-mail-make-context
                       :name "alpha" :full-name "Alpha" :address "alpha@example.org"
                       :maildir maildir :command "fetch-alpha")
                      (tychoish-mail-make-context
                       :name "beta" :full-name "Beta" :address "beta@example.org"
                       :maildir other-maildir :command "fetch-beta")))
          (cl-letf (((symbol-function 'cmdr-frame-record-home-frame) #'ignore)
                    ((symbol-function 'turn-off-hard-wrap) #'ignore)
                    ((symbol-function 'mu4e-message-at-point)
                     (lambda (&rest _) (list :path file :flags '(draft)))))
            (mu4e-context-switch nil "alpha")
            (setq draft (mu4e-compose-edit))
            (with-current-buffer draft
              (should (equal tychoish-mail--draft-home home))
              (mu4e-compose-context-switch nil "beta")
              (should (equal user-mail-address "beta@example.org"))
              (should (equal message-directory maildir))
              (should (equal message-auto-save-directory (expand-file-name "drafts" maildir)))
              (should (equal mu4e-get-mail-command "fetch-alpha"))
              (goto-char (point-max))
              (insert "Updated\n")
              (save-buffer))
            (with-temp-buffer
              (mu4e-context-switch nil "beta")
              (should (equal mu4e-mu-home other-home))
              (should (equal message-directory other-maildir))
              (should (equal mu4e-get-mail-command "fetch-beta")))
            (with-current-buffer draft
              (insert "Blocked\n")
              (should-error (save-buffer) :type 'user-error)
              (should-error (tychoish-mail--guard-draft-database) :type 'user-error))
            (with-temp-buffer (mu4e-context-switch nil "alpha"))
            (with-current-buffer draft
              (should (equal tychoish-mail--server-home home))
              (save-buffer))))
      (when (buffer-live-p draft)
        (with-current-buffer draft (set-buffer-modified-p nil))
        (kill-buffer draft))
      (unwind-protect
          (when (mu4e-running-p)
            (let ((mu4e-confirm-quit nil)) (mu4e-quit)))
        (delete-directory root t)))))

(provide 'test-mail-integration)
;;; test-mail-integration.el ends here
