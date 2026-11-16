;;; test-mail-context.el --- Mail context regression tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for account contexts and database transitions without sending mail.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'map)
(require 'test-helper)
(require 'cmdr-keys)
(require 'mailx)
(require 'mu4e)
(require 'mu4e-context)
(require 'mu4e-compose)

(defvar personal-mail-activation-timer)

(defmacro mailx-test--with-accounts (&rest body)
  "Evaluate BODY with two isolated test contexts."
  (declare (indent 0) (debug t))
  `(let ((mu4e-contexts
          (list (mailx-make-context :name "a" :full-name "Alpha" :address "alpha@example.org" :maildir "/tmp/mail-a" :command "fetch-a")
                (mailx-make-context :name "b" :full-name "Beta" :address "beta@example.org" :maildir "/tmp/mail-b" :command "fetch-b")))
         (mu4e--context-current nil)
         (mu4e--server-process nil)
         (mailx--stopping-process nil))
     ,@body))

(ert-deftest mailx-context/matches-to-and-cc ()
  (mailx-test--with-accounts
    (let* ((context (cadr mu4e-contexts))
           (match (mu4e-context-match-func context)))
      (should (funcall match '(:to ((:email "BETA@EXAMPLE.ORG")))))
      (should (funcall match '(:cc ((:email "beta@example.org")))))
      (should-not (funcall match '(:to ((:email "other@example.org")))))
      (should-not (funcall match nil)))))

(ert-deftest mailx-context/matching-excludes-bcc-and-unconfigured-alias ()
  (mailx-test--with-accounts
    (let ((match (mu4e-context-match-func (cadr mu4e-contexts))))
      (should-not (funcall match '(:bcc ((:email "beta@example.org")))))
      (should-not (funcall match '(:to ((:email "beta+alias@example.org"))))))))

(ert-deftest mailx-context/identity-variables ()
  (mailx-test--with-accounts
    (let ((vars (mu4e-context-vars (cadr mu4e-contexts))))
      (should (equal (alist-get 'user-mail-address vars) "beta@example.org"))
      (should (equal (alist-get 'user-full-name vars) "Beta"))
      (should (equal (alist-get 'message-signature-file vars) "beta@example.org"))
      (should (equal (alist-get 'message-sendmail-extra-arguments vars)
                     '("-a" "beta@example.org")))
      (should-not (assq 'message-directory vars))
      (should-not (assq 'message-auto-save-directory vars))
      (should-not (assq 'mu4e-get-mail-command vars))
      (should-not (assq 'mu4e-mu-home vars)))))

(ert-deftest mailx-context/manual-selection-applies-identity ()
  (mailx-test--with-accounts
    (let ((mu4e-context-changed-hook nil)
          (user-mail-address nil)
          (user-full-name nil)
          (message-signature-file nil)
          (message-sendmail-extra-arguments nil))
      (cl-letf (((symbol-function 'mailx-server-switch-database) #'ignore))
        (mu4e-context-switch nil "b")
        (should (equal (mu4e-context-name (mu4e-context-current)) "b"))
        (should (equal user-mail-address "beta@example.org"))
        (should (equal message-signature-file "beta@example.org"))
        (should (equal message-sendmail-extra-arguments '("-a" "beta@example.org")))))))

(ert-deftest mailx-context/database-switch-applies-store-settings ()
  (mailx-test--with-accounts
    (let ((mu4e-context-changed-hook nil)
          (message-directory "old-directory")
          (message-auto-save-directory "old-drafts")
          (mu4e-get-mail-command "old-fetch")
          switched)
      (cl-letf (((symbol-function 'mailx-server-switch-database)
                 (lambda (home) (setq switched home))))
        (mu4e-context-switch nil "b")
        (should (equal switched "/tmp/mail-b/.mu"))
        (should (equal message-directory "/tmp/mail-b"))
        (should (equal message-auto-save-directory "/tmp/mail-b/drafts"))
        (should (equal mu4e-get-mail-command "fetch-b"))))))

(ert-deftest mailx-context/same-database-switch-updates-fetch-command ()
  (let* ((mu4e-contexts
          (list (mailx-make-context :name "a" :full-name "A" :address "a@example.org" :maildir "/tmp/mail-a" :command "fetch-a")
                (mailx-make-context :name "b" :full-name "B" :address "b@example.org" :maildir "/tmp/mail-a" :command "fetch-b")))
         (mu4e--context-current nil)
         (message-directory nil)
         (message-auto-save-directory nil)
         (mu4e-get-mail-command nil)
         (user-mail-address nil)
         (user-full-name nil)
         observed
         (mu4e-context-changed-hook
          (list (lambda () (setq observed (list message-directory mu4e-get-mail-command))))))
    (cl-letf (((symbol-function 'mailx-server-switch-database) #'ignore))
      (mu4e-context-switch nil "a")
      (mu4e-context-switch nil "b")
      (should (equal observed '("/tmp/mail-a" "fetch-b"))))))

(ert-deftest mailx-context/failed-database-switch-preserves-store-settings ()
  (mailx-test--with-accounts
    (let ((message-directory "old-directory")
          (message-auto-save-directory "old-drafts")
          (mu4e-get-mail-command "old-fetch"))
      (cl-letf (((symbol-function 'mailx-server-switch-database)
                 (lambda (_) (error "switch failed"))))
        (should-error (mu4e-context-switch nil "b"))
        (should (equal message-directory "old-directory"))
        (should (equal message-auto-save-directory "old-drafts"))
        (should (equal mu4e-get-mail-command "old-fetch"))))))

(ert-deftest mailx-context/draft-identity-switch-preserves-store-settings ()
  (mailx-test--with-accounts
    (let ((message-directory "old-directory")
          (message-auto-save-directory "old-drafts")
          (mu4e-get-mail-command "old-fetch")
          (mailx--composing t)
          (user-mail-address nil)
          (mu4e-context-changed-hook nil))
      (cl-letf (((symbol-function 'mailx-server-switch-database)
                 (lambda (_) (error "unexpected database switch"))))
        (mu4e-context-switch nil "b")
        (should (equal user-mail-address "beta@example.org"))
        (should (equal message-directory "old-directory"))
        (should (equal message-auto-save-directory "old-drafts"))
        (should (equal mu4e-get-mail-command "old-fetch"))))))

(ert-deftest mailx-context/account-selector-dispatches-by-buffer ()
  (with-temp-buffer
    (let (called)
      (cl-letf (((symbol-function 'mu4e-context-switch)
                 (lambda (&rest _) (interactive) (setq called 'context)))
                ((symbol-function 'mu4e-compose-context-switch)
                 (lambda (&rest _) (interactive) (setq called 'compose))))
        (mailx-select-account)
        (should (eq called 'context))
        (setq major-mode 'mu4e-compose-mode)
        (mailx-select-account)
        (should (eq called 'compose))))))

(ert-deftest mailx-context/noninteractive-native-switch-passes-through ()
  (mailx-test--with-accounts
    (let (called)
      (cl-letf (((symbol-function 'mu4e-compose-context-switch)
                 (lambda (&rest _) (setq called t)))
                ((symbol-function 'mailx-server-switch-database) #'ignore))
        (mu4e-context-switch nil "b")
        (should-not called)
        (should (equal (mu4e-context-name (mu4e-context-current)) "b"))))))

(ert-deftest mailx-context/manual-compose-updates-from-and-signature ()
  (let* ((maildir (make-temp-file "mail-context-" t))
         (signature-dir (expand-file-name "tools/signatures" maildir))
         (mu4e-contexts (list (mailx-make-context
                                :name "b" :full-name "Beta" :address "beta@example.org" :maildir maildir :command "fetch-b")))
         (mu4e--context-current nil)
         (mu4e-context-changed-hook nil)
         (message-signature nil)
         (mu4e-mu-home "/tmp/mail-a/.mu")

         switched)
    (unwind-protect
        (progn
          (make-directory signature-dir t)
          (with-temp-file (expand-file-name "beta@example.org" signature-dir)
            (insert "Test signature\n"))
          (with-temp-buffer
            (message-mode)
            (setq major-mode 'mu4e-compose-mode)
            (insert "From: old@example.org\nTo: other@example.org\nSubject: test\n"
                    mail-header-separator "\n\nBody\n")
            (mailx-set-up-message-mode-buffer)
            (cl-letf (((symbol-function 'mailx-server-switch-database)
                       (lambda (&rest _) (setq switched t))))
              (mu4e-compose-context-switch nil "b"))
            (should-not switched)
            (should (equal mu4e-mu-home "/tmp/mail-a/.mu"))
            (goto-char (point-min))
            (should (re-search-forward "^From: .*beta@example\\.org" nil t))
            (should (search-forward "Test signature" nil t))
            (should (equal message-sendmail-extra-arguments
                           '("-a" "beta@example.org")))
            (set-buffer-modified-p nil)))
      (delete-directory maildir t))))

(ert-deftest mailx-context/deterministic-precedence ()
  (mailx-test--with-accounts
    (let* ((message '(:to ((:email "beta@example.org")
                           (:email "alpha@example.org"))))
           (selected (mu4e-context-determine message)))
      (should (equal (mu4e-context-name selected) "a"))
      (should-not (mu4e-context-determine '(:to ((:email "none@example.org"))) nil)))))

(ert-deftest mailx-context/no-match-asks ()
  (mailx-test--with-accounts
    (let (asked)
      (cl-letf (((symbol-function 'mu4e--context-ask-user)
                 (lambda (&rest _) (setq asked t) (cadr mu4e-contexts))))
        (should (eq (mu4e-context-determine nil mu4e-compose-context-policy)
                    (cadr mu4e-contexts)))
        (should asked)))))

(ert-deftest mailx-context/drafts-keep-identity-on-external-switch ()
  (mailx-test--with-accounts
    (let ((mu4e--context-current (car mu4e-contexts))
          (mu4e-context-changed-hook nil)
          (user-mail-address "alpha@example.org")
          (message-signature-file "alpha@example.org")
          (message-sendmail-extra-arguments '("-a" "alpha@example.org"))
          (mu4e-mu-home "/tmp/mail-a/.mu")
          (first (generate-new-buffer " *draft-one*"))
          (second (generate-new-buffer " *draft-two*")))
      (unwind-protect
          (progn
            (dolist (draft (list first second))
              (with-current-buffer draft
                (setq major-mode 'mu4e-compose-mode)
                (mailx-set-up-message-mode-buffer)))
            (cl-letf (((symbol-function 'mailx-server-switch-database)
                       (lambda (home) (setq mu4e-mu-home home))))
              (with-temp-buffer (mu4e-context-switch nil "b")))
            (should (equal user-mail-address "beta@example.org"))
            (dolist (draft (list first second))
              (with-current-buffer draft
                (should (equal user-mail-address "alpha@example.org"))
                (should (equal message-signature-file "alpha@example.org"))
                (should (equal message-sendmail-extra-arguments
                               '("-a" "alpha@example.org"))))))
        (kill-buffer first)
        (kill-buffer second)))))

(ert-deftest mailx-context/draft-creation-isolates-selected-identity ()
  (let* ((root (make-temp-file "mail-draft-root-" t))
         (mu4e-mu-home (expand-file-name ".mu" root))
         (mu4e--started t)
         (user-mail-address "alpha@example.org")
         (draft (generate-new-buffer " *mail-created-draft*")))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'mu4e-running-p) (lambda () t)))
            (ad:mailx-draft
             (lambda ()
             (setq user-mail-address "beta@example.org")
             (with-current-buffer draft
               (setq buffer-file-name (expand-file-name "drafts/message" root))
               (setq major-mode 'mu4e-compose-mode)
               (mailx-set-up-message-mode-buffer))
             draft)))
          (should (equal user-mail-address "alpha@example.org"))
          (with-current-buffer draft
            (should (equal user-mail-address "beta@example.org"))))
      (with-current-buffer draft (setq buffer-file-name nil))
      (kill-buffer draft)
      (delete-directory root t))))

(ert-deftest mailx-context/transition-blocks-draft-creation ()
  (let ((mailx--transitioning t)
        started)
    (cl-letf (((symbol-function 'mailx-server-ensure-ready)
               (lambda () (setq started t))))
      (should-error (ad:mailx-start-for-draft) :type 'user-error)
      (should-not started))))

(ert-deftest mailx-context/cold-draft-waits-for-server-readiness ()
  (let ((mu4e--started nil)
        (mu4e-pong-func nil)
        (mu4e-mu-home "/tmp/mail-a/.mu")

        running events)
    (cl-letf (((symbol-function 'mu4e-running-p) (lambda () running))
              ((symbol-function 'mu4e)
               (lambda (&rest _)
                 (push 'start events)
                 (setq running t
                       mu4e-pong-func (lambda (_) (setq mu4e--started t)))))
              ((symbol-function 'accept-process-output)
               (lambda (&rest _)
                 (push 'pong events)
                 (funcall mu4e-pong-func nil))))
      (ad:mailx-start-for-draft)
      (ad:mailx-draft
       (lambda ()
         (should mu4e--started)
         (push 'compose events)
         nil))
      (should (equal (nreverse events) '(start pong compose))))))

(ert-deftest mailx-context/draft-save-and-send-require-original-database ()
  (skip-unless (file-readable-p file))

  (with-temp-buffer
    (let ((mu4e-mu-home "/tmp/mail-b/.mu")
          (mu4e--server-process nil))
      (setq major-mode 'mu4e-compose-mode)
      (setq buffer-file-name "/tmp/mail-a/drafts/cur/message")
      (mailx-set-up-message-mode-buffer)
      (should (memq #'mailx--guard-draft-database message-send-hook))
      (cl-letf (((symbol-function 'mu4e-running-p) (lambda () t)))
        (should-error (run-hooks 'message-send-hook) :type 'user-error)
        (setq mu4e-mu-home "/tmp/mail-a/.mu")
        (mailx--guard-draft-database)))))

(ert-deftest mailx-context/save-buffer-refuses-wrong-database ()
  (let ((file (make-temp-file "mail-draft-save-"))
        (mu4e-mu-home "/tmp/mail-b/.mu"))
    (unwind-protect
        (progn
          (with-temp-file file (insert "original"))
          (with-current-buffer (find-file-noselect file)
            (unwind-protect
                (progn
                  (setq major-mode 'mu4e-compose-mode)
                  (mailx-set-up-message-mode-buffer)
                  (erase-buffer)
                  (insert "changed")
                  (cl-letf (((symbol-function 'mu4e-running-p) (lambda () t)))
                    (should-error (save-buffer) :type 'user-error))
                  (should (buffer-modified-p))
                  (should (equal (with-temp-buffer
                                   (insert-file-contents file)
                                   (buffer-string))
                                 "original")))
              (set-buffer-modified-p nil)
              (kill-buffer (current-buffer)))))
      (delete-file file))))

(ert-deftest mailx-context/reopened-draft-save-and-send-guards ()
  (let ((root (make-temp-file "mail-reopened-" t)))
    (unwind-protect
        (dolist (account '("a" "b"))
          (let* ((directory (expand-file-name account root))
                 (file (expand-file-name "drafts/cur/message" directory))
                 (mu4e-mu-home (expand-file-name ".mu" directory)))
            (make-directory (file-name-directory file) t)
            (with-temp-file file (insert "original"))
            (with-current-buffer (find-file-noselect file)
              (unwind-protect
                  (progn
                    (setq major-mode 'mu4e-compose-mode)
                    (mailx-set-up-message-mode-buffer)
                    (insert " updated")
                    (cl-letf (((symbol-function 'mu4e-running-p) (lambda () t)))
                      (save-buffer)
                      (mailx--guard-draft-database)
                      (setq mu4e-mu-home (expand-file-name "other/.mu" root))
                      (insert " blocked")
                      (should-error (save-buffer) :type 'user-error)
                      (should-error (mailx--guard-draft-database) :type 'user-error)))
                (set-buffer-modified-p nil)
                (kill-buffer (current-buffer))))))
      (delete-directory root t))))

(ert-deftest mailx-context/unknown-draft-is-blocked ()
  (let ((mu4e-mu-home "/tmp/mail-a/.mu"))
    (with-temp-buffer
      (setq major-mode 'mu4e-compose-mode)
      (setq buffer-file-name "/tmp/mail-b/drafts/cur/message")
      (cl-letf (((symbol-function 'mu4e-running-p) (lambda () t)))
        (should-error (mailx--guard-draft-database) :type 'user-error)))))

(ert-deftest mailx-context/reopened-draft-refuses-mismatched-sender ()
  (with-temp-buffer
    (message-mode)
    (setq major-mode 'mu4e-compose-mode)
    (insert "From: Alpha <alpha@example.org>\nTo: other@example.org\nSubject: test\n"
            mail-header-separator "\n\nBody\n")
    (unwind-protect
        (let ((user-mail-address "beta@example.org")
              (message-sendmail-extra-arguments '("-a" "beta@example.org")))
          (mailx-set-up-message-mode-buffer)
          (should (memq #'mailx--guard-draft-identity message-send-hook))
          (should-error (mailx--guard-draft-identity) :type 'user-error)
          (setq-local user-mail-address "alpha@example.org")
          (setq-local message-sendmail-extra-arguments '("-a" "alpha@example.org"))
          (mailx--guard-draft-identity))
      (set-buffer-modified-p nil))))

(ert-deftest mailx-context/header-separator-keeps-configured-value ()
  (should (equal (substring-no-properties mu4e--header-separator) mail-header-separator))
  (should (get-text-property 0 'read-only mu4e--header-separator)))

(ert-deftest mailx-context/sender-guard-rejects-unconfigured-alias ()
  (with-temp-buffer
    (message-mode)
    (insert "From: Beta <beta+alias@example.org>\nTo: x@example.org\nSubject: test\n"
            mail-header-separator "\n\nBody\n")
    (setq-local user-mail-address "beta@example.org")
    (should-error (mailx--guard-draft-identity) :type 'user-error)
    (set-buffer-modified-p nil)))

(ert-deftest mailx-context/cite-style-keeps-configured-citation-line ()
  (require 'message)
  (should (equal message-citation-line-format "On %A, %B %d %Y, %T, %N wrote:\n"))
  (should (assq 'message-cite-reply-position message-cite-style))
  (should-not (assq 'message-citation-line-format message-cite-style))
  (should-not (assq 'message-citation-line-function message-cite-style)))

(ert-deftest mailx-context/failure-preserves-previous-context ()
  (mailx-test--with-accounts
    (let* ((old-context (car mu4e-contexts))
           (mu4e--context-current old-context)

           (mu4e-mu-home "/tmp/mail-a/.mu")
           (mu4e--started nil)
           (mu4e--server-indexing nil)
           (mu4e--update-buffer nil)
           (mu4e-compose-parent-message nil)
           (mu4e-context-changed-hook nil)
           (running t))
      (cl-letf (((symbol-function 'buffer-list) (lambda (&rest _) nil))
                ((symbol-function 'yes-or-no-p) (lambda (&rest _) nil))
                ((symbol-function 'mu4e-running-p) (lambda () running))
                ((symbol-function 'mu4e-quit) (lambda (&rest _) (error "shutdown failed"))))
        (should-error (mu4e-context-switch nil "b"))
        (should (eq (mu4e-context-current) old-context))
        (should (equal mu4e-mu-home "/tmp/mail-a/.mu"))
        (mu4e-context-switch nil "a")
        (should (equal mu4e-mu-home "/tmp/mail-a/.mu"))))))

(ert-deftest mailx-context/personal-default-selection ()
  "The personal configuration selects the expected default context."
  (let* ((file (expand-file-name "user/personal.el" test-helper-root))
         (mu4e-contexts nil)
         (mu4e--context-current nil)
         selected config advised timer-scheduled refile-rule)
    (skip-unless (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (condition-case nil
          (while t
            (let ((form (read (current-buffer))))
              (cond
               ((and (eq (car-safe form) 'setq) (eq (cadr form) 'mu4e-contexts))
                (push form config))
               ((and (eq (car-safe form) 'defun)
                     (memq (cadr form) '(personal-mail-ensure-context personal-mail-refile-coop)))
                (push form config))
               ((and (eq (car-safe form) 'advice-add)
                     (member (cadr form) '('mu4e 'mu4e-update-index 'mu4e-update-mail-and-index)))
                (push form config))
               ((equal (seq-take form 2) '(with-eval-after-load 'mailx))
                (setq config (append (reverse (cddr form)) config))))))
        (end-of-file nil)))
    (should config)
    (cl-letf (((symbol-function 'personal-mail-ensure-context) #'ignore)
              ((symbol-function 'advice-add)
               (lambda (symbol &rest _) (push symbol advised)))
              ((symbol-function 'run-with-idle-timer)
               (lambda (&rest _) (setq timer-scheduled t)))
              ((symbol-function 'mailx-add-refile-rule)
               (lambda (rule) (setq refile-rule rule)))
              ((symbol-function 'mu4e-context-switch)
               (lambda (_force name) (setq selected name))))
      (eval `(progn ,@(nreverse config)) t)
      (should (= (length mu4e-contexts) 4))
      (should-not timer-scheduled)
      (should (eq refile-rule 'personal-mail-refile-coop))
      (should (equal (funcall refile-rule '(:from ((:email "board@managebuilding.com")))) "/coop"))
      (should-not (funcall refile-rule '(:from ((:email "board@example.com")))))
      (should (equal (sort advised #'string<)
                     (sort '(mu4e mu4e-update-index mu4e-update-mail-and-index) #'string<)))
      (dolist (case '(("derrida" "work" "tychoish-mail-kleinman")
                      ("derrida" "tychoish" "tychoish-mail-sam")
                      ("derrida" "hud" "tychoish-mail-tychoish")
                      ("derrida" "other" "tychoish-mail-work")
                      ("unknown" "work" "tychoish-mail-work")
                      ("deleuze" "work" "tychoish-mail-work")))
        (cl-letf (((symbol-function 'system-name) (lambda () (nth 0 case)))
                  ((symbol-function 'sprite-instance-name) (lambda () (nth 1 case))))
          (setq selected nil)
          (personal-mail-ensure-context)
          (should (equal selected (nth 2 case))))))))

(provide 'test-mail-context)
;;; test-mail-context.el ends here
