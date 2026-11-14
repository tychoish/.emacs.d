;;; setup-mail.el --- Mail configuration and account management -*- lexical-binding: t; -*-

;;; Commentary:
;; Mu4e email contexts, account switching, and refile rules.

;;; Code:


(require 'map)
(require 'cl-lib)

(autoload 'mu4e "mu4e" nil t)
(autoload 'mu4e-update-index "mu4e" nil t)
(autoload 'mu4e-update-mail-and-index "mu4e" nil t)
(autoload 'mu4e-quit "mu4e" nil t)

(declare-function message-field-value "message" (header &optional first))
(declare-function mu4e "mu4e")
(declare-function mu4e-compose-reply "mu4e-compose")
(declare-function mu4e-running-p "mu4e-server")
(declare-function make-mu4e-context "mu4e-context")
(declare-function mu4e-context-switch "mu4e-context")
(declare-function mu4e-compose-context-switch "mu4e-compose")
(declare-function mu4e-context-current "mu4e-context")
(declare-function mu4e-headers-mark-for-read "mu4e-headers")
(declare-function mu4e-headers-mark-for-unread "mu4e-headers")
(declare-function mu4e-mark-resolve-deferred-marks "mu4e-mark")
(declare-function mu4e-message-field "mu4e-message")
(declare-function mu4e-fetch-field "mu4e-message")
(declare-function mu4e-message-at-point "mu4e-message")
(declare-function mu4e-contact-email "mu4e-contacts")
(declare-function mu4e-contact-name "mu4e-contacts")

(eval-when-compile
  (require 'message)
  (require 'sendmail))

(defvar mm-discouraged-alternatives)
(defvar mml-secure-openpgp-sign-with-sender)
(defvar mu4e-compose-minor-mode-map)
(defvar mu4e-headers-actions)
(defvar mu4e-headers-mode-map)
(defvar mu4e-view-actions)
(defvar mu4e-refile-folder)
(defvar mu4e-bookmarks)
(defvar mu4e-compose-complete-only-after)
(defvar mu4e--header-separator)
(defvar mu4e-search-include-related)
(defvar mu4e-search-results-limit)
(defvar mu4e-mu-home)
(defvar mu4e--started)
(defvar mu4e--server-indexing)
(defvar mu4e--update-buffer)
(defvar mu4e-confirm-quit)
(defvar mu4e-context-policy)
(defvar mu4e-compose-context-policy)
(defvar mu4e--server-process)
(defvar mu4e-pong-func)
(defvar mu4e--context-current)
(defvar mu4e-get-mail-command)
(defvar message-signature-directory)
(defvar message-signature-file)
(defvar message-sendmail-extra-arguments)
(defvar mail-host-address)
(defvar tychoish-mail--composing nil
  "Non-nil while mu4e creates a draft.")
(defvar-local tychoish-mail--draft-home nil
  "Database home owning this draft, or nil if ownership is unknown.")

(defvar tychoish-mail--server-home nil
  "Database home of the current mu server, or nil when not running.")
(defvar tychoish-mail--transitioning nil
  "Non-nil while starting a replacement mu server.")
(defvar tychoish-mail--switching-compose-context nil
  "Non-nil inside `mu4e-compose-context-switch'.")

(add-to-list 'auto-mode-alist '(".*mutt.*" . message-mode))

(add-to-list 'display-buffer-alist '(tychoish--mu4e-buffer-p (display-buffer-reuse-window) (reusable-frames . t)))

(with-eval-after-load 'mu4e
  (seq-do (lambda (hook) (add-hook hook #'cmdr-frame-record-home-frame))
          '(mu4e-main-mode-hook
            mu4e-headers-mode-hook
            mu4e-view-mode-hook
            mu4e-compose-mode-hook)))

(with-eval-after-load 'message
  (keymap-set message-mode-map "M-q" #'ignore)
  (add-to-list 'mm-discouraged-alternatives "text/richtext")
  (add-to-list 'mm-discouraged-alternatives "text/html")
  (set-face-attribute 'message-separator nil :background (face-attribute 'default :background nil))
  ;; Gmail style minus its citation line, so `message-citation-line-format'
  ;; applies to both mu4e replies and `message-yank-original'.
  (setq message-cite-style
        (seq-remove (lambda (binding)
                      (memq (car binding) '(message-citation-line-function
                                            message-citation-line-format)))
                    message-cite-style-gmail)))

(with-eval-after-load 'mu4e-compose
  (keymap-set mu4e-compose-minor-mode-map "R" #'compose-reply-wide-or-not-please-ask)
  (keymap-set mu4e-compose-minor-mode-map "r" #'mu4e-headers-mark-for-read))

(with-eval-after-load 'mu4e-headers
  (add-to-list 'mu4e-headers-actions '("generate refile rule" . tychoish-mail-generate-refile-rule) t)
  (keymap-set mu4e-headers-mode-map "C-r" #'compose-reply-wide-or-not-please-ask)
  (keymap-set mu4e-headers-mode-map "o" #'mu4e-headers-mark-for-unread)
  (keymap-set mu4e-headers-mode-map "u" #'mu4e-headers-mark-for-unread)
  (keymap-set mu4e-headers-mode-map ";" #'mu4e-mark-resolve-deferred-marks))

(with-eval-after-load 'mu4e-view
  (add-to-list 'mu4e-view-actions '("unsubscribe" . tychoish-mail-unsubscribe) t)
  (add-to-list 'mu4e-view-actions '("generate refile rule" . tychoish-mail-generate-refile-rule) t))

(with-eval-after-load 'files
  (advice-add 'basic-save-buffer :before #'ad:tychoish-mail-guard-save))

(with-eval-after-load 'mu4e-server
  (advice-add 'mu4e--server-start :after #'tychoish-mail--record-server-home))

(with-eval-after-load 'mu4e-context
  (advice-add 'mu4e-context-switch :around #'ad:tychoish-mail-context-switch))

(with-eval-after-load 'mu4e-compose
  (advice-add 'mu4e-compose-context-switch :around #'ad:tychoish-mail-compose-context-switch))

(with-eval-after-load 'mu4e-draft
  (advice-add 'mu4e--draft :around #'ad:tychoish-mail-draft)
  (setq mu4e--header-separator (propertize mail-header-separator 'read-only t 'intangible t)))

(add-hook 'mu4e-compose-mode-hook 'turn-off-hard-wrap)
(add-hook 'mu4e-compose-mode-hook 'whitespace-cleanup)
(add-hook 'mu4e-compose-mode-hook 'tychoish-mail-set-up-message-mode-buffer)

(defconst tychoish-mail--refile-matchable-fields
  '((:from . address)
    (:to . address)
    (:cc . address)
    (:subject . string)
    (:maildir . string)
    (:list . string))
  "Alist of mu4e message fields available for refile rule generation.")

(defvar tychoish-mail--refile-rules nil
  "Functions of one argument MSG, tried in order by `tychoish-mail-refile-folder'.")

(setq mu4e-bookmarks
      '((:name "primary queues to file"
         :query "m:/inbox OR m:/prof"
         :key ?f)
        (:name "to read/process queue"
	 :query "m:/inbox OR flag:unread AND NOT (OR m:/sent OR flag:trashed OR m:/trash)"
	 :key ?q)
        (:name "all unread message"
	 :query "m:/inbox OR flag:unread AND NOT (flag:trashed OR m:/sent OR m:/trash)"
	 :key ?a)
        (:name "all sorted email"
	 :query "(NOT m:/inbox AND NOT m:/prof) AND flag:unread"
	 :key ?s)
        (:name "messages with images"
	 :query "mime:image/*"
	 :key ?p)
        (:name "mesages from today"
	 :query "date:today..now"
	 :key ?t)
        (:name "messages from the last week"
	 :query "date:7d..now"
	 :key ?w)))

(setq compose-mail-user-agent-warnings nil)
(setq sendmail-program "msmtp")

(setq mml-secure-openpgp-sign-with-sender t)

(setq mail-specify-envelope-from t)
(setq mail-user-agent 'mu4e-user-agent)
(setq mail-header-separator "--------------------------")

(setq mu4e-refile-folder #'tychoish-mail-refile-folder)
(setq mu4e-compose-context-policy 'ask)

(setq mu4e-compose-complete-only-after "2015-01-01")
(setq mu4e-search-include-related nil)
(setq mu4e-search-results-limit 1000)

(setq message-citation-line-format "On %A, %B %d %Y, %T, %N wrote:\n")
(setq message-citation-line-function 'message-insert-formatted-citation-line)
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq message-fill-column 80)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun tychoish-mail-set-up-message-mode-buffer ()
  "Configure local settings for mu4e composition."
  (setq-local use-hard-newlines t)
  (setq-local make-backup-files nil)
  (seq-do #'make-local-variable
          '(mu4e--context-current user-mail-address user-full-name
            message-directory message-auto-save-directory
            message-signature-directory message-signature-file message-signature
            mail-host-address message-sendmail-extra-arguments mu4e-get-mail-command))
  (add-hook 'before-save-hook #'tychoish-mail--guard-draft-database nil t)
  (add-hook 'message-send-hook #'tychoish-mail--guard-draft-database nil t)
  (add-hook 'message-send-hook #'tychoish-mail--guard-draft-identity nil t))

;;;###autoload
(defun tychoish-mail-add-refile-rule (rule)
  "Register RULE, a function of one argument MSG, as a refile rule.
RULE should return a maildir folder string, or nil to fall through to the
next registered rule."
  (add-to-list 'tychoish-mail--refile-rules rule t))

(defun tychoish-mail-refile-folder (msg)
  "Return refile folder for MSG by trying each registered rule."
  (or (seq-some (lambda (rule) (funcall rule msg)) tychoish-mail--refile-rules)
      "/archive"))

(defun tychoish-mail--field-display-value (msg field)
  "Return a human-readable string for FIELD value in MSG."
  (let ((val (mu4e-message-field msg field)))
    (cond
     ((null val) "(none)")
     ((and (listp val) (consp (car val)))
      (mapconcat (lambda (ct)
                   (or (mu4e-contact-name ct)
                       (mu4e-contact-email ct)
                       "?"))
                 val ", "))
     ((stringp val) val)
     (t (format "%S" val)))))

(defun tychoish-mail-generate-refile-rule (&optional msg)
  "Build a `tychoish-mail-add-refile-rule' form from MSG or the current message.
Prompts for a field (annotated with the message's current value), a regex
pattern, and a destination folder.  Puts the resulting form on the kill ring."
  (interactive)
  (let* ((msg (or msg (mu4e-message-at-point)
                  (user-error "No message at point")))
         (candidates
          (seq-map (lambda (entry)
                     (cons (symbol-name (car entry))
                           (tychoish-mail--field-display-value msg (car entry))))
                   tychoish-mail--refile-matchable-fields))
         (field-name (annotated-completing-read candidates
						:prompt "Match field: " :require-match t))
         (field (intern field-name))
         (field-type (map-elt tychoish-mail--refile-matchable-fields field))
         (regex (read-string
                 (format "Regex (current: %s): "
                         (tychoish-mail--field-display-value msg field))))
         (folder (read-string "Refile to folder: " "/"))
         (body (pcase field-type
                 ('address
                  `(when (seq-some (lambda (addr)
                                     (when-let* ((email (mu4e-contact-email addr)))
                                       (string-match-p ,regex email)))
                                   (mu4e-message-field msg ,field))
                     ,folder))
                 ('string
                  `(when (string-match-p ,regex
                                         (or (mu4e-message-field msg ,field) ""))
                     ,folder)))))
    (kill-new (pp-to-string `(tychoish-mail-add-refile-rule (lambda (msg) ,body))))
    (message "Refile rule copied to kill ring")))

(defun compose-reply-wide-or-not-please-ask ()
  "Ask whether to reply-to-all or not."
  (interactive)
  (mu4e-compose-reply (yes-or-no-p "Reply to all?")))

(defun tychoish-mail-unsubscribe (msg)
  "Unsubscribe from a mailing list using the List-Unsubscribe header in MSG.
For mailto: URIs, opens a compose buffer pre-filled with the unsubscribe
address, subject, and body.  For https: URIs, opens the URL in a browser."
  (interactive (list (mu4e-message-at-point)))
  (let* ((header (or (mu4e-fetch-field msg "List-Unsubscribe")
                     (user-error "No List-Unsubscribe header found")))
         (uris (let (result)
                 (with-temp-buffer
                   (insert header)
                   (goto-char (point-min))
                   (while (re-search-forward "<\\([^>]+\\)>" nil t)
                     (push (match-string 1) result)))
                 (nreverse result))))
    (unless uris
      (user-error "No unsubscribe URIs in List-Unsubscribe header"))
    (let ((uri (if (cdr uris)
                   (completing-read "Unsubscribe via: " uris nil t)
                 (car uris))))
      (cond
       ((string-prefix-p "mailto:" uri)
        (require 'url-mailto)
        (url-mailto (url-generic-parse-url uri)))
       ((string-match-p "\\`https?://" uri)
        (browse-url uri))
       (t
        (user-error "Unrecognized URI scheme in List-Unsubscribe: %s" uri))))))

;; display-buffer: mu4e buffers prefer the frame they were first shown on
(defun tychoish--mu4e-buffer-p (buf _action)
  "Return non-nil if BUF uses a mu4e major mode."
  (with-current-buffer buf
    (derived-mode-p 'mu4e-main-mode 'mu4e-headers-mode
                    'mu4e-view-mode 'mu4e-compose-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Contexts and database switching

(defun tychoish-mail-select-account ()
  "Select a mu4e context, updating the current draft when appropriate."
  (interactive)
  (require 'mu4e-context)
  (if (derived-mode-p 'mu4e-compose-mode)
      (call-interactively #'mu4e-compose-context-switch)
    (call-interactively #'mu4e-context-switch)))

(defun tychoish-mail-switch-context (name)
  "Select the mu4e context NAME."
  (require 'mu4e-context)
  (if (derived-mode-p 'mu4e-compose-mode)
      (mu4e-compose-context-switch nil name)
    (mu4e-context-switch nil name)))

(defun ad:tychoish-mail-context-switch (original &optional force name)
  "Call ORIGINAL with FORCE and NAME, keeping draft storage unchanged."
  (cond
   ((and (derived-mode-p 'mu4e-compose-mode)
         (not tychoish-mail--switching-compose-context))
    (mu4e-compose-context-switch force name)
    (mu4e-context-current))
   ((or tychoish-mail--composing (derived-mode-p 'mu4e-compose-mode))
    (let ((directory message-directory)
          (autosave message-auto-save-directory)
          (fetch mu4e-get-mail-command))
      (unwind-protect
          (funcall original force name)
        (setq message-directory directory)
        (setq message-auto-save-directory autosave)
        (setq mu4e-get-mail-command fetch))))
   (t (funcall original force name))))

(defun tychoish-mail--record-server-home (&rest _args)
  "Remember the home used to start the running mu server."
  (when (mu4e-running-p)
    (setq tychoish-mail--server-home mu4e-mu-home)))

(defun tychoish-mail--guard-draft-database (&rest _args)
  "Refuse draft operations unless its owning database is active."
  (unless (and tychoish-mail--draft-home
               (mu4e-running-p)
               (equal tychoish-mail--draft-home
                      (or tychoish-mail--server-home mu4e-mu-home)))
    (user-error "Switch to this draft's database before saving or sending")))

(defun ad:tychoish-mail-guard-save (&rest _args)
  "Guard saving mu4e drafts.
`before-save-hook' errors are demoted by `basic-save-buffer', so this
must be advice to be able to abort the save."
  (when (derived-mode-p 'mu4e-compose-mode)
    (tychoish-mail--guard-draft-database)))

(defun tychoish-mail--guard-draft-identity (&rest _args)
  "Refuse sending when the From header disagrees with the selected identity."
  (require 'mail-extr)
  (let* ((from (message-field-value "From"))
         (address (and from (cadr (mail-extract-address-components from)))))
    (unless (and address user-mail-address
                 (equal (downcase address) (downcase user-mail-address)))
      (user-error "Select the context matching this draft's From before sending"))))

(defun tychoish-mail--await-server-ready ()
  "Wait until the running mu server has completed its startup handler."
  (unless mu4e--started
    (let* ((ready nil)
           (deadline (+ (float-time) 10))
           (handler mu4e-pong-func)
           (wrapper (lambda (info)
                      (funcall handler info)
                      (setq ready t))))
      (unwind-protect
          (progn
            (setq mu4e-pong-func wrapper)
            (seq-some (lambda (_attempt)
                        (or ready
                            (not (mu4e-running-p))
                            (>= (float-time) deadline)
                            (progn (accept-process-output mu4e--server-process 0.1)
                                   nil)))
                      (number-sequence 1 100)))
        (when (eq mu4e-pong-func wrapper)
          (setq mu4e-pong-func handler)))
      (unless (and ready (mu4e-running-p))
        (error "Mu server did not become ready for %s" mu4e-mu-home)))))

(defun ad:tychoish-mail-draft (original &rest args)
  "Call ORIGINAL with ARGS, isolating its identity and database ownership."
  ;; Start first so the instance's default context selects the database
  ;; before compose-time identity selection is isolated.
  (unless (mu4e-running-p)
    (mu4e 'background))
  (tychoish-mail--await-server-ready)
  (let ((tychoish-mail--composing t)
        (mu4e--context-current mu4e--context-current)
        (user-mail-address user-mail-address)
        (user-full-name user-full-name)
        (message-directory message-directory)
        (message-auto-save-directory message-auto-save-directory)
        (message-signature-directory message-signature-directory)
        (message-signature-file message-signature-file)
        (message-signature message-signature)
        (mail-host-address mail-host-address)
        (message-sendmail-extra-arguments message-sendmail-extra-arguments)
        (mu4e-get-mail-command mu4e-get-mail-command))
    (let* ((draft (apply original args))
           (home (or tychoish-mail--server-home mu4e-mu-home))
           (root (and home (file-name-directory (directory-file-name home)))))
      (when (buffer-live-p draft)
        (with-current-buffer draft
          (setq-local tychoish-mail--draft-home
                      (and root buffer-file-name
                           (file-in-directory-p buffer-file-name root)
                           home))))
      draft)))

(defun ad:tychoish-mail-compose-context-switch (original &rest args)
  "Call ORIGINAL with ARGS while allowing its nested context switch."
  (let ((tychoish-mail--switching-compose-context t))
    (apply original args)))

(defun tychoish-mail--switch-database (home)
  "Prepare the mu database at HOME before entering its context.
Open drafts keep their original paths and are guarded during save and send."
  (let* ((running (and (featurep 'mu4e) (mu4e-running-p)))
         (old-home (if running
                       (or tychoish-mail--server-home mu4e-mu-home)
                     mu4e-mu-home)))
    (when (not (equal old-home home))
      (when (or mu4e--server-indexing
                (and (buffer-live-p mu4e--update-buffer)
                     (process-live-p (get-buffer-process mu4e--update-buffer))))
        (user-error "Cannot switch mail databases while an update is running"))
      (when running
        (let ((mu4e-confirm-quit nil))
          (mu4e-quit))
        (when (mu4e-running-p)
          (error "Failed to stop the previous mu server")))
      (setq tychoish-mail--server-home nil)
      (unless running
        (setq mu4e--started nil)))
    (setq mu4e-mu-home home)
    (when (and running (not (equal old-home home)))
      ;; Startup completes on pong, not when `mu4e' returns.
      (condition-case err
          (let ((mu4e-context-policy nil)
                (tychoish-mail--transitioning t))
            (mu4e 'background)
            (tychoish-mail--await-server-ready)
            (setq tychoish-mail--server-home home))
        ((error quit)
         (unwind-protect
             (when (mu4e-running-p)
               (let ((mu4e-confirm-quit nil)) (mu4e-quit)))
           (setq mu4e--started nil)
           (if (mu4e-running-p)
               (progn
                 (setq tychoish-mail--server-home home)
                 (setq mu4e-mu-home home))
             (setq tychoish-mail--server-home nil)
             (setq mu4e-mu-home old-home)))
         (signal (car err) (cdr err)))))))

(cl-defun tychoish-mail-make-context (&key name full-name address maildir command)
  "Build a mu4e context NAME for ADDRESS and FULL-NAME.
MAILDIR is the account's mail root and COMMAND fetches its mail."
  (let ((maildir (expand-file-name maildir)))
    (make-mu4e-context
     :name name
     :enter-func (lambda ()
                   (unless (or tychoish-mail--composing
                               (derived-mode-p 'mu4e-compose-mode))
                     (tychoish-mail--switch-database
                      (file-name-concat maildir ".mu"))))
     :match-func
     (lambda (msg)
       (when msg
         (seq-some (lambda (contact)
                     (when-let* ((email (mu4e-contact-email contact)))
                       (equal (downcase email) (downcase address))))
                   (append (mu4e-message-field msg :to)
                           (mu4e-message-field msg :cc)))))
     :vars `((user-mail-address . ,address)
             (user-full-name . ,full-name)
             (message-directory . ,maildir)
             (message-auto-save-directory . ,(file-name-concat maildir "drafts"))
             (message-signature-directory . ,(file-name-concat maildir "tools" "signatures"))
             (message-signature-file . ,address)
             (message-signature . t)
             (mail-host-address . ,(replace-regexp-in-string ".*@" "" address))
             (message-sendmail-extra-arguments . ,(list "-a" address))
             (mu4e-get-mail-command . ,command)))))

(provide 'setup-mail)
;;; setup-mail.el ends here
