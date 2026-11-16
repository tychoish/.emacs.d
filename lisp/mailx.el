;;; mailx.el --- Mail configuration and account management -*- lexical-binding: t; -*-

;;; Commentary:
;; Mu4e email contexts, account switching, and refile rules.

;;; Code:


(require 'map)
(require 'cl-lib)
(require 'mailx-server)

(autoload 'mu4e "mu4e" nil t)
(autoload 'mu4e-update-index "mu4e" nil t)
(autoload 'mu4e-update-mail-and-index "mu4e" nil t)
(autoload 'mu4e-quit "mu4e" nil t)
(autoload 'make-mu4e-context "mu4e-context")
(autoload 'mu4e-context-current "mu4e-context")
(autoload 'mu4e-context-switch "mu4e-context" nil t)
(autoload 'mu4e-compose-context-switch "mu4e-compose" nil t)

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
(defvar mu4e-compose-context-policy)
(defvar mu4e--context-current)
(defvar mu4e-get-mail-command)
(defvar message-signature-directory)
(defvar message-signature-file)
(defvar message-sendmail-extra-arguments)
(defvar mail-host-address)
(defvar mailx--composing nil
  "Non-nil while mu4e creates a draft.")

(add-to-list 'auto-mode-alist '(".*mutt.*" . message-mode))

(add-to-list 'display-buffer-alist '(mailx--buffer-p (display-buffer-reuse-window) (reusable-frames . t)))

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
  (keymap-set mu4e-compose-minor-mode-map "R" #'mailx-compose-reply)
  (keymap-set mu4e-compose-minor-mode-map "r" #'mu4e-headers-mark-for-read)
  (keymap-set mu4e-compose-minor-mode-map "<remap> <mu4e-context-switch>"
              #'mu4e-compose-context-switch))

(with-eval-after-load 'mu4e-headers
  (add-to-list 'mu4e-headers-actions '("generate refile rule" . mailx-generate-refile-rule) t)
  (keymap-set mu4e-headers-mode-map "C-r" #'mailx-compose-reply)
  (keymap-set mu4e-headers-mode-map "o" #'mu4e-headers-mark-for-unread)
  (keymap-set mu4e-headers-mode-map "u" #'mu4e-headers-mark-for-unread)
  (keymap-set mu4e-headers-mode-map ";" #'mu4e-mark-resolve-deferred-marks))

(with-eval-after-load 'mu4e-view
  (add-to-list 'mu4e-view-actions '("unsubscribe" . mailx-unsubscribe) t)
  (add-to-list 'mu4e-view-actions '("generate refile rule" . mailx-generate-refile-rule) t))

(advice-add 'basic-save-buffer :before #'ad:mailx-guard-save)

(advice-add 'mu4e--draft :around #'ad:mailx-draft)
(advice-add 'mu4e--draft :before #'ad:mailx-start-for-draft '((depth . -50)))

(add-hook 'mu4e-compose-mode-hook 'turn-off-hard-wrap)
(add-hook 'mu4e-compose-mode-hook 'whitespace-cleanup)
(add-hook 'mu4e-compose-mode-hook 'mailx-set-up-message-mode-buffer)

(defconst mailx--refile-matchable-fields
  '((:from . address)
    (:to . address)
    (:cc . address)
    (:subject . string)
    (:maildir . string)
    (:list . string))
  "Alist of mu4e message fields available for refile rule generation.")

(defvar mailx--refile-rules nil
  "Functions of one argument MSG, tried in order by `mailx-refile-folder'.")

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

(with-eval-after-load 'mu4e-draft
  (setq mu4e--header-separator
        (propertize mail-header-separator 'read-only t 'intangible t)))

(setq mu4e-refile-folder #'mailx-refile-folder)
(setq mu4e-compose-context-policy 'ask)

(setq mu4e-compose-complete-only-after "2015-01-01")
(setq mu4e-search-include-related nil)
(setq mu4e-search-results-limit 1000)

(setq message-citation-line-format "On %A, %B %d %Y, %T, %N wrote:\n")
(setq message-citation-line-function 'message-insert-formatted-citation-line)
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq message-fill-column 80)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun mailx-set-up-message-mode-buffer ()
  "Configure local settings for mu4e composition."
  (setq-local use-hard-newlines t)
  (setq-local make-backup-files nil)
  (seq-do #'make-local-variable
          '(mu4e--context-current user-mail-address user-full-name
            message-directory message-auto-save-directory
            message-signature-directory message-signature-file message-signature
            mail-host-address message-sendmail-extra-arguments mu4e-get-mail-command))
  (add-hook 'message-send-hook #'mailx--guard-draft-database nil t)
  (add-hook 'message-send-hook #'mailx--guard-draft-identity nil t))

;;;###autoload
(defun mailx-add-refile-rule (rule)
  "Register RULE, a function of one argument MSG, as a refile rule.
RULE should return a maildir folder string, or nil to fall through to the
next registered rule."
  (add-to-list 'mailx--refile-rules rule t))

(defun mailx-refile-folder (msg)
  "Return refile folder for MSG by trying each registered rule."
  (or (seq-some (lambda (rule) (funcall rule msg)) mailx--refile-rules)
      "/archive"))

(defun mailx--field-display-value (msg field)
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

(defun mailx-generate-refile-rule (&optional msg)
  "Build a `mailx-add-refile-rule' form from MSG or the current message.
Prompts for a field (annotated with the message's current value), a regex
pattern, and a destination folder.  Puts the resulting form on the kill ring."
  (interactive)
  (let* ((msg (or msg (mu4e-message-at-point)
                  (user-error "No message at point")))
         (candidates
          (seq-map (lambda (entry)
                     (cons (symbol-name (car entry))
                           (mailx--field-display-value msg (car entry))))
                   mailx--refile-matchable-fields))
         (field-name (annotated-completing-read candidates
						:prompt "Match field: " :require-match t))
         (field (intern field-name))
         (field-type (map-elt mailx--refile-matchable-fields field))
         (regex (read-string
                 (format "Regex (current: %s): "
                         (mailx--field-display-value msg field))))
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
    (kill-new (pp-to-string `(mailx-add-refile-rule (lambda (msg) ,body))))
    (message "Refile rule copied to kill ring")))

(defun mailx-compose-reply ()
  "Ask whether to reply-to-all or not."
  (interactive)
  (mu4e-compose-reply (yes-or-no-p "Reply to all?")))

(defun mailx-unsubscribe (msg)
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
(defun mailx--buffer-p (buf _action)
  "Return non-nil if BUF uses a mu4e major mode."
  (with-current-buffer buf
    (derived-mode-p 'mu4e-main-mode 'mu4e-headers-mode
                    'mu4e-view-mode 'mu4e-compose-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Contexts and database switching

(defun mailx-select-account ()
  "Select a mu4e context, updating the current draft when appropriate."
  (interactive)
  (if (derived-mode-p 'mu4e-compose-mode)
      (call-interactively #'mu4e-compose-context-switch)
    (call-interactively #'mu4e-context-switch)))

(defun mailx--guard-draft-database (&rest _args)
  "Refuse draft operations outside the active mu database's mail root."
  (let* ((home (mailx-server-current-home))
         (root (and home (file-name-directory (directory-file-name home)))))
    (unless (and root buffer-file-name (mu4e-running-p)
                 (file-in-directory-p buffer-file-name root))
      (user-error "Switch to this draft's database before saving or sending"))))

(defun ad:mailx-guard-save (&rest _args)
  "Guard saving mu4e drafts.
`before-save-hook' errors are demoted by `basic-save-buffer', so this
must be advice to be able to abort the save."
  (when (derived-mode-p 'mu4e-compose-mode)
    (mailx--guard-draft-database)))

(defun mailx--guard-draft-identity (&rest _args)
  "Refuse sending when the From header disagrees with the selected identity."
  (require 'mail-extr)
  (let* ((from (message-field-value "From"))
         (address (and from (cadr (mail-extract-address-components from)))))
    (unless (and address user-mail-address
                 (equal (downcase address) (downcase user-mail-address)))
      (user-error "Select the context matching this draft's From before sending"))))

(defun ad:mailx-start-for-draft (&rest _args)
  "Start the default database and await its readiness before composing."
  (when mailx--transitioning
    (user-error "Wait for the mail database switch before composing"))
  (mailx-server-ensure-ready))

(defun ad:mailx-draft (original &rest args)
  "Call ORIGINAL with ARGS, isolating the draft's selected identity."
  (let ((mailx--composing t)
        (mu4e--context-current mu4e--context-current)
        (user-mail-address user-mail-address)
        (user-full-name user-full-name)
        (message-signature-directory message-signature-directory)
        (message-signature-file message-signature-file)
        (message-signature message-signature)
        (mail-host-address mail-host-address)
        (message-sendmail-extra-arguments message-sendmail-extra-arguments))
    (apply original args)))

(cl-defun mailx-make-context (&key name full-name address maildir command)
  "Build a mu4e context NAME for ADDRESS and FULL-NAME.
MAILDIR is the account's mail root and COMMAND fetches its mail."
  (let ((maildir (expand-file-name maildir)))
    (make-mu4e-context
     :name name
     :enter-func (lambda ()
                   (unless (or mailx--composing
                               (derived-mode-p 'mu4e-compose-mode))
                     (mailx-server-switch-database
                      (file-name-concat maildir ".mu"))
                     (setq message-directory maildir)
                     (setq message-auto-save-directory (file-name-concat maildir "drafts"))
                     (setq mu4e-get-mail-command command)))
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
             (message-signature-directory . ,(file-name-concat maildir "tools" "signatures"))
             (message-signature-file . ,address)
             (message-signature . t)
             (mail-host-address . ,(replace-regexp-in-string ".*@" "" address))
             (message-sendmail-extra-arguments . ,(list "-a" address))))))

(provide 'mailx)
;;; mailx.el ends here
