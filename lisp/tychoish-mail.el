;; -*- lexical-binding: t -*-

(eval-when-compile
  (require 'xtd-macro))

(require 'map)
(require 'mu4e-autoloads nil t)

(autoload 'mu4e-update-index "mu4e-update")
(autoload 'annotated-completing-read "annotated-completing-read")
(autoload 'annotated-completing-read-directory "annotated-completing-read")

(declare-function mu4e "mu4e")
(declare-function mu4e-compose-reply "mu4e-compose")
(declare-function mu4e-headers-mark-for-read "mu4e-headers")
(declare-function mu4e-headers-mark-for-unread "mu4e-headers")
(declare-function mu4e-headers-mark-for-something "mu4e-headers")
(declare-function mu4e-mark-resolve-deferred-marks "mu4e-mark")
(declare-function mu4e-message-field "mu4e-message")
(declare-function mu4e-fetch-field "mu4e-message")
(declare-function mu4e-message-at-point "mu4e-message")
(declare-function mu4e-contact-email "mu4e-contacts")
(declare-function mu4e-contact-name "mu4e-contacts")
(declare-function cape-capf-prefix-length "cape")

(defconst tychoish-mail-id-template "tychoish-mail-%s")
(defvar tychoish-mail-accounts-table (make-hash-table :test #'equal))
(defvar tychoish-mail-account-current nil)

(with-eval-after-load 'mu4e
  (seq-do (lambda (hook) (add-hook hook #'hud--record-home-frame))
          '(mu4e-main-mode-hook
            mu4e-headers-mode-hook
            mu4e-view-mode-hook
            mu4e-compose-mode-hook)))

(add-to-list 'auto-mode-alist '(".*mutt.*" . message-mode))

(with-eval-after-load 'message
  (keymap-set message-mode-map "M-q" #'ignore)
  (setq message-citation-line-format "On %A, %B %d %Y, %T, %N wrote:\n")
  (setq message-citation-line-function 'message-insert-formatted-citation-line)
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq message-fill-column 80)
  (setq message-cite-style message-cite-style-gmail)
  (setq message-dont-reply-to-names t)
  (add-to-list 'mm-discouraged-alternatives "text/richtext")
  (add-to-list 'mm-discouraged-alternatives "text/html")
  (set-face-attribute 'message-separator nil :background (face-attribute 'default :background nil)))

(with-eval-after-load 'mu4e-compose
  (keymap-set mu4e-compose-minor-mode-map "R" #'compose-reply-wide-or-not-please-ask)
  (keymap-set mu4e-compose-minor-mode-map "r" #'mu4e-headers-mark-for-read))

(with-eval-after-load 'mu4e-headers
  (add-to-list 'mu4e-headers-actions
               '("generate refile rule" . tychoish-mail-generate-refile-rule) t)
  (keymap-set mu4e-headers-mode-map "C-r" #'compose-reply-wide-or-not-please-ask)
  (keymap-set mu4e-headers-mode-map "R" #'compose-reply-wide-or-not-please-ask)
  (keymap-set mu4e-headers-mode-map "r" #'mu4e-headers-mark-for-read)
  (keymap-set mu4e-headers-mode-map "o" #'mu4e-headers-mark-for-unread)
  (keymap-set mu4e-headers-mode-map "u" #'mu4e-headers-mark-for-unread)
  (keymap-set mu4e-headers-mode-map "*" #'mu4e-headers-mark-for-something)
  (keymap-set mu4e-headers-mode-map "#" #'mu4e-mark-resolve-deferred-marks)
  (keymap-set mu4e-headers-mode-map ";" #'mu4e-mark-resolve-deferred-marks))

(with-eval-after-load 'mu4e-view
  (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)
  (add-to-list 'mu4e-view-actions '("unsubscribe" . tychoish-mail-unsubscribe) t)
  (add-to-list 'mu4e-view-actions '("generate refile rule" . tychoish-mail-generate-refile-rule) t))

(add-hook 'mu4e-compose-pre-hook #'tychoish-mail-auto-switch-for-reply)
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

(defun tychoish-mail-add-refile-rule (rule)
  "Register RULE, a function of one argument MSG, as a refile rule.
RULE should return a maildir folder string, or nil to fall through to the
next registered rule."
  (add-to-list 'tychoish-mail--refile-rules rule t))

(defun tychoish-mail-refile-folder (msg)
  "Return refile folder for MSG by trying each registered rule."
  (or (seq-some (lambda (rule) (funcall rule msg)) tychoish-mail--refile-rules)
      "/archive"))

(setq mu4e-refile-folder #'tychoish-mail-refile-folder)

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
         (field (intern (concat ":" field-name)))
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

(defun tychoish-mail-test-refile-rules ()
  "Test all refile rules on the current message and display results."
  (interactive)
  (let* ((msg (or (mu4e-message-at-point)
                  (user-error "No message at point")))
         (buf (get-buffer-create "*mu4e-refile-test*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "Refile test for: %s\n\n"
                      (or (mu4e-message-field msg :subject) "(no subject)")))
      (if (null tychoish-mail--refile-rules)
          (insert "No refile rules defined.\n")
        (seq-map-indexed
         (lambda (rule i)
           (let ((result (condition-case err
                             (funcall rule msg)
                           (error (format "ERROR: %S" err)))))
             (insert (format "Rule %d: %s\n" (1+ i) (or result "no match")))))
         tychoish-mail--refile-rules))
      (insert (format "\nFinal result: %s\n"
                      (tychoish-mail-refile-folder msg))))
    (display-buffer buf)))

(setq mu4e-bookmarks
      '((:name "unread primary queues to file"
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
        (:name "inbox and prof (all)"
	 :query "m:/inbox OR m:/prof"
	 :key ?i)
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

(setq mail-imenu-generic-expression
      '(("Subject"  "^Subject: *\\(.*\\)" 1)
        ("Cc"       "^C[Cc]: *\\(.*\\)" 1)
        ("Bcc"      "^B[Cc]: *\\(.*\\)" 1)
        ("To"       "^To: *\\(.*\\)" 1)
        ("From"     "^From: *\\(.*\\)" 1)))

(setq mu4e-compose-complete-only-after "2015-01-01")
(setq mu4e-search-include-related nil)
(setq mu4e-search-results-limit 1000)

(defun tychoish-mail--reset-header-separator ()
  "Set `mail-header-separator' and mu4e's private mirror to the same value."
  (setq mail-header-separator (propertize "--------------------------" 'read-only t 'intangible t))
  (setq mu4e--header-separator mail-header-separator))

(tychoish-mail--reset-header-separator)

(defun tychoish-mail-set-up-message-mode-buffer ()
  (tychoish-mail--reset-header-separator)
  ;; mu4e--compose-setup-completion
  (setq-local completion-at-point-functions
	      `(,(cape-capf-prefix-length 'mu4e--compose-complete-contact-field 4)
		cape-emoji
		cape-dict
		tempel-complete))

  (setq-local use-hard-newlines t)
  (setq-local make-backup-files nil))

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
        (let* ((rest (substring uri (length "mailto:")))
               (parts (split-string rest "?"))
               (to (car parts))
               (params (when (cadr parts)
                         (url-parse-query-string (cadr parts))))
               (subject (cadr (assoc "subject" params)))
               (body (cadr (assoc "body" params))))
          (compose-mail to subject)
          (when body
            (save-excursion
              (goto-char (point-max))
              (insert body)))))
       ((string-match-p "\\`https?://" uri)
        (browse-url uri))
       (t
        (user-error "Unrecognized URI scheme in List-Unsubscribe: %s" uri))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; display-buffer configuration
;; Mark mu4e buffers as frame-sticky when first displayed

;; display-buffer: mu4e buffers prefer the frame they were first shown on
(defun tychoish--mu4e-buffer-p (buf _action)
  "Return non-nil if BUF uses a mu4e major mode."
  (with-current-buffer buf
    (derived-mode-p 'mu4e-main-mode 'mu4e-headers-mode
                    'mu4e-view-mode 'mu4e-compose-mode)))

(add-to-list 'display-buffer-alist
             '(tychoish--mu4e-buffer-p
               (display-buffer-reuse-window)
               (reusable-frames . t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; account configuration

(cl-defstruct (tychoish-mail-account
               (:constructor tychoish-mail-make-account
                             (&key id maildir name address keybinding (fetchmail mu4e-get-mail-command)
				   &aux (maildir (if (stringp maildir)
						      (expand-file-name maildir)
						    (user-error "maildir must be a string"))))))
  "Track mail account configurations.
Used internally by `tychoish-define-mail-account'.  Signatures always
live in MAILDIR's tools/signatures directory."
  (maildir
   (expand-file-name "~/mail")
   :documentation "path for maildirs"
   :type 'string)
  (id
   ""
   :documentation "symbol of function that activates this account"
   :type 'symbol)
  (address
   user-mail-address
   :documentation "email address"
   :type 'string)
  (name
   (user-full-name) ;; from /etc/password
   :documentation "(given) name, used to populate `USER-FULL-NAME'"
   :type 'string)
  (keybinding
   "m"
   :documentation "keybinding in the hud-mail-map keymap"
   :type 'char)
  (fetchmail
   mu4e-get-mail-command
   :documentation "external command to run to fetch mail."
   :type 'string))

(defun tychoish-mail-select-account (account-id)
  "Use consult to select an account/mail configuration."

  (interactive
   (list (annotated-completing-read
	  (thread-last
	    tychoish-mail-accounts-table
	    (map-apply (lambda (key value)
			 (cons key (format
 				    "%s <%s>%s"
 				    (tychoish-mail-account-name value)
 				    (tychoish-mail-account-address value)
 				    (if (equal tychoish-mail-account-current key)
 					(concat " " (propertize "[current]" 'face 'bold))
 				      ""))) )))
	  :prompt "mail-account => "
	  :require-match nil)))

  (let ((select-account-operation (intern account-id)))
    (funcall select-account-operation)))

(defun tychoish-mail-auto-switch-for-reply ()
  "Auto-switch to the account the parent message was addressed to."
  (when-let* ((msg mu4e-compose-parent-message)
              (recipients (append (mu4e-message-field msg :to)
                                  (mu4e-message-field msg :cc)))
              (addrs (seq-filter #'identity
                                 (seq-map (lambda (a)
                                            (when-let* ((e (mu4e-contact-email a)))
                                              (downcase e)))
                                          recipients)))
              (account-name (map-some
                             (lambda (name conf)
                               (when (member (downcase (tychoish-mail-account-address conf))
                                             addrs)
                                 name))
                             tychoish-mail-accounts-table)))
    (tychoish-mail-select-account account-name)))

(defun tychoish-mail--activate-account (account-name)
  "Configure mu4e/message state to send and receive as ACCOUNT-NAME."
  (let* ((conf (map-elt tychoish-mail-accounts-table account-name))
         (account-id (tychoish-mail-account-id conf))
         (maildir (tychoish-mail-account-maildir conf)))

    (setq tychoish-mail-account-current account-name)
    (setq message-directory maildir)
    (setq smtpmail-queue-dir (file-name-concat maildir "queue" "cur"))
    (setq mu4e-mu-home (file-name-concat maildir ".mu"))
    (setq message-auto-save-directory (file-name-concat maildir "drafts"))
    (tychoish-mail--reset-header-separator)

    (let ((address (tychoish-mail-account-address conf))
          (given-name (tychoish-mail-account-name conf)))

      (setq message-signature-directory (file-name-concat maildir "tools" "signatures"))
      (setq message-signature-file (or address account-id account-name))
      (setq message-signature t)

      (setq user-mail-address address)
      (setq user-full-name given-name)
      (setq mu4e-compose-reply-to-address address)

      (setq mail-host-address (replace-regexp-in-string ".*@" "" address))
      (setq message-sendmail-extra-arguments (list "-a" address))

      (when (eq major-mode 'mu4e-compose-mode)
        (goto-char (point-min))
        (let ((new-from (format "From: %s <%s>" given-name address)))
          (while (re-search-forward "^From:.*$" nil t 1)
            (replace-match new-from))))

      (setq mu4e-get-mail-command (tychoish-mail-account-fetchmail conf))

      (when (featurep 'mu4e)
        (mu4e 'background))

      (message "mail: configured address [%s]" address))))

(cl-defun tychoish-define-mail-account
    (&key name address key id
	  (command mu4e-get-mail-command)
	  (maildir "~/mail")
	  (instances '())
	  (systems '())
	  default)
  "Register a mail account and bind KEY in `hud-mail-map' to activate it.
Returns the symbol of the generated activation command."
  (unless (and name address key id maildir)
    (user-error "cannot define mail account without name, address, key and id %S" (list :name name :address address :key key :id id :maildir maildir)))

  (let* ((account-name (format tychoish-mail-id-template id))
         (configure-account-symbol (intern account-name))
	 (maildir (expand-file-name maildir)))

    (keymap-set hud-mail-map key configure-account-symbol)

    (setf (map-elt tychoish-mail-accounts-table account-name)
          (tychoish-mail-make-account
           :name name
           :address address
           :keybinding key
           :maildir maildir
           :fetchmail command
           :id id))

    (defalias configure-account-symbol
      (lambda ()
        (interactive)
        (tychoish-mail--activate-account account-name))
      (format "Switch mu4e to the %s <%s> account." name address))

    (when (or default
	      (not (and (null systems) (null instances))))
      (let ((activate-form
             `(when (or ,default
                        (and
                         (or (member (sprite-instance-name) ',instances)
                             (null ',instances))
                         (or (member (system-name) ',systems)
                             (null ',systems))))
                (,configure-account-symbol))))
        (eval activate-form t)
        (when (daemonp)
          (add-one-shot-hook
           :name (format "%s-frame-setup" account-name)
           :form activate-form
           :hook 'after-first-frame-created))))

    configure-account-symbol))

(provide 'tychoish-mail)
