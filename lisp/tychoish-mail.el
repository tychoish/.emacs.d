;; -*- lexical-binding: t -*-

(require 's)
(require 'f)
(require 'ht)

(require 'tychoish-common)
(require 'consult-tycho)

(require 'mu4e)

(require 'marginalia)

(defconst tychoish/mail-id-template "tychoish-mail-%s")

(defvar tychoish/mail-accounts-table (ht-create #'equal))

(defvar tychoish/mail-account-current nil)

(bind-keys :prefix "C-c m"
           :prefix-map tychoish/mail-map
	   ("a" . tychoish-mail-select-account)
           ("m" . mu4e)
           ("d" . mu4e-search-maildir)
           ("b" . mu4e-search-bookmark)
           ("c" . mu4e-compose-new))

(use-package consult-mu
  :load-path "external/consult-mu/"
  :defer t
  :bind (:map tychoish/mail-map
	 ("C-;" . consult-mu)
	 (";" . consult-mu-bookmark))
  :config
  (defun consult-mu-bookmark ()
    "Select `consult-mu' initial query from mu4e-bookmarks."
    (interactive)
    (let* ((bookmarks (ht-create #'equal))
	   (_ (mapc (lambda (bookmark) (setf (ht-get bookmarks (plist-get bookmark :name)) bookmark)) mu4e-bookmarks))
	   (longest-id (length-of-longest-item (ht-keys bookmarks)))
	   (selection (consult--read
		       bookmarks
		       :prompt "mu4e query =>> "
		       :annotate (lambda (candidate)
				   (let* ((bookmark (ht-get bookmarks candidate))
					  (key (plist-get bookmark :key))
					  (query (plist-get bookmark :query)))

				     (marginalia--fields
				      (:left (char-to-string key)
				       :format (format "%s(b%%s)" (prefix-padding-for-annotation candidate longest-id))
				       :face 'marginalia-key)
				      (query
				       :format (format "query: \"%s\"" query)
				       :face 'marginalia-value)))))))
      (consult-mu (plist-get (ht-get bookmarks selection) :query))))

  (defun tychoish/consult-mu-headers-template ()
    (concat "%f" (number-to-string
		  (floor (* (frame-width) 0.15)))
	    "%s" (number-to-string (floor (* (frame-width) 0.5)))
	    "%d13" "%g" "%x"))

  (setq consult-mu-maxnum 200)
  (setq consult-mu-preview-key 'any)
  (setq consult-mu-mark-previewed-as-read nil)
  (setq consult-mu-mark-viewed-as-read t)
  (setq consult-mu-use-wide-reply t)
  (setq consult-mu-headers-template 'tychoish/consult-mu-headers-template)

  (setq consult-mu-saved-searches-dynamics '("#flag:unread"))
  (setq consult-mu-saved-searches-async '("#flag:unread"))

  (use-package consult-mu-compose
    :load-path "external/consult-mu/extras/"
    :after (consult-mu)
    :config
    (setq consult-mu-compose-use-dired-attachment 'in-dired)
    (setq consult-mu-compose-preview-key "M-o"))

  (use-package consult-mu-contacts
    :load-path "external/consult-mu/extras/"
    :after (consult-mu)
    :config
    (setq consult-mu-contacts-ignore-case-fold-search t)
    (setq consult-mu-contacts-ignore-list '("^.*no.*reply.*")))

  (use-package consult-mu-embark
    :load-path "external/consult-mu/extras/"
    :after (consult-mu)
    :config
    (setq consult-mu-embark-attach-file-key "C-a")
    (with-eval-after-load 'consult-mu-compose
      (declare-function consult-mu-compose-embark-bind-attach-file-key "consult-mu-compose-embark")
      (require 'consult-mu-compose-embark)
      (consult-mu-compose-embark-bind-attach-file-key))

    (with-eval-after-load 'consult-mu-contacts
      (require 'consult-mu-contacts-embark))))

(with-eval-after-load 'message
  (bind-key "M-q" 'ignore message-mode-map)
  (setq-default message-citation-line-format "On %A, %B %d %Y, %T, %N wrote:\n")
  (setq-default message-citation-line-function 'message-insert-formatted-citation-line)
  (setq-default message-interactive t)
  (setq-default message-kill-buffer-on-exit nil)
  (setq-default message-send-mail-function 'message-send-mail-with-sendmail)
  (setq-default message-forward-as-mime nil)
  (setq-default message-fill-column 80)
  (setq-default message-cite-style message-cite-style-gmail)
  (setq-default message-dont-reply-to-names t)
  (setq-default message-signature t)
  (add-to-list 'mm-discouraged-alternatives "text/richtext")
  (add-to-list 'mm-discouraged-alternatives "text/html")
  (set-face-attribute 'message-separator nil :background (face-attribute 'default :background nil)))

(setq compose-mail-user-agent-warnings nil)
(setq sendmail-program "msmtp")
(setq smtpmail-queue-mail nil)

(setq mml-secure-openpgp-sign-with-sender t)
(setq mc-gpg-user-id (getenv "GPG_KEY_ID"))

(setq mail-signature t)
(setq mail-specify-envelope-from t)
(setq mail-user-agent 'mu4e-user-agent)
(setq mail-header-separator (propertize "--------------------------" 'read-only t 'intangible t))
(setq mail-imenu-generic-expression
      '(("Subject"  "^Subject: *\\(.*\\)" 1)
        ("Cc"       "^C[Cc]: *\\(.*\\)" 1)
        ("Bcc"      "^B[Cc]: *\\(.*\\)" 1)
        ("To"       "^To: *\\(.*\\)" 1)
        ("From"     "^From: *\\(.*\\)" 1)))

(with-eval-after-load 'mu4e
  (bind-keys :map mu4e-compose-minor-mode-map
             ("R" . compose-reply-wide-or-not-please-ask)
             ("r" . mu4e-headers-mark-for-read)
             :map mu4e-headers-mode-map
             ("C-r" . compose-reply-wide-or-not-please-ask)
             ("R" . compose-reply-wide-or-not-please-ask)
             ("r" . mu4e-headers-mark-for-read)
             ("o" . mu4e-headers-mark-for-unread)
             ("u" . mu4e-headers-mark-for-unread)
             ("*" . mu4e-headers-mark-for-something)
             ("#" . mu4e-mark-resolve-deferred-marks)
             (";" . mu4e-mark-resolve-deferred-marks)))

(setq mu4e-compose-complete-addresses t)
(setq mu4e-compose-complete-only-after "2015-01-01")
(setq mu4e-compose-keep-self-cc nil)
(setq mu4e-drafts-folder "/drafts")
(setq mu4e-maildir-shortcuts nil)
(setq mu4e-search-include-related nil)
(setq mu4e-search-results-limit 1000)
(setq mu4e-sent-folder "/sent")
(setq mu4e-trash-folder "/trash")
(setq mu4e-user-agent-string nil)
(setq mu4e--header-separator mail-header-separator)

(add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)

(add-hook 'mu4e-compose-mode-hook 'turn-off-hard-wrap)
(add-hook 'mu4e-compose-mode-hook 'whitespace-cleanup)
(add-hook 'mu4e-compose-mode-hook 'tychoish/set-up-message-mode-buffer)

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

(declare-function cape-capf-prefix-length "cape")
(declare-function cape-dict "cape")
(declare-function cape-emoji "cape-char")
(declare-function yasnippet-capf "`yasnippet-capf'")

(defun tychoish/set-up-message-mode-buffer ()
  (setq-local completion-at-point-functions
	      (list (cape-capf-prefix-length #'mu4e-complete-contact 4)
		    'cape-emoji
		    'cape-dict
		    'yasnippet-capf))

  (setq-local use-hard-newlines t)
  (setq-local make-backup-files nil))

(defun compose-reply-wide-or-not-please-ask ()
  "Ask whether to reply-to-all or not."
  (interactive)
  (mu4e-compose-reply (yes-or-no-p "Reply to all?")))

;;;###autoload
(defun tychoish-mail-select-account (account-id)
  "Use consult to select an account/mail configuration."

  (interactive
   (list (let ((table (ht-create)))
	   (ht-map (lambda (key account)
		     (ht-set table key (format "%s <%s>%s"
					       (tychoish/mail-account-name account)
					       (tychoish/mail-account-address account)
					       (or (when (equal tychoish/mail-account-current key) " -- CURRENT") ""))))
		   tychoish/mail-accounts-table)
	   (consult-tycho--read-annotated
	    table
	    :prompt "mail-account => "
	    :require-match nil
	    :command 'tychoish-mail-select-account
	    :category 'consult-mu))))

  (let ((select-account-operation (intern account-id)))
    (funcall select-account-operation)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; account configuration

(cl-deftype signature-source ()
  '(signature-file
    signature-directory
    signature-text))

(cl-defstruct (tychoish/mail-account
               (:constructor tychoish/mail-make-account
                             (&key id maildir name address keybinding signature signature-kind fetchmail
                              &aux (maildir (cond
                                             ((null maildir) (f-expand "~/mail"))
                                             ((not (stringp maildir)) (user-error "maildir must be a string"))
                                             ((not (f-directory-p maildir)) (user-error "maildir does not exist"))
                                             (t (f-expand maildir))))
                                   (signature (setq signature (and (when (trimmed-string-or-nil signature)
                                                                    (cond ((eq signature-kind 'signature-directory)
                                                                           (f-join maildir ".sig"))
                                                                          ((eq signature-kind 'signature-file)
                                                                           (f-join maildir ".sig" address))
                                                                          ((eq signature-kind 'signature-text)
                                                                           (user-error "signature text is not defined"))
                                                                          ((null signature-kind)
                                                                           (user-error "signature configuration is not supported"))
                                                                          (t signature)))
                                                                   signature)
                                                    ;; now validate
                                                    signature (cond
                                                               ((eq signature-kind 'signature-directory)
                                                                (if (or (null signature) (not (f-directory-p signature)))
                                                                    (user-error "signature directory does not exist")
                                                                  signature))
                                                               ((eq signature-kind 'signature-file)
                                                                (if (not (f-file-p signature))
                                                                    (user-error "signature file does not exist")
                                                                  signature))
                                                               ((eq signature-kind 'signature-text)
                                                                (or (when (not (s-contains-p "\n" signature))
                                                                      (warn "signature string does not contain newlines")
                                                                      signature)
                                                                    signature))
                                                               (t signature))))
                                   (signature-kind (cond
                                                    ((eq (type-of signature) 'signature-source) signature)
                                                    ((not (eq (type-of signature) 'string)) (user-error "invalid type for signature"))
                                                    ((f-directory-p signature) 'signature-directory)
                                                    ((f-exists-p signature) 'signature-file)
                                                    (t 'signature-text))))))

  "track mail account configurations. used internally by tychoish-define-mail-account"
  (maildir (expand-file-name "~/mail")
   :documentation "path for maildirs"
   :type 'string)
  (id nil
   :documentation "symbol of function that activates this account"
   :type 'symbol)
  (address user-mail-address
   :documentation "email address"
   :type '(string t))
  (name (user-full-name) ;; from /etc/password
   :documentation "(given) name, used to populate `USER-FULL-NAME'"
   :type 'string)
  (keybinding "m"
   :documentation "keybinding in the tychoish/mail-map keymap"
   :type 'char)
  (fetchmail mu4e-get-mail-command
   :documentation "external command to run to fetch mail."
   :type 'string)
  (signature ""
   :documentation "content or filename of signature"
   :type 'string)
  (signature-kind 'signature-directory
   :documentation "determines how signatures are configured"
   :type 'signature-source))

;;;###autoload
(cl-defmacro tychoish-define-mail-account
    (&key name address key id
	  (command mu4e-get-mail-command)
	  (maildir (expand-file-name "~/mail"))
	  (instances '())
	  (systems '())
	  default)

  (unless (and name address key id)
    (user-error "cannot define mail account without name, address, key and id %S" (list :name name :address address :key key :id id)))

  (let* ((account-name (format tychoish/mail-id-template id))
         (configure-account-symbol (intern account-name)))

    (define-key 'tychoish/mail-map (kbd key) configure-account-symbol)

    (ht-set tychoish/mail-accounts-table account-name
            (tychoish/mail-make-account
             :name name
             :address address
             :keybinding key
             :maildir maildir
             :fetchmail command
             :id id
             :signature-kind 'signature-directory
             :signature (f-join maildir "tools" "signatures")))

    (dolist (instance instances)
      (when (and (stringp instance) (string-equal instance tychoish/emacs-instance-id))
        (add-hook 'emacs-startup-hook configure-account-symbol)))

    (dolist (sysn systems)
      (when (and (stringp sysn) (string-equal sysn (system-name)))
        (add-hook 'emacs-startup-hook configure-account-symbol)))

    `(progn
       (defun ,configure-account-symbol ()
	 (interactive)

	 (let* ((account-id ,id)
		(account-name ,account-name)
		;; nothing beyond this point should access compilation env ->
		(conf (ht-get tychoish/mail-accounts-table account-name))
		(maildir (tychoish/mail-account-maildir conf)))

	   (setq tychoish/mail-account-current account-name)
	   (setq message-directory maildir)
	   (setq smtpmail-queue-dir (f-join maildir "queue" "cur"))
	   (setq mu4e-mu-home (f-join maildir ".mu"))
	   (setq message-auto-save-directory (f-join maildir "drafts"))

	   (let ((signature-kind (tychoish/mail-account-signature-kind conf))
		 (signature (tychoish/mail-account-signature conf))
		 (address (tychoish/mail-account-address conf))
		 (given-name (tychoish/mail-account-name conf)))

             (cond
              ((eq signature-kind 'signature-directory)
               (setq message-signature-directory (or signature (f-join maildir "tools" "signatures")))
               (setq message-signature-file (or address account-id account-name))
               (setq message-signature t))
              ((eq (tychoish/mail-account-signature-kind conf) 'signature-file)
               (setq message-signature-directory nil)
               (setq message-signature-file signature)
               (setq message-signature t))
              ((eq (tychoish/mail-account-signature-kind conf) 'signature-text)
               (setq message-signature-directory nil)
               (setq message-signature-file nil)
               (setq message-signature signature)))

             (setq user-mail-address address)
             (setq message-signature-file address)
             (setq user-full-name given-name)
             (setq mu4e-compose-reply-to-address address)

             (setq mail-host-address (s-replace-regexp ".*@" "" address))
             (setq message-sendmail-extra-arguments (list "-a" address))

             (when (eq major-mode 'mu4e-compose-mode)
               (goto-char (point-min))
               (let ((new-from (format "From: %s <%s>" given-name address)))
		 (while (re-search-forward "^From:.*$" nil t 1)
		   (replace-match new-from))))

             (setq mu4e-get-mail-command (tychoish/mail-account-fetchmail conf))

             (message (format "mail: configured address [%s]" address)))))

       ,(when default
	  (add-hook 'emacs-startup-hook configure-account-symbol)))))

(provide 'tychoish-mail)
