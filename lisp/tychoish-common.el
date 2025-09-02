;; -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; id-state -- emacs daemon/instance identification for state config

(defun tychoish-system-name ()
  (interactive)
  (message (s-join " " (list "system:" (system-name)))))

(defun tychoish/resolve-instance-id ()
  (let ((daemon (daemonp)))
    (or (when (eq daemon t) "primary")
        daemon
        cli/instance-id
        tychoish/emacs-instance-id
        "solo")))

(defun tychoish/conf-emacs-host-and-instance ()
  (list
   (if (eq system-type 'darwin)
       (car (s-split "\\." (system-name)))
     (system-name))
   (or tychoish/emacs-instance-id
       (tychoish/resolve-instance-id))))

(defconst tychoish/conf-state-directory-name "state")

(defun tychoish/conf-state-path (name)
  (f-join (expand-file-name user-emacs-directory)
	  tychoish/conf-state-directory-name
	  (tychoish-get-config-file-prefix name)))

(defun tychoish-get-config-file-prefix (name)
  "Build a config file basename, for NAME.
This combines the host name and the dameon name."
  (s-join "-" (append (tychoish/conf-emacs-host-and-instance) `(,name))))

(defun tychoish-get-config-file-path (name)
  "Return an absolute path for NAME in the configuration directory.
The is unique to the system and daemon instance."
  (f-expand (f-join user-emacs-directory (tychoish-get-config-file-prefix name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ssh-agent -- tools to make sure emacs session can connect to ssh-agent

(defun find-ssh-agent-socket-candidates ()
  (->> (-value-to-list (format "/run/user/%d/ssh-agent.socket" (user-uid)))
       (-concat (-sort #'s-less? (f-glob (f-join temporary-file-directory "ssh-*/agent.*" ))))
       (-distinct)
       (-non-nil)
       (-filter #'f-writable?)
       (nreverse)))

(defun tychoish/set-up-ssh-agent ()
  (let (env-value sockets)
    (unless (setq env-value (getenv "SSH_AUTH_SOCK"))
      (setq sockets (find-ssh-agent-socket-candidates))
      (when (and sockets
		 (<= 1 (length sockets)))
	(setq env-value (setenv "SSH_AUTH_SOCK" (car sockets)))))
    env-value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; display -- manage fonts, rendering, themes, for (mostly) gui emacs

(defun gui-p ()
  "Return t when the current session is or may be a GUI session."
  (when (or (daemonp) (window-system))
    t))

(defun contextual-menubar (&optional frame)
  "Display the menubar in FRAME (default: selected frame) if on a graphical display, but hide it if in terminal."
  (interactive)
  (set-frame-parameter frame 'menu-bar-lines (if (display-graphic-p frame) 1 0)))

(defun text-scale-reset ()
  (interactive)
  (text-scale-set 0))

(defun djcb-opacity-modify (&optional dec)
  "Modify the transparency of the frame.
If DEC is t, decrease the transparency, otherwise increase it in 10%-steps"
  (let* ((alpha-or-nil (frame-parameter nil 'alpha)) ; nil before setting
         (oldalpha (if alpha-or-nil alpha-or-nil 100))
         (newalpha (if dec (- oldalpha 2) (+ oldalpha 2))))
    (when (and (>= newalpha frame-alpha-lower-limit) (<= newalpha 100))
      (modify-frame-parameters nil (list (cons 'alpha newalpha))))))

(defun opacity-increase ()
  (interactive)
  (djcb-opacity-modify))

(defun opacity-decrease ()
  (interactive)
  (djcb-opacity-modify t))

(defun opacity-reset ()
  (interactive)
  (modify-frame-parameters nil `((alpha . 95))))

(defun disable-all-themes ()
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(defun tychoish-load-light-theme ()
  (interactive)
  (disable-all-themes)
  (when (load-theme 'modus-operandi t t)
    (enable-theme 'modus-operandi))
  (add-to-list 'default-frame-alist '(alpha . 97)))

(defun tychoish/ensure-light-theme ()
  (unless custom-enabled-themes
    (tychoish-load-light-theme)))

(defun tychoish/ensure-dark-theme ()
  (unless custom-enabled-themes
    (tychoish-load-dark-theme)))

(defun tychoish-load-dark-theme ()
  (interactive)
  (disable-all-themes)
  (when (load-theme 'modus-vivendi t t)
    (enable-theme 'modus-vivendi))
  (add-to-list 'default-frame-alist '(alpha . 95)))

(defun tychoish-setup-font (font-face-name size)
  (interactive "sName: \nNSize: ")
  (let ((new-font-name (concat font-face-name "-" (number-to-string size)))
	(font-cell (assoc 'font default-frame-alist)))
    (if font-cell
	(setcdr font-cell new-font-name)
      (add-to-list 'default-frame-alist (cons 'font new-font-name)))))

(defun tychoish/ensure-font (font-face-name size)
  (unless (assoc 'font default-frame-alist)
    (tychoish-setup-font font-face-name size)))

(defun tychoish/ensure-default-font ()
  (tychoish/ensure-font "Source Code Pro" 13))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; strings -- helper functions for handling strings

(defun trimmed-string-or-nil (value)
  (and (stringp value)
       (unless (string-empty-p (setq value (string-trim value))) value)
       value))

(defun string-with-non-whitespace-content-p (value)
  "Return t when `VALUE' is a string with non-whitespace content and nil otherwise."
  (and (stringp value)
       (not (string-empty-p (string-trim value)))))

(defun default-string (default input)
  "Return the DEFAULT value if the INPUT is empty or nil."
  (cond
   ((string-equal default input)
    default)
   ((eq input nil)
    default)
   ((string-equal input "")
    default)
   (t
    input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; lists -- helpers, mostly a-la dash.el

(defun -distinct-by-car (cell)
  (let ((-compare-fn (lambda (a b) (equal (car a) (car b)))))
    (-distinct cell)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; macros -- helper macros for common operations

(defmacro tychoish/set-tab-width (num)
  (unless (integerp num)
    (signal 'wrong-type-argument num))
  (unless (< num 32)
    (warn "INVALID cannot create tab width hook function to >= 32 (%s)" num))

  (let ((generated-name (intern (format "tychoish/set-local-tab-width-%d" num))))
    `(defun ,generated-name ()
       (set-tab-width ,num))))

(defmacro create-run-hooks-function-for (mode)
  (let* ((mode-name (symbol-name mode))
	 (hook-name (concat mode-name "-hook"))
	 (function-name (intern (concat "run-hooks-for-" mode-name))))
    `(defun ,function-name nil
       (run-hooks (intern ,hook-name)))))

(defmacro create-local-toggle-functions (value)
  (let* ((name (symbol-name value))
	 (ops (list
	       `(,(intern (concat "turn-on-" name "-local")) . t)
	       `(,(intern (concat "turn-off-" name "-local")) . nil)
	      `(,(intern (concat "toggle-" name "-local")) . (not ,value)))))

  `(progn
     ,@(mapcar (lambda (def)
                 `(defun ,(car def) ()
		    (interactive)
		    (setq-local ,value ,(cdr def))))
	       ops))))

(defmacro create-toggle-functions (value)
  (let* ((name (symbol-name value))
	 (ops (list
	       `(,(intern (concat "turn-on-" name)) . t)
	       `(,(intern (concat "turn-off-" name)) . nil)
	      `(,(intern (concat "toggle-" name )) . (not ,value)))))

  `(progn
     ,@(mapcar (lambda (def)
                 `(defun ,(car def) ()
		    (interactive)
		    (setq ,value ,(cdr def))))
	       ops))))

(defmacro with-silence (&rest body)
  "Report on NAME and the time taken to execute BODY."
  `(let ((inhibit-message t)
         (message-log-max nil))
     (null ,@body)))

(defmacro without-messages (&rest body)
  "Report on NAME and the time taken to execute BODY."
  `(let ((inhibit-message t))
     (null ,@body)))

(defmacro with-temp-keymap (map &rest body)
  "Create a temporary MAP and return it after evaluating it in the BODY."
  `(let ((,map (make-sparse-keymap)))
     ,@body
     map))

(defmacro -add-if-empty (op list-val)
  `(if ,list-val
       ,list-val
     (list ,op)))

(cl-defmacro add-hygenic-one-shot-hook (&key name hook function (args nil) (local nil))
  (let ((cleanup (intern (format "hygenic-one-shot-%s-%s" name (gensym)))))
    `(progn
       (add-hook ',hook ',cleanup nil ,local)
       (defun ,cleanup ,args
	 (if ,args
	     (apply #',function ,args)
	   (apply-partially #',function ,args))
         (remove-hook ',hook ',cleanup)
         (unintern ',cleanup))
       #',cleanup)))

(defmacro set-to-current-time-on-startup (variable)
  (let ((operation (intern (format "set-%s-to-current-time" (symbol-name variable)))))
    `(progn
       (add-hook 'emacs-startup-hook ',operation)
       (defun ,operation ()
	 (setq ,variable (current-time))))))

(defmacro with-timer (name &rest body)
  "Report on NAME and the time taken to execute BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%s: %.06fs" ,name (float-time (time-since time)))))

(defmacro with-slow-op-timer (name threshold &rest body)
  "Send a message the BODY operation of NAME takes longer to execute than the THRESHOLD."
  `(let* ((time (current-time))
	  (return-value ,@body)
	  (duration (time-to-seconds (time-since time))))
     (when (> duration  ,threshold)
       (message "%s: %.06fs" ,name duration))))

(defmacro f-has-ext-p-fn (ext)
  `(lambda (filename) (f-ext-p filename ext)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; macros -- configuration and setup

(cl-defmacro tychoish/gptel-set-up-backend (&key name model backend key)
  (let ((local-function-symbol (intern (format "tychoish/gptel-set-backend-%s" name)))
	(default-function-symbol (intern (format "tychoish/gptel-set-default-backend-%s" name))))
    `(progn
       (defun ,local-function-symbol ()
	 (interactive)
	 (setq-local gptel-model ,model)
	 (setq-local gptel-backend ,backend))

       (defun ,default-function-symbol ()
	 (interactive)
	 (setq-default gptel-model ,model)
	 (setq-default gptel-backend ,backend))

       (bind-keys :map gptel-mode-map
		  (,(format "C-c r a m %s" (downcase key)) . ,local-function-symbol)
		  (,(format "C-c r a m %s" (upcase key)) . ,default-function-symbol)))))

(defvar mu4e-get-mail-command "true")

(cl-defmacro tychoish/define-mail-account
    (&key name address key id (command mu4e-get-mail-command) (maildir (expand-file-name "~/mail")) (instances '()) (systems '()))

  (let ((symbol (intern (format "tychoish-mail-%s" id))))
    `(progn
       (define-key 'tychoish/mail-map (kbd ,key) ',symbol)
       (dolist (instance ,instances)
         (when (string-equal instance tychoish/emacs-instance-id)
           (add-hook 'emacs-startup-hook ',symbol)))

       (dolist (sysn ,systems)
         (when (string-equal sysn ,system-name)
           (add-hook 'emacs-startup-hook ',symbol)))

       (defun ,symbol ()
	 (interactive)
         (setq smtpmail-queue-dir ,(f-join maildir "queue" "cur"))
         (setq mu4e-mu-home ,(f-join maildir ".mu"))
         (setq message-directory ,maildir)
         (setq message-auto-save-directory ,(f-join maildir "drafts"))
         (setq message-signature-directory ,(f-join maildir "tools" "signatures"))

         (setq user-mail-address ,address)
         (setq message-signature-file ,address)
         (setq user-full-name ,name)
         (setq mu4e-compose-reply-to-address ,address)
         (setq mu4e-reply-to-address ,address)

         (setq mail-host-address ,(s-replace-regexp ".*@" "" address))
         (setq message-sendmail-extra-arguments '("-a" ,address))

         (when (eq major-mode 'mu4e-compose-mode)
           (goto-char (point-min))
           (let ((new-from ,(concat "From: " name " <" address ">")))
             (while (re-search-forward "^From:.*$" nil t 1)
               (replace-match new-from nil nil))))

         (setq mu4e-get-mail-command ,command)

         (message ,(concat "mail: configured address [" address "]"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; lines -- whitespace,  filling/wrapping, line manipulation

;; word-wrapping  --

(defalias 'turn-on-hard-wrap 'turn-off-soft-wrap)
(defalias 'turn-off-hard-wrap 'turn-on-soft-wrap)
(defalias 'toggle-soft-wrap 'toggle-on-soft-wrap)
(defalias 'toggle-hard-wrap 'toggle-off-soft-wrap)

(defun turn-on-soft-wrap ()
  (interactive)
  (let ((was-hard-wrapping auto-fill-function))
    (auto-fill-mode -1)
    (visual-fill-column-mode 1)
    (visual-line-mode 1)
    (when was-hard-wrapping
      (tychoish-show-wrapping-mode))))

(defun turn-off-soft-wrap ()
  (interactive)
  (let ((was-soft-wrapping (not auto-fill-function)))
    (visual-fill-column-mode -1)
    (visual-line-mode -1)
    (auto-fill-mode 1)
    (when was-soft-wrapping
      (tychoish-show-wrapping-mode))))

(defun toggle-word-wrap ()
  (interactive)
  (if auto-fill-function
      (turn-on-soft-wrap)
    (turn-on-hard-wrap)))

(defun tychoish-show-wrapping-mode ()
  (let ((buf (current-buffer))
	(wrapping-mode (if auto-fill-function
                           "hard"
                         "soft")))
  (message "wrapping mode `%s' for %s <%s>"
	   wrapping-mode
	   (buffer-local-value 'major-mode buf)
	   (buffer-name buf))))

(advice-add 'set-fill-column :around #'ad:set-fill-column-locally)

(defun ad:set-fill-column-locally (f &rest arg)
  (let ((had-default (default-boundp 'fill-column))
	(previous-default (default-value 'fill-column))
	(new-value (apply f arg)))
    (when had-default
      (setq-default fill-column previous-default))
    (setq-local fill-column new-value)))

(defun unfill-region (begin end)
  "Remove all linebreaks in a region but leave paragraphs
  indented text (quotes,code) and lines starting with an asterix (lists) intakt."
  (interactive "r")
  (replace-regexp "\\([^\n]\\)\n\\([^ *\n]\\)" "\\1 \\2" nil begin end))

;; whitespace  --

(defun set-tab-width (num-spaces)
  (interactive "nTab width: ")
  (setq-local tab-width num-spaces))

(defun font-lock-show-tabs ()
  "Return a font-lock style keyword for tab characters."
  '(("\t" 0 'trailing-whitespace prepend)))

(defun toggle-local-whitespace-cleanup ()
  "Reset the before-save hook to preven cleaning up."
  (interactive)
  (if (setq-local show-trailing-whitespace (not show-trailing-whitespace))
      (progn
	(add-hook 'before-save-hook 'whitespace-cleanup nil t)
	(message "turned on whitespace-cleanup for '%s'" (buffer-file-name (current-buffer))))
    (remove-hook 'before-save-hook 'whitespace-cleanup)
    (message "turned off whitespace-cleanup for '%s'" (buffer-file-name (current-buffer)))))

(defun font-lock-width-keyword (width)
  "Return a font-lock style keyword for strings beyond WIDTH that use `font-lock-warning-face'."
  `((,(format "^%s\\(.+\\)" (make-string width ?.))
     (1 font-lock-warning-face t))))

(defun uniquify-region-lines (beg end)
  "Remove duplicate adjacent lines between BEG and END."
  (interactive "*r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
      (replace-match "\\1"))))

(defun uniquify-buffer-lines ()
  "Remove duplicate adjacent lines in the current buffer."
  (interactive)
  (uniquify-region-lines (point-min) (point-max)))

;; move-text  -- use arrow keys to move whole

(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; clean kill ring -- "deduplicate kill-ring"

(defvar clean-kill-ring-filters '(string-blank-p))
(defvar clean-kill-ring-prevent-duplicates t)

(defun clean-kill-ring-filter-catch-p (string)
  "T if STRING satisfies at least one of `clean-kill-ring-filters'."
  (let ((caught nil)
        (s (substring-no-properties string)))
    (catch 'loop
      (dolist (filter clean-kill-ring-filters)
        (when (funcall filter s)
          (setq caught t)
          (throw 'loop t))))
    caught))

(defun clean-kill-ring-clean (&optional remove-dups)
  "Remove `kill-ring' members that satisfy one of`clean-kill-ring-filters'.

If REMOVE-DUPS or `clean-kill-ring-prevent-duplicates' is non-nil, or if called
interactively then remove duplicate items from the `kill-ring'."
  ;; from: https://github.com/NicholasBHubbard/clean-kill-ring.el/blob/main/clean-kill-ring.el
  (interactive (list t))
  (let ((new-kill-ring nil)
        (this-kill-ring-member nil)
        (i (1- (length kill-ring))))
    (while (>= i 0)
      (setq this-kill-ring-member (nth i kill-ring))
      (unless (clean-kill-ring-filter-catch-p this-kill-ring-member)
        (push this-kill-ring-member new-kill-ring))
      (setq i (1- i)))
    (if (or remove-dups clean-kill-ring-prevent-duplicates)
        (setq kill-ring (delete-dups new-kill-ring))
      (setq kill-ring new-kill-ring))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
        (line-beginning-position 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; bulk buffer killing -- kill groups of buffers efficeiently

(defalias 'kill-buffers-matching-name 'kill-matching-buffers)

(defun force-kill-buffers-matching-path (regexp)
  (interactive "sKill buffers visiting a path matching this regular expression: \n")
  (kill-buffers-matching-path regexp t t))

(defun kill-buffers-matching-path (regexp &optional internal-too no-ask)
  "Kill buffers whose name matches the specified REGEXP.
Ignores buffers whose name starts with a space, unless optional
prefix argument INTERNAL-TOO is non-nil.  Asks before killing
each buffer, unless NO-ASK is non-nil."
  (interactive "sKill buffers visiting a path matching this regular expression: \n")
  (let ((count 0))
    (dolist (buffer (buffer-list))
      (let ((name (buffer-file-name buffer)))
	(when (and name (not (string-equal name ""))
                   (or internal-too (/= (aref name 0) ?\s))
                   (string-match regexp name))
          (when (funcall (if no-ask 'kill-buffer 'kill-buffer-ask) buffer)
	    (setq count (+ 1 count))))))
    (message "killed %d buffers matching '%S'" count regexp)))

(defun kill-buffers-matching-mode (mode)
  "Kill all buffers matching the symbol defined by MODE.
Returns the number of buffers killed."
  (interactive (list (intern
    (completing-read
     "mode: " ;; prompt
     obarray  ;; collection
     (lambda (symbol) (s-ends-with? "-mode" (symbol-name symbol)))
     t nil nil major-mode))))
 (message "killing all buffers with mode \"%s\"" mode)
 (length (->> (buffer-list)
	      (--select (with-current-buffer it (eq major-mode mode)))
	      (mapc #'kill-buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; buffer/frame management -- helper interactive functions.

(defun pin-buffer-to-window-toggle ()
  "pin buffer to window, most useful in keeping chat buffers under control"
  (interactive)
  (let* ((buf (current-buffer))
	 (window (selected-window))
	 (current-state (window-dedicated-p window))
	 (buf-name (buffer-name buf)))

    (set-window-dedicated-p window (not current-state))

    (if current-state
	(message "pinned %s to window" buf-name)
      (message "unpinned %s from window" buf-name))))

(provide 'tychoish-common)
