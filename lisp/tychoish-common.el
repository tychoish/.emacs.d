;; -*- lexical-binding: t; -*-

(require 'f)
(require 's)
(require 'ht)
(require 'dash)

(require 'visual-fill-column)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; keybindings

(bind-keys ("M-<up>" . move-text-up)
           ("M-<down>" . move-text-down)
	   :prefix "C-c f"
	   :prefix-map tychoish/display-map
	   ("=" . text-scale-increase)
           ("-" . text-scale-decrease)
           ("0" . text-scale-reset)
	   :map tychoish/display-map ;; "C-c f"
	   :prefix "o"
	   :prefix-map tychoish/display-opacity-map
           ("=" . opacity-increase)
           ("-" . opacity-decrease)
           ("0" . opacity-reset))

(bind-keys :prefix "C-c t"
	   :prefix-map tychoish-core-map
           ("w" . toggle-local-whitespace-cleanup)
	   :map tychoish-core-map ;; "C-c t"
	   :prefix "t"
	   :prefix-map tychoish/theme-map
           ("r" . disable-all-themes) ;; reset
	   ("d" . tychoish-load-dark-theme)
	   ("l" . tychoish-load-light-theme))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; id-state -- emacs daemon/instance identification for state config

(defun tychoish-system-name ()
  (interactive)
  (message "system: %s" (system-name)))

(defconst tychoish-cache--buffer-name " tychoish-cache-buffer")

(defvar-local tychoish-cache--resolved-instance-id nil)
(defun tychoish/resolve-instance-id ()
  (with-current-buffer (get-buffer-create tychoish-cache--buffer-name)
    (or tychoish-cache--resolved-instance-id
	(setq tychoish-cache/resolved-instance-id
	      (let ((daemon (daemonp)))
		(or (when (eq daemon t) "primary")
		    daemon
		    cli/instance-id
		    tychoish/emacs-instance-id
		    "solo"))))))

(defun tychoish/set-up-instance-name ()
  (unless tychoish/emacs-instance-id
    (setq tychoish/emacs-instance-id (tychoish/resolve-instance-id))))

(defvar-local tychoish-cache--conf-emacs-host-and-instance nil)
(defun tychoish/conf-emacs-host-and-instance ()
  (with-current-buffer (get-buffer-create tychoish-cache--buffer-name)
    (or tychoish-cache--conf-emacs-host-and-instance
	(setq tychoish-cache--conf-emacs-host-and-instance
	      (list
	       (if (eq system-type 'darwin)
		   (car (s-split "\\." (system-name)))
		 (system-name))
	       (or tychoish/emacs-instance-id
		   (tychoish/resolve-instance-id)))))))

(defconst tychoish/conf-state-directory-name "state")

(defun tychoish/conf-state-path (name)
  (f-join user-emacs-directory
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

(defun symbol-join (&rest parts)
  (->> parts
       (-filter #'stringp)
       (s-join "-")))

(defun trimmed-string-or-nil (value)
  (and (stringp value)
       (unless (string-empty-p (setq value (string-trim value))) value)
       value))

(defun string-trim-non-word-chars (value)
  (and (stringp value)
       (unless (string-empty-p (setq value (string-trim value "\\W+" "\\W+"))) value)
       value))

(defun string-with-non-whitespace-content-p (value)
  "Return t when `VALUE' is a string with non-whitespace content and nil otherwise."
  (and (stringp value)
       (not (string-empty-p (string-trim value)))))

(defun prefix-padding-for-annotation (key longest)
  (make-string (abs (+ 4 (- longest (length key)))) ? ))

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

(defun larger (&optional first second)
  "Return the larger item or 0, if neither FIRST and SECOND are non-negative numbers."
  (if (> (setq first (if (numberp first) first 0))
	 (setq second (if (numberp second) second 0)))
      first
    second))

(defun smaller (&optional first second)
  (if (< (setq first (if (numberp first) first 0))
	 (setq second (if (numberp second) second 0)))
      first
    second))

(defun length-of-longest-item (items)
  "Return the length of the longest item in the list ITEMS."
  (->> items
       (-map #'length)
       (-reduce #'larger)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `dash.el' -- extensions and additons

(defun -distinct-by-car (cell)
  (let ((-compare-fn (lambda (a b) (equal (car a) (car b)))))
    (-distinct cell)))

(defun -distinct-by-alist-key (key cell)
  (let ((-compare-fn (lambda (a b) (equal (alist-get key a) (alist-get key b)))))
    (-distinct cell)))

(defun -unwind (list)
  "Flattens a list of lists into a list by collecting items in one list"
  (-flatten-n 1 list))

(defalias '-flat-map #'mapcan)
(defalias '-mapc #'mapc)
(defalias '-join #'nconc)
(defalias '-append #'append)

(defalias '-l #'list)
(defalias 'll #'list)
(defalias '-- #'list)

(defmacro --flat-map (form input-list)
  (declare (debug (def-form form)))
  `(mapcan (lambda (it) (ignore it) ,form) ,input-list))

(defmacro --mapc (form input-list)
  "Apply the form (with the current element avalible as the variable `it')
to all item in the list, primarily for side effects. Returns the input
list. This is an anaphoric equivalent to `mapc'. As opposed to `--each'
and `-each', which return nil, `-mapc' returns the input list."
  (declare (debug (def-form form)))
  `(mapc (lambda (it) (ignore it) ,form) ,input-list))

(defmacro --map-in-place (form items)
  "Apply the form, (with the current element as `it') to each item in the
list, distructively setting the return value of the form to the value in
the list."
  `(let* ((lst ,items)
	  (head lst))
     (while head
       (setf (car head) (funcall #'(lambda (it) (ignore it) ,form) (car head)))
       (setq head (cdr head)))
     lst))

(defmacro --in-place (form items)
  `(let ((head ,items)
	 (count 0))
     (while head
       (setf (car head) (funcall #'(lambda (it) (ignore it) ,form) (car head)))
       (setq head (cdr head))
       (setq count (+ 1 count)))
     count))

(defun -map-in-place (mapping-op items)
  (--map-in-place (funcall mapping-op it) items))

(defun -in-place (mapping-op items)
  (--in-place (funcall mapping-op it) items))

(defun -flatten-some (input)
  (unless (proper-list-p input)
    (user-error "can only flatten proper lists (has nil terminating final cdr)"))

  (let ((head input) flattened)
    (while head
      (if (listp (car head))
	  (setq flattened (nconc (car head) flattened))
	(setq flattened (cons (car head) flattened)))
      (setq head (cdr head)))

    (nreverse flattened)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `ht.el' -- extensions and aditions

(defmacro ht-get-function (table)
  `(lambda (key) (ht-get ,table key)))

(defmacro ht-set-function (table)
  `(lambda (key value) (ht-set ,table key value)))

(defmacro ht-contains-p-function (table)
  `(lambda (key) (ht-contains-p ,table key)))

(defmacro ht-make-get-function (table)
  (let ((name (symbol-name table)))
    `(defun ,(format "ht-%s-get" name) (key) (ht-get ,table key))))

(defmacro ht-make-set-function (table)
  (let ((name (symbol-name table)))
    `(defun ,(format "ht-%s-set" name) (key value) (ht-set ,table key value))))

(defmacro ht-make-contains-p-function (table)
  (let ((name (symbol-name table)))
    `(defun ,(format "ht-%s-contains-p" name) (key) (ht-contains-p ,table key))))

(cl-defmacro ht-named-table (name &optional &key (test #'equal))
  (let ((name (or (when (stringp name) (intern name))
		  (when (symbolp name) name)
		  (intern (format "%S" name))))
	(table (or (when (ht-p name) name)
			 (ht-create test))))
    `(progn
       (defvar ,name ,table
	 ,(format "Hash table `%s' with named accessor functions" name))
       (ht-make-get-function ,name)
       (ht-make-set-function ,name)
       (ht-make-contains-p-function ,name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `f.el' -- extensions and additions

(defun f-mtime (filename)
  (file-attribute-modification-time (file-attributes filename)))

(defmacro f-file-has-extension-function (extension)
  `(lambda (filename) (f-ext-p filename ,extension)))

(defmacro f-filename-is-function (name)
  `(lambda (filename) (f-filename-is-p filename ,name)))

(defun f-filename-is-p (entry name)
  (f-equal-p (f-filename entry) name))

(defmacro f-directories-containing-file-with-extension-function (extension)
  (when (s-prefix-p "." extension)
    (setq extension (string-trim-left extension "^\\.")))

  `(defun ,(intern (format "f-directories-containing-file-with-extension-%s" (string-replace "." "" (downcase extension)))) (paths)
     (when (stringp paths)
       (setq paths (list paths)))
     (->> paths
	  (--flat-map (f-entries it #'f-file-p))
	  (--filter (f-ext-p it ,extension))
	  (-map #'f-dirname)
	  (-distinct))))

(defun f-files-in-directory (path)
  (cond
   ((stringp path)
    (cond
     ((f-directory-p path) (f-entries path #'f-file-p))
     ((f-file-p path) (f-entries (f-dirname path) #'f-file-p))))
   ((listp path) (--flat-map (f-entries it #'f-file-p) path))))

(defmacro f-directories-containing-file-function (filename &rest files)
  (let ((filenames (cons filename files)))
  `(defun ,(intern (symbol-join "f-directories-containing-file" (string-replace "." "-" (downcase filename)))) (path &rest paths)
     (let ((filenames (list ,@filenames))
	   (paths (cons path paths)))
       (->> paths
	    (-flat-map #'f-files-in-directory)
	    (--filter (car (member (f-filename it) filenames)))
	    (-map #'f-dirname)
	    (-distinct))))))

(defun f-collapse-homedir (path)
  (string-replace (expand-file-name "~/") "~/" path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; macros -- helper macros for common operations

(defmacro with-slow-op-timer (name &rest body)
  "Send a message the BODY operation of NAME takes longer to execute than a hardcoded threshold."
  `(let* ((inhibit-message t)
	  (time (current-time))
	  (return-value (progn ,@body))
	  (duration (time-to-seconds (time-since time))))
     (when (and (or debug-on-error init-file-debug) (> duration tychoish/slow-op-time-threshold))
       (message "[slow-op]: %s: %.06fs" ,name duration))
     return-value))

(defmacro with-gc-suppressed (&rest body)
  `(progn
     (let ((gc-cons-threshold 800000000000000))
       ,@body)
     (let ((garbage-collection-messages t))
       (garbage-collect))))

(defmacro with-file-name-handler-disabled (&rest body)
  `(let ((file-name-handler-alist nil))
     ,@body))

(defmacro disabled (&rest body)
  `(unless 'disabled
     ,@body))

(defmacro cape-capf-wrapper (wrapper inner)
  (when inner
    (let* ((wrapper (if (stringp wrapper) (intern wrapper) wrapper))
	   (wrapper-name (symbol-name wrapper))
	   (capf-name (symbol-name inner))
	   (name (format "%s-<%s>" capf-name wrapper-name ))
	   (symb (intern name)))
      `(defun ,symb ()
	 (funcall ',wrapper ',inner)))))

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

(cl-defmacro create-toggle-functions (value &optional &key local keymap key)
  (let* ((name (symbol-name value))
	 (suffix (when local "local"))
	 (ops (list
	       `(,(intern (symbol-join "turn-on" name suffix)) t)
	       `(,(intern (symbol-join "turn-off" name suffix)) nil)
	       `(,(intern (symbol-join "toggle" name suffix)) (not ,value))))
	 (setter (if local 'setq-local 'setq)))


    (when (and keymap (not key))
      (user-error "must define both keymap and a key"))

    `(progn
       ,@(--map `(defun ,(car it) ()
		 (interactive)
		 (,setter ,value ,(cadr it)))
       ops)
    ,(when keymap
       `(bind-key ,key ',(car (nth 2 ops)) ,keymap)))))

(defmacro with-silence (&rest body)
  "Totally suppress message from either the minibuffer or the *Messages* buffer.."
  `(let ((inhibit-message t)
         (message-log-max nil))
     (null ,@body)))

(defmacro with-quiet (&rest body)
  "Suppress any messages from appearing in the minibuffer area."
  `(let ((inhibit-message t))
     (null ,@body)))

(defmacro with-force-write (&rest body)
  (declare (indent 1) (debug t))
  `(progn
     (setq buffer-read-only nil)
     ,@body
     (setq buffer-read-only t)))

(defmacro with-temp-keymap (map &rest body)
  "Create a temporary MAP and return it after evaluating it in the BODY."
  `(let ((,map (make-sparse-keymap)))
     ,@body
     map))

(cl-defmacro add-hygenic-one-shot-hook (&key name hook function (args nil) (local nil) (depth 0) (make-unique nil))
  (let* ((unique-tag (or (when make-unique (gensym "hook-"))
			 (make-symbol "hook")))
	 (cleanup (intern (symbol-join "hygenic-one-shot" name (symbol-name unique-tag))))
	 (hook (if (symbolp hook)
		  hook
		 (eval hook))))

    `(progn
       (defun ,cleanup ,args
	 (apply ,function ,args)
         (remove-hook ',hook ',cleanup ,local)
         (unintern ',cleanup obarray))
       (add-hook ',hook ',cleanup ,depth ,local))))

(cl-defmacro set-to-current-time-on-startup (variable &optional (depth 75))
  (let ((operation (intern (format "set-%s-to-current-time" (symbol-name variable)))))
    `(progn
       (add-hook 'emacs-startup-hook ',operation ,depth)
       (defun ,operation ()
	 (setq ,variable (current-time))))))

(cl-defmacro setq-when-nil (variable value &optional &key local)
  (unless (boundp variable)
    (user-error "can only `set-when-nil' with variables that are already defined."))

  `(unless ,variable
     ,(let ((setter (if local 'setq-local 'setq))
	    (resolved-value (if (functionp value) (funcall value) value)))
	`(,setter ,variable ,resolved-value))))

(defmacro with-timer (name &rest body)
  "Report on NAME and the time taken to execute BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%s: %.06fs" ,name (float-time (time-since time)))))

(defun compile-buffer-name (name)
  `(lambda (&optional _) ,name))

(defalias 'pa 'pos-arg)

(defmacro pos-arg (name &key is)
  "Allow positional arguments to have annotated call-sites."
  (unless (or (stringp name) (symbolp name))
    (user-error "cannot annotate a positional arg without a name"))
  is)

(defun byte-compile-all-user-emacs-files ()
  "Recompile all most relevant emacs lisp files in the current
installation that need to be recompiled. Call with a prefix argument to
forcibly recompile all emacs files. Returns a list of all files that
were recompiled." 
  (interactive)
  (->> (list user-emacs-directory
	     (f-join user-emacs-directory "lisp")
	     (f-join user-emacs-directory "user"))
       (--flat-map (f-entries it #'f-file-p))
       (--filter (f-ext-p it "el"))
       (--keep (when (not (eq 'no-byte-compile (byte-recompile-file it current-prefix-arg))) it))))

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

(defun toggle-word-wrap (&optional arg)
  (interactive)
  (when arg (user-error "ambiguous argument to `toggle-word-wrap'"))
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
  (replace-regexp-in-region "\\([^\n]\\)\n\\([^ *\n]\\)" "\\1 \\2" begin end))

;; whitespace  --

(defun set-tab-width (num-spaces)
  (interactive "nTab width: ")
  (setq-local tab-width num-spaces))

;;;###autoload
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

;;;###autoload
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

;; (advice-add
;;  'kill-region :before
;;  (lambda (inner start end &rest _)
;;    (apply inner (interactive
;; 		 (if mark-active
;; 		     (list start
;; 			   end)
;; 		   (list
;; 		    (line-beginning-position)
;; 		    (line-beginning-position 2)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; bulk buffer killing -- kill groups of buffers efficiently

(defun buffers-matching-path (regexp &optional internal-too)
  (->> (buffer-list)
       (--keep (let* ((buffer it)
		      (name (buffer-file-name buffer)))
		(when (and name (not (string-equal name ""))
			   (or internal-too (/= (aref name 0) ?\s))
			   (string-match regexp name))
		  buffer)))))

(defun buffers-matching-mode (mode)
  (->> (buffer-list)
       (--select (with-current-buffer it (eq major-mode mode)))
       (mapc #'kill-buffer)))

(defun buffers-matching-project (thing)
  (cond
   ((or (bufferp thing) (and (stringp thing) (get-buffer thing)))
    (with-current-buffer thing
      (buffers-matching-path (approximate-project-root))))
   ((and (stringp thing)
	 (f-exists-p thing))
    (->> (buffer-list)
	 (--keep (f-equal-p thing (buffer-file-name it)))
	 (-distinct)
	 (--flat-map (with-current-buffer it (buffers-matching-path (approximate-project-root))))))))

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
  (let* ((buffers (buffers-matching-path regexp internal-too))
	 (killed (->> buffers 
		      (--map (cons (buffer-file-name it) (funcall (if no-ask 'kill-buffer 'kill-buffer-ask) it)))
		      (--filter (cdr it))
		      (--keep (car it))
		      (-unwind)
		      (-non-nil))))

    (if (called-interactively-p 'any)
	(message "killed %d buffers matching '%S'" (length length) (s-join ", " killed))
      killed)))

(defconst reference-source-paths
  (append (cons package-user-dir package-directory-list) (list "/usr/share/emacs/.*" "/usr/lib/go/.*" ".*/src/emacs.*/src/.*"))
  "paths of reference files, typically opened by jump-to-definition")

(defun kill-all-reference-and-source-buffers ()
  "Kill all buffers for files in external (upstream) sources, likely opened
by jump-to-definition."
  (interactive)
  (let ((killed (->> reference-source-paths
		     (-flat-map #'force-kill-buffers-matching-path)
		     (-non-nil)
		     (-map #'f-collapse-homedir))))
    (if (called-interactively-p 'any)
	(message "killed %s refrence/source buffers [%s]" (length killed) (s-join ", " killed))
      killed)))

;;;###autoload
(defun kill-buffers-matching-mode (mode)
  "Kill all buffers matching the symbol defined by MODE.
Returns the number of buffers killed."
  (interactive (list (intern
    (completing-read
     "mode: " ;; prompt
     obarray  ;; collection
     (lambda (symbol) (s-ends-with? "-mode" (symbol-name symbol)))
     t nil nil major-mode))))
  (let* ((buffers (buffers-matching-mode mode))
	 (count (length buffers)))
    (message "killing all buffers (%d) with mode \"%s\"" count mode)
    (mapc #'kill-buffer buffers)
    count))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; buffer/frame management -- helper functions.

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

(defun buffer-line-count (&optional buf)
  "Return the number of lines in the specified buffer (name or buffer), defaulting to the current buffer."
  (car (buffer-line-statistics buf)))

(defun tychoish-run-current-major-mode-hooks (&optional buffer)
  "Run all mode-hooks for the current major mode."
  (interactive)
  (with-current-buffer (or (when (bufferp buffer) buffer)
			   (when (and (stringp buffer) (get-buffer buffer)) buffer)
			   (current-buffer))
    (apply #'run-mode-hooks (--keep (-concat (intern-soft (format "%s-hook" it))) (derived-mode-all-parents major-mode)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; project -- tools for managing groups of buffers and files by project

(declare-function project-root "project")
(declare-function project-current "project")

(declare-function projectile-project-root "projectile")
(declare-function projectile-project-name "projectile")

(defun approximate-project-root ()
  (or (when (featurep 'projectile) (trimmed-string-or-nil (projectile-project-root)))
      (when (and (featurep 'project) (project-current)) (project-root (project-current)))
      (expand-file-name default-directory)))

(defun approximate-project-name ()
  (string-trim-non-word-chars
   (or (when (and (package-installed-p 'projectile) (not (featurep 'projectile))) (require 'projectile) nil)
       (when (featurep 'projectile) (projectile-project-name))
       (when (project-current) (project-root (project-current)))
       (f-filename (expand-file-name default-directory)))))

(cl-defun mode-buffers-for-project (&optional &key (mode major-mode) (directory (projectile-project-root)))
  (--keep (with-current-buffer it
	     (when (and
		    (derived-mode-p mode)
		    (file-in-directory-p default-directory directory))
	       (current-buffer)))
	  (buffer-list)))

(cl-defun mode-buffers (&optional (mode major-mode) &key (visiting 'file))
  (--keep (with-current-buffer it
	    (when (and (derived-mode-p mode)
		       (cond ((eq visiting 'read-only) (when buffer-read-only t))
			     ((eq visiting 'stale) (when-let ((stale (funcall buffer-stale-function it)))
						     (and stale (not 'fast))))
			     ((eq visiting 'file) (when buffer-file-name t))
			     ((eq visiting 'internal) (when (not buffer-file-name) t))
			     (t t)))
	      (current-buffer)))
	  (buffer-list)))

(provide 'tychoish-common)
