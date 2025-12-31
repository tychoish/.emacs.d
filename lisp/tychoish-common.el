;; -*- lexical-binding: t; -*-

(require 'f)
(require 's)
(require 'ht)
(require 'fn)
(require 'dash)
(require 'anaphora)

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
	(setq-local tychoish-cache--conf-emacs-host-and-instance
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
  (s-join "-" (->> (tychoish/conf-emacs-host-and-instance)
		      (reverse)
		      (-concat (-l (when (or (equal "root" user-login-name)
					     (f-symlink-p user-emacs-directory))
				     user-login-name)
				   name))
		      (reverse)
		      (-non-nil))))

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

(bind-key "C-g" 'tychoish/super-abort-minibuffers minibuffer-local-map)

(defun gui-p ()
  "Return t when the current session is or may be a GUI session."
  (when (or (daemonp) (window-system))
    t))

(defun contextual-menubar (&optional frame)
  "Display the menubar in FRAME (default: selected frame) if on a graphical display, but hide it if in terminal."
  (interactive)
  (set-frame-parameter frame 'menu-bar-lines (if (display-graphic-p frame) 1 0)))

(defun tychoish/super-abort-minibuffers ()
  (interactive)
  (if (not (minibuffer-selected-window))
      (keyboard-quit)
    (abort-minibuffers)
    (minibuffer-keyboard-quit))
  (when (minibuffer-selected-window)
    (move-beginning-of-line nil)
    (kill-line)
    (abort-minibuffers)))

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

  (unless (member 'modus-operandi custom-enabled-themes)
    (when custom-enabled-themes
      (disable-all-themes))

    (if (custom-theme-p 'modus-operandi)
	(enable-theme 'modus-operandi)
      (load-theme 'modus-operandi t nil)))

  (unless (alist-get 'alpha default-frame-alist)
    (add-to-list 'default-frame-alist '(alpha . 97))))

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
      (add-to-list 'default-frame-alist (cons 'font new-font-name))))
  (assoc 'font default-frame-alist))

(defun tychoish/ensure-font (font-face-name size)
  (unless (assoc 'font default-frame-alist)
    (tychoish-setup-font font-face-name size)))

(defun tychoish/ensure-default-font ()
  (tychoish/ensure-font "Source Code Pro" 13))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; strings -- helper functions for handling strings

(defalias 's-equal #'string-equal)
(defalias 's-empty-p #'string-empty-p)

(defun s-join-spc (&rest words)
  (->> (-filter-s-trim words)
       (s-join " ")))

(defun -filter-s-trim (strs)
  (cl-check-type strs list "strs must be list")
  (->> strs
       (-filter #'stringp)
       (-map #'string-trim)
       (-remove #'s-empty-p)))

(defun s-or-char-equal (char value)
  (cl-check-type char character "char must be character type")
  (cl-check-type char (or string character) "value must be or string")

  (if (stringp value)
      (string-equal (char-to-string char) value)
    (char-equal char value)))

(cl-defmacro def-join-str-with (char &optional &key use-jargon-names space-padding)
  (cl-check-type char character "must create join function using the character to join the strings")

  (let* ((name (downcase (cond
	       ((s-or-char-equal ?- char) (if use-jargon-names "kebab" "hyphen"))
	       ((s-or-char-equal ?_ char) (if use-jargon-names "snake" "underscore"))
	       ((s-or-char-equal ?. char) (if use-jargon-names "dot" "period"))
	       ((s-or-char-equal ?  char) (if use-jargon-names "spc" "space"))
	       ((s-or-char-equal ?= char) "equal")
	       ((s-or-char-equal ?+ char) "plus")
	       ((s-or-char-equal ?| char) "pipe")
	       ((s-or-char-equal ?> char) "lt")
	       ((equal char ", ") "comma-space")
	       ((equal char "; ") "semicolon-space")
	       ((equal char ">=") "gte")
	       ((equal char "<=") "lte")
	       ((equal char "<=") "lte")
	       ((equal char "__") (if use-jargon "dunder" "double-underscore"))
	       ((equal char "--") (if use-jargon "ddash" "double-dash"))
	       ((equal char "=>") (if use-jargon-names "fat-arrow" "double-arrow"))
	       ((equal char "=>>") "dubble-fat-arrow")
	       ;; e.g. '(; : )
	       (t (s-join "-" (s-split " " (char-to-name char)))))))
	(op-name (concat "s-join-with-" name))
	(padding (if space-padding " " ""))
	(join-with (concat padding (char-to-string char) padding)))

  `(defun ,(intern op-name) (&rest words)
     (->> words
	  (-filter-s-trim)
	  (s-join ,join-with)))))

(def-join-str-with ?-)
(def-join-str-with ? )
(def-join-str-with ?_)
(def-join-str-with ?| :space-padding t)

(defun s-shortest (a b)
  (if (> (length b) (length a))
      a
    b))

(defun s-collapse-hyphens (str)
  (replace-regexp-in-string "-\\{3,\\}" "-" str))

(defun s-normalize-symbol-name (name)
  (let* ((sanatized (s-trim (s-collapse-whitespace name)))
	 (canonicalized (s-replace-all (--map `(,it "-") '("=" "_" " " "_" "'" "\"" "\\" "/")) name)))
    (s-collapse-hyphens canonicalized)))

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

(defun number-to-word (num)
  (cond
   ((eql num 1) "one")
   ((eql num 2) "two")
   ((eql num 3) "three")
   ((eql num 4) "four")
   ((eql num 5) "five")
   ((eql num 6) "six")
   ((eql num 7) "seven")
   ((eql num 8) "eight")
   ((eql num 9) "nine")
   ((eql num 10) "ten")
   (:else (user-error "no string form for %d" num))))

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

(cl-defun -distinct-by-car (cell &optional &key (test #'equal))
  "Take a list of cons cells and return a new list that contains only the
elements that have unique car values, ignoring the cdr entirely. Compare
values using the test function, which defaults to `equal'."
  (let ((-compare-fn (lambda (a b) (funcall test (car a) (car b)))))
    (-distinct cell)))

(cl-defun -distinct-paths (cell)
  (let ((-compare-fn #'f-equal-p)
    (-distinct cell))))

(cl-defun -distinct-by-alist-key (key cell &optional &key (test #'equal))
  "Compare a list of alists, and return a new list that contains only the
alists that have distinct values for a specific key. Compare values using the
test function, which defaults to `equal'."
  (let ((-compare-fn (lambda (a b) (funcall test (alist-get key a) (alist-get key b)))))
    (-distinct cell)))

(defun -unwind (list)
  "Flattens a list of lists into a list by collecting items in one list"
  (-flatten-n 1 list))

(defalias '-flat-map #'mapcan
  "`-flat-map' applies (maps) a function, which returns a list, to each
item in a list. The lists that result from the map operation are then
concatenated or joined. This provides a dash.el conforming API for the
`mapcan' operation.")

(defalias '-mapc #'mapc)
(defalias '-join #'nconc)
(defalias '-append #'append)
(defalias '-reverse #'nreverse)

(defalias '-c #'cons)
(defalias '-l #'list)
(defalias 'll #'list)
(defalias '-- #'list)

(defun -sparse-append (&rest items)
  "Joins elements in a list, omitting all nil values."
  (->> items
       (apply #'-append)
       (-non-nil)))

(defun -map-in-place (mapper items)
  "Apply the `mapper' function to every item in the list `items' and
replace the items in the original list with the results of the function,
returning the list. This is a destructive operation."
  (let ((output items)
	(head items))
     (while head
       (setf (car head) (funcall mapper (car head)))
       (setq head (cdr head)))
     output))

(defun -in-place (mapper items)
  "Apply the `mapper' function to every item in the list `items' and
replace the items in the original list with the results of the function,
returning a count of the number of items in the list. This is a
destructive operation."
  (let ((head items)
	(count 0))
    (while head
      (setf (car head) (funcall mapper (car head)))
      (setq head (cdr head))
      (cl-incf count))
    count))

(cl-defun -map-uniq (mapper input &optional &key (test #'equal))
  "Apply the `mapper' function to every item in the `input' list, returning
a new list that contains the unique output of the list. Comparisons use
the `test' function, which defaults to `equal'."
  (let ((head input)
	(seen (ht-create test))
	current
	output)
    (while head
      (unless (ht-contains-p seen (setq current (funcall mapper (car head))))
	(ht-set seen current current)
	(push current output))
      (setq head (cdr head)))
    output))

(defmacro --mapc (form input-list)
  "Apply the form (with the current element avalible as the variable `it')
to all item in the list, primarily for side effects. Returns the input
list. This is an anaphoric equivalent to `mapc'. As opposed to `--each'
and `-each', which return nil, `-mapc' returns the input list.

This is the anaphoric counterpart to `-mapc'."
  (declare (debug (def-form form)))
  `(mapc (lambda (it) (ignore it) ,form) ,input-list))

(defmacro --flat-map (form input-list)
  "`--flat-map' evaluates a form for very item in `input-list' with the
item bound to `it'. The form must return a list, and the returned lists
are then concatenated or joined into a single flattened list. This
provides a dash.el conforming API for the `mapcan' operation.

This is the anaphoric counterpart to `-flat-map'."
  (declare (debug (def-form form)))
  `(mapcan (lambda (it) (ignore it) ,form) ,input-list))

(defmacro --map-in-place (form items)
  "Apply the form, (with the current element as `it') to each item in the
list, distructively setting the return value of the form to the value in
the list.

This is the anaphoric counterpart to `-map-in-place'."
  (declare (debug (def-form form)))
  `(-map-in-place (lambda (it) (ignore it) ,form) ,items))

(defmacro --in-place (form items)
  "Take a list and replace each element in the list with the result of
evaluating `FORM' for that element. The original element is accessable
in the `FORM' as `it`. Returns the number of items in the list. This is
a destructive operation.

This is the anaphoric counterpart to `-in-place'."
  (declare (debug (def-form form)))
  `(-in-place (lambda (it) (ignore it) ,form) items)
  `(let ((head ,items)
	 (count 0))
     (while head
       (setf (car head) (funcall #'(lambda (it) (ignore it) ,form) (car head)))
       (setq head (cdr head))
       (setq count (+ 1 count)))
     count))

(defmacro --map-uniq (form input)
  "Apply `FORM' to every item in the input list, available as `it', and
collect the UNIQUE results, and returning them as a list.
Items are compared for uniqueness with `equal' by default, which can be
overridden with the `-compare-fn' dynamic variable.

This is the anaphoric counterpart to -map-unique. Although the handling
of the equality function customization differs slightly."
  (declare (debug (def-form form)))
  `(-map-uniq (lambda (it) (ignore it) ,form) ,input :test (dash--hash-test-fn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `ht.el' -- extensions and aditions

(defmacro ht-get-lambda (table)
  `(lambda (key) (ht-get ,table key)))

(defmacro ht-set-lambda (table)
  `(lambda (key value) (ht-set ,table key value)))

(defmacro ht-contains-p-lambda (table)
  `(lambda (key) (ht-contains-p ,table key)))

(defmacro ht-get-function (table)
  (let ((name (symbol-name table)))
    `(defun ,(format "ht-%s-get" name) (key) (ht-get ,table key))))

(defmacro ht-set-function (table)
  (let ((name (symbol-name table)))
    `(defun ,(format "ht-%s-set" name) (key value) (ht-set ,table key value))))

(defmacro ht-contains-p-function (table)
  (let ((name (symbol-name table)))
    `(defun ,(format "ht-%s-contains-p" name) (key) (ht-contains-p ,table key))))

(cl-defmacro define-ht-named-table (name &optional &key (test #'equal))
  (let ((name (or (when (stringp name) (intern name))
		  (when (symbolp name) name)
		  (intern (format "%S" name))))
	(table (or (when (ht-p name) name)
			 (ht-create test))))
    `(progn
       (defvar ,name ,table
	 ,(format "Hash table `%s' with named accessor functions." name))
       (ht-get-function ,name)
       (ht-set-function ,name)
       (ht-contains-p-function ,name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `f.el' -- extensions and additions

(defun f-mtime (filename)
  (file-attribute-modification-time (file-attributes filename)))

(defmacro f-file-with-ext-p-function (extension)
  `(defun ,(intern (format "f-%s-file-p" (string-replace "." "" (downcase extension)))) (file)
     (and (f-file-p file) (f-ext-p file ,extension))))

(defmacro f-filename-is-function (name)
  `(lambda (filename) (f-filename-is-p filename ,name)))

(defun f-filename-is-p (entry name)
  (f-equal-p (f-filename entry) name))

(defun f-when-file-exists (path)
  (when (f-exists-p path)
    path))

(defun f-distinct (sequence)
  (let ((-compare-fn #'f-equal-p))
    (-distinct sequence)))

(defmacro f-directories-containing-file-with-extension-function (extension)
  (when (s-prefix-p "." extension)
    (setq extension (string-trim-left extension "^\\.")))

  `(progn
     (f-file-with-ext-p-function ,extension)

     (defun ,(intern (format "f-directories-containing-file-with-extension-%s" (string-replace "." "" (downcase extension)))) (paths)
       (when (stringp paths)
	 (setq paths (list paths)))
       (->> paths
	    (--flat-map (f-entries it #'f-file-p))
	    (--filter (f-ext-p it ,extension))
	    (-map #'f-dirname)
	    (-distinct)))))

(defun f-files-in-directory (path)
  (cond
   ((stringp path)
    (cond
     ((f-directory-p path) (f-entries path #'f-file-p))
     ((f-file-p path) (f-entries (f-dirname path) #'f-file-p))))
   ((listp path) (--flat-map (f-entries it #'f-file-p) path))))

(defun f-recursive-directories-containing (filename &optional path)
  (->> (f-entries path (lambda (filename) (f-filename-is-p filename "go.mod")) t)
       (-map #'f-dirname)))

(defmacro f-directories-containing-file-function (filename &rest files)
  (let* ((filenames (cons filename files))
	 (symbol-filename (string-replace "." "-" (downcase filename)))
	 (pred (intern (s-join-with-hyphen "f-directory-contains" symbol-filename "file"))))
    `(progn
       (defun ,(intern (format "f-filename-is-%s-p" symbol-filename)) (path)
	 (apply #'or (--map (f-filename-is-p path it) ,filenames)))

       (defun ,pred (path)
	 (let ((matching (->> (f-files-in-directory path)
			      (--filter (f-equal-p ,filename (f-filename it)))
			      (-uniq)
			      (-non-nil))))
	   (when matching
	     (car matching))))

       (defun ,(intern (s-join-with-hyphen "f-directories-containing-file" symbol-filename)) (path &rest paths)
	 (let ((filenames (list ,@filenames))
	       (paths (cons path paths)))
	   (->> paths
		(-flat-map #'f-files-in-directory)
		(--filter (car (member (f-filename it) filenames)))
		(-map #'f-dirname)
		(-distinct)))))))

(defun f-collapse-homedir (path)
  (string-replace (expand-file-name "~/") "~/" path))

(defun f-visually-compress-path (num path)
  (->> (f-split path)
       (--map (if (length> it num)
		  (substring it 0 num)
		it))
       (--replace-where (string-equal it (f-path-separator)) "")
       (s-join (f-path-separator))))

(defmacro f-visual-compression-function (num)
  `(defun ,(intern (concat "f-visually-compress-to-" (number-to-word num))) (path)
       (f-visually-compress-path ,num path)))

(defun f-filter-directories (options &rest sequence)
  (let ((cannonicalize (option-set-p 'cannonicalize options)))
    (setq sequence (->> (if (stringp (car sequence))
			    sequence
			  (car sequence))
			(-keep #'trimmed-string-or-nil)
			(--map (or (when (f-absolute-p it) it)
				   (if cannonicalize
				       (expand-file-name it)
				     it)))
			(--keep (let ((path it))
				  (cond ((f-file-p path)
					 (when (f-directory-p (setq path (f-dirname path))) path))
					((f-directory-p it) it))))))

    (when cannonicalize
      (setq sequence (-map #'f-full sequence)))

    (when (option-set-p 'unique options)
      (setq sequence (f-distinct sequence)))

    (-filter #'f-directory-p sequence)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; macros -- helper macros for common operations

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

(cl-defmacro create-toggle-functions (value &optional &key short-name local keymap key)
  (let* ((name (or short-name (symbol-name value)))
	 (suffix (when local "local"))
	 (ops (list
	       `(,(intern (s-join-with-hyphen "turn-on" name suffix)) t)
	       `(,(intern (s-join-with-hyphen "turn-off" name suffix)) nil)
	       `(,(intern (s-join-with-hyphen "toggle" name suffix)) (not ,value))))
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

(cl-defmacro make-read-extended-command-for-prefix (prefix &optional &key bind-map bind-key key-alias)
  (unless (setq prefix (trimmed-string-or-nil prefix))
    (user-error "cannot build predicate function for '%s'" prefix))
  (setq prefix (s-normalize-symbol-name prefix))

  (let* ((predicate-name (format "read-extended-command-for-%s-prefix-p" prefix))
	 (predicate-symbol (intern predicate-name))
	 (user-command-name (format "execute-extended-%s-command" prefix))
	 (user-command-symbol (intern user-command-name)))
  `(progn (defun ,predicate-symbol (command buffer)
	    ,(format "Predicate for `read-extended-command-predicate' to filter commands returning only those that start with the prefix `%s'" prefix)
	    (s-prefix-p ,prefix (symbol-name command)))
	  (defun ,user-command-symbol ()
	    ,(format "Read extentend command but filtered for only those beginning with prefix `%s'." prefix)
	    (interactive)
	    (let ((read-extended-command-predicate #',predicate-symbol))
	      (execute-extended-command nil)))
	  ,(when bind-key
	     `(progn 
		(bind-keys
		 ,@(when bind-map `(:map ,bind-map))
		 (,bind-key . ,user-command-symbol))
		,(when key-alias
		   `(which-key-add-keymap-based-replacements ,(or bind-map global-map) ,bind-key ,key-alias)))))))

(defmacro with-toggle-once (name &rest body)
  (declare (indent 1) (debug t))
  (let ((operation (or (when (symbolp name) name)
		       (when (stringp name) (intern name))))
	(toggle (intern (s-join-with-hyphen (symbol-name name) "toggle-state"))))

  `(progn
     (defvar ,toggle nil
       "Toggle variable to avoid re-execution of expensive configuration (like setting environment variables.)")

     (defun ,operation ()
       (unless ,toggle
	 ,@body
	 (setq ,toggle t))))))

(defmacro with-prefix-arg (arg &rest body)
  `(let ((current-prefix-arg ,arg))
     ,@body))

(defmacro with-default-directory (path &rest body)
  "Run the body with `default-directory' set to the path provided"
  (declare (indent 1) (debug t))
  `(let ((default-directory ,path))
     ,@body))

(defmacro with-silence (&rest body)
  "Totally suppress message from either the minibuffer or the *Messages* buffer.."
  (declare (indent 1) (debug t))
  `(let ((inhibit-message t)
         (message-log-max nil))
     ,@body))

(defmacro with-quiet (&rest body)
  "Suppress any messages from appearing in the minibuffer area."
  (declare (indent 1) (debug t))
  `(let ((inhibit-message t))
     ,@body))

(defmacro with-force-write (&rest body)
  (declare (indent 1) (debug t))
  `(progn
     (setq buffer-read-only nil)
     ,@body
     (setq buffer-read-only t)))

(defmacro with-temp-keymap (map &rest body)
  "Create a temporary MAP and return it after evaluating it in the BODY."
  (declare (indent 1) (debug t))
  `(let ((,map (make-sparse-keymap)))
     ,@body
     map))

(cl-defmacro add-hygenic-one-shot-hook
    (&key
     name
     hook
     function
     result
     body
     form
     operation
     ;; flages and options; with defaults
     (args nil)
     (local nil)
     (depth 0)
     (make-unique nil)
     (cleanup nil))
  (let* ((unique-tag (or (when make-unique (gensym "hook-"))
			 (make-symbol "hook")))
	 (cleanup-symbol (intern (s-join-with-hyphen "hygenic-one-shot" name (symbol-name unique-tag))))
	 hooks)

    (when (eq hook 'after-first-frame-created)
      (setq hook (if (daemonp)
		     'server-after-make-frame-hook
		   'window-setup-hook)))

    (when (functionp hook)
      (setq hook (funcall hook)))

    (when (symbolp hook)
      (setq hooks (list hook)))

    (when (listp hook)
      (if (null (-remove #'symbolp hook))
	  (setq hooks hook)
	(setq hooks (eval hook)))
      (if (symbolp hooks)
	  (setq hooks (list hooks))))

    (unless hooks
      (user-error "must have a symbol, list of symbols or form that evaluates to same for hook [%S]" hooks))

    `(progn
       (defun ,cleanup-symbol ,args
	 (with-slow-op-timer
	  ,(format "<hygenic-hook> %s" name)

	  ,(aif (cond (form
		  form)
		(body
		 `,@body)
		(result
		 `,(eval result))
		(operation
		  (if args
		      `(apply ,operation ,args)
		    `(funcall ,operation)))
		((and (symbolp function) (functionp function))
		  (if args
		      `(apply ',function ,args)
		    `(funcall ',function)))
		((and (functionp function) (listp function))
		 function)
		((symbolp function)
		 (eval function))
		((listp function)
		 function))
	       it
	     (user-error "could not resolve the hook function from input for %s" name)))

	 ,@(--map
	    `(remove-hook ',it ',cleanup-symbol ,local)
	    (--remove (eq 'quote it) hooks))

         ,(if (or make-unique cleanup)
	     `(unintern ',cleanup-symbol obarray)
	   t))

       ,@(--map `(add-hook ',it ',cleanup-symbol ,depth ,local) (--remove (eq 'quote it) hooks)))))

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

(defmacro merge-predicate-functions (&rest preds)
  `(lambda (value)
    (let ((head ,preds)
	  (predicate ,(car preds))
	  (val t))
      (while (and val predicate)
	(setq val (funcall predicate value)
	      head (cdr head)
	      predicate (car head)))
      val)))

(declare-function package-installed-p "package")
(declare-function package-desc-p "package")

(defun package-avalible-p (name)
  (or (featurep name)
      (when (package-installed-p name)
	(require name)
	t)))

(autoload 'async-package-do-action "async-package")

(defun async-package-operation (op pkgs)
  (let* ((ops '(install upgrade 'reinstall))
	 (valid-packages (--filter (or (symbolp it) (package-desc-p it)) pkgs))
	 (filename (concat (f-join temporary-file-directory
			   (s-join "-" (list
					"emacs" tychoish/emacs-instance-id
					"async-package"
					(symbol-name op)))) ".log")))
    (unless (member op ops)
      (user-error "%s is not a valid operation %S" op ops))

    (unless valid-packages
      (user-error "must define one or more valid packages %s [%s]" valid-packages pkgs))

    (async-package-do-action op valid-packages filename)))

(defun package-install-async (pkgs)
  (interactive (list (intern (completing-read "async-install-package =>" package-archive-contents))))
  (async-package-operation 'install pkgs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; compile -- compilation mode helpers

(cl-defun tychoish/compile--post-hook-collection
    (buffer
     compile-result-message
     started-at
     &optional
     &key
     process-name
     program
     args
     send-when
     (alert-threshold 60)
     (notification-threshold 120))
  (let* ((end-at (current-time))
	 (duration (time-subtract end-at started-at))
	 (compile-result-message (s-trim compile-result-message))
	 (msg (format "build %s in %.06fs -- %s"
		      compile-result-message
		      (float-time duration)
		      (with-current-buffer buffer
		        (s-trim (car compilation-arguments))))))

    (when (or send-when
	      (> alert-threshold (float-time (time-since (current-idle-time))))
	      (and (> (float-time duration) notification-threshold) process-name program args))

      (apply #'async-start-process
	     (pa "emacs-process-name" :is process-name)
	     (pa "program" :is program)
	     (pa "on-finish" :is (lambda (out) (message "INFO: notify process for %s completed [%s] with %s" (buffer-name buffer) out compile-result-message)))
	     (-append args (strings-list (format "<%s> %s -- %s" tychoish/emacs-instance-id (buffer-name buffer) msg)))))

    (when (or send-when
	      (> (/ alert-threshold 2) (float-time (time-since (current-idle-time))))
	      (> alert-threshold (ffloor (float-time duration))))
      (alert
       msg
       :title (format "%s:%s:%s" tychoish/emacs-instance-id (buffer-name buffer) compile-result-message)
       :buffer buffer))

    (with-current-buffer buffer
      (save-excursion
	(setq buffer-read-only nil)
	(goto-char (point-min))
	(unless (eq (point-min) (re-search-forward "\\(^Compilation.*\n$\\|\n{2,}\\)"))
	  (replace-match ""))
	(goto-char (point-max))
	(compilation-insert-annotation
	 (format "\n--- %s completed in %.06fs at %s\n\n"
		 compile-result-message (float-time duration)
		 (format-time-string "%Y-%m-%d %H:%M:%S" end-at)))
	 (setq buffer-read-only t)))))

(defun compilation-finish-functions-reset-all ()
  (interactive)
  (->> (mode-buffers 'compilation-mode)
       (--mapc (with-current-buffer it
		 (setq-local compilation-finish-functions nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; lines -- whitespace,  filling/wrapping, line manipulation

;; word-wrapping  --

(defalias 'turn-on-hard-wrap 'turn-off-soft-wrap)
(defalias 'turn-off-hard-wrap 'turn-on-soft-wrap)
(defalias 'toggle-soft-wrap 'toggle-on-soft-wrap)
(defalias 'toggle-hard-wrap 'toggle-off-soft-wrap)

(declare-function visual-fill-column-mode "`visual-fill-column'")

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; process management and collecting shell output

(cl-defstruct p-result
  "Structure for handling results of external processes."
  (code -1
   :documentation "Exit code of process. -1 indicates the process hasn't run or isn't complete"
   :type integer)
  (command ""
   :documentation "String of the shell command"
   :type string)
  (output nil
   :type list
   :documentation "Output of command")
  (wrapper '("bash" "-c")
   :type list
   :documentation "prefix/wrapper used for command")
  (directory default-directory
   :type string
   :documentation "directory where command was run"))

(defun p-bash-lines-sync (command)
  (let ((proc (make-p-result :command command
			     :wrapper '("bash" "-c"))))
    (setf (p-result-output proc)
	  (apply #'process-lines-handling-status
		 (append
		  (list
		   (car (p-result-wrapper proc)) ;; e.g. "bash"
		   (lambda (code) (setf (p-result-code proc) code))) ;; status-handler
		  (strings-list (cadr (p-result-wrapper proc)))
		  (strings-list command))))
    proc))

(defun strings-list (&rest input &key options)
  "Try to produce a valid list-of strings. Returning lists of strings, filtering or coercing as needed. "
  (if (list-of-strings-p input)
      input
    (->> input
	 (--keep (cond
		  ((stringp it) it)
		  ((option-set-p 'filter options) nil)
		  ((option-set-p 'stringify) (format "%s" it))
		  (t (user-error "no way to handle %s value %s" (type-of it) it)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; bulk buffer killing -- kill groups of buffers efficiently

(defun buffer-derived-mode-p (buffer mode)
  (with-current-buffer buffer
    (when (derived-mode-p mode)
      t)))

(defun save-all-buffers ()
  (interactive)
  (save-some-buffers t t))

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

(defun buffer-directory (buf)
  "Return the `default-directory' of the provide buffer."
  (when (bufferp buf)
    (with-current-buffer buf
      (let ((file-name (buffer-file-name buf)))
	(cond ((null file-name) nil)
	      ((f-directory-p file-name) file-name)
	      ((f-file-p file-name) (f-dirname file-name))
	      (t default-directory))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; files and notes

(defun make-filename-slug (s)
  "Turn a string, S, into a slug for a blog post filename."
  (downcase
   (replace-regexp-in-string
    "[^A-Za-z0-9]" "-"
    (string-clean-whitespace s))))

(defun tychoish-insert-date ()
  "Insert date string."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; project -- tools for managing groups of buffers and files by project

(declare-function project-root "project")
(declare-function project-current "project")
(declare-function project-buffers "project")

(declare-function projectile-project-root "projectile")
(declare-function projectile-project-name "projectile")
(declare-function projectile-project-buffers "projectile")

(defun approximate-project-root ()
  (or (when (and (package-installed-p 'projectile) (not (featurep 'projectile))) (require 'projectile) nil)
      (when (package-avalible-p 'projectile)
	(trimmed-string-or-nil (projectile-project-root)))
      (when (and (featurep 'project) (project-current))
	(project-root (project-current)))
      (expand-file-name default-directory)))

(defun approximate-project-name ()
  (string-trim-non-word-chars
   (or (when (and (package-installed-p 'projectile) (not (featurep 'projectile))) (require 'projectile) nil)
       (when (featurep 'projectile) (projectile-project-name))
       (when (project-current) (project-root (project-current)))
       (f-filename (expand-file-name default-directory)))))

(defun approximate-project-buffers ()
  (or
   (when (and (package-installed-p 'projectile) (not (featurep 'projectile))) (require 'projectile) nil)
   (when (package-avalible-p 'projectile) (projectile-project-buffers))
   (when (and (featurep 'project) (project-current)) (project-buffers))
   (let ((directory (expand-file-name default-directory)))
     (->> (buffer-list)
	  (--filter (with-current-buffer it
		      (file-in-directory-p default-directory directory)))))))

(cl-defun mode-buffers-for-project (&optional &key (mode major-mode))
  (->> (approximate-project-buffers)
       (--filter (buffer-derived-mode-p it mode))))

(cl-defun mode-buffers (&optional (mode major-mode))
  (->> (buffer-list)
       (--keep
	(with-current-buffer it
	  (when (derived-mode-p mode)
	    (current-buffer))))))

(provide 'tychoish-common)
