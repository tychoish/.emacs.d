;; -*- lexical-binding: t; -*-

(require 'f)
(require 's)
(require 'ht)
(require 'dash)

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

(defun s-trimmed-or-nil (value)
  (and (stringp value)
       (unless (string-empty-p (setq value (string-trim value))) value)
       value))

(defun s-trim-non-word-chars (value)
  (and (stringp value)
       (unless (string-empty-p (setq value (string-trim value "\\W+" "\\W+"))) value)
       value))

(defun s-contains-whitespace-p (value)
  "Return t when `VALUE' is a string with non-whitespace content and nil otherwise."
  (and (stringp value)
       (string-empty-p (string-trim value))))

(defun prefix-padding-for-annotation (key longest)
  (make-string (abs (+ 4 (- longest (length key)))) ? ))

(defun s-default (default input)
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

(defun s-number-word (num)
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
   ((eql num 11) "eleven")
   ((eql num 12) "twelve")
   ((eql num 13) "thirteen")
   ((eql num 14) "fourteen")
   ((eql num 15) "fifteen")
   ((eql num 16) "sixteen")
   ((eql num 17) "seventeen")
   ((eql num 18) "eightteen")
   ((eql num 19) "nineteen")
   ((eql num 20) "twenty")
   (:else (user-error "no string form for %d" num))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; lists -- helpers, mostly a-la dash.el

(defun larger (&optional first second)
  "Return the larger value. If either FIRST or SECOND are not numbers, treat them as 0."
  (if (> (setq first (if (numberp first) first 0))
	 (setq second (if (numberp second) second 0)))
      first
    second))

(defun smaller (&optional first second)
  "Return the smaller value. If either FIRST or SECOND are not numbers, treat them as 0."
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
(defalias '-sparse #'-non-nil)

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

(defun f-atime (filename)
  (file-attribute-access-time (file-attributes filename)))

(defun f-make-slug (s)
  "Turn a string, S, into a hyphen-seperated slug for filename."
  (downcase
   (replace-regexp-in-string
    "[^A-Za-z0-9]" "-"
    (string-clean-whitespace s))))

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
  `(defun ,(intern (concat "f-visually-compress-to-" (s-number-word num))) (path)
       (f-visually-compress-path ,num path)))

(defun f-filter-directories (options &rest sequence)
  (let ((cannonicalize (option-set-p 'cannonicalize options)))
    (setq sequence (->> (if (stringp (car sequence))
			    sequence
			  (car sequence))
			(-keep #'s-trimmed-or-nil)
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

(provide 'xlib)
