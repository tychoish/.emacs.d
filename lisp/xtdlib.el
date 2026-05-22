;;; xtdlib.el --- Extended elisp utility library -*- lexical-binding: t -*-

;; Author: tychoish
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (f "0.20") (s "1.12") (ht "2.3") (dash "2.19"))
;; Keywords: extensions utility
;; URL: https://github.com/tychoish/xtdlib.el

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Extensions and additions for f.el, s.el, ht.el, and dash.el, plus
;; utility macros for hooks, timers, and annotations to improve code
;; ergonomics and clarity.

;;; Code:

(require 'f)
(require 's)
(require 'ht)
(require 'dash)

(declare-function bind-key "bind-key")
(declare-function bind-keys "bind-key")
(declare-function which-key-add-keymap-based-replacements "which-key")

(eval-when-compile
  (require 'cl-lib))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; slow-op -- reporting for operation timing

(defvar slow-op-reporting debug-on-error)
(defvar slow-op-threshold 0.01)

(defmacro with-slow-op-timer (name &rest body)
  "Send a message the BODY operation of NAME takes longer to execute than a hardcoded threshold."
  (declare (indent defun) (debug t))
  `(if (not slow-op-reporting)
       (progn ,@body)
     (let* ((inhibit-message t)
	    (time (current-time))
	    (return-value (progn ,@body))
	    (duration (time-to-seconds (time-since time))))
       (when (> duration slow-op-threshold)
	 (message "[op]: %s: %.06fs" ,name duration))
       return-value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; strings -- helper functions for handling strings

(defalias 's-equal #'string-equal)
(defalias 's-empty-p #'string-empty-p)

(defun -filter-s-trim (strs)
  "Return STRS with non-strings removed and remaining strings trimmed of whitespace.
Empty strings and strings that are entirely whitespace are excluded from the result."
  (cl-check-type strs list "strs must be list")
  (->> strs
       (-filter #'stringp)
       (-map #'string-trim)
       (-remove #'s-empty-p)))

(defun s-or-char-equal (char value)
  "Return t when CHAR equals VALUE, where VALUE may be a character or single-char string."
  (cl-check-type char  character             "char must be a character")
  (cl-check-type value (or string character) "value must be a string or character")
  (if (stringp value)
      (string-equal (char-to-string char) value)
    (char-equal char value)))

(eval-and-compile
  (defconst xtdlib--join-char-names
    '((?- "hyphen" "kebab")
      (?_ "underscore" "snake")
      (?. "period" "dot")
      (?\s "space" "spc")
      (?= "equal" "equal")
      (?+ "plus" "plus")
      (?| "pipe" "pipe")
      (?> "gt" "gt"))
    "Alist of (CHAR canonical-name jargon-name) for `s-define-join-string-function'."))

(cl-defmacro s-define-join-string-function (char &optional &key use-jargon-names space-padding)
  "Define a function `s-join-with-NAME' that joins non-empty strings with CHAR as separator.
NAME is derived from CHAR via `xtdlib--join-char-names', falling back to the Unicode character
name. When USE-JARGON-NAMES is non-nil, the alternate jargon name is used (e.g. kebab vs
hyphen). When SPACE-PADDING is non-nil, the separator is surrounded by spaces."
  (cl-check-type char character "must create join function using the character to join the strings")
  (let* ((entry (assoc char xtdlib--join-char-names))
         (name (downcase (if entry
                             (if use-jargon-names (nth 2 entry) (nth 1 entry))
                           (s-join "-" (s-split " " (char-to-name char))))))
	 (op-name (concat "s-join-with-" name))
	 (padding (if space-padding " " ""))
	 (join-with (concat padding (char-to-string char) padding)))
    `(defun ,(intern op-name) (&rest words)
       (->> words
	    (-filter-s-trim)
	    (s-join ,join-with)))))

(s-define-join-string-function ?-)
(s-define-join-string-function ? )
(s-define-join-string-function ?_)
(s-define-join-string-function ?- :use-jargon-names t)
(s-define-join-string-function ?  :use-jargon-names t)
(s-define-join-string-function ?_ :use-jargon-names t)
(s-define-join-string-function ?| :space-padding t)

(defun s-shortest (a b)
  "Return the shorter of strings A and B. When equal in length, return A."
  (if (>= (length b) (length a))
      a
    b))

(defun s-collapse-hyphens (str)
  "Replace runs of three or more consecutive hyphens in STR with a single hyphen."
  (replace-regexp-in-string "-\\{3,\\}" "-" str))

(defun s-normalize-symbol-name (name)
  "Normalize NAME to a clean hyphen-separated string suitable for use as a symbol name.
Trims outer whitespace, collapses internal whitespace, replaces common punctuation
with hyphens, and collapses runs of three or more hyphens."
  (let* ((sanatized (s-trim (s-collapse-whitespace name)))
	 (canonicalized (s-replace-all (--map (cons it "-") '("=" "_" " " "_" "'" "\"" "\\" "/")) sanatized)))
    (s-collapse-hyphens canonicalized)))

(defun s-trimmed-or-nil (value)
  "Return VALUE trimmed of surrounding whitespace, or nil if VALUE is not a string or is empty after trimming."
  (and (stringp value)
       (unless (string-empty-p (setq value (string-trim value))) value)
       value))

(defun s-trim-non-word-chars (value)
  "Trim leading and trailing non-word characters from VALUE.
Return nil if VALUE is not a string or is empty after trimming."
  (and (stringp value)
       (unless (string-empty-p (setq value (string-trim value "\\W+" "\\W+"))) value)
       value))

(defun s-blank-p (value)
  "Return t when VALUE is a string that is empty or consists entirely of whitespace.
Returns nil for non-strings, non-empty strings, and strings with non-whitespace content.
Use this to guard against blank user input or empty configuration values."
  (and (stringp value)
       (string-empty-p (string-trim value))))

(defalias 's-contains-whitespace-p #'s-blank-p)

(defun s-default (default input)
  "Return DEFAULT when INPUT is nil or an empty string, otherwise return INPUT."
  (if (or (null input) (string-equal input ""))
      default
    input))

(eval-and-compile
  (defun s-number-word (num)
    "Return the English word for integer NUM. Supports values 1 through 20."
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
     ((eql num 18) "eighteen")
     ((eql num 19) "nineteen")
     ((eql num 20) "twenty")
     (:else (user-error "no string form for %d" num)))))

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

(cl-defmacro make-add-to-list-fn (list &optional &key append)
  "Return a lambda that appends ITEM to LIST via `add-to-list'.
LIST may be a bare symbol or a quoted symbol."
  (let ((sym (if (and (consp list) (eq (car list) 'quote))
                 (cadr list)
               list)))
    `(lambda (item) (add-to-list ',sym item ,append))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `dash.el' -- extensions and additons

(cl-defun -distinct-by-car (cell &optional &key (test #'equal))
  "Take a list of cons cells and return a new list that contains only the
elements that have unique car values, ignoring the cdr entirely. Compare
values using the test function, which defaults to `equal'."
  (let ((-compare-fn (lambda (a b) (funcall test (car a) (car b)))))
    (-distinct cell)))

(cl-defun -distinct-paths (cell)
  "Return CELL with duplicate file paths removed, using `f-equal-p' for comparison.
Unlike `-distinct', this handles paths that differ only in trailing slashes or
relative vs. absolute form when the filesystem resolves them to the same location."
  (let ((-compare-fn #'f-equal-p))
    (-distinct cell)))

(cl-defun -distinct-by-alist-key (key cell &optional &key (test #'equal))
  "Compare a list of alists, and return a new list that contains only the
alists that have distinct values for a specific key. Compare values using the
test function, which defaults to `equal'."
  (let ((-compare-fn (lambda (a b) (funcall test (alist-get key a) (alist-get key b)))))
    (-distinct cell)))

(defun -unwind (list)
  "Flatten LIST by exactly one level, collecting nested list items into a single list.
Unlike `-flatten', which recurses fully, `-unwind' only removes one layer of nesting."
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

(defun -strings (&rest input)
  "Return INPUT as a flat list, optionally coercing non-string elements.
A trailing `:options OPT' pair in INPUT selects how non-strings are handled:
`filter' drops them, `stringify' formats them with `format'. OPT may be a
list combining these symbols. When `:options' is not provided or its value
is nil, INPUT is returned as-is without coercion. A `user-error' is signaled
only when OPT is some other, unrecognized value."
  (let* ((idx (cl-position :options input))
	 (options (and idx (nth (1+ idx) input)))
	 (input (if idx
		    (append (cl-subseq input 0 idx)
			    (cl-subseq input (+ idx 2)))
		  input)))
    (cond
     ((null options) input)
     ((option-set-p 'filter options)
      (-filter #'stringp input))
     ((option-set-p 'stringify options)
      (--map (if (stringp it) it (format "%s" it)) input))
     (t (user-error "invalid `:options' value for `-strings': %S" options)))))

(defun -sparse-append (&rest items)
  "Append ITEMS and remove all nil values from the concatenated result."
  (-non-nil (apply #'append items)))

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
  "Destructively apply MAPPER to every item in ITEMS, replacing each element in place.
Returns the count of elements processed. Use this when you need the element count rather
than the list itself; prefer `-map-in-place' when you need the modified list back."
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
  (declare (indent defun) (debug (def-form form)))
  `(mapc (lambda (it) (ignore it) ,form) ,input-list))

(defmacro --flat-map (form input-list)
  "`--flat-map' evaluates a form for very item in `input-list' with the
item bound to `it'. The form must return a list, and the returned lists
are then concatenated or joined into a single flattened list. This
provides a dash.el conforming API for the `mapcan' operation.

This is the anaphoric counterpart to `-flat-map'."
  (declare (indent defun) (debug (def-form form)))
  `(mapcan (lambda (it) (ignore it) ,form) ,input-list))

(defmacro --map-in-place (form items)
  "Apply the form, (with the current element as `it') to each item in the
list, distructively setting the return value of the form to the value in
the list.

This is the anaphoric counterpart to `-map-in-place'."
  (declare (indent defun) (debug (def-form form)))
  `(-map-in-place (lambda (it) (ignore it) ,form) ,items))

(defmacro --in-place (form items)
  "Take a list and replace each element in the list with the result of
evaluating `FORM' for that element. The original element is accessable
in the `FORM' as `it`. Returns the number of items in the list. This is
a destructive operation.

This is the anaphoric counterpart to `-in-place'."
  (declare (indent defun) (debug (def-form form)))
  `(-in-place (lambda (it) (ignore it) ,form) ,items))

(defmacro --map-uniq (form input)
  "Apply `FORM' to every item in the input list, available as `it', and
collect the UNIQUE results, and returning them as a list.
Items are compared for uniqueness with `equal' by default, which can be
overridden with the `-compare-fn' dynamic variable.

This is the anaphoric counterpart to -map-unique. Although the handling
of the equality function customization differs slightly."
  (declare (indent defun) (debug (def-form form)))
  `(-map-uniq (lambda (it) (ignore it) ,form) ,input :test (dash--hash-test-fn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `ht.el' -- extensions and aditions

(defmacro ht-get-lambda (table)
  "Return a lambda of one argument KEY that looks up KEY in TABLE."
  `(lambda (key) (ht-get ,table key)))

(defmacro ht-set-lambda (table)
  "Return a lambda of two arguments KEY and VALUE that sets KEY to VALUE in TABLE."
  `(lambda (key value) (ht-set ,table key value)))

(defmacro ht-contains-p-lambda (table)
  "Return a lambda of one argument KEY that tests whether KEY is present in TABLE."
  `(lambda (key) (ht-contains-p ,table key)))

(defmacro ht-get-function (table)
  (let ((name (symbol-name table)))
    `(defun ,(intern (format "ht-%s-get" name)) (key)
       "Get the value for KEY from the named hash table."
       (ht-get ,table key))))

(defmacro ht-set-function (table)
  (let ((name (symbol-name table)))
    `(defun ,(intern (format "ht-%s-set" name)) (key value)
       "Set KEY to VALUE in the named hash table, returning VALUE."
       (ht-set ,table key value))))

(defmacro ht-contains-p-function (table)
  (let ((name (symbol-name table)))
    `(defun ,(intern (format "ht-%s-contains-p" name)) (key)
       "Return non-nil if KEY is present in the named hash table."
       (ht-contains-p ,table key))))

(cl-defmacro ht-make-named-table (name &optional &key (test #'equal))
  "Define a named hash table variable and named accessor functions.
Creates `ht-NAME-get', `ht-NAME-set', and `ht-NAME-contains-p'. TEST defaults to `equal'."
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
  "Return the modification time of FILENAME as a time value."
  (file-attribute-modification-time (file-attributes filename)))

(defun f-atime (filename)
  "Return the access time of FILENAME as a time value."
  (file-attribute-access-time (file-attributes filename)))

(defun f-make-slug (s)
  "Turn a string, S, into a hyphen-seperated slug for filename."
  (downcase
   (replace-regexp-in-string
    "[^A-Za-z0-9]" "-"
    (string-clean-whitespace s))))

(defun f-filename-is-p (entry name)
  "Return non-nil if the filename component of path ENTRY equals NAME."
  (f-equal-p (f-filename entry) name))

(defun f-when-file-exists (path)
  "Return PATH if it exists on the filesystem, otherwise nil."
  (when (f-exists-p path)
    path))

(defun f-distinct (sequence)
  "Return SEQUENCE with duplicate paths removed using `f-equal-p' for comparison."
  (let ((-compare-fn #'f-equal-p))
    (-distinct sequence)))

(defmacro f-directories-containing-file-with-extension-function (extension)
  "Define `f-EXT-file-p' and `f-directories-containing-file-with-extension-EXT' for EXTENSION."
  (when (s-prefix-p "." extension)
    (setq extension (string-trim-left extension "^\\.")))

  `(progn
     (defun ,(intern (format "f-%s-file-p" (string-replace "." "" (downcase extension)))) (file)
       (and (f-file-p file) (f-ext-p file ,extension)))

     (defun ,(intern (format "f-directories-containing-file-with-extension-%s" (string-replace "." "" (downcase extension)))) (paths)
       (when (stringp paths)
	 (setq paths (list paths)))
       (->> paths
	    (--flat-map (f-entries it #'f-file-p))
	    (--filter (f-ext-p it ,extension))
	    (-map #'f-dirname)
	    (-distinct)))))

(defun f-files-in-directory (path)
  "Return a flat list of all files under PATH.
PATH may be a directory, a file (returns siblings), or a list of paths."
  (cond
   ((stringp path)
    (cond
     ((f-directory-p path) (f-entries path #'f-file-p))
     ((f-file-p path) (f-entries (f-dirname path) #'f-file-p))))
   ((listp path) (--flat-map (f-entries it #'f-file-p) path))))

(defun f-recursive-directories-containing (filename &optional path)
  "Return a list of directories under PATH that contain a file named FILENAME.
Searches recursively. PATH defaults to `default-directory' when nil."
  (->> (f-entries path (lambda (f) (f-filename-is-p f filename)) t)
       (-map #'f-dirname)))

(defmacro f-directories-containing-file-function (filename &rest files)
  "Define helper predicates and a search function for directories containing FILENAME.
Also accepts additional FILES as alternate names to match."
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
  "Replace the expanded home directory prefix in PATH with `~/'."
  (string-replace (expand-file-name "~/") "~/" path))

(defun f-visually-compress-path (num path)
  "Truncate each component of PATH to at most NUM characters, preserving the separator."
  (->> (f-split path)
       (--map (if (length> it num)
		  (substring it 0 num)
		it))
       (--replace-where (string-equal it (f-path-separator)) "")
       (s-join (f-path-separator))))

(defmacro f-visual-compression-function (num)
  "Define `f-visually-compress-to-NAME' that truncates each path component to NUM characters."
  `(defun ,(intern (concat "f-visually-compress-to-" (s-number-word num))) (path)
     (f-visually-compress-path ,num path)))

(defun f-filter-directories (options &rest sequence)
  "Return entries from SEQUENCE that are existing directories.
OPTIONS is a symbol or list: `cannonicalize' expands paths to absolute form,
`unique' deduplicates results via `f-equal-p'."
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

(f-visual-compression-function 1)
(f-visual-compression-function 2)
(f-visual-compression-function 3)
(f-visual-compression-function 4)
(f-visual-compression-function 5)
(f-visual-compression-function 6)
(f-visual-compression-function 7)
(f-visual-compression-function 8)
(f-visual-compression-function 9)
(f-visual-compression-function 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; options

(defun option-set-p (opt options)
  "Return non-nil if OPT is present in OPTIONS.
OPTIONS may be a single symbol or a list of symbols."
  (or (eq opt options)
      (and (listp options) (memq opt options))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; utility macros

(defmacro disabled (&rest body)
  "Wrap BODY so it never executes. Keeps code byte-compilable while effectively commenting it out."
  `(unless 'disabled
     ,@body))

(defmacro with-force-write (&rest body)
  "Temporarily clear `buffer-read-only', evaluate BODY, then restore it to t."
  (declare (indent defun) (debug t))
  `(prog1
       (progn
         (setq buffer-read-only nil)
         ,@body)
     (setq buffer-read-only t)))

(defmacro pos-arg (name &key is)
  "Allow positional arguments to have annotated call-sites."
  (declare (indent defun) (debug t))
  (unless (or (stringp name) (symbolp name))
    (user-error "cannot annotate a positional arg without a name"))
  is)

(defalias 'pa 'pos-arg)

(cl-defmacro add-one-shot-hook
    (&key name hook function result body form operation
	  ;; flags and options; with defaults
	  (args nil) (local nil) (persist nil) (count 1) (depth 0) (make-unique nil) (cleanup nil) (idle-timer nil))
  "Register a self-removing hook function named NAME on HOOK.
The hook body is specified via one of: FUNCTION (a symbol or lambda), FORM (an
expression), BODY (a list of forms), RESULT (evaluated at expansion time), or
OPERATION (called via funcall/apply with optional ARGS).

After firing COUNT times (default 1) the hook removes itself. Set PERSIST to t
to keep the hook active indefinitely. When IDLE-TIMER is a number, the body
runs in an idle timer of that many seconds rather than directly in the hook.

HOOK may be a symbol, a list of symbols, or the special sentinel
`after-first-frame-created', which routes to `window-setup-hook' in non-daemon
sessions and `server-after-make-frame-hook' in daemon sessions.

DEPTH controls hook insertion depth (default 0). LOCAL makes the hook
buffer-local. MAKE-UNIQUE generates an uninterned symbol to avoid name
collisions. CLEANUP uninterns the generated symbol after the hook fires."
  (let* ((unique-tag (or (when make-unique (gensym "hook-"))
			 (make-symbol "hook")))
	 (count-tag (cond (persist "perpeutal")
			  ((not (numberp count)) (user-error "must specify hook limited count as a number %d" count))
			  ((eq count 1) "one-shot")
			  (:else (format "run-%d-times" count))))
	 (cleanup-symbol (intern (s-join-with-hyphen "one-shot" count-tag name (symbol-name unique-tag))))
	 hooks)

    (when (or (eq hook 'after-first-frame-created)
              (equal hook '(quote after-first-frame-created)))
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

    (let* ((timer-name (format "<one-shot-hook> %s" name))
	   (filtered-hooks (--remove (eq 'quote it) hooks))
	   (resolved-form
	    (or (cond (form
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
		       (if args
			   `(apply ',function ,args)
			 `(funcall ',function)))
		      ((listp function)
		       function))
		(user-error "could not resolve the hook function from input for %s" name)))
	   (remove-hook-forms
	    (--map `(remove-hook ',it ',cleanup-symbol ,local) filtered-hooks))
	   (cleanup-expr
	    (if (or make-unique cleanup)
		`(unintern ',cleanup-symbol obarray)
	      t)))

      `(progn
	 (let ((count ,count)
	       (run-count 0))
	   (cl-flet ((counter-increment (lambda () (cl-incf run-count)))
		     (counter-expired (lambda () (and (not ,persist) (>= run-count count)))))

	     (defun ,cleanup-symbol ,args
	       ,@(if idle-timer
		     `((run-with-idle-timer ,idle-timer nil
					    (lambda ()
					      (with-slow-op-timer ,timer-name ,resolved-form)))
		       (counter-increment)
		       (when (counter-expired)
			 ,@remove-hook-forms
			 ,cleanup-expr))
		   `((with-slow-op-timer ,timer-name
		       ,resolved-form
		       (counter-increment)
		       (when (counter-expired)
			 ,@remove-hook-forms
			 ,cleanup-expr))))))

	   ,@(--map `(add-hook ',it ',cleanup-symbol ,depth ,local) filtered-hooks))))))

(defmacro make-run-hooks-function-for (mode)
  "Define a zero-argument function `run-hooks-for-MODE' that runs `MODE-hook'."
  (let* ((mode-name (symbol-name mode))
	 (hook-name (concat mode-name "-hook"))
	 (function-name (intern (concat "run-hooks-for-" mode-name))))
    `(defun ,function-name nil
       (run-hooks (intern ,hook-name)))))

(cl-defmacro create-toggle-functions (value &optional &key short-name local keymap key)
  "Define turn-on, turn-off, and toggle interactive commands for variable VALUE.
Use SHORT-NAME to override the generated name. LOCAL makes commands use `setq-local'.
Optionally bind the toggle to KEY in KEYMAP."
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
  "Define an interactive command that runs `execute-extended-command' filtered to PREFIX.
Only commands whose names begin with PREFIX are offered for completion.
Optionally bind the command to BIND-KEY in BIND-MAP with KEY-ALIAS as the which-key label."
  (declare (indent defun))
  (unless (setq prefix (s-trimmed-or-nil prefix))
    (user-error "cannot build predicate function for '%s'" prefix))
  (unless key-alias
    (setq key-alias (s-join-with-kebab prefix "commands")))

  (setq prefix (s-normalize-symbol-name prefix))

  (let* ((predicate-name (format "read-extended-command-for-%s-prefix-p" prefix))
	 (predicate-symbol (intern predicate-name))
	 (user-command-name (format "execute-extended-%s-command" prefix))
	 (user-command-symbol (intern user-command-name)))
    `(prog1
       (defun ,user-command-symbol ()
	 ,(format "Read extentend command but filtered for only those beginning with prefix `%s'." prefix)
	 (interactive)
	 (let ((read-extended-command-predicate #',predicate-symbol))
	   (execute-extended-command nil)))
       (defun ,predicate-symbol (command buffer)
	 ,(format "Predicate for `read-extended-command-predicate' to filter commands returning only those that start with the prefix `%s'" prefix)
	 (s-prefix-p ,prefix (symbol-name command)))
       ,(when bind-key
	  `(progn
	     (bind-keys
	      :map ,(or bind-map 'global-map)
	      (,bind-key . ,user-command-symbol))
	     ,(when key-alias
		`(which-key-add-keymap-based-replacements ,(or bind-map 'global-map) ,bind-key ,key-alias)))))))

(defmacro with-toggle-once (name &rest body)
  "Define a function NAME that executes BODY only the first time it is called.
Subsequent calls are no-ops. Uses an auto-generated toggle variable to guard execution."
  (declare (indent defun) (debug t))
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
  "Evaluate BODY with `current-prefix-arg' bound to ARG."
  `(let ((current-prefix-arg ,arg))
     ,@body))

(defmacro with-default-directory (path &rest body)
  "Run the body with `default-directory' set to the path provided"
  (declare (indent defun) (debug t))
  `(let ((default-directory ,path))
     ,@body))

(defmacro with-silence (&rest body)
  "Totally suppress message from either the minibuffer or the *Messages* buffer.."
  (declare (indent defun) (debug t))
  `(let ((inhibit-message t)
         (message-log-max nil))
     ,@body))

(defmacro with-quiet (&rest body)
  "Suppress any messages from appearing in the minibuffer area."
  (declare (indent defun) (debug t))
  `(let ((inhibit-message t))
     ,@body))

(defmacro with-temp-keymap (map &rest body)
  "Bind MAP to a fresh sparse keymap, evaluate BODY, and return the keymap.
Use this to build a keymap programmatically and return it in one expression."
  (declare (indent defun) (debug t))
  `(let ((,map (make-sparse-keymap)))
     ,@body
     ,map))

(cl-defmacro setq-when-nil (variable value &optional &key local)
  "Set VARIABLE to VALUE only if VARIABLE is currently nil. Use :local t for `setq-local'."
  `(unless ,variable
     (,(if local 'setq-local 'setq) ,variable ,value)))

(defmacro with-timer (name &rest body)
  "Report on NAME and the time taken to execute BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%s: %.06fs" ,name (float-time (time-since time)))))

(defun compile-buffer-name (name)
  "Return a function suitable for `compilation-buffer-name-function' that always returns NAME.
The returned function accepts one optional argument (the compilation mode) and ignores it."
  (lambda (&optional _) name))

(defmacro merge-predicate-functions (&rest preds)
  "Return a lambda that applies each predicate in PREDS to its argument with `and'.
Short-circuits on the first predicate that returns nil, consistent with `and' semantics."
  `(lambda (value)
     (and ,@(mapcar (lambda (p) `(funcall #',p value)) preds))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; project context -- lightweight wrappers over projectile / project.el

(declare-function projectile-project-root "projectile")
(declare-function projectile-project-name "projectile")
(declare-function projectile-project-buffers "projectile")

(declare-function project-root "project")
(declare-function project-current "project")
(declare-function project-buffers "project")

(defun buffer-derived-mode-p (buffer mode)
  (with-current-buffer buffer
    (when (derived-mode-p mode)
      t)))

(cl-defun mode-buffers-for-project (&optional &key (mode major-mode))
  "Return buffers in the current project whose major mode derives from MODE.
MODE defaults to `major-mode' of the calling buffer. Uses `approximate-project-buffers'
to determine project membership."
  (->> (approximate-project-buffers)
       (--filter (buffer-derived-mode-p it mode))))

(cl-defun mode-buffers (&optional (mode major-mode))
  "Return all live buffers whose major mode derives from MODE.
MODE defaults to `major-mode' of the calling buffer. Searches across all buffers,
not just the current project."
  (->> (buffer-list)
       (--keep
	(with-current-buffer it
	  (when (derived-mode-p mode)
	    (current-buffer))))))

(defun package-available-p (name)
  "Return non-nil if the feature NAME is currently loaded via `featurep'.
Use this to guard optional integrations with packages that may not be present."
  (featurep name))

(defun approximate-project-root ()
  "Return the current project root, falling back to `default-directory'."
  (or (when (package-available-p 'projectile)
        (s-trimmed-or-nil (projectile-project-root)))
      (when (and (featurep 'project) (project-current))
        (project-root (project-current)))
      (expand-file-name default-directory)))

(defun approximate-project-name ()
  "Return the current project name, falling back to the directory basename."
  (s-trim-non-word-chars
   (or (when (package-available-p 'projectile)
         (projectile-project-name))
       (when (project-current)
         (project-root (project-current)))
       (f-filename (expand-file-name default-directory)))))

(defun approximate-project-buffers ()
  "Return buffers belonging to the current project."
  (or (when (package-available-p 'projectile)
        (projectile-project-buffers))
      (when (and (featurep 'project) (project-current))
        (project-buffers (project-current)))
      (let ((directory (expand-file-name default-directory)))
        (->> (buffer-list)
             (--filter
	      (with-current-buffer it
                (file-in-directory-p default-directory directory)))))))

(provide 'xtdlib)
;;; xtdlib.el ends here
