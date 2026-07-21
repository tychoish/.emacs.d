;;; builder.el --- compilation buffer and command runner -*- lexical-binding: t -*-

;; Author: tychoish
;; Maintainer: tychoish
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, compilation, build
;; URL: https://github.com/tychoish/dot-emacs

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

;; Provides interactive compilation buffer selection and project-aware
;; command discovery for build and test workflows.  Candidates are
;; registered with `builder-register' and dispatched via `builder-run',
;; the primary entry point.  Project root detection and language-specific
;; command discovery are handled automatically based on the files present
;; in the project tree.

;;; Code:

(require 'compile)

(require 'f)
(require 'seq)
(require 'subr-x)
(require 'map)

(eval-when-compile
  (require 'xtd-macro))
(require 'xtd-project)
(require 'xtd-s)
(require 'annotated-completing-read)
(require 'eglot-test-at-point)

(declare-function approximate-project-root "xtd-project")
(declare-function approximate-project-name "xtd-project")
(declare-function approximate-project-buffers "xtd-project")
(declare-function mode-buffers "xtd-project")
(declare-function mode-buffers-for-project "xtd-project")
(declare-function compile-buffer-name "xtd-project")
(declare-function s-join-with-space "xtd-s")
(declare-function s-join-with-pipe "xtd-s")
(declare-function s-join-with-kebab "xtd-s")
(declare-function s-join-with-hyphen "xtd-s")
(declare-function s-shortest "xtd-s")
(declare-function s-trimmed-or-nil "xtd-s")

(declare-function lm-header "lisp-mnt")
(declare-function package-build-archive "package-build")
(defvar package-build-working-dir)
(defvar package-build-archive-dir)
(defvar package-build-recipes-dir)
(defvar package-build-verbose)
(defvar package-build-releases)

(f-directories-containing-file-with-extension-function "go")
(f-directories-containing-file-with-extension-function "py")
(f-directories-containing-file-with-extension-function "rs")
(f-directories-containing-file-with-extension-function "el")

(f-directories-containing-file-with-extension-function "md")
(f-directories-containing-file-with-extension-function "txt")
(f-directories-containing-file-with-extension-function "rst")
(f-directories-containing-file-with-extension-function "org")
(f-directories-containing-file-with-extension-function "mdwn")

(f-directories-containing-file-function "Cargo.toml")
(f-directories-containing-file-function "go.mod")
(f-directories-containing-file-function "justfile")
(f-directories-containing-file-function "makefile" "Makefile" "GNUmakefile")
(f-directories-containing-file-function "pyproject.toml")

;;;###autoload
(defun builder ()
  "Run compile operation selecting compile buffer and commands."
  (interactive)
  (builder-compile-project))

(cl-defun builder--compile-post-hook
    (buffer compile-result-message started-at &optional &key process-name program args send-when
     (alert-threshold 60)
     (notification-threshold 120))
  (let* ((end-at (current-time))
	 (duration (time-subtract end-at started-at))
	 (compile-result-message (string-trim compile-result-message))
	 (msg (format "build %s in %.06fs -- %s"
		      compile-result-message
		      (float-time duration)
		      (with-current-buffer buffer
		        (string-trim (car compilation-arguments))))))

    (when (or send-when
	      (> alert-threshold (float-time (time-since (current-idle-time))))
	      (and (> (float-time duration) notification-threshold) process-name program args))

      (apply #'async-start-process
	     (pa "emacs-process-name" :is process-name)
	     (pa "program" :is program)
	     (pa "on-finish" :is (lambda (out) (message "INFO: notify process for %s completed [%s] with %s" (buffer-name buffer) out compile-result-message)))
	     (append args (-strings (format "<%s> %s -- %s" sprite-instance-id (buffer-name buffer) msg)))))

    (when (or send-when
	      (> (/ alert-threshold 2) (float-time (time-since (current-idle-time))))
	      (> alert-threshold (ffloor (float-time duration))))
      (alert
       msg
       :title (format "%s:%s:%s" sprite-instance-id (buffer-name buffer) compile-result-message)
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

(defun builder-reset-finish-hooks ()
  (interactive)
  (seq-do (lambda (it) (with-current-buffer it
		 (setq-local compilation-finish-functions nil)))
	     (mode-buffers 'compilation-mode)))

(defun builder--buffer-for-command (command)
  "Return the name of a project compilation buffer that last ran COMMAND, or nil."
  (when-let* ((buf (seq-find (lambda (it) (with-current-buffer it
				     (string-equal (string-trim (or (car compilation-arguments) ""))
						    (string-trim command))))
			 (mode-buffers-for-project :mode 'compilation-mode))))
    (buffer-name buf)))

(defun builder--buffer-ran-command-p (buffer command)
  "Return non-nil if BUFFER last ran COMMAND."
  (with-current-buffer buffer
    (string-equal (string-trim (or (car compilation-arguments) ""))
		  (string-trim command))))

;;;###autoload
(defun builder-compile-project (&optional name command)
  (let* ((project-root-directory (approximate-project-root))
	 (start-at (current-time))
	 ;; Step 1: select and confirm command
	 (cc-result (builder--read-command command))
	 (candidate-name (car cc-result))
	 (candidates (cdr cc-result))
	 (candidate (map-elt candidates candidate-name))
	 (compile-command (read-from-minibuffer "edit command => " (builder-candidate-command candidate)))
	 (op-default-directory (or (builder-candidate-directory candidate) default-directory))
	 ;; Step 2: select buffer, pre-seeding the one that last ran this command
	 (prior-buf-name (builder--buffer-for-command compile-command))
	 (compilation-buffer-candidates (builder--project-compilation-buffers :name name))
	 (op-name (annotated-completing-read
		   compilation-buffer-candidates
		   :prompt "compilation buffer => "
		   :require-match nil
		   :initial-input prior-buf-name))
	 (compile-buf (get-buffer op-name)))

    (save-some-buffers
     t
     (lambda () (let ((filename (buffer-file-name)))
		  (and filename (file-in-directory-p filename project-root-directory)))))

    ;; Step 3: recompile if buffer already ran this command; prefix arg forces new compilation
    (let ((active-buf
	   (if (and compile-buf
		    (not current-prefix-arg)
		    (builder--buffer-ran-command-p compile-buf compile-command))
	       (progn (with-current-buffer compile-buf (recompile)) compile-buf)
	     (let ((default-directory op-default-directory))
	       (compilation-start
		compile-command
		'compilation-mode
		(compile-buffer-name op-name))))))

      (with-current-buffer active-buf
	(add-one-shot-hook :name (format "%s notification hook" op-name)
	 :hook 'compilation-finish-functions
	 :local t
	 :make-unique t
	 :args (compilation-buffer msg)
	 :form (builder--compile-post-hook
		compilation-buffer
		msg
		start-at
		:process-name "sardis-notify"
		:program "sardis"
		:args '("notify" "send")
		:send-when (or current-prefix-arg
			       (not (get-buffer-window active-buf t))
			       (< 10 (float-time (time-since (current-idle-time)))))))

	(when-let* ((candidate (map-elt candidates candidate-name))
		    (hook (builder-candidate-hook candidate)))
	  (when hook
	    (add-one-shot-hook :name (format "post-%s-hook-operation" op-name)
	     :hook 'compilation-finish-functions
	     :make-unique t
	     :local t
	     :args (compilation-buffer msg)
	     :form (funcall hook compilation-buffer msg))))

	(let ((op-window (get-buffer-window (current-buffer) (selected-frame))))
	  (if op-window (select-window op-window)
	    (switch-to-buffer-other-window (current-buffer))))))))

;;;###autoload
(defun builder-change-directory ()
  "Change the directory for the current compilation buffer."
  (interactive)
  (unless (derived-mode-p 'compilation-mode)
    (user-error "operation is only applicable for COMPILATION-MODE buffers"))
  (let ((directory (or (annotated-completing-read-directory
                        :candidates (seq-uniq
                                     (append (list compilation-directory default-directory)
                                             (annotated-completing-read--directory-default-candidates)
                                             (f-directories (approximate-project-root))))
                        :prompt "in directory =>> ")
                       compilation-directory
                       default-directory)))
    (setq-local default-directory directory
                compilation-directory directory)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; builder-compile-project implementation

(defun builder--candidate-priority (candidates name)
  "Return the priority of candidate NAME in CANDIDATES, defaulting to 2."
  (or (when-let* ((c (map-elt candidates name)))
	(builder-candidate-priority c))
      2))

(defun builder--make-sort-fn (candidates history-key)
  "Return a display-sort-function for `annotated-completing-read'.
Candidates that appear in HISTORY-KEY's history list (stored in
`annotated-completing-read-history') sort first by recency (lowest index =
most recent).  Candidates not in history are then ordered by structural
priority from CANDIDATES, with key length as a final tiebreaker."
  (lambda (items)
    (let* ((hist (map-elt annotated-completing-read-history history-key))
           (hist-rank (lambda (name)
                        (or (cl-position name hist :test #'equal)
                            most-positive-fixnum))))
      (sort (copy-sequence items)
        (lambda (a b)
          (let ((ha (funcall hist-rank a))
                (hb (funcall hist-rank b)))
            (cond
             ((< ha hb) t)
             ((> ha hb) nil)
             (t (let ((pa (builder--candidate-priority candidates a))
                      (pb (builder--candidate-priority candidates b)))
                  (or (< pa pb)
                      (and (= pa pb) (< (length a) (length b)))))))))))))

(defun builder--read-command (&optional command table)
  (let* ((candidates (or table
			 (builder--get-candidates default-directory command)))
         (annotation-table (let ((tbl (make-hash-table :test #'equal)))
                             (map-do (lambda (name cand)
                                        (map-put! tbl name (builder-candidate-annotation cand)))
                                      candidates)
                             tbl))
         (selection-name
          (annotated-completing-read
           annotation-table
           :prompt "compile command => "
           :require-match nil
           :history 'compile-history
           :sort-fn (builder--make-sort-fn candidates 'compile-history)))
	 (candidate (or (map-elt candidates selection-name)
			(let ((new-candidate (make-builder-candidate
					       :command selection-name
					       :directory default-directory
					       :annotation "user input")))
			  (builder--add-candidate candidates new-candidate)
			  new-candidate)))
	 (operation-name (builder-candidate-name candidate)))
    (cons operation-name candidates)))

(cl-defun builder--project-compilation-buffers (&optional &key name (project (approximate-project-name)))
  "Return a hash table of candidate compilation buffer names for PROJECT."
  (let ((buffer-table (make-hash-table :test #'equal))
        (default-names (list "push" "gen" "buf" "benchmark" "check" "compile" "run" "lint" "test" "build")))
    (when name
      (cl-pushnew name default-names :test #'equal))

    (dolist (it (mode-buffers-for-project :mode 'compilation-mode))
      (with-current-buffer it
        (map-put!
         buffer-table
         (buffer-name it)
         (format "reuse buffer: project=%s errors=%s lines=%s command='%s'"
                 project
                 compilation-num-errors-found
                 (buffer-line-count)
                 (string-trim (or (car compilation-arguments) ""))))))

    (dolist (it default-names)
      (let ((name (format "*%s-%s*" project it)))
        (if (map-contains-key buffer-table name)
            (map-put!
             buffer-table
             (generate-new-buffer-name name)
             (format "new %s buffer for project %s" name project))
          (map-put!
           buffer-table
           name
           (format "create default compilation buffer for %s" project)))))

    buffer-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; compilation candidate struct and table operations

(cl-defstruct (builder-candidate
               (:constructor nil)
               (:constructor make-builder-candidate
			     (&key command
				   (name (if (> (length command) 32) (concat (substring command 0 29) "...") command))
				   (directory default-directory)
				   (annotation (if (not (string-equal command name))
						   (format "'%s' in %s" command (f-abbrev directory))
						 command))
				   hook
				   notification-threshold
				   (priority 2))))
  "Structure for compilation candidates."
  (name
   "build"
   :documentation "name of command, default to using the command"
   :type string)
  (command
   "test"
   :documentation "shell command to run in a compile buffer."
   :type string)
  (directory
   default-directory
   :documentation "directory in which to run the command."
   :type string)
  (annotation
   "compilation command"
   :documentation "description of command, used for annotations."
   :type string)
  (hook
   nil
   :documentation "a function to run when the compilation completes"
   :type function)
  (notification-threshold
   30
   :documentation "if a command runs longer than this number of seconds, send a notification when the compile completes."
   :type integer)
  (priority
   2
   :documentation "display sort priority: 0=current-file, 1=rerun, 2=project, 3=history (lower = higher priority)"
   :type integer))

(defun builder--add-candidate (table candidate)
  (map-put! table (builder-candidate-name candidate) candidate))

(defun builder--add-candidates (table candidates)
  (seq-do (lambda (it) (builder--add-candidate table it))
	   (seq-filter #'builder-candidate-p candidates)))

(defvar builder-candidate-functions nil
  "List of functions that populate a table of possible compilation commands.
Each function is called with four arguments:
  PROJECT-ROOT-DIRECTORY — absolute path to the project root.
  PROJECT-NAME           — short name of the project (directory basename).
  DIRECTORIES            — list of directories derived from open project
                           buffers; this is what drives language-specific
                           discovery (e.g. finding go.mod or Cargo.toml files
                           near files the user actually has open).
  TABLE                  — a `ht' hash table of `builder-candidate' objects;
                           functions should call `builder--add-candidates' to
                           populate it and return t.")

(defun builder-add-candidates (fn)
  "Register FN as a candidate generator on `builder-candidate-functions'.
FN is called with (ROOT NAME DIRS TABLE): ROOT is the project root path, NAME
is the project name, DIRS is the set of open-buffer directories that drives
language-specific discovery, and TABLE is a `ht' of `builder-candidate'
objects.  FN should call `builder--add-candidates' to populate TABLE and
return t.

This is the plain-function alternative to `builder-register-candidates'.
Use it when you have a plain `defun' and do not need predicate filtering,
named-symbol generation, or mode-hook scoping."
  (add-hook 'builder-candidate-functions fn))

(cl-defmacro builder-register-candidates (&key name (predicate t) (hooks nil) pipeline)
  "Define a named candidate generator and register it on `builder-candidate-functions'.
NAME is used to derive the generated function symbol
`builder-candidates-for-NAME'.  PREDICATE gates execution (default t).
PIPELINE is an expression that evaluates to a list of `builder-candidate'
objects; it runs inside the generated function with PROJECT-ROOT-DIRECTORY,
PROJECT-NAME, DIRECTORIES, and OPERATION-TABLE bound.

When HOOKS is nil the generator is registered globally via
`builder-add-candidates'.  When HOOKS is a list of hook symbols the generator
is registered buffer-locally when any of those hooks fires, so candidates only
appear for buffers of the relevant major mode.

For simple cases that do not need predicate filtering or mode scoping, prefer
the plain-function API: write a `defun' with the four-argument protocol and
call `builder-add-candidates'."
  (let ((symbol-name (intern (format "builder-candidates-for-%s" name)))
	(hook-registering-function-name (intern (format "builder-candidate-registrar-for-%s" name))))
    `(progn
       (defun ,symbol-name (project-root-directory project-name directories operation-table)
	 ,(format "Candidate generator for `%s'; see `builder-register-candidates'." name)
	 (ignore project-root-directory project-name directories operation-table)
	 (when ,predicate
	   (builder--add-candidates
	    operation-table
	    ,pipeline))
	 t)

       ,(if (eql 0 (length hooks))
	    `(builder-add-candidates #',symbol-name)
	  `(let ((hooks ,hooks))
	     (defun ,hook-registering-function-name ()
	       (add-hook 'builder-candidate-functions #',symbol-name 0 t))

	     (seq-do (lambda (it) (add-hook it ',hook-registering-function-name)) hooks))))))

(defvar-local builder--cached-candidates nil)

(defvar builder--cache-bypass nil
  "Disables the compilation candidate cache (per-buffer).")

(create-toggle-functions builder--cache-bypass)

(defun builder--get-candidates (&optional directory command)
  "Generate candidate compilation commands based on mode and directory structure."
  (when current-prefix-arg
    (builder--clear-candidate-cache))

  (unless (or builder--cache-bypass
	      builder--cached-candidates)
    (let* ((project-root-directory (approximate-project-root))
	   (project-name (file-name-nondirectory project-root-directory))
	   (default-directory (or directory default-directory))
           (directories (seq-uniq
			    (append
			     (annotated-completing-read--directory-parents default-directory project-root-directory)
			     (seq-map #'f-full
				      (seq-filter #'file-directory-p
						 (seq-keep #'buffer-directory (approximate-project-buffers)))))))
	   (operation-table (make-hash-table :test #'equal)))

      (when command
	(builder--add-candidate
	 operation-table
	 (make-builder-candidate
	  :name (if (> (length command) 32) (concat (substring command 0 29) "...") command)
	  :command command
	  :annotation "runtime suggested candidate"
	  :directory (or directory project-root-directory default-directory))))

      (run-hook-with-args
       'builder-candidate-functions
       project-root-directory
       project-name
       directories
       operation-table)

      (setq-local builder--cached-candidates operation-table)))

  builder--cached-candidates)

(cl-defun builder--candidate-cache-p (&optional (buffer (current-buffer)))
  (with-current-buffer buffer
    (when builder--cached-candidates t)))

(defun builder-clear-all-caches ()
  (interactive)
  (seq-do #'builder--clear-candidate-cache
	   (seq-filter #'builder--candidate-cache-p (buffer-list))))

(cl-defun builder-clear-cache (&optional (buffer (current-buffer)))
  (interactive)
  (if current-prefix-arg
      (builder-clear-all-caches)
    (builder--clear-candidate-cache buffer)))

(cl-defun builder--clear-candidate-cache (&optional (buffer (current-buffer)))
  (with-current-buffer buffer
    (setq-local builder--cached-candidates nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; candidate discovery/generator registration

(builder-register-candidates
 :name "current-buffer-text-file"
 :predicate (when (and (buffer-file-name)
		       (derived-mode-p '(org-mode markdown-mode rst-mode)))
	      (save-buffer) t)
 :pipeline (let ((filename (buffer-file-name)))
	     (list (make-builder-candidate
		  :directory default-directory
		  :command (format "vale --output=line %s" filename)
		  :name (format "vale check %s" (f-collapse-homedir filename))
		  :annotation "find errors in the current file"
		  :priority 0
		  :hook (lambda (buffer msg) (builder--vale-insert-statistics buffer msg :filename filename)))
		 (make-builder-candidate
		  :directory default-directory
		  :command (format "vale --ls-metrics %s" filename)
		  :name (format "vale report %s" (f-collapse-homedir filename))
		  :annotation "report statistics about the current file"
		  :priority 0))))

(builder-register-candidates
 :name "minibuffer-shell-commands"
 :pipeline (seq-map
		 (lambda (it)
		   (let ((command (car it))
			 (source (cdr it))
			 (annotation-tag (if (string-equal default-directory project-root-directory) "project root" "working directory")))
		     (make-builder-candidate
		      :name (if (> (length command) 32) (concat (substring command 0 29) "...") command)
		      :command command
		      :directory project-root-directory
		      :annotation (format "operation from `%s' in the %s (%s)" source annotation-tag default-directory)
		      :priority 3)))
		 (-join (seq-map (lambda (it) (cons it "minibuffer-history")) (minibuffer-default-add-shell-commands))
			(seq-map (lambda (it) (cons it "shell-command-history")) shell-command-history))))

(builder-register-candidates
 :name "project-compilation-buffer-commands"
 :pipeline (seq-mapcat
		 (lambda (it)
		   (let ((command (car it))
			 (buf (cdr it)))
		     (seq-map (lambda (it)
				(let ((operation-directory it))
				  (make-builder-candidate
				   :name (format "rerun from %s [%s]" buf (f-visually-compress-to-five operation-directory))
				   :command command
				   :directory operation-directory
				   :annotation (format "command '%s' from %s in %s" command buf operation-directory)
				   :priority 1)))
			      (seq-filter #'identity
					 (seq-uniq
					  (list project-root-directory (buffer-directory buf) default-directory))))))
		 (seq-keep
		  (lambda (it) (with-current-buffer it
				 (when-let* ((command (s-trimmed-or-nil (car compilation-arguments))))
				   (cons command it))))
		  (mode-buffers-for-project :mode 'compilation-mode))))

(builder-register-candidates
 :name "makefiles"
 :pipeline (seq-mapcat
		 (lambda (it)
		   (let* ((directory (alist-get 'directory it))
			  (target (alist-get 'target it))
			  (is-current-directory (f-equal-p default-directory directory))
			  (is-project-root (f-equal-p project-root-directory directory))
			  (display-directory (s-shortest
					     (string-remove-suffix project-root-directory directory)
					     (if is-project-root
						 (concat "<" project-name ">")
					       (f-collapse-homedir directory))))
			  (command-template (if is-current-directory
					       (format "make %%s%s" target)
					     (format "make %%s-C %s %s" directory target)))
			  (name-template (if is-current-directory
					    command-template
					  (string-replace directory display-directory command-template)))
			  (annotation-template (cond
					       (is-project-root (format "build target %s in project root (%s)" target display-directory))
					       (is-current-directory (format "build target %s in current directory (%s)" target display-directory))
					       (t (format "build target %s in %s" target display-directory)))))
		     (seq-map (lambda (it)
				(let ((flag (car it))
				      (annotation-suffix (cdr it)))
				  (make-builder-candidate
				   :name (format name-template flag)
				   :command (format command-template flag)
				   :directory directory
				   :annotation (s-join-with-space annotation-template annotation-suffix))))
			      '((""	  "")
				("-k "	  ", continuing on error")
				("-B "	  ", unconditionally")
				("-k -B " ", unconditionally while continuing on error")))))
		 (seq-mapcat
		  (lambda (it)
		    (let* ((default-directory it)
			   (makefile-report (process-lines "bash" "-c" "make --dry-run --print-data-base | grep -E '^[a-zA-Z0-9_-]+:' | sed 's/:.*//'")))
		      (unless (string-prefix-p "make: ***" (car makefile-report))
			(seq-filter (lambda (it) (and it (alist-get 'directory it) (alist-get 'target it)))
				    (seq-map (lambda (it)
					      (let ((target it))
						`((target . ,target)
						  (directory . ,default-directory))))
					    makefile-report)))))
		  (f-directories-containing-file-makefile directories))))

(builder-register-candidates
 :name "go-packages"
 :pipeline (seq-mapcat
		 (lambda (it)
		   (let* ((prefix (concat "." (f-path-separator)))
			  (directory it)
			  (operation-directory (cond
				((or (f-equal-p directory project-root-directory) (f-directory-contains-go-mod-file directory)) "./")
				((string-prefix-p prefix directory) directory)
				((string-prefix-p (f-path-separator) directory) directory)
				(t (concat prefix directory))))
			  (operation-directory-tree (f-join operation-directory "..."))
			  (operation-directory-tree (if (string-equal "..." operation-directory-tree) "./..." operation-directory-tree))
			  (short-path (f-collapse-homedir operation-directory))
			  (package-path (string-replace project-root-directory "" operation-directory))
			  (path-for-project-tag (cond ((f-equal-p operation-directory project-root-directory) "")
						      ((equal short-path package-path) "")
						      (t package-path)))
			  (proj-path-for-name (s-join-with-space (format "<%s>/%s" project-name
									 (f-visually-compress-to-five path-for-project-tag)))))
		     (append
		      (-l (make-builder-candidate
			   :name (s-join-with-space "go build" (f-join proj-path-for-name "..."))
			   :command (s-join-with-space "go build" operation-directory-tree)
			   :directory directory
			   :annotation (s-join-with-space "build in" project-name "at" short-path "recursively"))
			  (make-builder-candidate
			   :name (s-join-with-space "go build +tests" (f-join proj-path-for-name "..."))
			   :command (s-join-with-space "go test -run=NOOP" operation-directory-tree)
			   :directory directory
			   :annotation (s-join-with-space "build in (including tests) for" project-name "at" short-path "recursively"))
			  (make-builder-candidate
			   :name (s-join-with-space "go test +race +coverage +gaps" proj-path-for-name)
			   :directory directory
			   :command (s-join-with-space
				     "go test -coverprofile=coverage.out -race" operation-directory ";"
				     "go tool cover -html=coverage.out -o=coverage.html;"
				     (s-join-with-pipe
				      "go tool cover -func=coverage.out"
				      "sed -r \"$(go list -f='s%{{.ImportPath}}%{{.Dir}}%')\""
				      "grep -v '100.0%'"
				      "column -t;"))
			   :annotation (s-join-with-space "collect and report coverage data for" short-path)))
		      (seq-mapcat
		       (lambda (it)
			 (let ((command-prefix (car it))
			       (annotation-prefix (cadr it)))
			   (seq-map
			    (lambda (it)
			      (let* ((timeout-spec it)
				     (is-default (equal timeout-spec ""))
				     (timeout-arg (unless is-default (concat "--timeout=" timeout-spec)))
				     (timeout-name (if is-default "no timeout" (s-join-with-space "a" timeout-spec "timeout"))))
				(make-builder-candidate
				 :name (s-join-with-space command-prefix timeout-spec (f-join proj-path-for-name "..."))
				 :command (s-join-with-space command-prefix timeout-arg operation-directory-tree)
				 :directory directory
				 :annotation (format "run go test in %s recursively with %s and %s" short-path annotation-prefix timeout-name))))
			    '("10s" "30s" "1m"))))
		       '(("go test -cover"  "the code coverage collector")
			 ("go test -race"   "the race detector")
			 ("go test"         "default options")))
		      (seq-map
		       (lambda (it)
			 (make-builder-candidate
			  :name (format "lint %s %s" it proj-path-for-name)
			  :command (format "golangci-lint run --enable-only=%s %s" it operation-directory)
			  :directory directory
			  :annotation (format "run (only) the %s linter in %s" it short-path)))
		       '("revive" "makezero" "exhaustruct")))))
		 (seq-uniq
		  (seq-filter #'identity
			      (append
			       (f-directories-containing-file-with-extension-go directories)
			       (f-directories-containing-file-go-mod project-root-directory))))))
(builder-register-candidates
 :name "go-files"
 :pipeline (when-let* ((filename (if (and (buffer-file-name) (derived-mode-p '(go-mode go-ts-mode)))
				     (buffer-file-name)
				   (thing-at-point 'filename)))
		       (exists (file-exists-p filename))
		       (is-golang (string-equal (file-name-extension filename) "go"))
		       (short-filename (f-collapse-homedir filename))
		       (basename (file-name-nondirectory filename)))
	     (-l (make-builder-candidate
		  :name (format "gofumpt +extra %s" basename)
		  :directory (f-dirname filename)
		  :command (format "gofumpt -extra -w %s" filename)
		  :annotation (format "run gofumpt with non-extra constraints on %s" short-filename)
		  :priority 0)
		 (make-builder-candidate
		  :name (format "gofumpt %s" basename)
		  :directory (f-dirname filename)
		  :command (format "gofumpt -w %s" filename)
		  :annotation (format "run gofumpt with standard gofmt constraints on %s" short-filename)
		  :priority 0)
		 (make-builder-candidate
		  :name (format "gci %s <import sort>" basename)
		  :directory (f-dirname filename)
		  :command (format "golangci-lint fmt --enable gci %s" filename)
		  :annotation (format "sort imports with gci for %s" short-filename)
		  :priority 0)
		 (make-builder-candidate
		  :name (format "meta fmt %s" basename)
		  :directory (f-dirname filename)
		  :command (format "golangci-lint fmt %s" filename)
		  :annotation (format "run meta formatter on %s" short-filename)
		  :priority 0))))

(builder-register-candidates
 :name "go-test-file"
 :pipeline
 (when-let* ((filename  (buffer-file-name))
             (_         (derived-mode-p '(go-mode go-ts-mode)))
             (_         (string-suffix-p "_test.go" filename))
             (directory (f-dirname filename))
             (basename  (file-name-nondirectory filename))
             (short-path (f-collapse-homedir directory))
             (test-names (eglot-test-at-point--go-names-in-buffer)))
   (seq-mapcat
    (lambda (it)
      (let* ((test-name (car it))
             (is-bench  (eq (cdr it) 'benchmark))
             (run-flags (if is-bench
                            (format "-bench=^%s$ -run=^$" test-name)
                          (format "-run=^%s$" test-name)))
             (type-name (if is-bench "benchmark" "test")))
        (seq-mapcat
         (lambda (it)
           (let ((race-flag  (car it))
                 (race-label (cadr it)))
             (seq-map
              (lambda (it)
                (let ((timeout-flag (format "-timeout=%s" it))
                      (timeout-label it))
                  (make-builder-candidate
                   :name       (s-join-with-space "go test -v" race-label timeout-label test-name basename)
                   :command    (s-join-with-space "go test -v" race-flag timeout-flag run-flags ".")
                   :directory  directory
                   :annotation (format "run %s %s in %s" type-name test-name short-path)
                   :priority   0)))
              '("10s" "30s" "1m"))))
         '(("" "")
           ("-race" "+race")))))
    test-names)))
(builder-register-candidates
 :name "go-modules"
 :pipeline (let ((go-mod-directories (f-directories-containing-file-go-mod directories)))
	     (seq-mapcat
	      (lambda (it)
		(let ((filename (file-name-nondirectory it))
		      (directory it))
		  (seq-filter
		   #'identity
		   (seq-map
		    (lambda (it)
		      (let ((name (if (car it) (car it) (cadr it)))
			    (command (cadr it))
			    (annotation (caddr it)))
			(make-builder-candidate
			 :name name
			 :command command
			 :directory directory
			 :annotation (s-join-with-space annotation filename))))
		    (append
		     '(("lint run"         "golangci-lint run"        "run `golangci-lint' in package")
		       ("lint fix"         "golangci-lint run --fix"  "run `golangci-lint' and fix trivial errors in package")
		       (nil                "go mod tidy"              "run `go mod tidy' in package")
		       (nil                "go doc -all"              "go doc for entire package")
		       ("go doc -outline"  "go doc --"                "go doc outline for package")
		       ("<pkgs> | xargs go test +race +coverage"
			    "go list -f '{{ if (or .TestGoFiles .XTestGoFiles) }}{{ .ImportPath }}{{ end }}' ./... | xargs --verbose go test -race -cover"
			    "run all tests (with the race detector) for all submodules of")
		       ("<pkgs> | xargs go build +test ./..."
			    "go list -f '{{ if (or .TestGoFiles .XTestGoFiles) }}{{ .ImportPath }}{{ end }}' ./... | xargs --verbose go build"
			   "build all tests in all packages and sub-packages for"))
		     (unless (eql 1 (length go-mod-directories))
		       '(("<mod> | xargs go test -race"
			       "find . -name 'go.mod' | xargs --verbose -I{} bash -c 'pushd $(dirname {}); go test -race -cover ./...'"
			      "run tests for all modules and submodules in")
			 ("<mod> | xargs go build ./..."
			       "find . -name 'go.mod' | xargs --verbose -I{} bash -c 'pushd $(dirname {}); go build ./...'"
			      "run builds for all modules and submodules in"))))))))
	      go-mod-directories)))
(builder-register-candidates
 :name "py-projects"
 :pipeline (seq-mapcat
	     (lambda (it)
	       (list
		(make-builder-candidate
		 :command "ruff check"
		 :directory it
		 :annotation (format "run `ruff check' in package %s" it))
		(make-builder-candidate
		 :name "ruff check fix"
		 :command "ruff check --fix --show-fixes"
		 :directory it
		 :annotation (format "run `ruff check' and automatically fix lint violations in package %s" it))
		(make-builder-candidate
		 :name "ruff check fix +unsafe"
		 :command "ruff check --fix --unsafe-fixes --show-fixes"
		 :directory it
		 :annotation (format "render all unsafe fixes (which may modify the intent of the code) in package %s" it))
		(make-builder-candidate
		 :name "ruff format"
		 :command "ruff format --respect-gitignore"
		 :directory it
		 :annotation (format "run `ruff format' in package %s" it))
		(make-builder-candidate
		 :command "ruff analyze"
		 :directory it
		 :annotation (format "run `ruff analyze' in package %s" it))))
	     (f-directories-containing-file-pyproject-toml directories)))

(builder-register-candidates
 :name "python"
 :pipeline (seq-mapcat
	     (lambda (it)
	       (list
		(make-builder-candidate
		 :command (format "black %s" it)
		 :directory it
		 :annotation (format "run `black' in the directory %s" it))
		(make-builder-candidate
		 :command (format "isort %s" it)
		 :directory it
		 :annotation (format "run `isort' in the directory %s" it))
		(make-builder-candidate
		 :command (format "isort --force-single-line-imports %s" it)
		 :directory it
		 :annotation (format "run `isort' in the directory %s (single line imports)" it))))
	     (f-directories-containing-file-with-extension-py directories)))

(builder-register-candidates
 :name "rust-project"
 :pipeline (append
	     (seq-mapcat
	      (lambda (it)
		(let ((annotation-directory (f-collapse-homedir it))
		      (directory it))
		  (seq-map
		   (lambda (it)
		     (if (f-equal-p project-root-directory directory)
			 (make-builder-candidate
			  :directory directory
			  :command (format "cargo %s" it)
			  :annotation (format "run cargo target '%s' in project root (%s)" it annotation-directory))
		       (make-builder-candidate
			:directory directory
			:name (format "cargo %s <%s>" it (file-name-sans-extension (file-name-nondirectory directory)))
			:command (format "cd %s; cargo %s" directory it)
			:annotation (format "run cargo target '%s' in package (%s)" it annotation-directory))))
		   '("build" "check" "test" "fix" "fmt" "clippy" "clippy --fix"))))
	      (f-directories-containing-file-cargo-toml directories))
	     (when (f-directories-containing-file-cargo-toml project-root-directory)
	       (let ((annotation-directory (f-collapse-homedir project-root-directory)))
		 (seq-mapcat
		  (lambda (it)
		    (list
		     (make-builder-candidate
		      :directory project-root-directory
		      :command (concat "cargo build --" it)
		      :annotation (format "build %s in project root (%s)" it annotation-directory))
		     (make-builder-candidate
		      :directory project-root-directory
		      :command (concat "cargo check --" it)
		      :annotation (format "run static analysis %s in project root (%s)" it annotation-directory))))
		  '("libs" "bins" "examples" "tests" "benches" "all-targets"))))))

(builder-register-candidates
 :name "justfile"
 :pipeline (seq-mapcat
	     (lambda (it)
	       (let ((directory it)
		     (annotation-directory (f-collapse-homedir it)))
		 (seq-map
		  (lambda (it)
		    (make-builder-candidate
		     :directory directory
		     :command (format "just %s" it)
		     :annotation (format "exec justfile target %s in %s" it annotation-directory)))
		  (when-let* ((default-directory directory)
			      (output (shell-command-to-string "just --summary"))
			      (candidates (split-string output)))
		    candidates))))
	     (f-directories-containing-file-justfile directories)))
(defun builder--vale-insert-statistics (buffer _message &key filename)
  (interactive)
  (let* ((table (json-parse-string (shell-command-to-string (format "vale ls-metrics --output=line %s" filename)))))
    (with-current-buffer buffer
      (insert (format "\nstatistics for %s" (propertize (f-collapse-homedir filename) 'face 'italic)))
      (map-do (lambda (key value) (insert (format "\n%s:%s" (propertize key 'face 'bold) value)))
	      table))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; emacs-lisp -- static analysis and linting targets

(defun builder--el-files-in-directory (directory)
  "Return a list of .el files in DIRECTORY, excluding byte-compiled .elc files."
  (seq-filter (lambda (it) (string-equal (file-name-extension it) "el")) (f-entries directory #'file-regular-p)))

(builder-register-candidates
 :name "emacs-lisp-file"
 :pipeline
 (when-let* ((filename (buffer-file-name))
             (_ (derived-mode-p 'emacs-lisp-mode))
             (short-name (f-collapse-homedir filename))
             (basename (file-name-nondirectory filename))
             (directory (f-dirname filename))
             (lisp-dir (f-join user-emacs-directory "lisp")))
   (-l (make-builder-candidate
        :name (format "byte-compile %s" basename)
        :command (format "emacs --batch -L %s -f package-initialize -f batch-byte-compile %s" lisp-dir filename)
        :directory directory
        :annotation (format "byte-compile %s" short-name))
       (make-builder-candidate
        :name (format "relint %s" basename)
        :command (format "emacs --batch -L %s -f package-initialize --eval \"(require 'relint)\" -f relint-batch %s" lisp-dir filename)
        :directory directory
        :annotation (format "check regexps in %s" short-name))
       (make-builder-candidate
        :name (format "package-lint %s" basename)
        :command (format "emacs --batch -L %s -f package-initialize --eval \"(require 'package-lint)\" -f package-lint-batch-and-exit %s" lisp-dir filename)
        :directory directory
        :annotation (format "lint package metadata in %s" short-name))
       (make-builder-candidate
        :name (format "elisp-lint %s" basename)
        :command (format "emacs --batch -L %s -f package-initialize --eval \"(require 'elisp-lint)\" -f elisp-lint-files-batch %s" lisp-dir filename)
        :directory directory
        :annotation (format "run elisp-lint on %s" short-name))
       (make-builder-candidate
        :name (format "elsa %s" basename)
        :command (format "emacs --batch -L %s -f package-initialize --eval \"(require 'elsa)\" -f elsa-run %s" lisp-dir filename)
        :directory directory
        :annotation (format "run elsa type analysis on %s" short-name)))))

(builder-register-candidates
 :name "emacs-lisp-project"
 :pipeline
 (when-let* ((_ (derived-mode-p 'emacs-lisp-mode))
             (el-files (builder--el-files-in-directory project-root-directory))
             (_ el-files)
             (file-args (string-join el-files " "))
             (lisp-dir (f-join user-emacs-directory "lisp")))
   (-l (make-builder-candidate
        :name (format "byte-compile <%s>" project-name)
        :command (format "emacs --batch -L %s -f package-initialize -f batch-byte-compile %s" lisp-dir file-args)
        :directory project-root-directory
        :annotation (format "byte-compile all .el files in %s" project-name))
       (make-builder-candidate
        :name (format "relint <%s>" project-name)
        :command (format "emacs --batch -L %s -f package-initialize --eval \"(require 'relint)\" -f relint-batch %s" lisp-dir file-args)
        :directory project-root-directory
        :annotation (format "check regexps across all .el files in %s" project-name))
       (make-builder-candidate
        :name (format "package-lint <%s>" project-name)
        :command (format "emacs --batch -L %s -f package-initialize --eval \"(require 'package-lint)\" -f package-lint-batch-and-exit %s" lisp-dir file-args)
        :directory project-root-directory
        :annotation (format "lint package metadata across all .el files in %s" project-name))
       (make-builder-candidate
        :name (format "elisp-lint <%s>" project-name)
        :command (format "emacs --batch -L %s -f package-initialize --eval \"(require 'elisp-lint)\" -f elisp-lint-files-batch %s" lisp-dir file-args)
        :directory project-root-directory
        :annotation (format "run elisp-lint across all .el files in %s" project-name))
       (make-builder-candidate
        :name (format "elsa <%s>" project-name)
        :command (format "emacs --batch -L %s -f package-initialize --eval \"(require 'elsa)\" -f elsa-run %s" lisp-dir file-args)
        :directory project-root-directory
        :annotation (format "run elsa type analysis across all .el files in %s" project-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; org-babel -- execute source blocks in org files

(defun builder--org-babel-select-file ()
  "Prompt for an org file to execute.
Candidates are open org-mode buffers first, then `org-agenda-files'.
Falls back to `read-file-name' when the candidate list is empty."
  (let* ((open-org (thread-last (buffer-list)
                     (seq-filter (lambda (b)
                                   (with-current-buffer b
                                     (and (derived-mode-p 'org-mode)
                                          (buffer-file-name)))))
                     (seq-map #'buffer-file-name)))
         (agenda (ignore-errors
                   (seq-filter #'file-exists-p (org-agenda-files))))
         (candidates (seq-uniq (append open-org agenda)))
         (choice (if candidates
                     (completing-read "Execute org file: " candidates)
                   (read-file-name "Execute org file: " default-directory
                                   nil t nil
                                   (lambda (f)
                                     (or (file-directory-p f)
                                         (string-suffix-p ".org" f)))))))
    (if (file-name-absolute-p choice)
        choice
      (expand-file-name choice default-directory))))

;;;###autoload
(defun builder-org-babel-execute-file (&optional file)
  "Execute all Babel source blocks in FILE without confirmation prompts.
When FILE is nil and the current buffer is `org-mode', execute it directly.
Otherwise prompt via `builder--org-babel-select-file' over open org buffers
and `org-agenda-files', falling back to `read-file-name'.
Buffers already visiting the file before this call are left open afterwards."
  (interactive)
  (let* ((target (or file
                     (if (derived-mode-p 'org-mode)
                         (buffer-file-name)
                       (builder--org-babel-select-file))))
         (already-open (find-buffer-visiting target))
         (buf (or already-open (find-file-noselect target))))
    (with-current-buffer buf
      (let ((org-confirm-babel-evaluate nil))
        (org-babel-execute-buffer)))
    (unless already-open
      (kill-buffer buf))))

;;;###autoload
(defun builder-org-babel-execute-directory (directory)
  "Execute all Babel source blocks in every .org file under DIRECTORY.
Calls `builder-org-babel-execute-file' on each file found by
`directory-files-recursively'."
  (interactive (list (read-directory-name "Execute org files in: ")))
  (seq-do #'builder-org-babel-execute-file
          (directory-files-recursively directory "\\.org\\'")))

(builder-register-candidates
 :name "org-babel-file"
 :pipeline
 (when-let* ((filename (buffer-file-name))
             (_ (derived-mode-p 'org-mode))
             (short-name (f-collapse-homedir filename))
             (basename (file-name-nondirectory filename))
             (directory (f-dirname filename)))
   (-l (make-builder-candidate
        :name (format "org-babel-execute %s (emacsclient)" basename)
        :command (format "emacsclient --eval '(builder-org-babel-execute-file \"%s\")'" filename)
        :directory directory
        :annotation (format "execute all babel blocks in %s via emacsclient" short-name)
        :priority 0)
       (make-builder-candidate
        :name (format "org-babel-execute %s (fresh emacs)" basename)
        :command (format "emacs --org-exec %s" (shell-quote-argument filename))
        :directory directory
        :annotation (format "execute all babel blocks in %s in a fresh Emacs" short-name)
        :priority 0))))

(builder-register-candidates
 :name "org-babel-directory"
 :pipeline
 (when-let* ((_ (derived-mode-p 'org-mode))
             (org-files (seq-filter (lambda (f) (string-suffix-p ".org" f))
                                    (f-files project-root-directory nil t)))
             (_ org-files))
   (-l (make-builder-candidate
        :name (format "org-babel-execute <%s> (emacsclient)" project-name)
        :command (format "emacsclient --eval '(builder-org-babel-execute-directory \"%s\")'"
                         project-root-directory)
        :directory project-root-directory
        :annotation (format "execute all org babel blocks in %s via emacsclient" project-name))
       (make-builder-candidate
        :name (format "org-babel-execute <%s> (fresh emacs)" project-name)
        :command (format "emacs --org-exec-dir %s"
                         (shell-quote-argument project-root-directory))
        :directory project-root-directory
        :annotation (format "execute all org files in %s in a fresh Emacs" project-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; emacs-lisp -- package operations (test/compile/build/clean)
;;
;; These commands operate on an "elisp package" rooted at
;; `approximate-project-root', identified by the convention that the
;; root contains <NAME>.el where NAME matches the directory name
;; (with an optional ".el" suffix on the directory itself, as in
;; "xtdlib.el/xtdlib.el"). Multi-file packages are supported: every
;; top-level .el file is treated as a source (excluding the generated
;; <NAME>-pkg.el and <NAME>-autoloads.el). Tests live under <root>/test/
;; as test-*.el or *-test.el. Dependencies come from <NAME>-pkg.el's
;; `define-package' form when present, otherwise from `Package-Requires'
;; on the main file.

(defun builder-elisp-package--name (root)
  "Return the package name for the elisp project at ROOT.
Strips a trailing \".el\" from the directory name so a project in
\"xtdlib.el/\" packages as \"xtdlib\"."
  (let ((basename (file-name-nondirectory (directory-file-name root))))
    (if (string-suffix-p ".el" basename)
        (string-remove-suffix ".el" basename)
      basename)))

(defun builder-elisp-package--main-file (root)
  "Return the absolute path to the main .el file under ROOT, or nil.
Looks for <NAME>.el where NAME comes from `builder-elisp-package--name'."
  (let ((path (f-join root (concat (builder-elisp-package--name root) ".el"))))
    (and (file-regular-p path) path)))

(defun builder-elisp-package-p (&optional root)
  "Return non-nil if ROOT looks like an elisp package.
ROOT defaults to `approximate-project-root'."
  (and (builder-elisp-package--main-file (or root (approximate-project-root))) t))

(defun builder-elisp-package--pkg-file (root)
  "Return absolute path to <NAME>-pkg.el under ROOT, or nil if absent."
  (let ((path (f-join root (concat (builder-elisp-package--name root) "-pkg.el"))))
    (and (file-regular-p path) path)))

(defun builder-elisp-package--source-files (root)
  "Return the list of top-level .el source files under ROOT.
Excludes test/ (not at top level), the generated <NAME>-pkg.el package
descriptor, and any <NAME>-autoloads.el file — none of which should be
byte-compiled as ordinary sources."
  (let* ((name (builder-elisp-package--name root))
         (skip (list (concat name "-pkg.el")
                     (concat name "-autoloads.el"))))
    (seq-remove (lambda (it) (member (file-name-nondirectory it) skip))
		 (seq-filter (lambda (it) (string-equal (file-name-extension it) "el")) (f-entries root #'file-regular-p)))))

(defun builder-elisp-package--test-files (root)
  "Return the list of test files under ROOT/test (test-*.el or *-test.el)."
  (let ((test-dir (f-join root "test")))
    (when (file-directory-p test-dir)
      (seq-filter (lambda (it)
		     (let ((name (file-name-nondirectory it)))
		       (or (string-prefix-p "test-" name)
			   (string-suffix-p "-test.el" name))))
		   (seq-filter (lambda (it) (string-equal (file-name-extension it) "el")) (f-entries test-dir #'file-regular-p))))))

(defun builder-elisp-package--read-pkg-deps (pkg-file)
  "Return dep symbols from a multi-file package's <NAME>-pkg.el descriptor.
Reads the `define-package' form's fourth argument (the requires list).
The pseudo-package `emacs' is omitted."
  (with-temp-buffer
    (insert-file-contents pkg-file)
    (goto-char (point-min))
    (let ((form (ignore-errors (read (current-buffer)))))
      (when (and (consp form) (eq (car form) 'define-package))
        (let ((reqs (nth 4 form)))
          (when (and (consp reqs) (eq (car reqs) 'quote))
            (setq reqs (cadr reqs)))
          (seq-remove (lambda (it) (eq it 'emacs))
			  (seq-map #'car reqs)))))))

(defun builder-elisp-package--read-deps (file-or-root)
  "Return list of dep package symbols for the package at FILE-OR-ROOT.
If FILE-OR-ROOT is a directory, prefer <NAME>-pkg.el's `define-package'
requires and fall back to the main file's `Package-Requires' header.
If it is a file, read `Package-Requires' from that file. The pseudo-
package `emacs' is omitted, since it is not installable."
  (require 'lisp-mnt)
  (cond
   ((and file-or-root (file-directory-p file-or-root))
    (or (when-let* ((pkg (builder-elisp-package--pkg-file file-or-root)))
          (builder-elisp-package--read-pkg-deps pkg))
        (when-let* ((main (builder-elisp-package--main-file file-or-root)))
          (builder-elisp-package--read-deps main))))
   ((and file-or-root (file-regular-p file-or-root))
    (with-temp-buffer
      (insert-file-contents file-or-root)
      (when-let* ((line (lm-header "Package-Requires")))
        (seq-remove (lambda (it) (eq it 'emacs))
			(seq-map #'car (read line))))))))

(defun builder-elisp-package--require-deps (deps)
  "Require each symbol in DEPS, signaling a `user-error' if any cannot load.
Use this from contexts that assume the user's `load-path' already provides them
\(direct M-x invocation, or batch with `-L' flags pointing into ~/.emacs.d/elpa)."
  (dolist (it deps)
    (unless (require it nil t)
      (user-error "elisp-package dependency not on load-path: %s" it))))

;;;###autoload
(defun builder-elisp-package-test ()
  "Run ert tests for the elisp package at `approximate-project-root'.
Loads <root>/<NAME>.el and any test/test-*.el or test/*-test.el files,
then runs ert. In batch mode exits with the test status; otherwise
opens the *ert* selector."
  (interactive)
  (require 'ert)
  (let* ((root      (file-name-as-directory (approximate-project-root)))
         (main-file (builder-elisp-package--main-file root)))
    (unless main-file
      (user-error "no <%s>.el at %s" (builder-elisp-package--name root) root))
    (builder-elisp-package--require-deps (builder-elisp-package--read-deps root))
    (let* ((test-dir (f-join root "test"))
           (load-path (seq-filter #'identity (list root (and (file-directory-p test-dir) test-dir) load-path))))
      (load main-file nil t)
      (dolist (it (builder-elisp-package--source-files root))
        (unless (f-equal-p it main-file)
          (load it nil t)))
      (dolist (it (builder-elisp-package--test-files root))
        (load it nil t)))
    (if noninteractive
        (ert-run-tests-batch-and-exit)
      (ert t))))

;;;###autoload
(defun builder-elisp-package-compile ()
  "Byte-compile every top-level .el source file in the elisp package at point.
In batch mode exits with status 1 if any file fails to compile."
  (interactive)
  (let* ((root      (file-name-as-directory (approximate-project-root)))
         (main-file (builder-elisp-package--main-file root)))
    (unless main-file
      (user-error "no <%s>.el at %s" (builder-elisp-package--name root) root))
    (builder-elisp-package--require-deps (builder-elisp-package--read-deps root))
    (let* ((load-path (cons root load-path))
           (failed 0))
      (dolist (it (builder-elisp-package--source-files root))
        (unless (byte-compile-file it)
          (cl-incf failed)))
      (when noninteractive
        (kill-emacs (if (zerop failed) 0 1)))
      (zerop failed))))

;;;###autoload
(defun builder-elisp-package-build ()
  "Build an installable .tar via `package-build' for the elisp package at point.
The recipe is generated at build time and uses a file:// URL pointing at the
local repository, so the package contents reflect HEAD; uncommitted changes
are not included. The tarball lands in <root>/build/packages/."
  (interactive)
  (require 'package-build)
  (require 'package-recipe)
  (let* ((root      (file-name-as-directory (approximate-project-root)))
         (name      (builder-elisp-package--name root))
         (main-file (builder-elisp-package--main-file root)))
    (unless main-file
      (user-error "no <%s>.el at %s" name root))
    (unless (file-directory-p (f-join root ".git"))
      (user-error "package build requires a git repository at %s" root))
    (unless (zerop (call-process "git" nil nil nil "-C" root
                                 "rev-parse" "--verify" "HEAD"))
      (user-error "package build requires at least one commit in %s" root))
    (let* ((build-root  (f-join root "build"))
           (working-dir (f-join build-root "working"))
           (archive-dir (f-join build-root "packages"))
           (recipes-dir (f-join build-root "recipes"))
           (recipe-path (f-join recipes-dir name)))
      (dolist (it (list build-root working-dir archive-dir recipes-dir))
        (make-directory it t))
      (with-temp-file recipe-path
        (let ((print-quoted t))
          (prin1 `(,(intern name)
                   :fetcher git
                   :url ,(concat "file://" (directory-file-name root))
                   :files ("*.el"))
                 (current-buffer)))
        (insert "\n"))
      (let ((package-build-working-dir (file-name-as-directory working-dir))
            (package-build-archive-dir (file-name-as-directory archive-dir))
            (package-build-recipes-dir (file-name-as-directory recipes-dir))
            (package-build-verbose t)
            (package-build-releases nil))
        (package-build-archive name))
      (when noninteractive (kill-emacs 0)))))

;;;###autoload
(defun builder-elisp-package-clean ()
  "Remove build artifacts (build/, .cache/, *.elc) for the elisp package at point."
  (interactive)
  (let ((root (file-name-as-directory (approximate-project-root))))
    (dolist (it (list (f-join root "build") (f-join root ".cache")))
      (when (file-directory-p it)
        (delete-directory it t)
        (message "removed %s" it)))
    (dolist (it (seq-filter (lambda (it) (string-equal (file-name-extension it) "elc"))
			  (f-entries root #'file-regular-p t)))
      (delete-file it)
      (message "removed %s" it))
    (when noninteractive (kill-emacs 0))))

;; package dependency analysis — builds on the private `package--get-deps'
;; (transitive closure) and `package-desc-reqs' (direct deps from local
;; metadata) primitives in package.el.  `package--removable-packages' is a
;; near-cousin but is hard-wired to `package-selected-packages'; these
;; helpers accept an arbitrary root list.

(defun builder-package--direct-deps (package)
  "Return the list of direct dependency symbols declared by PACKAGE.
Reads `Package-Requires' via `package-desc-reqs' on the entry in
`package-alist'; filters the `emacs' pseudo-dependency.  Returns nil
when PACKAGE is not installed."
  (when-let* ((desc (cadr (assq package package-alist))))
    (seq-remove (lambda (it) (eq it 'emacs))
		 (seq-map #'car (package-desc-reqs desc)))))

(defun builder-package-deps-map (packages)
  "Return a hash-table mapping each symbol in PACKAGES to its direct deps.
Values are lists of dependency symbols read from local package
metadata.  Packages not present in `package-alist' map to nil."
  (let ((table (make-hash-table :test 'eq)))
    (dolist (pkg packages table)
      (puthash pkg (builder-package--direct-deps pkg) table))))

(defun builder-package-deps-closure (packages)
  "Return the de-duplicated transitive closure of PACKAGES.
Includes the roots themselves even when they have no entry in
`package-alist' (e.g. packages loaded via `:load-path').
`package--get-deps' filters such roots out of its result because it
keys off `package-alist'; we union the raw roots back in so closure
membership is meaningful for non-package.el installs."
  (seq-uniq (append (copy-sequence packages)
                    (package--get-deps (copy-sequence packages)))))

(defvar builder-package-installed-exclude '(archives gnupg)
  "Directory-name symbols under `package-user-dir' to ignore.
These are subdirectories created by `package.el' that are not
packages (archive metadata, gpg keyring).  Consumed by
`builder-package--installed-on-disk'.")

(defun builder-package--installed-on-disk ()
  "Return package-name symbols rooted in `package-user-dir'.
Reads the directory directly so packages without a `package-alist'
entry are still surfaced.  A trailing `-<digits-and-dots>' version
suffix (as written by package.el) is stripped so the result is
comparable with `package-alist' keys."
  (when (file-directory-p package-user-dir)
    (thread-last (directory-files package-user-dir nil "\\`[^.]")
      (seq-filter (lambda (it) (file-directory-p (expand-file-name it package-user-dir))))
      (seq-map (lambda (it) (replace-regexp-in-string "-[0-9.]+\\'" "" it)))
      (seq-map #'intern)
      (seq-remove (lambda (it) (memq it builder-package-installed-exclude)))
      seq-uniq)))

(defun builder-package--declared-use-packages (&optional dir)
  "Return symbols declared via `use-package' in init source files.
Walks .el files under DIR (defaults to `user-emacs-directory'/lisp) by
`read'ing top-level forms and collecting the NAME from every
(use-package NAME ...) form, including those nested inside top-level
`progn'/`with-eval-after-load'/`eval-when-compile' wrappers.  Used to
augment `builder-package-unused' so packages loaded via `:load-path'
are not misclassified as unused."
  (let* ((dir (or dir (expand-file-name "lisp" user-emacs-directory)))
         (declared '())
         (walk nil))
    (setq walk
          (lambda (form)
            (when (consp form)
              (cond
               ((and (eq (car form) 'use-package) (symbolp (cadr form)))
                (push (cadr form) declared))
               ((memq (car form) '(progn prog1 prog2 eval-and-compile
                                        eval-when-compile with-eval-after-load
                                        when unless if))
                (dolist (sub (cdr form)) (funcall walk sub)))))))
    (when (file-directory-p dir)
      (dolist (file (directory-files-recursively dir "\\.el\\'"))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (condition-case nil
              (while t (funcall walk (read (current-buffer))))
            (end-of-file nil)
            (invalid-read-syntax nil)))))
    (seq-uniq declared)))

;;;###autoload
(defun builder-package-unused (packages)
  "Return packages on disk under `package-user-dir' not reachable from PACKAGES.
The reachable set is `builder-package-deps-closure' of PACKAGES *plus*
every name declared via `use-package' in user init files (see
`builder-package--declared-use-packages') — this keeps `:load-path'
installs out of the unused list.  The installed set comes from
`builder-package--installed-on-disk'.  Called interactively, defaults
PACKAGES to `package-selected-packages' and echoes the result."
  (interactive (list package-selected-packages))
  (let* ((roots (seq-uniq (append packages
                                  (builder-package--declared-use-packages))))
         (needed (builder-package-deps-closure roots))
         (unused (seq-sort (lambda (a b) (string-lessp (symbol-name a)
						    (symbol-name b)))
			  (seq-remove (lambda (it) (memq it needed))
				      (builder-package--installed-on-disk)))))
    (when (called-interactively-p 'interactive)
      (if unused
          (message "%d unused under %s: %s"
                   (length unused) package-user-dir
                   (mapconcat #'symbol-name unused " "))
        (message "no unused packages under %s" package-user-dir)))
    unused))

(builder-register-candidates
 :name "emacs-lisp-package"
 :pipeline
 (when-let* ((_ (derived-mode-p 'emacs-lisp-mode))
             (_ (builder-elisp-package-p project-root-directory))
             (display-name (builder-elisp-package--name project-root-directory))
             (lisp-dir (f-join user-emacs-directory "lisp")))
   (seq-map
    (lambda (it)
      (let ((op (car it))
	    (fn (cadr it))
	    (desc (caddr it)))
	(make-builder-candidate
	 :name (format "%s-<%s>" op display-name)
	 :command (format "emacs --batch -L %s -f package-initialize --eval \"(require 'builder)\" -f %s" lisp-dir fn)
	 :directory project-root-directory
	 :annotation (format "%s elisp package <%s>" desc display-name))))
    '(("test-package"    "builder-elisp-package-test"    "run ert tests for")
      ("compile-package" "builder-elisp-package-compile" "byte-compile sources of")
      ("build-package"   "builder-elisp-package-build"   "build installable .tar via package-build for")
      ("clean-package"   "builder-elisp-package-clean"   "remove build artifacts of")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; emacs-lisp -- Cask project operations

(builder-register-candidates
 :name "emacs-lisp-cask"
 :pipeline
 (when (file-regular-p (f-join project-root-directory "Cask"))
   (-l (make-builder-candidate
        :name (format "cask-install <%s>" project-name)
        :command "cask install"
        :directory project-root-directory
        :annotation (format "install Cask dependencies for %s" project-name))
       (make-builder-candidate
        :name (format "cask-test <%s>" project-name)
        :command "cask exec ert-runner"
        :directory project-root-directory
        :annotation (format "run ert tests via ert-runner for %s" project-name))
       (make-builder-candidate
        :name (format "cask-build <%s>" project-name)
        :command "cask build"
        :directory project-root-directory
        :annotation (format "byte-compile sources via Cask for %s" project-name))
       (make-builder-candidate
        :name (format "cask-clean <%s>" project-name)
        :command "cask clean-elc"
        :directory project-root-directory
        :annotation (format "remove compiled .elc files via Cask for %s" project-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; docker -- CI test runner

;;;###autoload
(defun tychoish-run-ci-tests-docker ()
  "Run CI tests for the current project in a Docker container.
Detects whether a Cask file is present in the project root to
select the appropriate silex/emacs image and test invocation.
With Cask: uses the \"-ci\" image variant and runs
\"cask install && cask exec ert-runner test/test-*.el\".
Without Cask: uses the plain image and runs emacs in batch mode
to load test files found under test/ and run ert."
  (interactive)
  (let* ((root (file-name-as-directory (approximate-project-root)))
         (project-name (file-name-nondirectory (directory-file-name root)))
         (has-cask (file-regular-p (f-join root "Cask")))
         (image (if has-cask
                    "silex/emacs:30.2-ci-cask"
                  "silex/emacs:30.2"))
         (test-command (if has-cask
                           "cask install && cask exec ert-runner test/test-*.el"
                         "emacs --batch -L . $(find test -name '*.el' | sed 's/^/-l /') -f ert-run-tests-batch-and-exit"))
         (command (format "docker run --rm -v %s:/workspace -w /workspace %s bash -c %s"
                          (shell-quote-argument (directory-file-name root))
                          image
                          (shell-quote-argument test-command)))
         (buf-name (format "*%s-docker-ci*" project-name))
         (default-directory root))
    (compilation-start
     command
     'compilation-mode
     (compile-buffer-name buf-name))))

(provide 'builder)
;;; builder.el ends here
