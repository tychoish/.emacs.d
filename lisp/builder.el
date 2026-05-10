;;; builder.el --- compilation buffer and command runner -*- lexical-binding: t -*-

;; Author: tychoish

;;; Commentary:

;; Interactive compilation buffer selection, command discovery, and
;; candidate registration for project-aware compile workflows.

;;; Code:

(require 'compile)

(require 'f)
(require 's)
(require 'ht)
(require 'dash)

(require 'xlib)
(require 'annotated-completing-read)
(require 'eglot-test-at-point)

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

(defun builder--go-module (&optional directory)
  (if go-module-path
      go-module-path
    (let* ((output (with-default-directory directory
		     (s-trim (shell-command-to-string "go list"))))
	   (proj-root (approximate-project-root)))
      (if (or (f-equal-p default-directory directory)
	      (s-prefix-p proj-root (f-full default-directory)))
	  (setq-local go-module-path output)
	output))))

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
	     (-append args (-strings (format "<%s> %s -- %s" tychoish/emacs-instance-id (buffer-name buffer) msg)))))

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

(defun builder-reset-finish-hooks ()
  (interactive)
  (->> (mode-buffers 'compilation-mode)
       (--mapc (with-current-buffer it
		 (setq-local compilation-finish-functions nil)))))

(defun builder--buffer-for-command (command)
  "Return the name of a project compilation buffer that last ran COMMAND, or nil."
  (when-let* ((buf (->> (mode-buffers-for-project :mode 'compilation-mode)
			(--first (with-current-buffer it
				   (string-equal (s-trim (or (car compilation-arguments) ""))
						 (s-trim command)))))))
    (buffer-name buf)))

(defun builder--buffer-ran-command-p (buffer command)
  "Return non-nil if BUFFER last ran COMMAND."
  (with-current-buffer buffer
    (string-equal (s-trim (or (car compilation-arguments) ""))
		  (s-trim command))))

;;;###autoload
(defun builder-compile-project (&optional name command)
  (let* ((project-root-directory (approximate-project-root))
	 (start-at (current-time))
	 ;; Step 1: select and confirm command
	 (cc-result (builder--read-command command))
	 (candidate-name (car cc-result))
	 (candidates (cdr cc-result))
	 (candidate (ht-get candidates candidate-name))
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

	(when-let* ((candidate (ht-get candidates candidate-name))
		    (hook (builder-candidate-hook candidate)))
	  (when hook
	    (add-one-shot-hook :name (format "post-%s-hook-operation" op-name)
	     :hook 'compilation-finish-functions
	     :make-unique t
	     :local t
	     :function (hook))))

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
                        :candidates (-distinct
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
  (or (when-let* ((c (ht-get candidates name)))
	(builder-candidate-priority c))
      2))

(defun builder--make-sort-fn (candidates history-key)
  "Return a display-sort-function for `annotated-completing-read'.
Candidates that appear in HISTORY-KEY's history list (stored in
`annotated-completing-read-history') sort first by recency (lowest index =
most recent).  Candidates not in history are then ordered by structural
priority from CANDIDATES, with key length as a final tiebreaker."
  (lambda (items)
    (let* ((hist (ht-get annotated-completing-read-history history-key))
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
         (annotation-table (let ((tbl (ht-create)))
                             (ht-each (lambda (name cand)
                                        (ht-set tbl name (builder-candidate-annotation cand)))
                                      candidates)
                             tbl))
         (selection-name
          (annotated-completing-read
           annotation-table
           :prompt "compile command => "
           :require-match nil
           :history 'compile-history
           :sort-fn (builder--make-sort-fn candidates 'compile-history)))
	 (candidate (ht-get candidates selection-name))
	 (operation-name (if candidate
			     (builder-candidate-name candidate)
			   (builder--add-candidate
			    candidates
			    (make-builder-candidate
			     :command selection-name
			     :directory default-directory
			     :annotation "user input"))
			   selection-name)))
    (cons operation-name candidates)))

(cl-defun builder--project-compilation-buffers (&optional &key name (project (approximate-project-name)))
  "Return a hash table of candidate compilation buffer names for PROJECT."
  (let ((buffer-table (ht-create))
        (default-names (list "push" "gen" "buf" "benchmark" "check" "compile" "run" "lint" "test" "build")))
    (when name
      (cl-pushnew name default-names :test #'equal))

    (--each (mode-buffers-for-project :mode 'compilation-mode)
      (with-current-buffer it
        (ht-set
         buffer-table
         (buffer-name it)
         (format "reuse buffer: project=%s errors=%s lines=%s command='%s'"
                 project
                 compilation-num-errors-found
                 (buffer-line-count)
                 (string-trim (or (car compilation-arguments) ""))))))

    (--each default-names
      (let ((name (format "*%s-%s*" project it)))
        (if (ht-contains-p buffer-table name)
            (ht-set
             buffer-table
             (generate-new-buffer-name name)
             (format "new %s buffer for project %s" name project))
          (ht-set
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
				   (name (s-truncate 32 command "..."))
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
  (ht-set table (builder-candidate-name candidate) candidate))

(defun builder--add-candidates (table candidates)
  (->> candidates
       (-filter #'builder-candidate-p)
       (--mapc (builder--add-candidate table it))))

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

;;;###autoload
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

	     (->> hooks (--mapc (add-hook it ',hook-registering-function-name))))))))

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
	   (project-name (f-filename project-root-directory))
	   (default-directory (or directory default-directory))
           (directories (->> (approximate-project-buffers)
			     (-keep #'buffer-directory)
			     (-filter #'f-directory-p)
			     (-map #'f-full)
			     (-append (annotated-completing-read--directory-parents default-directory project-root-directory))
			     (-distinct)))
	   (operation-table (ht-create)))

      (when command
	(builder--add-candidate
	 operation-table
	 (make-builder-candidate
	  :name (s-truncate 32 command "...")
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
  (->> (buffer-list)
       (--filter (builder--candidate-cache-p it))
       (-mapc #'builder--clear-candidate-cache)))

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
 :pipeline (->> (-join (--map (cons it "minibuffer-history") (minibuffer-default-add-shell-commands))
			(--map (cons it "shell-command-history") shell-command-history))
		(--map
		 (let ((command (car it))
		       (source (cdr it))
		       (annotation-tag (if (string-equal default-directory project-root-directory) "project root" "working directory")))
		   (make-builder-candidate
		    :name (s-truncate 32 command "...")
		    :command command
		    :directory project-root-directory
		    :annotation (format "operation from `%s' in the %s (%s)" source annotation-tag default-directory)
		    :priority 3)))))

(builder-register-candidates
 :name "project-compilation-buffer-commands"
 :pipeline (->> (mode-buffers-for-project
		 :mode 'compilation-mode)
		(--keep
		 (with-current-buffer it
		   (when-let* ((command (s-trimmed-or-nil (car compilation-arguments))))
		     (cons command it))))
		(--flat-map
		 (let ((command (car it))
		       (buf (cdr it)))
		   (->> (list project-root-directory (buffer-directory buf) default-directory)
			(-distinct)
			(-non-nil)
			(--map (let ((operation-directory it))
				 (make-builder-candidate
				  :name (format "rerun from %s [%s]" buf (f-visually-compress-to-five operation-directory))
				  :command command
				  :directory operation-directory
				  :annotation (format "command '%s' from %s in %s" command buf operation-directory)
				  :priority 1))))))))

(builder-register-candidates
 :name "makefiles"
 :pipeline (->> (f-directories-containing-file-makefile directories)
		(--flat-map (let* ((default-directory it)
				   (makefile-report (process-lines "bash" "-c" "make --dry-run --print-data-base | grep -E '^[a-zA-Z0-9_-]+:' | sed 's/:.*//'")))
			      (unless (string-prefix-p "make: ***" (car makefile-report))
				(->> makefile-report
				     (--map (let ((target it))
					      `((target . ,target)
						(directory . ,default-directory))))
				     (--filter (and it (alist-get 'directory it) (alist-get 'target it)))))))
		(--flat-map
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
		   (->> '((""         "")
			  ("-k "      ", continuing on error")
			  ("-B "      ", unconditionally")
			  ("-k -B "   ", unconditionally while continuing on error"))
			(--map (let ((flag (car it))
				     (annotation-suffix (cdr it)))
				 (make-builder-candidate
				  :name (format name-template flag)
				  :command (format command-template flag)
				  :directory directory
				  :annotation (s-join-with-space annotation-template annotation-suffix)))))))))

(builder-register-candidates
 :name "go-packages"
 :pipeline (->> (-append
		 (f-directories-containing-file-with-extension-go directories)
		 (f-directories-containing-file-go-mod project-root-directory))
		(-non-nil)
		(-uniq)
		(--flat-map
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
			(is-recursive (string-suffix-p "..." it))
			(path-for-project-tag (cond ((f-equal-p operation-directory project-root-directory) "")
						    ((equal short-path package-path) "")
						    (t package-path)))
			(proj-path-tag (format "<%s>/%s" project-name path-for-project-tag))
			(proj-path-for-name (s-join-with-space (format "<%s>/%s" project-name
								       (f-visually-compress-to-five path-for-project-tag))))
			(proj-path-recursive (f-join proj-path-for-name "...")))
		   (-append
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
				    (s-concat "sed -r \"$(go list -f='s%{{.ImportPath}}%{{.Dir}}%')\"")
				    "grep -v '100.0%'"
				    "column -t;"))
			 :annotation (s-join-with-space "collect and report coverage data for" short-path)))
		    (->> '(("go test -cover"  "the code coverage collector")
			   ("go test -race"   "the race detector")
			   ("go test"         "default options"))
			 (--flat-map
			  (let ((command-prefix (car it))
				(annotation-prefix (cadr it)))
			    (->> '("10s" "30s" "1m")
				 (--map
				  (let* ((timeout-spec it)
					 (is-default (equal timeout-spec ""))
					 (timeout-arg (unless is-default (concat "--timeout=" timeout-spec)))
					 (timeout-name (if is-default "no timeout" (s-join-with-space "a" timeout-spec "timeout"))))
				    (make-builder-candidate
				     :name (s-join-with-space command-prefix timeout-spec (f-join proj-path-for-name "..."))
				     :command (s-join-with-space command-prefix timeout-arg operation-directory-tree)
				     :directory directory
				     :annotation (format "run go test in %s recursively with %s and %s" short-path annotation-prefix timeout-name))))))))
		    (->> '("revive" "makezero" "exhaustruct")
			 (--map
			  (make-builder-candidate
			   :name (format "lint %s %s" it proj-path-for-name)
			   :command (format "golangci-lint run --enable-only=%s %s" it operation-directory)
			   :directory directory
			   :annotation (format "run (only) the %s linter in %s" it short-path)))))))))

(builder-register-candidates
 :name "go-files"
 :pipeline (when-let* ((filename (if (and (buffer-file-name) (derived-mode-p '(go-mode go-ts-mode)))
				     (buffer-file-name)
				   (thing-at-point 'filename)))
		       (exists (f-exists-p filename))
		       (is-golang (f-ext-p filename "go"))
		       (short-filename (f-collapse-homedir filename))
		       (basename (f-filename filename)))
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
             (basename  (f-filename filename))
             (short-path (f-collapse-homedir directory))
             (test-names (elgot-test-at-point--go-names-in-buffer)))
   (->> test-names
        (--flat-map
         (let* ((test-name (car it))
                (is-bench  (eq (cdr it) 'benchmark))
                (run-flags (if is-bench
                               (format "-bench=^%s$ -run=^$" test-name)
                             (format "-run=^%s$" test-name)))
                (type-name (if is-bench "benchmark" "test")))
           (->> '(("" "")
                  ("-race" "+race"))
                (--flat-map
                 (let ((race-flag  (car it))
                       (race-label (cadr it)))
		   (->> '("10s" "30s" "1m")
			(--map
			 (let ((timeout-flag (format "-timeout=%s" it))
			       (timeout-label it))
                           (make-builder-candidate
                            :name       (s-join-with-space "go test -v" race-label timeout-label test-name basename)
                            :command    (s-join-with-space "go test -v" race-flag timeout-flag run-flags ".")
                            :directory  directory
                            :annotation (format "run %s %s in %s" type-name test-name short-path)
                            :priority   0))))))))))))

(builder-register-candidates
 :name "go-modules"
 :pipeline (let ((go-mod-directories (f-directories-containing-file-go-mod directories)))
	     (->> go-mod-directories
		(--flat-map
		 (let ((filename (f-filename it))
		       (directory it))
		    (->> '(("lint run"         "golangci-lint run"        "run `golangci-lint' in package")
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
			 (-append
			  (unless (eql 1 (length go-mod-directories))
			    '(("<mod> | xargs go test -race"
			           "find . -name 'go.mod' | xargs --verbose -I{} bash -c 'pushd $(dirname {}); go test -race -cover ./...'"
				   "run tests for all modules and submodules in")
			      ("<mod> | xargs go build ./..."
			           "find . -name 'go.mod' | xargs --verbose -I{} bash -c 'pushd $(dirname {}); go build ./...'"
				   "run builds for all modules and submodules in"))))
			 (-non-nil)
			 (--map (let ((name (if (car it) (car it) (cadr it)))
				      (command (cadr it))
				      (annotation (caddr it)))
				  (make-builder-candidate
				   :name name
				   :command command
				   :directory directory
				   :annotation (s-join-with-space annotation filename))))))))))

(builder-register-candidates
 :name "py-projects"
 :pipeline (->> (f-directories-containing-file-pyproject-toml directories)
		(--flat-map
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
		   :annotation (format "run `ruff analyze' in package %s" it))))))

(builder-register-candidates
 :name "python"
 :pipeline (->> (f-directories-containing-file-with-extension-py directories)
		(--flat-map
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
		   :annotation (format "run `isort' in the directory %s (single line imports)" it))))))

(builder-register-candidates
 :name "rust-project"
 :pipeline (->> (f-directories-containing-file-cargo-toml directories)
		(--flat-map
		 (let ((annotation-directory (f-collapse-homedir it))
		       (directory it))
		   (->> '("build" "check" "test" "fix" "fmt" "clippy" "clippy --fix")
			(--map
			 (if (f-equal-p project-root-directory directory)
			     (make-builder-candidate
			      :directory directory
			      :command (format "cargo %s" it)
			      :annotation (format "run cargo target '%s' in project root (%s)" it annotation-directory))
			   (make-builder-candidate
			    :directory directory
			    :name (format "cargo %s <%s>" it (f-base directory))
			    :command (format "cd %s; cargo %s" directory it)
			    :annotation (format "run cargo target '%s' in package (%s)" it annotation-directory)))))))
		(-concat
		 (when (f-directories-containing-file-cargo-toml project-root-directory)
		   (let ((annotation-directory (f-collapse-homedir project-root-directory)))
		     (->> '("libs" "bins" "examples" "tests" "benches" "all-targets")
			  (--flat-map
			   (list
			    (make-builder-candidate
			     :directory project-root-directory
			     :command (concat "cargo build --" it)
			     :annotation (format "build %s in project root (%s)" it annotation-directory))
			    (make-builder-candidate
			     :directory project-root-directory
			     :command (concat "cargo check --" it)
			     :annotation (format "run static analysis %s in project root (%s)" it annotation-directory))))))))))

(builder-register-candidates
 :name "justfile"
 :pipeline (->> (f-directories-containing-file-justfile directories)
		(--flat-map
		 (let ((directory it)
		       (annotation-directory (f-collapse-homedir it)))
		   (->> (when-let* ((default-directory directory)
				    (output (shell-command-to-string "just --summary"))
				    (candidates (split-string output)))
			  candidates)
			(--map (make-builder-candidate
				:directory directory
				:command (format "just %s" it)
				:annotation (format "exec justfile target %s in %s" it annotation-directory))))))))

(defun builder--vale-insert-statistics (buffer _message &key filename)
  (interactive)
  (let* ((table (json-parse-string (shell-command-to-string (format "vale ls-metrics --output=line %s" filename)))))
    (insert (format "\nstatistics for %s" (propertize (f-collapse-homedir filename) 'face 'italic)))
    (ht-map (lambda (key value) (insert (format "\n%s:%s" (propertize key 'face 'bold) value)))
	    table)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; emacs-lisp -- static analysis and linting targets

(defun builder--el-files-in-directory (directory)
  "Return a list of .el files in DIRECTORY, excluding byte-compiled .elc files."
  (->> (f-entries directory #'f-file-p)
       (--filter (f-ext-p it "el"))))

(builder-register-candidates
 :name "emacs-lisp-file"
 :pipeline
 (when-let* ((filename (buffer-file-name))
             (_ (derived-mode-p 'emacs-lisp-mode))
             (short-name (f-collapse-homedir filename))
             (basename (f-filename filename))
             (directory (f-dirname filename))
             (load-path-args (s-join " " (--map (format "-L %s" it)
                                                (-filter #'f-directory-p load-path)))))
   (-l (make-builder-candidate
        :name (format "byte-compile %s" basename)
        :command (format "emacs --batch %s -f batch-byte-compile %s" load-path-args filename)
        :directory directory
        :annotation (format "byte-compile %s" short-name))
       (make-builder-candidate
        :name (format "relint %s" basename)
        :command (format "emacs --batch %s --eval \"(require 'relint)\" -f relint-batch %s" load-path-args filename)
        :directory directory
        :annotation (format "check regexps in %s" short-name))
       (make-builder-candidate
        :name (format "package-lint %s" basename)
        :command (format "emacs --batch %s --eval \"(require 'package-lint)\" -f package-lint-batch-and-exit %s" load-path-args filename)
        :directory directory
        :annotation (format "lint package metadata in %s" short-name))
       (make-builder-candidate
        :name (format "elisp-lint %s" basename)
        :command (format "emacs --batch %s --eval \"(require 'elisp-lint)\" -f elisp-lint-files-batch %s" load-path-args filename)
        :directory directory
        :annotation (format "run elisp-lint on %s" short-name))
       (make-builder-candidate
        :name (format "elsa %s" basename)
        :command (format "emacs --batch %s --eval \"(require 'elsa)\" -f elsa-run %s" load-path-args filename)
        :directory directory
        :annotation (format "run elsa type analysis on %s" short-name)))))

(builder-register-candidates
 :name "emacs-lisp-project"
 :pipeline
 (when-let* ((_ (derived-mode-p 'emacs-lisp-mode))
             (el-files (builder--el-files-in-directory project-root-directory))
             (_ el-files)
             (load-path-args (s-join " " (--map (format "-L %s" it)
                                                (-filter #'f-directory-p load-path))))
             (file-args (s-join " " el-files)))
   (-l (make-builder-candidate
        :name (format "byte-compile <%s>" project-name)
        :command (format "emacs --batch %s -f batch-byte-compile %s" load-path-args file-args)
        :directory project-root-directory
        :annotation (format "byte-compile all .el files in %s" project-name))
       (make-builder-candidate
        :name (format "relint <%s>" project-name)
        :command (format "emacs --batch %s --eval \"(require 'relint)\" -f relint-batch %s" load-path-args file-args)
        :directory project-root-directory
        :annotation (format "check regexps across all .el files in %s" project-name))
       (make-builder-candidate
        :name (format "package-lint <%s>" project-name)
        :command (format "emacs --batch %s --eval \"(require 'package-lint)\" -f package-lint-batch-and-exit %s" load-path-args file-args)
        :directory project-root-directory
        :annotation (format "lint package metadata across all .el files in %s" project-name))
       (make-builder-candidate
        :name (format "elisp-lint <%s>" project-name)
        :command (format "emacs --batch %s --eval \"(require 'elisp-lint)\" -f elisp-lint-files-batch %s" load-path-args file-args)
        :directory project-root-directory
        :annotation (format "run elisp-lint across all .el files in %s" project-name))
       (make-builder-candidate
        :name (format "elsa <%s>" project-name)
        :command (format "emacs --batch %s --eval \"(require 'elsa)\" -f elsa-run %s" load-path-args file-args)
        :directory project-root-directory
        :annotation (format "run elsa type analysis across all .el files in %s" project-name)))))

(provide 'builder)
;;; builder.el ends here
