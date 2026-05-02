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

(require 'annotated-completing-read)

(require 'tychoish-common)

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

(when (boundp 'tychoish/core-map)
  (bind-keys :map tychoish/core-map
	     ("c" . builder)))

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

(defun builder-reset-finish-hooks ()
  (interactive)
  (->> (mode-buffers 'compilation-mode)
       (--mapc (with-current-buffer it
		 (setq-local compilation-finish-functions nil)))))

;;;###autoload
(defun builder-compile-project (&optional name command)
  (let* ((compilation-buffer-candidates (builder--project-compilation-buffers :name name))
         (op-name (annotated-completing-read
                   compilation-buffer-candidates
                   :prompt "compilation buffer => "
                   :require-match nil))
         (compile-buf (get-buffer op-name))
	 (project-root-directory (approximate-project-root))
	 (start-at (current-time))
	 candidates
	 candidate-name)

    (save-some-buffers
     t
     (lambda () (let ((filename (buffer-file-name)))
		  (and filename (file-in-directory-p filename project-root-directory)))))

    (if (and compile-buf (y-or-n-p "recompile?"))
        (with-current-buffer compile-buf
            (recompile current-prefix-arg))
      (let* ((cc-result (builder--read-command command))
	     (selection-name (setq candidate-name (car cc-result)))
	     (table (setq candidates (cdr cc-result)))
	     (candidate (ht-get table selection-name))
	     (compile-command (read-from-minibuffer "edit command => " (builder-candidate-command candidate)))
	     (default-directory (or (builder-candidate-directory candidate) default-directory)))

	(compilation-start
	 compile-command
	 'compilation-mode
	 (compile-buffer-name op-name))))

    (with-current-buffer (or compile-buf op-name)
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
			     (not (get-buffer-window (or compile-buf op-name) t))
			     (< 10 (float-time (time-since (current-idle-time)))))))

      (when-let* ((candidate (ht-get candidates candidate-name))
		  (hook (builder-candidate-hook candidate)))
	(when hook
	  (add-one-shot-hook :name (format "post-%s-hook-operation" op-name)
	   :hook 'compilation-finish-functions
	   :make-unique t
	   :local t
	   :function (hook))))

      (let* ((op-window
	      (and t (get-buffer-window (current-buffer) (selected-frame)))))
	(if op-window (select-window op-window)
	  (switch-to-buffer-other-window (current-buffer)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; directory selection

(cl-defun builder--select-directory (&optional &key input-dirs (require-match nil))
  "Select a directory from a provided or likely set of `INPUT-DIRS'."
  (let* ((dirs (or (builder--clean-directory-options input-dirs)
                   (builder--directory-default-candidates)))
         (project-root (approximate-project-root))
         (tbl (ht-create)))
    (--each dirs
      (ht-set tbl it
              (cond
               ((f-equal-p it default-directory) "current directory")
               ((f-equal-p it project-root)      "project root")
               ((f-ancestor-p it default-directory) "parent")
               ((f-ancestor-p default-directory it) "child")
               ((f-equal-p (f-parent it) (f-parent default-directory)) "sibling")
               (t ""))))
    (annotated-completing-read tbl
                               :prompt "in directory =>> "
                               :require-match require-match)))

;;;###autoload
(defun builder-change-directory ()
  "Change the directory for the current compilation buffer."
  (interactive)
  (unless (derived-mode-p 'compilation-mode)
    (user-error "operation is only applicable for COMPILATION-MODE buffers"))
  (let ((directory (or (builder--select-directory
			:input-dirs (-distinct (append (list compilation-directory default-directory)
						       (builder--directory-default-candidates)
						       (f-directories (approximate-project-root)))))
		       compilation-directory
		       default-directory)))
    (setq-local default-directory directory
		compilation-directory directory)))

(defun builder--clean-directory-options (input)
  "Process INPUT list removing duplicates, nils, and empty or whitespace elements."
  (->> input
       (-keep #'s-trimmed-or-nil)
       (--map (or (when (f-absolute-p it) it)
		  (expand-file-name it)))
       (f-distinct)))

(defun builder--directory-parents (&optional start stop)
  "Generate list of intermediate paths between START and STOP."
  (let* ((start (or start default-directory))
	 (stop (or stop "~/"))
	 (stop-path (expand-file-name (string-trim stop)))
         (current (expand-file-name (string-trim start)))
         (output (list stop-path current)))
    (while (and
	    current
	    (or (not (string= current stop-path))
		(not (string-prefix-p stop-path current))))
      (setq current (file-name-parent-directory current))
      (push current output))
    (->> output
	 (f-filter-directories '(cannonicalize unique)))))

(defun builder--directory-default-candidates ()
  (let ((proj-root (approximate-project-root)))
    (--> (append
	  (builder--directory-parents default-directory proj-root)
	  (approximate-project-buffers)
	  (list (thing-at-point 'filename)
		      (thing-at-point 'existing-filename)
		      default-directory
		      user-emacs-directory
		      user-home-directory))
	 (if (or (and (length< it 16)
		      (not (f-equal-p user-home-directory proj-root)))
		 current-prefix-arg)
	     (-join (f-directories proj-root) it)
	   it)
	 (f-filter-directories '(cannonicalize unique) it))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; builder-compile-project implementation

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
           :history 'compile-history))
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
				   notification-threshold)))
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
   :type integer))

(defun builder--add-candidate (table candidate)
  (ht-set table (builder-candidate-name candidate) candidate))

(defun builder--add-candidates (table candidates)
  (->> candidates
       (-filter #'builder-candidate-p)
       (--mapc (builder--add-candidate table it))))

(defvar builder-candidate-functions nil
  "List of functions that populate a table of possible compilation commands.
All functions are called with (PROJECT-ROOT-DIRECTORY PROJECT-NAME DIRECTORIES TABLE)
where TABLE is a hash of `builder-candidate' objects.")

;;;###autoload
(cl-defmacro builder-register-candidates (&key name (predicate t) (hooks nil) pipeline)
  (let ((symbol-name (intern (format "builder-candidates-for-%s" name)))
	(hook-registering-function-name (intern (format "builder-candidate-registrar-for-%s" name))))
    `(progn
       (defun ,symbol-name (project-root-directory project-name directories operation-table)
	 ,(format "Build list of `builder-candidate' objects for suggestion in compilation buffers.")
	 (ignore project-root-directory project-name directories operation-table)
	 (when ,predicate
	   (builder--add-candidates
	    operation-table
	    ,pipeline))
	 t)

       ,(if (eql 0 (length hooks))
	    `(add-hook 'builder-candidate-functions #',symbol-name)
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
	   (directory (when (boundp 'directory) directory))
	   (default-directory (or directory default-directory))
           (directories (->> (approximate-project-buffers)
			     (-keep #'buffer-directory)
			     (-filter #'f-directory-p)
			     (-map #'f-full)
			     (-append (builder--directory-parents default-directory project-root-directory))
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
       (-keep #'builder--candidate-cache-p)
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
	     (-- (make-builder-candidate
		  :directory default-directory
		  :command (format "vale --output=line %s" filename)
		  :name (format "vale check %s" (f-collapse-homedir filename))
		  :annotation "find errors in the current file"
		  :hook (lambda (buffer msg) (builder--vale-insert-statistics buffer msg :filename filename)))
		 (make-builder-candidate
		  :directory default-directory
		  :command (format "vale --ls-metrics %s" filename)
		  :name (format "vale report %s" (f-collapse-homedir filename))
		  :annotation "report statistics about the current file"))))

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
		    :annotation (format "operation from `%s' in the %s (%s)" source annotation-tag default-directory))))))

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
		   (->> (-- project-root-directory (buffer-directory buf) default-directory)
			(-distinct)
			(-non-nil)
			(--map (let ((operation-directory it))
				 (make-builder-candidate
				  :name (format "rerun from %s [%s]" buf (f-visually-compress-to-five operation-directory))
				  :command command
				  :directory operation-directory
				  :annotation (format "command '%s' from %s in %s" command buf operation-directory)))))))))

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
		 (f-directories-containing-file-go-mod project-root-directory)
		 (-- project-root-directory))
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
		    (-- (make-builder-candidate
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
	     (-- (make-builder-candidate
		  :name (format "gofumpt +extra %s" basename)
		  :directory (f-dirname filename)
		  :command (format "gofumpt -extra -w %s" filename)
		  :annotation (format "run gofumpt with non-extra constraints on %s" short-filename))
		 (make-builder-candidate
		  :name (format "gofumpt %s" basename)
		  :directory (f-dirname filename)
		  :command (format "gofumpt -w %s" filename)
		  :annotation (format "run gofumpt with standard gofmt constraints on %s" short-filename))
		 (make-builder-candidate
		  :name (format "gci %s <import sort>" basename)
		  :directory (f-dirname filename)
		  :command (format "golangci-lint fmt --enable gci %s" filename)
		  :annotation (format "sort imports with gci for %s" short-filename))
		 (make-builder-candidate
		  :name (format "meta fmt %s" basename)
		  :directory (f-dirname filename)
		  :command (format "golangci-lint fmt %s" filename)
		  :annotation (format "run meta formatter on %s" short-filename)))))

(builder-register-candidates
 :name "go-test-file"
 :hooks '(go-ts-mode-hook go-mode-hook)
 :pipeline
 (when-let* ((filename  (buffer-file-name))
             (_         (derived-mode-p '(go-mode go-ts-mode)))
             (_         (string-suffix-p "_test.go" filename))
             (directory (f-dirname filename))
             (basename  (f-filename filename))
             (short-path (f-collapse-homedir directory))
             (test-names (tychoish/go-test-names-in-buffer)))
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
		   (->> '("" "10s" "30s" "1m")
			(--flat-map
			 (let ((timeout-flag (format "-timeout=%s" it))
			       (timeout-label it))
                           (make-builder-candidate
                            :name       (s-join-with-space "go test -v" race-label timeout-label test-name basename)
                            :command    (s-join-with-space "go test -v" race-flag timeout-flag run-flags ".")
                            :directory  directory
                            :annotation (format "run %s %s in %s" type-name test-name short-path)))))))))))))

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
  (let* ((table (json-parse-string (shell-command-to-string (format "vale ls-metrics --output=line %s" filename))))
	 (longest-key (length-of-longest-item (ht-keys table))))
    (insert (format "\nstatistics for %s" (propertize (f-collapse-homedir filename) 'face 'italic)))
    (ht-map (lambda (key value)
	      (insert (format "\n%s:%s%s"
			      (propertize key 'face 'bold)
			      (prefix-padding-for-annotation key longest-key)
			      value)))
	    table)))

(provide 'builder)
;;; builder.el ends here
