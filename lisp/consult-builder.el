;; -*- lexical-binding: t -*-

(require 'compile)

(require 'f)
(require 's)
(require 'ht)
(require 'dash)

(require 'consult)

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

(when (boundp 'tychoish-core-map)
  (bind-keys :map 'tychoish-core-map
	     ("c" . consult-builder)))

(defun consult-builder ()
  "Run compile operation selecting compile buffer and commands."
  (interactive)
  (tychoish/compile-project))

;;;###autoload
(defun tychoish/compile-project (&optional name command)
  (let* ((compilation-buffer-candidates (project-compilation-buffers :name name))
         (longest-key (length-of-longest-item (ht-keys compilation-buffer-candidates)))
         (op-name (consult--read
                   (ht-keys compilation-buffer-candidates)
                   :prompt "compilation buffer => "
                   :require-match nil
                   :annotate (lambda (key)
			       (concat (prefix-padding-for-annotation key longest-key)
                                       (ht-get compilation-buffer-candidates key)))))
         (compile-buf (get-buffer op-name))
	 (project-root-directory (approximate-project-root))
	 (start-at (current-time))
	 candidates
	 candidate-name)

    (save-some-buffers
     ;; save with now questions:
     t
     ;; only consider files (nil)
     (lambda () (let ((filename (buffer-file-name)))
		  (and filename (file-in-directory-p filename project-root-directory)))))

    (if compile-buf
        (with-current-buffer compile-buf
          (when (or current-prefix-arg command)
            (setq compilation-arguments nil))

          (let* ((cc-result (tychoish--compilation-read-command (or command compile-command)))
		 (selection-name (setq candidate-name (car cc-result)))
		 (table (setq candidates (cdr cc-result)))
		 (candidate (ht-get table selection-name))
		 (compile-command (tychoish-compilation-candidate-command candidate))
		 (default-directory (or (tychoish-compilation-candidate-directory candidate) default-directory)))

            (recompile current-prefix-arg)))

      (let* ((cc-result (tychoish--compilation-read-command command))
	     (selection-name (setq candidate-name (car cc-result)))
	     (table (setq candidates (cdr cc-result)))
	     (candidate (ht-get table selection-name))
	     (compile-command (tychoish-compilation-candidate-command candidate))
	     (default-directory (or (tychoish-compilation-candidate-directory candidate) default-directory)))

	(compilation-start
	 compile-command                    ;; the command
	 'compilation-mode                  ;; the default
	 (compile-buffer-name op-name))))

    (with-current-buffer (or compile-buf op-name)
      (add-hygenic-one-shot-hook
       :name (format "%s notification hook" op-name)
       :hook 'compilation-finish-functions
       :local t
       :make-unique t
       :args (compilation-buffer msg)
       :form (tychoish/compile--post-hook-collection
	      op-name (buffer-name compilation-buffer) start-at
	      :process-name "sardis-notify"
	      :program "sardis"
	      :args '("notify" "send" msg)
	      :send-when (or current-prefix-arg
			     (< 30 (float-time (time-since (current-idle-time))))
			     (not (get-buffer-window (or compile-buf op-name) t))))))

      (when-let* ((candidate (ht-get candidates candidate-name))
		  (hook (tychoish-compilation-candidate-hook candidate)))
	(when hook
      	  (add-hygenic-one-shot-hook
      	   :name (format "post-%s-hook-operation" op-name)
      	   :hook 'compilation-finish-functions
      	   :make-unique t
      	   :local t
      	   :function (hook))))

      (let*
	  ((op-window
	    (and t
		 (get-buffer-window (current-buffer) (selected-frame)))))
	(if op-window (select-window op-window)
	  (switch-to-buffer-other-window (current-buffer))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; directory selection
(cl-defun consult--select-directory (&optional &key input-dirs (require-match nil))
  "Select a directory from a provided or likely set of `INPUT-DIRS`'."
  (consult--read
   (or (clean-directory-options-for-selection input-dirs)
       (get-directory-default-candidate-list))
   :sort nil
   :command this-command
   :require-match require-match
   :prompt "in directory =>> "))

;;;###autoload
(defun compilation-buffer-change-directory ()
  "Change the directory for the current compilation buffer."
  (interactive)
  (unless (derived-mode-p 'compilation-mode)
    (user-error "operation is only applicable for COMPILATION-MODE buffers"))
  (let ((directory (or (consult--select-directory
			:input-dirs (-distinct (append (list compilation-directory default-directory)
						       (get-directory-default-candidate-list)
						       (f-directories (approximate-project-root)))))
		       compilation-directory
		       default-directory)))
    (setq-local default-directory directory
		compilation-directory directory)))

(defun clean-directory-options-for-selection (input)
  "Process `INPUT' list removing: duplicates, nils, and empty or whitespace elements."
  (when input
    (->> input
	 (-keep #'trimmed-string-or-nil)
	 (--map (or (when (f-absolute-p it) it)
		    (expand-file-name it)))
	 (f-distinct))))

(defun get-directory-parents (&optional start stop)
  "Generate list of intermediate paths between `START' and `STOP'."
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
	 (f-filter-directories '(cannonicalize unique) output))))

(defun get-directory-default-candidate-list ()
  (let ((proj-root (approximate-project-root)))
    (--> (append
	  (get-directory-parents default-directory proj-root)
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

(defun f-distinct (sequence)
  (let ((-compare-fn #'f-equal-p))
    (-distinct sequence)))

(defun option-set-p (opt options)
  (or (eq opt options)
      (and (listp options) (memq opt options))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tychoish/compile-project implementation

;; this is the inner "select which command to use" for entering a new compile command.
(defun tychoish--compilation-read-command (&optional command table)
  (let* ((candidates (or table
			 (tychoish--get-compilation-candidates default-directory command)))
         (names (->> candidates
                     (ht-values)
		     (--map (tychoish-compilation-candidate-name it))
                     (-sort #'string-lessp)))
         (longest-id (length-of-longest-item names))
         (selection-name
          (consult--read
           names
           :prompt "compile command => "
           :command this-command
           :history 'compile-history
           :annotate (lambda (key) (format "%s%s" (prefix-padding-for-annotation key longest-id)
                                           (tychoish-compilation-candidate-annotation (ht-get candidates key))))))
	 (candidate (ht-get candidates selection-name))
	 (operation-name (if candidate
			     (tychoish-compilation-candidate-name candidate)
			   (tychoish-cc-add-to-table
			    candidates
			    (make-compilation-candidate
			     :command selection-name
			     :directory default-directory
			     :annotation "user input"))
			   selection-name)))
    (cons operation-name candidates)))

(cl-defun project-compilation-buffers (&optional &key name (project (approximate-project-name)))
  "Find "
  (let ((buffer-table (ht-create))
        (default-names (list "push" "gen" "buf" "benchmark" "check" "compile" "run" "lint" "test" "build")))
    (when name
      (cl-pushnew name default-names :test #'equal))

    (--each ;; existing-compilation-buffers-for-project
        (mode-buffers-for-project
         :mode 'compilation-mode)

      (with-current-buffer it
        (ht-set
         buffer-table
         (buffer-name it)
         (format "reuse buffer: project=%s errors=%s lines=%s command='%s'"
                 project
                 compilation-num-errors-found
                 (buffer-line-count)
                 (string-trim (or (car compilation-arguments) ""))))))

    (--each default-names ;; default-compilation-buffer-names-for-project
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

;; compilation candidate discovery

(cl-defstruct (tychoish-compilation-candidate
               (:constructor nil) ;; disable default
               (:constructor make-compilation-candidate (&key command (name command) annotation (directory default-directory) hook notification-threshold)))
  "Structure for compilation candidates"
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
   :documentation "description of command, used for marginalia annotations."
   :type string)
  (hook
   nil
   :documentation "a function to run when the compilation completes"
   :type function)
  (notification-threshold
   300
   :documentation "if a command runs longer than this number of seconds, send a notification when the compile completes."
   :type integer))

(defun tychoish-cc-add-to-table (table candidate)
  (ht-set table (tychoish-compilation-candidate-name candidate) candidate))

(defun add-candidates-to-table (table candidates)
  (->> candidates
       (-filter #'tychoish-compilation-candidate-p)
       (--mapc (tychoish-cc-add-to-table table it))))

(defvar tychoish-compilation-candidate-functions nil
  "A List of functions that populate a table of possible completion commands

All functions are called with (PROJECT-ROOT-DIRECTORY DIRECTORIES TABLE)
as arguments where the `project-root-directory' is the root of the
current project, `directories' are all of the directories between the
current directory and the project root, and `table' is table of `tychoish--completion-candiate' objects.")

;;;###autoload
(cl-defmacro register-compilation-candidates (&key name (predicate t) (hooks nil) pipeline)
  (let ((symbol-name (intern (format "tychoish-compilation-candidates-for-%s" name)))
	(hook-registering-function-name (intern (format "tychoish-compilation-candidate-registrar-for-%s" name))))
    `(progn
       (defun ,symbol-name (project-root-directory project-name directories operation-table)
	 ,(format "Build list of `tychoish-compilation-candidate' objects for suggestion in compilation buffers")
	 (ignore project-root-directory project-name directories operation-table)
	 (when ,predicate
	   (add-candidates-to-table
	    operation-table
	    ,pipeline))
	 t)

       ,(if (eql 0 (length hooks))
	    `(add-hook 'tychoish-compilation-candidate-functions #',symbol-name)
	  `(let ((hooks ,hooks))
	     (defun ,hook-registering-function-name ()
	       (add-hook 'tychoish-compilation-candidate-functions #',symbol-name 0 t))

	     (->> hooks (--mapc (add-hook it ',hook-registering-function-name))))))))

(defun tychoish--get-compilation-candidates (&optional directory command)
 "Generate a sequence of candidate compilation commands based on mode and directory structure."
  (let* ((project-root-directory (approximate-project-root))
	 (project-name (f-filename project-root-directory))
	 (directory (when (boundp 'directory) directory)) ;;  maybe this should be a macro
	 (default-directory (or directory default-directory))
         (directories (->> (approximate-project-buffers)
			   (-keep #'buffer-directory)
			   (-append (get-directory-parents default-directory project-root-directory))
			   (-filter #'f-directory-p)
			   (-map #'f-full)
			   (-distinct)))
	 (operation-table (ht-create)))

    (when command
      (tychoish-cc-add-to-table
       operation-table
       (make-compilation-candidate
    	:command command
    	:annotation "runtime suggested candidate"
    	:directory (or directory project-root-directory default-directory))))

    (run-hook-with-args
     'tychoish-compilation-candidate-functions
        project-root-directory
	project-name
	directories
	operation-table)

    operation-table))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; candidate discovery/generator registration

(register-compilation-candidates
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
					    (format "<%s>/%s" project-name (string-remove-suffix project-root-directory directory))
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
				 (make-compilation-candidate
				  :name (format name-template flag)
				  :command (format command-template flag)
				  :directory directory
				  :annotation (s-join-with-space annotation-template annotation-suffix)))))))))

(register-compilation-candidates
 :name "minibuffer-shell-commands"
 :pipeline (->> (minibuffer-default-add-shell-commands)
		(--map
		 (if (string-equal default-directory project-root-directory)
		     (make-compilation-candidate
		      :command it
		      :directory project-root-directory
		      :annotation (format "operation from `'minibuffer-shell-commands' in the project root (%s)" project-root-directory))
		   (make-compilation-candidate
		    :command it
		    :directory default-directory
		    :annotation (format "operation from `'minibuffer-shell-commands' in the current directory (%s)" default-directory))))))

(register-compilation-candidates
 :name "shell-command-history"
 :pipeline (->> shell-command-history
		(--map
		 (if (string-equal default-directory project-root-directory)
		     (make-compilation-candidate
		      :command it
		      :directory project-root-directory
		      :annotation (format "operation from `'shell-command-history' in the project root (%s)" project-root-directory))
		   (make-compilation-candidate
		    :command it
		    :directory default-directory
		    :annotation (format "operation from `'shell-command-history' in the current directory (%s)" default-directory))))))

(register-compilation-candidates
 :name "project-compilation-buffer-commands"
 :pipeline (->> (mode-buffers-for-project
		 :mode 'compilation-mode)
		(--keep
		 (with-current-buffer it
		   (let ((key (trimmed-string-or-nil (car compilation-arguments))))
		     (when key
		       (cons key (buffer-name))))))
		(--map
		 (make-compilation-candidate
		  :command (car it)
		  :directory project-root-directory
		  :annotation (format "run %s, from compile buffer %s in the project root (%s) " (car it) (cdr it) project-root-directory)))))

(register-compilation-candidates
 :name "go-packages"
 :pipeline (->> (f-directories-containing-file-with-extension-go directories)
		(--flat-map
		 (let* ((prefix (concat "." (f-path-separator)))
			(directory it)
			(operation-directory (cond
			      ((or (f-equal-p directory project-root-directory) (f-directory-contains-go-mod-file directory)) "./")
			      ((string-prefix-p prefix directory) directory)
			      ((string-prefix-p (f-path-separator) directory) directory)
			      (t (concat prefix directory))))
			(dir-with-dots (f-join operation-directory "..." )))
		   (->> (-l dir-with-dots operation-directory)
			(--flat-map
			 (let* ((build-path it)
				(short-path (f-collapse-homedir it))
				(package-path (string-replace project-root-directory "" build-path))
				(is-recursive (string-suffix-p "..." it))
				(path-for-project-tag (cond ((f-equal-p build-path project-root-directory) "")
							    ((equal short-path package-path) "")
							    (t package-path)))
				(proj-path-tag (format "<%s>/%s" project-name path-for-project-tag))
				(proj-path-for-name (s-join-with-space (format "<%s>/%s" project-name
									       (f-visually-compress-to-four path-for-project-tag)
									       (if is-recursive " [recursive]" ""))))
				(annotation-tag (if is-recursive
						    "(with subdirectories)"
						  "")))

			   (-append
			    (->> '("revive" "reassign" "prealloc" "predeclared" "nosprinthostport" "thelper" "makezero" "importas" "fatcontext" "exptostd" "exhaustruct")
				 (--map (make-compilation-candidate
					 :name (format "run lint %s %s" it proj-path-for-name)
					 :command (format "golangci-lint run --enable-only=%s %s" it build-path)
					 :directory directory
					 :annotation (format "run (only) the %s linter in %s" it short-path))))
			    (-- (make-compilation-candidate
				 :name (s-join-with-space "go build" proj-path-for-name)
				 :command (s-join-with-space "go build" build-path)
				 :directory directory
				 :annotation (s-join-with-space "build in" project-name "at" short-path annotation-tag)))
			    (->> '(("go test -v"                "verbose mode")
				   ("go test -v -cover"         "the code coverage collector in verbose mode")
				   ("go test -v -race"          "the race detector in verbose mode")
				   ("go test -v -cover -race"   "the race detector and collecting coverage data in verbose mode")
				   ("go test -cover"            "the code coverage collector")
				   ("go test -race"             "the race detector")
				   ("go test -race -cover"      "the race detector and collecting coverage data")
				   ("go test"                   "default options")
				   ("go test -run=NOOP"         "building all sources, including tests, without running tests"))
				 (--flat-map
				  (let ((command-prefix (car it))
					(annotation-prefix (cadr it)))
				     (->> '("" "10s" "20s" "30s" "40s" "1m" "90s" "2m" "4m" "8m")
					  (--map
					   (let* ((timeout-spec it)
						  (is-default (equal timeout-spec ""))
						  (timeout-arg (unless is-default (concat "--timeout=" timeout-spec)))
						  (timeout-name (if is-default "no timeout" (s-join-with-space "a" timeout-spec "timeout"))))
					     (make-compilation-candidate
					      :name (s-join-with-space command-prefix timeout-spec proj-path-for-name)
					      :command (s-join-with-space command-prefix timeout-arg build-path)
					      :directory directory
					      :annotation (format "run go test in %s with %s and %s" short-path annotation-prefix timeout-name annotation-tag))))))))))))))))

(register-compilation-candidates
 :name "go-files"
 :pipeline (when-let* ((filename (if (and (buffer-file-name) (derived-mode-p '(go-mode go-ts-mode)))
				     (buffer-file-name)
				   (thing-at-point 'filename)))
		       (exists (f-exists-p filename))
		       (is-golang (f-ext-p filename "go"))
		       (short-filename (f-collapse-homedir filename))
		       (basename (f-filename filename)))
	     (-- (make-compilation-candidate
		  :name (format "gofumpt +extra %s" basename)
		  :directory (f-dirname filename)
		  :command (format "gofumpt -extra -w %s" filename)
		  :annotation (format "run gofumpt with non-extra constraints on %s" short-filename))
		 (make-compilation-candidate
		  :name (format "gofumpt %s" basename)
		  :directory (f-dirname filename)
		  :command (format "gofumpt -w %s" filename)
		  :annotation (format "run gofumpt with standard gofmt constraints on %s" short-filename))
		 (make-compilation-candidate
		  :name (format "gci %s <import sort>" basename)
		  :directory (f-dirname filename)
		  :command (format "golangci-lint fmt --enable gci %s" filename)
		  :annotation (format "sort imports with gci for %s" short-filename))
		 (make-compilation-candidate
		  :name (format "meta fmt %s" basename)
		  :directory (f-dirname filename)
		  :command (format "golangci-lint fmt %s" filename)
		  :annotation (format "run meta formatter on %s" short-filename)))))

(register-compilation-candidates
 :name "go-modules"
 :pipeline (let ((go-mod-directories (f-directories-containing-file-go-mod directories)))
	     (->> go-mod-directories
		(--flat-map
		 (let ((filename (f-filename it))
		       (directory it))
		    (->> '((nil                "golangci-lint run"        "run `golangci-lint' in package")
			   (nil                "golangci-lint run --fix"  "run `golangci-lint' and fix trivial errors in package")
			   (nil                "go mod tidy"              "run `go mod tidy' in package")
			   (nil                "go doc -all"              "go doc for entire package")
			   ("go doc -outline"  "go doc --"                "go doc outline for package")
			   ("<pkgs...> | xargs go test -race -v"
			        "go list -f '{{ if (or .TestGoFiles .XTestGoFiles) }}{{ .ImportPath }}{{ end }}' ./... | xargs --verbose go test -race -v"
			        "run all tests (with the race detector) for all submodules test")
			   ("<pkgs...> | xargs go test -run=NOOP ./..."
			        "go list -f '{{ if (or .TestGoFiles .XTestGoFiles) }}{{ .ImportPath }}{{ end }}' ./... | xargs --verbose go test -run=NOOP"
				"build all tests in all packages and sub-packages for"))
			 (-append
			  (when (or (eql 1 (length go-mod-directories))
				    (f-equal-p directory project-root-directory))
			    '(("<modules> | xargs go test -race -v"
			           "find . -name 'go.mod' | xargs --verbose -I{} bash -c 'pushd $(dirname {}); go test -race -v ./...'"
				   "run tests for all modules and submodules in")
			      ("<modules> | xargs go test -run=NNOP ./..."
			           "find . -name 'go.mod' | xargs --verbose -I{} bash -c 'pushd $(dirname {}); go test -run=NOOP ./...'"
				   "run tests for all modules and submodules in")
			      ("<modules> | xargs go build ./..."
			           "find . -name 'go.mod' | xargs --verbose -I{} bash -c 'pushd $(dirname {}); go build ./...'"
				   "run tests for all modules and submodules in"))))
			 (-non-nil)
			 (--map (let ((name (if (car it) (car it) (cadr it)))
				      (command (cadr it))
				      (annotation (caddr it)))
				  (make-compilation-candidate
				   :name name
				   :command command
				   :directory directory
				   :annotation (s-join-with-space annotation filename))))))))))

(register-compilation-candidates
 :name "py-projects"
 :pipeline (->> (f-directories-containing-file-pyproject-toml directories)
		(--flat-map
		 (list
		  (make-compilation-candidate
		   :command "ruff check"
		   :directory it
		   :annotation (format "run `ruff check' in package %s" it))
		  (make-compilation-candidate
		   :name "ruff check fix"
		   :command "ruff check --fix --show-fixes"
		   :directory it
		   :annotation (format "run `ruff check' and automatically fix lint violations in package %s" it))
		  (make-compilation-candidate
		   :name "ruff check fix +unsafe"
		   :command "ruff check --fix --unsafe-fixes --show-fixes"
		   :directory it
		   :annotation (format "render all unsafe fixes (which may modify the intent of the code) in package %s" it))
		  (make-compilation-candidate
		   :name "ruff format"
		   :command "ruff format --respect-gitignore"
		   :directory it
		   :annotation (format "run `ruff format' in package %s" it))
		  (make-compilation-candidate
		   :command "ruff analyze"
		   :directory it
		   :annotation (format "run `ruff analyze' in package %s" it))))))

(register-compilation-candidates
 :name "python"
 :pipeline (->> (f-directories-containing-file-with-extension-py directories)
		(--flat-map
		 (list
		  (make-compilation-candidate
		   :command (format "black %s" it)
		   :directory it
		   :annotation (format "run `black' in the directory %s" it))
		  (make-compilation-candidate
		   :command (format "isort %s" it)
		   :directory it
		   :annotation (format "run `isort' in the directory %s" it))
		  (make-compilation-candidate
		   :command (format "isort --force-single-line-imports %s" it)
		   :directory it
		   :annotation (format "run `isort' in the directory %s (single line imports)" it))))))

(register-compilation-candidates
 :name "rust-project"
 :pipeline (->> (f-directories-containing-file-cargo-toml directories)
		(--flat-map
		 (let ((annotation-directory (f-collapse-homedir it))
		       (directory it))
		   (->> '("build" "check" "test" "fix" "fmt" "clippy" "clippy --fix")
			(--map
			 (if (f-equal-p project-root-directory directory)
			     (make-compilation-candidate
			      :directory directory
			      :command (format "cargo %s" it)
			      :annotation (format "run cargo target '%s' in project root (%s)" it annotation-directory))
			   (make-compilation-candidate
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
			    (make-compilation-candidate
			     :directory project-root-directory
			     :command (concat "cargo build --" it)
			     :annotation (format "build %s in project root (%s)" it annotation-directory))
			    (make-compilation-candidate
			     :directory project-root-directory
			     :command (concat "cargo check --" it)
			     :annotation (format "run static analysis %s in project root (%s)" it annotation-directory))))))))))

(register-compilation-candidates
 :name "justfile"
 :pipeline (->> (f-directories-containing-file-justfile directories)
		(--flat-map
		 (let ((directory it)
		       (annotation-directory (f-collapse-homedir it)))
		   (->> (when-let* ((default-directory directory)
				    (output (shell-command-to-string "just --summary"))
				    (candidates (split-string output)))
			  candidates)
			(--map (make-compilation-candidate
				:directory directory
				:command (format "just %s" it)
				:annotation (format "exec justfile target %s in %s" it annotation-directory))))))))

(register-compilation-candidates
 :name "current-buffer-text-file"
 :predicate (when (and (buffer-file-name)
		       (derived-mode-p '(org-mode markdown-mode rst-mode)))
 	      (save-buffer) t)
 :pipeline (let ((filename (buffer-file-name)))
	     (-- (make-compilation-candidate
		  :directory default-directory
		  :command (format "vale --output=line %s" filename)
		  :name (format "vale check %s" (f-collapse-homedir filename))
		  :annotation "find errors in the current file"
		  :hook (lambda (buffer msg) (with-current-buffer buffer (tychoish--vale-insert-statistics filename))))
		 (make-compilation-candidate
		  :directory default-directory
		  :command (format "vale --ls-metrics %s" filename)
		  :name (format "vale report %s" (f-collapse-homedir filename))
		  :annotation "report statics about the current file"))))

(defun tychoish--vale-insert-statistics (buffer _message &key filename)
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

(provide 'consult-builder)
