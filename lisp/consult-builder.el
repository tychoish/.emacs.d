;; -*- lexical-binding: t -*-

(require 'compile)

(require 'dash)
(require 'ht)
(require 'f)
(require 's)

(require 'consult)

(require 'tychoish-common)

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
         (compile-buf (get-buffer op-name)))

    (save-some-buffers t 'save-some-buffers-root)

    (if compile-buf
        (with-current-buffer compile-buf
          (when (or current-prefix-arg command)
            (setq compilation-arguments nil))

          (let* ((compile-command (tychoish--compilation-read-command (or command compile-command))))
            (recompile current-prefix-arg)))

      (compilation-start
       (tychoish--compilation-read-command nil)  ;; the command
       'compilation-mode                         ;; the default
       (compile-buffer-name op-name)))

    (if-let* ((op-window (get-buffer-window op-name (selected-frame))))
        (select-window op-window)
      (switch-to-buffer-other-window (get-buffer op-name)))))

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
			:input-dirs (append (list compilation-directory default-directory)
					    (get-directory-default-candidate-list)))
		       compilation-directory
		       default-directory)))
    (setq-local default-directory directory
		compilation-directory directory)))

(defun clean-directory-options-for-selection (input)
  "Process `INPUT' list removing: duplicates, nils, and empty or whitespace elements."
  (when (stringp input)
    (setq input (list input)))
  (when (listp input)
    (->> input
	 (-keep #'trimmed-string-or-nil)
	 (-map #'expand-file-name)
	 (-sort #'string-greaterp)
	 (-distinct))))

(defun get-directory-parents (start stop)
  "Generate list of intermediate paths between `START' and `STOP'."
  (let* ((stop-path (expand-file-name (string-trim stop)))
         (current (expand-file-name (string-trim start)))
         (output (list stop-path current)))
    (while (and
	    current
	    (or (not (string= current stop-path))
		(not (string-prefix-p stop-path current))))
      (setq current (file-name-parent-directory current))
      (push current output))
    (-non-nil output)))

(defun get-directory-default-candidate-list ()
  (let ((proj-root (approximate-project-root)))
    (->> (get-directory-parents default-directory (or proj-root ""))
	 (-join (list proj-root
		      (thing-at-point 'filename)
                      (thing-at-point 'existing-filename)
		      default-directory
                      user-emacs-directory
                      "~/"))
	 (-filter #'stringp)
	 (-map #'expand-file-name)
	 (-distinct))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tychoish/compile-project implementation

(defun tychoish--get-compilation-candidates (&optional directory command)
 "Generate a sequence of candidate compilation commands based on mode and directory structure."
  (let* ((project-root-directory (approximate-project-root))
	 (default-directory (or directory default-directory))
         (directories (get-directory-parents default-directory project-root-directory))
         (operation-table (ht-create)))

    (run-hook-with-args 'tychoish-compilation-candidate-functions project-root-directory directories operation-table)

    (when command
      (tychoish-cc-add-to-table
       operation-table
       (make-compilation-candidate
	:command command
	:annotation "runtime suggested candidate"
	:directory (or directory project-root-directory default-directory))))

    operation-table))

;; this is the inner "select which command to use" for entering a new compile command.
(defun tychoish--compilation-read-command (command)
  (let* ((candidates (tychoish--get-compilation-candidates default-directory command))
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
	 (selection (ht-get candidates selection-name)))

    (if selection
	(tychoish-compilation-candidate-name selection)
      selection-name)))

(cl-defun project-compilation-buffers (&optional &key name (project (approximate-project-name)))
  "Find "
  (let ((buffer-table (ht-create))
        (default-names (list "build" "compilation" "test" "lint" "check" "benchmark" "run" "push")))
    (when name
      (cl-pushnew name default-names :test #'equal))

    (--each ;; existing-compilation-buffers-for-project
        (mode-buffers-for-project
         :directory (approximate-project-root)
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
               (:constructor make-compilation-candidate (&key command annotation (directory default-directory) (name command))))
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
   :documentation "directory in which to run the command. Not presently used."
   :type string)
  (annotation
   "compilation command"
   :documentation "description of command, used for marginalia annotations."
   :type string))

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
       (defun ,symbol-name (project-root-directory directories operation-table)
	 ,(format "Build list of `tychoish-compilation-candidate' objects for suggestion in compilation buffers")
	 (ignore project-root-directory directories operation-table)
	 (when ,predicate
	   (add-candidates-to-table
	    operation-table
	    ,pipeline)))

       ,(if (eql 0 (length hooks))
	    `(add-hook 'tychoish-compilation-candidate-functions #',symbol-name)
	  `(let ((hooks ,hooks))
	     (defun ,hook-registering-function-name ()
	       (add-hook 'tychoish-compilation-candidate-functions #',symbol-name 0 t))

	     (with-eval-after-load "compile"
	       (push 'compilation-mode-hook hooks))

	     (->> hooks (--mapc (add-hook it ',hook-registering-function-name))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; candidate discovery/generator registration


(f-directories-containing-file-function "Cargo.toml")
(f-directories-containing-file-function "go.mod")
(f-directories-containing-file-function "justfile")
(f-directories-containing-file-function "makefile" "Makefile" "GNUmakefile")
(f-directories-containing-file-function "pyproject.toml")

(f-directories-containing-file-with-extension-function "go")
(f-directories-containing-file-with-extension-function "py")
(f-directories-containing-file-with-extension-function "rs")

(f-directories-containing-file-with-extension-function "md")
(f-directories-containing-file-with-extension-function "txt")
(f-directories-containing-file-with-extension-function "rst")
(f-directories-containing-file-with-extension-function "org")
(f-directories-containing-file-with-extension-function "mdwn")

(defun tychoish--compilation-discover-make-targets (&optional directory)
  (let* ((default-directory (or directory default-directory))
	 (makefile-report (shell-command-to-string "make --dry-run --print-data-base | grep -E '^[a-zA-Z0-9_-]+:' | sed 's/:.*//'")))
  (unless (s-contains-p "***" makefile-report)
    (->> (split-string makefile-report)
	 (--map (list (cons 'target it) (cons 'directory default-directory)))))))

(register-compilation-candidates
 :name "makefiles"
 :pipeline (->> directories
		(-flat-map #'f-directories-containing-file-makefile)
		(-flat-map #'tychoish--compilation-discover-make-targets)
		(--flat-map
		 (let* ((directory (alist-get 'directory it))
			(target (alist-get 'target it))
			(annotation-directory (f-collapse-homedir directory))
			(is-current-directory (string-equal default-directory directory))
			(command-template (or (when is-current-directory (format "make %%s%s" target))
					      (format "make %%s-C %s %s" directory target)))
			(annotation-template (or (when is-current-directory (format "run target %s in project root (%s)" target annotation-directory))
						 (format "run target %s in %s" target  directory))))
		   (list
		    (make-compilation-candidate
		     :command (format command-template "-k ")
		     :directory directory
		     :annotation (concat annotation-template ", continuing on error"))
		    (make-compilation-candidate
		     :command (format command-template "")
		     :directory directory
		     :annotation annotation-template)
		    (make-compilation-candidate
		     :command (format command-template "-B ")
		     :directory directory
		     :annotation (concat annotation-template ", unconditionally"))
		    (make-compilation-candidate
		     :command (format command-template "-k -B ")
		     :directory directory
		     :annotation (concat annotation-template ", unconditionally while continuing on error")))))))

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
		    :annotation (format "operation from `'shell-command-history' in the current directory (%s)" default-directory)))
		 )))

(register-compilation-candidates
 :name "project-compilation-buffer-commands"
 :pipeline (->> (mode-buffers-for-project
		 :mode 'compilation-mode
		 :directory project-root-directory)
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
		 (let* ((directory it)
			(prefix (concat "." (f-path-separator)))
			(dir (if (f-equal-p directory default-directory) "./" directory))
			(dir (cond
			      ((string-prefix-p prefix dir) dir)
			      ((string-prefix-p (f-path-separator) dir) dir)
			      (t (concat prefix dir))))
			(dir-with-dots (or (when (string-suffix-p "/" dir) (concat dir "..."))
					   (concat dir "/..."))))
		   (->> (list (cons "go test -v %s" "run go tests (insert )n verbose mode in %s")
			      (cons "go test -v -cover %s" "run go tests in verbose mode and collect coverage data in %s")
			      (cons "go test -v -race %s" "run go tests in verbose mode with the race detector in %s")
			      (cons "go test -v -cover -race %s" "run go tests in verbose mode with the race detector AND collect coverage data in %s")
			      (cons "go test -cover %s" "run go tests while collecting coverage data in %s")
			      (cons "go test -race %s" "run go tests with the race detector in %s")
			      (cons "go test -race -cover %s" "run go tests in verbose mode with the race detector AND collect coverage data in %s")
			      (cons "golint %s" "run golint for %s")
			      (cons "go test %s" "run go tests in %s")
			      (cons "go test %s -run=NOOP" "build all sources, including tests in %s")
			      (cons "go build %s" "build the go package in %s"))
			(--flat-map (list
				     (cons (format (car it) dir) (format (cdr it) dir))
				     (cons (format (car it) dir-with-dots) (concat (format (cdr it) dir) ", and all subdirectories"))))
			(--map (make-compilation-candidate
				:command (car it)
				:directory directory
				:annotation (cdr it))))))))

(register-compilation-candidates
 :name "go-modules"
 :pipeline (->> (f-directories-containing-file-go-mod directories)
		(--flat-map
		 (list
		  (make-compilation-candidate
		   :command "golangci-lint run"
		   :directory it
		   :annotation (format "run `golangci-lint' in package %s" (f-filename it)))
		  (make-compilation-candidate
		   :command "golangci-lint run --fix"
		   :directory it
		   :annotation (format "run `golangci-lint' in package %s with fixing trivial errors" (f-filename it)))
		  (make-compilation-candidate
		   :name "go list <pkgs...> | xargs -t go test -race -v"
		   :command "go list -f '{{ if (or .TestGoFiles .XTestGoFiles) }}{{ .ImportPath }}{{ end }}' ./... | xargs -t go test -race -v"
		   :directory it
		   :annotation (format "crazy go xargs test %s" (f-filename it)))
		  (make-compilation-candidate
		   :command "go mod tidy"
		   :directory it
		   :annotation (format "run `go mod tidy' in package %s" (f-filename it)))
		  (make-compilation-candidate
		   :command "go doc -all"
		   :directory it
		   :annotation (format "full go doc for entire package `%s'" (f-filename it)))
		  (make-compilation-candidate
		   :command "go doc --"
		   :name "go doc"
		   :directory it
		   :annotation (format "go doc outline for package `%s'" (f-filename it)))))))

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
 :hooks '(rust-ts-mode rust-mode rustic-mode dired-mode)
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

(provide 'consult-builder)
