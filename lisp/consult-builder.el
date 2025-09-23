(require 'consult)
(require 'ht)
(require 'dash)
(require 'f)

;; this is the inner "select which command to use" for entering a new compile command.
(defun tychoish--compilation-read-command (command)
  (let* ((candidates (tychoish--get-compilation-candidates default-directory))
         (names (->> candidates
                     (ht-map (lambda (key value) (tychoish--compilation-candidate-name value)))
                     (-sort #'string-lessp)))
         (longest-id (length-of-longest-item names))
         (selection-name
          (consult--read
           names
           :prompt "compile command => "
           :command this-command
           :history 'compile-history
           :annotate (lambda (key) (format "%s%s" (prefix-padding-for-annotation key longest-id)
                                           (tychoish--compilation-candidate-annotation (ht-get candidates key))))))
	 (selection (ht-get candidates selection-name)))

    (if selection
	(tychoish--compilation-candidate-name selection)
      selection-name)))

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
          (when (trimmed-string-or-nil compile-command)
            (recompile current-prefix-arg)))
      (compilation-start
       compile-command        ;; the command
       'compilation-mode      ;; the default
       (compile-buffer-name op-name)))

    (if-let* ((op-window (get-buffer-window op-name (selected-frame))))
        (select-window op-window)
      (switch-to-buffer-other-window (get-buffer op-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; directory selection

(defun clean-directory-options-for-selection (input)
  "Process `INPUT' list removing: duplicates, nils, and empty or whitespace elements."
  (->> input
       (-keep #'trimmed-string-or-nil)
       (-map #'expand-file-name)
       (-sort #'string-greaterp)
       (-distinct)))

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
	 (-join (list default-directory
                      user-emacs-directory
                      "~/"
                      proj-root
                      (thing-at-point 'filename)
                      (thing-at-point 'existing-filename)))
       (-filter #'stringp)
       (-map #'expand-file-name)
       (-distinct))))

(cl-defun consult--select-directory (&optional &key input-dirs (require-match nil))
  "Select a directory from a provided or likely set of `INPUT-DIRS`'."
  (consult--read
   (or (when (listp input-dirs)
         (clean-directory-options-for-selection input-dirs))
       (when (stringp input-dirs)
         (clean-directory-options-for-selection (list input-dirs)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; helper functions

(f-directory-containing-file-with-extension-function ".go")
(f-directory-containing-file-with-extension-function ".py")
(f-directory-containing-file-with-extension-function ".rs")
(f-directory-containing-file-function "go.mod")
(f-directory-containing-file-function "pyproject.toml")

(cl-defun project-compilation-buffers (&optional &key name (project (approximate-project-name)))
  (let ((buffer-table (ht-create))
        (default-names (list "build" "compilation" "test" "lint" "check" "benchmark" "run")))
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
                 (string-trim (car compilation-arguments))))))

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

(cl-defstruct (tychoish--compilation-candidate
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

(cl-defmethod push-compilation-candidate ((candidate tychoish--compilation-candidate) (table hash-table))
  (ht-set table (tychoish--compilation-candidate-name candidate) candidate))

(defun add-candidates-to-table (table candidates)
  (mapc (lambda (item) (push-compilation-candidate item table)) candidates))

(defvar tychoish--compilation-candidate-functions nil
  "A List of functions that populate a table of possible completion commands

All functions are called with (PROJECT-ROOT-DIRECTORY DIRECTORIES TABLE)
as arguments where the `project-root-directory' is the root of the
current project, `directories' are all of the directories between the
current directory and the project root, and `table' is table of `tychoish--completion-candiate' objects.")

(defun tychoish--get-compilation-candidates (&optional directory)
 "Generate a sequence of candidate compilation commands based on mode and directory structure."
  (let* ((project-root-directory (approximate-project-root))
	 (default-directory (or directory default-directory))
         (directories (get-directory-parents default-directory project-root-directory))
         (operation-table (ht-create)))

    (run-hook-with-args 'tychoish--compilation-candidate-functions project-root-directory directories operation-table)
    operation-table))

(cl-defun project-compilation-buffers (&optional &key name (project (approximate-project-name)))
  (let ((buffer-table (ht-create))
        (default-names (list "build" "compilation" "test" "lint" "check" "benchmark" "run")))
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
                 (string-trim (car compilation-arguments))))))

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



;;;###autoload
(cl-defmacro register-compilation-candidates (&key name (local nil) pipeline)
  (let ((symbol-name (intern (format "tychoish--compilation-candidates-for-%s" name))))
    `(progn
       (defun ,symbol-name (project-root-directory directories operation-table)
	 ,(format "Build list of `tychoish--compilation-candidate' objects for suggestion in compilation buffers")
	 (add-candidates-to-table
	  operation-table
	  ,pipeline))
       (add-hook 'tychoish--compilation-candidate-functions #',symbol-name 0 ,local))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; candidate discovery/generator registration

(register-compilation-candidates
 :name pyprojects
 :pipeline (->> directories
		(--flat-map (f-entries it (f-filename-is-predicate "pyproject.toml")))
		(-map #'f-dirname)
		(-distinct)
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
 :pipeline (->> directories
		(--flat-map (f-entries it (f-file-has-ext-predicate "py")))
		(-map #'f-dirname)
		(-distinct)
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
 :name "makefiles"
 :pipeline (->> directories
		(--filter (f-directory-has-file-p it '("makefile" "Makefile")))
		(--flat-map
		 (let ((directory it))
		   (--map (cons directory it) '("build" "test" "lint" "fmt"))))
		(--flat-map
		 (let* ((directory (car it))
			(target (cdr it))
			(is-project-root (equal default-directory directory))
			(command-template (or (when is-project-root (format "make %%s%s" target))
					      (format "make %%s-C %s %s" directory target)))
			(annotation-template (or (when is-project-root (format "run target %s" target))
						 (format "run target %s in %s" target directory))))
		   (list
		    (make-compilation-candidate
		     :command (format command-template "-k ")
		     :annotation (concat annotation-template ", continuing on error"))
		    (make-compilation-candidate
		     :command (format command-template "")
		     :annotation annotation-template)
		    (make-compilation-candidate
		     :command (format command-template "-B ")
		     :annotation (concat annotation-template ", unconditionally"))
		    (make-compilation-candidate
		     :command (format command-template "-k -B ")
		     :annotation (concat annotation-template ", unconditionally while continuing on error")))))))


(register-compilation-candidates
 :name "go-packages"
 :pipeline (->> directories
		(--flat-map (f-entries it (f-file-has-ext-predicate "go")))
		(-map #'f-dirname)
		(-distinct)
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
 :pipeline (->> directories
		(--flat-map (f-entries it (f-filename-is-predicate "go.mod")))
		(-map #'f-dirname)
		(-distinct)
		(--flat-map
		 (list
		  (make-compilation-candidate
		   :command "golangci-lint run"
		   :directory it
		   :annotation (format "run `golangci-lint' in package %s" (f-filename it)))
		  (make-compilation-candidate
		   :command "golangci-lint run --allow-parallel-runners"
		   :directory it
		   :annotation (format "run `golangci-lint' in package %s with parallel runners" (f-filename it)))
		  (make-compilation-candidate
		   :name "go list <pkgs...> | xargs -t go test -race -v"
		   :command "go list -f '{{ if (or .TestGoFiles .XTestGoFiles) }}{{ .ImportPath }}{{ end }}' ./... | xargs -t go test -race -v"
		   :directory it
		   :annotation (format "crazy go xargs test" (f-filename it)))
		  (make-compilation-candidate
		   :command "go mod tidy"
		   :directory it
		   :annotation (format "run `go mod tidy' in package %s" (f-filename it)))))))

(register-compilation-candidates
 :name "minibuffer-shell-commands"
 :pipeline (->> (minibuffer-default-add-shell-commands)
		(--flat-map
		 (list
		  (when (not (string= default-directory project-root-directory))
		    (make-compilation-candidate
		     :command it
		     :directory default-directory
		     :annotation (format "operation from `'minibuffer-shell-commands' in the current directory (%s)" default-directory)))
		  (make-compilation-candidate
		   :command it
		   :directory project-root-directory
		   :annotation (format "operation from `'minibuffer-shell-commands' in the project root (%s)" project-root-directory))))
		(-non-nil)))

(register-compilation-candidates
 :name "shell-command-history"
 :pipeline (->> shell-command-history
		(--flat-map
		 (list
		  (when (not (string= default-directory project-root-directory))
		    (make-compilation-candidate
		     :command it
		     :directory default-directory
		     :annotation (format "operation from `'shell-command-history' in the current directory (%s)" default-directory)))
		  (make-compilation-candidate
		   :command it
		   :directory project-root-directory
		   :annotation (format "operation from `'shell-command-history' in the project root (%s)" project-root-directory))))
		(-non-nil)))

(register-compilation-candidates
 :name "project-compilation-buffer-commands"
 :pipeline (->> (mode-buffers-for-project
		 :mode 'compilation-mode
		 :directory default-directory)
		(--keep
		 (with-current-buffer it
		   (cons (string-trim (car compilation-arguments)) (buffer-name))))
		(--map-in-place
		 (make-compilation-candidate
		  :command (car it)
		  :directory project-root-directory
		  :annotation (format "run %s, from compile buffer %s in the project root (%s) " (car it) (cdr it) project-root-directory)))))

(provide 'consult-builder)
