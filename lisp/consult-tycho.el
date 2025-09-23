;;; consult-tycho.el --- tycho(ish) consult helpers -*- lexical-binding: t -*-

;; Author: sam kleinman
;; Maintainer: tychoish

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

;; These are mostly helpers to for writing my own set of consult
;; helpers largely for "better" incremental (rip)grep tools.

;;; Code:

(require 'consult)
(require 'ht)

(defun consult-apropos--get-all-symbols ()
  "In progress consult source function.
Builds and returns a list of (name kind symbol) values in persuit of a
helm-appropos replacement."
  (let ((symbs '()))
    (mapatoms (lambda (symb)
                (let ((name (symbol-name symb)))
                  (cond
                   ((local-variable-p symb) (push `(,name 'local-variable ,symb) symbs))
                   ((custom-variable-p symb) (push `(,name 'custom-variable ,symb) symbs))
                   ((commandp symb) (push `(,name 'command ,symb) symbs))
                   ((functionp symb) (push `(,name 'function ,symb) symbs))
                   ((facep symb) (push `(,name 'face ,symb) symbs))
                   ((string-match-p "%" name))
                   (t (push `(,name 'variable ,symb) symbs))))))
    symbs))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; consult-tycho: org-capture

(defun consult-org--setup-template ()
  "Add special template to capture to a target selectable via consult. Named for clarity in hooks."
  (add-to-list 'org-capture-templates
               '("c" "consult-org-capture: select heading interactively ..." entry (consult-org--capture-target 'agenda)
                  "* TODO %?\n  %i"
                  :prepend t
                  :kill-buffer t
                  :empty-lines-after 1)))

(defun consult-org--capture-target ()
  "Choose a capture target interactively.
This function returns a value suitable for use as the `target'
entry of `org-capture-templates'."
  (list 'function
        (lambda ()
          (unless (derived-mode-p #'org-mode)
                 (user-error "Must call from an Org buffer.")
          (let ((consult--read-config
                 `((,this-command
                    :prompt "org-apture target: "
                    :preview-key "M-."))))
            (set-buffer
             (save-window-excursion
               (consult-org-heading nil 'agenda)
               (current-buffer))))))))

;;;###autoload
(defun consult-org-capture ()
  "Select a capture template interactively."
  (interactive)
  ;; TODO remove this hack so that things are loaded in time
  (require 'tychoish-org)
  (let* ((templ (cl-loop for template in org-capture-templates
                         when (> (length template) 2)
                           collect (cons (nth 1 template) (nth 0 template))))
         (capture-template
          (consult--read templ
                         :prompt "org-capture-templates=>: "
                         :require-match nil
                         :group (consult--type-group templ)
                         :narrow (consult--type-narrow templ)
                         :annotate (lambda (selection) (format " --> [%s]" (cdr (assoc selection templ))))
                         :lookup (lambda (selection candidates &rest _) (cdr (assoc selection candidates)))
                         :category 'org-capture
                         :history '(:input consult-org--capture-history))))
    (unless (string-with-non-whitespace-content-p capture-template)
      (user-error "must select a valid templae %s (%s)" capture-template (type-of capture-template)))
    (org-capture nil capture-template)))

;;;###autoload
(defun consult-org-capture-target ()
  (interactive)
  (org-capture nil "c"))

(add-hook 'org-capture-mode-hook #'consult-org--setup-template)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; consult-tycho: contextual directory selection

(defun consult-tycho--clean-directory-options-for-selection (input)
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
  (->> (get-directory-parents default-directory (or (approximate-project-root) ""))
       (-join (list default-directory
                    user-emacs-directory
                    "~/"
                    (approximate-project-root)
                    (thing-at-point 'filename)
                    (thing-at-point 'existing-filename)))
       (-filter #'stringp)
       (-map #'expand-file-name)
       (-distinct)))

(cl-defun consult-tycho--select-directory (&optional &key input-dirs (require-match nil))
  "Select a directory from a provided or likely set of `INPUT-DIRS`'."
  (consult--read
   (consult-tycho--clean-directory-options-for-selection
    (or (when (listp input-dirs)
          (consult-tycho--clean-directory-options-for-selection input-dirs))
        (when-let* ((strp (stringp input-dirs))
                    (input-dirs (trimmed-string-or-nil input-dirs)))
          (list input-dirs))
        (get-directory-default-candidate-list)))
   :sort nil
   :command this-command
   :require-match require-match
   :prompt "in directory =>> "))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; consult-tycho: increment-grep

(defun consult-tycho--resolve-initial-grep (prompt prompt-annotation initial &key context)
  "Return the initial text for a query, processing `INITIAL' as needed."
  ;; if the string is empty or only whitespace, it's undefined,
  ;; otherwise use it.
  (or (trimmed-string-or-nil initial)
      ;; with the prefix argument, ask the user
      (when current-prefix-arg
        (consult-tycho--select-context-for-operation
         (format "%s<init:%s>: " prompt prompt-annotation)))
      ;; otherwise, provide the empty string...
      ""))

(defun consult-tycho--select-context-for-operation (&optional prompt seed-list)
  "Pick string to use as context in a follow up operation."
  (let ((this-command this-command)
        (selections (consult-tycho--context-base-list seed-list))
	(prompt (or prompt "grep =>>")))
    (or (when (length= selections 1) (nth 0 selections))
        (when (length> selections 1)
          (consult--read selections
           :sort nil
           :command this-command
           :require-match nil
           :prompt prompt)))))

(defun consult-tycho--context-base-list (&optional seed)
  (->> (-join
        (if (listp seed)
            seed
          (list seed))
        (if-let* ((mark-pos (mark))
			(start (or (region-beginning) (min (point) mark-pos)))
			(end (or (region-end) (max mark-pos (point)))))
	    (s-lines (s-trim (buffer-substring start end)))
	  '())
        (-take 10 kill-ring)
        (cond ((derived-mode-p 'text-mode)
               (list (thing-at-point 'word)
                     (thing-at-point 'email)
                     (thing-at-point 'url)
                     (thing-at-point 'sentence)))
              ((derived-mode-p 'prog-mode)
               (list (thing-at-point 'symbol)
                     (thing-at-point 'word)
                     (thing-at-point 'sexp)
                     (thing-at-point 'defun)))
              (t '()))
        (list (thing-at-point 'line)))
       (-keep #'trimmed-string-or-nil)
       (-distinct)))

(cl-defun consult-tycho--incremental-grep (&key (prompt "=>> ") (builder '()) (initial ""))
  "Do incremental grep-type operation. Like the `consult-grep' operation
upon which it was based, permits interoperability between git-grep ag, ack, and rg"
  (let ((consult-async-input-debounce 0.025)
        (consult-async-input-throttle 0.05)
        (consult-async-refresh-delay 0.025)
        (this-command this-command))
    (consult--read
     (consult--process-collection builder
       :transform (consult--grep-format builder)
       :file-handler t)
     :prompt prompt
     :lookup #'consult--lookup-member
     :state (consult--grep-state)
     :initial initial
     :add-history (thing-at-point 'symbol)
     :require-match nil
     :category 'consult-grep
     :command this-command
     :sort nil
     :group nil ;; #'consult--prefix-group <- this groups results by common prefix (e.g. file)
     :history '(:input consult--grep-history))))

;;;###autoload
(defun consult-rg (&optional dir initial &key context)
  "Start and iterative rg session. DIR and INITIAL integrate with the consult-grep API."
  (interactive "P")
  ;; `consult--directory-prompt' --> '(prompt paths <default>-dir)
  (let* ((prompt-paths-dir (consult--directory-prompt "rg" (or (trimmed-string-or-nil dir)
							       (consult-tycho--select-directory)
							       (approximate-project-root))))
         (default-directory (nth 2 prompt-paths-dir))
         (prompt (nth 0 prompt-paths-dir))
         (initial (if (and (or context (not initial)) (not (eq context 'override)))
		      (consult-tycho--select-context-for-operation (format "rg(init) =>> "))
		    initial)))

    (consult-tycho--incremental-grep
     :prompt prompt
     :builder (consult--ripgrep-make-builder (nth 1 prompt-paths-dir))
     :initial initial)))

;;;###autoload
(defun consult-rg-project (&optional initial &key context)
  "Start an iterative rg session in the project root, if possible, falling back as necessary."
  (interactive "P")
  (consult-rg
   (or (approximate-project-root) (consult-tycho--select-directory))
   initial
   :context (or context current-prefix-arg 'override)))

;;;###autoload
(defun consult-rg-pwd (&optional initial &key context)
  "Start an iterative rg session for the current directory."
  ;; (let ((base-directory (f-base default-directory)))
  (interactive "P")

  (consult-rg
   (or default-directory (consult-tycho--select-directory))
   initial
   :context (or context current-prefix-arg 'override)))

;;;###autoload
(defun consult-rg-pwd-wizard (&optional initial)
  "Start an iterative rg session with context, with prompting to start a query for a collection of likely candidates."
  ;; (let ((base-directory (f-base default-directory)))
  (interactive "P")
  (consult-rg-pwd initial :context t))

;;;###autoload
(defun consult-rg-project-wizard (&optional initial)
  "Start an iterative rg session with context. Always run the search in the project root, falling back if there isn't a discernable root."
  ;; (let ((base-directory (f-base default-directory)))
  (interactive "P")
  (consult-rg-project initial :context t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; consult-tycho: project compilation mode

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

(defmacro f-file-has-ext-predicate (extension)
  `(lambda (filename) (f-ext-p filename ,extension)))

(defmacro f-filename-is-predicate (name)
  `(lambda (filename) (string= (f-filename filename) ,name)))

(defmacro f-directory-containing-file-function (filename)
  `(defun ,(intern (format "f-directory-containing-file-%s" (string-replace "." "-" filename))) (filename)
     (and (f-file-p filename)
	  (string= (f-filename filename) ,filename)
	  (f-dirname filename))))

(defmacro f-directory-containing-files-with-extension-function (extension)
  `(defun ,(intern (format "f-directory-containing-files-with-%s-extension" (string-replace "." "" extension))) (filename)
     (and (f-file-p filename)
	  (f-ext-p filename) ,extension)
	  (f-dirname filename)))

(f-directory-containing-files-with-extension-function ".go")
(f-directory-containing-files-with-extension-function ".py")
(f-directory-containing-files-with-extension-function ".rs")
(f-directory-containing-file-function "go.mod")
(f-directory-containing-file-function "pyproject.toml")

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

(cl-defmacro register-compilation-candidates (&key name (local nil) pipeline)
  (let ((symbol-name (intern (format "tychoish--compilation-candidates-for-%s" name))))
    `(progn
       (defun ,symbol-name (project-root-directory directories operation-table)
	 ,(format "Build list of `tychoish--compilation-candidate' objects for suggestion in compilation buffers")
	 (add-candidates-to-table
	  operation-table
	  ,pipeline))
       (add-hook 'tychoish--compilation-candidate-functions #',symbol-name 0 ,local))))

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

(cl-defun project-compilation-buffers (&optional &key (name "build") (project (approximate-project-name)))
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

;;;###autoload
(defun compilation-buffer-change-directory ()
  "Change the directory for the current compilation buffer."
  (interactive)
  (unless (derived-mode-p 'compilation-mode)
    (user-error "operation is only applicable for COMPILATION-MODE buffers"))
  (let* ((fallback-directories (list compilation-directory default-directory))
	 (options (append (get-directory-default-candidate-list) fallback-directories))
	 (selection (consult-tycho--select-directory :input-dirs options))
	 (directory (or selection compilation-directory default-directory)))
    (setq-local default-directory directory
		compilation-directory directory)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; consult-tycho: mail

;;;###autoload
(defun tychoish-mail-select-account (account-id)
  "Use consult to select an account/mail configuration."
  (interactive
   (list
    (let* ((accounts (ht-keys tychoish/mail-accounts))
	   (longest-key (length-of-longest-item accounts)))
      (consult--read
       accounts
       :prompt "mail-account => "
       :require-match nil
       :annotate (tychoish/mail-get-account-annotation-function longest-key)))))

  (let ((select-account-operation (intern account-id)))
    (funcall select-account-operation)))

(provide 'consult-tycho)
;;; consult-tycho.el ends here
