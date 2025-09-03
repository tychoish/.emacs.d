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
         (_ (message "OPTIONS: %s" templ))
         (_ (message "SOURCE: %s" org-capture-templates))
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
    (while (or (not (string= current stop-path))
               (not (string-prefix-p stop-path current)))
      (push (setq current (file-name-parent-directory current)) output))
    output))

(defun get-directory-default-candidate-list ()
  (append (get-directory-parents default-directory (or (projectile-project-root) ""))
	  (list default-directory
		user-emacs-directory
		(expand-file-name "~/")
		(projectile-project-root)
		(thing-at-point 'filename)
		(thing-at-point 'existing-filename))))

(defun consult-tycho--select-directory (&optional &key input-dirs require-match)
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
   :prompt "in directory: "))

(defun consult-tycho--discover-directory (dir)
  "Expand or produce a non-zero directory for DIR."
  (or (when current-prefix-arg
        (consult-tycho--select-directory :dirs dir))
      (trimmed-string-or-nil dir)
      (or (projectile-project-root)
          default-directory)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; consult-tycho: increment-grep

(defun consult-tycho--resolve-initial-grep (prompt prompt-annotation initial)
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

(defun consult-tycho--select-context-for-operation (prompt &optional seed-list)
  "Pick string to use as context in a follow up operation using PROMPT."
  (let* ((current-selection )
         (this-command this-command)
         (selections (consult-tycho--context-base-list (list current-selection))))
    (or (when (length= selections 1) (nth 0 selections))
        (when (length> selections 1)
          (consult--read selections
           :sort nil
           :command this-command
           :require-match nil
           :prompt (format "%s: " prompt))))))

(defun consult-tycho--context-base-list (&optional seed-list)
  (->> (list (s-trim (buffer-substring (region-beginning) (region-end))))
       (-concat seed-list)
       (-concat (cl-subseq kill-ring 0 (min 3 (length kill-ring))))
       (-concat
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
              (t '())))
       (-keep #'trimmed-string-or-nil)
       (-add-if-empty (thing-at-point 'line))
       (-filter (lambda (elem) (and (> 32 (length elem))
				    (< 2 (length elem)))))
       (-sort #'string-greaterp)
       (-distinct)))

(defun consult-tycho--incremental-grep (prompt builder initial)
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
     :group nil ;; #'consult--prefix-group <- this groups results by common prefix (e.g. file)
     :history '(:input consult--grep-history)
     :sort nil)))

;;;###autoload
(defun consult-rg (&optional dir initial)
  "Start and iterative rg session.
DIR and INITIAL integrate with the consult-grep API."
  (interactive "P")
  ;; `consult--directory-prompt' --> '(prompt paths <default>-dir)
  (let* ((prompt-paths-dir (consult--directory-prompt "rg" (consult-tycho--discover-directory dir)))
         (default-directory (nth 2 prompt-paths-dir)))
    (consult-tycho--incremental-grep
     ;; prompt:
     (nth 0 prompt-paths-dir)
     ;; builder:
     (consult--ripgrep-make-builder (nth 1 prompt-paths-dir))
     ;; initial:
     (consult-tycho--resolve-initial-grep "rg" "regex" initial))))

;;;###autoload
(defun consult-rg-for-thing (&optional dir initial)
  "Start an iterative rg session with context.
DIR and INITIAL integrate with the consult-grep API."
  (interactive "P")
  (consult-rg dir (consult-tycho--select-context-for-operation "rg<thing>")))

;;;###autoload
(defun consult-rg-pwd (&optional initial)
  "Start an iterative rg session with context.
DIR and INITIAL integrate with the consult-grep API."
  (interactive "P")
  (let ((dir-base (f-base default-directory)))
    (consult-tycho--incremental-grep
     (format "rg<pwd:%s>: " dir-base)
     (consult--ripgrep-make-builder (list (expand-file-name default-directory)))
     (consult-tycho--resolve-initial-grep "rg" (f-base default-directory) initial))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; consult-tycho: project compilation mode

(cl-defun make-compilation-target-alist (&key cmd directory annotation)
  `(list (cmd . ,cmd) (directory . ,directory) (annotation . ,annotation)))

(defun compilation-candidates-for-make-targets (&rest target-cells)
  (->> target-cells
       (-map (lambda (cell)
	       (list
		(make-compilation-target-alist
		 :cmd (format "make -k %s" (alist-get 'cmd cell))
		 :directory (alist-get 'directory cell)
		 :annotation (format "%s, continuing on error" (alist-get 'annotation cell)))
		(make-compilation-target-alist
		 :cmd (format "make %s" (alist-get 'cmd cell))
		 :directory (alist-get 'directory cell)
		 :annotation (alist-get 'annotation cell))
		(make-compilation-target-alist
		 :cmd (format "make -B %s" (alist-get 'cmd cell))
		 :directory (alist-get 'directory cell)
		 :annotation (format "%s, forcing a rebuild, even when the target is up to date" (alist-get 'annotation cell))))))
       (-flatten-n 1)))

;; TODO: this should be a collection of hooks that you register and
;; are given a buffer or a directory (and?) each function returns
;; some candidates (or not)
(defun tychoish--get-compilation-candidates (&optional directory)
  "Generate a mapping of copilation commands to a cons cell of the directory and an annotation (command . (directory . username))"
  (unless directory (setq directory default-directory))
  (let* ((proj (projectile-project-root))
	 (package-directories (get-directory-parents directory proj))
	 (make-directories (-keep (lambda (dir) (when (or (file-in-directory-p "makefile" dir)
							  (file-in-directory-p "Makefile" dir))
						  dir))
				  package-directories))
	 (go-mod-directories (-keep (lambda (dir) (when (f-exists-p (f-join dir "go.mod")) dir))
				    package-directories))
	 (go-pkg-directories (->> go-mod-directories
				  (-map (lambda (gmd) (get-directory-parents directory gmd)))
				  (-flatten)
				  (-distinct)))
	 (project-compile-buffer-commands (->> (mode-buffers-for-project :mode 'compilation-mode :directory directory)
					       (-keep (lambda (buf) (with-current-buffer buf (cons (string-trim (car compilation-arguments)) (buffer-name))))))))
    (->>
     (-concat
      (-map (lambda (dir)
	      (if (eql directory dir)
		  (compilation-candidates-for-make-targets
		   (make-compilation-target-alist :cmd "build" :directory default-directory :annotation "run build target")
		   (make-compilation-target-alist :cmd "test" :directory default-directory :annotation "run test target")
		   (make-compilation-target-alist :cmd "lint" :directory default-directory :annotation "run lint target"))
		(-map (lambda (target)
			(compilation-candidates-for-make-targets
			 (make-compilation-target-alist
			  :cmd (format "-C %s %s" dir target)
			  :directory dir
			  :annotation (format "run build target in %s" target))))
		      '("build" "test" "lint"))))
	    make-directories)
      (-flatten-n 1 (-map (lambda (dir)
			    (-map (lambda (command-template)
				    (let* ((prefix (concat "." (f-path-separator)))
					   (cmd-dir dir)
					   (dir (if (equal dir default-directory) "./" dir))
					   (dir (cond
						 ((string-prefix-p (f-path-separator) dir) dir)
						 ((string-prefix-p prefix dir) dir)
						 (t (concat prefix dir))))
					   (dir-with-dots (concat dir "...")))
				      (list
				       (make-compilation-target-alist
					:cmd (format (car command-template) dir)
					:directory dir
					:annotation (format (cdr command-template) dir))
				       (make-compilation-target-alist
					:cmd (format (car command-template) dir-with-dots)
					:directory dir
					:annotation (format "%s, and all subdirectories" (cdr command-template))))))
				  (list (cons "go test -v %s" "run go tests in verbose mode in %s")
					(cons "go test -v -cover %s" "run go tests in verbose mode and collect coverage data in %s")
					(cons "go test -v -race %s" "run go tests in verbose mode with the race detector in %s")
					(cons "go test -v -cover -race %s" "run go tests in verbose mode with the race detector AND collect coverage data in %s")
					(cons "go test -cover %s" "run go tests while collecting coverage data in %s")
					(cons "go test -race %s" "run go tests with the race detector in %s")
					(cons "go test -race -cover %s" "run go tests in verbose mode with the race detector AND collect coverage data in %s")
					(cons "golint %s" "run golint for %s")
					(cons "go test %s" "run go tests in %s")
					(cons "go test %s -run=NOOP" "build go tests in %s but skip tests")
 					(cons "go build %s" "build the go package in %s"))))
			  go-pkg-directories))
      (-map (lambda (cmd)
	      (list
	       (make-compilation-target-alist
		:cmd cmd
		:directory directory
		:annotation (format "operation from `'minibuffer-shell-commands' in current directory (%s)" directory))
	       (make-compilation-target-alist
		:cmd cmd
		:directory proj
		:annotation (format "operation from `'minibuffer-shell-commands' in current directory (%s)" proj))))
	    (minibuffer-default-add-shell-commands))
      (-map (lambda (cmd)
	      (list
	       (make-compilation-target-alist
		:cmd cmd
		:directory directory
		:annotation (format "operation from `'shell-command-history' in current directory (%s)" directory))
	       (make-compilation-target-alist
		:cmd cmd
		:directory proj
		:annotation (format "operation from `'shell-command-history' in current directory (%s)" directory))))
	    shell-command-history)
      (-map (lambda (dir)
	      (list
	       (make-compilation-target-alist
		:cmd "golangci-lint run"
		:directory dir
		:annotation (format "run `golangci-lint' in package %s" (f-filename dir)))
	       (make-compilation-target-alist
		:cmd "golangci-lint run --allow-parallel-runners"
		:directory dir
		:annotation (format "run `golangci-lint' in package %s with parallel runners" (f-filename dir)))
	       ;; (make-compilation-target-alist
	       ;;  :cmd "go list -f '{{ if (or .TestGoFiles .XTestGoFiles) }}{{ .ImportPath }}{{ end }}' ./... | xargs -t go test -race -v"
	       ;;  :directory dir
	       ;;  :annotation (format "crazy go xargs test" (f-filename dir)))
	       (make-compilation-target-alist
		:cmd "go mod tidy"
		:directory dir
		:annotation (format "run `go mod tidy' in package %s" (f-filename dir)))))
	    go-mod-directories)
      (-map (lambda (cmd)
	      (list (make-compilation-target-alist :cmd (car cmd) :directory proj :annotation (format "run %s, from compile buffer %s" (car cmd) (cdr cmd)))))
	    project-compile-buffer-commands))
     (-flatten-n 1)
     (-distinct-by-alist-key 'cmd)
     (-sort (lambda (st nd) (string-lessp (alist-get 'cmd st) (alist-get 'cmd nd)))))))

;; this is the inner "select which command to use" for entering a new compile command.
(defun tychoish--compilation-read-command (command)
  (let* ((candidates (tychoish--get-compilation-candidates default-directory))
	 (data (-keep (lambda (tripple) (when-let* ((cmd (alist-get 'cmd tripple))
						    (annotation (alist-get 'annotation tripple)))
					  (cons cmd annotation))) candidates))
	 (options (-map #'car data))
	 (longest-id (length-of-longest-item options)))
    (consult--read options
		   :prompt "compile command => "
		   :command this-command
		   :history 'compile-history
		   :require-match nil
		   :annotate (lambda (key) (format "%s%s" (prefix-padding-for-annotation key longest-id) (cdr (assoc key data)))))))

(cl-defun project-compilation-buffers (&optional &key (name "build") (project (approximate-project-name)))
  (let* ((default-names (->> (list name "build" "compilation" "test" "lint" "check" "benchmark" "run")
			     (-non-nil)
			     (-map (lambda (name) (format "*%s-%s*" project name)))
			     (-distinct)
			     (-sort #'string-lessp)))
	 (existing-compilation-buffers (-map #'buffer-name
					     (mode-buffers-for-project
					      :directory (approximate-project-root)
					      :mode 'compilation-mode)))
	 (longest-id (larger (length-of-longest-item default-names)
			     (length-of-longest-item existing-compilation-buffers))))
    (->> (-concat
	  (-map (lambda (name)
		  (with-current-buffer name
		    (cons name (format "%s -- reuse buffer: project=%s errors=%s lines=%s command='%s'"
				       (prefix-padding-for-annotation name longest-id)
				       project
				       compilation-num-errors-found
				       (buffer-line-count)
				       (string-trim (car compilation-arguments))))))
		existing-compilation-buffers)
	  (-map (lambda (name) (cons name (format "%s -- create default compilation buffer for %s" (prefix-padding-for-annotation name longest-id) project)))
		default-names)
	  (->> default-names
	       (-map #'generate-new-buffer-name)
	       (-map (lambda (name) (cons name (format "%s -- new buffer for project %s" (prefix-padding-for-annotation name longest-id) project))))))
	 (-distinct-by-car))))

(defun tychoish/compile-project (&optional name command)
  (let* ((compilation-buffer-candidates (project-compilation-buffers :name name))
         (op-name (consult--read
		   (mapc #'car compilation-buffer-candidates)
		   :prompt "compilation buffer => "
		   :require-match nil
		   :annotate (lambda (key) (cdr (assoc key compilation-buffer-candidates)))))
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
    (switch-to-buffer-other-window (get-buffer op-name))))

(provide 'consult-tycho)
;;; consult-tycho.el ends here
