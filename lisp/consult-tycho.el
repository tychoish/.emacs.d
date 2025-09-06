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
  (let ((this-command this-command)
        (selections (consult-tycho--context-base-list seed-list)))
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
        (s-lines (s-trim (buffer-substring (region-beginning) (region-end))))
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

(defun consult-tycho--incremental-grep (&key prompt builder initial)
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
         (default-directory (nth 2 prompt-paths-dir))
         (prompt (nth 0 prompt-paths-dir))
         (initial (or initial (consult-tycho--select-context-for-operation prompt))))
    (consult-tycho--incremental-grep
     :prompt prompt
     :builder (consult--ripgrep-make-builder (nth 1 prompt-paths-dir))
     :initial (consult-tycho--resolve-initial-grep "rg" "regex" initial))))

;;;###autoload
(defun consult-rg-for-thing (&optional dir initial)
  "Start an iterative rg session with context.
DIR and INITIAL integrate with the consult-grep API."
  (interactive "P")
  (consult-rg dir (consult-tycho--select-context-for-operation "rg<thing>: ")))

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

(defun compilation-candidates-for-make-targets (directory &rest targets)
  (let ((command-template "make %s%s")
        (annotation-template "run target %s%s"))

    (unless (equal default-directory directory)
      (setq command-template (format "make %%s-C %s %%s" directory))
      (setq annotation-template (format "run target %%s in %s%%s" directory)))

    (->> targets
         (--flat-map
          (list
           (make-compilation-candidate
            :command (format command-template "-k " it)
            :annotation (format annotation-template it ", continuing on error"))
           (make-compilation-candidate
            :command (format command-template "" it)
            :annotation (format annotation-template it ""))
           (make-compilation-candidate
            :command (format command-template "-B " it)
            :annotation (format annotation-template it ", unconditionally"))
           (make-compilation-candidate
            :command (format command-template "-k -B " it)
            :annotation (format annotation-template it ", unconditionally while continuing on error")))))))

(defun compilation-candidates-for-go-packages (directory)
  (->> (list (cons "go test -v %s" "run go tests in verbose mode in %s")
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

       (--flat-map
        (let* ((prefix (concat "." (f-path-separator)))
               (dir (if (equal directory default-directory) "./" directory))
               (dir (cond
                     ((string-prefix-p (f-path-separator) dir) dir)
                     ((string-prefix-p prefix dir) dir)
                     (t (concat prefix dir))))
               (dir-with-dots (concat dir "...")))
          (list
           (make-compilation-candidate
            :command (format (car it) dir)
            :directory directory
            :annotation (format (cdr it) dir))
           (make-compilation-candidate
            :command (format (car it) dir-with-dots)
            :directory directory
            :annotation (format "%s, and all subdirectories" (format (cdr it) dir))))))))


;; TODO: this should be a collection of hooks that you register and
;; are given a buffer or a directory (and?) each function returns
;; some candidates (or not)
(defun tychoish--get-compilation-candidates (&optional directory)
  "Generate a sequence of candidate compile commands (an alist) with ('name 'cmd 'directory 'annotation) keys."
  (unless directory (setq directory default-directory))
  (let* ((proj (approximate-project-root))
         (package-directories (get-directory-parents directory proj))
         (go-mod-directories (->> package-directories
                                  (-keep (lambda (dir) (when (f-exists-p (f-join dir "go.mod")) dir)))))
         (go-pkg-directories (->> go-mod-directories
                                  (-flat-map (lambda (gmd) (get-directory-parents directory gmd)))
                                  (-distinct)))
         (operation-table (ht-create)))

    (add-candidates-to-table
     operation-table
     (->> package-directories
          (--filter (f-directory-has-file-p it '("makefile" "Makefile")))
          (--flat-map
           (compilation-candidates-for-make-targets directory it "build" "test" "lint"))))

    (add-candidates-to-table
     operation-table
     (->> go-mod-directories
          (--flat-map (compilation-candidates-for-go-packages it))))

    (add-candidates-to-table
     operation-table
     (->> go-mod-directories
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

    (add-candidates-to-table
     operation-table
     (->> (minibuffer-default-add-shell-commands)
          (--flat-map
           (list
            (make-compilation-candidate
             :command it
             :directory directory
             :annotation (format "operation from `'minibuffer-shell-commands' in current directory (%s)" directory))
            (make-compilation-candidate
             :command it
             :directory proj
             :annotation (format "operation from `'minibuffer-shell-commands' in current directory (%s)" proj))))))

    (add-candidates-to-table
     operation-table
     (->> shell-command-history
          (--flat-map
           (list
            (make-compilation-candidate
             :command it
             :directory directory
             :annotation (format "operation from `'shell-command-history' in current directory (%s)" directory))
            (make-compilation-candidate
             :command it
             :directory proj
             :annotation (format "operation from `'shell-command-history' in current directory (%s)" directory))))))

    (add-candidates-to-table
     operation-table
     (->> (mode-buffers-for-project
           :mode 'compilation-mode
           :directory directory)
          (--keep
           (with-current-buffer it
             (cons (string-trim (car compilation-arguments)) (buffer-name))))
          (--map-in-place
           (make-compilation-candidate
            :command (car it)
            :directory proj
            :annotation (format "run %s, from compile buffer %s" (car it) (cdr it))))))

    operation-table))

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
           :require-match nil
           :annotate (lambda (key) (format "%s%s" (prefix-padding-for-annotation key longest-id)
                                           (tychoish--compilation-candidate-annotation (ht-get candidates key)))))))

    (tychoish--compilation-candidate-name (ht-get candidates selection-name))))

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
                   :annotate (lambda (key) (concat (prefix-padding-for-annotation key longest-key)
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

(provide 'consult-tycho)
;;; consult-tycho.el ends here
