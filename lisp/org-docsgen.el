;;; org-docsgen.el --- Generate org API documentation from Emacs Lisp -*- lexical-binding: t; -*-

;;; Commentary:
;; Shared documentation generator for README.org org-babel blocks.
;; Call `org-docsgen-run' from a #+BEGIN_SRC emacs-lisp block to generate
;; org-mode API reference from the .el files in the same directory.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'cl-lib)

(declare-function org-babel-execute-src-block "ob-core")

(defun org-docsgen--find-el-files ()
  "Return sorted non-test .el files in the directory of the current org buffer."
  (seq-sort
   #'string<
   (seq-remove
    (lambda (f) (string-match-p "-test\\.el$\\|test-.*\\.el$\\|/test/" f))
    (directory-files (file-name-directory (buffer-file-name)) t "\\.el$" t))))

(defun org-docsgen--current-level ()
  "Return the outline level of the heading enclosing the current position."
  (save-excursion
    (org-back-to-heading t)
    (org-current-level)))

(defun org-docsgen--heading (level)
  "Return an org heading prefix string of LEVEL stars followed by a space."
  (concat (make-string level ?*) " "))

(defun org-docsgen--clear-subtree ()
  "Delete child headings and content from the current org heading."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (org-end-of-subtree t) (point))))
      (forward-line 1)
      (while (and (< (point) end) (not (org-at-heading-p)))
        (forward-line 1))
      (when (< (point) end)
        (delete-region (point) end)))))

(defun org-docsgen--section-name (comment-text)
  "Convert a comment COMMENT-TEXT to a title-cased section name."
  (let* ((name (string-trim (car (split-string comment-text " -- "))))
         (name (replace-regexp-in-string "-" " " name)))
    (mapconcat (lambda (s) (if (string-match-p "\\." s) s (capitalize s)))
               (split-string name "[ \t]+" t) " ")))

(defun org-docsgen--format-doc (doc)
  "Format DOC string for org output, separating the arglist (fn ...) para."
  (let* ((paras (split-string doc "\n\n" t))
         (fn-para (seq-find (lambda (p) (string-match "\\`(fn " (string-trim p))) paras))
         (rest-paras (seq-remove (lambda (p) (string-match "\\`(fn " (string-trim p))) paras)))
    (mapconcat #'identity
               (delq nil
                     (list
                      (when fn-para (concat ": " (string-trim fn-para)))
                      (when rest-paras
                        (mapconcat
                         (lambda (para)
                           (if (string-match "\\`[ \t]" para)
                               para
                             (mapconcat #'string-trim (split-string para "\n" t) " ")))
                         rest-paras "\n\n"))))
               "\n\n")))

(defun org-docsgen--defkind (def-keyword)
  "Return a kind symbol for the DEF-KEYWORD string."
  (cond
   ((member def-keyword '("defvar" "defconst" "defvar-local")) 'variable)
   ((equal def-keyword "defcustom") 'custom)
   (t 'function)))

(defun org-docsgen--kind-tag (sym kind)
  "Return a tag string for SYM with KIND."
  (cond
   ((commandp sym) " [Command]")
   ((eq kind 'custom) " [Option]")
   ((eq kind 'variable) " [Variable]")
   (t "")))

(defun org-docsgen--format-sym (name kind heading)
  "Format a single symbol NAME of KIND under HEADING."
  (let* ((sym (intern name))
         (fn-p (fboundp sym))
         (var-p (and (not fn-p) (boundp sym)))
         (doc (or (when fn-p (documentation sym))
                  (when var-p (documentation-property sym 'variable-documentation)))))
    (format "%s~%s~%s\n\n%s\n\n"
            (concat heading (if doc "" "TODO "))
            name
            (org-docsgen--kind-tag sym kind)
            (if doc (org-docsgen--format-doc doc) "*no docstring*"))))

(defun org-docsgen--include-p (name kind autoload-p nil-init-p scope include-kinds namespace)
  "Return non-nil when NAME/KIND should be included in the output.
AUTOLOAD-P is t when preceded by ;;;###autoload.
NIL-INIT-P is t when the form is a bare `(defvar NAME nil ...)' forward declaration.
SCOPE, INCLUDE-KINDS, NAMESPACE come from `org-docsgen-run'."
  (and (not (string-match-p "[a-z]--" name))
       ;; Exclude foreign forward declarations: (defvar NAME nil) outside namespace.
       (not (and (eq kind 'variable)
                 nil-init-p
                 namespace
                 (not (string-prefix-p namespace name))))
       (cond
        ((eq kind 'variable) (memq 'variables include-kinds))
        ((eq kind 'custom)
         (or (memq 'customs include-kinds)
             (and (memq scope '(exported autoloaded)) autoload-p)))
        (t
         (or (eq scope 'exported)
             (and (eq scope 'autoloaded) autoload-p)
             (eq scope 'interactive))))))

(defun org-docsgen--collect-ruler (el-files scope include-kinds namespace)
  "Collect sections from EL-FILES using long ;;;;... ruler + ;; Name delimiters.
Returns an alist of (SECTION-NAME-OR-NIL . SYMS) in source order."
  (let (sections current-section current-syms after-ruler autoload-next)
    (seq-do
     (lambda (el-file)
       (with-temp-buffer
         (insert-file-contents el-file)
         (goto-char (point-min))
         (while (not (eobp))
           (let ((line (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position))))
             (cond
              ((string-match "^;\\{10,\\}" line)
               (when current-syms
                 (push (cons current-section (nreverse current-syms)) sections))
               (setq current-section nil current-syms nil after-ruler t))
              ((and after-ruler (string-match "^;; \\(.+\\)" line))
               (setq current-section (org-docsgen--section-name (match-string 1 line))
                     after-ruler nil))
              ((string-match "^;;;###autoload" line)
               (setq autoload-next t))
              ((string-match "^(\\(?:cl-\\)?\\(def[^ ]+\\) +'?\\([^ ()]+\\)\\( nil\\b\\)?" line)
               (let* ((def-kw (match-string 1 line))
                      (sym-name (match-string 2 line))
                      (nil-init (not (null (match-string 3 line))))
                      (kind (org-docsgen--defkind def-kw)))
                 (when (org-docsgen--include-p sym-name kind autoload-next nil-init
                                               scope include-kinds namespace)
                   (push (list sym-name kind) current-syms))
                 (setq autoload-next nil)))
              ((not (string-empty-p (string-trim line)))
               (setq autoload-next nil))))
           (forward-line 1)))
       (when current-syms
         (push (cons current-section (nreverse current-syms)) sections))
       (setq current-section nil current-syms nil after-ruler nil autoload-next nil))
     el-files)
    (nreverse sections)))

(defun org-docsgen--collect-triple-semi (el-files scope include-kinds namespace)
  "Collect sections from EL-FILES using ;;; Section Name delimiters.
Returns an alist of (SECTION-NAME-OR-NIL . SYMS) in source order."
  (let (sections current-section current-syms autoload-next)
    (seq-do
     (lambda (el-file)
       (with-temp-buffer
         (insert-file-contents el-file)
         (goto-char (point-min))
         (while (not (eobp))
           (let ((line (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position))))
             (cond
              ((and (string-match "^;;; \\([A-Z][^:\n]+\\)$" line)
                    (not (member (match-string 1 line) '("Commentary" "Code"))))
               (when current-syms
                 (push (cons current-section (nreverse current-syms)) sections))
               (setq current-section (match-string 1 line)
                     current-syms nil
                     autoload-next nil))
              ((string-match "^;;;###autoload" line)
               (setq autoload-next t))
              ((string-match "^(\\(?:cl-\\)?\\(def[^ ]+\\) +'?\\([^ ()]+\\)\\( nil\\b\\)?" line)
               (let* ((def-kw (match-string 1 line))
                      (sym-name (match-string 2 line))
                      (nil-init (not (null (match-string 3 line))))
                      (kind (org-docsgen--defkind def-kw)))
                 (when (org-docsgen--include-p sym-name kind autoload-next nil-init
                                               scope include-kinds namespace)
                   (push (list sym-name kind) current-syms))
                 (setq autoload-next nil)))
              ((not (string-empty-p (string-trim line)))
               (setq autoload-next nil))))
           (forward-line 1)))
       (when current-syms
         (push (cons current-section (nreverse current-syms)) sections))
       (setq current-section nil current-syms nil autoload-next nil))
     el-files)
    (nreverse sections)))

(defun org-docsgen--emit (sections scope level)
  "Emit SECTIONS as org output via `princ' at heading depth relative to LEVEL.
Section headings are emitted at LEVEL+1; symbol headings at LEVEL+2.
When there are zero or one named sections the section heading is suppressed
and symbols are emitted at LEVEL+1 instead."
  (let* ((named-sections (seq-filter #'car sections))
         (flat-p (<= (length named-sections) 1))
         (section-h (org-docsgen--heading (1+ level)))
         (sym-h (org-docsgen--heading (if flat-p (1+ level) (+ 2 level)))))
    (seq-do
     (lambda (section)
       (let ((heading (car section))
             (syms (if (eq scope 'interactive)
                       (seq-filter (lambda (e) (commandp (intern (car e)))) (cdr section))
                     (cdr section))))
         (when syms
           (when (and heading (not flat-p))
             (princ (format "%s%s\n\n" section-h heading)))
           (seq-do (lambda (entry)
                     (princ (org-docsgen--format-sym (car entry) (cadr entry) sym-h)))
                   syms))))
     sections)))

;;;###autoload
(cl-defun org-docsgen-run (&key
                            el-files
                            (scope 'exported)
                            (include-kinds '(variables customs))
                            namespace
                            (section-style 'ruler))
  "Generate org-mode API documentation and princ it to stdout.

EL-FILES is a list of .el paths to document; defaults to all non-test .el
files in the directory of the current buffer.

SCOPE controls which definitions are included:
  `exported'    -- all public symbols (no -- in name)
  `autoloaded'  -- only ;;;###autoload-annotated symbols
  `interactive' -- all interactive commands
  A list        -- explicit symbol names, emitted in order

INCLUDE-KINDS lists additional kinds beyond what SCOPE selects:
  `variables' -- defvar / defconst / defvar-local
  `customs'   -- defcustom

NAMESPACE is a string prefix (e.g. \"sprite\").  A `(defvar NAME nil)'
form whose NAME does not start with NAMESPACE is treated as a foreign
forward declaration and excluded.

SECTION-STYLE is `ruler' (default: long ;;;;... lines) or `triple-semi'
\(;;; Section Name headers used by agent-shell-queue)."
  (let ((level (org-docsgen--current-level)))
    (org-docsgen--clear-subtree)
    (let ((files (or el-files (org-docsgen--find-el-files))))
      (seq-do (lambda (f)
                (add-to-list 'load-path (file-name-directory f))
                (require (intern (file-name-base f)) nil t))
              files)
      (if (listp scope)
          (seq-do (lambda (name)
                    (princ (org-docsgen--format-sym name 'function
                                                    (org-docsgen--heading (1+ level)))))
                  scope)
        (let ((sections (if (eq section-style 'triple-semi)
                            (org-docsgen--collect-triple-semi files scope include-kinds namespace)
                          (org-docsgen--collect-ruler files scope include-kinds namespace))))
          (org-docsgen--emit sections scope level))))))

;;;###autoload
(defun org-docsgen-regenerate-readme (readme-file)
  "Regenerate README-FILE's API reference by re-running its `org-docsgen-run' block.
Locates the first `#+BEGIN_SRC emacs-lisp' block calling `org-docsgen-run',
executes it, and saves the buffer.  Binds `org-confirm-babel-evaluate' to
nil for the duration of the call so this can run non-interactively (e.g.
via emacsclient) without blocking on a confirmation prompt -- the block is
config-authored source, not untrusted content, so skipping the prompt here
is safe.  Intended for use by agent skills via emacsclient:
  emacsclient --eval \\='(org-docsgen-regenerate-readme \"external/foo/README.org\")\\='"
  (interactive "fREADME.org file: ")
  (let ((expanded (expand-file-name readme-file)))
    (with-current-buffer (find-file-noselect expanded)
      (save-excursion
        (goto-char (point-min))
        (unless (re-search-forward "^#\\+BEGIN_SRC emacs-lisp" nil t)
          (user-error "org-docsgen-regenerate-readme: no emacs-lisp source block in %s" expanded))
        (unless (re-search-forward "org-docsgen-run" nil t)
          (user-error "org-docsgen-regenerate-readme: no `org-docsgen-run' call in %s" expanded))
        (let ((org-confirm-babel-evaluate nil))
          (org-babel-execute-src-block)))
      (save-buffer))
    (message "org-docsgen: regenerated %s" expanded)))

(provide 'org-docsgen)
;;; org-docsgen.el ends here
