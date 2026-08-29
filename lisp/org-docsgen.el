;;; org-docsgen.el --- Generate org API documentation from Emacs Lisp -*- lexical-binding: t; -*-

;;; Commentary:
;; Shared documentation generator for org-babel blocks in any `.org' file.
;; Call `org-docsgen-run' from a #+BEGIN_SRC emacs-lisp block to generate
;; org-mode API reference from the .el files in the same directory (or from
;; an explicit `:el-files' list).  Regenerate with `org-docsgen-regenerate-
;; file', `org-docsgen-regenerate-directory', or `org-docsgen-regenerate-
;; dwim'.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'cl-lib)
(require 'xtd-project)

(declare-function org-babel-execute-src-block "ob-core")

(defun org-docsgen--find-el-files ()
  "Return sorted non-test, non-generated .el files in the current org buffer's directory.
Excludes test files as well as `-pkg.el' and `-autoloads.el': these are
generated package metadata, never meant to be `require'd or scanned as
documentable source (`-pkg.el' in particular is only ever valid as data
read by package.el, not as code — it commonly contains unquoted symbols
like `:kind vc' that error if actually evaluated via `load')."
  (seq-sort
   #'string<
   (seq-remove
    (lambda (f) (string-match-p "-test\\.el$\\|test-.*\\.el$\\|/test/\\|-pkg\\.el$\\|-autoloads\\.el$" f))
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

(defun org-docsgen--fn-arg-names (fn-para)
  "Return the formal argument names in FN-PARA, a \"(fn ARG...)\" string.
Drops `&optional'/`&rest' markers.  Returns nil when FN-PARA is nil or
does not look like a signature paragraph."
  (when (and fn-para (string-match "\\`(fn \\(.*\\))\\'" (string-trim fn-para)))
    (seq-remove (lambda (s) (member s '("&optional" "&rest")))
                (split-string (match-string 1 (string-trim fn-para)) "[ \t]+" t))))

(defun org-docsgen--quote-symbol-refs (text)
  "Re-render curly-quoted symbol references in TEXT as org code spans.
`substitute-command-keys' has already turned the docstring's \\=' quote
convention into curly ‘...’ pairs; when the quoted name resolves to a
bound function, variable, or face, render it as a `~...~' org code span
instead so it reads as code rather than as scare-quoted prose.
Reads the name out of the whole match (stripping the curly quotes)
rather than a subexpression, since `match-data' is not reliably valid
inside a `replace-regexp-in-string' function REP once it calls back out
to other regexp-using code (e.g. `intern-soft')."
  (replace-regexp-in-string
   "‘[^’]+’"
   (lambda (whole)
     (let* ((name (substring whole 1 -1))
            (sym (intern-soft name)))
       (if (and sym (or (fboundp sym) (boundp sym) (facep sym)))
           (format "~%s~" name)
         whole)))
   text t t))

(defun org-docsgen--emphasize-args (text arg-names)
  "Wrap whole-word occurrences of ARG-NAMES in TEXT with org italic markup.
ARG-NAMES are the formal parameter names (as written in upper case in
the docstring prose, per Emacs Lisp docstring convention) collected by
`org-docsgen--fn-arg-names'."
  (if (null arg-names)
      text
    (let ((case-fold-search nil))
      (replace-regexp-in-string
       (regexp-opt arg-names 'words)
       (lambda (whole) (format "/%s/" whole))
       text t t))))

(defun org-docsgen--default-doc-filter (text arg-names)
  "Default `org-docsgen-doc-filter-function': annotate TEXT for org.
Renders known-symbol quoted references as code spans and italicizes
occurrences of ARG-NAMES (see `org-docsgen--quote-symbol-refs' and
`org-docsgen--emphasize-args')."
  (org-docsgen--emphasize-args (org-docsgen--quote-symbol-refs text) arg-names))

(defcustom org-docsgen-doc-filter-function #'org-docsgen--default-doc-filter
  "Function used to annotate docstring prose for org rendering.
Called with two arguments, TEXT (a paragraph of already-flowed
docstring prose, excluding the \"(fn ...)\" signature paragraph) and
ARG-NAMES (the symbol's formal parameter names, or nil), and must
return the annotated string.  Set to nil to disable annotation and emit
docstring prose verbatim.  Override with `org-docsgen-run''s
`:doc-filter' to use a different filter for one generation run."
  :type '(choice (const :tag "None" nil) function)
  :group 'org-docsgen)

(defun org-docsgen--format-doc (doc &optional doc-filter)
  "Format DOC string for org output, separating the arglist (fn ...) para.
DOC is expected to already have docstring escapes (e.g. `\\=' for a literal
quote) resolved by `substitute-command-keys' via `documentation'/
`documentation-property'; any that survive are stripped defensively so
the source-level escaping convention never leaks into the generated org
text.  DOC-FILTER defaults to `org-docsgen-doc-filter-function' and is
applied to the prose paragraphs (not the \"(fn ...)\" signature)."
  (let* ((doc (replace-regexp-in-string "\\\\=\\(.\\)" "\\1" doc))
         (paras (split-string doc "\n\n" t))
         (fn-para (seq-find (lambda (p) (string-match "\\`(fn " (string-trim p))) paras))
         (rest-paras (seq-remove (lambda (p) (string-match "\\`(fn " (string-trim p))) paras))
         (filter (if (eq doc-filter :default) org-docsgen-doc-filter-function doc-filter))
         (arg-names (org-docsgen--fn-arg-names fn-para)))
    (mapconcat #'identity
               (delq nil
                     (list
                      (when fn-para (concat ": " (string-trim fn-para)))
                      (when rest-paras
                        (mapconcat
                         (lambda (para)
                           (let ((flowed (if (string-match "\\`[ \t]" para)
                                              para
                                            (mapconcat #'string-trim (split-string para "\n" t) " "))))
                             (if filter (funcall filter flowed arg-names) flowed)))
                         rest-paras "\n\n"))))
               "\n\n")))

(defun org-docsgen--defkind (def-keyword)
  "Return a kind symbol for the DEF-KEYWORD string."
  (cond
   ((member def-keyword '("defvar" "defconst" "defvar-local")) 'variable)
   ((equal def-keyword "defcustom") 'custom)
   (t 'function)))

(defun org-docsgen--kind-tag (sym kind)
  "Return an org tag string for SYM with KIND, or nil when none applies."
  (cond
   ((commandp sym) ":command:")
   ((eq kind 'custom) ":option:")
   ((eq kind 'variable) ":variable:")))

(defun org-docsgen--format-sym (name kind heading &optional doc-filter)
  "Format a single symbol NAME of KIND under HEADING.
DOC-FILTER is passed through to `org-docsgen--format-doc'."
  (let* ((sym (intern name))
         (fn-p (fboundp sym))
         (var-p (and (not fn-p) (boundp sym)))
         (doc (or (when fn-p (documentation sym))
                  (when var-p (documentation-property sym 'variable-documentation))))
         (tag (org-docsgen--kind-tag sym kind)))
    (format "%s%s%s\n\n%s\n\n"
            (concat heading (if doc "" "TODO "))
            name
            (if tag (concat " " tag) "")
            (if doc (org-docsgen--format-doc doc doc-filter) "*no docstring*"))))

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

(defun org-docsgen--partition-by-groups (files group-spec)
  "Partition FILES into an alist of (GROUP-NAME . GROUP-FILES) based on GROUP-SPEC.
GROUP-SPEC can be:
  `file'  -- group by each file's base name
  a list  -- list of group specs. Each element can be:
             - a string prefix (e.g. \"agent-shell-menu\")
             - a list/cons (GROUP-NAME . MATCH-PREFIXES-OR-FILES)
               e.g. (\"agent-shell-queue-persistence\" \"agent-shell-queue-persistence\" \"agent-shell-queue-db\")
             files are matched against the longest/most specific pattern first,
             and emitted in the order of GROUP-SPEC."
  (cond
   ((eq group-spec 'file)
    (seq-map (lambda (f) (cons (file-name-base f) (list f))) files))
   ((listp group-spec)
    (let* ((patterns nil)
           (group-names nil))
      (seq-do
       (lambda (entry)
         (if (consp entry)
             (let ((grp (car entry))
                   (prefixes (if (listp (cdr entry)) (cdr entry) (list (cdr entry)))))
               (push grp group-names)
               (seq-do (lambda (p) (push (cons p grp) patterns)) prefixes))
           (push entry group-names)
           (push (cons entry entry) patterns)))
       group-spec)
      (setq group-names (nreverse (seq-uniq group-names)))
      (setq patterns (sort patterns (lambda (a b) (> (length (car a)) (length (car b))))))
      (let ((buckets (make-hash-table :test #'equal))
            (unmatched nil))
        (seq-do
         (lambda (f)
           (let* ((base (file-name-base f))
                  (matched-pair (seq-find (lambda (p) (string-prefix-p (car p) base)) patterns)))
             (if matched-pair
                 (puthash (cdr matched-pair)
                          (append (gethash (cdr matched-pair) buckets nil) (list f))
                          buckets)
               (push f unmatched))))
         files)
        (append
         (delq nil
               (seq-map
                (lambda (g)
                  (when-let* ((g-files (gethash g buckets)))
                    (cons g g-files)))
                group-names))
         (when unmatched
           (list (cons "Other" (nreverse unmatched))))))))
   (t
    (list (cons nil files)))))

(defun org-docsgen--emit (sections scope level &optional doc-filter)
  "Emit SECTIONS as org output via `princ' at heading depth relative to LEVEL.
Section headings are emitted at LEVEL+1; symbol headings at LEVEL+2.
When there are zero or one named sections the section heading is suppressed
and symbols are emitted at LEVEL+1 instead.  DOC-FILTER is passed through
to `org-docsgen--format-sym'."
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
                     (princ (org-docsgen--format-sym (car entry) (cadr entry) sym-h doc-filter)))
                   syms))))
     sections)))

;;;###autoload
(cl-defun org-docsgen-run (&key
                            el-files
                            (scope 'exported)
                            (include-kinds '(variables customs))
                            namespace
                            (section-style 'ruler)
                            group-by
                            (doc-filter :default))
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
\(;;; Section Name headers used by agent-shell-queue).

DOC-FILTER overrides `org-docsgen-doc-filter-function' for this run;
pass nil to emit docstring prose verbatim, unannotated."
  (let ((level (org-docsgen--current-level))
        (doc-filter (if (eq doc-filter :default) org-docsgen-doc-filter-function doc-filter)))
    (org-docsgen--clear-subtree)
    (let ((files (or el-files (org-docsgen--find-el-files))))
      (seq-do (lambda (f)
                (add-to-list 'load-path (file-name-directory f))
                (require (intern (file-name-base f)) nil t))
              files)
      ;; Buffer the whole body and trim trailing blank lines: each entry ends
      ;; in "\n\n" for spacing between entries, which would otherwise leave a
      ;; dangling blank line (or more) after the last one.
      (princ
       (concat
        (string-trim-right
         (with-output-to-string
           (if (listp scope)
               (seq-do (lambda (name)
                         (princ (org-docsgen--format-sym name 'function
                                                         (org-docsgen--heading (1+ level))
                                                         doc-filter)))
                       scope)
             (if group-by
                 (let ((groups (org-docsgen--partition-by-groups files group-by)))
                   (seq-do
                    (lambda (grp)
                      (let* ((grp-name (car grp))
                             (grp-files (cdr grp))
                             (sections (if (eq section-style 'triple-semi)
                                           (org-docsgen--collect-triple-semi grp-files scope include-kinds namespace)
                                         (org-docsgen--collect-ruler grp-files scope include-kinds namespace))))
                        (when sections
                          (when grp-name
                            (princ (format "%s%s\n\n" (org-docsgen--heading (1+ level)) grp-name)))
                          (org-docsgen--emit sections scope (if grp-name (1+ level) level) doc-filter))))
                    groups))
               (let ((sections (if (eq section-style 'triple-semi)
                                   (org-docsgen--collect-triple-semi files scope include-kinds namespace)
                                 (org-docsgen--collect-ruler files scope include-kinds namespace))))
                 (org-docsgen--emit sections scope level doc-filter)))))))))))

(defun org-docsgen--buffer-has-run-p (&optional buffer)
  "Return non-nil when BUFFER (default the current buffer) has an
`org-docsgen-run' src block."
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (and (re-search-forward "^#\\+BEGIN_SRC emacs-lisp" nil t)
           (re-search-forward "org-docsgen-run" nil t)))))

(defun org-docsgen--file-has-run-p (file)
  "Return non-nil when FILE contains an `org-docsgen-run' src block."
  (with-temp-buffer
    (insert-file-contents file)
    (org-docsgen--buffer-has-run-p)))

(defun org-docsgen--org-files-in-directory (dir)
  "Return `.org' files directly in DIR (non-recursive) with a docsgen block."
  (seq-filter #'org-docsgen--file-has-run-p
              (directory-files dir t "\\.org\\'")))

(defun org-docsgen--org-files-in-tree (dir)
  "Return `.org' files under DIR (recursively) with a docsgen block."
  (seq-filter #'org-docsgen--file-has-run-p
              (directory-files-recursively dir "\\.org\\'")))

(defun org-docsgen--execute-run-block (file)
  "Execute the `org-docsgen-run' block in FILE and save the buffer."
  (with-current-buffer (find-file-noselect file)
    (save-excursion
      (goto-char (point-min))
      (unless (re-search-forward "^#\\+BEGIN_SRC emacs-lisp" nil t)
        (user-error "org-docsgen: no emacs-lisp source block in %s" file))
      (unless (re-search-forward "org-docsgen-run" nil t)
        (user-error "org-docsgen: no `org-docsgen-run' call in %s" file))
      (let ((org-confirm-babel-evaluate nil))
        (org-babel-execute-src-block)))
    (save-buffer))
  (message "org-docsgen: regenerated %s" file))

(defun org-docsgen--read-target-file ()
  "Prompt for a docsgen target file, preferring the current buffer's file.
Completion candidates are `.org' files with a docsgen block found
anywhere under `default-directory'."
  (or (and buffer-file-name (org-docsgen--buffer-has-run-p) buffer-file-name)
      (let ((candidates (org-docsgen--org-files-in-tree default-directory)))
        (if candidates
            (completing-read "Regenerate docsgen file: " candidates nil t)
          (read-file-name "Regenerate docsgen file: " nil nil t)))))

;;;###autoload
(defun org-docsgen-regenerate-file (file)
  "Regenerate the `org-docsgen-run' block in FILE and save it.
FILE may be any org file containing such a block -- a README, a file
under a docs/ directory, or anything else; there is nothing
README-specific about this command.
Interactively, defaults to the current buffer's file when it already
has a docsgen block, otherwise prompts with completion over `.org'
files found anywhere under `default-directory'.  Intended for use by
agent skills via emacsclient:
  emacsclient --eval \\='(org-docsgen-regenerate-file \"docs/api.org\")\\='"
  (interactive (list (org-docsgen--read-target-file)))
  (let ((expanded (expand-file-name file)))
    (unless (file-exists-p expanded)
      (user-error "org-docsgen-regenerate-file: no such file %s" expanded))
    (org-docsgen--execute-run-block expanded)))

;;;###autoload
(defun org-docsgen-regenerate-directory (&optional dir)
  "Regenerate every docsgen block among the `.org' files directly in DIR.
DIR defaults to `default-directory'.  Only files directly in DIR are
considered, not subdirectories -- use `org-docsgen-regenerate-file' with
its recursive completion, or call this once per directory, to cover a
whole tree.  Signals a `user-error' when DIR has no `.org' file with an
`org-docsgen-run' block."
  (interactive (list (read-directory-name "Regenerate docsgen directory: " default-directory)))
  (let* ((dir (or dir default-directory))
         (files (org-docsgen--org-files-in-directory dir)))
    (unless files
      (user-error "org-docsgen-regenerate-directory: no docsgen block found in %s" dir))
    (dolist (file files)
      (org-docsgen--execute-run-block file))))

;;;###autoload
(defun org-docsgen-regenerate-dwim ()
  "Regenerate docsgen docs for the current context.
When the current buffer visits a file with an `org-docsgen-run' block,
regenerate that buffer in place.  Otherwise regenerate every docsgen
block among the `.org' files directly in `default-directory', falling
back to `approximate-project-root'."
  (interactive)
  (cond
   ((and buffer-file-name (org-docsgen--buffer-has-run-p))
    (org-docsgen-regenerate-file buffer-file-name))
   ((org-docsgen--org-files-in-directory default-directory)
    (org-docsgen-regenerate-directory default-directory))
   ((org-docsgen--org-files-in-directory (approximate-project-root))
    (org-docsgen-regenerate-directory (approximate-project-root)))
   (t
    (user-error "org-docsgen-regenerate-dwim: no docsgen block found in `default-directory' or project root"))))

(provide 'org-docsgen)
;;; org-docsgen.el ends here
