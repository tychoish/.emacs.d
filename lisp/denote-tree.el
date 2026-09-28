;;; denote-tree.el --- Structural sequence tree manipulation, linting, and refactoring for Denote -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (denote "3.0.0") (denote-sequence "0.3.0") (annotated-completing-read "0.1.0"))
;; Keywords: docs, denote, convenience, tools
;; URL: https://github.com/tychoish/denote-tree

;; This file is not part of GNU Emacs.

;;; Commentary:
;; A standalone extension for `denote-sequence' providing structural operations,
;; refactoring, and integrity tooling for Denote sequence trees (Luhmann-style
;; folgezettel):
;; - Sequence alignment linting, diagnostics (Flymake & Flycheck), and frontmatter auto-fix
;; - Child sequence gap compaction (repacking) with interactive preview
;; - Sibling and parent subtree swapping
;; - Recursive reparenting and renumbering with alphanumeric alternation rewriting
;; - Sequence insertion with subsequent sibling shifting
;; - Bulk subtree keyword retagging
;; - Sequence hierarchy fold state management and smart auto-folding
;; - Public lifecycle notification hooks for external extensions

;;; Code:

(require 'seq)
(require 'map)
(require 'subr-x)
(require 'outline)
(require 'denote)
(require 'denote-sequence)
(require 'annotated-completing-read)

(defvar flycheck-checkers)
(defvar savehist-additional-variables)
(defvar denote-file-types)
(defvar denote-sequence-hierarchy-mode-map)
(autoload 'annotated-completing-read-table "annotated-completing-read")

(declare-function flymake-make-diagnostic "flymake")
(declare-function flycheck-define-generic-checker "flycheck")
(declare-function flycheck-error-new-at "flycheck")

(defgroup denote-tree nil
  "Structural sequence tree manipulation, linting, and refactoring for Denote."
  :group 'denote
  :link '(url-link "https://github.com/tychoish/denote-tree"))

;;; Lifecycle notification hooks

(defcustom denote-tree-after-rename-functions nil
  "Hook run after a single sequence note rename.
Each function is called with four arguments:
  (OLD-FILE NEW-FILE OLD-SIG NEW-SIG)."
  :group 'denote-tree
  :type 'hook)

(defcustom denote-tree-after-reseq-functions nil
  "Hook run after a batch subtree renumbering or reparenting operation.
Each function is called with one argument: PLAN, an alist of (FILE . NEW-SEQ)."
  :group 'denote-tree
  :type 'hook)

(defcustom denote-tree-after-lint-fix-functions nil
  "Hook run after fixing sequence frontmatter.
Called with argument FILES (the list of modified files)."
  :group 'denote-tree
  :type 'hook)

(defcustom denote-tree-after-swap-functions nil
  "Hook run after a sibling or parent sequence swap.
Each function is called with four arguments:
  (SEQ-A SEQ-B FILES-A FILES-B)."
  :group 'denote-tree
  :type 'hook)

;;; Sequence utilities

(defun denote-tree--char-digit-p (c)
  "Return non-nil if character C is an ASCII decimal digit."
  (and (>= c ?0) (<= c ?9)))

(defun denote-tree--sequence-descendant-p (ancestor descendant)
  "Return non-nil if DESCENDANT is a descendant of ANCESTOR.
Both are sequence ID strings using the denote alphanumeric scheme, where
hierarchy levels alternate between digit and letter characters."
  (when (and ancestor descendant
             (not (string= ancestor descendant))
             (string-prefix-p ancestor descendant))
    (let ((next-char (aref descendant (length ancestor)))
          (last-char (aref ancestor (1- (length ancestor)))))
      (not (eq (denote-tree--char-digit-p last-char)
               (denote-tree--char-digit-p next-char))))))

(defun denote-tree--sequence-depth (seq-id)
  "Return the nesting depth of SEQ-ID (0 = root, 1 = first child level, etc.)."
  (if (or (null seq-id) (string-empty-p seq-id))
      0
    (let ((depth 0)
          (prev nil))
      (dolist (c (string-to-list seq-id) depth)
        (when (and prev
                   (not (eq (denote-tree--char-digit-p prev)
                            (denote-tree--char-digit-p c))))
          (setq depth (1+ depth)))
        (setq prev c)))))

(defun denote-tree--direct-child-p (parent child)
  "Return non-nil if CHILD is an immediate child of PARENT.
Both are sequence ID strings in the sequence hierarchy."
  (and (denote-tree--sequence-descendant-p parent child)
       (= (denote-tree--sequence-depth child)
          (1+ (denote-tree--sequence-depth parent)))))

;;; Sequence hierarchy folding

(defcustom denote-tree-hierarchy-initial-fold-depth nil
  "Depth to collapse to when the hierarchy view first opens.
nil (default) shows everything expanded, matching upstream.  An integer N
folds anything deeper than N levels — equivalent to `outline-hide-sublevels'."
  :type '(choice (const :tag "Expand all" nil) natnum)
  :group 'denote-tree)

(defvar denote-tree-hierarchy-fold-sequences nil
  "Sequence-ID strings whose whole subtree starts folded, unconditionally.
Toggled from a `denote-sequence-hierarchy-mode' buffer with
`denote-tree-hierarchy-toggle-fold-sequence' rather than customized
statically; persisted across sessions via `savehist-mode'.")

(defcustom denote-tree-hierarchy-auto-fold-min-size nil
  "Fold a top-level section (root sequence + descendants) this small.
A section counts as its root note plus every descendant.  nil disables
this rule."
  :type '(choice (const :tag "Disabled" nil) natnum)
  :group 'denote-tree)

(defcustom denote-tree-hierarchy-auto-fold-max-size nil
  "Fold a top-level section (root sequence + descendants) larger than this.
nil disables the rule."
  :type '(choice (const :tag "Disabled" nil) natnum)
  :group 'denote-tree)

(defun denote-tree--hierarchy-heading-positions ()
  "Return a list of (POINT LEVEL SEQUENCE) for every heading in the buffer.
LEVEL comes from the `denote-sequence-hierarchy-level' text property and
SEQUENCE from `denote-retrieve-filename-signature' on the file at that
property; both are set by `denote-sequence-view-hierarchy'."
  (let (result)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when-let* ((level (get-text-property (point) 'denote-sequence-hierarchy-level))
                    (file (get-text-property (point) 'denote-sequence-hierarchy-file)))
          (push (list (point) level (denote-retrieve-filename-signature file)) result))
        (forward-line 1)))
    (nreverse result)))

(defun denote-tree--hierarchy-section-sizes (headings)
  "Return an alist of (POINT . SIZE) for each entry in HEADINGS.
HEADINGS is the list returned by `denote-tree--hierarchy-heading-positions'.
SIZE counts the heading itself plus every following heading whose level is
strictly deeper, stopping at the next heading whose level is the same or
shallower."
  (let (sizes)
    (while headings
      (let* ((entry (car headings))
             (level (nth 1 entry))
             (size 1))
        (catch 'done
          (dolist (other (cdr headings))
            (if (> (nth 1 other) level)
                (setq size (1+ size))
              (throw 'done nil))))
        (push (cons (nth 0 entry) size) sizes))
      (setq headings (cdr headings)))
    (nreverse sizes)))

(defun denote-tree--hierarchy-should-fold-p (seq size)
  "Return non-nil if top-level section SEQ of size SIZE should be folded."
  (or (member seq denote-tree-hierarchy-fold-sequences)
      (and denote-tree-hierarchy-auto-fold-min-size
           (<= size denote-tree-hierarchy-auto-fold-min-size))
      (and denote-tree-hierarchy-auto-fold-max-size
           (> size denote-tree-hierarchy-auto-fold-max-size))))

(defun denote-tree--hierarchy-apply-initial-fold ()
  "Fold sections of a freshly populated hierarchy buffer per user options.
Composes `denote-tree-hierarchy-initial-fold-depth',
`denote-tree-hierarchy-fold-sequences',
`denote-tree-hierarchy-auto-fold-min-size', and
`denote-tree-hierarchy-auto-fold-max-size' — a section folds if any
enabled rule applies to it."
  (when denote-tree-hierarchy-initial-fold-depth
    (outline-hide-sublevels denote-tree-hierarchy-initial-fold-depth))
  (when (or denote-tree-hierarchy-fold-sequences
            denote-tree-hierarchy-auto-fold-min-size
            denote-tree-hierarchy-auto-fold-max-size)
    (let* ((headings (denote-tree--hierarchy-heading-positions))
           (sizes (denote-tree--hierarchy-section-sizes headings)))
      (dolist (entry headings)
        (pcase-let ((`(,pos ,level ,seq) entry))
          (when (= level 1)
            (when (denote-tree--hierarchy-should-fold-p seq (cdr (assq pos sizes)))
              (save-excursion
                (goto-char pos)
                (outline-hide-subtree)))))))))

(add-hook 'denote-sequence-hierarchy-mode-hook #'denote-tree--hierarchy-apply-initial-fold t)

(defun denote-tree--hierarchy-goto-root ()
  "Move point to the top-level (level 1) heading enclosing point."
  (while (> (denote-sequence-hierarchy-get-level) 1)
    (outline-up-heading 1 t)))

;;;###autoload
(defun denote-tree-hierarchy-toggle-fold-sequence ()
  "Toggle persistent folding of the sequence section at point.
Adds or removes the root sequence at point from
`denote-tree-hierarchy-fold-sequences' — remembered across sessions via
`savehist-mode', not a static default — and folds or unfolds the section
to match immediately."
  (interactive)
  (save-excursion
    (denote-tree--hierarchy-goto-root)
    (if-let* ((file (get-text-property (point) 'denote-sequence-hierarchy-file))
              (seq (denote-retrieve-filename-signature file)))
        (if (member seq denote-tree-hierarchy-fold-sequences)
            (progn
              (setq denote-tree-hierarchy-fold-sequences
                    (remove seq denote-tree-hierarchy-fold-sequences))
              (outline-show-subtree)
              (message "Sequence %s: fold no longer persisted" seq))
          (setq denote-tree-hierarchy-fold-sequences
                (cons seq denote-tree-hierarchy-fold-sequences))
          (outline-hide-subtree)
          (message "Sequence %s: will stay folded" seq))
      (user-error "No sequence heading at point"))))

;;;###autoload
(defun denote-tree-hierarchy-clear-fold-sequences ()
  "Forget every sequence toggled to stay folded, then refresh the view."
  (interactive)
  (setq denote-tree-hierarchy-fold-sequences nil)
  (message "Cleared all persisted sequence folds")
  (when (derived-mode-p 'denote-sequence-hierarchy-mode)
    (revert-buffer)))

;;; Sequence hierarchy fold-sequence remapping across renames

(defun denote-tree--hierarchy-remap-fold-sequence-prefix (old-seq new-seq)
  "Rewrite entries under OLD-SEQ to NEW-SEQ in the persisted fold list.
Any entry equal to OLD-SEQ, or with OLD-SEQ as a proper prefix (a folded
descendant of a renamed subtree), has that prefix replaced by NEW-SEQ."
  (when (and old-seq new-seq (not (equal old-seq new-seq)))
    (setq denote-tree-hierarchy-fold-sequences
          (seq-map (lambda (s)
                     (if (string-prefix-p old-seq s)
                         (concat new-seq (substring s (length old-seq)))
                       s))
                   denote-tree-hierarchy-fold-sequences))))

(defun denote-tree--hierarchy-remap-fold-sequence-many (pairs)
  "Rewrite fold-list entries per PAIRS, a list of (OLD-SEQ . NEW-SEQ).
Only exact matches are rewritten."
  (setq denote-tree-hierarchy-fold-sequences
        (seq-map (lambda (s) (or (cdr (assoc s pairs)) s))
                 denote-tree-hierarchy-fold-sequences)))

(defun denote-tree--hierarchy-swap-fold-sequence (seq-a seq-b)
  "Swap SEQ-A and SEQ-B wherever they appear (exact match) in the fold list."
  (setq denote-tree-hierarchy-fold-sequences
        (seq-map (lambda (s)
                   (cond ((equal s seq-a) seq-b)
                         ((equal s seq-b) seq-a)
                         (t s)))
                 denote-tree-hierarchy-fold-sequences)))

(defun denote-tree--hierarchy-swap-fold-sequence-prefix (seq-a seq-b)
  "Swap SEQ-A and SEQ-B subtree prefixes in the persisted fold list."
  (setq denote-tree-hierarchy-fold-sequences
        (seq-map (lambda (s)
                   (cond ((string-prefix-p seq-a s) (concat seq-b (substring s (length seq-a))))
                         ((string-prefix-p seq-b s) (concat seq-a (substring s (length seq-b))))
                         (t s)))
                 denote-tree-hierarchy-fold-sequences)))

;;; Context file resolution and annotated selection

(defun denote-tree--file-at-point ()
  "Return the Denote file implied by current point/buffer context, or nil."
  (cond
   ((derived-mode-p 'denote-sequence-hierarchy-mode)
    (get-text-property (point) 'denote-sequence-hierarchy-file))
   ((and (fboundp 'tabulated-list-get-id)
         (or (derived-mode-p 'tabulated-list-mode)
             (eq major-mode 'denote-dash-mode)))
    (let ((id (tabulated-list-get-id)))
      (when (and (stringp id) (file-exists-p id)) id)))
   ((derived-mode-p 'dired-mode)
    (let ((f (dired-get-filename nil t)))
      (when (and f (denote-file-has-identifier-p f)) f)))
   ((and buffer-file-name (denote-file-has-identifier-p buffer-file-name))
    buffer-file-name)))

(defun denote-tree--note-label (file)
  "Return label for FILE in completion."
  (if-let* ((title (denote-retrieve-title-or-filename file (denote-filetype-heuristics file)))
            (sig (denote-retrieve-filename-signature file))
            ((not (string-empty-p sig))))
      (format "[D:%s] %s" sig title)
    title))

(defun denote-tree--note-annotation (file)
  "Return a keywords + last-modified-date annotation string for FILE."
  (string-join
   (append (denote-extract-keywords-from-path file)
           (list (format-time-string "%Y-%m-%d" (file-attribute-modification-time (file-attributes file)))))
   "  "))

(defun denote-tree--note-labels (files)
  "Return an alist of (LABEL . FILE) for FILES, disambiguating duplicate labels."
  (let ((labeled (seq-map (lambda (f) (cons (denote-tree--note-label f) f)) files))
        (counts (make-hash-table :test #'equal)))
    (seq-do (lambda (pair) (setf (map-elt counts (car pair)) (1+ (or (map-elt counts (car pair)) 0))))
            labeled)
    (seq-map (lambda (pair)
               (if (> (map-elt counts (car pair)) 1)
                   (cons (format "%s (%s)" (car pair) (denote-retrieve-filename-identifier (cdr pair)))
                         (cdr pair))
                 pair))
             labeled)))

;;;###autoload
(defun denote-tree-note-prompt (&optional prompt default-file files)
  "Read a Denote note file via an annotated-completing-read UI.
Candidates cover FILES, or every note in `denote-directory-files' when nil.
PROMPT overrides the default prompt text.  DEFAULT-FILE, or
`denote-tree--file-at-point' when nil, pre-selects a candidate."
  (let* ((candidates (or files (denote-directory-files)))
         (labels (denote-tree--note-labels candidates))
         (default-file (or default-file (denote-tree--file-at-point)))
         (default-label (when default-file
                          (car (rassoc default-file labels))))
         (prompt-str (or prompt
                         (if default-label
                             (format "Note (default %s): " default-label)
                           "Note: ")))
         (table (annotated-completing-read-table
                 labels
                 :annotation (lambda (cand)
                               (when-let* ((f (cdr (assoc cand labels))))
                                 (format "  %s" (denote-tree--note-annotation f))))
                 :predicate nil)))
    (let ((choice (completing-read prompt-str table nil t nil nil default-label)))
      (or (cdr (assoc choice labels))
          (user-error "No note selected")))))

(defun denote-tree--context-file ()
  "Return the context Denote file or prompt the user."
  (or (denote-tree--file-at-point)
      (denote-tree-note-prompt "File: " nil (denote-directory-files))))

(defun denote-tree--target-file ()
  "Return the target file for sequence operations.
Uses `denote-tree--file-at-point', else prompts among sequence-bearing notes."
  (or (denote-tree--file-at-point)
      (denote-tree-note-prompt "File: " nil (seq-filter #'denote-sequence-file-p
                                                        (denote-directory-files)))))

;;; Sequence alignment lint / autofix

(defun denote-tree--signature-line (sig file-type)
  "Return a complete frontmatter signature line for SIG given FILE-TYPE.
Uses the value-formatting function from `denote-file-types' so quotes are
added for Markdown types (YAML/TOML) and omitted for Org/text."
  (let* ((entry (alist-get file-type denote-file-types))
         (val-fn (plist-get entry :signature-value-function))
         (formatted (if val-fn (funcall val-fn sig) sig)))
    (pcase file-type
      ((or 'org 'text) (format "#+signature: %s" formatted))
      ('markdown-yaml  (format "signature: %s" formatted))
      ('markdown-toml  (format "signature = %s" formatted))
      (_               (format "#+signature: %s" sig)))))

(defun denote-tree--kill-visiting-buffers (files)
  "Kill buffers visiting any file in FILES."
  (dolist (f files)
    (when-let* ((b (find-buffer-visiting f)))
      (kill-buffer b))))

(defun denote-tree--format-lint-entry (file)
  "Format a lint report line for FILE."
  (let* ((file-type (denote-filetype-heuristics file))
         (fs (denote-retrieve-filename-signature file))
         (fms (denote-retrieve-front-matter-signature-value file file-type)))
    (format "  %-40s  filename=%-10s  frontmatter=%s\n"
            (file-name-nondirectory file)
            (or fs "—")
            (or fms "—"))))

(defun denote-tree--sequence-aligned-p (file)
  "Return non-nil if FILE's filename and frontmatter signatures agree."
  (let* ((file-type (denote-filetype-heuristics file))
         (filename-sig (denote-retrieve-filename-signature file))
         (fm-sig (denote-retrieve-front-matter-signature-value file file-type)))
    (equal filename-sig fm-sig)))

(defun denote-tree--fix-frontmatter-from-filename (file)
  "Update FILE's frontmatter signature to match its filename signature.
Returns t if a change was made, nil if already aligned.
- Filename sig present, frontmatter differs or absent: write/insert signature.
- Frontmatter sig present, filename has none: delete the frontmatter line."
  (let* ((file-type (denote-filetype-heuristics file))
         (filename-sig (denote-retrieve-filename-signature file))
         (fm-sig (denote-retrieve-front-matter-signature-value file file-type)))
    (unless (equal filename-sig fm-sig)
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (goto-char (point-min))
          (cond
           ;; Both present but differ: rewrite the existing frontmatter line
           ((and filename-sig fm-sig)
            (denote--rewrite-front-matter-line
             'signature
             (denote-tree--signature-line filename-sig file-type)
             file-type))
           ;; Filename has sig, frontmatter doesn't: insert after identifier line
           ((and filename-sig (null fm-sig))
            (when-let* ((key-fn (denote--get-component-key-regexp-function 'identifier))
                        (id-re (funcall key-fn file-type))
                        (_ (re-search-forward id-re nil t)))
              (end-of-line)
              (insert "\n" (denote-tree--signature-line filename-sig file-type))))
           ;; Frontmatter has sig, filename doesn't: delete the frontmatter line
           ((and (null filename-sig) fm-sig)
            (when-let* ((key-fn (denote--get-component-key-regexp-function 'signature))
                        (sig-re (funcall key-fn file-type))
                        (_ (re-search-forward sig-re nil t)))
              (delete-region (line-beginning-position)
                             (min (1+ (line-end-position)) (point-max)))))))
        (save-buffer))
      t)))

(defun denote-tree--collect-sequence-mismatches ()
  "Return Denote files whose filename and frontmatter signatures disagree."
  (seq-filter (lambda (f) (not (denote-tree--sequence-aligned-p f)))
              (denote-directory-files)))

;;;###autoload
(defun denote-tree-lint-sequences ()
  "Show all Denote notes whose filename and frontmatter signatures are misaligned."
  (interactive)
  (let* ((mismatches (denote-tree--collect-sequence-mismatches))
         (buf (get-buffer-create "*Denote Sequence Lint*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if (null mismatches)
            (insert "All sequence notes are aligned.\n")
          (insert (format "%d mismatch%s:\n\n"
                          (length mismatches)
                          (if (= (length mismatches) 1) "" "es")))
          (dolist (file mismatches)
            (insert (denote-tree--format-lint-entry file)))
          (insert "\nRun `denote-tree-fix-all-sequence-frontmatter' to fix "
                  "(filename authoritative).\n"
                  "Run `denote-rename-file-using-front-matter' per-file to go "
                  "the other direction.\n")))
      (special-mode))
    (pop-to-buffer buf)))

;;;###autoload
(defun denote-tree-fix-sequence-frontmatter ()
  "Fix frontmatter signature to match filename for note at point or file.
Resolves target file via `denote-tree--file-at-point', so it works from
`denote-tree-mode', `denote-sequence-hierarchy-mode', Dired, or a visited
Denote buffer."
  (interactive)
  (when-let* ((file (denote-tree--file-at-point)))
    (if (denote-tree--fix-frontmatter-from-filename file)
        (progn
          (message "Fixed: %s" (file-name-nondirectory file))
          (run-hook-with-args 'denote-tree-after-lint-fix-functions (list file))
          (when (derived-mode-p 'denote-sequence-hierarchy-mode) (revert-buffer)))
      (message "Already aligned: %s" (file-name-nondirectory file)))))

;;;###autoload
(defun denote-tree-fix-all-sequence-frontmatter ()
  "Fix frontmatter for mismatched Denote notes, filename as truth."
  (interactive)
  (let* ((mismatches (denote-tree--collect-sequence-mismatches))
         (n (length mismatches)))
    (when (zerop n)
      (user-error "No sequence frontmatter mismatches found"))
    (unless (yes-or-no-p (format "Fix frontmatter in %d note%s? "
                                 n (if (= n 1) "" "s")))
      (user-error "Cancelled"))
    (let ((fixed 0) (errors 0))
      (seq-do (lambda (file)
                (condition-case err
                    (when (denote-tree--fix-frontmatter-from-filename file)
                      (setq fixed (1+ fixed)))
                  (error
                   (setq errors (1+ errors))
                   (message "Error in %s: %s"
                            (file-name-nondirectory file)
                            (error-message-string err)))))
              mismatches)
      (run-hook-with-args 'denote-tree-after-lint-fix-functions mismatches)
      (message "Fixed %d/%d note%s%s."
               fixed n (if (= fixed 1) "" "s")
               (if (> errors 0) (format " (%d errors)" errors) "")))
      (when (derived-mode-p 'denote-sequence-hierarchy-mode) (revert-buffer))
    ))


;;; Flymake and Flycheck diagnostic backend

;;;###autoload
(defun denote-tree-flymake (report-fn &rest _args)
  "Flymake backend for Denote sequence alignment diagnostics.
Reports a warning diagnostic when a visited note has mismatched
filename and frontmatter signatures."
  (when (and buffer-file-name (denote-file-has-identifier-p buffer-file-name))
    (let* ((file buffer-file-name)
           (file-type (denote-filetype-heuristics file))
           (fn-sig (denote-retrieve-filename-signature file))
           (fm-sig (denote-retrieve-front-matter-signature-value file file-type))
           diags)
      (unless (equal fn-sig fm-sig)
        (save-excursion
          (goto-char (point-min))
          (let* ((key-fn (denote--get-component-key-regexp-function 'signature))
                 (sig-re (and key-fn (funcall key-fn file-type)))
                 (found (and sig-re (re-search-forward sig-re nil t)))
                 (beg (if found (line-beginning-position) (point-min)))
                 (end (if found (line-end-position) (min (point-max) (+ (point-min) 20))))
                 (msg (format "Sequence signature mismatch: filename=%s, frontmatter=%s"
                              (or fn-sig "(none)") (or fm-sig "(none)"))))
            (push (flymake-make-diagnostic (current-buffer) beg end :warning msg) diags))))
      (funcall report-fn diags))))

;;;###autoload
(defun denote-tree-setup-flymake ()
  "Enable Flymake integration for Denote sequence alignment in the current buffer."
  (interactive)
  (add-hook 'flymake-diagnostic-functions #'denote-tree-flymake nil t))

(with-eval-after-load 'flycheck
  (when (fboundp 'flycheck-define-generic-checker)
    (flycheck-define-generic-checker 'denote-tree-sequence
      "Flycheck checker for Denote sequence signature frontmatter alignment."
      :start (lambda (checker callback)
               (let ((file buffer-file-name)
                     diags)
                 (when (and file (denote-file-has-identifier-p file))
                   (let* ((file-type (denote-filetype-heuristics file))
                          (fn-sig (denote-retrieve-filename-signature file))
                          (fm-sig (denote-retrieve-front-matter-signature-value file file-type)))
                     (unless (equal fn-sig fm-sig)
                       (save-excursion
                         (goto-char (point-min))
                         (let* ((key-fn (denote--get-component-key-regexp-function 'signature))
                                (sig-re (and key-fn (funcall key-fn file-type)))
                                (found (and sig-re (re-search-forward sig-re nil t)))
                                (line (if found (line-number-at-pos) 1))
                                (msg (format "Sequence signature mismatch: filename=%s, frontmatter=%s"
                                             (or fn-sig "(none)") (or fm-sig "(none)"))))
                           (push (flycheck-error-new-at line 1 'warning msg :checker checker) diags))))))
                 (funcall callback 'finished diags)))
      :modes '(org-mode markdown-mode text-mode)
      :predicate (lambda () (and buffer-file-name (denote-file-has-identifier-p buffer-file-name))))
    (add-to-list 'flycheck-checkers 'denote-tree-sequence t)))

;;; Sequence repack

(defun denote-tree--sequence-direct-children (prefix)
  "Return all Denote files that are direct children of PREFIX.
If PREFIX is nil or empty, returns all root-level sequence files (depth 0).
A direct child has exactly one more alternating segment than PREFIX."
  (let* ((pfx-depth (if (and prefix (not (string-empty-p (or prefix ""))))
                        (denote-tree--sequence-depth prefix)
                      -1))
         (target-depth (1+ pfx-depth))
         (files (if (and prefix (not (string-empty-p (or prefix ""))))
                    (denote-sequence-get-all-files-with-prefix prefix)
                  (denote-sequence-get-all-files))))
    (seq-filter
     (lambda (f)
       (when-let* ((sig (denote-retrieve-filename-signature f)))
         (= (denote-tree--sequence-depth sig) target-depth)))
     files)))

(defun denote-tree--compact-child-seq (n prefix)
  "Return the Nth (1-based) compact child sequence for PREFIX.
Root level (nil/empty PREFIX) and letter-ending PREFIX use numbers (1, 2, 3…).
Digit-ending PREFIX uses letters (a, b, c…)."
  (let* ((last-char (and prefix
                         (not (string-empty-p (or prefix "")))
                         (aref prefix (1- (length prefix)))))
         (use-letters (and last-char (denote-tree--char-digit-p last-char)))
         (suffix (if use-letters
                     (char-to-string (+ ?a (1- n)))
                   (number-to-string n))))
    (concat (or prefix "") suffix)))

(defun denote-tree--fix-all-frontmatter-silent ()
  "Fix all sequence frontmatter mismatches without confirmation.  Returns count."
  (let ((fixed 0))
    (seq-do (lambda (file)
              (condition-case nil
                  (when (denote-tree--fix-frontmatter-from-filename file)
                    (setq fixed (1+ fixed)))
                (error nil)))
            (denote-tree--collect-sequence-mismatches))
    fixed))

(defun denote-tree--repack-subtree-plan (child-file new-child-seq)
  "Build plan pairs for CHILD-FILE and its subtree under NEW-CHILD-SEQ."
  (let ((old-child-seq (denote-retrieve-filename-signature child-file)))
    (mapcar (lambda (f)
              (cons f (concat new-child-seq
                              (string-remove-prefix
                               old-child-seq
                               (denote-retrieve-filename-signature f)))))
            (denote-tree--subtree-files old-child-seq))))

(defun denote-tree--repack-plan (to-rename)
  "Expand TO-RENAME child pairs into a flat file rename plan.
TO-RENAME is a list of (CHILD-FILE . NEW-CHILD-SEQ) pairs.  Each child is
expanded to its full subtree — captured from the current on-disk state,
before any renames happen — so descendants stay consistent with their
parent's new sequence.  Return a list of (FILE . NEW-SEQ) pairs, one per
file that must be renamed: the child itself and all of its descendants."
  (mapcan (lambda (pair)
            (denote-tree--repack-subtree-plan (car pair) (cdr pair)))
          to-rename))

(defun denote-tree--apply-repack-plan (plan)
  "Rename every (FILE . NEW-SEQ) pair in PLAN, returning the count renamed.
Every file in PLAN is staged to a unique temporary signature before any
final rename happens.  This is essential: renaming subtrees one at a time
can leave an old-but-not-yet-renamed sibling and a freshly-renamed file
briefly sharing the same signature, which a later subtree's prefix lookup
would then sweep up and merge into the wrong destination.  Staging the
whole batch first means no real signature is ever a temporary duplicate."
  (denote-tree--kill-visiting-buffers (mapcar #'car plan))
  (let ((staged (seq-map-indexed
                 (lambda (pair i)
                   (let* ((file (car pair))
                          (new-seq (cdr pair))
                          (sig (denote-retrieve-filename-signature file))
                          (tmp-sig (format "rpacktmp%d" i))
                          (tmp-path (denote-tree--rename-signature-component file sig tmp-sig)))
                     (rename-file file tmp-path t)
                     (list tmp-path tmp-sig new-seq)))
                 plan)))
    (seq-do (lambda (entry)
              (pcase-let ((`(,tmp-path ,tmp-sig ,new-seq) entry))
                (let ((new-path (denote-tree--rename-signature-component
                                 tmp-path tmp-sig new-seq)))
                  (rename-file tmp-path new-path t)
                  (denote-tree--fix-frontmatter-from-filename new-path))))
            staged)
    (length staged)))

(defun denote-tree--repack-preview-buffer (plan)
  "Populate and return the *Denote Repack Preview* buffer describing PLAN.
PLAN is a list of (FILE . NEW-SEQ) pairs, as returned by
`denote-tree--repack-plan'."
  (let ((buf (get-buffer-create "*Denote Repack Preview*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%d file%s will be renamed:\n\n"
                        (length plan) (if (= (length plan) 1) "" "s")))
        (seq-do (lambda (pair)
                  (let* ((file (car pair))
                         (new-seq (cdr pair))
                         (old-seq (denote-retrieve-filename-signature file)))
                    (insert (format "  %-10s -> %-10s  %s\n"
                                    old-seq new-seq (file-name-nondirectory file)))))
                plan))
      (special-mode))
    buf))

(defun denote-tree--dismiss-repack-preview (buf)
  "Dismiss the repack preview buffer BUF by quitting its window and killing it."
  (when (and buf (buffer-live-p buf))
    (when-let* ((win (get-buffer-window buf)))
      (quit-window t win))
    (when (buffer-live-p buf)
      (kill-buffer buf))))
;;;###autoload
(defun denote-tree-repack-children (prefix)
  "Compact direct children of PREFIX so their last segment has no gaps.
Renames each child's entire subtree (child and all descendants) so that
descendants stay consistent with their parent's new sequence.  Children
are ordered by sequence value (not lexicographically, so \"10\" sorts
after \"2\" rather than before it), and the whole batch of renames is
staged through temporary signatures together so that repacking multiple
children at once never merges two of them into the same destination.

Before touching any file, shows a preview buffer listing every rename
that would happen and asks for confirmation; declining leaves every file
untouched.  Works from `denote-tree-mode' or interactively."
  (interactive
   (list (read-string "Sequence prefix (empty = root): "
                      (when (derived-mode-p 'denote-tree-mode)
                        (when-let* ((f (tabulated-list-get-id)))
                          (denote-retrieve-filename-signature f))))))
  (let* ((prefix (if (string-empty-p prefix) nil prefix))
         (children (denote-tree--sequence-direct-children prefix))
         (sorted (or (denote-sequence-sort-files children)
                     (user-error "No children found for %s" (or prefix "(root)")))))
    (let* ((expected (seq-map-indexed
                      (lambda (_f i)
                        (denote-tree--compact-child-seq (1+ i) prefix))
                      sorted))
           (to-rename (seq-filter
                       (lambda (pair)
                         (not (string= (denote-retrieve-filename-signature (car pair))
                                       (cdr pair))))
                       (seq-mapn #'cons sorted expected))))
      (cond
       ((null to-rename)
        (message "Sequences for %s already compact." (or prefix "(root)")))
       (t
        (let* ((plan (denote-tree--repack-plan to-rename))
               (prev-buf (denote-tree--repack-preview-buffer plan)))
          (pop-to-buffer prev-buf)
          (if (yes-or-no-p (format "Rename %d file%s of %s as previewed? "
                                   (length plan) (if (= (length plan) 1) "" "s")
                                   (or prefix "(root)")))
              (progn
                (denote-tree--dismiss-repack-preview prev-buf)
                (seq-do (lambda (pair)
                          (denote-tree--hierarchy-remap-fold-sequence-prefix
                           (denote-retrieve-filename-signature (car pair)) (cdr pair)))
                        to-rename)
                (denote-tree--apply-repack-plan plan)
                (let ((n (denote-tree--fix-all-frontmatter-silent)))
                  (message "Repacked %d/%d children of %s; fixed %d frontmatter %s."
                           (length to-rename) (length sorted) (or prefix "(root)")
                           n (if (= n 1) "note" "notes"))))
            (denote-tree--dismiss-repack-preview prev-buf)
            (message "Repack cancelled; no files were changed."))))))
    ))

;;; Sequence swap

(defun denote-tree--sequence-parent (seq)
  "Return the parent sequence of SEQ, or nil if SEQ is a root.
The parent is the longest proper prefix whose depth is one less than SEQ's.
Computed by finding the last type-transition (digit↔letter) in SEQ."
  (when (and seq (not (string-empty-p seq))
             (> (denote-tree--sequence-depth seq) 0))
    (let ((last-transition nil))
      (dotimes (i (1- (length seq)))
        (when (not (eq (denote-tree--char-digit-p (aref seq i))
                       (denote-tree--char-digit-p (aref seq (1+ i)))))
          (setq last-transition (1+ i))))
      (when last-transition
        (substring seq 0 last-transition)))))

(defun denote-tree--rename-signature-component (file old-sig new-sig)
  "Return FILE's path with its ==SIG== component changed from OLD-SIG to NEW-SIG."
  (let ((dir (file-name-directory file))
        (base (file-name-nondirectory file)))
    (concat dir (replace-regexp-in-string
                 (concat "==" (regexp-quote old-sig) "--")
                 (concat "==" new-sig "--")
                 base t t))))

;;;###autoload
(defun denote-tree-swap-with-parent ()
  "Swap the sequence of the note at point with its direct parent.
Both nodes must have files in the denote directory.  Uses a three-step
rename (child→tmp, parent→child-seq, tmp→parent-seq) to avoid collision,
then fixes frontmatter signatures on both files."
  (interactive)
  (let* ((file (denote-tree--target-file))
         (seq (or (denote-retrieve-filename-signature file)
                  (user-error "File has no sequence: %s" (file-name-nondirectory file))))
         (parent-seq (or (denote-tree--sequence-parent seq)
                         (user-error "%s is a root sequence — nothing to swap with" seq)))
         (parent-file (or (seq-find (lambda (f)
                                      (equal (denote-retrieve-filename-signature f) parent-seq))
                                    (denote-directory-files))
                          (user-error "No file found for parent sequence %s" parent-seq))))
    (unless (yes-or-no-p (format "Swap %s ↔ %s? " seq parent-seq))
      (user-error "Cancelled"))
    (let ((new-path (denote-tree--rename-signature-component file seq parent-seq))
          (new-par-path (denote-tree--rename-signature-component parent-file parent-seq seq))
          (tmp-path (denote-tree--rename-signature-component file seq "__swaptmp__")))
      (denote-tree--kill-visiting-buffers (list file parent-file))
      (rename-file file tmp-path t)
      (rename-file parent-file new-par-path t)
      (rename-file tmp-path new-path t)
      (denote-tree--fix-frontmatter-from-filename new-path)
      (denote-tree--fix-frontmatter-from-filename new-par-path)
      (denote-tree--hierarchy-swap-fold-sequence seq parent-seq)
      (run-hook-with-args 'denote-tree-after-swap-functions seq parent-seq (list file) (list parent-file))
      (message "Swapped %s ↔ %s" seq parent-seq)
      )))

;;; Sequence sibling swap

(defun denote-tree--subtree-files (seq)
  "Return the file for SEQ and all its descendant files, sequence-sorted."
  (denote-sequence-sort-files (denote-sequence-get-all-files-with-prefix seq)))

(defun denote-tree--sequence-siblings (seq)
  "Return the sorted list of SEQ's direct siblings, SEQ included.
Siblings share SEQ's parent, or are all root sequences when SEQ is one."
  (denote-sequence-sort-files
   (denote-tree--sequence-direct-children (denote-tree--sequence-parent seq))))

(defun denote-tree--swap-subtrees (seq-a seq-b)
  "Swap the sequence subtrees rooted at SEQ-A and SEQ-B.
Recursively renames SEQ-A, SEQ-B, and all their descendants so the two
subtrees exchange positions, then fixes the frontmatter signature of
every renamed file.  SEQ-A and SEQ-B must be direct siblings."
  (let* ((files-a (denote-tree--subtree-files seq-a))
         (files-b (denote-tree--subtree-files seq-b)))
    (unless files-a (user-error "No files found for sequence %s" seq-a))
    (unless files-b (user-error "No files found for sequence %s" seq-b))
    (denote-tree--hierarchy-swap-fold-sequence-prefix seq-a seq-b)
    (run-hook-with-args 'denote-tree-after-swap-functions seq-a seq-b files-a files-b)
    (denote-tree--kill-visiting-buffers (append files-a files-b))
    ;; Stage 1: move SEQ-A's subtree aside under unique temp signatures,
    ;; remembering each file's original signature to recover it later.
    (let ((staged
           (seq-map-indexed
            (lambda (f i)
              (let* ((sig (denote-retrieve-filename-signature f))
                     (tmp-sig (format "swaptmp%d" i))
                     (tmp-path (denote-tree--rename-signature-component f sig tmp-sig)))
                (rename-file f tmp-path t)
                (list sig tmp-sig tmp-path)))
            files-a)))
      ;; Stage 2: move SEQ-B's subtree into SEQ-A's namespace.
      (seq-do (lambda (f)
                (let* ((old-sig (denote-retrieve-filename-signature f))
                       (new-sig (concat seq-a (substring old-sig (length seq-b))))
                       (new-path (denote-tree--rename-signature-component f old-sig new-sig)))
                  (rename-file f new-path t)
                  (denote-tree--fix-frontmatter-from-filename new-path)))
              files-b)
      ;; Stage 3: move the staged SEQ-A subtree into SEQ-B's namespace.
      (seq-do (lambda (entry)
                (pcase-let ((`(,orig-sig ,tmp-sig ,tmp-path) entry))
                  (let* ((new-sig (concat seq-b (substring orig-sig (length seq-a))))
                         (new-path (denote-tree--rename-signature-component tmp-path tmp-sig new-sig)))
                    (rename-file tmp-path new-path t)
                    (denote-tree--fix-frontmatter-from-filename new-path))))
              staged))))

(defun denote-tree--swap-with-sibling (direction)
  "Swap the note at point (with its subtree) with a sibling.
DIRECTION is the symbol `previous' or `next', selecting which of the
current sequence's siblings to swap with."
  (let* ((file (denote-tree--target-file))
         (seq (denote-retrieve-filename-signature file)))
    (unless seq
      (user-error "File has no sequence: %s" (file-name-nondirectory file)))
    (let* ((siblings (denote-tree--sequence-siblings seq))
           (position (seq-position (seq-map #'denote-retrieve-filename-signature siblings) seq))
           (other (pcase direction
                    ('previous (and position (> position 0)
                                    (nth (1- position) siblings)))
                    ('next (and position
                                (nth (1+ position) siblings))))))
      (unless position
        (error "Cannot locate %s among its own siblings" seq))
      (unless other
        (user-error "No %s sibling for sequence %s" direction seq))
      (let ((other-seq (denote-retrieve-filename-signature other)))
        (unless (yes-or-no-p (format "Swap %s ↔ %s (with descendants)? " seq other-seq))
          (user-error "Cancelled"))
        (denote-tree--swap-subtrees seq other-seq)
        (message "Swapped %s ↔ %s" seq other-seq)
        ))))

;;;###autoload
(defun denote-tree-swap-with-previous ()
  "Swap the note at point, with its subtree, with its previous sibling.
Descendants of both nodes move along with their parent, so the whole
subtrees exchange positions.  See also `denote-tree-swap-with-parent'."
  (interactive)
  (denote-tree--swap-with-sibling 'previous))

;;;###autoload
(defun denote-tree-swap-with-next ()
  "Swap the note at point, with its subtree, with its next sibling.
Descendants of both nodes move along with their parent, so the whole
subtrees exchange positions.  See also `denote-tree-swap-with-parent'."
  (interactive)
  (denote-tree--swap-with-sibling 'next))

;;; Recursive reparent with correct alphanumeric suffix rewriting

(defun denote-tree--seq-last-type (seq)
  "Return :digit or :letter for the type of the last character in SEQ."
  (if (denote-tree--char-digit-p (aref seq (1- (length seq))))
      :digit
    :letter))

(defun denote-tree--segment-to-number (str type)
  "Convert segment STR of TYPE (:digit or :letter) to an integer."
  (if (eq type :digit)
      (string-to-number str)
    (string-to-number (denote-sequence--alpha-to-number str))))

(defun denote-tree--number-to-segment (num type)
  "Convert NUM to a segment string of TYPE (:digit or :letter)."
  (if (eq type :digit)
      (number-to-string num)
    (denote-sequence--number-to-alpha (number-to-string num))))

(defun denote-tree--seq-split-segments (suffix first-type)
  "Parse SUFFIX into list of integer segment values starting with FIRST-TYPE."
  (let ((pos 0)
        (current-type first-type)
        (positions nil))
    (while (< pos (length suffix))
      (let ((start pos))
        (if (eq current-type :digit)
            (progn
              (while (and (< pos (length suffix))
                          (denote-tree--char-digit-p (aref suffix pos)))
                (setq pos (1+ pos)))
              (push (denote-tree--segment-to-number (substring suffix start pos) :digit)
                    positions)
              (setq current-type :letter))
          (while (and (< pos (length suffix))
                      (not (denote-tree--char-digit-p (aref suffix pos))))
            (setq pos (1+ pos)))
          (push (denote-tree--segment-to-number (substring suffix start pos) :letter)
                positions)
          (setq current-type :digit))))
    (nreverse positions)))

(defun denote-tree--alphanumeric-suffix-rewrite (suffix old-root-last-type new-root-last-type)
  "Rewrite SUFFIX so it is valid under a root ending with NEW-ROOT-LAST-TYPE.
OLD-ROOT-LAST-TYPE is the type of the last character of the old root (:digit
or :letter).  Returns the suffix unchanged when both types agree."
  (if (eq old-root-last-type new-root-last-type)
      suffix
    (let* ((old-first (if (eq old-root-last-type :digit) :letter :digit))
           (new-first (if (eq new-root-last-type :digit) :letter :digit))
           (positions (denote-tree--seq-split-segments suffix old-first))
           (rebuild-type new-first)
           (result ""))
      (dolist (p positions result)
        (setq result (concat result (denote-tree--number-to-segment p rebuild-type)))
        (setq rebuild-type (if (eq rebuild-type :digit) :letter :digit))))))

(defun denote-tree--reparent-target-sequence (file-with-sequence)
  "Return the destination sequence for a reparent operation.
When FILE-WITH-SEQUENCE names a file or a sequence, return a new child of
it.  When nil, return a new top-level sequence instead — the \"become a
root\" case that plain `denote-sequence-reparent' has no path for: it can
only ever make CURRENT-FILE a child of some other file.  `renumber-recursive'
is the only existing command that can send a note to an arbitrary target
sequence, including one with no parent, which is why NEW-SEQ is computed
the same way here as there."
  (if file-with-sequence
      (let ((target-seq (or (denote-sequence-file-p file-with-sequence)
                            (denote-sequence-p file-with-sequence)
                            (user-error "No sequence found in `%s'" file-with-sequence))))
        (denote-sequence--get-new-child target-seq))
    (denote-sequence--get-new-parent)))

(defun denote-tree--recursive-reseq-plan (current-file new-seq)
  "Return the rename plan for moving CURRENT-FILE's subtree to NEW-SEQ.
Plan is a list of (FILE . NEW-SEQ) pairs.  Includes CURRENT-FILE itself
plus every descendant, each rewritten under NEW-SEQ with its relative
suffix preserved, correcting the type alternation (letter/digit) of
descendant sequences when the old and new roots end in different character
types — a bug in upstream `denote-sequence-reparent-recursive'.  Shared by
`denote-tree--reparent-recursive-apply' and `denote-tree-renumber-recursive'."
  (let* ((root-seq (denote-retrieve-filename-signature current-file))
         (descendants (when root-seq
                        (denote-sequence-get-relative root-seq 'all-children)))
         (old-last-type (when root-seq (denote-tree--seq-last-type root-seq)))
         (new-last-type (denote-tree--seq-last-type new-seq)))
    (cons
     (cons current-file new-seq)
     (seq-keep (lambda (child)
                 (when-let* ((child-seq (denote-retrieve-filename-signature child)))
                   (cons child
                         (concat new-seq
                                 (denote-tree--alphanumeric-suffix-rewrite
                                  (string-remove-prefix root-seq child-seq)
                                  old-last-type new-last-type)))))
               descendants))))

(defun denote-tree--apply-reseq-plan (plan)
  "Rename every (FILE . NEW-SEQ) pair in PLAN via `denote-rename-file'.
Unlike `denote-tree--apply-repack-plan', entries need no staging through
temporary signatures: reparent/renumber targets always land under a
different subtree than any source file, so no two entries can collide
mid-rename.  Suppresses `denote-rename-confirmations' for the duration —
per-file prompting would be unusable across a whole subtree."
  (let ((denote-rename-confirmations nil))
    (seq-do (lambda (pair)
              (denote-rename-file (car pair) 'keep-current 'keep-current
                                  (cdr pair) 'keep-current 'keep-current))
            plan)))

(defun denote-tree--recursive-reseq-confirm-and-apply (plan operation-verb)
  "Apply PLAN, previewing and confirming first when it spans multiple files.
PLAN is a list of (FILE . NEW-SEQ) pairs, as returned by
`denote-tree--recursive-reseq-plan'.  When PLAN has only one entry (the
target file has no descendants), applies it directly with no prompt — the
operation can only ever affect that one file.  When PLAN has more than one
entry, pops the same *Denote Repack Preview* buffer used by
`denote-tree-repack-children' and asks for confirmation before
renaming anything; declining leaves every file untouched.

OPERATION-VERB names the operation for the confirmation prompt and the
cancellation message (e.g. \"Reparent\", \"Renumber\").

Returns non-nil if PLAN was applied, nil if the user declined."
  (if (<= (length plan) 1)
      (progn (denote-tree--apply-reseq-plan plan) t)
    (let ((prev-buf (denote-tree--repack-preview-buffer plan)))
      (pop-to-buffer prev-buf)
      (if (yes-or-no-p (format "%s %d files (this note + %d descendant%s) as previewed? "
                               operation-verb (length plan) (1- (length plan))
                               (if (= (length plan) 2) "" "s")))
          (progn
            (denote-tree--dismiss-repack-preview prev-buf)
            (denote-tree--apply-reseq-plan plan)
            t)
        (denote-tree--dismiss-repack-preview prev-buf)
        (message "%s cancelled; no files were changed." operation-verb)
        nil))))

(defun denote-tree--execute-reseq (current-file new-seq operation-verb)
  "Execute recursive resequence of CURRENT-FILE to NEW-SEQ with OPERATION-VERB."
  (let* ((plan (denote-tree--recursive-reseq-plan current-file new-seq))
         (seq-pairs (seq-map (lambda (pair)
                               (cons (denote-retrieve-filename-signature (car pair)) (cdr pair)))
                             plan)))
    (when (denote-tree--recursive-reseq-confirm-and-apply plan operation-verb)
      (denote-tree--hierarchy-remap-fold-sequence-many seq-pairs)
      (run-hook-with-args 'denote-tree-after-reseq-functions plan)
      t)))

(defun denote-tree--reparent-recursive-apply (current-file new-seq)
  "Re-parent CURRENT-FILE and all descendants onto NEW-SEQ.
Builds the plan via `denote-tree--recursive-reseq-plan' and applies it via
`denote-tree--recursive-reseq-confirm-and-apply', which previews and
confirms whenever CURRENT-FILE has descendants and applies directly,
without prompting, when it does not."
  (denote-tree--execute-reseq current-file new-seq "Reparent"))

;;;###autoload
(defun denote-tree-reparent (current-file file-with-sequence &optional recursive)
  "Re-parent CURRENT-FILE to be a child of FILE-WITH-SEQUENCE.
Wraps `denote-sequence-reparent'.  That command's own interactive spec
resolves CURRENT-FILE via `denote-sequence--get-current-file-for-renaming'
and then, purely to build the target prompt's label text, calls the same
private file-or-prompt helper a second time — doubling the raw \"Rename
FILE Denote-style\" prompt whenever neither Dired nor a visited buffer is
in scope, which is exactly the case from a `denote-dash' or
sequence-hierarchy buffer.  This resolves CURRENT-FILE once via
`denote-tree--target-file' and reuses it for the label instead.

FILE-WITH-SEQUENCE may be nil, meaning \"make CURRENT-FILE a new top-level
sequence instead of a child of anything\" — plain `denote-sequence-reparent'
has no such path; it can only reparent onto another file.  Interactively
this is offered as its own y-or-n-p question before the child-of prompt,
so promoting a note out of its hierarchy doesn't require reaching for
`denote-tree-renumber-recursive' and typing a sequence by hand.

Only offers the \"reparent recursively?\" question when CURRENT-FILE
actually has descendants; a leaf note is reparented directly with no
recursion question at all, since recursing over zero descendants is a
no-op.  When it does have descendants and RECURSIVE ends up non-nil, the
multi-file rename goes through `denote-tree--reparent-recursive-apply',
which previews and confirms before touching any file; see that function.

Suppresses `denote-rename-confirmations' for the duration: this is a single
logical operation, and per-file prompting (as `denote-rename-file' does by
default) would be unusable for any subtree beyond a couple of files."
  (interactive
   (let* ((current-file (denote-tree--target-file))
          (root-p (y-or-n-p
                   (format "Make `%s' a new top-level sequence (no parent)? "
                           (propertize current-file 'face 'denote-faces-prompt-current-name))))
          (has-descendants (when-let* ((seq (denote-retrieve-filename-signature current-file)))
                              (denote-sequence-get-relative seq 'all-children))))
     (list
      current-file
      (unless root-p
        (denote-sequence-file-prompt
         (format "Reparent `%s' to be a child of"
                 (propertize current-file 'face 'denote-faces-prompt-current-name))))
      (and has-descendants
           (y-or-n-p "Reparent recursively (include descendants)? ")))))
  (let ((denote-rename-confirmations nil)
        (old-seq (denote-retrieve-filename-signature current-file)))
    (if recursive
        (denote-tree--reparent-recursive-apply
         current-file (denote-tree--reparent-target-sequence file-with-sequence))
      (if file-with-sequence
          (let ((new-seq (denote-tree--reparent-target-sequence file-with-sequence)))
            (denote-sequence-reparent current-file file-with-sequence nil)
            (denote-tree--hierarchy-remap-fold-sequence-prefix old-seq new-seq))
        (let ((new-seq (denote-sequence--get-new-parent)))
          (denote-rename-file current-file 'keep-current 'keep-current
                              new-seq 'keep-current 'keep-current)
          (denote-tree--hierarchy-remap-fold-sequence-prefix old-seq new-seq))))))

;;;###autoload
(defun denote-tree-reparent-recursive (current-file file-with-sequence)
  "Re-parent CURRENT-FILE and all descendants to be children of FILE-WITH-SEQUENCE.
FILE-WITH-SEQUENCE may be nil, meaning \"promote to a new top-level
sequence\"; see `denote-tree-reparent' for the full explanation.

Direct entry point for the recursive case; see `denote-tree-reparent' for
the interactive command that asks whether to recurse instead of requiring
a separate command.  Still previews and confirms via
`denote-tree--reparent-recursive-apply' whenever CURRENT-FILE has
descendants; only a genuinely single-file case is prompt-free."
  (interactive
   (let* ((current-file (denote-tree--target-file))
          (root-p (y-or-n-p
                   (format "Make `%s' a new top-level sequence (no parent)? "
                           (propertize current-file 'face 'denote-faces-prompt-current-name)))))
     (list
      current-file
      (unless root-p
        (denote-sequence-file-prompt
         (format "Reparent `%s' (recursively) to be a child of"
                 (propertize current-file 'face 'denote-faces-prompt-current-name)))))))
  (denote-tree--reparent-recursive-apply
   current-file (denote-tree--reparent-target-sequence file-with-sequence)))

;;;###autoload
(defun denote-tree-renumber-recursive (current-file new-seq)
  "Renumber CURRENT-FILE to NEW-SEQ, renumbering all descendants to match.
Like `denote-tree-reparent-recursive', but NEW-SEQ is the target sequence
itself rather than derived as a new child of another file's sequence — use
this to move a subtree to a specific sequence ID instead of appending it
under a parent.

Builds the plan via `denote-tree--recursive-reseq-plan' and applies it via
`denote-tree--recursive-reseq-confirm-and-apply' — previewing and
confirming whenever CURRENT-FILE has descendants, applying directly
without a prompt when it does not."
  (interactive
   (let ((current-file (denote-tree--target-file)))
     (list
      current-file
      (denote-sequence-with-error-p
       (read-string
        (format "Renumber `%s' (recursively) to sequence: "
                (propertize current-file 'face 'denote-faces-prompt-current-name)))))))
  (unless (denote-retrieve-filename-signature current-file)
    (user-error "File has no sequence: %s" (file-name-nondirectory current-file)))
  (denote-tree--execute-reseq current-file new-seq "Renumber"))

;;; Sequence insertion

(defun denote-tree--increment-sequence (seq)
  "Return SEQ with its last component incremented."
  (let* ((parts (denote-sequence-split seq))
         (new-last (denote-sequence-increment-partial (car (last parts))))
         (scheme (cdr (denote-sequence-and-scheme-p seq))))
    (denote-sequence-join (append (butlast parts) (list new-last)) scheme)))

(defun denote-tree--sequence-direct-sibling-p (seq-id file)
  "Return non-nil if FILE is a direct sibling of SEQ-ID (same depth)."
  (when-let* ((fsig (denote-retrieve-filename-signature file)))
    (= (length (denote-sequence-split fsig))
       (length (denote-sequence-split seq-id)))))

;;;###autoload
(defun denote-tree--following-siblings (seq-id)
  "Return direct siblings of SEQ-ID at or after SEQ-ID, in descending order."
  (let* ((parent-prefix (denote-sequence--get-prefix-for-siblings seq-id))
         (candidates (if (or (null parent-prefix) (string-empty-p (or parent-prefix "")))
                         (denote-sequence-get-all-files)
                       (denote-sequence-get-all-files-with-prefix parent-prefix)))
         (siblings (seq-filter (lambda (f) (denote-tree--sequence-direct-sibling-p seq-id f))
                               candidates)))
    (thread-last siblings
      (seq-filter (lambda (f)
                    (not (string< (denote-retrieve-filename-signature f) seq-id))))
      (seq-sort (lambda (a b)
                  (string> (denote-retrieve-filename-signature a)
                           (denote-retrieve-filename-signature b)))))))

(defun denote-tree--shift-sequences (files)
  "Shift sequence signatures forward for FILES."
  (let ((denote-rename-confirmations nil))
    (dolist (f files)
      (let* ((old-sig (denote-retrieve-filename-signature f))
             (new-sig (denote-tree--increment-sequence old-sig)))
        (denote-rename-file f 'keep-current 'keep-current
                            new-sig 'keep-current 'keep-current)
        (denote-tree--hierarchy-remap-fold-sequence-prefix old-sig new-sig)))))

;;;###autoload
(defun denote-tree-insert-sequence-note ()
  "Insert a new note at the current note's sequence position.
All following siblings are shifted forward by one to make room.
Works from `denote-tree-mode' (note at point), a Denote note buffer,
or prompts for a file."
  (interactive)
  (let* ((file (denote-tree--target-file))
         (seq-id (or (denote-retrieve-filename-signature file)
                     (user-error "File has no sequence ID: %s"
                                 (file-name-nondirectory file))))
         (to-rename (denote-tree--following-siblings seq-id)))
    (unless (yes-or-no-p (format "Insert before %s, shifting %d sibling%s? "
                                 seq-id (length to-rename)
                                 (if (= (length to-rename) 1) "" "s")))
      (user-error "Cancelled"))
    (denote-tree--shift-sequences to-rename)
    (denote (read-string "Title: ")
            (denote-keywords-prompt)
            nil nil nil nil seq-id)
    ))

;;; Bulk retag across a sequence subtree

(defconst denote-tree--retag-operations
  '(("add"     add     "Add one or more keywords")
    ("remove"  remove  "Remove one or more keywords")
    ("replace" replace "Replace one keyword with another"))
  "Operations offered by `denote-tree-retag-sequence'.")

(defun denote-tree--retag-apply (keywords operation add-kws remove-kws)
  "Return the new keyword list for KEYWORDS under OPERATION, or `:unchanged'.
`:unchanged' (rather than nil) marks a no-op so callers can tell it apart
from a legitimate rename to an empty keyword list.  ADD-KWS and REMOVE-KWS
are lists of keyword strings; their meaning depends on OPERATION:

- `add': KEYWORDS plus ADD-KWS, deduplicated.  `:unchanged' when every
  keyword in ADD-KWS is already present.

- `remove': KEYWORDS minus REMOVE-KWS.  `:unchanged' when none of
  REMOVE-KWS is present, so files that never had the keyword are left
  untouched rather than being renamed for no reason.

- `replace': KEYWORDS with REMOVE-KWS (the single old keyword) swapped for
  ADD-KWS (the single new keyword).  `:unchanged' unless the old keyword is
  actually present — a file that never had it is left untouched entirely,
  it does not pick up the new keyword by side effect."
  (pcase operation
    ('add
     (let ((new (seq-uniq (append keywords add-kws))))
       (if (= (length new) (length (seq-uniq keywords))) :unchanged new)))
    ('remove
     (if (seq-intersection keywords remove-kws)
         (seq-difference keywords remove-kws)
       :unchanged))
    ('replace
     (if (seq-intersection keywords remove-kws)
         (seq-uniq (append (seq-difference keywords remove-kws) add-kws))
       :unchanged))))

(defun denote-tree--retag-prompt-keywords (operation existing-keywords seq-id)
  "Prompt for keyword parameters under OPERATION on SEQ-ID.
EXISTING-KEYWORDS is the list of keywords currently present in the subtree.
Returns a cons (ADD-KWS . REMOVE-KWS)."
  (pcase operation
    ('add
     (cons (completing-read-multiple "Add keyword(s): " nil) nil))
    ('remove
     (unless existing-keywords
       (user-error "No keywords found under sequence %s" seq-id))
     (cons nil (completing-read-multiple "Remove keyword(s): " existing-keywords nil t)))
    ('replace
     (unless existing-keywords
       (user-error "No keywords found under sequence %s" seq-id))
     (let* ((old-kw (annotated-completing-read
                     (seq-map (lambda (k) (cons k nil)) existing-keywords)
                     :prompt "Replace keyword:"
                     :require-match t))
            (new-kw (read-string (format "Replace `%s' with: " old-kw))))
       (cons (list new-kw) (list old-kw))))))

;;;###autoload
(defun denote-tree-retag-sequence ()
  "Add, remove, or replace a keyword across a sequence and all its descendants.
Resolves the target sequence from the note at point in `denote-tree-mode' or
`denote-sequence-hierarchy-mode', the current buffer file, or a prompt.

Only files actually affected by the chosen operation are renamed — see
`denote-tree--retag-apply' for the exact per-operation semantics; in
particular `replace' never adds the new keyword to a file that didn't
already carry the old one."
  (interactive)
  (let* ((file (denote-tree--target-file))
         (seq-id (or (denote-retrieve-filename-signature file)
                     (annotated-completing-read
                      (seq-map (lambda (s) (cons s nil))
                               (denote-sequence-get-all-sequences))
                      :prompt "Sequence:"
                      :require-match t)))
         (files (or (denote-tree--subtree-files seq-id)
                    (user-error "No files found for sequence %s" seq-id)))
         (existing-keywords (seq-uniq (seq-mapcat #'denote-extract-keywords-from-path files)))
         (op-choice (annotated-completing-read
                     (seq-map (lambda (e) (cons (nth 0 e) (nth 2 e)))
                              denote-tree--retag-operations)
                     :prompt "Operation:"
                     :require-match t))
         (operation (nth 1 (assoc op-choice denote-tree--retag-operations))))
    (pcase-let ((`(,add-kws . ,remove-kws)
                 (denote-tree--retag-prompt-keywords operation existing-keywords seq-id)))
      (let* ((planned (mapcar (lambda (f)
                                (cons f (denote-tree--retag-apply
                                         (denote-extract-keywords-from-path f)
                                         operation add-kws remove-kws)))
                              files))
             (affected (seq-remove (lambda (pair) (eq (cdr pair) :unchanged)) planned)))
        (unless affected
          (user-error "No files in sequence %s are affected by this operation" seq-id))
        (unless (yes-or-no-p (format "%s across sequence %s (%d of %d file%s)? "
                                     op-choice seq-id (length affected) (length files)
                                     (if (= (length files) 1) "" "s")))
          (user-error "Cancelled"))
        (let ((denote-rename-confirmations nil))
          (dolist (pair affected)
            (denote-rename-file
             (car pair) 'keep-current (cdr pair)
             'keep-current 'keep-current 'keep-current))))
      (when (derived-mode-p 'denote-sequence-hierarchy-mode) (revert-buffer)))))



(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables 'denote-tree-hierarchy-fold-sequences))

(with-eval-after-load 'denote-sequence
  (when (boundp 'denote-sequence-hierarchy-mode-map)
    (define-key denote-sequence-hierarchy-mode-map (kbd "r")   #'denote-tree-repack-children)
    (define-key denote-sequence-hierarchy-mode-map (kbd "C-r") #'denote-tree-repack-children)
    (define-key denote-sequence-hierarchy-mode-map (kbd "M-r") #'denote-tree-swap-with-parent)
    (define-key denote-sequence-hierarchy-mode-map (kbd "M-p") #'denote-tree-swap-with-previous)
    (define-key denote-sequence-hierarchy-mode-map (kbd "M-n") #'denote-tree-swap-with-next)
    (define-key denote-sequence-hierarchy-mode-map (kbd "m")   #'denote-tree-reparent)
    (define-key denote-sequence-hierarchy-mode-map (kbd "u")   #'denote-tree-renumber-recursive)
    (define-key denote-sequence-hierarchy-mode-map (kbd "k")   #'denote-tree-retag-sequence)
    (define-key denote-sequence-hierarchy-mode-map (kbd "z")   #'denote-tree-hierarchy-toggle-fold-sequence)
    (define-key denote-sequence-hierarchy-mode-map (kbd "h")   #'denote-tree-fix-sequence-frontmatter)
    (define-key denote-sequence-hierarchy-mode-map (kbd "C-l") #'denote-tree-fix-all-sequence-frontmatter)))

(provide 'denote-tree)
;;; denote-tree.el ends here
