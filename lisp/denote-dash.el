;;; denote-dash.el --- Unified Denote dashboard: list view and dispatch -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides a tabulated-list browser for Denote notes (denote-dash-mode)
;; and a transient dispatch menu (denote-dash-dispatch), consolidating
;; all Denote-related UI into a single package.
;;
;; Entry points:
;;   `denote-dash'          — open the *denote-dash* list buffer
;;   `denote-dash-dispatch' — open the transient command menu

;;; Code:

(require 'transient)
(require 'tabulated-list)
(require 'seq)
(require 'map)
(require 'denote)

;;; Declarations

(declare-function denote "denote")
(declare-function denote-open-or-create "denote")
(declare-function denote-link "denote")
(declare-function denote-backlinks "denote")
(declare-function denote-rename-file "denote")
(declare-function denote-dired "denote")
(declare-function denote-rename-file-using-front-matter "denote")
(declare-function denote-directory-files "denote")
(declare-function denote-retrieve-filename-title "denote")
(declare-function denote-retrieve-title-or-filename "denote")
(declare-function denote-retrieve-filename-identifier "denote")
(declare-function denote-retrieve-filename-signature "denote")
(declare-function denote-extract-keywords-from-path "denote")
(declare-function denote-filetype-heuristics "denote")
(declare-function denote-retrieve-front-matter-signature-value "denote")
(declare-function denote--rewrite-front-matter-line "denote")
(declare-function denote--get-component-key-regexp-function "denote")
(declare-function consult-denote-find "consult-denote")
(declare-function consult-denote-grep "consult-denote")
(declare-function consult-notes "consult-notes")
(declare-function consult-notes-search-in-all-notes "consult-notes")
(declare-function denote-journal-new-entry "denote-journal")
(declare-function denote-sequence-new-child "denote-sequence")
(declare-function denote-sequence-new-sibling "denote-sequence")
(declare-function denote-sequence-new-parent "denote-sequence")
(declare-function denote-sequence-link-to-parent "denote-sequence")
(declare-function denote-sequence-reparent "denote-sequence")
(declare-function denote-sequence-increment-partial "denote-sequence")
(declare-function denote-sequence--get-prefix-for-siblings "denote-sequence")
(declare-function denote-sequence-get-all-files-with-prefix "denote-sequence")
(declare-function denote-sequence-get-all-files "denote-sequence")
(declare-function denote-sequence-sort-files "denote-sequence")
(declare-function denote-sequence-split "denote-sequence")
(declare-function denote-sequence-join "denote-sequence")
(declare-function denote-sequence-and-scheme-p "denote-sequence")
(declare-function denote-sequence-file-p "denote-sequence")
(declare-function denote-keywords-prompt "denote")
(declare-function denote-markdown-convert-links-to-markdown-format "denote-markdown")
(declare-function denote-markdown-convert-links-to-denote-format "denote-markdown")
(declare-function denote-review-set-date "denote-review")
(declare-function denote-review-display-list "denote-review")
(declare-function denote-explore-random-note "denote-explore")
(declare-function denote-explore-missing-links "denote-explore")
(declare-function denote-explore-barchart-keywords "denote-explore")
(declare-function denote-explore-barchart-timeline "denote-explore")
(declare-function denote-explore-duplicate-notes "denote-explore")
(declare-function denote-org-link-to-heading "denote-org")
(declare-function denote-org-backlinks-for-heading "denote-org")
(declare-function denote-org-extract-org-subtree "denote-org")
(declare-function denote-org-dblock-insert-links "denote-org")
(declare-function denote-org-dblock-insert-backlinks "denote-org")
(declare-function denote-org-dblock-insert-files "denote-org")
(declare-function tychoish-org-extract-subtree-and-link "tychoish-org")
(declare-function org-back-to-heading "org")
(declare-function org-end-of-meta-data "org")
(declare-function org-end-of-subtree "org")
(declare-function org-get-heading "org")
(declare-function org-get-tags "org")
(declare-function org-map-entries "org")
(declare-function org-read-date "org")
(declare-function org-up-heading-safe "org")

;;; Custom variables

(defgroup denote-dash nil
  "Unified Denote dashboard and dispatch."
  :group 'denote)

(defcustom denote-dash-filter-shortcuts
  '(("projects"  . (and "project" (not "archive")))
    ("journal"   . "journal")
    ("reference" . "reference"))
  "Named filter shortcuts for the denote-dash buffer.
Each entry is (NAME . FILTER-EXPR) where FILTER-EXPR uses the denote-dash
filter language: strings, (or ...), (and ...), (not ...)."
  :type '(alist :key-type string :value-type sexp)
  :group 'denote-dash)

(defcustom denote-dash-initial-columns '(sequence title keywords id)
  "Columns shown when the *denote-dash* buffer is first created.
Valid symbols: fold, sequence, title, keywords, modified, id, directory, git."
  :type '(repeat (choice (const fold) (const sequence) (const title)
                         (const keywords) (const modified) (const id)
                         (const directory) (const git)))
  :group 'denote-dash)

(defcustom denote-dash-title-source 'filename
  "How to retrieve the title for the title column.
`filename' uses the title encoded in the filename (fast).
`front-matter' reads the title from the file's front matter (accurate but slower)."
  :type '(choice (const filename) (const front-matter))
  :group 'denote-dash)

(defcustom denote-dash-sequence-column-width 6
  "Display width of the sequence ID column."
  :type 'natnum
  :group 'denote-dash)

(defcustom denote-dash-git-column-enabled nil
  "When non-nil, allow enabling the git status column via column toggle."
  :type 'boolean
  :group 'denote-dash)

;;; Buffer-local state

(defvar-local denote-dash--current-filter nil
  "Active filter expression, or nil to show all notes.")

(defvar-local denote-dash--filter-history nil
  "Minibuffer history for denote-dash filter expressions.")

(defvar-local denote-dash--active-directory nil
  "Directory path restriction, or nil to show notes from all directories.")

(defvar-local denote-dash--visible-columns nil
  "Ordered list of column symbols currently displayed.")

(defvar-local denote-dash--fold-state nil
  "Hash table mapping sequence-id string to fold state symbol.
Absent entries default to `subtree'.  States: `folded', `children', `subtree'.")

(defvar-local denote-dash--global-cycle-depth nil
  "Integer maximum visible depth for global S-TAB cycle, or nil for per-node state.")

(defvar-local denote-dash--show-non-sequence t
  "When nil, notes without a sequence ID are hidden.")

(defvar-local denote-dash--column-widths nil
  "Alist of (COLUMN . WIDTH) overrides for this buffer.")

;;; Module-level cache

(defvar denote-dash--git-cache nil
  "Hash table of relative-path → git-status-char, invalidated on refresh.")

;;; Sequence utilities

(defun denote-dash--char-digit-p (c)
  "Return non-nil if character C is an ASCII decimal digit."
  (and (>= c ?0) (<= c ?9)))

(defun denote-dash--sequence-descendant-p (ancestor descendant)
  "Return non-nil if DESCENDANT is a descendant of ANCESTOR.
Both are sequence ID strings using the denote alphanumeric scheme, where
hierarchy levels alternate between digit and letter characters."
  (when (and ancestor descendant
             (not (string= ancestor descendant))
             (string-prefix-p ancestor descendant))
    (let ((next-char (aref descendant (length ancestor)))
          (last-char (aref ancestor (1- (length ancestor)))))
      (not (eq (denote-dash--char-digit-p last-char)
               (denote-dash--char-digit-p next-char))))))

(defun denote-dash--sequence-depth (seq-id)
  "Return the nesting depth of SEQ-ID (0 = root, 1 = first child level, etc.)."
  (if (or (null seq-id) (string-empty-p seq-id))
      0
    (let ((depth 0)
          (prev nil))
      (seq-do (lambda (c)
                (when (and prev
                           (not (eq (denote-dash--char-digit-p prev)
                                    (denote-dash--char-digit-p c))))
                  (setq depth (1+ depth)))
                (setq prev c))
              seq-id)
      depth)))

(defun denote-dash--direct-child-p (parent child)
  "Return non-nil if CHILD is an immediate child of PARENT in the sequence hierarchy."
  (and (denote-dash--sequence-descendant-p parent child)
       (= (denote-dash--sequence-depth child)
          (1+ (denote-dash--sequence-depth parent)))))

;;; Filter expression evaluator

(defun denote-dash--matches-p (keywords expr)
  "Return non-nil if KEYWORDS (list of strings) satisfies filter EXPR.
EXPR may be: nil (match all), a string (keyword member test),
or (or ...), (and ...), (not ...) compound forms."
  (pcase expr
    ('nil t)
    ((pred stringp) (member expr keywords))
    (`(or  . ,args) (seq-some   (lambda (e) (denote-dash--matches-p keywords e)) args))
    (`(and . ,args) (seq-every-p (lambda (e) (denote-dash--matches-p keywords e)) args))
    (`(not ,arg)    (not (denote-dash--matches-p keywords arg)))))

(defun denote-dash--valid-filter-p (expr)
  "Return non-nil if EXPR is a syntactically valid filter expression."
  (pcase expr
    ('nil t)
    ((pred stringp) t)
    (`(or  . ,args) (seq-every-p #'denote-dash--valid-filter-p args))
    (`(and . ,args) (seq-every-p #'denote-dash--valid-filter-p args))
    (`(not ,arg)    (denote-dash--valid-filter-p arg))
    (_ nil)))

;;; Directory utilities

(defun denote-dash--all-directories ()
  "Return list of all configured denote directories."
  (if (listp denote-directory) denote-directory (list denote-directory)))

(defun denote-dash--denote-root ()
  "Return the first denote directory as an expanded absolute path."
  (expand-file-name (car (denote-dash--all-directories))))

;;; Git status

(defun denote-dash--build-git-cache ()
  "Run git status on the denote root and return a hash table of path → status char."
  (let ((cache (make-hash-table :test #'equal))
        (root (denote-dash--denote-root)))
    (when (file-directory-p (expand-file-name ".git" root))
      (with-temp-buffer
        (when (zerop (call-process "git" nil t nil "-C" root "status" "--porcelain"))
          (goto-char (point-min))
          (while (re-search-forward "^\\(.\\).? \\(.*\\)$" nil t)
            (setf (map-elt cache (match-string 2)) (match-string 1))))))
    cache))

(defun denote-dash--git-status-char (file)
  "Return git status character for FILE, or space if clean/untracked."
  (when denote-dash--git-cache
    (or (map-elt denote-dash--git-cache
                 (file-relative-name file (denote-dash--denote-root)))
        " ")))

;;; Fold state

(defun denote-dash--fold-visible-p (seq-id all-seq-ids)
  "Return non-nil if a note with SEQ-ID is visible given current fold state.
ALL-SEQ-IDS is the precomputed list of all sequence IDs in the collection."
  (if (null seq-id)
      t
    (cond
     (denote-dash--global-cycle-depth
      (<= (denote-dash--sequence-depth seq-id) denote-dash--global-cycle-depth))
     (t
      (seq-every-p
       (lambda (anc)
         (let ((state (map-elt denote-dash--fold-state anc 'subtree)))
           (cond
            ((eq state 'folded)   nil)
            ((eq state 'children) (denote-dash--direct-child-p anc seq-id))
            (t                    t))))
       (seq-filter (lambda (c) (denote-dash--sequence-descendant-p c seq-id))
                   all-seq-ids))))))

(defun denote-dash--fold-indicator (seq-id)
  "Return a one-character fold state indicator for SEQ-ID."
  (if (null seq-id)
      " "
    (pcase (map-elt denote-dash--fold-state seq-id 'subtree)
      ('folded   "▶")
      ('children "▼")
      (_         " "))))

;;; ID formatting

(defun denote-dash--format-id (id)
  "Format Denote ID string ID (YYYYMMDDTHHmmSS) as YYYY-MM-DD HH:mm:ss."
  (if (and id (= (length id) 15))
      (format "%s-%s-%s %s:%s:%s"
              (substring id 0 4) (substring id 4 6) (substring id 6 8)
              (substring id 9 11) (substring id 11 13) (substring id 13 15))
    (or id "")))

;;; Data layer

(defun denote-dash--file-visible-p (file all-seq-ids)
  "Return non-nil if FILE should appear given current filter, directory, and fold state."
  (let ((seq-id (denote-retrieve-filename-signature file)))
    (and (denote-dash--matches-p (denote-extract-keywords-from-path file)
                                 denote-dash--current-filter)
         (or (null denote-dash--active-directory)
             (string-prefix-p (expand-file-name denote-dash--active-directory) file))
         (or denote-dash--show-non-sequence seq-id)
         (denote-dash--fold-visible-p seq-id all-seq-ids))))

(defun denote-dash--compute-entries ()
  "Return the full list of `tabulated-list-mode' entries for the current state."
  (let* ((files (denote-directory-files))
         (all-seq-ids (thread-last files
                                   (seq-map #'denote-retrieve-filename-signature)
                                   (seq-filter #'identity))))
    (thread-last files
                 (seq-filter (lambda (f) (denote-dash--file-visible-p f all-seq-ids)))
                 (seq-map (lambda (file)
                            (let ((seq-id (denote-retrieve-filename-signature file)))
                              (list file
                                    (apply #'vector
                                           (seq-map
                                            (lambda (col)
                                              (pcase col
                                                ('fold      (denote-dash--fold-indicator seq-id))
                                                ('sequence  (or seq-id ""))
                                                ('title     (if (eq denote-dash-title-source 'front-matter)
                                                               (denote-retrieve-title-or-filename file (denote-filetype-heuristics file))
                                                             (or (denote-retrieve-filename-title file) (file-name-base file))))
                                                ('keywords  (string-join (denote-extract-keywords-from-path file) " "))
                                                ('modified  (format-time-string "%Y-%m-%d" (file-attribute-modification-time (file-attributes file))))
                                                ('id        (denote-dash--format-id (denote-retrieve-filename-identifier file)))
                                                ('directory (file-relative-name (file-name-directory file) (denote-dash--denote-root)))
                                                ('git       (or (denote-dash--git-status-char file) " "))))
                                            denote-dash--visible-columns)))))))))

;;; Column format

(defun denote-dash--column-width (col)
  "Return display width for COL, consulting buffer-local overrides first."
  (or (alist-get col denote-dash--column-widths)
      (pcase col
        ('fold      1)
        ('sequence  denote-dash-sequence-column-width)
        ('title     44)
        ('keywords  25)
        ('modified  10)
        ('id        19)
        ('directory 20)
        ('git       1)
        (_          10))))

(defun denote-dash--setup-columns ()
  "Configure `tabulated-list-format' from current visible columns and reinit header."
  (setq tabulated-list-format
        (apply #'vector
               (seq-map (lambda (col)
                          (pcase col
                            ('fold      (list "" (denote-dash--column-width 'fold) nil))
                            ('sequence  (list "Seq" (denote-dash--column-width 'sequence) t))
                            ('title     (list "Title" (denote-dash--column-width 'title) t))
                            ('keywords  (list "Keywords" (denote-dash--column-width 'keywords) t))
                            ('modified  (list "Modified" (denote-dash--column-width 'modified) t))
                            ('id        (list "ID" (denote-dash--column-width 'id) t))
                            ('directory (list "Dir" (denote-dash--column-width 'directory) t))
                            ('git       (list "G" (denote-dash--column-width 'git) nil))))
                        denote-dash--visible-columns)))
  (tabulated-list-init-header))

;;; Refresh

(defun denote-dash-refresh ()
  "Rebuild and redisplay the *denote-dash* buffer."
  (interactive)
  (when (member 'git denote-dash--visible-columns)
    (setq denote-dash--git-cache (denote-dash--build-git-cache)))
  (denote-dash--setup-columns)
  (setq tabulated-list-entries (denote-dash--compute-entries))
  (tabulated-list-print t))

;;; Mode definition

(defvar-keymap denote-dash-mode-map
  :doc "Keymap for `denote-dash-mode'."
  "RET"     #'denote-dash-open-note
  "o"       #'denote-dash-open-note-other-window
  "f"       #'denote-dash-filter
  "C-f"     #'denote-dash-clear-filter
  "e"       #'denote-dash-filter-expression
  "s"       #'denote-dash-filter-shortcut
  "d"       #'denote-dash-cycle-directory
  "c"       #'denote-dash-column-transient
  "<tab>"   #'denote-dash-cycle-fold
  "S-<tab>" #'denote-dash-global-cycle
  "M-<tab>" #'denote-dash-expand-all
  "z"       #'denote-dash-collapse-all
  "t"       #'denote-dash-toggle-non-sequence
  "r"       #'denote-rename-file-using-front-matter
  "l"       #'denote-dash-lint-sequences
  "C-r"     #'denote-dash-repack-sequence-children
  "M-r"     #'denote-dash-swap-with-parent
  "M-p"     #'denote-dash-swap-with-previous
  "M-n"     #'denote-dash-swap-with-next
  "n"       #'denote
  "g"       #'denote-dash-refresh
  "?"       #'denote-dash-dispatch
  "q"       #'quit-window)

(define-derived-mode denote-dash-mode tabulated-list-mode "ddash"
  "Major mode for browsing and filtering Denote notes.

\\{denote-dash-mode-map}"
  (setq-local denote-dash--fold-state (make-hash-table :test #'equal))
  (let ((cols (or denote-dash--persisted-columns denote-dash-initial-columns)))
    (setq-local denote-dash--visible-columns
                (seq-filter (lambda (c) (member c cols)) denote-dash-column-order)))
  (setq-local tabulated-list-sort-key nil))

;;; Entry point

;;;###autoload
(defun denote-dash ()
  "Open or switch to the *denote-dash* buffer."
  (interactive)
  (let ((buf (get-buffer-create "*denote-dash*")))
    (pop-to-buffer buf)
    (unless (derived-mode-p 'denote-dash-mode)
      (denote-dash-mode))
    (denote-dash-refresh)))

;;; Navigation

(defun denote-dash-open-note ()
  "Open the Denote note at point."
  (interactive)
  (when-let* ((file (tabulated-list-get-id)))
    (find-file file)))

(defun denote-dash-open-note-other-window ()
  "Open the Denote note at point in another window."
  (interactive)
  (when-let* ((file (tabulated-list-get-id)))
    (find-file-other-window file)))

;;; Filter commands

(defun denote-dash--all-keywords ()
  "Return a sorted, deduplicated list of all keywords across all Denote notes."
  (thread-last (denote-directory-files)
               (seq-map #'denote-extract-keywords-from-path)
               (apply #'append)
               (seq-uniq)
               (seq-sort #'string<)))

(defun denote-dash-filter (arg)
  "Filter notes by keyword selection using completing-read-multiple.
Without prefix ARG, build an AND expression; with prefix ARG, build OR."
  (interactive "P")
  (let* ((selected (completing-read-multiple "Filter keywords: " (denote-dash--all-keywords)))
         (expr (cond
                ((null selected) nil)
                ((= (length selected) 1) (car selected))
                (arg `(or ,@selected))
                (t `(and ,@selected)))))
    (setq denote-dash--current-filter expr)
    (denote-dash-refresh)))

(defun denote-dash-clear-filter ()
  "Remove the current filter and show all notes."
  (interactive)
  (setq denote-dash--current-filter nil)
  (denote-dash-refresh))

(defun denote-dash-filter-expression ()
  "Set the filter from a raw Lisp expression entered in the minibuffer."
  (interactive)
  (let* ((initial (when denote-dash--current-filter
                    (format "%S" denote-dash--current-filter)))
         (input (read-string "Filter expression: " initial 'denote-dash--filter-history))
         (expr (condition-case nil
                   (car (read-from-string input))
                 (error (user-error "Unreadable filter expression: %s" input)))))
    (unless (denote-dash--valid-filter-p expr)
      (user-error "Invalid filter expression: %s" input))
    (setq denote-dash--current-filter expr)
    (denote-dash-refresh)))

(defun denote-dash-filter-shortcut ()
  "Apply a named filter shortcut from `denote-dash-filter-shortcuts'."
  (interactive)
  (when (null denote-dash-filter-shortcuts)
    (user-error "No shortcuts configured; customize `denote-dash-filter-shortcuts'"))
  (let* ((chosen (completing-read "Shortcut: "
                                  (seq-map #'car denote-dash-filter-shortcuts) nil t))
         (expr (cdr (assoc chosen denote-dash-filter-shortcuts))))
    (setq denote-dash--current-filter expr)
    (denote-dash-refresh)))

;;; Directory restriction

(defun denote-dash-cycle-directory ()
  "Cycle directory restriction: all → each configured silo → all."
  (interactive)
  (let* ((dirs (denote-dash--all-directories))
         (next (if (null denote-dash--active-directory)
                   (car dirs)
                 (car (cdr (member denote-dash--active-directory dirs))))))
    (setq denote-dash--active-directory next)
    (message "Directory filter: %s" (or next "all"))
    (denote-dash-refresh)))

;;; Sequence fold commands

(defun denote-dash-cycle-fold ()
  "Cycle fold state on the sequence note at point: subtree → folded → children → subtree."
  (interactive)
  (when-let* ((file (tabulated-list-get-id))
              (seq-id (denote-retrieve-filename-signature file)))
    (setf (map-elt denote-dash--fold-state seq-id)
          (pcase (map-elt denote-dash--fold-state seq-id 'subtree)
            ('subtree  'folded)
            ('folded   'children)
            ('children 'subtree)))
    (setq denote-dash--global-cycle-depth nil)
    (denote-dash-refresh)))

(defun denote-dash-collapse-all ()
  "Collapse all sequence notes to folded state."
  (interactive)
  (setq denote-dash--global-cycle-depth nil)
  (seq-do (lambda (seq-id)
            (setf (map-elt denote-dash--fold-state seq-id) 'folded))
          (thread-last (denote-directory-files)
                       (seq-map #'denote-retrieve-filename-signature)
                       (seq-filter #'identity)))
  (denote-dash-refresh))

(defun denote-dash-expand-all ()
  "Expand all sequence notes to show full subtrees."
  (interactive)
  (setq denote-dash--global-cycle-depth nil)
  (clrhash denote-dash--fold-state)
  (denote-dash-refresh))

(defun denote-dash-global-cycle ()
  "Step through global fold depths: 0 (roots only) → 1 → … → nil (all expanded)."
  (interactive)
  (let* ((max-depth (seq-reduce
                     #'max
                     (thread-last (denote-directory-files)
                                  (seq-map #'denote-retrieve-filename-signature)
                                  (seq-filter #'identity)
                                  (seq-map #'denote-dash--sequence-depth))
                     0))
         (next (cond
                ((null denote-dash--global-cycle-depth) 0)
                ((>= denote-dash--global-cycle-depth max-depth) nil)
                (t (1+ denote-dash--global-cycle-depth)))))
    (setq denote-dash--global-cycle-depth next)
    (message "Global fold depth: %s" (if next (number-to-string next) "all"))
    (denote-dash-refresh)))

(defun denote-dash-toggle-non-sequence ()
  "Toggle visibility of notes without a sequence identifier."
  (interactive)
  (setq denote-dash--show-non-sequence (not denote-dash--show-non-sequence))
  (message "Non-sequence notes: %s"
           (if denote-dash--show-non-sequence "shown" "hidden"))
  (denote-dash-refresh))

;;; Column ordering and toggle

(defvar denote-dash-column-order
  '(sequence fold title keywords id directory modified git)
  "Canonical display order for columns; determines left-to-right position.")

(defvar denote-dash--persisted-columns nil
  "Column list saved across sessions for `*denote-dash*'.")

(defun denote-dash--toggle-column (col)
  "Toggle visibility of column COL and refresh the buffer."
  (let ((cols (if (member col denote-dash--visible-columns)
                  (seq-remove (lambda (c) (eq c col)) denote-dash--visible-columns)
                (cons col denote-dash--visible-columns))))
    (setq denote-dash--visible-columns
          (seq-filter (lambda (c) (member c cols)) denote-dash-column-order))
    (setq denote-dash--persisted-columns denote-dash--visible-columns))
  (denote-dash-refresh))

(defun denote-dash-toggle-sequence-column ()
  "Toggle the sequence ID column."
  (interactive) (denote-dash--toggle-column 'sequence))

(defun denote-dash-toggle-fold-column ()
  "Toggle the fold indicator column."
  (interactive) (denote-dash--toggle-column 'fold))

(defun denote-dash-toggle-title-column ()
  "Toggle the title column."
  (interactive) (denote-dash--toggle-column 'title))

(defun denote-dash-toggle-keywords-column ()
  "Toggle the keywords column."
  (interactive) (denote-dash--toggle-column 'keywords))

(defun denote-dash-toggle-modified-column ()
  "Toggle the last-modified date column."
  (interactive) (denote-dash--toggle-column 'modified))

(defun denote-dash-toggle-id-column ()
  "Toggle the Denote ID column."
  (interactive) (denote-dash--toggle-column 'id))

(defun denote-dash-toggle-directory-column ()
  "Toggle the directory column."
  (interactive) (denote-dash--toggle-column 'directory))

(defun denote-dash-toggle-git-column ()
  "Toggle the git status column."
  (interactive)
  (unless denote-dash-git-column-enabled
    (user-error "Set `denote-dash-git-column-enabled' to t to enable the git column"))
  (denote-dash--toggle-column 'git))

(defun denote-dash-set-column-width ()
  "Interactively set the display width for a visible column."
  (interactive)
  (let* ((col (intern (completing-read "Column: "
                                       (seq-map #'symbol-name denote-dash--visible-columns)
                                       nil t)))
         (current (denote-dash--column-width col))
         (width (read-number (format "Width for %s (current %d): " col current) current)))
    (setf (alist-get col denote-dash--column-widths) width)
    (denote-dash-refresh)))

(defun denote-dash--front-matter-description ()
  "Transient label showing current title source."
  (if (eq denote-dash-title-source 'front-matter)
      "titles: front-matter [on]"
    "titles: front-matter [off]"))

(transient-define-suffix denote-dash-toggle-front-matter-titles ()
  "Toggle title rendering between filename and front-matter sources."
  :description #'denote-dash--front-matter-description
  (interactive)
  (setq-local denote-dash-title-source
              (if (eq denote-dash-title-source 'front-matter) 'filename 'front-matter))
  (message "Title source: %s" denote-dash-title-source)
  (denote-dash-refresh))

(transient-define-prefix denote-dash-column-transient ()
  "Toggle columns and display options in the *denote-dash* buffer."
  [["Columns"
    ("s" "sequence"  denote-dash-toggle-sequence-column)
    ("f" "fold"      denote-dash-toggle-fold-column)
    ("t" "title"     denote-dash-toggle-title-column)
    ("k" "keywords"  denote-dash-toggle-keywords-column)
    ("m" "modified"  denote-dash-toggle-modified-column)
    ("i" "id"        denote-dash-toggle-id-column)
    ("d" "directory" denote-dash-toggle-directory-column)
    ("g" "git"       denote-dash-toggle-git-column)]
   ["Display"
    ("r" denote-dash-toggle-front-matter-titles)
    ("w" "set column width" denote-dash-set-column-width)]])

;;; Savehist

(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables 'denote-dash--filter-history)
  (add-to-list 'savehist-additional-variables 'denote-dash--persisted-columns))

;;; Org datetree import

(defconst denote-dash--datetree-day-re
  "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}"
  "Regexp matching a YYYY-MM-DD date prefix in an org datetree day heading.")

(defun denote-dash--datetree-day-date (heading)
  "Return the YYYY-MM-DD prefix of HEADING if it is a datetree day heading, else nil."
  (when (string-match denote-dash--datetree-day-re heading)
    (match-string 0 heading)))

(defun denote-dash--datetree-parent-date ()
  "Return the date string if the immediate parent is a datetree day heading, else nil."
  (save-excursion
    (when (org-up-heading-safe)
      (denote-dash--datetree-day-date (org-get-heading t t t t)))))

(defun denote-dash--entry-body ()
  "Return the body text of the current org heading as a trimmed string."
  (save-excursion
    (org-back-to-heading t)
    (forward-line 1)
    (org-end-of-meta-data t)
    (string-trim
     (buffer-substring-no-properties
      (point)
      (save-excursion (org-end-of-subtree t) (point))))))

(defun denote-dash--collect-datetree-entries ()
  "Return a list of plists for all entries in the current buffer's org datetree.
Each plist has :title, :date, :tags, :body.  Only direct children of
day-level headings (YYYY-MM-DD ...) are collected."
  (let (entries)
    (org-map-entries
     (lambda ()
       (when-let* ((date (denote-dash--datetree-parent-date))
                   (title (string-trim (org-get-heading t t t t)))
                   (_ (not (string-empty-p title))))
         (push (list :title title
                     :date  date
                     :tags  (org-get-tags nil t)
                     :body  (denote-dash--entry-body))
               entries))))
    (nreverse entries)))

(defun denote-dash--import-entry (entry)
  "Create a Denote note from datetree ENTRY plist (:title :date :tags :body)."
  (let* ((title (plist-get entry :title))
         (tags  (plist-get entry :tags))
         (body  (plist-get entry :body))
         (time  (org-read-date nil t (plist-get entry :date))))
    (save-window-excursion
      (denote title tags 'org nil time)
      (when (and body (not (string-empty-p body)))
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert "\n" body)
        (save-buffer)))))

(defun denote-dash-import-from-datetree (file &optional from-date to-date)
  "Import org datetree entries from FILE as individual Denote notes.
With optional FROM-DATE and TO-DATE (YYYY-MM-DD strings), restricts
import to that inclusive date range.  Interactively, prompts for the
file and whether to apply a date restriction."
  (interactive
   (let* ((f (read-file-name "Import from datetree: " nil nil t nil
                             (lambda (n)
                               (or (file-directory-p n)
                                   (string-suffix-p ".org" n)))))
          (restrict (yes-or-no-p "Restrict to a date range? "))
          (from (when restrict (org-read-date nil nil nil "From (inclusive): ")))
          (to   (when restrict (org-read-date nil nil nil "To (inclusive): "))))
     (list f from to)))
  (let* ((filtered (seq-filter
                    (lambda (e)
                      (let ((d (plist-get e :date)))
                        (and (or (null from-date) (not (string< d from-date)))
                             (or (null to-date)   (not (string< to-date d))))))
                    (with-current-buffer (find-file-noselect file)
                      (denote-dash--collect-datetree-entries))))
         (n (length filtered)))
    (when (zerop n)
      (user-error "No datetree entries found%s"
                  (if (or from-date to-date)
                      (format " between %s and %s" from-date to-date)
                    "")))
    (unless (yes-or-no-p (format "Create %d denote note%s from %s? "
                                 n (if (= n 1) "" "s")
                                 (file-name-nondirectory file)))
      (user-error "Import cancelled"))
    (let ((ok 0) (fail 0))
      (seq-do (lambda (entry)
                (condition-case err
                    (progn (denote-dash--import-entry entry) (setq ok (1+ ok)))
                  (error
                   (setq fail (1+ fail))
                   (message "Skipped %S: %s"
                            (plist-get entry :title)
                            (error-message-string err)))))
              filtered)
      (message "Imported %d/%d note%s%s."
               ok n (if (= ok 1) "" "s")
               (if (> fail 0) (format " (%d failed)" fail) "")))))

;;; Sequence alignment lint / autofix

(defvar denote-file-types)

(defun denote-dash--signature-line (sig file-type)
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

(defun denote-dash--sequence-aligned-p (file)
  "Return non-nil if FILE's filename and frontmatter signatures agree."
  (let* ((file-type (denote-filetype-heuristics file))
         (filename-sig (denote-retrieve-filename-signature file))
         (fm-sig (denote-retrieve-front-matter-signature-value file file-type)))
    (equal filename-sig fm-sig)))

(defun denote-dash--fix-frontmatter-from-filename (file)
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
             (denote-dash--signature-line filename-sig file-type)
             file-type))
           ;; Filename has sig, frontmatter doesn't: insert after identifier line
           ((and filename-sig (null fm-sig))
            (when-let* ((key-fn (denote--get-component-key-regexp-function 'identifier))
                        (id-re (funcall key-fn file-type))
                        (_ (re-search-forward id-re nil t)))
              (end-of-line)
              (insert "\n" (denote-dash--signature-line filename-sig file-type))))
           ;; Frontmatter has sig, filename doesn't: delete the frontmatter line
           ((and (null filename-sig) fm-sig)
            (when-let* ((key-fn (denote--get-component-key-regexp-function 'signature))
                        (sig-re (funcall key-fn file-type))
                        (_ (re-search-forward sig-re nil t)))
              (delete-region (line-beginning-position)
                             (min (1+ (line-end-position)) (point-max)))))))
        (save-buffer))
      t)))

(defun denote-dash--collect-sequence-mismatches ()
  "Return list of all Denote files whose filename and frontmatter signatures disagree."
  (seq-filter (lambda (f) (not (denote-dash--sequence-aligned-p f)))
              (denote-directory-files)))

(defun denote-dash-lint-sequences ()
  "Show all Denote notes whose filename and frontmatter signatures are misaligned."
  (interactive)
  (let* ((mismatches (denote-dash--collect-sequence-mismatches))
         (buf (get-buffer-create "*Denote Sequence Lint*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if (null mismatches)
            (insert "All sequence notes are aligned.\n")
          (insert (format "%d mismatch%s:\n\n"
                          (length mismatches)
                          (if (= (length mismatches) 1) "" "es")))
          (seq-do (lambda (file)
                    (let* ((file-type (denote-filetype-heuristics file))
                           (fs (denote-retrieve-filename-signature file))
                           (fms (denote-retrieve-front-matter-signature-value
                                 file file-type)))
                      (insert (format "  %-40s  filename=%-10s  frontmatter=%s\n"
                                      (file-name-nondirectory file)
                                      (or fs "—")
                                      (or fms "—")))))
                  mismatches)
          (insert "\nRun `denote-dash-fix-all-sequence-frontmatter' to fix "
                  "(filename authoritative).\n"
                  "Run `denote-rename-file-using-front-matter' per-file to go "
                  "the other direction.\n")))
      (special-mode))
    (pop-to-buffer buf)))

(defun denote-dash-fix-sequence-frontmatter ()
  "Fix frontmatter signature to match filename for the note at point or current file.
In `denote-dash-mode', operates on the note at point; otherwise on `buffer-file-name'."
  (interactive)
  (when-let* ((file (if (derived-mode-p 'denote-dash-mode)
                        (tabulated-list-get-id)
                      (buffer-file-name))))
    (if (denote-dash--fix-frontmatter-from-filename file)
        (progn
          (message "Fixed: %s" (file-name-nondirectory file))
          (when (derived-mode-p 'denote-dash-mode) (denote-dash-refresh)))
      (message "Already aligned: %s" (file-name-nondirectory file)))))

(defun denote-dash-fix-all-sequence-frontmatter ()
  "Fix frontmatter signatures for all mismatched Denote notes, using filename as truth."
  (interactive)
  (let* ((mismatches (denote-dash--collect-sequence-mismatches))
         (n (length mismatches)))
    (when (zerop n)
      (user-error "No sequence frontmatter mismatches found"))
    (unless (yes-or-no-p (format "Fix frontmatter in %d note%s? "
                                 n (if (= n 1) "" "s")))
      (user-error "Cancelled"))
    (let ((fixed 0) (errors 0))
      (seq-do (lambda (file)
                (condition-case err
                    (when (denote-dash--fix-frontmatter-from-filename file)
                      (setq fixed (1+ fixed)))
                  (error
                   (setq errors (1+ errors))
                   (message "Error in %s: %s"
                            (file-name-nondirectory file)
                            (error-message-string err)))))
              mismatches)
      (message "Fixed %d/%d note%s%s."
               fixed n (if (= fixed 1) "" "s")
               (if (> errors 0) (format " (%d errors)" errors) "")))
    (when (derived-mode-p 'denote-dash-mode) (denote-dash-refresh))))

;;; Sequence repack

(defun denote-dash--sequence-direct-children (prefix)
  "Return all Denote files that are direct children of PREFIX.
If PREFIX is nil or empty, returns all root-level sequence files (depth 0).
A direct child has exactly one more alternating segment than PREFIX."
  (let* ((pfx-depth (if (and prefix (not (string-empty-p (or prefix ""))))
                        (denote-dash--sequence-depth prefix)
                      -1))
         (target-depth (1+ pfx-depth))
         (files (if (and prefix (not (string-empty-p (or prefix ""))))
                    (denote-sequence-get-all-files-with-prefix prefix)
                  (denote-sequence-get-all-files))))
    (seq-filter
     (lambda (f)
       (when-let* ((sig (denote-retrieve-filename-signature f)))
         (= (denote-dash--sequence-depth sig) target-depth)))
     files)))

(defun denote-dash--compact-child-seq (n prefix)
  "Return the Nth (1-based) compact child sequence for PREFIX.
Children alternate type: if PREFIX ends in a digit (or is nil/empty),
children use letters (a, b, c…); if it ends in a letter, children use
numbers (1, 2, 3…)."
  (let* ((last-char (and prefix
                         (not (string-empty-p (or prefix "")))
                         (aref prefix (1- (length prefix)))))
         (use-letters (or (null last-char)
                          (denote-dash--char-digit-p last-char)))
         (suffix (if use-letters
                     (char-to-string (+ ?a (1- n)))
                   (number-to-string n))))
    (concat (or prefix "") suffix)))

(defun denote-dash--fix-all-frontmatter-silent ()
  "Fix all sequence frontmatter mismatches without confirmation.  Returns count."
  (let ((fixed 0))
    (seq-do (lambda (file)
              (condition-case nil
                  (when (denote-dash--fix-frontmatter-from-filename file)
                    (setq fixed (1+ fixed)))
                (error nil)))
            (denote-dash--collect-sequence-mismatches))
    fixed))

(defun denote-dash-repack-sequence-children (prefix)
  "Compact direct children of PREFIX so their last segment has no gaps.
PREFIX is a sequence string (e.g. \"3a1\"); empty string means root level.
Renames files from highest sequence first to avoid collisions, then
syncs all frontmatter.  Works from `denote-dash-mode' or interactively."
  (interactive
   (list (read-string "Sequence prefix (empty = root): "
                      (when (derived-mode-p 'denote-dash-mode)
                        (when-let* ((f (tabulated-list-get-id)))
                          (denote-retrieve-filename-signature f))))))
  (let* ((prefix (if (string-empty-p prefix) nil prefix))
         (children (denote-dash--sequence-direct-children prefix))
         (sorted (seq-sort (lambda (a b)
                             (string< (denote-retrieve-filename-signature a)
                                      (denote-retrieve-filename-signature b)))
                           children)))
    (when (null sorted)
      (user-error "No children found for %s" (or prefix "(root)")))
    (let* ((expected (seq-map-indexed
                      (lambda (_f i)
                        (denote-dash--compact-child-seq (1+ i) prefix))
                      sorted))
           (to-rename (seq-filter
                       (lambda (pair)
                         (not (string= (denote-retrieve-filename-signature (car pair))
                                       (cdr pair))))
                       (seq-mapn #'cons sorted expected))))
      (if (null to-rename)
          (message "Sequences for %s already compact." (or prefix "(root)"))
        (unless (yes-or-no-p
                 (format "Repack %d/%d children of %s? "
                         (length to-rename) (length sorted) (or prefix "(root)")))
          (user-error "Cancelled"))
        ;; Rename highest sequence first to avoid intermediate collisions
        (seq-do (lambda (pair)
                  (denote-rename-file (car pair) 'keep-current 'keep-current
                                      (cdr pair) 'keep-current 'keep-current))
                (seq-sort (lambda (a b)
                            (string> (denote-retrieve-filename-signature (car a))
                                     (denote-retrieve-filename-signature (car b))))
                          to-rename)))
      ;; Always sync frontmatter after — covers both renames and any prior drift
      (let ((n (denote-dash--fix-all-frontmatter-silent)))
        (message "Repacked %d/%d children of %s; fixed %d frontmatter %s."
                 (length to-rename) (length sorted) (or prefix "(root)")
                 n (if (= n 1) "note" "notes")))
      (when (derived-mode-p 'denote-dash-mode)
        (denote-dash-refresh)))))

;;; Sequence swap

(defun denote-dash--sequence-parent (seq)
  "Return the parent sequence of SEQ, or nil if SEQ is a root.
The parent is the longest proper prefix whose depth is one less than SEQ's.
Computed by finding the last type-transition (digit↔letter) in SEQ."
  (when (and seq (not (string-empty-p seq))
             (> (denote-dash--sequence-depth seq) 0))
    (let ((last-transition nil))
      (dotimes (i (1- (length seq)))
        (when (not (eq (denote-dash--char-digit-p (aref seq i))
                       (denote-dash--char-digit-p (aref seq (1+ i)))))
          (setq last-transition (1+ i))))
      (when last-transition
        (substring seq 0 last-transition)))))

(defun denote-dash--rename-signature-component (file old-sig new-sig)
  "Return FILE's path with its ==SIG== component changed from OLD-SIG to NEW-SIG."
  (let ((dir (file-name-directory file))
        (base (file-name-nondirectory file)))
    (concat dir (replace-regexp-in-string
                 (concat "==" (regexp-quote old-sig) "--")
                 (concat "==" new-sig "--")
                 base t t))))

(defun denote-dash-swap-with-parent ()
  "Swap the sequence of the note at point with its direct parent.
Both nodes must have files in the denote directory.  Uses a three-step
rename (child→tmp, parent→child-seq, tmp→parent-seq) to avoid collision,
then fixes frontmatter signatures on both files."
  (interactive)
  (let* ((file (denote-dash--target-file))
         (seq (denote-retrieve-filename-signature file)))
    (unless seq
      (user-error "File has no sequence: %s" (file-name-nondirectory file)))
    (let ((parent-seq (denote-dash--sequence-parent seq)))
      (unless parent-seq
        (user-error "%s is a root sequence — nothing to swap with" seq))
      (let ((parent-file
             (seq-find (lambda (f)
                         (equal (denote-retrieve-filename-signature f) parent-seq))
                       (denote-directory-files))))
        (unless parent-file
          (user-error "No file found for parent sequence %s" parent-seq))
        (unless (yes-or-no-p (format "Swap %s ↔ %s? " seq parent-seq))
          (user-error "Cancelled"))
        (let* ((new-path (denote-dash--rename-signature-component file seq parent-seq))
               (new-par-path (denote-dash--rename-signature-component parent-file parent-seq seq))
               (tmp-path (denote-dash--rename-signature-component file seq "__swaptmp__")))
          ;; Kill any buffers visiting files we are about to rename so they
          ;; do not become stale (pointing to a path that no longer exists).
          (seq-do (lambda (f)
                    (when-let* ((buf (find-buffer-visiting f)))
                      (kill-buffer buf)))
                  (list file parent-file))
          (rename-file file tmp-path t)
          (rename-file parent-file new-par-path t)
          (rename-file tmp-path new-path t)
          (denote-dash--fix-frontmatter-from-filename new-path)
          (denote-dash--fix-frontmatter-from-filename new-par-path)
          (message "Swapped %s ↔ %s" seq parent-seq)
          (when (derived-mode-p 'denote-dash-mode)
            (denote-dash-refresh)))))))

;;; Sequence sibling swap

(defun denote-dash--subtree-files (seq)
  "Return the file for SEQ and all its descendant files, sequence-sorted."
  (denote-sequence-sort-files (denote-sequence-get-all-files-with-prefix seq)))

(defun denote-dash--sequence-siblings (seq)
  "Return the sorted list of SEQ's direct siblings, SEQ included.
Siblings share SEQ's parent, or are all root sequences when SEQ is one."
  (denote-sequence-sort-files
   (denote-dash--sequence-direct-children (denote-dash--sequence-parent seq))))

(defun denote-dash--swap-subtrees (seq-a seq-b)
  "Swap the sequence subtrees rooted at SEQ-A and SEQ-B.
Recursively renames SEQ-A, SEQ-B, and all their descendants so the two
subtrees exchange positions, then fixes the frontmatter signature of
every renamed file.  SEQ-A and SEQ-B must be direct siblings."
  (let* ((files-a (denote-dash--subtree-files seq-a))
         (files-b (denote-dash--subtree-files seq-b)))
    (unless files-a (user-error "No files found for sequence %s" seq-a))
    (unless files-b (user-error "No files found for sequence %s" seq-b))
    (seq-do (lambda (f)
              (when-let* ((buf (find-buffer-visiting f)))
                (kill-buffer buf)))
            (append files-a files-b))
    ;; Stage 1: move SEQ-A's subtree aside under unique temp signatures,
    ;; remembering each file's original signature to recover it later.
    (let ((staged
           (seq-map-indexed
            (lambda (f i)
              (let* ((sig (denote-retrieve-filename-signature f))
                     (tmp-sig (format "swaptmp%d" i))
                     (tmp-path (denote-dash--rename-signature-component f sig tmp-sig)))
                (rename-file f tmp-path t)
                (list sig tmp-sig tmp-path)))
            files-a)))
      ;; Stage 2: move SEQ-B's subtree into SEQ-A's namespace.
      (seq-do (lambda (f)
                (let* ((old-sig (denote-retrieve-filename-signature f))
                       (new-sig (concat seq-a (substring old-sig (length seq-b))))
                       (new-path (denote-dash--rename-signature-component f old-sig new-sig)))
                  (rename-file f new-path t)
                  (denote-dash--fix-frontmatter-from-filename new-path)))
              files-b)
      ;; Stage 3: move the staged SEQ-A subtree into SEQ-B's namespace.
      (seq-do (lambda (entry)
                (pcase-let ((`(,orig-sig ,tmp-sig ,tmp-path) entry))
                  (let* ((new-sig (concat seq-b (substring orig-sig (length seq-a))))
                         (new-path (denote-dash--rename-signature-component tmp-path tmp-sig new-sig)))
                    (rename-file tmp-path new-path t)
                    (denote-dash--fix-frontmatter-from-filename new-path))))
              staged))))

(defun denote-dash--swap-with-sibling (direction)
  "Swap the note at point (with its subtree) with a sibling.
DIRECTION is the symbol `previous' or `next', selecting which of the
current sequence's siblings to swap with."
  (let* ((file (denote-dash--target-file))
         (seq (denote-retrieve-filename-signature file)))
    (unless seq
      (user-error "File has no sequence: %s" (file-name-nondirectory file)))
    (let* ((siblings (denote-dash--sequence-siblings seq))
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
        (denote-dash--swap-subtrees seq other-seq)
        (message "Swapped %s ↔ %s" seq other-seq)
        (when (derived-mode-p 'denote-dash-mode)
          (denote-dash-refresh))))))

;;;###autoload
(defun denote-dash-swap-with-previous ()
  "Swap the note at point, with its subtree, with its previous sibling.
Descendants of both nodes move along with their parent, so the whole
subtrees exchange positions.  See also `denote-dash-swap-with-parent'."
  (interactive)
  (denote-dash--swap-with-sibling 'previous))

;;;###autoload
(defun denote-dash-swap-with-next ()
  "Swap the note at point, with its subtree, with its next sibling.
Descendants of both nodes move along with their parent, so the whole
subtrees exchange positions.  See also `denote-dash-swap-with-parent'."
  (interactive)
  (denote-dash--swap-with-sibling 'next))

;;; Sequence insertion

(defun denote-dash--increment-sequence (seq)
  "Return SEQ with its last component incremented."
  (let* ((parts (denote-sequence-split seq))
         (new-last (denote-sequence-increment-partial (car (last parts))))
         (scheme (cdr (denote-sequence-and-scheme-p seq))))
    (denote-sequence-join (append (butlast parts) (list new-last)) scheme)))

(defun denote-dash--sequence-direct-sibling-p (seq-id file)
  "Return non-nil if FILE is a direct sibling of SEQ-ID (same depth)."
  (when-let* ((fsig (denote-retrieve-filename-signature file)))
    (= (length (denote-sequence-split fsig))
       (length (denote-sequence-split seq-id)))))

(defun denote-dash--target-file ()
  "Return the target file for sequence operations.
Uses the note at point in `denote-dash-mode', the current buffer file,
or prompts with completing-read."
  (cond
   ((derived-mode-p 'denote-dash-mode) (tabulated-list-get-id))
   (buffer-file-name buffer-file-name)
   (t (completing-read "File: "
                       (seq-filter #'denote-sequence-file-p (denote-directory-files))
                       nil t))))

;;;###autoload
(defun denote-dash-insert-sequence-note ()
  "Insert a new note at the current note's sequence position.
All following siblings are shifted forward by one to make room.
Works from `denote-dash-mode' (note at point), a Denote note buffer,
or prompts for a file."
  (interactive)
  (let* ((file (denote-dash--target-file))
         (seq-id (denote-retrieve-filename-signature file)))
    (unless seq-id
      (user-error "File has no sequence ID: %s" (file-name-nondirectory file)))
    (let* ((parent-prefix (denote-sequence--get-prefix-for-siblings seq-id))
           (candidates (if (or (null parent-prefix) (string-empty-p (or parent-prefix "")))
                           (denote-sequence-get-all-files)
                         (denote-sequence-get-all-files-with-prefix parent-prefix)))
           (siblings (seq-filter (lambda (f) (denote-dash--sequence-direct-sibling-p seq-id f))
                                 candidates))
           (to-rename (thread-last siblings
                                   (seq-filter (lambda (f)
                                                 (string>= (denote-retrieve-filename-signature f) seq-id)))
                                   (seq-sort (lambda (a b)
                                               (string> (denote-retrieve-filename-signature a)
                                                        (denote-retrieve-filename-signature b)))))))
      (unless (yes-or-no-p (format "Insert before %s, shifting %d sibling%s? "
                                   seq-id (length to-rename)
                                   (if (= (length to-rename) 1) "" "s")))
        (user-error "Cancelled"))
      (seq-do (lambda (f)
                (denote-rename-file f 'keep-current 'keep-current
                                    (denote-dash--increment-sequence
                                     (denote-retrieve-filename-signature f))
                                    'keep-current 'keep-current))
              to-rename)
      (denote (read-string "Title: ")
              (denote-keywords-prompt)
              nil nil nil nil seq-id)
      (when (derived-mode-p 'denote-dash-mode)
        (denote-dash-refresh)))))

;;; Dispatch transient
;;
;; Key layout — all prefix groups share a first character, no single-char
;; binding shadows a multi-char prefix:
;;   v* = view (vv=dash) + review (vd, vl)
;;   s* = sequence (ss, sh, sp)
;;   f* = find (ff, fg, fn, fs, fo, fd, fb)
;;   l* = link (ll, lp)
;;   r* = rename (rr, rf, rp)
;;   e* = explore (er, em, ek, et, ed)
;;   i* = import (id)
;;   a* = align (al=lint, af=fix all)
;;   o* = org commands (ox, or, ol, ob, od, op, of)
;;   c* = convert (cm, cd)
;;   n, j = bare single-key create commands

;;;###autoload
(transient-define-prefix denote-dash-dispatch ()
  "Dispatch Denote commands by area."
  [["Find"
    ("ff" "find file"          consult-denote-find)
    ("fg" "grep"               consult-denote-grep)
    ("fn" "notes"              consult-notes)
    ("fs" "search all notes"   consult-notes-search-in-all-notes)
    ("fo" "open or create"     denote-open-or-create)
    ("fd" "dired"              denote-dired)
    ("fb" "backlinks"          denote-backlinks)]
   ["Create"
    ("n"  "new note"           denote)
    ("j"  "journal entry"      denote-journal-new-entry)
    ("ss" "seq sibling"        denote-sequence-new-sibling)
    ("sh" "seq child"          denote-sequence-new-child)
    ("sp" "seq parent"         denote-sequence-new-parent)
    ("si" "insert at seq"      denote-dash-insert-sequence-note)]
   ["Explore"
    ("er" "random note"        denote-explore-random-note)
    ("em" "missing links"      denote-explore-missing-links)
    ("ek" "keyword chart"      denote-explore-barchart-keywords)
    ("et" "timeline"           denote-explore-barchart-timeline)
    ("ed" "duplicates"         denote-explore-duplicate-notes)]
   ["View" :if-not-derived denote-dash-mode
    ("vv" "note list (dash)"   denote-dash)]]
  [["Link"
    ("ll" "insert link"        denote-link)
    ("lp" "link to parent"     denote-sequence-link-to-parent)]
   ["Rename"
    ("rr" "rename file"        denote-rename-file)
    ("rf" "rename from fm"     denote-rename-file-using-front-matter)
    ("rp" "reparent seq"       denote-sequence-reparent)]
   ["Review"
    ("vd" "set review date"    denote-review-set-date)
    ("vl" "review list"        denote-review-display-list)]
   ["Align"
    ("al" "lint sequences"     denote-dash-lint-sequences)
    ("af" "fix all frontmatter" denote-dash-fix-all-sequence-frontmatter)
    ("ar" "repack children"    denote-dash-repack-sequence-children)
    ("as" "swap with parent"   denote-dash-swap-with-parent)
    ("ap" "swap with previous" denote-dash-swap-with-previous)
    ("an" "swap with next"     denote-dash-swap-with-next)]
   ["Import"
    ("id" "from org datetree"  denote-dash-import-from-datetree)]]
  [["Columns" :if-derived denote-dash-mode
    ("c" "toggle columns…"     denote-dash-column-transient)]]
  [["Org" :if-derived org-mode
    ("ox" "extract subtree"    denote-org-extract-org-subtree)
    ("or" "extract + link"     tychoish-org-extract-subtree-and-link)
    ("ol" "link to heading"    denote-org-link-to-heading)
    ("ob" "heading backlinks"  denote-org-backlinks-for-heading)
    ("od" "dblock: links"      denote-org-dblock-insert-links)
    ("op" "dblock: backlinks"  denote-org-dblock-insert-backlinks)
    ("of" "dblock: files"      denote-org-dblock-insert-files)]
   ["Convert" :if-derived markdown-mode
    ("cm" "links → markdown"   denote-markdown-convert-links-to-markdown-format)
    ("cd" "links → denote"     denote-markdown-convert-links-to-denote-format)]])

(provide 'denote-dash)
;;; denote-dash.el ends here
