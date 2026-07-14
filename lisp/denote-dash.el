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
(require 'denote-sequence)

;;; Declarations

(declare-function consult-denote-find "consult-denote")
(declare-function consult-denote-grep "consult-denote")
(declare-function consult-notes "consult-notes")
(declare-function consult-notes-search-in-all-notes "consult-notes")
(declare-function denote-journal-new-entry "denote-journal")
(declare-function denote-markdown-convert-links-to-markdown-format "denote-markdown")
(declare-function denote-markdown-convert-links-to-denote-format "denote-markdown")
(declare-function denote-review-set-date "denote-review")
(declare-function denote-review-display-list "denote-review")
(declare-function annotated-completing-read "annotated-completing-read")
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
(declare-function org-migrate-subtree-to-denote "orgx")
(declare-function org-back-to-heading "org")
(declare-function org-end-of-meta-data "org")
(declare-function org-end-of-subtree "org")
(declare-function org-get-heading "org")
(declare-function org-get-tags "org")
(declare-function org-map-entries "org")
(declare-function org-read-date "org")
(declare-function org-up-heading-safe "org")
(declare-function denote-dash-lint-sequences "denote-dash-repack")
(declare-function denote-dash-fix-all-sequence-frontmatter "denote-dash-repack")
(declare-function denote-dash-repack-sequence-children "denote-dash-repack")
(declare-function denote-dash-swap-with-parent "denote-dash-repack")
(declare-function denote-dash-swap-with-previous "denote-dash-repack")
(declare-function denote-dash-swap-with-next "denote-dash-repack")
(declare-function denote-dash-reparent-recursive "denote-dash-repack")
(declare-function denote-dash-insert-sequence-note "denote-dash-repack")

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

(defcustom denote-dash-title-source 'front-matter
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

(defvar-local denote-dash--narrowed-sequences nil
  "List of sequence-ID strings to narrow to, or nil to show all sequences.")

(defvar-local denote-dash--keyword-toggles nil
  "List of keyword strings toggled on for `denote-dash--current-filter'.")

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

(defun denote-dash--sequence-in-narrow-p (seq-id narrowed)
  "Return non-nil if SEQ-ID equals or descends from a sequence in NARROWED."
  (seq-some (lambda (n) (or (string= n seq-id)
                            (denote-dash--sequence-descendant-p n seq-id)))
            narrowed))

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
         (or (null denote-dash--narrowed-sequences)
             (and seq-id (denote-dash--sequence-in-narrow-p seq-id denote-dash--narrowed-sequences)))
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

(defvar-keymap denote-dash-narrow-map
  :doc "Keymap for sequence/keyword narrowing in `denote-dash-mode'."
  "s" #'denote-dash-narrow-to-sequence
  "t" #'denote-dash-toggle-sequence-narrow
  "w" #'denote-dash-widen
  "k" #'denote-dash-toggle-keyword)

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
  "C-n"     denote-dash-narrow-map
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

;;; Buffer management

(defun denote-dash--note-buffers ()
  "Return the list of live buffers visiting a Denote note file."
  (seq-filter
   (lambda (buf)
     (when-let* ((file (buffer-file-name buf)))
       (denote-file-has-identifier-p file)))
   (buffer-list)))

;;;###autoload
(defun denote-dash-close-all-notes ()
  "Close every open Denote note buffer, prompting to save modified ones.
For a modified buffer whose file no longer exists on disk — for example
because a sequence operation renamed it out from under the buffer — the
user is informed and asked whether to save (which recreates the file) or
discard the changes."
  (interactive)
  (let ((buffers (denote-dash--note-buffers))
        (closed 0)
        (saved 0)
        (stale nil))
    (if (null buffers)
        (message "No open Denote note buffers")
      (seq-do
       (lambda (buf)
         (with-current-buffer buf
           (when (buffer-modified-p)
             (cond
              ((not (and (buffer-file-name) (file-exists-p (buffer-file-name))))
               (push (buffer-name) stale)
               (if (yes-or-no-p
                    (format "%s: file no longer exists on disk; save anyway (recreates it)? "
                            (buffer-name)))
                   (progn (save-buffer) (setq saved (1+ saved)))
                 (set-buffer-modified-p nil)))
              ((y-or-n-p (format "Save %s? " (buffer-name)))
               (save-buffer)
               (setq saved (1+ saved)))
              (t (set-buffer-modified-p nil)))))
         (kill-buffer buf)
         (setq closed (1+ closed)))
       buffers)
      (message "Closed %d Denote buffer%s (%d saved)%s"
               closed (if (= closed 1) "" "s") saved
               (if stale
                   (format "; %d had missing files: %s"
                           (length stale) (string-join (nreverse stale) ", "))
                 "")))))

;;; Sequence hierarchy buffer directory

(defun denote-dash--hierarchy-sync-directory ()
  "Set `default-directory' to the file at point, else the first Denote directory.
Each line in a `denote-sequence-view-hierarchy' buffer carries the note
path in the `denote-sequence-hierarchy-file' text property."
  (setq default-directory
        (or (when-let* ((file (get-text-property (point) 'denote-sequence-hierarchy-file)))
              (file-name-directory file))
            (car (denote-directories))
            default-directory)))

(defun denote-dash--hierarchy-setup-directory ()
  "Initialize a hierarchy buffer's `default-directory' and track point movement."
  (denote-dash--hierarchy-sync-directory)
  (add-hook 'post-command-hook #'denote-dash--hierarchy-sync-directory nil t))

(add-hook 'denote-sequence-hierarchy-mode-hook #'denote-dash--hierarchy-setup-directory)

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

(defun denote-dash-toggle-keyword ()
  "Toggle one keyword in `denote-dash--keyword-toggles' using a single ACR pick.
Without a prefix argument the toggled keywords are OR'd together; with a
prefix argument they are AND'd."
  (interactive)
  (require 'annotated-completing-read)
  (let* ((keyword (annotated-completing-read
                   (seq-map (lambda (k) (cons k nil)) (denote-dash--all-keywords))
                   :prompt "Toggle keyword: " :require-match t))
         (toggles (if (member keyword denote-dash--keyword-toggles)
                      (remove keyword denote-dash--keyword-toggles)
                    (cons keyword denote-dash--keyword-toggles))))
    (setq denote-dash--keyword-toggles toggles)
    (setq denote-dash--current-filter
          (cond
           ((null toggles) nil)
           ((= (length toggles) 1) (car toggles))
           (current-prefix-arg `(and ,@toggles))
           (t `(or ,@toggles))))
    (denote-dash-refresh)))

;;; Sequence narrowing

(defun denote-dash-narrow-to-sequence ()
  "Narrow the displayed notes to one or more sequences, selected all at once."
  (interactive)
  (let ((selected (completing-read-multiple
                   "Narrow to sequences: " (denote-sequence-get-all-sequences))))
    (setq denote-dash--narrowed-sequences selected)
    (denote-dash-refresh)))

(defun denote-dash-toggle-sequence-narrow ()
  "Toggle a single sequence in or out of the current narrow set."
  (interactive)
  (require 'annotated-completing-read)
  (let* ((seq-id (annotated-completing-read
                  (seq-map (lambda (s) (cons s nil)) (denote-sequence-get-all-sequences))
                  :prompt "Toggle sequence: " :require-match t)))
    (setq denote-dash--narrowed-sequences
          (if (member seq-id denote-dash--narrowed-sequences)
              (remove seq-id denote-dash--narrowed-sequences)
            (cons seq-id denote-dash--narrowed-sequences)))
    (denote-dash-refresh)))

(defun denote-dash-widen ()
  "Clear sequence narrowing and show notes from all sequences."
  (interactive)
  (setq denote-dash--narrowed-sequences nil)
  (denote-dash-refresh))

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

;;; Column sort

(defun denote-dash--sortable-column-names ()
  "Return names of columns in the current `tabulated-list-format' that sort."
  (thread-last tabulated-list-format
               (seq-filter (lambda (col) (aref col 2)))
               (seq-map (lambda (col) (aref col 0)))))

(defun denote-dash-select-sort-column ()
  "Choose which column to sort the *denote-dash* buffer by."
  (interactive)
  (let ((name (completing-read "Sort by column: "
                               (denote-dash--sortable-column-names) nil t)))
    (setq tabulated-list-sort-key (cons name nil))
    (denote-dash-refresh)))

(defun denote-dash-toggle-sort-direction ()
  "Flip the direction of the active sort in the *denote-dash* buffer."
  (interactive)
  (unless tabulated-list-sort-key
    (user-error "No sort column selected"))
  (setcdr tabulated-list-sort-key (not (cdr tabulated-list-sort-key)))
  (denote-dash-refresh))

(defun denote-dash-clear-sort ()
  "Clear the active sort, restoring the default file order."
  (interactive)
  (setq tabulated-list-sort-key nil)
  (denote-dash-refresh))

(defun denote-dash--sort-description ()
  "Transient label showing the active sort column and direction."
  (if tabulated-list-sort-key
      (format "sort: %s %s" (car tabulated-list-sort-key)
              (if (cdr tabulated-list-sort-key) "▼" "▲"))
    "sort: (none)"))

(transient-define-prefix denote-dash-sort-transient ()
  "Select and modify the active sort column in the *denote-dash* buffer."
  [:description #'denote-dash--sort-description
   ("s" "select column"    denote-dash-select-sort-column)
   ("d" "toggle direction" denote-dash-toggle-sort-direction)
   ("x" "clear"            denote-dash-clear-sort)])

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
    ("g" "git"       denote-dash-toggle-git-column)
    ("o" "sort…"     denote-dash-sort-transient)]
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

;;; Sequence alignment, repack, swap, and reparent live in denote-dash-repack.el

(defun denote-dash-rename-all-files-using-front-matter ()
  "Rename every Denote note's filename to match its front matter.
Inverse of `denote-dash-fix-all-sequence-frontmatter': that command makes
frontmatter match the filename, this makes the filename match frontmatter,
for every note, via `denote-rename-file-using-front-matter'."
  (interactive)
  (let* ((files (denote-directory-files))
         (n (length files)))
    (unless (yes-or-no-p (format "Rename %d note%s from front matter? "
                                 n (if (= n 1) "" "s")))
      (user-error "Cancelled"))
    (let ((renamed 0) (errors 0)
          (denote-rename-confirmations nil))
      (seq-do (lambda (file)
                (condition-case err
                    (progn (denote-rename-file-using-front-matter file)
                           (setq renamed (1+ renamed)))
                  (error
                   (setq errors (1+ errors))
                   (message "Skipped %s: %s"
                            (file-name-nondirectory file)
                            (error-message-string err)))))
              files)
      (message "Renamed %d/%d note%s%s."
               renamed n (if (= renamed 1) "" "s")
               (if (> errors 0) (format " (%d errors)" errors) "")))
    (when (derived-mode-p 'denote-dash-mode) (denote-dash-refresh))))

;;; File type migration

(defun denote-dash--front-matter-end (file-type)
  "Return the position where FILE-TYPE's front matter block ends in the
current buffer.  Finds the last line matching any of the title, keywords,
signature, identifier, or date key regexps for FILE-TYPE, then also
consumes a trailing delimiter-only line (e.g. --- or +++, used to close
YAML/TOML front matter) and any blank separator lines that follow it."
  (let ((end (point-min)))
    (seq-do (lambda (component)
              (save-excursion
                (goto-char (point-min))
                (when (re-search-forward
                       (funcall (denote--get-component-key-regexp-function component) file-type)
                       nil t)
                  (setq end (max end (1+ (line-end-position)))))))
            '(title keywords signature identifier date))
    (goto-char end)
    (when (looking-at "[ \t]*\\(?:-\\{3,\\}\\|\\+\\{3,\\}\\)[ \t]*\n")
      (goto-char (match-end 0)))
    (skip-chars-forward "\n")
    (point)))

(defun denote-dash-convert-file-type (file new-file-type)
  "Migrate FILE's front matter and extension to NEW-FILE-TYPE.
Rewrites only the front matter block (title, keywords, signature,
identifier, date) in NEW-FILE-TYPE's syntax and renames the file to
match NEW-FILE-TYPE's extension.  The note body is left untouched —
converting its prose between formats (e.g. Org markup to Markdown
syntax) is left to the author."
  (interactive
   (list (denote-dash--target-file)
         (denote--valid-file-type (or (denote-file-type-prompt) denote-file-type))))
  (let* ((old-file-type (denote-filetype-heuristics file)))
    (when (eq old-file-type new-file-type)
      (user-error "File is already of type %s" new-file-type))
    (let* ((id (or (denote-retrieve-filename-identifier file) ""))
           (date (denote-retrieve-front-matter-date-value file old-file-type))
           (title (or (denote-retrieve-title-or-filename file old-file-type) ""))
           (keywords (denote-retrieve-front-matter-keywords-value file old-file-type))
           (signature (or (denote-retrieve-filename-signature file) ""))
           (new-front-matter (denote--format-front-matter title date keywords id signature new-file-type))
           (new-name (denote-format-file-name (file-name-directory file) id keywords title
                                              (denote--file-extension new-file-type) signature)))
      (unless (yes-or-no-p (format "Convert %s from %s to %s (front matter + extension only)? "
                                   (file-name-nondirectory file) old-file-type new-file-type))
        (user-error "Cancelled"))
      (let ((buf (find-file-noselect file)))
        (with-current-buffer buf
          (save-excursion
            (goto-char (point-min))
            (delete-region (point-min) (denote-dash--front-matter-end old-file-type))
            (goto-char (point-min))
            (insert new-front-matter))
          (save-buffer))
        (kill-buffer buf))
      (unless (string= (expand-file-name file) (expand-file-name new-name))
        (rename-file file new-name))
      (message "Converted %s -> %s" (file-name-nondirectory file) (file-name-nondirectory new-name))
      (when (derived-mode-p 'denote-dash-mode) (denote-dash-refresh)))))

;;; Target file resolution

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
;;   a* = sequence (al=lint, af=fix all)
;;   o* = org commands (ox, or, ol, ob, od, op, of)
;;   c* = convert (cm, cd)
;;   w* = narrow (ws, wt, ww, wk)
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
   ["View"
    ("vv" "note list (dash)"   denote-dash
     :inapt-if-derived denote-dash-mode)
    ("vh" "sequence hierarchy" denote-sequence-view-hierarchy
     :inapt-if-derived denote-sequence-hierarchy-mode)
    ("vc" "close all notes"    denote-dash-close-all-notes)]
   ["Columns" :if-derived denote-dash-mode
    ("c" "toggle columns…"     denote-dash-column-transient)]
   ["Narrow" :if-derived denote-dash-mode
    ("ws" "narrow to seq(s)"   denote-dash-narrow-to-sequence)
    ("wt" "toggle seq"         denote-dash-toggle-sequence-narrow)
    ("ww" "widen"              denote-dash-widen)
    ("wk" "toggle keyword"     denote-dash-toggle-keyword)]]
  [["Link"
    ("ll" "insert link"        denote-link)
    ("lp" "link (sequence)"    denote-sequence-link)]
   ["Rename"
    ("rr" "rename file"        denote-rename-file)
    ("rf" "rename from fm"     denote-rename-file-using-front-matter)
    ("rt" "retag (keywords)"   denote-rename-file-keywords)
    ("rc" "convert file type"  denote-dash-convert-file-type)
    ("rp" "reparent seq"       denote-sequence-reparent)
    ("rs" "reparent recursive" denote-dash-reparent-recursive)]
   ["Review"
    ("vd" "set review date"    denote-review-set-date)
    ("vl" "review list"        denote-review-display-list)]
   ["Sequence"
    ("al" "lint sequences"     denote-dash-lint-sequences)
    ("af" "fix all frontmatter" denote-dash-fix-all-sequence-frontmatter)
    ("au" "update all from fm" denote-dash-rename-all-files-using-front-matter)
    ("ar" "repack children"    denote-dash-repack-sequence-children)
    ("as" "swap with parent"   denote-dash-swap-with-parent)
    ("ap" "swap with previous" denote-dash-swap-with-previous)
    ("an" "swap with next"     denote-dash-swap-with-next)]
   ["Import"
    ("id" "from org datetree"  denote-dash-import-from-datetree)]]
  [["Org" :if-derived org-mode
    ("ox" "extract subtree"    denote-org-extract-org-subtree)
    ("or" "extract + link"     org-migrate-subtree-to-denote)
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
