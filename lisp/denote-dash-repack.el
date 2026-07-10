;;; denote-dash-repack.el --- Denote sequence renumbering and manipulation -*- lexical-binding: t; -*-

;;; Commentary:
;; Sequence-signature manipulation for Denote notes: aligning frontmatter
;; signatures with filenames, repacking/compacting gapped sequences,
;; swapping siblings or a note with its parent, recursive reparenting, and
;; inserting a note at a specific sequence position.  Companion to
;; `denote-dash', which owns the browsing UI and the low-level sequence
;; string primitives (depth/descendant/digit-type checks) these commands
;; build on.

;;; Code:

(require 'seq)
(require 'tabulated-list)
(require 'denote)
(require 'denote-sequence)
(require 'denote-dash)

(defvar denote-file-types)

;;; Sequence alignment lint / autofix

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
Root level (nil/empty PREFIX) and letter-ending PREFIX use numbers (1, 2, 3…).
Digit-ending PREFIX uses letters (a, b, c…)."
  (let* ((last-char (and prefix
                         (not (string-empty-p (or prefix "")))
                         (aref prefix (1- (length prefix)))))
         (use-letters (and last-char (denote-dash--char-digit-p last-char)))
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

(defun denote-dash--repack-plan (to-rename)
  "Expand TO-RENAME child pairs into a flat file rename plan.
TO-RENAME is a list of (CHILD-FILE . NEW-CHILD-SEQ) pairs.  Each child is
expanded to its full subtree — captured from the current on-disk state,
before any renames happen — so descendants stay consistent with their
parent's new sequence.  Return a list of (FILE . NEW-SEQ) pairs, one per
file that must be renamed: the child itself and all of its descendants."
  (seq-mapcat
   (lambda (pair)
     (let* ((child-file (car pair))
            (new-child-seq (cdr pair))
            (old-child-seq (denote-retrieve-filename-signature child-file)))
       (seq-map (lambda (f)
                  (cons f (concat new-child-seq
                                  (string-remove-prefix
                                   old-child-seq (denote-retrieve-filename-signature f)))))
                (denote-dash--subtree-files old-child-seq))))
   to-rename))

(defun denote-dash--apply-repack-plan (plan)
  "Rename every (FILE . NEW-SEQ) pair in PLAN, returning the count renamed.
Every file in PLAN is staged to a unique temporary signature before any
final rename happens.  This is essential: renaming subtrees one at a time
can leave an old-but-not-yet-renamed sibling and a freshly-renamed file
briefly sharing the same signature, which a later subtree's prefix lookup
would then sweep up and merge into the wrong destination.  Staging the
whole batch first means no real signature is ever a temporary duplicate."
  (let ((staged (seq-map-indexed
                 (lambda (pair i)
                   (let* ((file (car pair))
                          (new-seq (cdr pair))
                          (sig (denote-retrieve-filename-signature file))
                          (tmp-sig (format "rpacktmp%d" i))
                          (tmp-path (denote-dash--rename-signature-component file sig tmp-sig)))
                     (when-let* ((buf (find-buffer-visiting file)))
                       (kill-buffer buf))
                     (rename-file file tmp-path t)
                     (list tmp-path tmp-sig new-seq)))
                 plan)))
    (seq-do (lambda (entry)
              (pcase-let ((`(,tmp-path ,tmp-sig ,new-seq) entry))
                (let ((new-path (denote-dash--rename-signature-component
                                 tmp-path tmp-sig new-seq)))
                  (rename-file tmp-path new-path t)
                  (denote-dash--fix-frontmatter-from-filename new-path))))
            staged)
    (length staged)))

(defun denote-dash--repack-preview-buffer (plan)
  "Populate and return the *Denote Repack Preview* buffer describing PLAN.
PLAN is a list of (FILE . NEW-SEQ) pairs, as returned by
`denote-dash--repack-plan'."
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

(defun denote-dash-repack-sequence-children (prefix)
  "Compact direct children of PREFIX so their last segment has no gaps.
Renames each child's entire subtree (child and all descendants) so that
descendants stay consistent with their parent's new sequence.  Children
are ordered by sequence value (not lexicographically, so \"10\" sorts
after \"2\" rather than before it), and the whole batch of renames is
staged through temporary signatures together so that repacking multiple
children at once never merges two of them into the same destination.

Before touching any file, shows a preview buffer listing every rename
that would happen and asks for confirmation; declining leaves every file
untouched.  Works from `denote-dash-mode' or interactively."
  (interactive
   (list (read-string "Sequence prefix (empty = root): "
                      (when (derived-mode-p 'denote-dash-mode)
                        (when-let* ((f (tabulated-list-get-id)))
                          (denote-retrieve-filename-signature f))))))
  (let* ((prefix (if (string-empty-p prefix) nil prefix))
         (children (denote-dash--sequence-direct-children prefix))
         (sorted (denote-sequence-sort-files children)))
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
      (cond
       ((null to-rename)
        (message "Sequences for %s already compact." (or prefix "(root)")))
       (t
        (let ((plan (denote-dash--repack-plan to-rename)))
          (pop-to-buffer (denote-dash--repack-preview-buffer plan))
          (if (yes-or-no-p (format "Rename %d file%s of %s as previewed? "
                                   (length plan) (if (= (length plan) 1) "" "s")
                                   (or prefix "(root)")))
              (progn
                (denote-dash--apply-repack-plan plan)
                (let ((n (denote-dash--fix-all-frontmatter-silent)))
                  (message "Repacked %d/%d children of %s; fixed %d frontmatter %s."
                           (length to-rename) (length sorted) (or prefix "(root)")
                           n (if (= n 1) "note" "notes"))))
            (message "Repack cancelled; no files were changed."))))))
    (when (derived-mode-p 'denote-dash-mode)
      (denote-dash-refresh))))

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

;;; Recursive reparent with correct alphanumeric suffix rewriting

(defun denote-dash--seq-last-type (seq)
  "Return :digit or :letter for the type of the last character in SEQ."
  (if (denote-dash--char-digit-p (aref seq (1- (length seq))))
      :digit
    :letter))

(defun denote-dash--alphanumeric-suffix-rewrite (suffix old-root-last-type new-root-last-type)
  "Rewrite SUFFIX so it is valid under a root ending with NEW-ROOT-LAST-TYPE.
OLD-ROOT-LAST-TYPE is the type of the last character of the old root (:digit
or :letter).  Returns the suffix unchanged when both types agree."
  (if (eq old-root-last-type new-root-last-type)
      suffix
    (let* ((old-first (if (eq old-root-last-type :digit) :letter :digit))
           (new-first (if (eq new-root-last-type :digit) :letter :digit))
           (pos 0)
           (current-type old-first)
           (positions nil))
      (while (< pos (length suffix))
        (if (eq current-type :digit)
            (let ((start pos))
              (while (and (< pos (length suffix))
                          (denote-dash--char-digit-p (aref suffix pos)))
                (setq pos (1+ pos)))
              (push (string-to-number (substring suffix start pos)) positions)
              (setq current-type :letter))
          (let ((start pos))
            (while (and (< pos (length suffix))
                        (not (denote-dash--char-digit-p (aref suffix pos))))
              (setq pos (1+ pos)))
            (push (string-to-number
                   (denote-sequence--alpha-to-number (substring suffix start pos)))
                  positions)
            (setq current-type :digit))))
      (let ((rebuild-type new-first)
            (result ""))
        (seq-do (lambda (p)
                  (setq result
                        (concat result
                                (if (eq rebuild-type :digit)
                                    (number-to-string p)
                                  (denote-sequence--number-to-alpha (number-to-string p)))))
                  (setq rebuild-type (if (eq rebuild-type :digit) :letter :digit)))
                (nreverse positions))
        result))))

;;;###autoload
(defun denote-dash-reparent-recursive (current-file file-with-sequence)
  "Re-parent CURRENT-FILE and all descendants to be children of FILE-WITH-SEQUENCE.
Corrects the type alternation (letter/digit) of descendant sequences when the
old and new roots end in different character types — a bug in the upstream
`denote-sequence-reparent-recursive'.

Suppresses `denote-rename-confirmations' for the duration: this is a single
logical operation that may rename many descendants, and prompting once per
file (as `denote-rename-file' does by default) would be unusable for any
subtree beyond a couple of files."
  (interactive
   (list
    (denote-sequence--get-current-file-for-renaming)
    (denote-sequence-file-prompt
     (format "Reparent `%s' (recursively) to be a child of"
             (propertize (denote--rename-dired-file-or-current-file-or-prompt)
                         'face 'denote-faces-prompt-current-name)))))
  (let* ((root-seq (denote-retrieve-filename-signature current-file))
         (target-seq (or (denote-sequence-file-p file-with-sequence)
                         (denote-sequence-p file-with-sequence)
                         (user-error "No sequence found in `%s'" file-with-sequence)))
         (new-seq (denote-sequence--get-new-child target-seq))
         (descendants (when root-seq
                        (denote-sequence-get-relative root-seq 'all-children)))
         (rename-fn (lambda (file seq)
                      (denote-rename-file file 'keep-current 'keep-current
                                          seq 'keep-current 'keep-current)))
         (old-last-type (when root-seq (denote-dash--seq-last-type root-seq)))
         (new-last-type (denote-dash--seq-last-type new-seq))
         (denote-rename-confirmations nil))
    (funcall rename-fn current-file new-seq)
    (seq-do (lambda (child)
              (when-let* ((child-seq (denote-retrieve-filename-signature child)))
                (let* ((raw-suffix (string-remove-prefix root-seq child-seq))
                       (fixed-suffix (denote-dash--alphanumeric-suffix-rewrite
                                      raw-suffix old-last-type new-last-type)))
                  (funcall rename-fn child (concat new-seq fixed-suffix)))))
            descendants)))

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
                                                 (not (string< (denote-retrieve-filename-signature f) seq-id))))
                                   (seq-sort (lambda (a b)
                                               (string> (denote-retrieve-filename-signature a)
                                                        (denote-retrieve-filename-signature b)))))))
      (unless (yes-or-no-p (format "Insert before %s, shifting %d sibling%s? "
                                   seq-id (length to-rename)
                                   (if (= (length to-rename) 1) "" "s")))
        (user-error "Cancelled"))
      ;; Suppress per-file confirmations: the shift above is one logical
      ;; operation already confirmed once; `denote-rename-file' would
      ;; otherwise prompt again for each shifted sibling.
      (let ((denote-rename-confirmations nil))
        (seq-do (lambda (f)
                  (denote-rename-file f 'keep-current 'keep-current
                                      (denote-dash--increment-sequence
                                       (denote-retrieve-filename-signature f))
                                      'keep-current 'keep-current))
                to-rename))
      (denote (read-string "Title: ")
              (denote-keywords-prompt)
              nil nil nil nil seq-id)
      (when (derived-mode-p 'denote-dash-mode)
        (denote-dash-refresh)))))

(provide 'denote-dash-repack)
;;; denote-dash-repack.el ends here
