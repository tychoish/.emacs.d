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
(require 'annotated-completing-read)

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
Resolves the target file via `denote-dash--file-at-point', so it works from
`denote-dash-mode', `denote-sequence-hierarchy-mode', Dired, or a visited
Denote buffer."
  (interactive)
  (when-let* ((file (denote-dash--file-at-point)))
    (if (denote-dash--fix-frontmatter-from-filename file)
        (progn
          (message "Fixed: %s" (file-name-nondirectory file))
          (cond
           ((derived-mode-p 'denote-dash-mode) (denote-dash-refresh))
           ((derived-mode-p 'denote-sequence-hierarchy-mode) (revert-buffer))))
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
                (seq-do (lambda (pair)
                          (denote-dash--hierarchy-remap-fold-sequence-prefix
                           (denote-retrieve-filename-signature (car pair)) (cdr pair)))
                        to-rename)
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
          (denote-dash--hierarchy-swap-fold-sequence seq parent-seq)
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
    (denote-dash--hierarchy-swap-fold-sequence-prefix seq-a seq-b)
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

(defun denote-dash--reparent-target-sequence (file-with-sequence)
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

(defun denote-dash--recursive-reseq-plan (current-file new-seq)
  "Return the (FILE . NEW-SEQ) rename plan for moving CURRENT-FILE's subtree to NEW-SEQ.
Includes CURRENT-FILE itself plus every descendant, each rewritten under
NEW-SEQ with its relative suffix preserved, correcting the type alternation
(letter/digit) of descendant sequences when the old and new roots end in
different character types — a bug in the upstream
`denote-sequence-reparent-recursive'.  Shared by
`denote-dash--reparent-recursive-apply' and `denote-dash-renumber-recursive'."
  (let* ((root-seq (denote-retrieve-filename-signature current-file))
         (descendants (when root-seq
                        (denote-sequence-get-relative root-seq 'all-children)))
         (old-last-type (when root-seq (denote-dash--seq-last-type root-seq)))
         (new-last-type (denote-dash--seq-last-type new-seq)))
    (cons
     (cons current-file new-seq)
     (seq-keep (lambda (child)
                 (when-let* ((child-seq (denote-retrieve-filename-signature child)))
                   (cons child
                         (concat new-seq
                                 (denote-dash--alphanumeric-suffix-rewrite
                                  (string-remove-prefix root-seq child-seq)
                                  old-last-type new-last-type)))))
               descendants))))

(defun denote-dash--apply-reseq-plan (plan)
  "Rename every (FILE . NEW-SEQ) pair in PLAN via `denote-rename-file'.
Unlike `denote-dash--apply-repack-plan', entries need no staging through
temporary signatures: reparent/renumber targets always land under a
different subtree than any source file, so no two entries can collide
mid-rename.  Suppresses `denote-rename-confirmations' for the duration —
per-file prompting would be unusable across a whole subtree."
  (let ((denote-rename-confirmations nil))
    (seq-do (lambda (pair)
              (denote-rename-file (car pair) 'keep-current 'keep-current
                                  (cdr pair) 'keep-current 'keep-current))
            plan)))

(defun denote-dash--recursive-reseq-confirm-and-apply (plan operation-verb)
  "Apply PLAN, previewing and confirming first when it spans multiple files.
PLAN is a list of (FILE . NEW-SEQ) pairs, as returned by
`denote-dash--recursive-reseq-plan'.  When PLAN has only one entry (the
target file has no descendants), applies it directly with no prompt — the
operation can only ever affect that one file.  When PLAN has more than one
entry, pops the same *Denote Repack Preview* buffer used by
`denote-dash-repack-sequence-children' and asks for confirmation before
renaming anything; declining leaves every file untouched.

OPERATION-VERB names the operation for the confirmation prompt and the
cancellation message (e.g. \"Reparent\", \"Renumber\").

Returns non-nil if PLAN was applied, nil if the user declined."
  (if (<= (length plan) 1)
      (progn (denote-dash--apply-reseq-plan plan) t)
    (pop-to-buffer (denote-dash--repack-preview-buffer plan))
    (if (yes-or-no-p (format "%s %d files (this note + %d descendant%s) as previewed? "
                             operation-verb (length plan) (1- (length plan))
                             (if (= (length plan) 2) "" "s")))
        (progn (denote-dash--apply-reseq-plan plan) t)
      (message "%s cancelled; no files were changed." operation-verb)
      nil)))

(defun denote-dash--reparent-recursive-apply (current-file new-seq)
  "Re-parent CURRENT-FILE and all descendants onto NEW-SEQ.
Builds the plan via `denote-dash--recursive-reseq-plan' and applies it via
`denote-dash--recursive-reseq-confirm-and-apply', which previews and
confirms whenever CURRENT-FILE has descendants and applies directly,
without prompting, when it does not."
  (let* ((plan (denote-dash--recursive-reseq-plan current-file new-seq))
         (seq-pairs (seq-map (lambda (pair)
                                (cons (denote-retrieve-filename-signature (car pair)) (cdr pair)))
                              plan)))
    (when (denote-dash--recursive-reseq-confirm-and-apply plan "Reparent")
      (denote-dash--hierarchy-remap-fold-sequence-many seq-pairs))))

;;;###autoload
(defun denote-dash-reparent (current-file file-with-sequence &optional recursive)
  "Re-parent CURRENT-FILE to be a child of FILE-WITH-SEQUENCE.
Wraps `denote-sequence-reparent'.  That command's own interactive spec
resolves CURRENT-FILE via `denote-sequence--get-current-file-for-renaming'
and then, purely to build the target prompt's label text, calls the same
private file-or-prompt helper a second time — doubling the raw \"Rename
FILE Denote-style\" prompt whenever neither Dired nor a visited buffer is
in scope, which is exactly the case from a `denote-dash' or
sequence-hierarchy buffer.  This resolves CURRENT-FILE once via
`denote-dash--target-file' and reuses it for the label instead.

FILE-WITH-SEQUENCE may be nil, meaning \"make CURRENT-FILE a new top-level
sequence instead of a child of anything\" — plain `denote-sequence-reparent'
has no such path; it can only reparent onto another file.  Interactively
this is offered as its own y-or-n-p question before the child-of prompt,
so promoting a note out of its hierarchy doesn't require reaching for
`denote-dash-renumber-recursive' and typing a sequence by hand.

Only offers the \"reparent recursively?\" question when CURRENT-FILE
actually has descendants; a leaf note is reparented directly with no
recursion question at all, since recursing over zero descendants is a
no-op.  When it does have descendants and RECURSIVE ends up non-nil, the
multi-file rename goes through `denote-dash--reparent-recursive-apply',
which previews and confirms before touching any file; see that function.

Suppresses `denote-rename-confirmations' for the duration: this is a single
logical operation, and per-file prompting (as `denote-rename-file' does by
default) would be unusable for any subtree beyond a couple of files."
  (interactive
   (let* ((current-file (denote-dash--target-file))
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
        (denote-dash--reparent-recursive-apply
         current-file (denote-dash--reparent-target-sequence file-with-sequence))
      (if file-with-sequence
          (let ((new-seq (denote-dash--reparent-target-sequence file-with-sequence)))
            (denote-sequence-reparent current-file file-with-sequence nil)
            (denote-dash--hierarchy-remap-fold-sequence-prefix old-seq new-seq))
        (let ((new-seq (denote-sequence--get-new-parent)))
          (denote-rename-file current-file 'keep-current 'keep-current
                              new-seq 'keep-current 'keep-current)
          (denote-dash--hierarchy-remap-fold-sequence-prefix old-seq new-seq))))))

;;;###autoload
(defun denote-dash-reparent-recursive (current-file file-with-sequence)
  "Re-parent CURRENT-FILE and all descendants to be children of FILE-WITH-SEQUENCE.
FILE-WITH-SEQUENCE may be nil, meaning \"promote to a new top-level
sequence\"; see `denote-dash-reparent' for the full explanation.

Direct entry point for the recursive case; see `denote-dash-reparent' for
the interactive command that asks whether to recurse instead of requiring
a separate command.  Still previews and confirms via
`denote-dash--reparent-recursive-apply' whenever CURRENT-FILE has
descendants; only a genuinely single-file case is prompt-free."
  (interactive
   (let* ((current-file (denote-dash--target-file))
          (root-p (y-or-n-p
                   (format "Make `%s' a new top-level sequence (no parent)? "
                           (propertize current-file 'face 'denote-faces-prompt-current-name)))))
     (list
      current-file
      (unless root-p
        (denote-sequence-file-prompt
         (format "Reparent `%s' (recursively) to be a child of"
                 (propertize current-file 'face 'denote-faces-prompt-current-name)))))))
  (denote-dash--reparent-recursive-apply
   current-file (denote-dash--reparent-target-sequence file-with-sequence)))

;;;###autoload
(defun denote-dash-renumber-recursive (current-file new-seq)
  "Renumber CURRENT-FILE to NEW-SEQ, renumbering all descendants to match.
Like `denote-dash-reparent-recursive', but NEW-SEQ is the target sequence
itself rather than derived as a new child of another file's sequence — use
this to move a subtree to a specific sequence ID instead of appending it
under a parent.

Builds the plan via `denote-dash--recursive-reseq-plan' and applies it via
`denote-dash--recursive-reseq-confirm-and-apply' — previewing and
confirming whenever CURRENT-FILE has descendants, applying directly
without a prompt when it does not."
  (interactive
   (let ((current-file (denote-dash--target-file)))
     (list
      current-file
      (denote-sequence-with-error-p
       (read-string
        (format "Renumber `%s' (recursively) to sequence: "
                (propertize current-file 'face 'denote-faces-prompt-current-name)))))))
  (unless (denote-retrieve-filename-signature current-file)
    (user-error "File has no sequence: %s" (file-name-nondirectory current-file)))
  (let* ((plan (denote-dash--recursive-reseq-plan current-file new-seq))
         (seq-pairs (seq-map (lambda (pair)
                                (cons (denote-retrieve-filename-signature (car pair)) (cdr pair)))
                              plan)))
    (when (denote-dash--recursive-reseq-confirm-and-apply plan "Renumber")
      (denote-dash--hierarchy-remap-fold-sequence-many seq-pairs))))

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
                  (let* ((old-sig (denote-retrieve-filename-signature f))
                         (new-sig (denote-dash--increment-sequence old-sig)))
                    (denote-rename-file f 'keep-current 'keep-current
                                        new-sig 'keep-current 'keep-current)
                    (denote-dash--hierarchy-remap-fold-sequence-prefix old-sig new-sig)))
                to-rename))
      (denote (read-string "Title: ")
              (denote-keywords-prompt)
              nil nil nil nil seq-id)
      (when (derived-mode-p 'denote-dash-mode)
        (denote-dash-refresh)))))

;;; Bulk retag across a sequence subtree

(defconst denote-dash--retag-operations
  '(("add"     add     "Add one or more keywords")
    ("remove"  remove  "Remove one or more keywords")
    ("replace" replace "Replace one keyword with another"))
  "Operations offered by `denote-dash-retag-sequence'.")

(defun denote-dash--retag-apply (keywords operation add-kws remove-kws)
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

;;;###autoload
(defun denote-dash-retag-sequence ()
  "Add, remove, or replace a keyword across a sequence and all its descendants.
Resolves the target sequence from the note at point in `denote-dash-mode' or
`denote-sequence-hierarchy-mode', the current buffer file, or a prompt.

Only files actually affected by the chosen operation are renamed — see
`denote-dash--retag-apply' for the exact per-operation semantics; in
particular `replace' never adds the new keyword to a file that didn't
already carry the old one."
  (interactive)
  (let* ((file (denote-dash--target-file))
         (seq-id (or (denote-retrieve-filename-signature file)
                     (annotated-completing-read
                      (seq-map
		       (lambda (s) (cons s nil))
		       (denote-sequence-get-all-sequences))
                      :prompt "Sequence:"
		      :require-match t)))
         (files (denote-dash--subtree-files seq-id))
         (existing-keywords (seq-uniq (seq-mapcat #'denote-extract-keywords-from-path files))))
    (unless files
      (user-error "No files found for sequence %s" seq-id))
    (let* ((op-choice (annotated-completing-read
		       (seq-map
			(lambda (e) (cons (nth 0 e) (nth 2 e)))
			denote-dash--retag-operations)
		       :prompt "Operation:"
		       :require-match t))
           (operation (nth 1 (assoc op-choice denote-dash--retag-operations)))
	   add-kws remove-kws)
      (pcase operation
        ('add
         (setq add-kws (completing-read-multiple "Add keyword(s): " nil)))
        ('remove
         (unless existing-keywords
           (user-error "No keywords found under sequence %s" seq-id))
         (setq remove-kws (completing-read-multiple "Remove keyword(s): " existing-keywords nil t)))
        ('replace
         (unless existing-keywords
           (user-error "No keywords found under sequence %s" seq-id))
         (let ((old-kw (annotated-completing-read
                        (seq-map
			 (lambda (k) (cons k nil))
			 existing-keywords)
                        :prompt "Replace keyword:"
			:require-match t)))
           (setq remove-kws (list old-kw))
           (setq add-kws (list (read-string (format "Replace `%s' with: " old-kw)))))))
      (let* ((planned (seq-map (lambda (f)
                                 (cons f (denote-dash--retag-apply
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
        ;; Suppress per-file confirmations: the single confirmation above
        ;; already covers the whole bulk operation.
        (let ((denote-rename-confirmations nil))
          (seq-do (lambda (pair)
                    (denote-rename-file
                     (car pair) 'keep-current (cdr pair)
                     'keep-current 'keep-current 'keep-current))
                  affected)))
      (cond
       ((derived-mode-p 'denote-dash-mode) (denote-dash-refresh))
       ((derived-mode-p 'denote-sequence-hierarchy-mode) (revert-buffer))))))

(with-eval-after-load 'denote-sequence
  (define-key denote-sequence-hierarchy-mode-map (kbd "k")   #'denote-dash-retag-sequence)
  (define-key denote-sequence-hierarchy-mode-map (kbd "l")   #'denote-dash-lint-sequences)
  (define-key denote-sequence-hierarchy-mode-map (kbd "C-r") #'denote-dash-repack-sequence-children)
  (define-key denote-sequence-hierarchy-mode-map (kbd "M-r") #'denote-dash-swap-with-parent)
  (define-key denote-sequence-hierarchy-mode-map (kbd "M-p") #'denote-dash-swap-with-previous)
  (define-key denote-sequence-hierarchy-mode-map (kbd "M-n") #'denote-dash-swap-with-next)
  (define-key denote-sequence-hierarchy-mode-map (kbd "m")   #'denote-dash-reparent)
  (define-key denote-sequence-hierarchy-mode-map (kbd "u")   #'denote-dash-renumber-recursive)
  (define-key denote-sequence-hierarchy-mode-map (kbd "i")   #'denote-dash-insert-sequence-note)
  (define-key denote-sequence-hierarchy-mode-map (kbd "h")   #'denote-dash-fix-sequence-frontmatter)
  (define-key denote-sequence-hierarchy-mode-map (kbd "C-l") #'denote-dash-fix-all-sequence-frontmatter))

(provide 'denote-dash-repack)
;;; denote-dash-repack.el ends here
