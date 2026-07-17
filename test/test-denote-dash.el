;;; test-denote-dash.el --- ERT tests for denote-dash.el -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for the pure functions in denote-dash.el: filter expression evaluator,
;; sequence hierarchy utilities, fold visibility logic, and column validation.

;;; Code:

(require 'ert)
(require 'denote-dash)
(require 'denote-review)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-dash--char-digit-p

(ert-deftest denote-dash-test/char-digit-p-digits ()
  "Digit characters 0-9 return non-nil."
  (should (denote-dash--char-digit-p ?0))
  (should (denote-dash--char-digit-p ?5))
  (should (denote-dash--char-digit-p ?9)))

(ert-deftest denote-dash-test/char-digit-p-letters ()
  "Letter characters return nil."
  (should-not (denote-dash--char-digit-p ?a))
  (should-not (denote-dash--char-digit-p ?z))
  (should-not (denote-dash--char-digit-p ?A)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-dash--sequence-depth

(ert-deftest denote-dash-test/sequence-depth-root ()
  "Root-level sequence IDs have depth 0."
  (should (= 0 (denote-dash--sequence-depth "1")))
  (should (= 0 (denote-dash--sequence-depth "2")))
  (should (= 0 (denote-dash--sequence-depth "10"))))

(ert-deftest denote-dash-test/sequence-depth-children ()
  "First-level children have depth 1."
  (should (= 1 (denote-dash--sequence-depth "1a")))
  (should (= 1 (denote-dash--sequence-depth "1b")))
  (should (= 1 (denote-dash--sequence-depth "2a"))))

(ert-deftest denote-dash-test/sequence-depth-grandchildren ()
  "Second-level children have depth 2."
  (should (= 2 (denote-dash--sequence-depth "1a1")))
  (should (= 2 (denote-dash--sequence-depth "1b2"))))

(ert-deftest denote-dash-test/sequence-depth-deeper ()
  "Third-level has depth 3."
  (should (= 3 (denote-dash--sequence-depth "1a1a")))
  (should (= 3 (denote-dash--sequence-depth "1a1b"))))

(ert-deftest denote-dash-test/sequence-depth-nil ()
  "Nil and empty string return depth 0."
  (should (= 0 (denote-dash--sequence-depth nil)))
  (should (= 0 (denote-dash--sequence-depth ""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-dash--sequence-descendant-p

(ert-deftest denote-dash-test/descendant-p-direct-child ()
  "Direct children are recognized as descendants."
  (should (denote-dash--sequence-descendant-p "1" "1a"))
  (should (denote-dash--sequence-descendant-p "1a" "1a1"))
  (should (denote-dash--sequence-descendant-p "1a1" "1a1a")))

(ert-deftest denote-dash-test/descendant-p-grandchild ()
  "Grandchildren are recognized as descendants."
  (should (denote-dash--sequence-descendant-p "1" "1a1"))
  (should (denote-dash--sequence-descendant-p "1" "1a1a")))

(ert-deftest denote-dash-test/descendant-p-same-level-sibling ()
  "Same-level siblings are not descendants (same prefix, same char type)."
  (should-not (denote-dash--sequence-descendant-p "1" "10"))
  (should-not (denote-dash--sequence-descendant-p "1" "11"))
  (should-not (denote-dash--sequence-descendant-p "1a" "1ab")))

(ert-deftest denote-dash-test/descendant-p-equal-ids ()
  "An ID is not its own descendant."
  (should-not (denote-dash--sequence-descendant-p "1" "1"))
  (should-not (denote-dash--sequence-descendant-p "1a" "1a")))

(ert-deftest denote-dash-test/descendant-p-unrelated ()
  "Unrelated IDs return nil."
  (should-not (denote-dash--sequence-descendant-p "2" "1a"))
  (should-not (denote-dash--sequence-descendant-p "1b" "1a1")))

(ert-deftest denote-dash-test/descendant-p-nil ()
  "Nil arguments return nil."
  (should-not (denote-dash--sequence-descendant-p nil "1a"))
  (should-not (denote-dash--sequence-descendant-p "1" nil))
  (should-not (denote-dash--sequence-descendant-p nil nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-dash--direct-child-p

(ert-deftest denote-dash-test/direct-child-p-yes ()
  "Immediate children are recognized."
  (should (denote-dash--direct-child-p "1" "1a"))
  (should (denote-dash--direct-child-p "1a" "1a1"))
  (should (denote-dash--direct-child-p "1a1" "1a1a")))

(ert-deftest denote-dash-test/direct-child-p-grandchild ()
  "Grandchildren are not direct children."
  (should-not (denote-dash--direct-child-p "1" "1a1"))
  (should-not (denote-dash--direct-child-p "1" "1a1a")))

(ert-deftest denote-dash-test/direct-child-p-unrelated ()
  "Unrelated IDs return nil."
  (should-not (denote-dash--direct-child-p "2" "1a"))
  (should-not (denote-dash--direct-child-p "1b" "1a1")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-dash--sequence-in-narrow-p

(ert-deftest denote-dash-test/in-narrow-p-exact-match ()
  "A sequence equal to a narrowed entry is in the narrow set."
  (should (denote-dash--sequence-in-narrow-p "1a" '("1a"))))

(ert-deftest denote-dash-test/in-narrow-p-descendant-match ()
  "A descendant of a narrowed sequence is in the narrow set."
  (should (denote-dash--sequence-in-narrow-p "1a1" '("1a"))))

(ert-deftest denote-dash-test/in-narrow-p-unrelated ()
  "An unrelated sequence is not in the narrow set."
  (should-not (denote-dash--sequence-in-narrow-p "2" '("1a"))))

(ert-deftest denote-dash-test/in-narrow-p-multiple-narrowed ()
  "Matching any one of several narrowed sequences is sufficient."
  (should (denote-dash--sequence-in-narrow-p "2b" '("1a" "2"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-dash--file-visible-p with sequence narrowing

(ert-deftest denote-dash-test/file-visible-narrowed-shows-descendant ()
  "With a narrow set active, descendants of a narrowed sequence are visible."
  (let ((denote-dash--current-filter nil)
        (denote-dash--narrowed-sequences '("1a"))
        (denote-dash--active-directory nil)
        (denote-dash--show-non-sequence t)
        (denote-dash--fold-state (make-hash-table :test #'equal))
        (denote-dash--global-cycle-depth nil))
    (should (denote-dash--file-visible-p
             "/tmp/20240101T120000==1a1--child.org" '("1a" "1a1")))))

(ert-deftest denote-dash-test/file-visible-narrowed-hides-unrelated ()
  "With a narrow set active, unrelated sequences are hidden."
  (let ((denote-dash--current-filter nil)
        (denote-dash--narrowed-sequences '("1a"))
        (denote-dash--active-directory nil)
        (denote-dash--show-non-sequence t)
        (denote-dash--fold-state (make-hash-table :test #'equal))
        (denote-dash--global-cycle-depth nil))
    (should-not (denote-dash--file-visible-p
                 "/tmp/20240101T120000==2--other.org" '("1a" "2")))))

(ert-deftest denote-dash-test/file-visible-nil-narrow-shows-everything ()
  "A nil narrow set imposes no restriction."
  (let ((denote-dash--current-filter nil)
        (denote-dash--narrowed-sequences nil)
        (denote-dash--active-directory nil)
        (denote-dash--show-non-sequence t)
        (denote-dash--fold-state (make-hash-table :test #'equal))
        (denote-dash--global-cycle-depth nil))
    (should (denote-dash--file-visible-p
             "/tmp/20240101T120000==2--other.org" '("1a" "2")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-dash--matches-p

(ert-deftest denote-dash-test/matches-p-nil-filter ()
  "nil filter matches everything."
  (should (denote-dash--matches-p '() nil))
  (should (denote-dash--matches-p '("foo" "bar") nil)))

(ert-deftest denote-dash-test/matches-p-string-hit ()
  "String filter matches when keyword is present."
  (should (denote-dash--matches-p '("project" "journal") "project"))
  (should (denote-dash--matches-p '("project") "project")))

(ert-deftest denote-dash-test/matches-p-string-miss ()
  "String filter does not match when keyword is absent."
  (should-not (denote-dash--matches-p '("journal") "project"))
  (should-not (denote-dash--matches-p '() "project")))

(ert-deftest denote-dash-test/matches-p-and ()
  "(and ...) requires all keywords."
  (should (denote-dash--matches-p '("project" "reference") '(and "project" "reference")))
  (should-not (denote-dash--matches-p '("project") '(and "project" "reference"))))

(ert-deftest denote-dash-test/matches-p-or ()
  "(or ...) requires at least one keyword."
  (should (denote-dash--matches-p '("journal") '(or "project" "journal")))
  (should (denote-dash--matches-p '("project") '(or "project" "journal")))
  (should-not (denote-dash--matches-p '("idea") '(or "project" "journal"))))

(ert-deftest denote-dash-test/matches-p-not ()
  "(not ...) inverts the match."
  (should (denote-dash--matches-p '("journal") '(not "project")))
  (should-not (denote-dash--matches-p '("project") '(not "project"))))

(ert-deftest denote-dash-test/matches-p-nested ()
  "Nested expressions compose correctly."
  (let ((kws '("project" "idea")))
    (should (denote-dash--matches-p kws '(and "project" (not "archive"))))
    (should-not (denote-dash--matches-p kws '(and "project" (not "idea"))))))

(ert-deftest denote-dash-test/matches-p-or-and-not ()
  "Mixed (or (and ...) ...) forms evaluate correctly."
  (should (denote-dash--matches-p '("project") '(or (and "project" "reference") "project")))
  (should-not (denote-dash--matches-p '("idea") '(or (and "project" "reference") "project"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-dash--valid-filter-p

(ert-deftest denote-dash-test/valid-filter-nil ()
  "nil is a valid filter."
  (should (denote-dash--valid-filter-p nil)))

(ert-deftest denote-dash-test/valid-filter-string ()
  "Strings are valid filters."
  (should (denote-dash--valid-filter-p "project")))

(ert-deftest denote-dash-test/valid-filter-compound ()
  "Compound forms are valid when their sub-expressions are valid."
  (should (denote-dash--valid-filter-p '(and "a" "b")))
  (should (denote-dash--valid-filter-p '(or "a" (not "b"))))
  (should (denote-dash--valid-filter-p '(not "a"))))

(ert-deftest denote-dash-test/valid-filter-invalid ()
  "Unknown form heads return nil."
  (should-not (denote-dash--valid-filter-p '(xor "a" "b")))
  (should-not (denote-dash--valid-filter-p 42))
  (should-not (denote-dash--valid-filter-p '(and "a" 99))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-dash--fold-visible-p

(ert-deftest denote-dash-test/fold-visible-nil-seq ()
  "Non-sequence notes (nil seq-id) are always visible."
  (let ((denote-dash--fold-state (make-hash-table :test #'equal))
        (denote-dash--global-cycle-depth nil))
    (should (denote-dash--fold-visible-p nil '("1" "1a")))))

(ert-deftest denote-dash-test/fold-visible-no-ancestors ()
  "Root with no ancestors is always visible."
  (let ((denote-dash--fold-state (make-hash-table :test #'equal))
        (denote-dash--global-cycle-depth nil))
    (should (denote-dash--fold-visible-p "1" '("1" "1a" "1a1")))))

(ert-deftest denote-dash-test/fold-visible-folded-ancestor-hides ()
  "A folded ancestor hides its descendants."
  (let ((denote-dash--fold-state (make-hash-table :test #'equal))
        (denote-dash--global-cycle-depth nil))
    (setf (map-elt denote-dash--fold-state "1") 'folded)
    (should-not (denote-dash--fold-visible-p "1a" '("1" "1a" "1a1")))
    (should-not (denote-dash--fold-visible-p "1a1" '("1" "1a" "1a1")))))

(ert-deftest denote-dash-test/fold-visible-children-shows-direct-child ()
  "A `children' ancestor shows its direct children but hides deeper descendants."
  (let ((denote-dash--fold-state (make-hash-table :test #'equal))
        (denote-dash--global-cycle-depth nil))
    (setf (map-elt denote-dash--fold-state "1") 'children)
    (should     (denote-dash--fold-visible-p "1a"  '("1" "1a" "1a1")))
    (should-not (denote-dash--fold-visible-p "1a1" '("1" "1a" "1a1")))))

(ert-deftest denote-dash-test/fold-visible-subtree-shows-all ()
  "A `subtree' ancestor (default) shows all descendants."
  (let ((denote-dash--fold-state (make-hash-table :test #'equal))
        (denote-dash--global-cycle-depth nil))
    (setf (map-elt denote-dash--fold-state "1") 'subtree)
    (should (denote-dash--fold-visible-p "1a"  '("1" "1a" "1a1")))
    (should (denote-dash--fold-visible-p "1a1" '("1" "1a" "1a1")))))

(ert-deftest denote-dash-test/fold-visible-global-depth-zero ()
  "Global depth 0 shows only root-level (depth-0) sequences."
  (let ((denote-dash--fold-state (make-hash-table :test #'equal))
        (denote-dash--global-cycle-depth 0))
    (should     (denote-dash--fold-visible-p "1"   '("1" "1a" "1a1")))
    (should-not (denote-dash--fold-visible-p "1a"  '("1" "1a" "1a1")))
    (should-not (denote-dash--fold-visible-p "1a1" '("1" "1a" "1a1")))))

(ert-deftest denote-dash-test/fold-visible-global-depth-one ()
  "Global depth 1 shows roots and first-level children."
  (let ((denote-dash--fold-state (make-hash-table :test #'equal))
        (denote-dash--global-cycle-depth 1))
    (should     (denote-dash--fold-visible-p "1"   '("1" "1a" "1a1")))
    (should     (denote-dash--fold-visible-p "1a"  '("1" "1a" "1a1")))
    (should-not (denote-dash--fold-visible-p "1a1" '("1" "1a" "1a1")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-dash-close-all-notes (buffer-based)
;;
;; NOTE: this command closes ALL open Denote note buffers in the session,
;; so these tests are only safe in a fresh (batch) Emacs, never a live one
;; with real notes open.

(require 'cl-lib)

(defun denote-dash-test--visit-note (dir id title &optional write)
  "Open a buffer visiting a Denote-named note in DIR and return it.
ID is the timestamp identifier, TITLE the note title.  When WRITE is
non-nil the file is created on disk first; otherwise the buffer visits a
path that does not yet exist."
  (let ((file (expand-file-name
               (format "%s--%s.org" id (downcase (replace-regexp-in-string " " "-" title)))
               dir)))
    (when write
      (with-temp-file file
        (insert "#+title:      " title "\n#+identifier: " id "\n\nBody.\n")))
    (find-file-noselect file)))

(ert-deftest denote-dash-test/close-all-notes-none-open ()
  "With no Denote note buffers open, the command reports and does nothing."
  (should (equal "No open Denote note buffers" (denote-dash-close-all-notes))))

(ert-deftest denote-dash-test/close-all-notes-kills-unmodified ()
  "Unmodified note buffers are closed without prompting."
  (let ((dir (make-temp-file "denote-dash-test-" t)))
    (unwind-protect
        (let ((buf (denote-dash-test--visit-note dir "20240101T120000" "Note" t)))
          (should (buffer-live-p buf))
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (&rest _) (error "should not prompt for unmodified buffer")))
                    ((symbol-function 'yes-or-no-p)
                     (lambda (&rest _) (error "should not prompt for unmodified buffer"))))
            (denote-dash-close-all-notes))
          (should-not (buffer-live-p buf)))
      (delete-directory dir t))))

(ert-deftest denote-dash-test/close-all-notes-saves-modified-existing ()
  "A modified buffer whose file exists prompts to save, then is closed."
  (let ((dir (make-temp-file "denote-dash-test-" t)))
    (unwind-protect
        (let ((buf (denote-dash-test--visit-note dir "20240101T120000" "Note" t)))
          (with-current-buffer buf
            (goto-char (point-max))
            (insert "extra\n"))
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
            (denote-dash-close-all-notes))
          (should-not (buffer-live-p buf))
          (with-temp-buffer
            (insert-file-contents (expand-file-name "20240101T120000--note.org" dir))
            (should (search-forward "extra" nil t))))
      (delete-directory dir t))))

(ert-deftest denote-dash-test/close-all-notes-missing-file-discard ()
  "A modified buffer whose file is gone is not recreated when the user declines."
  (let ((dir (make-temp-file "denote-dash-test-" t)))
    (unwind-protect
        (let* ((buf (denote-dash-test--visit-note dir "20240101T120000" "Gone" nil))
               (file (buffer-file-name buf)))
          (with-current-buffer buf (insert "unsaved\n"))
          (should-not (file-exists-p file))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
            (denote-dash-close-all-notes))
          (should-not (buffer-live-p buf))
          (should-not (file-exists-p file)))
      (delete-directory dir t))))

(ert-deftest denote-dash-test/close-all-notes-missing-file-save-recreates ()
  "Declining is discard; accepting the prompt recreates the missing file."
  (let ((dir (make-temp-file "denote-dash-test-" t)))
    (unwind-protect
        (let* ((buf (denote-dash-test--visit-note dir "20240101T120000" "Gone" nil))
               (file (buffer-file-name buf)))
          (with-current-buffer buf (insert "recreated\n"))
          (should-not (file-exists-p file))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
            (denote-dash-close-all-notes))
          (should-not (buffer-live-p buf))
          (should (file-exists-p file)))
      (delete-directory dir t))))

(ert-deftest denote-dash-test/close-all-notes-ignores-non-denote ()
  "Buffers not visiting a Denote note are left untouched."
  (let ((dir (make-temp-file "denote-dash-test-" t)))
    (unwind-protect
        (let ((plain (find-file-noselect (expand-file-name "plain.txt" dir))))
          (unwind-protect
              (progn
                (denote-dash-close-all-notes)
                (should (buffer-live-p plain)))
            (with-current-buffer plain (set-buffer-modified-p nil))
            (kill-buffer plain)))
      (delete-directory dir t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-dash--hierarchy-sync-directory

(ert-deftest denote-dash-test/hierarchy-sync-directory-fallback ()
  "With no file property at point, default-directory becomes the first Denote dir."
  (let ((dir (make-temp-file "denote-dash-test-" t)))
    (unwind-protect
        (let ((denote-directory (list dir)))
          (with-temp-buffer
            (insert "no property here\n")
            (goto-char (point-min))
            (denote-dash--hierarchy-sync-directory)
            (should (equal (car (denote-directories)) default-directory))))
      (delete-directory dir t))))

(ert-deftest denote-dash-test/hierarchy-sync-directory-file-at-point ()
  "With a file property at point, default-directory is that file's directory."
  (with-temp-buffer
    (insert (propertize "1a  Title\n"
                        'denote-sequence-hierarchy-file
                        "/some/where/20240101T120000==1a--title.org"))
    (goto-char (point-min))
    (denote-dash--hierarchy-sync-directory)
    (should (equal "/some/where/" default-directory))))

(ert-deftest denote-dash-test/hierarchy-mode-hook-registered ()
  "The directory-setup function is on `denote-sequence-hierarchy-mode-hook'."
  (should (memq #'denote-dash--hierarchy-setup-directory
                denote-sequence-hierarchy-mode-hook)))

(ert-deftest denote-dash-test/hierarchy-fold-hook-registered ()
  "The initial-fold function is on `denote-sequence-hierarchy-mode-hook'."
  (should (memq #'denote-dash--hierarchy-apply-initial-fold
                denote-sequence-hierarchy-mode-hook)))

(ert-deftest denote-dash-test/hierarchy-same-window-hook-registered ()
  "The same-window function is on `denote-sequence-hierarchy-mode-hook'."
  (should (memq #'denote-dash--hierarchy-use-same-window
                denote-sequence-hierarchy-mode-hook)))

(ert-deftest denote-dash-test/hierarchy-use-same-window-sets-local-var ()
  "Running the hook function makes note selection use `find-file'."
  (with-temp-buffer
    (let ((denote-open-link-function #'find-file-other-window))
      (denote-dash--hierarchy-use-same-window)
      (should (eq denote-open-link-function #'find-file))
      (should (local-variable-p 'denote-open-link-function)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-dash--hierarchy-heading-positions / --hierarchy-section-sizes
;;
;; Synthetic tree, matching what `denote-sequence-view-hierarchy' inserts:
;;   1          (singleton root, size 1)
;;   2          (root with 2 children, size 3)
;;     2a
;;     2b
;;   3          (root with 5 descendants, size 6)
;;     3a
;;       3a1
;;       3a2
;;     3b
;;       3b1

(defun denote-dash-test--insert-hierarchy-tree (tree)
  "Insert TREE, a list of (LEVEL . SEQUENCE), as propertized hierarchy lines.
Mirrors the text properties `denote-sequence-view-hierarchy' sets on each
line: `denote-sequence-hierarchy-level' and `denote-sequence-hierarchy-file'
(a fake but Denote-compliant path encoding SEQUENCE)."
  (dolist (entry tree)
    (let* ((level (car entry))
           (seq (cdr entry))
           (file (format "/tmp/x/20240101T100000==%s--note.org" seq)))
      (insert (propertize (format "%s\n" seq)
                          'denote-sequence-hierarchy-level level
                          'denote-sequence-hierarchy-file file)))))

(defconst denote-dash-test--hierarchy-tree
  '((1 . "1")
    (1 . "2") (2 . "2a") (2 . "2b")
    (1 . "3") (2 . "3a") (3 . "3a1") (3 . "3a2") (2 . "3b") (3 . "3b1"))
  "Synthetic hierarchy tree shared by the fold tests.")

(defun denote-dash-test--hierarchy-point-for (seq)
  "Return the buffer position of the line for SEQ in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (let (found)
      (while (and (not found) (not (eobp)))
        (when (equal seq (denote-retrieve-filename-signature
                           (get-text-property (point) 'denote-sequence-hierarchy-file)))
          (setq found (point)))
        (forward-line 1))
      found)))

(ert-deftest denote-dash-test/hierarchy-heading-positions-collects-all ()
  "Every inserted line is collected, in order, with its level and sequence."
  (with-temp-buffer
    (denote-dash-test--insert-hierarchy-tree denote-dash-test--hierarchy-tree)
    (let ((headings (denote-dash--hierarchy-heading-positions)))
      (should (= (length denote-dash-test--hierarchy-tree) (length headings)))
      (should (equal (mapcar #'cdr denote-dash-test--hierarchy-tree)
                     (mapcar (lambda (h) (nth 2 h)) headings))))))

(ert-deftest denote-dash-test/hierarchy-section-sizes-singleton ()
  "A root with no children has section size 1."
  (with-temp-buffer
    (denote-dash-test--insert-hierarchy-tree denote-dash-test--hierarchy-tree)
    (let* ((headings (denote-dash--hierarchy-heading-positions))
           (sizes (denote-dash--hierarchy-section-sizes headings))
           (pos (denote-dash-test--hierarchy-point-for "1")))
      (should (= 1 (cdr (assq pos sizes)))))))

(ert-deftest denote-dash-test/hierarchy-section-sizes-two-children ()
  "A root with 2 direct children has section size 3."
  (with-temp-buffer
    (denote-dash-test--insert-hierarchy-tree denote-dash-test--hierarchy-tree)
    (let* ((headings (denote-dash--hierarchy-heading-positions))
           (sizes (denote-dash--hierarchy-section-sizes headings))
           (pos (denote-dash-test--hierarchy-point-for "2")))
      (should (= 3 (cdr (assq pos sizes)))))))

(ert-deftest denote-dash-test/hierarchy-section-sizes-nested-descendants ()
  "A root with 5 nested descendants has section size 6."
  (with-temp-buffer
    (denote-dash-test--insert-hierarchy-tree denote-dash-test--hierarchy-tree)
    (let* ((headings (denote-dash--hierarchy-heading-positions))
           (sizes (denote-dash--hierarchy-section-sizes headings))
           (pos (denote-dash-test--hierarchy-point-for "3")))
      (should (= 6 (cdr (assq pos sizes)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-dash--hierarchy-apply-initial-fold

(defun denote-dash-test--hierarchy-level-at-point ()
  "`outline-level' function for the synthetic hierarchy test buffers."
  (get-text-property (point) 'denote-sequence-hierarchy-level))

(defmacro denote-dash-test--with-hierarchy-buffer (&rest body)
  "Run BODY in a temp buffer populated like a real hierarchy view, with
`outline-minor-mode' configured the same way `denote-sequence-hierarchy-mode' does."
  `(with-temp-buffer
     (denote-dash-test--insert-hierarchy-tree denote-dash-test--hierarchy-tree)
     (setq-local outline-regexp "[\s[:alnum:]]+")
     (setq-local outline-level #'denote-dash-test--hierarchy-level-at-point)
     (outline-minor-mode 1)
     ,@body))

(ert-deftest denote-dash-test/hierarchy-apply-fold-depth ()
  "A depth of 1 folds every root's children, but not the roots themselves."
  (denote-dash-test--with-hierarchy-buffer
   (let ((denote-dash-hierarchy-initial-fold-depth 1)
         (denote-dash-hierarchy-fold-sequences nil)
         (denote-dash-hierarchy-auto-fold-min-size nil)
         (denote-dash-hierarchy-auto-fold-max-size nil))
     (denote-dash--hierarchy-apply-initial-fold)
     (should-not (outline-invisible-p (denote-dash-test--hierarchy-point-for "1")))
     (should-not (outline-invisible-p (denote-dash-test--hierarchy-point-for "2")))
     (should (outline-invisible-p (denote-dash-test--hierarchy-point-for "2a"))))))

(ert-deftest denote-dash-test/hierarchy-apply-fold-explicit-sequences ()
  "Only the listed sequence's subtree folds; unrelated roots stay expanded."
  (denote-dash-test--with-hierarchy-buffer
   (let ((denote-dash-hierarchy-initial-fold-depth nil)
         (denote-dash-hierarchy-fold-sequences '("3"))
         (denote-dash-hierarchy-auto-fold-min-size nil)
         (denote-dash-hierarchy-auto-fold-max-size nil))
     (denote-dash--hierarchy-apply-initial-fold)
     (should (outline-invisible-p (denote-dash-test--hierarchy-point-for "3a")))
     (should-not (outline-invisible-p (denote-dash-test--hierarchy-point-for "2a"))))))

(ert-deftest denote-dash-test/hierarchy-apply-fold-min-size ()
  "Sections at or below the min-size threshold fold; larger ones don't."
  (denote-dash-test--with-hierarchy-buffer
   (let ((denote-dash-hierarchy-initial-fold-depth nil)
         (denote-dash-hierarchy-fold-sequences nil)
         (denote-dash-hierarchy-auto-fold-min-size 1)
         (denote-dash-hierarchy-auto-fold-max-size nil))
     (denote-dash--hierarchy-apply-initial-fold)
     ;; "1" has size 1 (<= 1): nothing under it to hide, but it must not error.
     (should-not (outline-invisible-p (denote-dash-test--hierarchy-point-for "1")))
     ;; "2" has size 3 (> 1): stays expanded.
     (should-not (outline-invisible-p (denote-dash-test--hierarchy-point-for "2a"))))))

(ert-deftest denote-dash-test/hierarchy-apply-fold-max-size ()
  "Sections larger than the max-size threshold fold; smaller ones don't."
  (denote-dash-test--with-hierarchy-buffer
   (let ((denote-dash-hierarchy-initial-fold-depth nil)
         (denote-dash-hierarchy-fold-sequences nil)
         (denote-dash-hierarchy-auto-fold-min-size nil)
         (denote-dash-hierarchy-auto-fold-max-size 3))
     (denote-dash--hierarchy-apply-initial-fold)
     ;; "3" has size 6 (> 3): folds.
     (should (outline-invisible-p (denote-dash-test--hierarchy-point-for "3a")))
     ;; "2" has size 3 (not > 3): stays expanded.
     (should-not (outline-invisible-p (denote-dash-test--hierarchy-point-for "2a"))))))

(ert-deftest denote-dash-test/hierarchy-apply-fold-composes-rules ()
  "An explicit-list fold and a max-size fold both apply when both are set."
  (denote-dash-test--with-hierarchy-buffer
   (let ((denote-dash-hierarchy-initial-fold-depth nil)
         (denote-dash-hierarchy-fold-sequences '("2"))
         (denote-dash-hierarchy-auto-fold-min-size nil)
         (denote-dash-hierarchy-auto-fold-max-size 3))
     (denote-dash--hierarchy-apply-initial-fold)
     (should (outline-invisible-p (denote-dash-test--hierarchy-point-for "2a")))
     (should (outline-invisible-p (denote-dash-test--hierarchy-point-for "3a"))))))

(ert-deftest denote-dash-test/hierarchy-apply-fold-noop-when-unconfigured ()
  "With every option nil, nothing folds."
  (denote-dash-test--with-hierarchy-buffer
   (let ((denote-dash-hierarchy-initial-fold-depth nil)
         (denote-dash-hierarchy-fold-sequences nil)
         (denote-dash-hierarchy-auto-fold-min-size nil)
         (denote-dash-hierarchy-auto-fold-max-size nil))
     (denote-dash--hierarchy-apply-initial-fold)
     (should-not (outline-invisible-p (denote-dash-test--hierarchy-point-for "2a")))
     (should-not (outline-invisible-p (denote-dash-test--hierarchy-point-for "3a1"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-dash--hierarchy-goto-root / denote-dash-hierarchy-toggle-fold-sequence

(ert-deftest denote-dash-test/hierarchy-goto-root-from-descendant ()
  "Point moves to the top-level heading enclosing a deeply nested descendant."
  (denote-dash-test--with-hierarchy-buffer
   (goto-char (denote-dash-test--hierarchy-point-for "3a1"))
   (denote-dash--hierarchy-goto-root)
   (should (= (point) (denote-dash-test--hierarchy-point-for "3")))))

(ert-deftest denote-dash-test/hierarchy-goto-root-already-at-root ()
  "Point already on a root heading does not move."
  (denote-dash-test--with-hierarchy-buffer
   (goto-char (denote-dash-test--hierarchy-point-for "2"))
   (denote-dash--hierarchy-goto-root)
   (should (= (point) (denote-dash-test--hierarchy-point-for "2")))))

(ert-deftest denote-dash-test/hierarchy-toggle-fold-sequence-adds-and-folds ()
  "Toggling from an unfolded descendant marks its root and hides the subtree."
  (denote-dash-test--with-hierarchy-buffer
   (let ((denote-dash-hierarchy-fold-sequences nil))
     (goto-char (denote-dash-test--hierarchy-point-for "3a"))
     (denote-dash-hierarchy-toggle-fold-sequence)
     (should (equal '("3") denote-dash-hierarchy-fold-sequences))
     (should (outline-invisible-p (denote-dash-test--hierarchy-point-for "3a"))))))

(ert-deftest denote-dash-test/hierarchy-toggle-fold-sequence-removes-and-unfolds ()
  "Toggling an already-marked root unmarks it and shows the subtree again."
  (denote-dash-test--with-hierarchy-buffer
   (let ((denote-dash-hierarchy-fold-sequences '("3")))
     (goto-char (denote-dash-test--hierarchy-point-for "3"))
     (outline-hide-subtree)
     (denote-dash-hierarchy-toggle-fold-sequence)
     (should-not denote-dash-hierarchy-fold-sequences)
     (should-not (outline-invisible-p (denote-dash-test--hierarchy-point-for "3a"))))))

(ert-deftest denote-dash-test/hierarchy-clear-fold-sequences ()
  "Clearing empties the persisted fold list."
  (let ((denote-dash-hierarchy-fold-sequences '("1" "2a" "3")))
    (denote-dash-hierarchy-clear-fold-sequences)
    (should-not denote-dash-hierarchy-fold-sequences)))

(ert-deftest denote-dash-test/hierarchy-fold-sequences-persisted-via-savehist ()
  "The fold-sequences variable is registered for `savehist-mode' persistence."
  (require 'savehist)
  (should (memq 'denote-dash-hierarchy-fold-sequences savehist-additional-variables)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-dash--hierarchy-remap-fold-sequence-prefix / --swap-fold-sequence*

(ert-deftest denote-dash-test/hierarchy-remap-fold-sequence-prefix-exact ()
  "An exact-match entry is rewritten to the new sequence."
  (let ((denote-dash-hierarchy-fold-sequences '("8a" "9b")))
    (denote-dash--hierarchy-remap-fold-sequence-prefix "8a" "3c")
    (should (equal '("3c" "9b") denote-dash-hierarchy-fold-sequences))))

(ert-deftest denote-dash-test/hierarchy-remap-fold-sequence-prefix-descendant ()
  "A folded descendant of the renamed subtree keeps its relative suffix."
  (let ((denote-dash-hierarchy-fold-sequences '("8a1")))
    (denote-dash--hierarchy-remap-fold-sequence-prefix "8a" "3c")
    (should (equal '("3c1") denote-dash-hierarchy-fold-sequences))))

(ert-deftest denote-dash-test/hierarchy-remap-fold-sequence-prefix-noop-unrelated ()
  "Entries outside the renamed prefix are left alone."
  (let ((denote-dash-hierarchy-fold-sequences '("9b")))
    (denote-dash--hierarchy-remap-fold-sequence-prefix "8a" "3c")
    (should (equal '("9b") denote-dash-hierarchy-fold-sequences))))

(ert-deftest denote-dash-test/hierarchy-swap-fold-sequence-exact ()
  "Two single entries trade places without a sequential-application clash."
  (let ((denote-dash-hierarchy-fold-sequences '("8a" "8")))
    (denote-dash--hierarchy-swap-fold-sequence "8a" "8")
    (should (equal '("8" "8a") denote-dash-hierarchy-fold-sequences))))

(ert-deftest denote-dash-test/hierarchy-swap-fold-sequence-prefix-swaps-subtrees ()
  "Descendant entries under either sibling swap prefixes together."
  (let ((denote-dash-hierarchy-fold-sequences '("8a1" "8b2")))
    (denote-dash--hierarchy-swap-fold-sequence-prefix "8a" "8b")
    (should (equal '("8b1" "8a2") denote-dash-hierarchy-fold-sequences))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-dash--review-pending-p

(defun denote-dash-test--make-org-note-with-reviewdate (dir id reviewdate)
  "Create a minimal org Denote note in DIR and return its path.
ID is the timestamp string.  REVIEWDATE, when non-nil, is an ISO 8601
date string (\"YYYY-MM-DD\") written as the note's reviewdate frontmatter."
  (let ((file (expand-file-name (concat id "--test.org") dir)))
    (with-temp-file file
      (insert "#+title:      Test\n")
      (insert "#+identifier: " id "\n")
      (when reviewdate (insert "#+reviewdate: [" reviewdate "]\n"))
      (insert "\nBody.\n"))
    file))

(ert-deftest denote-dash-test/review-pending-no-reviewdate ()
  "A note with no reviewdate at all is always pending."
  (let ((dir (make-temp-file "denote-dash-test-" t)))
    (unwind-protect
        (let ((file (denote-dash-test--make-org-note-with-reviewdate
                     dir "20240101T120000" nil)))
          (should (denote-dash--review-pending-p file)))
      (delete-directory dir t))))

(ert-deftest denote-dash-test/review-pending-recent-reviewdate ()
  "A note reviewed today is not pending."
  (let ((dir (make-temp-file "denote-dash-test-" t)))
    (unwind-protect
        (let ((file (denote-dash-test--make-org-note-with-reviewdate
                     dir "20240101T120000" (format-time-string "%F"))))
          (should-not (denote-dash--review-pending-p file)))
      (delete-directory dir t))))

(ert-deftest denote-dash-test/review-pending-stale-reviewdate ()
  "A note reviewed long before the interval is pending again."
  (let ((dir (make-temp-file "denote-dash-test-" t))
        (denote-dash-review-interval-days 90))
    (unwind-protect
        (let ((file (denote-dash-test--make-org-note-with-reviewdate
                     dir "20240101T120000" "2000-01-01")))
          (should (denote-dash--review-pending-p file)))
      (delete-directory dir t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Review indicator functions

(ert-deftest denote-dash-test/review-indicator-icon ()
  "The icon indicator shows a filled circle when pending, blank otherwise."
  (should (equal "●" (denote-dash-review-indicator-icon t)))
  (should (equal " " (denote-dash-review-indicator-icon nil))))

(ert-deftest denote-dash-test/review-indicator-text ()
  "The text indicator shows \"due\" when pending, blank otherwise."
  (should (equal "due" (denote-dash-review-indicator-text t)))
  (should (equal "" (denote-dash-review-indicator-text nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transient key-collision regressions
;;
;; A transient's key strings must be unique and no single-char binding may
;; shadow a multi-char one, else the shadowed suffix is silently unreachable.

(ert-deftest denote-dash-test/dispatch-transient-no-key-collisions ()
  "`denote-dash-dispatch' has no duplicate keys.
Prefix-shadowing is checked separately, scoped to the narrow/retag keys
this test suite added: the dispatch has one pre-existing, intentional
prefix overlap between the bare \"c\" (Columns, visible only in
`denote-dash-mode') and \"cd\"/\"cm\" (Convert, visible only in
`markdown-mode') — safe because the two groups are gated by mutually
exclusive major modes and can never be simultaneously reachable."
  (let ((keys (transient-test/collect-keys 'denote-dash-dispatch)))
    (should-not (transient-test/duplicate-keys keys))))

(ert-deftest denote-dash-test/dispatch-transient-new-keys-no-collisions ()
  "The narrow (`w*') and retag (`ak') keys don't shadow or get shadowed."
  (let* ((keys (transient-test/collect-keys 'denote-dash-dispatch))
         (new-keys '("ws" "wt" "ww" "wk" "ak"))
         (conflicts (transient-test/key-prefix-conflicts keys)))
    (dolist (nk new-keys)
      (should (= 1 (seq-count (lambda (k) (equal k nk)) keys))))
    (should-not (seq-filter (lambda (c) (or (member (nth 0 c) new-keys)
                                            (member (nth 1 c) new-keys)))
                            conflicts))))

(ert-deftest denote-dash-test/column-transient-no-key-collisions ()
  "`denote-dash-column-transient' has no duplicate keys or key/prefix shadowing."
  (let ((keys (transient-test/collect-keys 'denote-dash-column-transient)))
    (should-not (transient-test/duplicate-keys keys))
    (should-not (transient-test/key-prefix-conflicts keys))))

(provide 'test-denote-dash)
;;; test-denote-dash.el ends here
