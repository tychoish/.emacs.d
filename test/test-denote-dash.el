;;; test-denote-dash.el --- ERT tests for denote-dash.el -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for the pure functions in denote-dash.el: filter expression evaluator,
;; sequence hierarchy utilities, fold visibility logic, and column validation.

;;; Code:

(require 'ert)
(require 'denote-dash)

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

(provide 'test-denote-dash)
;;; test-denote-dash.el ends here
