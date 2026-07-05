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
;; denote-dash--signature-line

(ert-deftest denote-dash-test/signature-line-org ()
  "Org signature line uses #+signature: prefix, no quotes."
  (should (equal "#+signature: 1a" (denote-dash--signature-line "1a" 'org))))

(ert-deftest denote-dash-test/signature-line-text ()
  "Text signature line matches org format, no quotes."
  (should (equal "#+signature: 1a" (denote-dash--signature-line "1a" 'text))))

(ert-deftest denote-dash-test/signature-line-markdown-yaml ()
  "Markdown YAML signature line wraps the value in quotes."
  (should (equal "signature: \"1a\"" (denote-dash--signature-line "1a" 'markdown-yaml))))

(ert-deftest denote-dash-test/signature-line-markdown-toml ()
  "Markdown TOML signature line uses = and wraps the value in quotes."
  (should (equal "signature = \"1a\"" (denote-dash--signature-line "1a" 'markdown-toml))))

(ert-deftest denote-dash-test/signature-line-unknown-type ()
  "Unknown file type falls back to org format without quotes."
  (should (equal "#+signature: 2b" (denote-dash--signature-line "2b" 'unknown))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-dash--sequence-aligned-p (uses temp files)

(defun denote-dash-test--make-org-note (dir id sig title)
  "Create a minimal org Denote note in DIR and return its path.
ID is the timestamp string, SIG the sequence or nil, TITLE the note title."
  (let* ((name (concat id
                       (when sig (concat "==" sig))
                       "--" (downcase (replace-regexp-in-string " " "-" title))
                       ".org"))
         (file (expand-file-name name dir)))
    (with-temp-file file
      (insert "#+title:      " title "\n")
      (insert "#+date:       [2024-01-01 Mon]\n")
      (insert "#+identifier: " id "\n")
      (when sig (insert "#+signature: " sig "\n"))
      (insert "#+filetags:   :test:\n\n")
      (insert "Body text.\n"))
    file))

(ert-deftest denote-dash-test/aligned-p-both-match ()
  "File is aligned when filename and frontmatter signatures match."
  (let ((dir (make-temp-file "denote-dash-test-" t)))
    (unwind-protect
        (let* ((file (denote-dash-test--make-org-note
                      dir "20240101T120000" "1a" "Test Note"))
               (denote-directory (list dir)))
          (should (denote-dash--sequence-aligned-p file)))
      (delete-directory dir t))))

(ert-deftest denote-dash-test/aligned-p-both-nil ()
  "File with no sequence in filename or frontmatter is aligned."
  (let ((dir (make-temp-file "denote-dash-test-" t)))
    (unwind-protect
        (let* ((file (denote-dash-test--make-org-note
                      dir "20240101T120000" nil "Plain Note"))
               (denote-directory (list dir)))
          (should (denote-dash--sequence-aligned-p file)))
      (delete-directory dir t))))

(ert-deftest denote-dash-test/aligned-p-sig-missing-from-frontmatter ()
  "File is not aligned when filename has sig but frontmatter does not."
  (let ((dir (make-temp-file "denote-dash-test-" t)))
    (unwind-protect
        (let* ((file (denote-dash-test--make-org-note
                      dir "20240101T120000" nil "Plain Note"))
               ;; Manually rename to add ==1a without updating frontmatter
               (new-name (expand-file-name
                          "20240101T120000==1a--plain-note.org" dir))
               (_ (rename-file file new-name))
               (denote-directory (list dir)))
          (should-not (denote-dash--sequence-aligned-p new-name)))
      (delete-directory dir t))))

(provide 'test-denote-dash)
;;; test-denote-dash.el ends here
