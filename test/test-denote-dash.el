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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-dash--sequence-parent

(ert-deftest denote-dash-test/sequence-parent-root ()
  "Root sequences (depth 0) have no parent."
  (should-not (denote-dash--sequence-parent "1"))
  (should-not (denote-dash--sequence-parent "3"))
  (should-not (denote-dash--sequence-parent nil))
  (should-not (denote-dash--sequence-parent "")))

(ert-deftest denote-dash-test/sequence-parent-depth-1 ()
  "Depth-1 sequences return the root number as parent."
  (should (equal "1"  (denote-dash--sequence-parent "1a")))
  (should (equal "1"  (denote-dash--sequence-parent "1b")))
  (should (equal "3"  (denote-dash--sequence-parent "3b"))))

(ert-deftest denote-dash-test/sequence-parent-depth-2 ()
  "Depth-2 sequences return the depth-1 letter sequence as parent."
  (should (equal "1a"  (denote-dash--sequence-parent "1a1")))
  (should (equal "3a"  (denote-dash--sequence-parent "3a1")))
  (should (equal "3b"  (denote-dash--sequence-parent "3b9"))))

(ert-deftest denote-dash-test/sequence-parent-depth-3 ()
  "Depth-3 sequences return the depth-2 numeric sequence as parent."
  (should (equal "1a1"  (denote-dash--sequence-parent "1a1a")))
  (should (equal "3a1"  (denote-dash--sequence-parent "3a1b")))
  (should (equal "3a1"  (denote-dash--sequence-parent "3a1o"))))

(ert-deftest denote-dash-test/sequence-parent-multi-digit ()
  "Multi-digit segments are handled: parent of 3a12 is 3a."
  (should (equal "3a" (denote-dash--sequence-parent "3a12"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-dash-swap-with-parent (file-based)

(defun denote-dash-test--kill-dir-buffers (dir)
  "Kill all buffers whose file is under DIR.
Clears the modified flag on each buffer first: `kill-buffer' unconditionally
calls `kill-buffer--possibly-save' (via `read-multiple-choice') for a
modified file-visiting buffer regardless of `kill-buffer-query-functions',
so a freshly `denote'-created note that was never saved would otherwise
block on a \"Buffer modified; kill anyway?\" prompt."
  (seq-do (lambda (buf)
            (when-let* ((f (buffer-file-name buf)))
              (when (string-prefix-p (expand-file-name dir) (expand-file-name f))
                (with-current-buffer buf (set-buffer-modified-p nil))
                (kill-buffer buf))))
          (buffer-list)))

(ert-deftest denote-dash-test/swap-with-parent-renames-files ()
  "Swap exchanges the ==SEQ== component in both filenames."
  (let ((dir (make-temp-file "denote-dash-test-" t)))
    (unwind-protect
        (let* ((parent-file (denote-dash-test--make-org-note
                             dir "20240101T100000" "1a" "Parent"))
               (child-file  (denote-dash-test--make-org-note
                             dir "20240101T110000" "1a1" "Child"))
               (denote-directory (list dir)))
          (with-current-buffer (find-file-noselect child-file)
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
              (denote-dash-swap-with-parent)))
          ;; Kill any buffers the swap opened in the temp dir
          (denote-dash-test--kill-dir-buffers dir)
          ;; The file whose timestamp was 110000 now carries sequence 1a
          (should (directory-files dir nil "20240101T110000==1a--"))
          ;; The file whose timestamp was 100000 now carries sequence 1a1
          (should (directory-files dir nil "20240101T100000==1a1--")))
      (denote-dash-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-dash-test/swap-with-parent-updates-frontmatter ()
  "Swap updates #+signature: in both files to match the new filename."
  (let ((dir (make-temp-file "denote-dash-test-" t)))
    (unwind-protect
        (let* ((parent-file (denote-dash-test--make-org-note
                             dir "20240101T100000" "1a" "Parent"))
               (child-file  (denote-dash-test--make-org-note
                             dir "20240101T110000" "1a1" "Child"))
               (denote-directory (list dir)))
          (with-current-buffer (find-file-noselect child-file)
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
              (denote-dash-swap-with-parent)))
          (denote-dash-test--kill-dir-buffers dir)
          ;; Check frontmatter of the swapped child (now at 1a)
          (let ((new-child (car (directory-files dir t "20240101T110000==1a--"))))
            (should new-child)
            (with-temp-buffer
              (insert-file-contents new-child)
              (should (search-forward "#+signature: 1a" nil t))))
          ;; Check frontmatter of the demoted parent (now at 1a1)
          (let ((new-parent (car (directory-files dir t "20240101T100000==1a1--"))))
            (should new-parent)
            (with-temp-buffer
              (insert-file-contents new-parent)
              (should (search-forward "#+signature: 1a1" nil t)))))
      (denote-dash-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-dash-test/swap-with-parent-no-stale-buffers ()
  "After swap, no buffer visits a path that no longer exists."
  (let ((dir (make-temp-file "denote-dash-test-" t)))
    (unwind-protect
        (let* ((parent-file (denote-dash-test--make-org-note
                             dir "20240101T100000" "1a" "Parent"))
               (child-file  (denote-dash-test--make-org-note
                             dir "20240101T110000" "1a1" "Child"))
               (denote-directory (list dir)))
          ;; Open both files first so buffers exist before the swap
          (find-file-noselect parent-file)
          (with-current-buffer (find-file-noselect child-file)
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
              (denote-dash-swap-with-parent)))
          (denote-dash-test--kill-dir-buffers dir)
          ;; No buffer should point to the old paths (they no longer exist)
          (should-not (find-buffer-visiting parent-file))
          (should-not (find-buffer-visiting child-file)))
      (denote-dash-test--kill-dir-buffers dir)
      (delete-directory dir t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-dash-swap-with-previous / denote-dash-swap-with-next (file-based)

(defun denote-dash-test--make-tree (dir)
  "Create a two-branch sequence tree in DIR, return a plist of its files.
Branch one is 1/1a/1a1, branch two is 2/2a; 1 and 2 are root siblings."
  (list :root1       (denote-dash-test--make-org-note dir "20240101T100000" "1"   "Root One")
        :child1      (denote-dash-test--make-org-note dir "20240101T110000" "1a"  "Child One")
        :grandchild1 (denote-dash-test--make-org-note dir "20240101T120000" "1a1" "Grandchild One")
        :root2       (denote-dash-test--make-org-note dir "20240101T130000" "2"   "Root Two")
        :child2      (denote-dash-test--make-org-note dir "20240101T140000" "2a"  "Child Two")))

(ert-deftest denote-dash-test/swap-with-next-moves-subtree-recursively ()
  "Swapping root 1 with sibling 2 renumbers each whole subtree, not just the root."
  (let ((dir (make-temp-file "denote-dash-test-" t)))
    (unwind-protect
        (let* ((tree (denote-dash-test--make-tree dir))
               (denote-directory (list dir)))
          (with-current-buffer (find-file-noselect (plist-get tree :root1))
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
              (denote-dash-swap-with-next)))
          (denote-dash-test--kill-dir-buffers dir)
          (should (directory-files dir nil "20240101T100000==2--"))
          (should (directory-files dir nil "20240101T110000==2a--"))
          (should (directory-files dir nil "20240101T120000==2a1--"))
          (should (directory-files dir nil "20240101T130000==1--"))
          (should (directory-files dir nil "20240101T140000==1a--")))
      (denote-dash-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-dash-test/swap-with-previous-moves-subtree-recursively ()
  "Swapping root 2 with previous sibling 1 gives the same result as swap-with-next."
  (let ((dir (make-temp-file "denote-dash-test-" t)))
    (unwind-protect
        (let* ((tree (denote-dash-test--make-tree dir))
               (denote-directory (list dir)))
          (with-current-buffer (find-file-noselect (plist-get tree :root2))
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
              (denote-dash-swap-with-previous)))
          (denote-dash-test--kill-dir-buffers dir)
          (should (directory-files dir nil "20240101T100000==2--"))
          (should (directory-files dir nil "20240101T110000==2a--"))
          (should (directory-files dir nil "20240101T120000==2a1--"))
          (should (directory-files dir nil "20240101T130000==1--"))
          (should (directory-files dir nil "20240101T140000==1a--")))
      (denote-dash-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-dash-test/swap-with-next-updates-frontmatter-recursively ()
  "Frontmatter signature is fixed on descendants too, not just the swapped roots."
  (let ((dir (make-temp-file "denote-dash-test-" t)))
    (unwind-protect
        (let* ((tree (denote-dash-test--make-tree dir))
               (denote-directory (list dir)))
          (with-current-buffer (find-file-noselect (plist-get tree :root1))
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
              (denote-dash-swap-with-next)))
          (denote-dash-test--kill-dir-buffers dir)
          (let ((grandchild (car (directory-files dir t "20240101T120000==2a1--"))))
            (should grandchild)
            (with-temp-buffer
              (insert-file-contents grandchild)
              (should (search-forward "#+signature: 2a1" nil t)))))
      (denote-dash-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-dash-test/swap-with-previous-errors-when-first ()
  "The first sibling has no previous sibling to swap with."
  (let ((dir (make-temp-file "denote-dash-test-" t)))
    (unwind-protect
        (let* ((tree (denote-dash-test--make-tree dir))
               (denote-directory (list dir)))
          (with-current-buffer (find-file-noselect (plist-get tree :root1))
            (should-error (denote-dash-swap-with-previous) :type 'user-error)))
      (denote-dash-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-dash-test/swap-with-next-errors-when-last ()
  "The last sibling has no next sibling to swap with."
  (let ((dir (make-temp-file "denote-dash-test-" t)))
    (unwind-protect
        (let* ((tree (denote-dash-test--make-tree dir))
               (denote-directory (list dir)))
          (with-current-buffer (find-file-noselect (plist-get tree :root2))
            (should-error (denote-dash-swap-with-next) :type 'user-error)))
      (denote-dash-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-dash-test/swap-with-next-no-stale-buffers ()
  "After a subtree swap, no buffer visits a path that no longer exists."
  (let ((dir (make-temp-file "denote-dash-test-" t)))
    (unwind-protect
        (let* ((tree (denote-dash-test--make-tree dir))
               (denote-directory (list dir)))
          (find-file-noselect (plist-get tree :grandchild1))
          (find-file-noselect (plist-get tree :child2))
          (with-current-buffer (find-file-noselect (plist-get tree :root1))
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
              (denote-dash-swap-with-next)))
          (denote-dash-test--kill-dir-buffers dir)
          (should-not (find-buffer-visiting (plist-get tree :root1)))
          (should-not (find-buffer-visiting (plist-get tree :grandchild1)))
          (should-not (find-buffer-visiting (plist-get tree :child2))))
      (denote-dash-test--kill-dir-buffers dir)
      (delete-directory dir t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-dash-reparent-recursive

(ert-deftest denote-dash-test/reparent-recursive-fixes-type-alternation ()
  "Reparent recursive rewrites suffix types when old and new roots differ.
Old root \"1a1\" ends digit (letter-children); target \"2\" ends digit so
its first child is \"2a\" which ends letter (digit-children).  The child
\"1a1a\" (letter suffix \"a\") must become \"2a1\" (digit suffix \"1\"),
not \"2aa\"."
  (let ((dir (make-temp-file "denote-dash-test-" t)))
    (unwind-protect
        (let* ((denote-sequence-scheme 'alphanumeric)
               (_root   (denote-dash-test--make-org-note dir "20240101T100000" "1a1"  "Root"))
               (_child  (denote-dash-test--make-org-note dir "20240101T110000" "1a1a" "Child"))
               (_target (denote-dash-test--make-org-note dir "20240101T120000" "2"    "Target"))
               (denote-directory (list dir))
               (root-file (car (directory-files dir t "20240101T100000=="))))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
            (denote-dash-reparent-recursive root-file _target))
          (denote-dash-test--kill-dir-buffers dir)
          ;; "1a1" → first letter child of "2" → "2a"
          (should (directory-files dir nil "20240101T100000==2a--"))
          ;; "1a1a": suffix "a" (letter) in old context (digit-ending "1a1"),
          ;; rewritten to digit "1" in new context (letter-ending "2a")
          (should (directory-files dir nil "20240101T110000==2a1--")))
      (denote-dash-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-dash-test/reparent-recursive-same-type-no-change ()
  "When old and new roots end in the same type, suffix characters are preserved."
  (let ((dir (make-temp-file "denote-dash-test-" t)))
    (unwind-protect
        (let* ((denote-sequence-scheme 'alphanumeric)
               (_root   (denote-dash-test--make-org-note dir "20240101T100000" "1a"  "Root"))
               (_child  (denote-dash-test--make-org-note dir "20240101T110000" "1a1" "Child"))
               (_target (denote-dash-test--make-org-note dir "20240101T120000" "2a"  "Target"))
               (denote-directory (list dir))
               (root-file (car (directory-files dir t "20240101T100000=="))))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
            (denote-dash-reparent-recursive root-file _target))
          (denote-dash-test--kill-dir-buffers dir)
          ;; "1a" (ends letter) → first digit child of "2a" (ends letter) → "2a1"
          (should (directory-files dir nil "20240101T100000==2a1--"))
          ;; "1a1": suffix "1" (digit) in old context (letter-ending "1a"),
          ;; new root "2a1" ends digit → children are letters → "a"
          (should (directory-files dir nil "20240101T110000==2a1a--")))
      (denote-dash-test--kill-dir-buffers dir)
      (delete-directory dir t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-dash-insert-sequence-note

(ert-deftest denote-dash-test/insert-sequence-note-shifts-siblings-without-prompting ()
  "Shifting siblings does not block on a per-file `y-or-n-p' confirmation.
Regression test: `denote-rename-file' prompts per-file under its default
`denote-rename-confirmations'; the shift loop must suppress that so a
single top-level `yes-or-no-p' confirmation is enough."
  (let ((dir (make-temp-file "denote-dash-test-" t)))
    (unwind-protect
        (let* ((_f1 (denote-dash-test--make-org-note dir "20240101T100000" "1" "One"))
               (_f2 (denote-dash-test--make-org-note dir "20240101T110000" "2" "Two"))
               (denote-directory (list dir))
               (file (car (directory-files dir t "20240101T100000=="))))
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (&rest _) (error "should not prompt per-file")))
                    ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                    ((symbol-function 'read-string) (lambda (&rest _) "New"))
                    ((symbol-function 'denote-keywords-prompt) (lambda (&rest _) nil))
                    ((symbol-function 'denote-dash--target-file) (lambda () file)))
            (denote-dash-insert-sequence-note))
          ;; the new note took the vacated "1" position; `denote' leaves it
          ;; in an unsaved buffer, so check the buffer rather than the disk
          (should (seq-find (lambda (buf)
                              (when-let* ((f (buffer-file-name buf)))
                                (string-match-p "==1--" f)))
                            (buffer-list)))
          (denote-dash-test--kill-dir-buffers dir)
          ;; "One" (was "1") shifted forward to "2"
          (should (directory-files dir nil "20240101T100000==2--"))
          ;; "Two" (was "2") shifted forward to "3"
          (should (directory-files dir nil "20240101T110000==3--")))
      (denote-dash-test--kill-dir-buffers dir)
      (delete-directory dir t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-dash-repack-sequence-children

(ert-deftest denote-dash-test/repack-root-uses-numbers ()
  "Root-level repack assigns numeric sequences, not letters."
  (let ((dir (make-temp-file "denote-dash-test-" t)))
    (unwind-protect
        (let* ((_f1 (denote-dash-test--make-org-note dir "20240101T100000" "1" "One"))
               (_f3 (denote-dash-test--make-org-note dir "20240101T110000" "3" "Three"))
               (denote-directory (list dir)))
          (denote-dash-repack-sequence-children "")
          (denote-dash-test--kill-dir-buffers dir)
          ;; "1" stays "1", "3" compacts to "2"
          (should (directory-files dir nil "20240101T100000==1--"))
          (should (directory-files dir nil "20240101T110000==2--")))
      (denote-dash-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-dash-test/repack-letter-children-compact-gaps ()
  "Children of a digit-ending prefix use letters; gaps are compacted."
  (let ((dir (make-temp-file "denote-dash-test-" t)))
    (unwind-protect
        (let* ((_fp  (denote-dash-test--make-org-note dir "20240101T090000" "3a6" "Parent"))
               (_fk  (denote-dash-test--make-org-note dir "20240101T100000" "3a6k" "First"))
               (_fl  (denote-dash-test--make-org-note dir "20240101T110000" "3a6l" "Second"))
               (denote-directory (list dir)))
          (denote-dash-repack-sequence-children "3a6")
          (denote-dash-test--kill-dir-buffers dir)
          ;; k (11th letter) → a, l (12th letter) → b
          (should (directory-files dir nil "20240101T100000==3a6a--"))
          (should (directory-files dir nil "20240101T110000==3a6b--")))
      (denote-dash-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-dash-test/repack-propagates-to-subtree ()
  "Repacking a child also renames that child's descendants."
  (let ((dir (make-temp-file "denote-dash-test-" t)))
    (unwind-protect
        (let* ((_fp   (denote-dash-test--make-org-note dir "20240101T090000" "3a6"   "Parent"))
               (_fk   (denote-dash-test--make-org-note dir "20240101T100000" "3a6k"  "Child"))
               (_fk1  (denote-dash-test--make-org-note dir "20240101T110000" "3a6k1" "Grandchild"))
               (denote-directory (list dir)))
          (denote-dash-repack-sequence-children "3a6")
          (denote-dash-test--kill-dir-buffers dir)
          ;; 3a6k → 3a6a; its child 3a6k1 must follow → 3a6a1
          (should (directory-files dir nil "20240101T100000==3a6a--"))
          (should (directory-files dir nil "20240101T110000==3a6a1--")))
      (denote-dash-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-dash-test/repack-no-gaps-is-noop ()
  "Repack does nothing and emits a message when sequences are already compact."
  (let ((dir (make-temp-file "denote-dash-test-" t)))
    (unwind-protect
        (let* ((_fa (denote-dash-test--make-org-note dir "20240101T100000" "3a6a" "First"))
               (_fb (denote-dash-test--make-org-note dir "20240101T110000" "3a6b" "Second"))
               (denote-directory (list dir)))
          ;; Should not signal user-error or rename anything
          (denote-dash-repack-sequence-children "3a6")
          (denote-dash-test--kill-dir-buffers dir)
          (should (directory-files dir nil "20240101T100000==3a6a--"))
          (should (directory-files dir nil "20240101T110000==3a6b--")))
      (denote-dash-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(provide 'test-denote-dash)
;;; test-denote-dash.el ends here
