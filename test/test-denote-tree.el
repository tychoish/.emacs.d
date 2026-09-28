;;; test-denote-tree.el --- ERT tests for denote-tree.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Tests for sequence signature alignment, repacking/compacting, swapping,
;; recursive reparenting, and sequence-position insertion — all backed by
;; real temp-directory Denote notes rather than mocked data, since these
;; operations rename files on disk.

;;; Code:

(require 'ert)
(require 'denote-tree)
(require 'annotated-completing-read)

;; denote-tree--signature-line

(ert-deftest denote-tree-test/signature-line-org ()
  "Org signature line uses #+signature: prefix, no quotes."
  (should (equal "#+signature: 1a" (denote-tree--signature-line "1a" 'org))))

(ert-deftest denote-tree-test/signature-line-text ()
  "Text signature line matches org format, no quotes."
  (should (equal "#+signature: 1a" (denote-tree--signature-line "1a" 'text))))

(ert-deftest denote-tree-test/signature-line-markdown-yaml ()
  "Markdown YAML signature line wraps the value in quotes."
  (should (equal "signature: \"1a\"" (denote-tree--signature-line "1a" 'markdown-yaml))))

(ert-deftest denote-tree-test/signature-line-markdown-toml ()
  "Markdown TOML signature line uses = and wraps the value in quotes."
  (should (equal "signature = \"1a\"" (denote-tree--signature-line "1a" 'markdown-toml))))

(ert-deftest denote-tree-test/signature-line-unknown-type ()
  "Unknown file type falls back to org format without quotes."
  (should (equal "#+signature: 2b" (denote-tree--signature-line "2b" 'unknown))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-tree--sequence-aligned-p (uses temp files)

(defun denote-tree-test--make-org-note (dir id sig title)
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

(ert-deftest denote-tree-test/aligned-p-both-match ()
  "File is aligned when filename and frontmatter signatures match."
  (let ((dir (make-temp-file "denote-tree-test-" t)))
    (unwind-protect
        (let* ((file (denote-tree-test--make-org-note
                      dir "20240101T120000" "1a" "Test Note"))
               (denote-directory (list dir)))
          (should (denote-tree--sequence-aligned-p file)))
      (delete-directory dir t))))

(ert-deftest denote-tree-test/aligned-p-both-nil ()
  "File with no sequence in filename or frontmatter is aligned."
  (let ((dir (make-temp-file "denote-tree-test-" t)))
    (unwind-protect
        (let* ((file (denote-tree-test--make-org-note
                      dir "20240101T120000" nil "Plain Note"))
               (denote-directory (list dir)))
          (should (denote-tree--sequence-aligned-p file)))
      (delete-directory dir t))))

(ert-deftest denote-tree-test/aligned-p-sig-missing-from-frontmatter ()
  "File is not aligned when filename has sig but frontmatter does not."
  (let ((dir (make-temp-file "denote-tree-test-" t)))
    (unwind-protect
        (let* ((file (denote-tree-test--make-org-note
                      dir "20240101T120000" nil "Plain Note"))
               ;; Manually rename to add ==1a without updating frontmatter
               (new-name (expand-file-name
                          "20240101T120000==1a--plain-note.org" dir))
               (_ (rename-file file new-name))
               (denote-directory (list dir)))
          (should-not (denote-tree--sequence-aligned-p new-name)))
      (delete-directory dir t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-tree--sequence-parent

(ert-deftest denote-tree-test/sequence-parent-root ()
  "Root sequences (depth 0) have no parent."
  (should-not (denote-tree--sequence-parent "1"))
  (should-not (denote-tree--sequence-parent "3"))
  (should-not (denote-tree--sequence-parent nil))
  (should-not (denote-tree--sequence-parent "")))

(ert-deftest denote-tree-test/sequence-parent-depth-1 ()
  "Depth-1 sequences return the root number as parent."
  (should (equal "1"  (denote-tree--sequence-parent "1a")))
  (should (equal "1"  (denote-tree--sequence-parent "1b")))
  (should (equal "3"  (denote-tree--sequence-parent "3b"))))

(ert-deftest denote-tree-test/sequence-parent-depth-2 ()
  "Depth-2 sequences return the depth-1 letter sequence as parent."
  (should (equal "1a"  (denote-tree--sequence-parent "1a1")))
  (should (equal "3a"  (denote-tree--sequence-parent "3a1")))
  (should (equal "3b"  (denote-tree--sequence-parent "3b9"))))

(ert-deftest denote-tree-test/sequence-parent-depth-3 ()
  "Depth-3 sequences return the depth-2 numeric sequence as parent."
  (should (equal "1a1"  (denote-tree--sequence-parent "1a1a")))
  (should (equal "3a1"  (denote-tree--sequence-parent "3a1b")))
  (should (equal "3a1"  (denote-tree--sequence-parent "3a1o"))))

(ert-deftest denote-tree-test/sequence-parent-multi-digit ()
  "Multi-digit segments are handled: parent of 3a12 is 3a."
  (should (equal "3a" (denote-tree--sequence-parent "3a12"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-tree-swap-with-parent (file-based)

(defun denote-tree-test--kill-dir-buffers (dir)
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

(ert-deftest denote-tree-test/swap-with-parent-renames-files ()
  "Swap exchanges the ==SEQ== component in both filenames."
  (let ((dir (make-temp-file "denote-tree-test-" t)))
    (unwind-protect
        (let* ((parent-file (denote-tree-test--make-org-note
                             dir "20240101T100000" "1a" "Parent"))
               (child-file  (denote-tree-test--make-org-note
                             dir "20240101T110000" "1a1" "Child"))
               (denote-directory (list dir)))
          (with-current-buffer (find-file-noselect child-file)
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
              (denote-tree-swap-with-parent)))
          ;; Kill any buffers the swap opened in the temp dir
          (denote-tree-test--kill-dir-buffers dir)
          ;; The file whose timestamp was 110000 now carries sequence 1a
          (should (directory-files dir nil "20240101T110000==1a--"))
          ;; The file whose timestamp was 100000 now carries sequence 1a1
          (should (directory-files dir nil "20240101T100000==1a1--")))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-tree-test/swap-with-parent-updates-frontmatter ()
  "Swap updates #+signature: in both files to match the new filename."
  (let ((dir (make-temp-file "denote-tree-test-" t)))
    (unwind-protect
        (let* ((parent-file (denote-tree-test--make-org-note
                             dir "20240101T100000" "1a" "Parent"))
               (child-file  (denote-tree-test--make-org-note
                             dir "20240101T110000" "1a1" "Child"))
               (denote-directory (list dir)))
          (with-current-buffer (find-file-noselect child-file)
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
              (denote-tree-swap-with-parent)))
          (denote-tree-test--kill-dir-buffers dir)
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
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-tree-test/swap-with-parent-no-stale-buffers ()
  "After swap, no buffer visits a path that no longer exists."
  (let ((dir (make-temp-file "denote-tree-test-" t)))
    (unwind-protect
        (let* ((parent-file (denote-tree-test--make-org-note
                             dir "20240101T100000" "1a" "Parent"))
               (child-file  (denote-tree-test--make-org-note
                             dir "20240101T110000" "1a1" "Child"))
               (denote-directory (list dir)))
          ;; Open both files first so buffers exist before the swap
          (find-file-noselect parent-file)
          (with-current-buffer (find-file-noselect child-file)
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
              (denote-tree-swap-with-parent)))
          (denote-tree-test--kill-dir-buffers dir)
          ;; No buffer should point to the old paths (they no longer exist)
          (should-not (find-buffer-visiting parent-file))
          (should-not (find-buffer-visiting child-file)))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-tree-test/swap-with-parent-remaps-persisted-fold ()
  "A persisted fold entry follows its sequence through the swap."
  (let ((dir (make-temp-file "denote-tree-test-" t)))
    (unwind-protect
        (let* ((parent-file (denote-tree-test--make-org-note
                             dir "20240101T100000" "1a" "Parent"))
               (child-file  (denote-tree-test--make-org-note
                             dir "20240101T110000" "1a1" "Child"))
               (denote-directory (list dir))
               (denote-tree-hierarchy-fold-sequences '("1a1")))
          (with-current-buffer (find-file-noselect child-file)
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
              (denote-tree-swap-with-parent)))
          (denote-tree-test--kill-dir-buffers dir)
          ;; "1a1" (the folded child) is now at "1a"
          (should (equal '("1a") denote-tree-hierarchy-fold-sequences)))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-tree-swap-with-previous / denote-tree-swap-with-next (file-based)

(defun denote-tree-test--make-tree (dir)
  "Create a two-branch sequence tree in DIR, return a plist of its files.
Branch one is 1/1a/1a1, branch two is 2/2a; 1 and 2 are root siblings."
  (list :root1       (denote-tree-test--make-org-note dir "20240101T100000" "1"   "Root One")
        :child1      (denote-tree-test--make-org-note dir "20240101T110000" "1a"  "Child One")
        :grandchild1 (denote-tree-test--make-org-note dir "20240101T120000" "1a1" "Grandchild One")
        :root2       (denote-tree-test--make-org-note dir "20240101T130000" "2"   "Root Two")
        :child2      (denote-tree-test--make-org-note dir "20240101T140000" "2a"  "Child Two")))

(ert-deftest denote-tree-test/swap-with-next-moves-subtree-recursively ()
  "Swapping root 1 with sibling 2 renumbers each whole subtree, not just the root."
  (let ((dir (make-temp-file "denote-tree-test-" t)))
    (unwind-protect
        (let* ((denote-sequence-scheme 'alphanumeric)
               (tree (denote-tree-test--make-tree dir))
               (denote-directory (list dir)))
          (with-current-buffer (find-file-noselect (plist-get tree :root1))
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
              (denote-tree-swap-with-next)))
          (denote-tree-test--kill-dir-buffers dir)
          (should (directory-files dir nil "20240101T100000==2--"))
          (should (directory-files dir nil "20240101T110000==2a--"))
          (should (directory-files dir nil "20240101T120000==2a1--"))
          (should (directory-files dir nil "20240101T130000==1--"))
          (should (directory-files dir nil "20240101T140000==1a--")))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-tree-test/swap-with-previous-moves-subtree-recursively ()
  "Swapping root 2 with previous sibling 1 gives the same result as swap-with-next."
  (let ((dir (make-temp-file "denote-tree-test-" t)))
    (unwind-protect
        (let* ((denote-sequence-scheme 'alphanumeric)
               (tree (denote-tree-test--make-tree dir))
               (denote-directory (list dir)))
          (with-current-buffer (find-file-noselect (plist-get tree :root2))
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
              (denote-tree-swap-with-previous)))
          (denote-tree-test--kill-dir-buffers dir)
          (should (directory-files dir nil "20240101T100000==2--"))
          (should (directory-files dir nil "20240101T110000==2a--"))
          (should (directory-files dir nil "20240101T120000==2a1--"))
          (should (directory-files dir nil "20240101T130000==1--"))
          (should (directory-files dir nil "20240101T140000==1a--")))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-tree-test/swap-with-next-updates-frontmatter-recursively ()
  "Frontmatter signature is fixed on descendants too, not just the swapped roots."
  (let ((dir (make-temp-file "denote-tree-test-" t)))
    (unwind-protect
        (let* ((denote-sequence-scheme 'alphanumeric)
               (tree (denote-tree-test--make-tree dir))
               (denote-directory (list dir)))
          (with-current-buffer (find-file-noselect (plist-get tree :root1))
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
              (denote-tree-swap-with-next)))
          (denote-tree-test--kill-dir-buffers dir)
          (let ((grandchild (car (directory-files dir t "20240101T120000==2a1--"))))
            (should grandchild)
            (with-temp-buffer
              (insert-file-contents grandchild)
              (should (search-forward "#+signature: 2a1" nil t)))))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-tree-test/swap-with-previous-errors-when-first ()
  "The first sibling has no previous sibling to swap with."
  (let ((dir (make-temp-file "denote-tree-test-" t)))
    (unwind-protect
        (let* ((tree (denote-tree-test--make-tree dir))
               (denote-directory (list dir)))
          (with-current-buffer (find-file-noselect (plist-get tree :root1))
            (should-error (denote-tree-swap-with-previous) :type 'user-error)))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-tree-test/swap-with-next-errors-when-last ()
  "The last sibling has no next sibling to swap with."
  (let ((dir (make-temp-file "denote-tree-test-" t)))
    (unwind-protect
        (let* ((tree (denote-tree-test--make-tree dir))
               (denote-directory (list dir)))
          (with-current-buffer (find-file-noselect (plist-get tree :root2))
            (should-error (denote-tree-swap-with-next) :type 'user-error)))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-tree-test/swap-with-next-no-stale-buffers ()
  "After a subtree swap, no buffer visits a path that no longer exists."
  (let ((dir (make-temp-file "denote-tree-test-" t)))
    (unwind-protect
        (let* ((tree (denote-tree-test--make-tree dir))
               (denote-directory (list dir)))
          (find-file-noselect (plist-get tree :grandchild1))
          (find-file-noselect (plist-get tree :child2))
          (with-current-buffer (find-file-noselect (plist-get tree :root1))
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
              (denote-tree-swap-with-next)))
          (denote-tree-test--kill-dir-buffers dir)
          (should-not (find-buffer-visiting (plist-get tree :root1)))
          (should-not (find-buffer-visiting (plist-get tree :grandchild1)))
          (should-not (find-buffer-visiting (plist-get tree :child2))))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-tree-reparent-recursive

(ert-deftest denote-tree-test/reparent-recursive-fixes-type-alternation ()
  "Reparent recursive rewrites suffix types when old and new roots differ.
Old root \"1a1\" ends digit (letter-children); target \"2\" ends digit so
its first child is \"2a\" which ends letter (digit-children).  The child
\"1a1a\" (letter suffix \"a\") must become \"2a1\" (digit suffix \"1\"),
not \"2aa\"."
  (let ((dir (make-temp-file "denote-tree-test-" t)))
    (unwind-protect
        (let* ((denote-sequence-scheme 'alphanumeric)
               (_root   (denote-tree-test--make-org-note dir "20240101T100000" "1a1"  "Root"))
               (_child  (denote-tree-test--make-org-note dir "20240101T110000" "1a1a" "Child"))
               (_target (denote-tree-test--make-org-note dir "20240101T120000" "2"    "Target"))
               (denote-directory (list dir))
               (root-file (car (directory-files dir t "20240101T100000=="))))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
            (denote-tree-reparent-recursive root-file _target))
          (denote-tree-test--kill-dir-buffers dir)
          ;; "1a1" → first letter child of "2" → "2a"
          (should (directory-files dir nil "20240101T100000==2a--"))
          ;; "1a1a": suffix "a" (letter) in old context (digit-ending "1a1"),
          ;; rewritten to digit "1" in new context (letter-ending "2a")
          (should (directory-files dir nil "20240101T110000==2a1--")))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-tree-test/reparent-recursive-same-type-no-change ()
  "When old and new roots end in the same type, suffix characters are preserved."
  (let ((dir (make-temp-file "denote-tree-test-" t)))
    (unwind-protect
        (let* ((denote-sequence-scheme 'alphanumeric)
               (_root   (denote-tree-test--make-org-note dir "20240101T100000" "1a"  "Root"))
               (_child  (denote-tree-test--make-org-note dir "20240101T110000" "1a1" "Child"))
               (_target (denote-tree-test--make-org-note dir "20240101T120000" "2a"  "Target"))
               (denote-directory (list dir))
               (root-file (car (directory-files dir t "20240101T100000=="))))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
            (denote-tree-reparent-recursive root-file _target))
          (denote-tree-test--kill-dir-buffers dir)
          ;; "1a" (ends letter) → first digit child of "2a" (ends letter) → "2a1"
          (should (directory-files dir nil "20240101T100000==2a1--"))
          ;; "1a1": suffix "1" (digit) in old context (letter-ending "1a"),
          ;; new root "2a1" ends digit → children are letters → "a"
          (should (directory-files dir nil "20240101T110000==2a1a--")))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-tree-test/reparent-nil-target-promotes-to-root ()
  "A nil FILE-WITH-SEQUENCE promotes the file to a new top-level sequence."
  (let ((dir (make-temp-file "denote-tree-test-" t)))
    (unwind-protect
        (let* ((denote-sequence-scheme 'alphanumeric)
               (_root (denote-tree-test--make-org-note dir "20240101T100000" "1a1" "Root"))
               (denote-directory (list dir))
               (root-file (car (directory-files dir t "20240101T100000=="))))
          (denote-tree-reparent root-file nil)
          (denote-tree-test--kill-dir-buffers dir)
          (should (directory-files dir nil "20240101T100000==2--")))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-tree-test/reparent-recursive-nil-target-promotes-subtree-to-root ()
  "A nil FILE-WITH-SEQUENCE with RECURSIVE promotes CURRENT-FILE and its
descendants to a new top-level sequence, instead of requiring
`denote-tree-renumber-recursive' and a hand-typed sequence."
  (let ((dir (make-temp-file "denote-tree-test-" t)))
    (unwind-protect
        (let* ((denote-sequence-scheme 'alphanumeric)
               (_root  (denote-tree-test--make-org-note dir "20240101T100000" "1a1"  "Root"))
               (_child (denote-tree-test--make-org-note dir "20240101T110000" "1a1a" "Child"))
               (denote-directory (list dir))
               (root-file (car (directory-files dir t "20240101T100000=="))))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
            (denote-tree-reparent-recursive root-file nil))
          (denote-tree-test--kill-dir-buffers dir)
          (should (directory-files dir nil "20240101T100000==2--"))
          (should (directory-files dir nil "20240101T110000==2a--")))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-tree-renumber-recursive

(ert-deftest denote-tree-test/renumber-recursive-fixes-type-alternation ()
  "Renumber recursive rewrites suffix types when old and new roots differ.
Old root \"1a\" ends letter (digit-children); given new sequence \"9\" ends
digit (letter-children).  The child \"1a1\" (digit suffix \"1\") must become
\"9a\" (letter suffix \"a\"), not \"91\"."
  (let ((dir (make-temp-file "denote-tree-test-" t)))
    (unwind-protect
        (let* ((denote-sequence-scheme 'alphanumeric)
               (_root  (denote-tree-test--make-org-note dir "20240101T100000" "1a"  "Root"))
               (_child (denote-tree-test--make-org-note dir "20240101T110000" "1a1" "Child"))
               (denote-directory (list dir))
               (root-file (car (directory-files dir t "20240101T100000=="))))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
            (denote-tree-renumber-recursive root-file "9"))
          (denote-tree-test--kill-dir-buffers dir)
          (should (directory-files dir nil "20240101T100000==9--"))
          (should (directory-files dir nil "20240101T110000==9a--")))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-tree-test/renumber-recursive-same-type-no-change ()
  "When old root and new sequence end in the same type, suffixes are preserved."
  (let ((dir (make-temp-file "denote-tree-test-" t)))
    (unwind-protect
        (let* ((denote-sequence-scheme 'alphanumeric)
               (_root  (denote-tree-test--make-org-note dir "20240101T100000" "1a"  "Root"))
               (_child (denote-tree-test--make-org-note dir "20240101T110000" "1a1" "Child"))
               (denote-directory (list dir))
               (root-file (car (directory-files dir t "20240101T100000=="))))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
            (denote-tree-renumber-recursive root-file "9a"))
          (denote-tree-test--kill-dir-buffers dir)
          (should (directory-files dir nil "20240101T100000==9a--"))
          (should (directory-files dir nil "20240101T110000==9a1--")))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-tree-test/renumber-recursive-remaps-persisted-fold ()
  "Persisted folds on the root and a descendant follow the renumber."
  (let ((dir (make-temp-file "denote-tree-test-" t)))
    (unwind-protect
        (let* ((denote-sequence-scheme 'alphanumeric)
               (_root  (denote-tree-test--make-org-note dir "20240101T100000" "1a"  "Root"))
               (_child (denote-tree-test--make-org-note dir "20240101T110000" "1a1" "Child"))
               (denote-directory (list dir))
               (root-file (car (directory-files dir t "20240101T100000==")))
               (denote-tree-hierarchy-fold-sequences '("1a" "1a1")))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
            (denote-tree-renumber-recursive root-file "9"))
          (denote-tree-test--kill-dir-buffers dir)
          (should (equal '("9" "9a") denote-tree-hierarchy-fold-sequences)))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-tree-test/renumber-recursive-errors-without-sequence ()
  "Renumbering a file with no existing sequence signals a user-error."
  (let ((dir (make-temp-file "denote-tree-test-" t)))
    (unwind-protect
        (let* ((denote-sequence-scheme 'alphanumeric)
               (_root (denote-tree-test--make-org-note dir "20240101T100000" nil "Root"))
               (denote-directory (list dir))
               (root-file (car (directory-files dir t "20240101T100000"))))
          (should-error (denote-tree-renumber-recursive root-file "9") :type 'user-error))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-tree-insert-sequence-note

(ert-deftest denote-tree-test/insert-sequence-note-shifts-siblings-without-prompting ()
  "Shifting siblings does not block on a per-file `y-or-n-p' confirmation.
Regression test: `denote-rename-file' prompts per-file under its default
`denote-rename-confirmations'; the shift loop must suppress that so a
single top-level `yes-or-no-p' confirmation is enough."
  (let ((dir (make-temp-file "denote-tree-test-" t)))
    (unwind-protect
        (let* ((_f1 (denote-tree-test--make-org-note dir "20240101T100000" "1" "One"))
               (_f2 (denote-tree-test--make-org-note dir "20240101T110000" "2" "Two"))
               (denote-directory (list dir))
               (file (car (directory-files dir t "20240101T100000=="))))
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (&rest _) (error "should not prompt per-file")))
                    ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                    ((symbol-function 'read-string) (lambda (&rest _) "New"))
                    ((symbol-function 'denote-keywords-prompt) (lambda (&rest _) nil))
                    ((symbol-function 'denote-tree--target-file) (lambda () file)))
            (denote-tree-insert-sequence-note))
          ;; the new note took the vacated "1" position; `denote' leaves it
          ;; in an unsaved buffer, so check the buffer rather than the disk
          (should (seq-find (lambda (buf)
                              (when-let* ((f (buffer-file-name buf)))
                                (string-match-p "==1--" f)))
                            (buffer-list)))
          (denote-tree-test--kill-dir-buffers dir)
          ;; "One" (was "1") shifted forward to "2"
          (should (directory-files dir nil "20240101T100000==2--"))
          ;; "Two" (was "2") shifted forward to "3"
          (should (directory-files dir nil "20240101T110000==3--")))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-tree-repack-children

(ert-deftest denote-tree-test/repack-root-uses-numbers ()
  "Root-level repack assigns numeric sequences, not letters."
  (let ((dir (make-temp-file "denote-tree-test-" t)))
    (unwind-protect
        (let* ((_f1 (denote-tree-test--make-org-note dir "20240101T100000" "1" "One"))
               (_f3 (denote-tree-test--make-org-note dir "20240101T110000" "3" "Three"))
               (denote-directory (list dir)))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
            (denote-tree-repack-children ""))
          (denote-tree-test--kill-dir-buffers dir)
          ;; "1" stays "1", "3" compacts to "2"
          (should (directory-files dir nil "20240101T100000==1--"))
          (should (directory-files dir nil "20240101T110000==2--")))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-tree-test/repack-letter-children-compact-gaps ()
  "Children of a digit-ending prefix use letters; gaps are compacted."
  (let ((dir (make-temp-file "denote-tree-test-" t)))
    (unwind-protect
        (let* ((denote-sequence-scheme 'alphanumeric)
               (_fp  (denote-tree-test--make-org-note dir "20240101T090000" "3a6" "Parent"))
               (_fk  (denote-tree-test--make-org-note dir "20240101T100000" "3a6k" "First"))
               (_fl  (denote-tree-test--make-org-note dir "20240101T110000" "3a6l" "Second"))
               (denote-directory (list dir)))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
            (denote-tree-repack-children "3a6"))
          (denote-tree-test--kill-dir-buffers dir)
          ;; k (11th letter) → a, l (12th letter) → b
          (should (directory-files dir nil "20240101T100000==3a6a--"))
          (should (directory-files dir nil "20240101T110000==3a6b--")))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-tree-test/repack-propagates-to-subtree ()
  "Repacking a child also renames that child's descendants."
  (let ((dir (make-temp-file "denote-tree-test-" t)))
    (unwind-protect
        (let* ((denote-sequence-scheme 'alphanumeric)
               (_fp   (denote-tree-test--make-org-note dir "20240101T090000" "3a6"   "Parent"))
               (_fk   (denote-tree-test--make-org-note dir "20240101T100000" "3a6k"  "Child"))
               (_fk1  (denote-tree-test--make-org-note dir "20240101T110000" "3a6k1" "Grandchild"))
               (denote-directory (list dir)))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
            (denote-tree-repack-children "3a6"))
          (denote-tree-test--kill-dir-buffers dir)
          ;; 3a6k → 3a6a; its child 3a6k1 must follow → 3a6a1
          (should (directory-files dir nil "20240101T100000==3a6a--"))
          (should (directory-files dir nil "20240101T110000==3a6a1--")))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-tree-test/repack-no-gaps-is-noop ()
  "Repack does nothing and emits a message when sequences are already compact."
  (let ((dir (make-temp-file "denote-tree-test-" t)))
    (unwind-protect
        (let* ((denote-sequence-scheme 'alphanumeric)
               (_fa (denote-tree-test--make-org-note dir "20240101T100000" "3a6a" "First"))
               (_fb (denote-tree-test--make-org-note dir "20240101T110000" "3a6b" "Second"))
               (denote-directory (list dir)))
          ;; Should not signal user-error or rename anything
          (denote-tree-repack-children "3a6")
          (denote-tree-test--kill-dir-buffers dir)
          (should (directory-files dir nil "20240101T100000==3a6a--"))
          (should (directory-files dir nil "20240101T110000==3a6b--")))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-tree-test/repack-declining-confirmation-changes-nothing ()
  "Declining the preview confirmation leaves every file untouched."
  (let ((dir (make-temp-file "denote-tree-test-" t)))
    (unwind-protect
        (let* ((_f1 (denote-tree-test--make-org-note dir "20240101T100000" "1" "One"))
               (_f3 (denote-tree-test--make-org-note dir "20240101T110000" "3" "Three"))
               (denote-directory (list dir)))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
            (denote-tree-repack-children ""))
          (denote-tree-test--kill-dir-buffers dir)
          ;; "3" was NOT compacted to "2" since the user declined
          (should (directory-files dir nil "20240101T100000==1--"))
          (should (directory-files dir nil "20240101T110000==3--")))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-tree-test/repack-preview-buffer-lists-every-rename ()
  "The preview buffer lists every file that will be renamed, old and new."
  (let ((dir (make-temp-file "denote-tree-test-" t)))
    (unwind-protect
        (let* ((denote-sequence-scheme 'alphanumeric)
               (_fp  (denote-tree-test--make-org-note dir "20240101T090000" "3a6"  "Parent"))
               (_fk  (denote-tree-test--make-org-note dir "20240101T100000" "3a6k" "Child"))
               (_fk1 (denote-tree-test--make-org-note dir "20240101T110000" "3a6k1" "Grandchild"))
               (denote-directory (list dir))
               (seen-preview nil))
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (&rest _)
                       (when (get-buffer "*Denote Repack Preview*")
                         (with-current-buffer "*Denote Repack Preview*"
                           (setq seen-preview (buffer-string))))
                       nil)))
            (denote-tree-repack-children "3a6"))
          (denote-tree-test--kill-dir-buffers dir)
          (should seen-preview)
          (should (string-match-p "2 files will be renamed" seen-preview))
          (should (string-match-p "3a6k +-> +3a6a" seen-preview))
          (should (string-match-p "3a6k1 +-> +3a6a1" seen-preview)))
      (denote-tree-test--kill-dir-buffers dir)
      (when-let* ((buf (get-buffer "*Denote Repack Preview*"))) (kill-buffer buf))
      (delete-directory dir t))))

(ert-deftest denote-tree-test/repack-preview-buffer-killed-on-confirmation ()
  "The preview buffer is dismissed and killed upon confirmation."
  (let ((dir (make-temp-file "denote-tree-test-" t)))
    (unwind-protect
        (let* ((denote-sequence-scheme 'alphanumeric)
               (_fp  (denote-tree-test--make-org-note dir "20240101T090000" "3a6"  "Parent"))
               (_fk  (denote-tree-test--make-org-note dir "20240101T100000" "3a6k" "Child"))
               (denote-directory (list dir)))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
            (denote-tree-repack-children "3a6"))
          (denote-tree-test--kill-dir-buffers dir)
          (should-not (get-buffer "*Denote Repack Preview*")))
      (denote-tree-test--kill-dir-buffers dir)
      (when-let* ((buf (get-buffer "*Denote Repack Preview*"))) (kill-buffer buf))
      (delete-directory dir t))))

;; Regression tests for a real repack bug: compacting multiple gapped
;; children at once (e.g. 1,2,4,5 -> 1,2,3,4) used to process children
;; highest-signature-first, one whole subtree at a time.  That left a
;; transient state where a freshly-renamed file and an as-yet-untouched
;; sibling briefly shared the same signature; the next subtree's prefix
;; lookup then swept up both and merged them into the same destination,
;; silently flattening two distinct branches into one.

(ert-deftest denote-tree-test/repack-multiple-gaps-no-collision ()
  "Compacting two gapped root children at once must not merge them.
Reproduces a real bug: repacking 1,2,4,5 (gap at 3) used to leave both
4 and 5 renamed to the same sequence \"3\"."
  (let ((dir (make-temp-file "denote-tree-test-" t)))
    (unwind-protect
        (let* ((denote-sequence-scheme 'alphanumeric)
               (_f1 (denote-tree-test--make-org-note dir "20240101T100000" "1" "One"))
               (_f2 (denote-tree-test--make-org-note dir "20240101T110000" "2" "Two"))
               (_f4 (denote-tree-test--make-org-note dir "20240101T120000" "4" "Four"))
               (_f5 (denote-tree-test--make-org-note dir "20240101T130000" "5" "Five"))
               (denote-directory (list dir)))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
            (denote-tree-repack-children ""))
          (denote-tree-test--kill-dir-buffers dir)
          (should (directory-files dir nil "20240101T100000==1--"))
          (should (directory-files dir nil "20240101T110000==2--"))
          (should (directory-files dir nil "20240101T120000==3--"))
          ;; "Five" must land on its own sequence "4", not collide with "Four"
          (should (directory-files dir nil "20240101T130000==4--")))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-tree-test/repack-multiple-gaps-preserves-subtrees ()
  "Compacting multiple gapped children keeps each child's own descendants.
Extends `denote-tree-test/repack-multiple-gaps-no-collision' with a child
under each of the renamed nodes: the bug also merged descendants of two
unrelated subtrees onto the same signature."
  (let ((dir (make-temp-file "denote-tree-test-" t)))
    (unwind-protect
        (let* ((denote-sequence-scheme 'alphanumeric)
               (_f1  (denote-tree-test--make-org-note dir "20240101T100000" "1"  "One"))
               (_f2  (denote-tree-test--make-org-note dir "20240101T110000" "2"  "Two"))
               (_f4  (denote-tree-test--make-org-note dir "20240101T120000" "4"  "Four"))
               (_f4a (denote-tree-test--make-org-note dir "20240101T125000" "4a" "FourChild"))
               (_f5  (denote-tree-test--make-org-note dir "20240101T130000" "5"  "Five"))
               (_f5a (denote-tree-test--make-org-note dir "20240101T135000" "5a" "FiveChild"))
               (denote-directory (list dir)))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
            (denote-tree-repack-children ""))
          (denote-tree-test--kill-dir-buffers dir)
          (should (directory-files dir nil "20240101T120000==3--"))
          (should (directory-files dir nil "20240101T125000==3a--"))
          (should (directory-files dir nil "20240101T130000==4--"))
          ;; FiveChild must follow Five to "4a", not also land on "3a"
          (should (directory-files dir nil "20240101T135000==4a--")))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-tree-test/repack-orders-numerically-not-lexicographically ()
  "Children are ordered by sequence value, not by raw string comparison.
Reproduces a real bug: sorting signatures with `string<' put \"10\" before
\"2\" (since \"1\" < \"2\" as characters), so compacting them assigned \"2\"
the *second* slot instead of the first."
  (let ((dir (make-temp-file "denote-tree-test-" t)))
    (unwind-protect
        (let* ((denote-sequence-scheme 'alphanumeric)
               (_f2  (denote-tree-test--make-org-note dir "20240101T100000" "2"  "Two"))
               (_f10 (denote-tree-test--make-org-note dir "20240101T110000" "10" "Ten"))
               (denote-directory (list dir)))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
            (denote-tree-repack-children ""))
          (denote-tree-test--kill-dir-buffers dir)
          ;; Numeric order is 2 < 10, so "Two" takes the first slot
          (should (directory-files dir nil "20240101T100000==1--"))
          (should (directory-files dir nil "20240101T110000==2--")))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-tree-swap-with-parent — complete file/buffer cleanup
;; These tests verify that ONLY the two expected files exist after a swap
;; and that OLD paths are completely gone, not just that new paths exist.

(ert-deftest denote-tree-test/swap-with-parent-old-files-removed ()
  "After swap, the two original file paths no longer exist on disk."
  (let ((dir (make-temp-file "denote-tree-test-" t)))
    (unwind-protect
        (let* ((parent-file (denote-tree-test--make-org-note
                             dir "20240101T100000" "1a" "Parent"))
               (child-file  (denote-tree-test--make-org-note
                             dir "20240101T110000" "1a1" "Child"))
               (denote-directory (list dir)))
          (with-current-buffer (find-file-noselect child-file)
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
              (denote-tree-swap-with-parent)))
          (denote-tree-test--kill-dir-buffers dir)
          ;; Original paths must not exist
          (should-not (file-exists-p parent-file))
          (should-not (file-exists-p child-file)))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-tree-test/swap-with-parent-exactly-two-files ()
  "After swap, the directory contains exactly the two renamed files — no extras."
  (let ((dir (make-temp-file "denote-tree-test-" t)))
    (unwind-protect
        (let* ((parent-file (denote-tree-test--make-org-note
                             dir "20240101T100000" "1a" "Parent"))
               (child-file  (denote-tree-test--make-org-note
                             dir "20240101T110000" "1a1" "Child"))
               (denote-directory (list dir)))
          (with-current-buffer (find-file-noselect child-file)
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
              (denote-tree-swap-with-parent)))
          (denote-tree-test--kill-dir-buffers dir)
          (should (= 2 (length (directory-files dir nil "\\.org$")))))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-tree-reparent-recursive — no-legacy-file guarantee

(ert-deftest denote-tree-test/reparent-recursive-old-files-removed ()
  "After reparent-recursive, no file with an old sequence exists on disk."
  (let ((dir (make-temp-file "denote-tree-test-" t)))
    (unwind-protect
        (let* ((denote-sequence-scheme 'alphanumeric)
               (root-file   (denote-tree-test--make-org-note dir "20240101T100000" "1a1"  "Root"))
               (child-file  (denote-tree-test--make-org-note dir "20240101T110000" "1a1a" "Child"))
               (target-file (denote-tree-test--make-org-note dir "20240101T120000" "2"    "Target"))
               (denote-directory (list dir)))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
            (denote-tree-reparent-recursive root-file target-file))
          (denote-tree-test--kill-dir-buffers dir)
          ;; Old sequence files must be gone
          (should-not (file-exists-p root-file))
          (should-not (file-exists-p child-file))
          ;; New files must exist
          (should (directory-files dir nil "20240101T100000==2a--"))
          (should (directory-files dir nil "20240101T110000==2a1--")))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-tree-test/reparent-recursive-no-stale-buffers ()
  "After reparent-recursive, no buffer visits a path that no longer exists."
  (let ((dir (make-temp-file "denote-tree-test-" t)))
    (unwind-protect
        (let* ((denote-sequence-scheme 'alphanumeric)
               (root-file   (denote-tree-test--make-org-note dir "20240101T100000" "1a1"  "Root"))
               (child-file  (denote-tree-test--make-org-note dir "20240101T110000" "1a1a" "Child"))
               (target-file (denote-tree-test--make-org-note dir "20240101T120000" "2"    "Target"))
               (denote-directory (list dir)))
          (find-file-noselect root-file)
          (find-file-noselect child-file)
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
            (denote-tree-reparent-recursive root-file target-file))
          (denote-tree-test--kill-dir-buffers dir)
          (should-not (find-buffer-visiting root-file))
          (should-not (find-buffer-visiting child-file)))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; upstream denote-sequence-reparent-recursive — bug confirmation tests
;;
;; These tests call the upstream `denote-sequence-reparent-recursive'
;; (from elpa) directly to document known bugs for an upstream issue report.
;;
;; Bug 1 — Type-alternation error: when reparenting from a root whose last
;;   char type differs from the new root's last char type, descendant suffixes
;;   are copied verbatim instead of being rewritten to maintain letter/digit
;;   alternation.  E.g. reparenting "1a1" under "2" should yield "2a1" for
;;   the child, but upstream produces "2aa" (suffix "a" copied unchanged).
;;
;; Bug 2 — Legacy files: `denote-rename-file' with `denote-rename-confirmations'
;;   not suppressed prompts once per file in the recursive operation.  Each
;;   individual rename is atomic (declining just skips that file), so a
;;   decline partway through leaves the recursive operation half-done: earlier
;;   files are already renamed (with their front matter possibly left stale,
;;   if that file's own front-matter-rewrite prompt was declined) while later
;;   descendants are never touched at all, producing an inconsistent tree
;;   where descendants no longer share a common sequence prefix with their
;;   reparented ancestor.

(ert-deftest denote-tree-test/upstream-reparent-recursive-type-alternation-bug ()
  "Upstream denote-sequence-reparent-recursive produces wrong suffix type.
Expected FAILURE with upstream: child '1a1a' is renamed to '2aa' instead
of the correct '2a1'.  This test documents the bug for an upstream report."
  :expected-result :failed
  (let ((dir (make-temp-file "denote-tree-test-upstream-" t)))
    (unwind-protect
        (let* ((denote-sequence-scheme 'alphanumeric)
               (_root   (denote-tree-test--make-org-note dir "20240101T100000" "1a1"  "Root"))
               (_child  (denote-tree-test--make-org-note dir "20240101T110000" "1a1a" "Child"))
               (target  (denote-tree-test--make-org-note dir "20240101T120000" "2"    "Target"))
               (denote-directory (list dir))
               (root-file (car (directory-files dir t "20240101T100000=="))))
          ;; `denote-rename-file' asks its per-file confirmations via
          ;; `y-or-n-p' (not `yes-or-no-p'); stub that instead so the run
          ;; doesn't block on a real prompt in a batch/daemon test run.
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
            (denote-sequence-reparent-recursive root-file target))
          (denote-tree-test--kill-dir-buffers dir)
          ;; Correct: child suffix "a" (letter in digit-ending ctx) → "1" (digit in letter-ending ctx)
          ;; Upstream bug: produces "2aa" — this assertion fails, confirming the bug
          (should (directory-files dir nil "20240101T110000==2a1--")))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-tree-test/upstream-reparent-recursive-legacy-files ()
  "Upstream denote-sequence-reparent-recursive leaves the tree half-migrated
when confirmations fire and one is declined partway through.
Expected FAILURE with upstream when `denote-rename-confirmations' is non-nil.
Demonstrates the need to suppress `denote-rename-confirmations' around the
operation — as `denote-tree-reparent-recursive' does."
  :expected-result :failed
  (let ((dir (make-temp-file "denote-tree-test-upstream-" t)))
    (unwind-protect
        (let* ((denote-sequence-scheme 'alphanumeric)
               (root-file   (denote-tree-test--make-org-note dir "20240101T100000" "1a"  "Root"))
               (child-file  (denote-tree-test--make-org-note dir "20240101T110000" "1a1" "Child"))
               (target-file (denote-tree-test--make-org-note dir "20240101T120000" "2a"  "Target"))
               ;; Leave denote-rename-confirmations at its default (non-nil)
               ;; and stub y-or-n-p to accept the first prompt (the root's own
               ;; rename) and decline every prompt after that, simulating a
               ;; user who is prompted partway through a recursive operation
               ;; and accidentally declines.
               (call-count 0)
               (denote-directory (list dir)))
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (&rest _)
                       (setq call-count (1+ call-count))
                       ;; Accept the first file prompt, decline the rest
                       (= call-count 1))))
            (ignore-errors
              (denote-sequence-reparent-recursive root-file target-file)))
          (denote-tree-test--kill-dir-buffers dir)
          ;; Each per-file rename is atomic: declining its prompt just skips
          ;; that file rather than leaving a duplicate old+new pair.  
          (should-not (file-exists-p child-file)))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-tree--retag-apply

(ert-deftest denote-tree-test/retag-apply-add ()
  "Adding keywords appends them and deduplicates."
  (should (equal '("a" "b" "c")
                 (denote-tree--retag-apply '("a" "b") 'add '("b" "c") nil))))

(ert-deftest denote-tree-test/retag-apply-add-already-present-is-noop ()
  "Adding a keyword that every file already has returns `:unchanged'."
  (should (eq :unchanged (denote-tree--retag-apply '("a" "b") 'add '("a") nil))))

(ert-deftest denote-tree-test/retag-apply-remove ()
  "Removing keywords drops only the requested ones."
  (should (equal '("a") (denote-tree--retag-apply '("a" "b") 'remove nil '("b")))))

(ert-deftest denote-tree-test/retag-apply-remove-to-empty ()
  "Removing every remaining keyword returns an empty list, not `:unchanged'
— an empty result is a legitimate change, not a no-op."
  (should (equal '() (denote-tree--retag-apply '("a") 'remove nil '("a")))))

(ert-deftest denote-tree-test/retag-apply-remove-absent-is-noop ()
  "Removing a keyword that is not present returns `:unchanged', so the file
is left untouched instead of being renamed to the same keyword set."
  (should (eq :unchanged (denote-tree--retag-apply '("a") 'remove nil '("missing")))))

(ert-deftest denote-tree-test/retag-apply-replace-present ()
  "Replace swaps the old keyword for the new one when the old one is present."
  (should (equal '("a" "c") (denote-tree--retag-apply '("a" "b") 'replace '("c") '("b")))))

(ert-deftest denote-tree-test/retag-apply-replace-absent-is-noop ()
  "Replace returns `:unchanged' for a file that never had the old
keyword — it must not pick up the new keyword as a side effect."
  (should (eq :unchanged (denote-tree--retag-apply '("a") 'replace '("c") '("b")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-tree-retag-sequence (file-based)

(defun denote-tree-test--make-org-note-with-keywords (dir id sig keywords title)
  "Create a minimal org Denote note in DIR with KEYWORDS and return its path.
ID is the timestamp string, SIG the sequence or nil, KEYWORDS a list of
strings, TITLE the note title."
  (let* ((name (concat id
                       (when sig (concat "==" sig))
                       (when keywords (concat "__" (string-join keywords "_")))
                       "--" (downcase (replace-regexp-in-string " " "-" title))
                       ".org"))
         (file (expand-file-name name dir)))
    (with-temp-file file
      (insert "#+title:      " title "\n")
      (insert "#+date:       [2024-01-01 Mon]\n")
      (insert "#+identifier: " id "\n")
      (when sig (insert "#+signature: " sig "\n"))
      (when keywords (insert "#+filetags:   :" (string-join keywords ":") ":\n"))
      (insert "\nBody text.\n"))
    file))

(defun denote-tree-test--find-by-identifier (dir id)
  "Return the path in DIR whose Denote identifier is ID.
Renaming can reorder filename components, so callers must not assume a
fixed substring like \"==SIG__\" survives a rename; look up by identifier
instead."
  (seq-find (lambda (f) (equal id (denote-retrieve-filename-identifier f)))
            (directory-files dir t (regexp-quote id))))

(ert-deftest denote-tree-test/retag-sequence-add-across-subtree ()
  "Adding a keyword touches the sequence root and every descendant."
  (let ((dir (make-temp-file "denote-tree-test-" t)))
    (unwind-protect
        (let* ((root (denote-tree-test--make-org-note-with-keywords
                      dir "20240101T100000" "1" '("alpha") "Root"))
               (_child (denote-tree-test--make-org-note-with-keywords
                        dir "20240101T110000" "1a" '("beta") "Child"))
               (denote-directory (list dir))
               (denote-sequence-scheme 'alphanumeric))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                    ((symbol-function 'annotated-completing-read)
                     (lambda (&rest _) "add"))
                    ((symbol-function 'completing-read-multiple)
                     (lambda (&rest _) '("gamma")))
                    ((symbol-function 'denote-tree--target-file) (lambda () root)))
            (denote-tree-retag-sequence))
          (should (member "gamma" (denote-extract-keywords-from-path
                                   (denote-tree-test--find-by-identifier dir "20240101T100000"))))
          (should (member "gamma" (denote-extract-keywords-from-path
                                   (denote-tree-test--find-by-identifier dir "20240101T110000")))))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-tree-test/retag-sequence-remove-does-not-prompt-per-file ()
  "The subtree-wide rename loop suppresses per-file confirmation prompts."
  (let ((dir (make-temp-file "denote-tree-test-" t)))
    (unwind-protect
        (let* ((root (denote-tree-test--make-org-note-with-keywords
                      dir "20240101T100000" "1" '("alpha" "beta") "Root"))
               (_child (denote-tree-test--make-org-note-with-keywords
                        dir "20240101T110000" "1a" '("alpha") "Child"))
               (denote-directory (list dir))
               (denote-sequence-scheme 'alphanumeric))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                    ((symbol-function 'y-or-n-p)
                     (lambda (&rest _) (error "should not prompt per-file")))
                    ((symbol-function 'annotated-completing-read)
                     (lambda (&rest _) "remove"))
                    ((symbol-function 'completing-read-multiple)
                     (lambda (&rest _) '("alpha")))
                    ((symbol-function 'denote-tree--target-file) (lambda () root)))
            (denote-tree-retag-sequence))
          (should-not (member "alpha" (denote-extract-keywords-from-path
                                       (denote-tree-test--find-by-identifier dir "20240101T100000"))))
          (should-not (member "alpha" (denote-extract-keywords-from-path
                                       (denote-tree-test--find-by-identifier dir "20240101T110000")))))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-tree-test/retag-sequence-replace-only-touches-files-with-old-keyword ()
  "Replace swaps the keyword only on files that carry the old one, and
leaves every other file in the subtree completely untouched — it must
not add the new keyword to files that never had the old one."
  (let ((dir (make-temp-file "denote-tree-test-" t)))
    (unwind-protect
        (let* ((root (denote-tree-test--make-org-note-with-keywords
                      dir "20240101T100000" "1" '("alpha") "Root"))
               (_child (denote-tree-test--make-org-note-with-keywords
                        dir "20240101T110000" "1a" '("beta") "Child"))
               (denote-directory (list dir))
               (denote-sequence-scheme 'alphanumeric))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                    ((symbol-function 'annotated-completing-read)
                     (let ((calls 0))
                       (lambda (&rest _)
                         (setq calls (1+ calls))
                         (if (= calls 1) "replace" "alpha"))))
                    ((symbol-function 'read-string) (lambda (&rest _) "gamma"))
                    ((symbol-function 'denote-tree--target-file) (lambda () root)))
            (denote-tree-retag-sequence))
          ;; Root had "alpha": swapped to "gamma".
          (should (member "gamma" (denote-extract-keywords-from-path
                                   (denote-tree-test--find-by-identifier dir "20240101T100000"))))
          (should-not (member "alpha" (denote-extract-keywords-from-path
                                       (denote-tree-test--find-by-identifier dir "20240101T100000"))))
          ;; Child never had "alpha": left with "beta" only, no "gamma" added.
          (should (equal '("beta") (denote-extract-keywords-from-path
                                    (denote-tree-test--find-by-identifier dir "20240101T110000")))))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-tree-test/retag-sequence-add-skips-files-that-already-have-it ()
  "Adding a keyword the root already has, but a descendant lacks, only
renames the descendant."
  (let ((dir (make-temp-file "denote-tree-test-" t)))
    (unwind-protect
        (let* ((root (denote-tree-test--make-org-note-with-keywords
                      dir "20240101T100000" "1" '("gamma") "Root"))
               (_child (denote-tree-test--make-org-note-with-keywords
                        dir "20240101T110000" "1a" '("beta") "Child"))
               (denote-directory (list dir))
               (denote-sequence-scheme 'alphanumeric))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                    ((symbol-function 'annotated-completing-read)
                     (lambda (&rest _) "add"))
                    ((symbol-function 'completing-read-multiple)
                     (lambda (&rest _) '("gamma")))
                    ((symbol-function 'denote-tree--target-file) (lambda () root)))
            (denote-tree-retag-sequence))
          (should (equal '("gamma") (denote-extract-keywords-from-path
                                     (denote-tree-test--find-by-identifier dir "20240101T100000"))))
          (should (equal '("beta" "gamma") (denote-extract-keywords-from-path
                                            (denote-tree-test--find-by-identifier dir "20240101T110000")))))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-tree-test/retag-sequence-no-affected-files-errors ()
  "If no file in the subtree is affected by the operation, error out instead
of silently doing nothing (or, worse, touching files it shouldn't)."
  (let ((dir (make-temp-file "denote-tree-test-" t)))
    (unwind-protect
        (let* ((root (denote-tree-test--make-org-note-with-keywords
                      dir "20240101T100000" "1" '("alpha") "Root"))
               (denote-directory (list dir))
               (denote-sequence-scheme 'alphanumeric))
          (cl-letf (((symbol-function 'yes-or-no-p)
                     (lambda (&rest _) (error "should not reach confirmation")))
                    ((symbol-function 'annotated-completing-read)
                     (lambda (&rest _) "remove"))
                    ((symbol-function 'completing-read-multiple)
                     (lambda (&rest _) '("missing")))
                    ((symbol-function 'denote-tree--target-file) (lambda () root)))
            (should-error (denote-tree-retag-sequence) :type 'user-error))
          (should (member "alpha" (denote-extract-keywords-from-path root))))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-tree-test/retag-sequence-declining-confirmation-changes-nothing ()
  "Declining the single top-level confirmation leaves every file untouched."
  (let ((dir (make-temp-file "denote-tree-test-" t)))
    (unwind-protect
        (let* ((root (denote-tree-test--make-org-note-with-keywords
                      dir "20240101T100000" "1" '("alpha") "Root"))
               (denote-directory (list dir)))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil))
                    ((symbol-function 'annotated-completing-read)
                     (lambda (&rest _) "add"))
                    ((symbol-function 'completing-read-multiple)
                     (lambda (&rest _) '("gamma")))
                    ((symbol-function 'denote-tree--target-file) (lambda () root)))
            (ignore-errors (denote-tree-retag-sequence)))
          (should (member "alpha" (denote-extract-keywords-from-path root)))
          (should-not (member "gamma" (denote-extract-keywords-from-path root))))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper, buffer cleanup, and parsing tests

(ert-deftest denote-tree-test/kill-visiting-buffers ()
  "Kill visiting buffers cleans up buffers visiting the specified files."
  (let* ((f1 (make-temp-file "denote-test-k1"))
         (f2 (make-temp-file "denote-test-k2"))
         (b1 (find-file-noselect f1))
         (b2 (find-file-noselect f2)))
    (unwind-protect
        (progn
          (should (buffer-live-p b1))
          (should (buffer-live-p b2))
          (denote-tree--kill-visiting-buffers (list f1 f2))
          (should-not (buffer-live-p b1))
          (should-not (buffer-live-p b2)))
      (when (buffer-live-p b1) (kill-buffer b1))
      (when (buffer-live-p b2) (kill-buffer b2))
      (delete-file f1)
      (delete-file f2))))

(ert-deftest denote-tree-test/seq-split-segments ()
  "Segment splitting parses alternating letter and digit sequences into numbers."
  (should (equal '(1 1 2 2) (denote-tree--seq-split-segments "a1b2" :letter)))
  (should (equal '(1 1 2 2) (denote-tree--seq-split-segments "1a2b" :digit)))
  (should (equal '(10 3) (denote-tree--seq-split-segments "10c" :digit))))

(ert-deftest denote-tree-test/segment-conversions ()
  "Converts segment strings to numbers and back according to type."
  (should (= 5 (denote-tree--segment-to-number "5" :digit)))
  (should (= 2 (denote-tree--segment-to-number "b" :letter)))
  (should (equal "5" (denote-tree--number-to-segment 5 :digit)))
  (should (equal "b" (denote-tree--number-to-segment 2 :letter))))

(ert-deftest denote-tree-test/increment-sequence-various-depths ()
  "Increment sequence advances the last component across depths."
  (should (equal "2" (denote-tree--increment-sequence "1")))
  (should (equal "1b" (denote-tree--increment-sequence "1a")))
  (should (equal "1a2" (denote-tree--increment-sequence "1a1")))
  (should (equal "1a1c" (denote-tree--increment-sequence "1a1b"))))

(ert-deftest denote-tree-test/format-lint-entry ()
  "Formats lint entries with filename and signature comparisons."
  (let* ((dir (make-temp-file "denote-lint-test-" t))
         (denote-directory (list dir)))
    (unwind-protect
        (let* ((aligned (denote-tree-test--make-org-note dir "20240101T100000" "1" "Aligned"))
               (entry (denote-tree--format-lint-entry aligned)))
          (should (string-match-p "filename=1" entry))
          (should (string-match-p "frontmatter=1" entry)))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-tree-test/lint-sequences-buffer ()
  "Lint sequences command populates the report buffer."
  (let ((buf (get-buffer-create "*Denote Sequence Lint*")))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'denote-tree--collect-sequence-mismatches)
                     (lambda () nil)))
            (denote-tree-lint-sequences)
            (with-current-buffer "*Denote Sequence Lint*"
              (should (string-match-p "All sequence notes are aligned" (buffer-string))))))
      (when (get-buffer "*Denote Sequence Lint*")
        (kill-buffer "*Denote Sequence Lint*")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Swap and insert error conditions

(ert-deftest denote-tree-test/swap-with-parent-no-sequence-error ()
  "Swapping with parent errors when target note has no sequence."
  (let* ((dir (make-temp-file "denote-swap-err-" t))
         (denote-directory (list dir)))
    (unwind-protect
        (let ((note (denote-tree-test--make-org-note dir "20240101T100000" nil "NoSeq")))
          (cl-letf (((symbol-function 'denote-tree--target-file) (lambda () note)))
            (should-error (denote-tree-swap-with-parent) :type 'user-error)))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-tree-test/swap-with-parent-root-sequence-error ()
  "Swapping with parent errors when target note is a root sequence."
  (let* ((dir (make-temp-file "denote-swap-root-" t))
         (denote-directory (list dir)))
    (unwind-protect
        (let ((root (denote-tree-test--make-org-note dir "20240101T100000" "1" "Root")))
          (cl-letf (((symbol-function 'denote-tree--target-file) (lambda () root)))
            (should-error (denote-tree-swap-with-parent) :type 'user-error)))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-tree-test/swap-with-parent-cancelled ()
  "Declining confirmation in swap-with-parent cancels without modifying files."
  (let* ((dir (make-temp-file "denote-swap-canc-" t))
         (denote-directory (list dir)))
    (unwind-protect
        (let* ((p (denote-tree-test--make-org-note dir "20240101T100000" "1" "Parent"))
               (c (denote-tree-test--make-org-note dir "20240101T100100" "1a" "Child")))
          (cl-letf (((symbol-function 'denote-tree--target-file) (lambda () c))
                    ((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
            (should-error (denote-tree-swap-with-parent) :type 'user-error))
          (should (file-exists-p p))
          (should (file-exists-p c)))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-tree-test/insert-sequence-note-no-sequence-error ()
  "Inserting sequence note errors when target note has no sequence ID."
  (let* ((dir (make-temp-file "denote-ins-err-" t))
         (denote-directory (list dir)))
    (unwind-protect
        (let ((note (denote-tree-test--make-org-note dir "20240101T100000" nil "NoSeq")))
          (cl-letf (((symbol-function 'denote-tree--target-file) (lambda () note)))
            (should-error (denote-tree-insert-sequence-note) :type 'user-error)))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-tree-test/insert-sequence-note-cancelled ()
  "Declining confirmation cancels insert without modifying files."
  (let* ((dir (make-temp-file "denote-ins-canc-" t))
         (denote-directory (list dir)))
    (unwind-protect
        (let ((note (denote-tree-test--make-org-note dir "20240101T100000" "1" "Root")))
          (cl-letf (((symbol-function 'denote-tree--target-file) (lambda () note))
                    ((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
            (should-error (denote-tree-insert-sequence-note) :type 'user-error))
          (should (file-exists-p note)))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-tree-test/following-siblings-descending-order ()
  "Following siblings returns matching siblings in descending sequence order."
  (let* ((dir (make-temp-file "denote-sibs-" t))
         (denote-directory (list dir))
         (denote-sequence-scheme 'alphanumeric))
    (unwind-protect
        (let* ((f1 (denote-tree-test--make-org-note dir "20240101T100000" "1" "One"))
               (f2 (denote-tree-test--make-org-note dir "20240101T100100" "2" "Two"))
               (f3 (denote-tree-test--make-org-note dir "20240101T100200" "3" "Three"))
               (sibs (denote-tree--following-siblings "2")))
          (should (= 2 (length sibs)))
          (should (equal f3 (car sibs)))
          (should (equal f2 (cadr sibs))))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))


(defun denote-tree-test--insert-hierarchy-tree (tree)
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

(defconst denote-tree-test--hierarchy-tree
  '((1 . "1")
    (1 . "2") (2 . "2a") (2 . "2b")
    (1 . "3") (2 . "3a") (3 . "3a1") (3 . "3a2") (2 . "3b") (3 . "3b1"))
  "Synthetic hierarchy tree shared by the fold tests.")

(defun denote-tree-test--hierarchy-point-for (seq)
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

(ert-deftest denote-tree-test/hierarchy-heading-positions-collects-all ()
  "Every inserted line is collected, in order, with its level and sequence."
  (with-temp-buffer
    (denote-tree-test--insert-hierarchy-tree denote-tree-test--hierarchy-tree)
    (let ((headings (denote-tree--hierarchy-heading-positions)))
      (should (= (length denote-tree-test--hierarchy-tree) (length headings)))
      (should (equal (mapcar #'cdr denote-tree-test--hierarchy-tree)
                     (mapcar (lambda (h) (nth 2 h)) headings))))))

(ert-deftest denote-tree-test/hierarchy-section-sizes-singleton ()
  "A root with no children has section size 1."
  (with-temp-buffer
    (denote-tree-test--insert-hierarchy-tree denote-tree-test--hierarchy-tree)
    (let* ((headings (denote-tree--hierarchy-heading-positions))
           (sizes (denote-tree--hierarchy-section-sizes headings))
           (pos (denote-tree-test--hierarchy-point-for "1")))
      (should (= 1 (cdr (assq pos sizes)))))))

(ert-deftest denote-tree-test/hierarchy-section-sizes-two-children ()
  "A root with 2 direct children has section size 3."
  (with-temp-buffer
    (denote-tree-test--insert-hierarchy-tree denote-tree-test--hierarchy-tree)
    (let* ((headings (denote-tree--hierarchy-heading-positions))
           (sizes (denote-tree--hierarchy-section-sizes headings))
           (pos (denote-tree-test--hierarchy-point-for "2")))
      (should (= 3 (cdr (assq pos sizes)))))))

(ert-deftest denote-tree-test/hierarchy-section-sizes-nested-descendants ()
  "A root with 5 nested descendants has section size 6."
  (with-temp-buffer
    (denote-tree-test--insert-hierarchy-tree denote-tree-test--hierarchy-tree)
    (let* ((headings (denote-tree--hierarchy-heading-positions))
           (sizes (denote-tree--hierarchy-section-sizes headings))
           (pos (denote-tree-test--hierarchy-point-for "3")))
      (should (= 6 (cdr (assq pos sizes)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-tree--hierarchy-apply-initial-fold

(defun denote-tree-test--hierarchy-level-at-point ()
  "`outline-level' function for the synthetic hierarchy test buffers."
  (get-text-property (point) 'denote-sequence-hierarchy-level))

(defmacro denote-tree-test--with-hierarchy-buffer (&rest body)
  "Run BODY in a temp buffer populated like a real hierarchy view, with
`outline-minor-mode' configured the same way `denote-sequence-hierarchy-mode' does."
  `(with-temp-buffer
     (denote-tree-test--insert-hierarchy-tree denote-tree-test--hierarchy-tree)
     (setq-local outline-regexp "[\s[:alnum:]]+")
     (setq-local outline-level #'denote-tree-test--hierarchy-level-at-point)
     (outline-minor-mode 1)
     ,@body))

(ert-deftest denote-tree-test/hierarchy-apply-fold-depth ()
  "A depth of 1 folds every root's children, but not the roots themselves."
  (denote-tree-test--with-hierarchy-buffer
   (let ((denote-tree-hierarchy-initial-fold-depth 1)
         (denote-tree-hierarchy-fold-sequences nil)
         (denote-tree-hierarchy-auto-fold-min-size nil)
         (denote-tree-hierarchy-auto-fold-max-size nil))
     (denote-tree--hierarchy-apply-initial-fold)
     (should-not (outline-invisible-p (denote-tree-test--hierarchy-point-for "1")))
     (should-not (outline-invisible-p (denote-tree-test--hierarchy-point-for "2")))
     (should (outline-invisible-p (denote-tree-test--hierarchy-point-for "2a"))))))

(ert-deftest denote-tree-test/hierarchy-apply-fold-explicit-sequences ()
  "Only the listed sequence's subtree folds; unrelated roots stay expanded."
  (denote-tree-test--with-hierarchy-buffer
   (let ((denote-tree-hierarchy-initial-fold-depth nil)
         (denote-tree-hierarchy-fold-sequences '("3"))
         (denote-tree-hierarchy-auto-fold-min-size nil)
         (denote-tree-hierarchy-auto-fold-max-size nil))
     (denote-tree--hierarchy-apply-initial-fold)
     (should (outline-invisible-p (denote-tree-test--hierarchy-point-for "3a")))
     (should-not (outline-invisible-p (denote-tree-test--hierarchy-point-for "2a"))))))

(ert-deftest denote-tree-test/hierarchy-apply-fold-min-size ()
  "Sections at or below the min-size threshold fold; larger ones don't."
  (denote-tree-test--with-hierarchy-buffer
   (let ((denote-tree-hierarchy-initial-fold-depth nil)
         (denote-tree-hierarchy-fold-sequences nil)
         (denote-tree-hierarchy-auto-fold-min-size 1)
         (denote-tree-hierarchy-auto-fold-max-size nil))
     (denote-tree--hierarchy-apply-initial-fold)
     ;; "1" has size 1 (<= 1): nothing under it to hide, but it must not error.
     (should-not (outline-invisible-p (denote-tree-test--hierarchy-point-for "1")))
     ;; "2" has size 3 (> 1): stays expanded.
     (should-not (outline-invisible-p (denote-tree-test--hierarchy-point-for "2a"))))))

(ert-deftest denote-tree-test/hierarchy-apply-fold-max-size ()
  "Sections larger than the max-size threshold fold; smaller ones don't."
  (denote-tree-test--with-hierarchy-buffer
   (let ((denote-tree-hierarchy-initial-fold-depth nil)
         (denote-tree-hierarchy-fold-sequences nil)
         (denote-tree-hierarchy-auto-fold-min-size nil)
         (denote-tree-hierarchy-auto-fold-max-size 3))
     (denote-tree--hierarchy-apply-initial-fold)
     ;; "3" has size 6 (> 3): folds.
     (should (outline-invisible-p (denote-tree-test--hierarchy-point-for "3a")))
     ;; "2" has size 3 (not > 3): stays expanded.
     (should-not (outline-invisible-p (denote-tree-test--hierarchy-point-for "2a"))))))

(ert-deftest denote-tree-test/hierarchy-apply-fold-composes-rules ()
  "An explicit-list fold and a max-size fold both apply when both are set."
  (denote-tree-test--with-hierarchy-buffer
   (let ((denote-tree-hierarchy-initial-fold-depth nil)
         (denote-tree-hierarchy-fold-sequences '("2"))
         (denote-tree-hierarchy-auto-fold-min-size nil)
         (denote-tree-hierarchy-auto-fold-max-size 3))
     (denote-tree--hierarchy-apply-initial-fold)
     (should (outline-invisible-p (denote-tree-test--hierarchy-point-for "2a")))
     (should (outline-invisible-p (denote-tree-test--hierarchy-point-for "3a"))))))

(ert-deftest denote-tree-test/hierarchy-apply-fold-noop-when-unconfigured ()
  "With every option nil, nothing folds."
  (denote-tree-test--with-hierarchy-buffer
   (let ((denote-tree-hierarchy-initial-fold-depth nil)
         (denote-tree-hierarchy-fold-sequences nil)
         (denote-tree-hierarchy-auto-fold-min-size nil)
         (denote-tree-hierarchy-auto-fold-max-size nil))
     (denote-tree--hierarchy-apply-initial-fold)
     (should-not (outline-invisible-p (denote-tree-test--hierarchy-point-for "2a")))
     (should-not (outline-invisible-p (denote-tree-test--hierarchy-point-for "3a1"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-tree--hierarchy-goto-root / denote-tree-hierarchy-toggle-fold-sequence

(ert-deftest denote-tree-test/hierarchy-goto-root-from-descendant ()
  "Point moves to the top-level heading enclosing a deeply nested descendant."
  (denote-tree-test--with-hierarchy-buffer
   (goto-char (denote-tree-test--hierarchy-point-for "3a1"))
   (denote-tree--hierarchy-goto-root)
   (should (= (point) (denote-tree-test--hierarchy-point-for "3")))))

(ert-deftest denote-tree-test/hierarchy-goto-root-already-at-root ()
  "Point already on a root heading does not move."
  (denote-tree-test--with-hierarchy-buffer
   (goto-char (denote-tree-test--hierarchy-point-for "2"))
   (denote-tree--hierarchy-goto-root)
   (should (= (point) (denote-tree-test--hierarchy-point-for "2")))))

(ert-deftest denote-tree-test/hierarchy-toggle-fold-sequence-adds-and-folds ()
  "Toggling from an unfolded descendant marks its root and hides the subtree."
  (denote-tree-test--with-hierarchy-buffer
   (let ((denote-tree-hierarchy-fold-sequences nil))
     (goto-char (denote-tree-test--hierarchy-point-for "3a"))
     (denote-tree-hierarchy-toggle-fold-sequence)
     (should (equal '("3") denote-tree-hierarchy-fold-sequences))
     (should (outline-invisible-p (denote-tree-test--hierarchy-point-for "3a"))))))

(ert-deftest denote-tree-test/hierarchy-toggle-fold-sequence-removes-and-unfolds ()
  "Toggling an already-marked root unmarks it and shows the subtree again."
  (denote-tree-test--with-hierarchy-buffer
   (let ((denote-tree-hierarchy-fold-sequences '("3")))
     (goto-char (denote-tree-test--hierarchy-point-for "3"))
     (outline-hide-subtree)
     (denote-tree-hierarchy-toggle-fold-sequence)
     (should-not denote-tree-hierarchy-fold-sequences)
     (should-not (outline-invisible-p (denote-tree-test--hierarchy-point-for "3a"))))))

(ert-deftest denote-tree-test/hierarchy-clear-fold-sequences ()
  "Clearing empties the persisted fold list."
  (let ((denote-tree-hierarchy-fold-sequences '("1" "2a" "3")))
    (denote-tree-hierarchy-clear-fold-sequences)
    (should-not denote-tree-hierarchy-fold-sequences)))

(ert-deftest denote-tree-test/hierarchy-fold-sequences-persisted-via-savehist ()
  "The fold-sequences variable is registered for `savehist-mode' persistence."
  (require 'savehist)
  (should (memq 'denote-tree-hierarchy-fold-sequences savehist-additional-variables)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-tree--hierarchy-remap-fold-sequence-prefix / --swap-fold-sequence*

(ert-deftest denote-tree-test/hierarchy-remap-fold-sequence-prefix-exact ()
  "An exact-match entry is rewritten to the new sequence."
  (let ((denote-tree-hierarchy-fold-sequences '("8a" "9b")))
    (denote-tree--hierarchy-remap-fold-sequence-prefix "8a" "3c")
    (should (equal '("3c" "9b") denote-tree-hierarchy-fold-sequences))))

(ert-deftest denote-tree-test/hierarchy-remap-fold-sequence-prefix-descendant ()
  "A folded descendant of the renamed subtree keeps its relative suffix."
  (let ((denote-tree-hierarchy-fold-sequences '("8a1")))
    (denote-tree--hierarchy-remap-fold-sequence-prefix "8a" "3c")
    (should (equal '("3c1") denote-tree-hierarchy-fold-sequences))))

(ert-deftest denote-tree-test/hierarchy-remap-fold-sequence-prefix-noop-unrelated ()
  "Entries outside the renamed prefix are left alone."
  (let ((denote-tree-hierarchy-fold-sequences '("9b")))
    (denote-tree--hierarchy-remap-fold-sequence-prefix "8a" "3c")
    (should (equal '("9b") denote-tree-hierarchy-fold-sequences))))

(ert-deftest denote-tree-test/hierarchy-swap-fold-sequence-exact ()
  "Two single entries trade places without a sequential-application clash."
  (let ((denote-tree-hierarchy-fold-sequences '("8a" "8")))
    (denote-tree--hierarchy-swap-fold-sequence "8a" "8")
    (should (equal '("8" "8a") denote-tree-hierarchy-fold-sequences))))

(ert-deftest denote-tree-test/hierarchy-swap-fold-sequence-prefix-swaps-subtrees ()
  "Descendant entries under either sibling swap prefixes together."
  (let ((denote-tree-hierarchy-fold-sequences '("8a1" "8b2")))
    (denote-tree--hierarchy-swap-fold-sequence-prefix "8a" "8b")
    (should (equal '("8b1" "8a2") denote-tree-hierarchy-fold-sequences))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flymake backend & hooks tests

(ert-deftest denote-tree-test/flymake-reports-mismatch ()
  "Flymake backend generates warning diagnostic when signature misaligned."
  (let ((temp-file (make-temp-file "denote-tree-test-" nil ".org")))
    (unwind-protect
        (with-temp-buffer
          (insert "#+title: Test Note\n#+identifier: 20260927T120000\n#+signature: 1a\n\nBody.\n")
          (write-region (point-min) (point-max) temp-file nil 'silent)
          (let* ((renamed-file (concat (file-name-directory temp-file)
                                       "20260927T120000==1b--test-note__tag.org")))
            (rename-file temp-file renamed-file t)
            (setq temp-file renamed-file)
            (with-current-buffer (find-file-noselect temp-file)
              (let (reported-diags)
                (denote-tree-flymake (lambda (diags) (setq reported-diags diags)))
                (should (= (length reported-diags) 1))
                (should (string-search "Sequence signature mismatch"
                                       (flymake-diagnostic-text (car reported-diags)))))
              (kill-buffer (find-buffer-visiting temp-file)))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(ert-deftest denote-tree-test/after-swap-hook-fires ()
  "The `denote-tree-after-swap-functions' hook fires when swapping."
  (let ((dir (make-temp-file "denote-tree-test-" t)))
    (unwind-protect
        (let* ((denote-sequence-scheme 'alphanumeric)
               (tree (denote-tree-test--make-tree dir))
               (denote-directory (list dir))
               hook-args)
          (let ((denote-tree-after-swap-functions
                 (list (lambda (seq-a seq-b files-a files-b)
                         (setq hook-args (list seq-a seq-b files-a files-b))))))
            (with-current-buffer (find-file-noselect (plist-get tree :root1))
              (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
                (denote-tree-swap-with-next))))
          (should hook-args)
          (should (equal (car hook-args) "1"))
          (should (equal (cadr hook-args) "2")))
      (denote-tree-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(provide 'test-denote-tree)
;;; test-denote-tree.el ends here
