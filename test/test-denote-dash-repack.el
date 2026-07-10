;;; test-denote-dash-repack.el --- ERT tests for denote-dash-repack.el -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for sequence signature alignment, repacking/compacting, swapping,
;; recursive reparenting, and sequence-position insertion — all backed by
;; real temp-directory Denote notes rather than mocked data, since these
;; operations rename files on disk.

;;; Code:

(require 'ert)
(require 'denote-dash-repack)

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
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
            (denote-dash-repack-sequence-children ""))
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
        (let* ((denote-sequence-scheme 'alphanumeric)
               (_fp  (denote-dash-test--make-org-note dir "20240101T090000" "3a6" "Parent"))
               (_fk  (denote-dash-test--make-org-note dir "20240101T100000" "3a6k" "First"))
               (_fl  (denote-dash-test--make-org-note dir "20240101T110000" "3a6l" "Second"))
               (denote-directory (list dir)))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
            (denote-dash-repack-sequence-children "3a6"))
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
        (let* ((denote-sequence-scheme 'alphanumeric)
               (_fp   (denote-dash-test--make-org-note dir "20240101T090000" "3a6"   "Parent"))
               (_fk   (denote-dash-test--make-org-note dir "20240101T100000" "3a6k"  "Child"))
               (_fk1  (denote-dash-test--make-org-note dir "20240101T110000" "3a6k1" "Grandchild"))
               (denote-directory (list dir)))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
            (denote-dash-repack-sequence-children "3a6"))
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
        (let* ((denote-sequence-scheme 'alphanumeric)
               (_fa (denote-dash-test--make-org-note dir "20240101T100000" "3a6a" "First"))
               (_fb (denote-dash-test--make-org-note dir "20240101T110000" "3a6b" "Second"))
               (denote-directory (list dir)))
          ;; Should not signal user-error or rename anything
          (denote-dash-repack-sequence-children "3a6")
          (denote-dash-test--kill-dir-buffers dir)
          (should (directory-files dir nil "20240101T100000==3a6a--"))
          (should (directory-files dir nil "20240101T110000==3a6b--")))
      (denote-dash-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-dash-test/repack-declining-confirmation-changes-nothing ()
  "Declining the preview confirmation leaves every file untouched."
  (let ((dir (make-temp-file "denote-dash-test-" t)))
    (unwind-protect
        (let* ((_f1 (denote-dash-test--make-org-note dir "20240101T100000" "1" "One"))
               (_f3 (denote-dash-test--make-org-note dir "20240101T110000" "3" "Three"))
               (denote-directory (list dir)))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
            (denote-dash-repack-sequence-children ""))
          (denote-dash-test--kill-dir-buffers dir)
          ;; "3" was NOT compacted to "2" since the user declined
          (should (directory-files dir nil "20240101T100000==1--"))
          (should (directory-files dir nil "20240101T110000==3--")))
      (denote-dash-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-dash-test/repack-preview-buffer-lists-every-rename ()
  "The preview buffer lists every file that will be renamed, old and new."
  (let ((dir (make-temp-file "denote-dash-test-" t)))
    (unwind-protect
        (let* ((denote-sequence-scheme 'alphanumeric)
               (_fp  (denote-dash-test--make-org-note dir "20240101T090000" "3a6"  "Parent"))
               (_fk  (denote-dash-test--make-org-note dir "20240101T100000" "3a6k" "Child"))
               (_fk1 (denote-dash-test--make-org-note dir "20240101T110000" "3a6k1" "Grandchild"))
               (denote-directory (list dir)))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
            (denote-dash-repack-sequence-children "3a6"))
          (denote-dash-test--kill-dir-buffers dir)
          (with-current-buffer "*Denote Repack Preview*"
            (let ((contents (buffer-string)))
              (should (string-match-p "2 files will be renamed" contents))
              (should (string-match-p "3a6k +-> +3a6a" contents))
              (should (string-match-p "3a6k1 +-> +3a6a1" contents)))))
      (denote-dash-test--kill-dir-buffers dir)
      (when-let* ((buf (get-buffer "*Denote Repack Preview*"))) (kill-buffer buf))
      (delete-directory dir t))))

;; Regression tests for a real repack bug: compacting multiple gapped
;; children at once (e.g. 1,2,4,5 -> 1,2,3,4) used to process children
;; highest-signature-first, one whole subtree at a time.  That left a
;; transient state where a freshly-renamed file and an as-yet-untouched
;; sibling briefly shared the same signature; the next subtree's prefix
;; lookup then swept up both and merged them into the same destination,
;; silently flattening two distinct branches into one.

(ert-deftest denote-dash-test/repack-multiple-gaps-no-collision ()
  "Compacting two gapped root children at once must not merge them.
Reproduces a real bug: repacking 1,2,4,5 (gap at 3) used to leave both
4 and 5 renamed to the same sequence \"3\"."
  (let ((dir (make-temp-file "denote-dash-test-" t)))
    (unwind-protect
        (let* ((denote-sequence-scheme 'alphanumeric)
               (_f1 (denote-dash-test--make-org-note dir "20240101T100000" "1" "One"))
               (_f2 (denote-dash-test--make-org-note dir "20240101T110000" "2" "Two"))
               (_f4 (denote-dash-test--make-org-note dir "20240101T120000" "4" "Four"))
               (_f5 (denote-dash-test--make-org-note dir "20240101T130000" "5" "Five"))
               (denote-directory (list dir)))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
            (denote-dash-repack-sequence-children ""))
          (denote-dash-test--kill-dir-buffers dir)
          (should (directory-files dir nil "20240101T100000==1--"))
          (should (directory-files dir nil "20240101T110000==2--"))
          (should (directory-files dir nil "20240101T120000==3--"))
          ;; "Five" must land on its own sequence "4", not collide with "Four"
          (should (directory-files dir nil "20240101T130000==4--")))
      (denote-dash-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-dash-test/repack-multiple-gaps-preserves-subtrees ()
  "Compacting multiple gapped children keeps each child's own descendants.
Extends `denote-dash-test/repack-multiple-gaps-no-collision' with a child
under each of the renamed nodes: the bug also merged descendants of two
unrelated subtrees onto the same signature."
  (let ((dir (make-temp-file "denote-dash-test-" t)))
    (unwind-protect
        (let* ((denote-sequence-scheme 'alphanumeric)
               (_f1  (denote-dash-test--make-org-note dir "20240101T100000" "1"  "One"))
               (_f2  (denote-dash-test--make-org-note dir "20240101T110000" "2"  "Two"))
               (_f4  (denote-dash-test--make-org-note dir "20240101T120000" "4"  "Four"))
               (_f4a (denote-dash-test--make-org-note dir "20240101T125000" "4a" "FourChild"))
               (_f5  (denote-dash-test--make-org-note dir "20240101T130000" "5"  "Five"))
               (_f5a (denote-dash-test--make-org-note dir "20240101T135000" "5a" "FiveChild"))
               (denote-directory (list dir)))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
            (denote-dash-repack-sequence-children ""))
          (denote-dash-test--kill-dir-buffers dir)
          (should (directory-files dir nil "20240101T120000==3--"))
          (should (directory-files dir nil "20240101T125000==3a--"))
          (should (directory-files dir nil "20240101T130000==4--"))
          ;; FiveChild must follow Five to "4a", not also land on "3a"
          (should (directory-files dir nil "20240101T135000==4a--")))
      (denote-dash-test--kill-dir-buffers dir)
      (delete-directory dir t))))

(ert-deftest denote-dash-test/repack-orders-numerically-not-lexicographically ()
  "Children are ordered by sequence value, not by raw string comparison.
Reproduces a real bug: sorting signatures with `string<' put \"10\" before
\"2\" (since \"1\" < \"2\" as characters), so compacting them assigned \"2\"
the *second* slot instead of the first."
  (let ((dir (make-temp-file "denote-dash-test-" t)))
    (unwind-protect
        (let* ((denote-sequence-scheme 'alphanumeric)
               (_f2  (denote-dash-test--make-org-note dir "20240101T100000" "2"  "Two"))
               (_f10 (denote-dash-test--make-org-note dir "20240101T110000" "10" "Ten"))
               (denote-directory (list dir)))
          (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
            (denote-dash-repack-sequence-children ""))
          (denote-dash-test--kill-dir-buffers dir)
          ;; Numeric order is 2 < 10, so "Two" takes the first slot
          (should (directory-files dir nil "20240101T100000==1--"))
          (should (directory-files dir nil "20240101T110000==2--")))
      (denote-dash-test--kill-dir-buffers dir)
      (delete-directory dir t))))


(provide 'test-denote-dash-repack)
;;; test-denote-dash-repack.el ends here
