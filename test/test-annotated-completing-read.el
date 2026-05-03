;;; test-annotated-completing-read.el --- ERT tests for annotated-completing-read -*- lexical-binding: t -*-

;; These tests are designed to run inside a live Emacs session with the full
;; config loaded (M-x ert RET t RET), or via:
;;   (ert-run-tests-batch-and-exit "annotated-completing-read")

(require 'ert)
(require 'ht)
(require 'annotated-completing-read)

;;; Helpers

(defmacro acr-with-mock (table return-value &rest body)
  "Call `annotated-completing-read' on TABLE with RETURN-VALUE as mock result.
Within BODY, `captured-args' is bound to the argument list that
`completing-read' was called with, and `captured-collection' is its second
element (the completion table function)."
  (declare (indent 2))
  `(let (captured-args captured-collection)
     (cl-letf (((symbol-function 'completing-read)
                (lambda (&rest args)
                  (setq captured-args args
                        captured-collection (nth 1 args))
                  ,return-value)))
       ,@body)))

(defun acr-metadata (collection)
  "Return the metadata alist from a completion COLLECTION function."
  (cdr (funcall collection "" nil 'metadata)))

;;; Guard

(ert-deftest annotated-completing-read/rejects-non-hash-table ()
  (should-error (annotated-completing-read "string")    :type 'user-error)
  (should-error (annotated-completing-read '(a . b))    :type 'user-error)
  (should-error (annotated-completing-read nil)         :type 'user-error)
  (should-error (annotated-completing-read [vec])       :type 'user-error))

(ert-deftest annotated-completing-read/accepts-plain-hash-table ()
  (let ((table (make-hash-table :test #'equal)))
    (puthash "foo" "bar" table)
    (acr-with-mock table "foo"
      (should (equal "foo" (annotated-completing-read table))))))

(ert-deftest annotated-completing-read/accepts-ht-table ()
  (let ((table (ht ("foo" "bar"))))
    (acr-with-mock table "foo"
      (should (equal "foo" (annotated-completing-read table))))))

;;; Prompt normalisation

(ert-deftest annotated-completing-read/prompt-trailing-space-added ()
  (let ((table (ht ("a" "ann"))))
    (acr-with-mock table "a"
      (annotated-completing-read table :prompt "Select")
      (should (equal "Select " (nth 0 captured-args))))))

(ert-deftest annotated-completing-read/prompt-trailing-space-not-doubled ()
  (let ((table (ht ("a" "ann"))))
    (acr-with-mock table "a"
      (annotated-completing-read table :prompt "Select ")
      (should (equal "Select " (nth 0 captured-args))))))

;;; History

(ert-deftest annotated-completing-read/history-keyed-by-this-command ()
  (let ((annotated-completing-read-history (ht-create))
        (this-command 'my-test-command)
        (table (ht ("x" "note"))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _coll _pred _req _init hist &rest _)
                 (push "x" (symbol-value hist))
                 "x")))
      (annotated-completing-read table))
    (should (equal '("x")
                   (ht-get annotated-completing-read-history 'my-test-command)))))

(ert-deftest annotated-completing-read/explicit-history-key-isolates ()
  (let ((annotated-completing-read-history (ht-create))
        (this-command 'other-command)
        (table (ht ("x" "note"))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _coll _pred _req _init hist &rest _)
                 (push "x" (symbol-value hist))
                 "x")))
      (annotated-completing-read table :history 'explicit-key))
    (should (equal '("x")
                   (ht-get annotated-completing-read-history 'explicit-key)))
    (should (null (ht-get annotated-completing-read-history 'other-command)))))

(ert-deftest annotated-completing-read/history-accumulates-across-calls ()
  (let ((annotated-completing-read-history (ht-create))
        (this-command 'accumulate-cmd)
        (table (ht ("x" "1") ("y" "2"))))
    (cl-letf (((symbol-function 'completing-read)
               (lambda (_prompt _coll _pred _req _init hist &rest _)
                 (let ((val (if (null (symbol-value hist)) "x" "y")))
                   (push val (symbol-value hist))
                   val))))
      (annotated-completing-read table)
      (annotated-completing-read table))
    (should (equal '("y" "x")
                   (ht-get annotated-completing-read-history 'accumulate-cmd)))))

;;; Completion metadata — annotation

(ert-deftest annotated-completing-read/annotation-function-present ()
  (let ((table (ht ("alpha" "first letter") ("beta" "second letter"))))
    (acr-with-mock table "alpha"
      (annotated-completing-read table)
      (let ((annotate (alist-get 'annotation-function (acr-metadata captured-collection))))
        (should (functionp annotate))
        (should (string-match-p "first letter"  (funcall annotate "alpha")))
        (should (string-match-p "second letter" (funcall annotate "beta")))))))

(ert-deftest annotated-completing-read/annotation-alignment ()
  "Padding ensures the annotation column position is constant across candidates.
Annotation values must be the same length for a total-width comparison to hold;
the invariant being tested is that key+padding is constant, not key+padding+value."
  (let ((table (ht ("a" "x") ("much-longer-key" "y"))))
    (acr-with-mock table "a"
      (annotated-completing-read table)
      (let* ((annotate  (alist-get 'annotation-function (acr-metadata captured-collection)))
             (ann-short (funcall annotate "a"))
             (ann-long  (funcall annotate "much-longer-key")))
        (should (= (+ (length "a")               (length ann-short))
                   (+ (length "much-longer-key") (length ann-long))))))))

;;; Completion metadata — category

(ert-deftest annotated-completing-read/category-surfaced-in-metadata ()
  (let ((table (ht ("a" "ann"))))
    (acr-with-mock table "a"
      (annotated-completing-read table :category 'my-category)
      (should (eq 'my-category
                  (alist-get 'category (acr-metadata captured-collection)))))))

(ert-deftest annotated-completing-read/no-category-when-omitted ()
  (let ((table (ht ("a" "ann"))))
    (acr-with-mock table "a"
      (annotated-completing-read table)
      (should (null (alist-get 'category (acr-metadata captured-collection)))))))

;;; Completion metadata — group

(ert-deftest annotated-completing-read/no-group-fn-without-group-name ()
  (let ((table (ht ("a" "ann"))))
    (acr-with-mock table "a"
      (annotated-completing-read table)
      (should (null (alist-get 'group-function (acr-metadata captured-collection)))))))

(ert-deftest annotated-completing-read/group-name-string-constant ()
  (let ((table (ht ("a" "ann") ("b" "ann2"))))
    (acr-with-mock table "a"
      (annotated-completing-read table :group-name "My Group")
      (let ((gfn (alist-get 'group-function (acr-metadata captured-collection))))
        (should (functionp gfn))
        (should (equal "My Group" (funcall gfn "a" nil)))
        (should (equal "My Group" (funcall gfn "b" nil)))))))

(ert-deftest annotated-completing-read/group-name-function ()
  (let ((table (ht ("TestFoo" "t") ("BenchBar" "b"))))
    (acr-with-mock table "TestFoo"
      (annotated-completing-read
       table
       :group-name (lambda (c) (if (string-prefix-p "Bench" c) "Benchmarks" "Tests")))
      (let ((gfn (alist-get 'group-function (acr-metadata captured-collection))))
        (should (equal "Tests"      (funcall gfn "TestFoo"  nil)))
        (should (equal "Benchmarks" (funcall gfn "BenchBar" nil)))))))

(ert-deftest annotated-completing-read/group-display-defaults-to-identity ()
  (let ((table (ht ("TestFoo" "t"))))
    (acr-with-mock table "TestFoo"
      (annotated-completing-read table :group-name "Tests")
      (let ((gfn (alist-get 'group-function (acr-metadata captured-collection))))
        (should (equal "TestFoo" (funcall gfn "TestFoo" t)))))))

(ert-deftest annotated-completing-read/group-display-function ()
  (let ((table (ht ("TestFoo" "t") ("TestBar" "b"))))
    (acr-with-mock table "TestFoo"
      (annotated-completing-read
       table
       :group-name    "Tests"
       :group-display (lambda (c) (string-remove-prefix "Test" c)))
      (let ((gfn (alist-get 'group-function (acr-metadata captured-collection))))
        (should (equal "Tests" (funcall gfn "TestFoo" nil)))
        (should (equal "Foo"   (funcall gfn "TestFoo" t)))
        (should (equal "Bar"   (funcall gfn "TestBar" t)))))))

(ert-deftest annotated-completing-read/group-display-without-group-name-ignored ()
  "group-display alone does not produce a group-function."
  (let ((table (ht ("a" "ann"))))
    (acr-with-mock table "a"
      (annotated-completing-read table :group-display #'upcase)
      (should (null (alist-get 'group-function (acr-metadata captured-collection)))))))

;;; require-match / arbitrary input

(ert-deftest annotated-completing-read/arbitrary-input-returned-verbatim ()
  "When require-match is nil, input not in TABLE is returned as-is."
  (let ((table (ht ("known" "ann"))))
    (acr-with-mock table "totally-new"
      (should (equal "totally-new"
                     (annotated-completing-read table :require-match nil))))))

(ert-deftest annotated-completing-read/require-match-passed-through ()
  "The require-match value reaches completing-read unchanged."
  (let ((table (ht ("a" "ann"))))
    (acr-with-mock table "a"
      (annotated-completing-read table :require-match t)
      (should (eq t (nth 3 captured-args))))
    (acr-with-mock table "a"
      (annotated-completing-read table :require-match nil)
      (should (null (nth 3 captured-args))))))

;;; initial-input

(ert-deftest annotated-completing-read/initial-input-passed-through ()
  (let ((table (ht ("foo" "bar"))))
    (acr-with-mock table "foo"
      (annotated-completing-read table :initial-input "fo")
      (should (equal "fo" (nth 4 captured-args))))))

(ert-deftest annotated-completing-read/initial-input-nil-by-default ()
  (let ((table (ht ("foo" "bar"))))
    (acr-with-mock table "foo"
      (annotated-completing-read table)
      (should (null (nth 4 captured-args))))))

;;; Collection completions (not just metadata)

(ert-deftest annotated-completing-read/collection-returns-candidates ()
  (let ((table (ht ("alpha" "1") ("beta" "2") ("gamma" "3"))))
    (acr-with-mock table "alpha"
      (annotated-completing-read table)
      (let ((all (funcall captured-collection "" nil t)))
        (should (member "alpha" all))
        (should (member "beta"  all))
        (should (member "gamma" all))))))

(ert-deftest annotated-completing-read/collection-filters-by-prefix ()
  (let ((table (ht ("alpha" "1") ("beta" "2") ("aleph" "3"))))
    (acr-with-mock table "alpha"
      (annotated-completing-read table)
      (let ((matches (funcall captured-collection "al" nil t)))
        (should (member "alpha" matches))
        (should (member "aleph" matches))
        (should-not (member "beta" matches))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; completing-read--context-candidates: return type and seeds

(ert-deftest annotated-completing-read/candidates-returns-hash-table ()
  (let ((kill-ring nil))
    (with-temp-buffer
      (should (hash-table-p (completing-read--context-candidates))))))

(ert-deftest annotated-completing-read/candidates-seed-string-included ()
  (let ((kill-ring nil))
    (with-temp-buffer
      (should (ht-contains-p (completing-read--context-candidates "hello") "hello")))))

(ert-deftest annotated-completing-read/candidates-seed-annotation ()
  (let ((kill-ring nil))
    (with-temp-buffer
      (should (equal "seed" (ht-get (completing-read--context-candidates "hello") "hello"))))))

(ert-deftest annotated-completing-read/candidates-seed-list ()
  (let ((kill-ring nil))
    (with-temp-buffer
      (let ((tbl (completing-read--context-candidates '("foo" "bar"))))
        (should (ht-contains-p tbl "foo"))
        (should (ht-contains-p tbl "bar"))))))

(ert-deftest annotated-completing-read/candidates-seed-too-long-excluded ()
  "Seeds >= 128 characters are excluded."
  (let ((kill-ring nil)
        (long-seed (make-string 130 ?x)))
    (with-temp-buffer
      (should-not (ht-contains-p (completing-read--context-candidates long-seed) long-seed)))))

(ert-deftest annotated-completing-read/candidates-empty-seed-excluded ()
  (let ((kill-ring nil))
    (with-temp-buffer
      (should-not (ht-contains-p (completing-read--context-candidates "") "")))))

(ert-deftest annotated-completing-read/candidates-whitespace-seed-excluded ()
  (let ((kill-ring nil))
    (with-temp-buffer
      (let ((tbl (completing-read--context-candidates "   ")))
        (should-not (ht-contains-p tbl ""))
        (should-not (ht-contains-p tbl "   "))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; completing-read--context-candidates: kill ring

(ert-deftest annotated-completing-read/candidates-kill-ring-included ()
  (let ((kill-ring (list "from-kill-ring")))
    (with-temp-buffer
      (should (ht-contains-p (completing-read--context-candidates) "from-kill-ring")))))

(ert-deftest annotated-completing-read/candidates-kill-ring-annotation-format ()
  "Kill ring entries are annotated as kill-ring [N] starting at index 1."
  (let ((kill-ring (list "first-item")))
    (with-temp-buffer
      (should (equal "kill-ring [1]" (ht-get (completing-read--context-candidates) "first-item"))))))

(ert-deftest annotated-completing-read/candidates-kill-ring-limited-to-ten ()
  "At most 10 kill ring entries are included."
  (let ((kill-ring (mapcar (lambda (n) (format "kill-%02d" n)) (number-sequence 1 15))))
    (with-temp-buffer
      (let* ((tbl (completing-read--context-candidates))
             (kill-keys (cl-remove-if-not (lambda (k) (string-prefix-p "kill-" k))
                                          (ht-keys tbl))))
        (should (<= (length kill-keys) 10))))))

(ert-deftest annotated-completing-read/candidates-kill-ring-long-item-excluded ()
  "Kill ring items >= 128 characters are excluded."
  (let ((kill-ring (list (make-string 130 ?k))))
    (with-temp-buffer
      (should (= 0 (ht-size (completing-read--context-candidates)))))))

(ert-deftest annotated-completing-read/candidates-kill-ring-whitespace-excluded ()
  (let ((kill-ring (list "   " "\t\n")))
    (with-temp-buffer
      (should (= 0 (ht-size (completing-read--context-candidates)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; completing-read--context-candidates: region

(ert-deftest annotated-completing-read/candidates-region-included ()
  "Active region content is included as a candidate."
  (let ((kill-ring nil))
    (with-temp-buffer
      (insert "selected text")
      (cl-letf (((symbol-function 'use-region-p) (lambda () t))
                ((symbol-function 'region-beginning) (lambda () 1))
                ((symbol-function 'region-end) (lambda () (point-max))))
        (should (ht-contains-p (completing-read--context-candidates) "selected text"))))))

(ert-deftest annotated-completing-read/candidates-region-annotation-format ()
  "Region annotation contains both 'region' and the buffer name."
  (let ((kill-ring nil))
    (with-temp-buffer
      (rename-buffer "my-test-buf" t)
      ;; Insert longer line so line candidate != region candidate, preventing overwrite.
      (insert "prefix region content suffix")
      (cl-letf (((symbol-function 'use-region-p) (lambda () t))
                ((symbol-function 'region-beginning) (lambda () 8))
                ((symbol-function 'region-end) (lambda () 22)))
        ;; positions 8–22 = "region content" (14 chars)
        (let ((annotation (ht-get (completing-read--context-candidates) "region content")))
          (should (string-match-p "region" annotation))
          (should (string-match-p "my-test-buf" annotation)))))))

(ert-deftest annotated-completing-read/candidates-region-excluded-when-inactive ()
  "When use-region-p is nil no region candidate is added."
  (let ((kill-ring nil))
    (with-temp-buffer
      (insert "some text")
      (cl-letf (((symbol-function 'use-region-p) (lambda () nil)))
        (let* ((tbl (completing-read--context-candidates))
               (annotation (ht-get tbl "some text")))
          (should-not (and annotation (string-match-p "region" annotation))))))))

(ert-deftest annotated-completing-read/candidates-region-too-long-excluded ()
  "Region content >= 128 characters is excluded."
  (let ((kill-ring nil)
        (long-text (make-string 130 ?r)))
    (with-temp-buffer
      (insert long-text)
      (cl-letf (((symbol-function 'use-region-p) (lambda () t))
                ((symbol-function 'region-beginning) (lambda () 1))
                ((symbol-function 'region-end) (lambda () (point-max))))
        (should-not (ht-contains-p (completing-read--context-candidates) long-text))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; completing-read--context-candidates: current line

(ert-deftest annotated-completing-read/candidates-line-included ()
  "The current line is included as a candidate."
  (let ((kill-ring nil))
    (with-temp-buffer
      (insert "current line text")
      (goto-char (point-min))
      (should (ht-contains-p (completing-read--context-candidates) "current line text")))))

(ert-deftest annotated-completing-read/candidates-line-annotation-format ()
  "Line annotation contains both 'line' and the buffer name."
  (let ((kill-ring nil))
    (with-temp-buffer
      (rename-buffer "line-test-buf" t)
      (insert "my line")
      (goto-char (point-min))
      (let ((annotation (ht-get (completing-read--context-candidates) "my line")))
        (should (string-match-p "line" annotation))
        (should (string-match-p "line-test-buf" annotation))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; completing-read--context-candidates: thing at point

(ert-deftest annotated-completing-read/candidates-thing-at-point-in-prog-mode ()
  "Symbols at point in prog-mode buffers are included."
  (let ((kill-ring nil))
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert "my-symbol")
      (goto-char 5)
      (should (ht-contains-p (completing-read--context-candidates) "my-symbol")))))

(ert-deftest annotated-completing-read/candidates-thing-at-point-annotation ()
  "thing-at-point candidates carry an 'at point' annotation."
  (let ((kill-ring nil))
    (with-temp-buffer
      (emacs-lisp-mode)
      ;; Surround the word so the line candidate differs from the symbol.
      (insert "prefix my-func suffix")
      (goto-char 12)  ; inside "my-func"
      (let ((annotation (ht-get (completing-read--context-candidates) "my-func")))
        (should (stringp annotation))
        (should (string-match-p "at point" annotation))))))

(ert-deftest annotated-completing-read/candidates-thing-not-added-in-fundamental-mode ()
  "fundamental-mode produces no 'at point' candidates."
  (let ((kill-ring nil))
    (with-temp-buffer
      (insert "someword")
      (goto-char 4)
      (let* ((tbl (completing-read--context-candidates))
             (annotation (ht-get tbl "someword")))
        (should-not (and annotation (string-match-p "at point" annotation)))))))

(ert-deftest annotated-completing-read/candidates-thing-too-long-excluded ()
  "thing-at-point values with length >= 64 are excluded from at-point candidates."
  (let ((kill-ring nil)
        (long-word (make-string 65 ?w)))
    (with-temp-buffer
      (emacs-lisp-mode)
      (insert long-word)
      (goto-char 1)
      (let ((annotation (ht-get (completing-read--context-candidates) long-word)))
        ;; may appear as a line candidate, but must not be annotated as at-point
        (should-not (and annotation (string-match-p "at point" annotation)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; completing-read-context-from-point

(ert-deftest annotated-completing-read/context-from-point-returns-string ()
  (let ((kill-ring (list "candidate")))
    (with-temp-buffer
      (cl-letf (((symbol-function 'annotated-completing-read)
                 (lambda (_tbl &rest _) "candidate")))
        (should (stringp (completing-read-context-from-point)))))))

(ert-deftest annotated-completing-read/context-from-point-returns-selection ()
  (let ((kill-ring (list "chosen")))
    (with-temp-buffer
      (cl-letf (((symbol-function 'annotated-completing-read)
                 (lambda (_tbl &rest _) "chosen")))
        (should (equal "chosen" (completing-read-context-from-point)))))))

(ert-deftest annotated-completing-read/context-from-point-passes-prompt ()
  "The PROMPT argument is forwarded to annotated-completing-read."
  (let ((kill-ring (list "item"))
        received-prompt)
    (with-temp-buffer
      (cl-letf (((symbol-function 'annotated-completing-read)
                 (lambda (_tbl &rest args)
                   (setq received-prompt (plist-get args :prompt))
                   "item")))
        (completing-read-context-from-point "my prompt: ")
        (should (equal "my prompt: " received-prompt))))))

(ert-deftest annotated-completing-read/context-from-point-history-defaults-to-this-command ()
  "When :history is unspecified, this-command is used as the history key."
  (let ((kill-ring (list "item"))
        received-history)
    (with-temp-buffer
      (cl-letf (((symbol-function 'annotated-completing-read)
                 (lambda (_tbl &rest args)
                   (setq received-history (plist-get args :history))
                   "item")))
        (let ((this-command 'my-calling-command))
          (completing-read-context-from-point))
        (should (eq 'my-calling-command received-history))))))

(ert-deftest annotated-completing-read/context-from-point-explicit-history-key ()
  "An explicit :history symbol is forwarded to annotated-completing-read."
  (let ((kill-ring (list "item"))
        received-history)
    (with-temp-buffer
      (cl-letf (((symbol-function 'annotated-completing-read)
                 (lambda (_tbl &rest args)
                   (setq received-history (plist-get args :history))
                   "item")))
        (completing-read-context-from-point nil nil :history 'my-history)
        (should (eq 'my-history received-history))))))

(ert-deftest annotated-completing-read/context-from-point-seed-in-candidates ()
  "The SEED argument produces candidates annotated as 'seed'."
  (let ((kill-ring nil)
        received-table)
    (with-temp-buffer
      (cl-letf (((symbol-function 'annotated-completing-read)
                 (lambda (tbl &rest _)
                   (setq received-table tbl)
                   "myseed")))
        (completing-read-context-from-point nil "myseed")
        (should (ht-contains-p received-table "myseed"))
        (should (equal "seed" (ht-get received-table "myseed")))))))

(ert-deftest annotated-completing-read/context-from-point-empty-returns-empty-string ()
  "Returns \"\" without prompting when no candidates are available."
  (let ((kill-ring nil))
    (with-temp-buffer
      ;; fundamental-mode, empty buffer, no kill ring, no region
      (should (equal "" (completing-read-context-from-point))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; annotated-completing-read--length-of-longest

(ert-deftest annotated-completing-read/length-of-longest-basic ()
  (should (= 5 (annotated-completing-read--length-of-longest '("ab" "hello" "hi")))))

(ert-deftest annotated-completing-read/length-of-longest-single-element ()
  (should (= 3 (annotated-completing-read--length-of-longest '("foo")))))

(ert-deftest annotated-completing-read/length-of-longest-all-same-length ()
  (should (= 3 (annotated-completing-read--length-of-longest '("foo" "bar" "baz")))))

(ert-deftest annotated-completing-read/length-of-longest-empty-string ()
  (should (= 0 (annotated-completing-read--length-of-longest '("")))))

(ert-deftest annotated-completing-read/length-of-longest-empty-list ()
  (should (= 0 (annotated-completing-read--length-of-longest '()))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; annotated-completing-read--prefix-padding
;; Formula: (abs (+ 4 (- longest (length key))))

(ert-deftest annotated-completing-read/prefix-padding-key-shorter-than-longest ()
  ;; key="foo" len=3, longest=10 → abs(4 + (10-3)) = 11 spaces
  (let ((pad (annotated-completing-read--prefix-padding "foo" 10)))
    (should (stringp pad))
    (should (= 11 (length pad)))
    (should (string-match-p "^ +$" pad))))

(ert-deftest annotated-completing-read/prefix-padding-key-equals-longest ()
  ;; key="foo" len=3, longest=3 → abs(4 + 0) = 4 spaces
  (should (= 4 (length (annotated-completing-read--prefix-padding "foo" 3)))))

(ert-deftest annotated-completing-read/prefix-padding-key-longer-than-longest ()
  ;; key="foobar" len=6, longest=3 → abs(4 + (3-6)) = abs(1) = 1 space
  (should (= 1 (length (annotated-completing-read--prefix-padding "foobar" 3)))))

(ert-deftest annotated-completing-read/prefix-padding-returns-spaces ()
  (should (equal "    " (annotated-completing-read--prefix-padding "foo" 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; completing-read--directory-clean

(ert-deftest annotated-completing-read/directory-clean-removes-nil ()
  "Nil entries in the input are removed."
  (let ((result (completing-read--directory-clean (list "/tmp/" nil "/usr/"))))
    (should-not (member nil result))))

(ert-deftest annotated-completing-read/directory-clean-removes-empty ()
  "Empty-string entries are removed."
  (let ((result (completing-read--directory-clean (list "/tmp/" "" "/usr/"))))
    (should-not (member "" result))))

(ert-deftest annotated-completing-read/directory-clean-removes-whitespace-only ()
  "Whitespace-only entries are removed."
  (let ((result (completing-read--directory-clean (list "/tmp/" "   " "/usr/"))))
    (should (cl-every (lambda (d) (not (string-blank-p d))) result))))

(ert-deftest annotated-completing-read/directory-clean-keeps-valid ()
  "Valid absolute paths survive cleaning."
  (let ((result (completing-read--directory-clean (list "/tmp/" "/usr/"))))
    (should (member "/tmp/" result))
    (should (member "/usr/" result))))

(ert-deftest annotated-completing-read/directory-clean-empty-input ()
  "Empty input list returns nil."
  (should (null (completing-read--directory-clean nil))))

(ert-deftest annotated-completing-read/directory-clean-all-nil ()
  "All-nil input returns nil."
  (should (null (completing-read--directory-clean (list nil nil nil)))))

(ert-deftest annotated-completing-read/directory-clean-deduplicates ()
  "Duplicate paths are removed."
  (let ((result (completing-read--directory-clean (list "/tmp/" "/tmp/" "/usr/"))))
    (should (= (length result) (length (cl-remove-duplicates result :test #'equal))))))

(ert-deftest annotated-completing-read/directory-clean-expands-relative ()
  "Relative paths are expanded to absolute paths."
  (let* ((default-directory "/tmp/")
         (result (completing-read--directory-clean (list "subdir"))))
    (should (cl-every #'f-absolute-p result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; completing-read--directory-parents

(defmacro acr-directory-test--with-temp-tree (root-var dirs &rest body)
  "Bind ROOT-VAR to a temp directory, create DIRS under it, run BODY, clean up."
  (declare (indent 2))
  `(let ((,root-var (file-name-as-directory (make-temp-file "acr-dir-test" t))))
     (unwind-protect
         (progn
           (dolist (d (list ,@dirs))
             (make-directory (expand-file-name d ,root-var) t))
           ,@body)
       (delete-directory ,root-var t))))

(ert-deftest annotated-completing-read/directory-parents-includes-start ()
  "The starting directory appears in the output."
  (acr-directory-test--with-temp-tree root ("a/b/c")
    (let* ((start (file-name-as-directory (expand-file-name "a/b/c" root)))
           (result (completing-read--directory-parents start root)))
      (should (cl-some (lambda (d) (f-equal-p d start)) result)))))

(ert-deftest annotated-completing-read/directory-parents-includes-stop ()
  "The stop directory appears in the output."
  (acr-directory-test--with-temp-tree root ("a/b")
    (let* ((start (file-name-as-directory (expand-file-name "a/b" root)))
           (result (completing-read--directory-parents start root)))
      (should (cl-some (lambda (d) (f-equal-p d root)) result)))))

(ert-deftest annotated-completing-read/directory-parents-returns-list ()
  "The function returns a list."
  (let ((result (completing-read--directory-parents (expand-file-name "~/") (expand-file-name "~/"))))
    (should (listp result))))

(ert-deftest annotated-completing-read/directory-parents-start-equals-stop ()
  "When start and stop are the same, at most one entry is returned."
  (let* ((dir (file-name-as-directory temporary-file-directory))
         (result (completing-read--directory-parents dir dir)))
    (should (listp result))
    (should (<= (length result) 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; completing-read-directory annotation labels

(ert-deftest annotated-completing-read/directory-labels-current ()
  "The current directory is labelled 'current directory'."
  (let* ((dir (expand-file-name "/tmp/"))
         (default-directory dir))
    (cl-letf (((symbol-function 'approximate-project-root) (lambda () dir))
              ((symbol-function 'annotated-completing-read)
               (lambda (tbl &rest _)
                 (should (equal "current directory" (ht-get tbl dir)))
                 dir)))
      (completing-read-directory :candidates (list dir)))))

(ert-deftest annotated-completing-read/directory-labels-project-root ()
  "A directory matching the project root is labelled 'project root'."
  (let* ((root (expand-file-name "/tmp/project/"))
         (other (expand-file-name "/tmp/other/"))
         (default-directory other))
    (cl-letf (((symbol-function 'approximate-project-root) (lambda () root))
              ((symbol-function 'annotated-completing-read)
               (lambda (tbl &rest _)
                 (should (equal "project root" (ht-get tbl root)))
                 root)))
      (completing-read-directory :candidates (list root)))))

(ert-deftest annotated-completing-read/directory-labels-parent ()
  "A directory that is an ancestor of the current dir is labelled 'parent'."
  (let* ((parent (expand-file-name "/tmp/"))
         (child (expand-file-name "/tmp/sub/"))
         (default-directory child))
    (cl-letf (((symbol-function 'approximate-project-root) (lambda () parent))
              ((symbol-function 'annotated-completing-read)
               (lambda (tbl &rest _)
                 (should (member (ht-get tbl parent) '("project root" "parent")))
                 parent)))
      (completing-read-directory :candidates (list parent)))))

(ert-deftest annotated-completing-read/directory-prompt-forwarded ()
  "The :prompt keyword is forwarded to annotated-completing-read."
  (let* ((dir (expand-file-name "/tmp/"))
         (default-directory dir)
         received-prompt)
    (cl-letf (((symbol-function 'approximate-project-root) (lambda () dir))
              ((symbol-function 'annotated-completing-read)
               (lambda (_tbl &rest args)
                 (setq received-prompt (plist-get args :prompt))
                 dir)))
      (completing-read-directory :candidates (list dir) :prompt "pick: ")
      (should (equal "pick: " received-prompt)))))

(ert-deftest annotated-completing-read/directory-candidates-override ()
  "When :candidates is provided, default candidates are not computed."
  (let* ((dir (expand-file-name "/tmp/"))
         (default-directory dir)
         received-table)
    (cl-letf (((symbol-function 'approximate-project-root) (lambda () dir))
              ((symbol-function 'completing-read--directory-default-candidates)
               (lambda () (error "should not be called")))
              ((symbol-function 'annotated-completing-read)
               (lambda (tbl &rest _)
                 (setq received-table tbl)
                 dir)))
      (completing-read-directory :candidates (list dir))
      (should (ht-contains-p received-table dir)))))

(ert-deftest annotated-completing-read/directory-no-groups-below-threshold ()
  "No :group-name is passed when there are 8 or fewer candidates."
  (let* ((dirs (--map (format "/tmp/dir%d/" it) (number-sequence 1 8)))
         (default-directory "/tmp/dir1/")
         received-group-name)
    (cl-letf (((symbol-function 'approximate-project-root) (lambda () "/tmp/dir1/"))
              ((symbol-function 'annotated-completing-read)
               (lambda (_tbl &rest args)
                 (setq received-group-name (plist-get args :group-name))
                 "/tmp/dir1/")))
      (completing-read-directory :candidates dirs)
      (should (null received-group-name)))))

(ert-deftest annotated-completing-read/directory-groups-above-threshold ()
  ":group-name is a function when there are more than 8 candidates."
  (let* ((dirs (--map (format "/tmp/dir%d/" it) (number-sequence 1 9)))
         (default-directory "/tmp/dir1/")
         received-group-name)
    (cl-letf (((symbol-function 'approximate-project-root) (lambda () "/tmp/dir1/"))
              ((symbol-function 'annotated-completing-read)
               (lambda (_tbl &rest args)
                 (setq received-group-name (plist-get args :group-name))
                 "/tmp/dir1/")))
      (completing-read-directory :candidates dirs)
      (should (functionp received-group-name)))))

(ert-deftest annotated-completing-read/directory-group-labels ()
  "The group function returns the relationship label directly."
  (let* ((root "/tmp/project/")
         (current "/tmp/project/src/")
         (parent "/tmp/")
         (child "/tmp/project/src/sub/")
         (sibling "/tmp/project/lib/")
         (other "/home/user/")
         (dirs (list root current parent child sibling other
                     "/a/" "/b/" "/c/"))  ; pad to >8
         (default-directory current)
         received-group-fn)
    (cl-letf (((symbol-function 'approximate-project-root) (lambda () root))
              ((symbol-function 'completing-read--directory-entry-counts) (lambda (_) ""))
              ((symbol-function 'annotated-completing-read)
               (lambda (_tbl &rest args)
                 (setq received-group-fn (plist-get args :group-name))
                 current)))
      (completing-read-directory :candidates dirs)
      (should (functionp received-group-fn))
      (should (equal "current directory" (funcall received-group-fn current)))
      (should (equal "project root"      (funcall received-group-fn root)))
      (should (equal "child"             (funcall received-group-fn child)))
      (should (equal "parent"            (funcall received-group-fn parent)))
      (should (equal "other"             (funcall received-group-fn other))))))

(ert-deftest annotated-completing-read/directory-grouped-annotation-is-counts ()
  "When grouped, the table passed to annotated-completing-read contains entry counts."
  (let* ((dirs (--map (format "/tmp/dir%d/" it) (number-sequence 1 9)))
         (default-directory "/tmp/dir1/")
         received-table)
    (cl-letf (((symbol-function 'approximate-project-root) (lambda () "/tmp/dir1/"))
              ((symbol-function 'completing-read--directory-entry-counts)
               (lambda (d) (format "counts:%s" d)))
              ((symbol-function 'annotated-completing-read)
               (lambda (tbl &rest _)
                 (setq received-table tbl)
                 "/tmp/dir1/")))
      (completing-read-directory :candidates dirs)
      (should (ht-p received-table))
      (should (string-prefix-p "counts:" (ht-get received-table "/tmp/dir1/"))))))

(ert-deftest annotated-completing-read/directory-ungrouped-annotation-is-relationship ()
  "When not grouped (<=8 items), the table contains relationship labels."
  (let* ((root "/tmp/project/")
         (current "/tmp/project/src/")
         (dirs (list root current "/a/" "/b/"))
         (default-directory current)
         received-table)
    (cl-letf (((symbol-function 'approximate-project-root) (lambda () root))
              ((symbol-function 'annotated-completing-read)
               (lambda (tbl &rest _)
                 (setq received-table tbl)
                 current)))
      (completing-read-directory :candidates dirs)
      (should (equal "project root"      (ht-get received-table root)))
      (should (equal "current directory" (ht-get received-table current))))))

(provide 'test-annotated-completing-read)
;;; test-annotated-completing-read.el ends here
