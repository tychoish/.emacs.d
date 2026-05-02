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

(provide 'test-annotated-completing-read)
;;; test-annotated-completing-read.el ends here
