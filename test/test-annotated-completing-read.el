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

(provide 'test-annotated-completing-read)
;;; test-annotated-completing-read.el ends here
