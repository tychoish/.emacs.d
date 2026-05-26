;;; test-xtdlib.el --- ERT tests for xtdlib.el -*- lexical-binding: t -*-

;; Run inside a live Emacs session with full config loaded:
;;   M-x ert RET t RET
;; or filtered:
;;   (ert "^xtdlib/")

(require 'ert)
(require 'cl-lib)
(require 'xtdlib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; String helpers

;;; -filter-s-trim

(ert-deftest xtdlib/-filter-s-trim-basic ()
  (should (equal '("foo" "bar") (-filter-s-trim '("foo" "bar")))))

(ert-deftest xtdlib/-filter-s-trim-trims-whitespace ()
  (should (equal '("foo" "bar") (-filter-s-trim '("  foo  " " bar")))))

(ert-deftest xtdlib/-filter-s-trim-removes-non-strings ()
  (should (equal '("foo") (-filter-s-trim '("foo" nil 42 t)))))

(ert-deftest xtdlib/-filter-s-trim-removes-empty-strings ()
  (should (equal '("foo") (-filter-s-trim '("foo" "" "  ")))))

(ert-deftest xtdlib/-filter-s-trim-empty-list ()
  (should (equal nil (-filter-s-trim '()))))

(ert-deftest xtdlib/-filter-s-trim-rejects-non-list ()
  (should-error (-filter-s-trim "not-a-list") :type 'wrong-type-argument))

(ert-deftest xtdlib/-filter-s-trim-rejects-string-arg ()
  (should-error (-filter-s-trim 42) :type 'wrong-type-argument))

;;; s-or-char-equal

(ert-deftest xtdlib/s-or-char-equal-char-vs-same-char ()
  (should (s-or-char-equal ?a ?a)))

(ert-deftest xtdlib/s-or-char-equal-char-vs-different-char ()
  (should-not (s-or-char-equal ?a ?b)))

(ert-deftest xtdlib/s-or-char-equal-char-vs-matching-string ()
  (should (s-or-char-equal ?a "a")))

(ert-deftest xtdlib/s-or-char-equal-char-vs-non-matching-string ()
  (should-not (s-or-char-equal ?a "b")))

(ert-deftest xtdlib/s-or-char-equal-non-char-first-arg-errors ()
  (should-error (s-or-char-equal "a" "a") :type 'wrong-type-argument))

;;; s-shortest

(ert-deftest xtdlib/s-shortest-returns-first-when-shorter ()
  (should (equal "ab" (s-shortest "ab" "abc"))))

(ert-deftest xtdlib/s-shortest-returns-second-when-shorter ()
  (should (equal "a" (s-shortest "ab" "a"))))

(ert-deftest xtdlib/s-shortest-returns-first-when-equal-length ()
  (should (equal "ab" (s-shortest "ab" "cd"))))

(ert-deftest xtdlib/s-shortest-empty-string ()
  (should (equal "" (s-shortest "" "abc"))))

;;; s-collapse-hyphens

(ert-deftest xtdlib/s-collapse-hyphens-collapses-triple ()
  (should (equal "foo-bar" (s-collapse-hyphens "foo---bar"))))

(ert-deftest xtdlib/s-collapse-hyphens-collapses-many ()
  (should (equal "foo-bar" (s-collapse-hyphens "foo-----bar"))))

(ert-deftest xtdlib/s-collapse-hyphens-leaves-double-unchanged ()
  (should (equal "foo--bar" (s-collapse-hyphens "foo--bar"))))

(ert-deftest xtdlib/s-collapse-hyphens-leaves-single-unchanged ()
  (should (equal "foo-bar" (s-collapse-hyphens "foo-bar"))))

(ert-deftest xtdlib/s-collapse-hyphens-no-hyphens ()
  (should (equal "foobar" (s-collapse-hyphens "foobar"))))

(ert-deftest xtdlib/s-collapse-hyphens-all-hyphens ()
  (should (equal "-" (s-collapse-hyphens "---"))))

;;; s-trimmed-or-nil

(ert-deftest xtdlib/s-trimmed-or-nil-returns-trimmed-string ()
  (should (equal "foo" (s-trimmed-or-nil "  foo  "))))

(ert-deftest xtdlib/s-trimmed-or-nil-already-trimmed ()
  (should (equal "foo" (s-trimmed-or-nil "foo"))))

(ert-deftest xtdlib/s-trimmed-or-nil-empty-string-returns-nil ()
  (should-not (s-trimmed-or-nil "")))

(ert-deftest xtdlib/s-trimmed-or-nil-whitespace-only-returns-nil ()
  (should-not (s-trimmed-or-nil "   ")))

(ert-deftest xtdlib/s-trimmed-or-nil-nil-returns-nil ()
  (should-not (s-trimmed-or-nil nil)))

(ert-deftest xtdlib/s-trimmed-or-nil-non-string-returns-nil ()
  (should-not (s-trimmed-or-nil 42))
  (should-not (s-trimmed-or-nil t))
  (should-not (s-trimmed-or-nil '(a b))))

;;; s-trim-non-word-chars

(ert-deftest xtdlib/s-trim-non-word-chars-trims-punctuation ()
  (should (equal "foo" (s-trim-non-word-chars "...foo..."))))

(ert-deftest xtdlib/s-trim-non-word-chars-keeps-inner-non-word ()
  (should (equal "foo-bar" (s-trim-non-word-chars "...foo-bar..."))))

(ert-deftest xtdlib/s-trim-non-word-chars-already-clean ()
  (should (equal "foo" (s-trim-non-word-chars "foo"))))

(ert-deftest xtdlib/s-trim-non-word-chars-nil-returns-nil ()
  (should-not (s-trim-non-word-chars nil)))

(ert-deftest xtdlib/s-trim-non-word-chars-non-string-returns-nil ()
  (should-not (s-trim-non-word-chars 42)))

;;; s-blank-p (was s-contains-whitespace-p)

(ert-deftest xtdlib/s-blank-p-all-whitespace-returns-t ()
  (should (s-blank-p "   ")))

(ert-deftest xtdlib/s-blank-p-empty-string-returns-t ()
  (should (s-blank-p "")))

(ert-deftest xtdlib/s-blank-p-non-whitespace-returns-nil ()
  (should-not (s-blank-p "foo")))

(ert-deftest xtdlib/s-blank-p-mixed-content-returns-nil ()
  (should-not (s-blank-p "  foo  ")))

(ert-deftest xtdlib/s-blank-p-non-string-returns-nil ()
  (should-not (s-blank-p nil))
  (should-not (s-blank-p 42)))

(ert-deftest xtdlib/s-contains-whitespace-p-is-alias-for-s-blank-p ()
  (should (fboundp 's-contains-whitespace-p))
  (should (s-contains-whitespace-p ""))
  (should (s-contains-whitespace-p "  "))
  (should-not (s-contains-whitespace-p "foo")))


;;; s-default

(ert-deftest xtdlib/s-default-returns-input-when-non-empty ()
  (should (equal "bar" (s-default "default" "bar"))))

(ert-deftest xtdlib/s-default-returns-default-for-nil ()
  (should (equal "default" (s-default "default" nil))))

(ert-deftest xtdlib/s-default-returns-default-for-empty-string ()
  (should (equal "default" (s-default "default" ""))))

(ert-deftest xtdlib/s-default-returns-default-when-input-equals-default ()
  (should (equal "foo" (s-default "foo" "foo"))))

;;; s-number-word

(ert-deftest xtdlib/s-number-word-boundaries ()
  (should (equal "one"    (s-number-word 1)))
  (should (equal "ten"    (s-number-word 10)))
  (should (equal "twenty" (s-number-word 20))))

(ert-deftest xtdlib/s-number-word-mid-range ()
  (should (equal "five"     (s-number-word 5)))
  (should (equal "thirteen" (s-number-word 13)))
  (should (equal "nineteen" (s-number-word 19))))

(ert-deftest xtdlib/s-number-word-zero-errors ()
  (should-error (s-number-word 0) :type 'user-error))

(ert-deftest xtdlib/s-number-word-out-of-range-errors ()
  (should-error (s-number-word 21) :type 'user-error)
  (should-error (s-number-word 100) :type 'user-error))

(ert-deftest xtdlib/s-number-word-exact-spellings ()
  (should (equal "one"       (s-number-word 1)))
  (should (equal "two"       (s-number-word 2)))
  (should (equal "three"     (s-number-word 3)))
  (should (equal "four"      (s-number-word 4)))
  (should (equal "five"      (s-number-word 5)))
  (should (equal "six"       (s-number-word 6)))
  (should (equal "seven"     (s-number-word 7)))
  (should (equal "eight"     (s-number-word 8)))
  (should (equal "nine"      (s-number-word 9)))
  (should (equal "ten"       (s-number-word 10)))
  (should (equal "eleven"    (s-number-word 11)))
  (should (equal "twelve"    (s-number-word 12)))
  (should (equal "thirteen"  (s-number-word 13)))
  (should (equal "fourteen"  (s-number-word 14)))
  (should (equal "fifteen"   (s-number-word 15)))
  (should (equal "sixteen"   (s-number-word 16)))
  (should (equal "seventeen" (s-number-word 17)))
  (should (equal "eighteen"  (s-number-word 18)))
  (should (equal "nineteen"  (s-number-word 19)))
  (should (equal "twenty"    (s-number-word 20))))

;;; generated join functions

(ert-deftest xtdlib/s-join-with-hyphen-basic ()
  (should (equal "foo-bar-baz" (s-join-with-hyphen "foo" "bar" "baz"))))

(ert-deftest xtdlib/s-join-with-hyphen-filters-empty ()
  (should (equal "foo-bar" (s-join-with-hyphen "foo" nil "" "bar"))))

(ert-deftest xtdlib/s-join-with-kebab-is-same-as-hyphen ()
  (should (equal (s-join-with-hyphen "foo" "bar")
                 (s-join-with-kebab "foo" "bar"))))

(ert-deftest xtdlib/s-join-with-underscore-basic ()
  (should (equal "foo_bar" (s-join-with-underscore "foo" "bar"))))

(ert-deftest xtdlib/s-join-with-snake-is-same-as-underscore ()
  (should (equal (s-join-with-underscore "foo" "bar")
                 (s-join-with-snake "foo" "bar"))))

(ert-deftest xtdlib/s-join-with-space-basic ()
  (should (equal "foo bar" (s-join-with-space "foo" "bar"))))

(ert-deftest xtdlib/s-join-with-spc-is-same-as-space ()
  (should (equal (s-join-with-space "foo" "bar")
                 (s-join-with-spc "foo" "bar"))))

(ert-deftest xtdlib/s-join-with-pipe-adds-space-padding ()
  (should (equal "foo | bar" (s-join-with-pipe "foo" "bar"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Numeric helpers

(ert-deftest xtdlib/larger-basic ()
  (should (= 5 (larger 3 5)))
  (should (= 5 (larger 5 3))))

(ert-deftest xtdlib/larger-equal-returns-second ()
  (should (= 3 (larger 3 3))))

(ert-deftest xtdlib/larger-nil-treated-as-zero ()
  (should (= 0 (larger nil nil)))
  (should (= 5 (larger nil 5)))
  (should (= 5 (larger 5 nil))))

(ert-deftest xtdlib/larger-non-number-treated-as-zero ()
  (should (= 5 (larger "string" 5)))
  (should (= 5 (larger 5 "string"))))

(ert-deftest xtdlib/larger-negative-numbers ()
  (should (= -1 (larger -1 -5))))

(ert-deftest xtdlib/smaller-basic ()
  (should (= 3 (smaller 3 5)))
  (should (= 3 (smaller 5 3))))

(ert-deftest xtdlib/smaller-nil-treated-as-zero ()
  (should (= 0 (smaller nil nil)))
  (should (= 0 (smaller nil 5)))
  (should (= 0 (smaller 5 nil))))

(ert-deftest xtdlib/smaller-non-number-treated-as-zero ()
  (should (= 0 (smaller "string" 5))))

(ert-deftest xtdlib/smaller-negative-numbers ()
  (should (= -5 (smaller -1 -5))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; make-add-to-list-fn

(defvar xtdlib--test-add-to-list-target nil)

(ert-deftest xtdlib/make-add-to-list-fn-returns-lambda ()
  (should (functionp (make-add-to-list-fn xtdlib--test-add-to-list-target))))

(ert-deftest xtdlib/make-add-to-list-fn-unquoted-adds-item ()
  (setq xtdlib--test-add-to-list-target nil)
  (funcall (make-add-to-list-fn xtdlib--test-add-to-list-target) "a")
  (should (member "a" xtdlib--test-add-to-list-target)))

(ert-deftest xtdlib/make-add-to-list-fn-quoted-adds-item ()
  (setq xtdlib--test-add-to-list-target nil)
  (funcall (make-add-to-list-fn 'xtdlib--test-add-to-list-target) "a")
  (should (member "a" xtdlib--test-add-to-list-target)))

(ert-deftest xtdlib/make-add-to-list-fn-quoted-and-unquoted-same-result ()
  (setq xtdlib--test-add-to-list-target nil)
  (funcall (make-add-to-list-fn xtdlib--test-add-to-list-target) "a")
  (let ((unquoted-result xtdlib--test-add-to-list-target))
    (setq xtdlib--test-add-to-list-target nil)
    (funcall (make-add-to-list-fn 'xtdlib--test-add-to-list-target) "a")
    (should (equal unquoted-result xtdlib--test-add-to-list-target))))

(ert-deftest xtdlib/make-add-to-list-fn-no-duplicates ()
  (setq xtdlib--test-add-to-list-target '("a"))
  (funcall (make-add-to-list-fn xtdlib--test-add-to-list-target) "a")
  (should (= 1 (length xtdlib--test-add-to-list-target))))

(ert-deftest xtdlib/make-add-to-list-fn-prepends-by-default ()
  (setq xtdlib--test-add-to-list-target '("b"))
  (funcall (make-add-to-list-fn xtdlib--test-add-to-list-target) "a")
  (should (equal "a" (car xtdlib--test-add-to-list-target))))

(ert-deftest xtdlib/make-add-to-list-fn-append-adds-to-end ()
  (setq xtdlib--test-add-to-list-target '("a"))
  (funcall (make-add-to-list-fn xtdlib--test-add-to-list-target :append t) "b")
  (should (equal "b" (car (last xtdlib--test-add-to-list-target)))))

(ert-deftest xtdlib/make-add-to-list-fn-mapc-appends-all ()
  (setq xtdlib--test-add-to-list-target nil)
  (mapc (make-add-to-list-fn xtdlib--test-add-to-list-target :append t)
        '("a" "b" "c"))
  (should (equal '("a" "b" "c") xtdlib--test-add-to-list-target)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dash extensions

(ert-deftest xtdlib/-distinct-by-car-removes-duplicate-car ()
  (let ((result (-distinct-by-car '((a . 1) (a . 2)))))
    (should (= 1 (length result)))
    (should (eq 'a (caar result)))))

(ert-deftest xtdlib/-distinct-by-car-preserves-unique-keys ()
  (let ((result (-distinct-by-car '((a . 1) (b . 2) (c . 3)))))
    (should (= 3 (length result)))))

(ert-deftest xtdlib/-distinct-by-car-mixed ()
  (let ((result (-distinct-by-car '((a . 1) (b . 2) (a . 9)))))
    (should (= 2 (length result)))
    (should (assq 'a result))
    (should (assq 'b result))))

(ert-deftest xtdlib/-distinct-by-car-empty-list ()
  (should (equal nil (-distinct-by-car '()))))

(ert-deftest xtdlib/-unwind-nested-lists ()
  (should (equal '(1 2 3 4) (-unwind '((1 2) (3 4))))))

(ert-deftest xtdlib/-unwind-empty-list ()
  (should (equal nil (-unwind '()))))

(ert-deftest xtdlib/-unwind-single-level-only ()
  ;; flattens exactly one level, not recursively
  (should (equal '((1 2) (3 4)) (-unwind '(((1 2)) ((3 4)))))))

(ert-deftest xtdlib/-sparse-append-filters-nils ()
  (should (equal '(1 2 3) (-sparse-append '(1 nil 2) '(nil 3)))))

(ert-deftest xtdlib/-sparse-append-all-nil ()
  (should (equal nil (-sparse-append '(nil) '(nil nil)))))

(ert-deftest xtdlib/-sparse-append-no-nils ()
  (should (equal '(1 2 3) (-sparse-append '(1 2) '(3)))))

(ert-deftest xtdlib/-map-in-place-modifies-list ()
  (let ((lst (list 1 2 3)))
    (-map-in-place #'1+ lst)
    (should (equal '(2 3 4) lst))))

(ert-deftest xtdlib/-map-in-place-returns-same-list ()
  (let ((lst (list 1 2 3)))
    (should (eq lst (-map-in-place #'identity lst)))))

(ert-deftest xtdlib/-map-in-place-identity-preserves-values ()
  (let ((lst (list 1 2 3)))
    (-map-in-place #'identity lst)
    (should (equal '(1 2 3) lst))))

(ert-deftest xtdlib/-in-place-returns-count ()
  (let ((lst (list 1 2 3)))
    (should (= 3 (-in-place #'1+ lst)))))

(ert-deftest xtdlib/-in-place-modifies-list ()
  (let ((lst (list 10 20 30)))
    (-in-place #'1+ lst)
    (should (equal '(11 21 31) lst))))

(ert-deftest xtdlib/-in-place-empty-list-returns-zero ()
  (should (= 0 (-in-place #'identity nil))))

(ert-deftest xtdlib/-map-uniq-removes-duplicates ()
  (let ((result (-map-uniq #'identity '(1 2 1 3 2))))
    (should (= 3 (length result)))
    (should (member 1 result))
    (should (member 2 result))
    (should (member 3 result))))

(ert-deftest xtdlib/-map-uniq-with-transform ()
  ;; maps strings to their first character; "foo" and "fig" both → "f"
  (let ((result (-map-uniq (lambda (s) (substring s 0 1)) '("foo" "bar" "fig"))))
    (should (= 2 (length result)))))

(ert-deftest xtdlib/-map-uniq-all-unique ()
  (should (= 3 (length (-map-uniq #'identity '(1 2 3))))))

;;; anaphoric macros

(ert-deftest xtdlib/--mapc-executes-for-each-element ()
  (let ((acc nil))
    (--mapc (push it acc) '(1 2 3))
    (should (equal '(3 2 1) acc))))

(ert-deftest xtdlib/--mapc-empty-list ()
  (let ((ran nil))
    (--mapc (setq ran t) '())
    (should-not ran)))

(ert-deftest xtdlib/--flat-map-basic ()
  (should (equal '(1 2 2 3) (--flat-map (list it (1+ it)) '(1 2)))))

(ert-deftest xtdlib/--flat-map-empty-list ()
  (should (equal nil (--flat-map (list it) '()))))

(ert-deftest xtdlib/--map-in-place-modifies-via-it ()
  (let ((lst (list 1 2 3)))
    (--map-in-place (* it 2) lst)
    (should (equal '(2 4 6) lst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ht extensions

(ert-deftest xtdlib/ht-get-lambda-retrieves-value ()
  (let* ((tbl (make-hash-table :test #'equal))
         (getter (ht-get-lambda tbl)))
    (ht-set tbl "key" "val")
    (should (equal "val" (funcall getter "key")))))

(ert-deftest xtdlib/ht-get-lambda-missing-key-returns-nil ()
  (let* ((tbl (make-hash-table :test #'equal))
         (getter (ht-get-lambda tbl)))
    (should-not (funcall getter "missing"))))

(ert-deftest xtdlib/ht-set-lambda-stores-value ()
  (let* ((tbl (make-hash-table :test #'equal))
         (setter (ht-set-lambda tbl)))
    (funcall setter "key" "val")
    (should (equal "val" (ht-get tbl "key")))))

(ert-deftest xtdlib/ht-contains-p-lambda-found ()
  (let* ((tbl (make-hash-table :test #'equal))
         (pred (ht-contains-p-lambda tbl)))
    (ht-set tbl "key" "val")
    (should (funcall pred "key"))))

(ert-deftest xtdlib/ht-contains-p-lambda-not-found ()
  (let* ((tbl (make-hash-table :test #'equal))
         (pred (ht-contains-p-lambda tbl)))
    (should-not (funcall pred "missing"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; option-set-p

(ert-deftest xtdlib/option-set-p-single-symbol-match ()
  (should (option-set-p 'foo 'foo)))

(ert-deftest xtdlib/option-set-p-single-symbol-no-match ()
  (should-not (option-set-p 'foo 'bar)))

(ert-deftest xtdlib/option-set-p-list-contains-opt ()
  (should (option-set-p 'foo '(bar foo baz))))

(ert-deftest xtdlib/option-set-p-list-missing-opt ()
  (should-not (option-set-p 'foo '(bar baz))))

(ert-deftest xtdlib/option-set-p-nil-options ()
  (should-not (option-set-p 'foo nil)))

(ert-deftest xtdlib/option-set-p-empty-list ()
  (should-not (option-set-p 'foo '())))

(ert-deftest xtdlib/option-set-p-single-element-list-match ()
  (should (option-set-p 'foo '(foo))))

(ert-deftest xtdlib/option-set-p-uses-eq-not-equal ()
  ;; only works with symbols (eq), not equal strings
  (should-not (option-set-p "foo" "foo")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility macros

(ert-deftest xtdlib/disabled-never-executes-body ()
  (let ((ran nil))
    (disabled (setq ran t))
    (should-not ran)))

(ert-deftest xtdlib/disabled-with-multiple-forms ()
  (let ((count 0))
    (disabled
     (cl-incf count)
     (cl-incf count)
     (cl-incf count))
    (should (= 0 count))))

(ert-deftest xtdlib/pos-arg-returns-is-value ()
  (should (= 42 (pos-arg "arg" :is 42))))

(ert-deftest xtdlib/pos-arg-symbol-name ()
  (should (equal "hello" (pos-arg my-arg :is "hello"))))

(ert-deftest xtdlib/pos-arg-nil-is-value ()
  (should (eq nil (pos-arg "arg" :is nil))))

(ert-deftest xtdlib/pos-arg-non-string-non-symbol-name-errors ()
  (should-error (pos-arg 42 :is "val") :type 'user-error))

(ert-deftest xtdlib/pa-alias-works ()
  (should (= 99 (pa "x" :is 99))))

(ert-deftest xtdlib/with-slow-op-timer-returns-body-value ()
  (should (equal 42 (with-slow-op-timer "test" 42))))

(ert-deftest xtdlib/with-slow-op-timer-executes-body ()
  (let ((ran nil))
    (with-slow-op-timer "test" (setq ran t))
    (should ran)))

(ert-deftest xtdlib/with-slow-op-timer-returns-last-form ()
  (should (equal "last" (with-slow-op-timer "test"
                          "first"
                          "last"))))

(ert-deftest xtdlib/with-force-write-allows-insertion-in-read-only-buffer ()
  (with-temp-buffer
    (setq buffer-read-only t)
    (with-force-write
      (insert "hello"))
    (should (equal "hello" (buffer-string)))))

(ert-deftest xtdlib/with-force-write-restores-read-only ()
  (with-temp-buffer
    (setq buffer-read-only t)
    (with-force-write
      (insert "x"))
    (should buffer-read-only)))

(ert-deftest xtdlib/with-force-write-returns-body-value ()
  (with-temp-buffer
    (setq buffer-read-only t)
    (should (equal 99 (with-force-write 99)))))

(ert-deftest xtdlib/with-default-directory-sets-path ()
  (let (captured)
    (with-default-directory "/tmp"
      (setq captured default-directory))
    (should (equal "/tmp" captured))))

(ert-deftest xtdlib/with-default-directory-restores-original ()
  (let ((orig default-directory))
    (with-default-directory "/tmp" nil)
    (should (equal orig default-directory))))

(ert-deftest xtdlib/with-silence-sets-inhibit-message ()
  (let ((inhibit-message nil))
    (with-silence
      (should inhibit-message))))

(ert-deftest xtdlib/with-silence-sets-message-log-max-nil ()
  (let ((message-log-max 100))
    (with-silence
      (should (eq nil message-log-max)))))

(ert-deftest xtdlib/with-silence-restores-after-body ()
  (let ((inhibit-message nil))
    (with-silence nil)
    (should-not inhibit-message)))

(ert-deftest xtdlib/with-quiet-sets-inhibit-message ()
  (let ((inhibit-message nil))
    (with-quiet
      (should inhibit-message))))

(ert-deftest xtdlib/with-quiet-does-not-touch-message-log-max ()
  (let ((message-log-max 100))
    (with-quiet
      (should (= 100 message-log-max)))))

(ert-deftest xtdlib/with-prefix-arg-sets-current-prefix-arg ()
  (let (captured)
    (with-prefix-arg 4
      (setq captured current-prefix-arg))
    (should (= 4 captured))))

(ert-deftest xtdlib/with-prefix-arg-restores-original ()
  (let ((current-prefix-arg nil))
    (with-prefix-arg 4 nil)
    (should (eq nil current-prefix-arg))))

(ert-deftest xtdlib/with-prefix-arg-nil-value ()
  (let ((current-prefix-arg 4))
    (let (captured)
      (with-prefix-arg nil
        (setq captured current-prefix-arg))
      (should (eq nil captured)))))

(ert-deftest xtdlib/with-timer-executes-body ()
  (let ((ran nil))
    (with-timer "test" (setq ran t))
    (should ran)))

(ert-deftest xtdlib/compile-buffer-name-returns-callable ()
  (should (functionp (compile-buffer-name "my-buffer"))))

(ert-deftest xtdlib/compile-buffer-name-returned-fn-returns-name ()
  (should (equal "my-buf" (funcall (compile-buffer-name "my-buf")))))

(ert-deftest xtdlib/compile-buffer-name-returned-fn-ignores-optional-arg ()
  (should (equal "my-buf" (funcall (compile-buffer-name "my-buf") "ignored"))))

;;; with-toggle-once

(defvar xtdlib--test-toggle-state nil)

(ert-deftest xtdlib/with-toggle-once-body-runs-on-first-call ()
  (let ((count 0))
    (with-toggle-once xtdlib--test-once-basic
      (cl-incf count))
    (setq xtdlib--test-once-basic-toggle-state nil)
    (xtdlib--test-once-basic)
    (should (= 1 count))))

(ert-deftest xtdlib/with-toggle-once-body-does-not-run-again ()
  (let ((count 0))
    (with-toggle-once xtdlib--test-once-idempotent
      (cl-incf count))
    (setq xtdlib--test-once-idempotent-toggle-state nil)
    (xtdlib--test-once-idempotent)
    (xtdlib--test-once-idempotent)
    (xtdlib--test-once-idempotent)
    (should (= 1 count))))

(ert-deftest xtdlib/with-toggle-once-defines-a-function ()
  (with-toggle-once xtdlib--test-once-fn-exists nil)
  (should (fboundp 'xtdlib--test-once-fn-exists)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; add-one-shot-hook

(defvar xtdlib--test-hook-var nil
  "Scratch hook variable for add-one-shot-hook tests.")

(ert-deftest xtdlib/add-one-shot-hook-runs-once ()
  (setq xtdlib--test-hook-var nil)
  (let ((fire-count 0))
    (add-one-shot-hook
     :name "xtdlib-test-count"
     :hook 'xtdlib--test-hook-var
     :form (cl-incf fire-count))
    (run-hooks 'xtdlib--test-hook-var)
    (run-hooks 'xtdlib--test-hook-var)
    (run-hooks 'xtdlib--test-hook-var)
    (should (= 1 fire-count))))

(ert-deftest xtdlib/add-one-shot-hook-removes-itself-after-firing ()
  (setq xtdlib--test-hook-var nil)
  (add-one-shot-hook
   :name "xtdlib-test-removal"
   :hook 'xtdlib--test-hook-var
   :form t)
  (should xtdlib--test-hook-var)
  (run-hooks 'xtdlib--test-hook-var)
  (should-not xtdlib--test-hook-var))

(ert-deftest xtdlib/add-one-shot-hook-adds-to-hook ()
  (setq xtdlib--test-hook-var nil)
  (add-one-shot-hook
   :name "xtdlib-test-add"
   :hook 'xtdlib--test-hook-var
   :form t)
  (should xtdlib--test-hook-var))

(ert-deftest xtdlib/add-one-shot-hook-persist-runs-every-time ()
  (setq xtdlib--test-hook-var nil)
  (let ((fire-count 0))
    (add-one-shot-hook
     :name "xtdlib-test-persist"
     :hook 'xtdlib--test-hook-var
     :persist t
     :form (cl-incf fire-count))
    (run-hooks 'xtdlib--test-hook-var)
    (run-hooks 'xtdlib--test-hook-var)
    (run-hooks 'xtdlib--test-hook-var)
    ;; clean up
    (setq xtdlib--test-hook-var nil)
    (should (= 3 fire-count))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; f extensions (pure/path functions only — no fs I/O)

(ert-deftest xtdlib/f-make-slug-basic ()
  (should (equal "hello-world" (f-make-slug "Hello World"))))

(ert-deftest xtdlib/f-make-slug-special-characters ()
  (should (equal "foo-bar-baz" (f-make-slug "foo/bar_baz"))))

(ert-deftest xtdlib/f-make-slug-already-slug ()
  (should (equal "foo-bar" (f-make-slug "foo-bar"))))

(ert-deftest xtdlib/f-make-slug-all-special ()
  (should (string-match-p "^-+$" (f-make-slug "!@#$%"))))

(ert-deftest xtdlib/f-filename-is-p-match ()
  (should (f-filename-is-p "/some/path/foo.el" "foo.el")))

(ert-deftest xtdlib/f-filename-is-p-no-match ()
  (should-not (f-filename-is-p "/some/path/foo.el" "bar.el")))

(ert-deftest xtdlib/f-filename-is-p-different-ext ()
  (should-not (f-filename-is-p "/some/path/foo.el" "foo.elc")))

(ert-deftest xtdlib/f-collapse-homedir-replaces-home-prefix ()
  (let* ((home (expand-file-name "~/"))
         (path (concat home "projects/foo")))
    (should (equal "~/projects/foo" (f-collapse-homedir path)))))

(ert-deftest xtdlib/f-collapse-homedir-non-home-path-unchanged ()
  (should (equal "/tmp/foo/bar" (f-collapse-homedir "/tmp/foo/bar"))))

(ert-deftest xtdlib/f-when-file-exists-returns-path-when-file-exists ()
  (let ((tmp (make-temp-file "xtdlib-test-")))
    (unwind-protect
        (should (equal tmp (f-when-file-exists tmp)))
      (delete-file tmp))))

(ert-deftest xtdlib/f-when-file-exists-returns-nil-for-nonexistent ()
  (should-not (f-when-file-exists "/no/such/path/xtdlib-test-nonexistent")))

(ert-deftest xtdlib/f-visually-compress-path-truncates-components ()
  ;; path "/home/tychoish" with num=3 → each component truncated to 3
  (let ((result (f-visually-compress-path 3 "/home/tychoish")))
    (should (stringp result))
    ;; no component (except root "") should be longer than 3 chars
    (dolist (part (split-string result "/"))
      (should (<= (length part) 3)))))

(ert-deftest xtdlib/f-visually-compress-path-short-components-unchanged ()
  ;; components already <= num remain unchanged
  (let ((result (f-visually-compress-path 10 "/tmp/foo")))
    (should (string-match-p "tmp" result))
    (should (string-match-p "foo" result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; package-available-p

(ert-deftest xtdlib/package-available-p-returns-t-for-loaded-feature ()
  (provide 'xtdlib-test-fake-feature)
  (should (package-available-p 'xtdlib-test-fake-feature)))

(ert-deftest xtdlib/package-available-p-returns-nil-for-unknown-package ()
  (should-not (package-available-p 'xtdlib-test-nonexistent-package-zzz)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; approximate-project-root

(ert-deftest xtdlib/approximate-project-root-returns-string ()
  (should (stringp (approximate-project-root))))

(ert-deftest xtdlib/approximate-project-root-returns-absolute-path ()
  (should (f-absolute-p (approximate-project-root))))

(ert-deftest xtdlib/approximate-project-root-fallback-is-default-directory ()
  "When no project backend is active the result is default-directory."
  (let ((default-directory temporary-file-directory))
    (cl-letf (((symbol-function 'package-available-p) (lambda (_) nil))
              ((symbol-function 'package-installed-p) (lambda (_) nil))
              ((symbol-function 'project-current) (lambda (&rest _) nil)))
      (should (f-equal-p (expand-file-name temporary-file-directory)
                         (approximate-project-root))))))

(ert-deftest xtdlib/approximate-project-root-uses-project-el-when-available ()
  "When project.el reports a root it is returned."
  (let* ((expected "/tmp/fake-project/"))
    (cl-letf (((symbol-function 'package-available-p) (lambda (_) nil))
              ((symbol-function 'package-installed-p) (lambda (_) nil))
              ((symbol-function 'featurep)
               (lambda (sym &rest _)
                 (if (eq sym 'project) t (featurep sym))))
              ((symbol-function 'project-current) (lambda (&rest _) t))
              ((symbol-function 'project-root) (lambda (_) expected)))
      (should (equal expected (approximate-project-root))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; approximate-project-name

(ert-deftest xtdlib/approximate-project-name-returns-string ()
  (should (stringp (approximate-project-name))))

(ert-deftest xtdlib/approximate-project-name-non-empty ()
  (should (< 0 (length (approximate-project-name)))))

(ert-deftest xtdlib/approximate-project-name-fallback-is-directory-basename ()
  "When no project backend is active the result derives from default-directory."
  (let ((default-directory "/tmp/my-project/"))
    (cl-letf (((symbol-function 'package-installed-p) (lambda (_) nil))
              ((symbol-function 'featurep)
               (lambda (sym &rest _)
                 (if (eq sym 'projectile) nil (featurep sym))))
              ((symbol-function 'project-current) (lambda (&rest _) nil)))
      (should (equal "my-project" (approximate-project-name))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; approximate-project-buffers

(ert-deftest xtdlib/approximate-project-buffers-returns-list ()
  (should (listp (approximate-project-buffers))))

(ert-deftest xtdlib/approximate-project-buffers-contains-only-buffers ()
  (dolist (b (approximate-project-buffers))
    (should (bufferp b))))

(ert-deftest xtdlib/approximate-project-buffers-fallback-includes-current ()
  "When no project backend is active, the fallback includes the current buffer."
  (with-temp-buffer
    (let ((buf (current-buffer)))
      (cl-letf (((symbol-function 'package-available-p) (lambda (_) nil))
                ((symbol-function 'package-installed-p) (lambda (_) nil))
                ((symbol-function 'project-current) (lambda (&rest _) nil)))
        (should (memq buf (approximate-project-buffers)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; s-normalize-symbol-name

(ert-deftest xtdlib/s-normalize-symbol-name-spaces-to-hyphens ()
  (should (equal "hello-world" (s-normalize-symbol-name "hello world"))))

(ert-deftest xtdlib/s-normalize-symbol-name-punctuation-to-hyphens ()
  (should (equal "foo-bar" (s-normalize-symbol-name "foo=bar")))
  (should (equal "foo-bar" (s-normalize-symbol-name "foo/bar"))))

(ert-deftest xtdlib/s-normalize-symbol-name-collapses-consecutive-hyphens ()
  (should (equal "foo-bar" (s-normalize-symbol-name "foo   bar"))))

(ert-deftest xtdlib/s-normalize-symbol-name-already-normalized ()
  (should (equal "foo-bar" (s-normalize-symbol-name "foo-bar"))))

(ert-deftest xtdlib/s-normalize-symbol-name-trims-leading-and-trailing-whitespace ()
  (should (equal "foo-bar" (s-normalize-symbol-name "  foo bar  "))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -distinct-paths

(ert-deftest xtdlib/-distinct-paths-removes-duplicates ()
  (should (equal 1 (length (-distinct-paths (list "/tmp/foo" "/tmp/foo"))))))

(ert-deftest xtdlib/-distinct-paths-preserves-unique-paths ()
  (let ((paths (list "/tmp/foo" "/tmp/bar")))
    (should (equal 2 (length (-distinct-paths paths))))))

(ert-deftest xtdlib/-distinct-paths-empty-list ()
  (should (null (-distinct-paths nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -distinct-by-alist-key

(ert-deftest xtdlib/-distinct-by-alist-key-removes-duplicate-key-values ()
  (let ((input '(((name . "a") (val . 1))
                 ((name . "b") (val . 2))
                 ((name . "a") (val . 3)))))
    (should (= 2 (length (-distinct-by-alist-key 'name input))))))

(ert-deftest xtdlib/-distinct-by-alist-key-preserves-first-occurrence ()
  (let* ((input '(((name . "a") (val . 1))
                  ((name . "a") (val . 2))))
         (result (-distinct-by-alist-key 'name input)))
    (should (= 1 (alist-get 'val (car result))))))

(ert-deftest xtdlib/-distinct-by-alist-key-empty-list ()
  (should (null (-distinct-by-alist-key 'name nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; -strings

(ert-deftest xtdlib/-strings-all-strings-unchanged ()
  (should (equal '("a" "b") (-strings "a" "b"))))

(ert-deftest xtdlib/-strings-empty-returns-empty ()
  (should (null (-strings))))

(ert-deftest xtdlib/-strings-non-string-without-options-passes-through ()
  (should (equal '("a" 42) (-strings "a" 42))))

(ert-deftest xtdlib/-strings-non-string-alone-passes-through ()
  (should (equal '(42) (-strings 42))))

(ert-deftest xtdlib/-strings-mixed-types-without-options-pass-through ()
  (should (equal '("a" 1 sym ("nested")) (-strings "a" 1 'sym '("nested")))))

(ert-deftest xtdlib/-strings-options-filter-drops-non-strings ()
  (should (equal '("a" "b") (-strings "a" 42 "b" :options 'filter))))

(ert-deftest xtdlib/-strings-options-filter-all-non-strings ()
  (should (null (-strings 1 2 3 :options 'filter))))

(ert-deftest xtdlib/-strings-options-stringify-formats-non-strings ()
  (should (equal '("a" "42" "b") (-strings "a" 42 "b" :options 'stringify))))

(ert-deftest xtdlib/-strings-options-stringify-symbol ()
  (should (equal '("foo") (-strings 'foo :options 'stringify))))

(ert-deftest xtdlib/-strings-options-list-filter ()
  (should (equal '("a") (-strings "a" 42 :options '(filter)))))

(ert-deftest xtdlib/-strings-options-list-stringify ()
  (should (equal '("a" "42") (-strings "a" 42 :options '(stringify)))))

(ert-deftest xtdlib/-strings-options-filter-precedes-stringify ()
  (should (equal '("a") (-strings "a" 42 :options '(filter stringify)))))

(ert-deftest xtdlib/-strings-options-no-effect-when-all-strings ()
  (should (equal '("a" "b") (-strings "a" "b" :options 'filter)))
  (should (equal '("a" "b") (-strings "a" "b" :options 'stringify))))

(ert-deftest xtdlib/-strings-options-nil-passes-through ()
  (should (equal '("a" "b")  (-strings "a" "b"  :options nil)))
  (should (equal '("a" 42)   (-strings "a" 42   :options nil)))
  (should (equal '(42)       (-strings 42       :options nil))))

(ert-deftest xtdlib/-strings-options-nil-equivalent-to-omitted ()
  (should (equal (-strings "a" "b") (-strings "a" "b" :options nil)))
  (should (equal (-strings "a" 42)  (-strings "a" 42  :options nil))))

(ert-deftest xtdlib/-strings-options-unknown-signals-user-error ()
  (should-error (-strings "a" 42 :options 'unknown) :type 'user-error)
  (should-error (-strings "a" 42 :options 99)       :type 'user-error)
  (should-error (-strings "a" 42 :options "filter") :type 'user-error))

(ert-deftest xtdlib/-strings-options-unknown-errors-even-when-all-strings ()
  (should-error (-strings "a" "b" :options 'unknown) :type 'user-error))

(ert-deftest xtdlib/-strings-options-only ()
  (should (null (-strings :options 'filter)))
  (should (null (-strings :options nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; --in-place

(ert-deftest xtdlib/--in-place-modifies-via-it ()
  (let ((lst (list 1 2 3)))
    (--in-place (* it 2) lst)
    (should (equal '(2 4 6) lst))))

(ert-deftest xtdlib/--in-place-returns-count ()
  (let ((lst (list 1 2 3)))
    (should (= 3 (--in-place it lst)))))

(ert-deftest xtdlib/--in-place-empty-list-returns-zero ()
  (should (= 0 (--in-place it nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; --map-uniq

(ert-deftest xtdlib/--map-uniq-removes-duplicates ()
  (should (equal '(1 2 3) (sort (--map-uniq it '(1 2 2 3 3)) #'<))))

(ert-deftest xtdlib/--map-uniq-applies-transform ()
  (let ((result (--map-uniq (upcase it) '("a" "b" "a"))))
    (should (= 2 (length result)))
    (should (member "A" result))
    (should (member "B" result))))

(ert-deftest xtdlib/--map-uniq-empty-list ()
  (should (null (--map-uniq it nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ht-make-named-table

(ert-deftest xtdlib/ht-make-named-table-creates-variable ()
  (ht-make-named-table xtdlib--test-ht-table)
  (should (boundp 'xtdlib--test-ht-table))
  (should (ht-p xtdlib--test-ht-table)))

(ert-deftest xtdlib/ht-make-named-table-generates-get-function ()
  (ht-make-named-table xtdlib--test-ht-get)
  (should (fboundp 'ht-xtdlib--test-ht-get-get)))

(ert-deftest xtdlib/ht-make-named-table-generates-set-function ()
  (ht-make-named-table xtdlib--test-ht-set)
  (should (fboundp 'ht-xtdlib--test-ht-set-set)))

(ert-deftest xtdlib/ht-make-named-table-generates-contains-p-function ()
  (ht-make-named-table xtdlib--test-ht-contains)
  (should (fboundp 'ht-xtdlib--test-ht-contains-contains-p)))

(ert-deftest xtdlib/ht-make-named-table-set-and-get-roundtrip ()
  (ht-make-named-table xtdlib--test-ht-roundtrip)
  (ht-xtdlib--test-ht-roundtrip-set "key" "value")
  (should (equal "value" (ht-xtdlib--test-ht-roundtrip-get "key"))))

(ert-deftest xtdlib/ht-make-named-table-contains-p-after-set ()
  (ht-make-named-table xtdlib--test-ht-cpset)
  (should-not (ht-xtdlib--test-ht-cpset-contains-p "k"))
  (ht-xtdlib--test-ht-cpset-set "k" t)
  (should (ht-xtdlib--test-ht-cpset-contains-p "k")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; setq-when-nil

(ert-deftest xtdlib/setq-when-nil-sets-when-nil ()
  (let ((x nil))
    (setq-when-nil x 42)
    (should (= 42 x))))

(ert-deftest xtdlib/setq-when-nil-does-not-overwrite-non-nil ()
  (let ((x 99))
    (setq-when-nil x 42)
    (should (= 99 x))))

(ert-deftest xtdlib/setq-when-nil-local-uses-setq-local ()
  (with-temp-buffer
    (setq-local default-directory nil)
    (setq-when-nil default-directory "/tmp/" :local t)
    (should (equal "/tmp/" default-directory))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; with-temp-keymap

(ert-deftest xtdlib/with-temp-keymap-returns-keymap ()
  (should (keymapp (with-temp-keymap km nil))))

(ert-deftest xtdlib/with-temp-keymap-body-can-bind-keys ()
  (let ((km (with-temp-keymap km
              (define-key km (kbd "C-x") #'ignore))))
    (should (eq #'ignore (lookup-key km (kbd "C-x"))))))

(ert-deftest xtdlib/with-temp-keymap-is-fresh-each-call ()
  (let ((km1 (with-temp-keymap km nil))
        (km2 (with-temp-keymap km nil)))
    (should-not (eq km1 km2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; make-run-hooks-function-for

(ert-deftest xtdlib/make-run-hooks-function-for-defines-function ()
  (make-run-hooks-function-for xtdlib--test-mode)
  (should (fboundp 'run-hooks-for-xtdlib--test-mode)))

(ert-deftest xtdlib/make-run-hooks-function-for-runs-hook ()
  (let ((fired nil))
    (make-run-hooks-function-for xtdlib--test-fire-mode)
    (add-hook 'xtdlib--test-fire-mode-hook (lambda () (setq fired t)))
    (run-hooks-for-xtdlib--test-fire-mode)
    (should fired)
    (remove-hook 'xtdlib--test-fire-mode-hook (lambda () (setq fired t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; create-toggle-functions

(defvar xtdlib--test-toggle-flag nil)

(create-toggle-functions xtdlib--test-toggle-flag)

(ert-deftest xtdlib/create-toggle-functions-defines-turn-on ()
  (should (fboundp 'turn-on-xtdlib--test-toggle-flag)))

(ert-deftest xtdlib/create-toggle-functions-defines-turn-off ()
  (should (fboundp 'turn-off-xtdlib--test-toggle-flag)))

(ert-deftest xtdlib/create-toggle-functions-defines-toggle ()
  (should (fboundp 'toggle-xtdlib--test-toggle-flag)))

(ert-deftest xtdlib/create-toggle-functions-turn-on-sets-t ()
  (setq xtdlib--test-toggle-flag nil)
  (turn-on-xtdlib--test-toggle-flag)
  (should xtdlib--test-toggle-flag))

(ert-deftest xtdlib/create-toggle-functions-turn-off-sets-nil ()
  (setq xtdlib--test-toggle-flag t)
  (turn-off-xtdlib--test-toggle-flag)
  (should-not xtdlib--test-toggle-flag))

(ert-deftest xtdlib/create-toggle-functions-toggle-flips-value ()
  (setq xtdlib--test-toggle-flag nil)
  (toggle-xtdlib--test-toggle-flag)
  (should xtdlib--test-toggle-flag)
  (toggle-xtdlib--test-toggle-flag)
  (should-not xtdlib--test-toggle-flag))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; make-read-extended-command-for-prefix

(make-read-extended-command-for-prefix "xtdlib-test")

(ert-deftest xtdlib/make-read-extended-command-for-prefix-defines-predicate ()
  (should (fboundp 'read-extended-command-for-xtdlib-test-prefix-p)))

(ert-deftest xtdlib/make-read-extended-command-for-prefix-defines-command ()
  (should (fboundp 'execute-extended-xtdlib-test-command)))

(ert-deftest xtdlib/make-read-extended-command-for-prefix-predicate-matches ()
  (should (read-extended-command-for-xtdlib-test-prefix-p 'xtdlib-test-something nil)))

(ert-deftest xtdlib/make-read-extended-command-for-prefix-predicate-rejects ()
  (should-not (read-extended-command-for-xtdlib-test-prefix-p 'other-command nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; add-one-shot-hook: extended coverage

(defvar xtdlib--test-hook-a nil)
(defvar xtdlib--test-hook-b nil)

(ert-deftest xtdlib/add-one-shot-hook-count-2-fires-twice ()
  (setq xtdlib--test-hook-var nil)
  (let ((fire-count 0))
    (add-one-shot-hook
     :name "xtdlib-test-count2"
     :hook 'xtdlib--test-hook-var
     :count 2
     :form (cl-incf fire-count))
    (run-hooks 'xtdlib--test-hook-var)
    (run-hooks 'xtdlib--test-hook-var)
    (run-hooks 'xtdlib--test-hook-var)
    (should (= 2 fire-count))))

(ert-deftest xtdlib/add-one-shot-hook-removes-itself-after-count-2 ()
  (setq xtdlib--test-hook-var nil)
  (add-one-shot-hook
   :name "xtdlib-test-removal2"
   :hook 'xtdlib--test-hook-var
   :count 2
   :form t)
  (run-hooks 'xtdlib--test-hook-var)
  (should xtdlib--test-hook-var)
  (run-hooks 'xtdlib--test-hook-var)
  (should-not xtdlib--test-hook-var))

(ert-deftest xtdlib/add-one-shot-hook-multiple-hooks ()
  (setq xtdlib--test-hook-a nil
        xtdlib--test-hook-b nil)
  (let ((fire-count 0))
    (add-one-shot-hook
     :name "xtdlib-test-multi"
     :hook '(xtdlib--test-hook-a xtdlib--test-hook-b)
     :form (cl-incf fire-count))
    (run-hooks 'xtdlib--test-hook-a)
    (should (= 1 fire-count))
    ;; hook removed itself from both after one fire
    (run-hooks 'xtdlib--test-hook-b)
    (should (= 1 fire-count))))

(ert-deftest xtdlib/add-one-shot-hook-after-first-frame-created-routes-to-window-setup ()
  "after-first-frame-created routes to window-setup-hook in non-daemon mode and
server-after-make-frame-hook in daemon mode.  The routing is decided at
macro-expansion time via (daemonp), so this test mirrors that logic."
  (let* ((target-hook (if (daemonp) 'server-after-make-frame-hook 'window-setup-hook))
         (before (symbol-value target-hook)))
    (add-one-shot-hook
     :name "xtdlib-test-frame-sentinel"
     :hook after-first-frame-created
     :form t)
    (unwind-protect
        (should (> (length (symbol-value target-hook)) (length before)))
      (set target-hook before))))

(ert-deftest xtdlib/add-one-shot-hook-after-first-frame-created-quoted-routes-to-window-setup ()
  "The quoted sentinel form routes to the same hook as the unquoted form."
  (let* ((target-hook (if (daemonp) 'server-after-make-frame-hook 'window-setup-hook))
         (before (symbol-value target-hook)))
    (add-one-shot-hook
     :name "xtdlib-test-frame-sentinel-quoted"
     :hook 'after-first-frame-created
     :form t)
    (unwind-protect
        (should (> (length (symbol-value target-hook)) (length before)))
      (set target-hook before))))

(ert-deftest xtdlib/add-one-shot-hook-idle-timer-schedules-timer ()
  "With :idle-timer, the hook body is deferred via run-with-idle-timer."
  (setq xtdlib--test-hook-var nil)
  (let ((timer-fired nil)
        (scheduled nil))
    (cl-letf (((symbol-function 'run-with-idle-timer)
               (lambda (_delay _repeat fn &rest _args)
                 (setq scheduled t)
                 (funcall fn))))
      (add-one-shot-hook
       :name "xtdlib-test-idle"
       :hook 'xtdlib--test-hook-var
       :idle-timer 0.1
       :form (setq timer-fired t))
      (run-hooks 'xtdlib--test-hook-var)
      (should scheduled)
      (should timer-fired)
      (should-not xtdlib--test-hook-var))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; merge-predicate-functions

(ert-deftest xtdlib/merge-predicate-functions-all-pass ()
  (let ((pred (merge-predicate-functions stringp (lambda (v) (> (length v) 2)))))
    (should (funcall pred "hello"))))

(ert-deftest xtdlib/merge-predicate-functions-first-fails ()
  (let ((pred (merge-predicate-functions numberp stringp)))
    (should-not (funcall pred "not-a-number"))))

(ert-deftest xtdlib/merge-predicate-functions-second-fails ()
  (let ((pred (merge-predicate-functions stringp (lambda (v) (> (length v) 10)))))
    (should-not (funcall pred "short"))))

(ert-deftest xtdlib/merge-predicate-functions-single-predicate ()
  (let ((pred (merge-predicate-functions numberp)))
    (should (funcall pred 42))
    (should-not (funcall pred "x"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; f extensions: filesystem functions

(ert-deftest xtdlib/f-mtime-returns-time-value ()
  (let ((tmp (make-temp-file "xtdlib-test-mtime-")))
    (unwind-protect
        (let ((mtime (f-mtime tmp)))
          (should mtime)
          (should (listp mtime)))
      (delete-file tmp))))

(ert-deftest xtdlib/f-atime-returns-time-value ()
  (let ((tmp (make-temp-file "xtdlib-test-atime-")))
    (unwind-protect
        (let ((atime (f-atime tmp)))
          (should atime)
          (should (listp atime)))
      (delete-file tmp))))

(ert-deftest xtdlib/f-distinct-removes-duplicate-paths ()
  (should (= 1 (length (f-distinct (list "/tmp/foo" "/tmp/foo"))))))

(ert-deftest xtdlib/f-distinct-preserves-unique-paths ()
  (should (= 2 (length (f-distinct (list "/tmp/foo" "/tmp/bar"))))))

(ert-deftest xtdlib/f-files-in-directory-returns-files ()
  (let ((dir (make-temp-file "xtdlib-test-dir-" t)))
    (unwind-protect
        (let ((f (expand-file-name "test.el" dir)))
          (with-temp-file f (insert ""))
          (let ((files (f-files-in-directory dir)))
            (should (listp files))
            (should (= 1 (length files)))
            (should (string-match-p "test\\.el" (car files)))))
      (delete-directory dir t))))

(ert-deftest xtdlib/f-files-in-directory-handles-string-path ()
  (let ((dir (make-temp-file "xtdlib-test-dir2-" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "a.el" dir) (insert ""))
          (should (listp (f-files-in-directory dir))))
      (delete-directory dir t))))

(ert-deftest xtdlib/f-recursive-directories-containing-finds-filename ()
  (let* ((root (make-temp-file "xtdlib-test-rdc-" t))
         (sub  (expand-file-name "sub" root)))
    (unwind-protect
        (progn
          (make-directory sub)
          (with-temp-file (expand-file-name "Makefile" sub) (insert ""))
          (let ((dirs (f-recursive-directories-containing "Makefile" root)))
            (should (= 1 (length dirs)))
            (should (f-equal-p sub (car dirs)))))
      (delete-directory root t))))

(ert-deftest xtdlib/f-recursive-directories-containing-empty-when-not-found ()
  (let ((dir (make-temp-file "xtdlib-test-rdcempty-" t)))
    (unwind-protect
        (should (null (f-recursive-directories-containing "no-such-file" dir)))
      (delete-directory dir t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; f-visually-compress-to-N (generated functions)

(ert-deftest xtdlib/f-visually-compress-to-one-limits-to-1-char ()
  (let ((result (f-visually-compress-to-one "/home/tychoish/projects/foo")))
    (dolist (part (split-string result "/"))
      (should (<= (length part) 1)))))

(ert-deftest xtdlib/f-visually-compress-to-five-limits-to-5-chars ()
  (let ((result (f-visually-compress-to-five "/home/tychoish/projects/longname")))
    (dolist (part (split-string result "/"))
      (should (<= (length part) 5)))))

(ert-deftest xtdlib/f-visually-compress-to-ten-preserves-short-components ()
  (let ((result (f-visually-compress-to-ten "/tmp/foo")))
    (should (string-match-p "foo" result))))

(provide 'test-xtdlib)
;;; test-xtdlib.el ends here
