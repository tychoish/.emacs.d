;;; test-xlib.el --- ERT tests for xlib.el -*- lexical-binding: t -*-

;; Run inside a live Emacs session with full config loaded:
;;   M-x ert RET t RET
;; or filtered:
;;   (ert "^xlib/")

(require 'ert)
(require 'cl-lib)
(require 'xlib)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; String helpers

(ert-deftest xlib/s-join-spc-basic ()
  (should (equal "foo bar" (s-join-spc "foo" "bar"))))

(ert-deftest xlib/s-join-spc-single-word ()
  (should (equal "foo" (s-join-spc "foo"))))

(ert-deftest xlib/s-join-spc-filters-nil ()
  (should (equal "foo bar" (s-join-spc "foo" nil "bar"))))

(ert-deftest xlib/s-join-spc-filters-non-strings ()
  (should (equal "foo bar" (s-join-spc "foo" 42 t "bar"))))

(ert-deftest xlib/s-join-spc-filters-empty-and-whitespace ()
  (should (equal "foo bar" (s-join-spc "foo" "" "   " "bar"))))

(ert-deftest xlib/s-join-spc-all-empty-returns-empty ()
  (should (equal "" (s-join-spc "" "  " nil))))

(ert-deftest xlib/s-join-spc-trims-inputs ()
  (should (equal "foo bar" (s-join-spc "  foo  " "  bar  "))))

;;; -filter-s-trim

(ert-deftest xlib/-filter-s-trim-basic ()
  (should (equal '("foo" "bar") (-filter-s-trim '("foo" "bar")))))

(ert-deftest xlib/-filter-s-trim-trims-whitespace ()
  (should (equal '("foo" "bar") (-filter-s-trim '("  foo  " " bar")))))

(ert-deftest xlib/-filter-s-trim-removes-non-strings ()
  (should (equal '("foo") (-filter-s-trim '("foo" nil 42 t)))))

(ert-deftest xlib/-filter-s-trim-removes-empty-strings ()
  (should (equal '("foo") (-filter-s-trim '("foo" "" "  ")))))

(ert-deftest xlib/-filter-s-trim-empty-list ()
  (should (equal nil (-filter-s-trim '()))))

(ert-deftest xlib/-filter-s-trim-rejects-non-list ()
  (should-error (-filter-s-trim "not-a-list") :type 'wrong-type-argument))

(ert-deftest xlib/-filter-s-trim-rejects-string-arg ()
  (should-error (-filter-s-trim 42) :type 'wrong-type-argument))

;;; s-or-char-equal

(ert-deftest xlib/s-or-char-equal-char-vs-same-char ()
  (should (s-or-char-equal ?a ?a)))

(ert-deftest xlib/s-or-char-equal-char-vs-different-char ()
  (should-not (s-or-char-equal ?a ?b)))

(ert-deftest xlib/s-or-char-equal-char-vs-matching-string ()
  (should (s-or-char-equal ?a "a")))

(ert-deftest xlib/s-or-char-equal-char-vs-non-matching-string ()
  (should-not (s-or-char-equal ?a "b")))

(ert-deftest xlib/s-or-char-equal-non-char-first-arg-errors ()
  (should-error (s-or-char-equal "a" "a") :type 'wrong-type-argument))

;;; s-shortest

(ert-deftest xlib/s-shortest-returns-first-when-shorter ()
  (should (equal "ab" (s-shortest "ab" "abc"))))

(ert-deftest xlib/s-shortest-returns-second-when-shorter ()
  (should (equal "a" (s-shortest "ab" "a"))))

(ert-deftest xlib/s-shortest-returns-first-when-equal-length ()
  (should (equal "ab" (s-shortest "ab" "cd"))))

(ert-deftest xlib/s-shortest-empty-string ()
  (should (equal "" (s-shortest "" "abc"))))

;;; s-collapse-hyphens

(ert-deftest xlib/s-collapse-hyphens-collapses-triple ()
  (should (equal "foo-bar" (s-collapse-hyphens "foo---bar"))))

(ert-deftest xlib/s-collapse-hyphens-collapses-many ()
  (should (equal "foo-bar" (s-collapse-hyphens "foo-----bar"))))

(ert-deftest xlib/s-collapse-hyphens-leaves-double-unchanged ()
  (should (equal "foo--bar" (s-collapse-hyphens "foo--bar"))))

(ert-deftest xlib/s-collapse-hyphens-leaves-single-unchanged ()
  (should (equal "foo-bar" (s-collapse-hyphens "foo-bar"))))

(ert-deftest xlib/s-collapse-hyphens-no-hyphens ()
  (should (equal "foobar" (s-collapse-hyphens "foobar"))))

(ert-deftest xlib/s-collapse-hyphens-all-hyphens ()
  (should (equal "-" (s-collapse-hyphens "---"))))

;;; s-trimmed-or-nil

(ert-deftest xlib/s-trimmed-or-nil-returns-trimmed-string ()
  (should (equal "foo" (s-trimmed-or-nil "  foo  "))))

(ert-deftest xlib/s-trimmed-or-nil-already-trimmed ()
  (should (equal "foo" (s-trimmed-or-nil "foo"))))

(ert-deftest xlib/s-trimmed-or-nil-empty-string-returns-nil ()
  (should-not (s-trimmed-or-nil "")))

(ert-deftest xlib/s-trimmed-or-nil-whitespace-only-returns-nil ()
  (should-not (s-trimmed-or-nil "   ")))

(ert-deftest xlib/s-trimmed-or-nil-nil-returns-nil ()
  (should-not (s-trimmed-or-nil nil)))

(ert-deftest xlib/s-trimmed-or-nil-non-string-returns-nil ()
  (should-not (s-trimmed-or-nil 42))
  (should-not (s-trimmed-or-nil t))
  (should-not (s-trimmed-or-nil '(a b))))

;;; s-trim-non-word-chars

(ert-deftest xlib/s-trim-non-word-chars-trims-punctuation ()
  (should (equal "foo" (s-trim-non-word-chars "...foo..."))))

(ert-deftest xlib/s-trim-non-word-chars-keeps-inner-non-word ()
  (should (equal "foo-bar" (s-trim-non-word-chars "...foo-bar..."))))

(ert-deftest xlib/s-trim-non-word-chars-already-clean ()
  (should (equal "foo" (s-trim-non-word-chars "foo"))))

(ert-deftest xlib/s-trim-non-word-chars-nil-returns-nil ()
  (should-not (s-trim-non-word-chars nil)))

(ert-deftest xlib/s-trim-non-word-chars-non-string-returns-nil ()
  (should-not (s-trim-non-word-chars 42)))

;;; s-contains-whitespace-p
;; NOTE: despite the docstring, this function returns t when the string
;; consists ENTIRELY of whitespace (the implementation checks string-empty-p
;; after trimming), and nil when there is any non-whitespace content.

(ert-deftest xlib/s-contains-whitespace-p-all-whitespace-returns-t ()
  (should (s-contains-whitespace-p "   ")))

(ert-deftest xlib/s-contains-whitespace-p-empty-string-returns-t ()
  (should (s-contains-whitespace-p "")))

(ert-deftest xlib/s-contains-whitespace-p-non-whitespace-returns-nil ()
  (should-not (s-contains-whitespace-p "foo")))

(ert-deftest xlib/s-contains-whitespace-p-mixed-content-returns-nil ()
  (should-not (s-contains-whitespace-p "  foo  ")))

(ert-deftest xlib/s-contains-whitespace-p-non-string-returns-nil ()
  (should-not (s-contains-whitespace-p nil))
  (should-not (s-contains-whitespace-p 42)))


;;; s-default

(ert-deftest xlib/s-default-returns-input-when-non-empty ()
  (should (equal "bar" (s-default "default" "bar"))))

(ert-deftest xlib/s-default-returns-default-for-nil ()
  (should (equal "default" (s-default "default" nil))))

(ert-deftest xlib/s-default-returns-default-for-empty-string ()
  (should (equal "default" (s-default "default" ""))))

(ert-deftest xlib/s-default-returns-default-when-input-equals-default ()
  (should (equal "foo" (s-default "foo" "foo"))))

;;; s-number-word

(ert-deftest xlib/s-number-word-boundaries ()
  (should (equal "one"    (s-number-word 1)))
  (should (equal "ten"    (s-number-word 10)))
  (should (equal "twenty" (s-number-word 20))))

(ert-deftest xlib/s-number-word-mid-range ()
  (should (equal "five"     (s-number-word 5)))
  (should (equal "thirteen" (s-number-word 13)))
  (should (equal "nineteen" (s-number-word 19))))

(ert-deftest xlib/s-number-word-zero-errors ()
  (should-error (s-number-word 0) :type 'user-error))

(ert-deftest xlib/s-number-word-out-of-range-errors ()
  (should-error (s-number-word 21) :type 'user-error)
  (should-error (s-number-word 100) :type 'user-error))

;;; generated join functions

(ert-deftest xlib/s-join-with-hyphen-basic ()
  (should (equal "foo-bar-baz" (s-join-with-hyphen "foo" "bar" "baz"))))

(ert-deftest xlib/s-join-with-hyphen-filters-empty ()
  (should (equal "foo-bar" (s-join-with-hyphen "foo" nil "" "bar"))))

(ert-deftest xlib/s-join-with-kebab-is-same-as-hyphen ()
  (should (equal (s-join-with-hyphen "foo" "bar")
                 (s-join-with-kebab "foo" "bar"))))

(ert-deftest xlib/s-join-with-underscore-basic ()
  (should (equal "foo_bar" (s-join-with-underscore "foo" "bar"))))

(ert-deftest xlib/s-join-with-snake-is-same-as-underscore ()
  (should (equal (s-join-with-underscore "foo" "bar")
                 (s-join-with-snake "foo" "bar"))))

(ert-deftest xlib/s-join-with-space-basic ()
  (should (equal "foo bar" (s-join-with-space "foo" "bar"))))

(ert-deftest xlib/s-join-with-spc-is-same-as-space ()
  (should (equal (s-join-with-space "foo" "bar")
                 (s-join-with-spc "foo" "bar"))))

(ert-deftest xlib/s-join-with-pipe-adds-space-padding ()
  (should (equal "foo | bar" (s-join-with-pipe "foo" "bar"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Numeric helpers

(ert-deftest xlib/larger-basic ()
  (should (= 5 (larger 3 5)))
  (should (= 5 (larger 5 3))))

(ert-deftest xlib/larger-equal-returns-second ()
  (should (= 3 (larger 3 3))))

(ert-deftest xlib/larger-nil-treated-as-zero ()
  (should (= 0 (larger nil nil)))
  (should (= 5 (larger nil 5)))
  (should (= 5 (larger 5 nil))))

(ert-deftest xlib/larger-non-number-treated-as-zero ()
  (should (= 5 (larger "string" 5)))
  (should (= 5 (larger 5 "string"))))

(ert-deftest xlib/larger-negative-numbers ()
  (should (= -1 (larger -1 -5))))

(ert-deftest xlib/smaller-basic ()
  (should (= 3 (smaller 3 5)))
  (should (= 3 (smaller 5 3))))

(ert-deftest xlib/smaller-nil-treated-as-zero ()
  (should (= 0 (smaller nil nil)))
  (should (= 0 (smaller nil 5)))
  (should (= 0 (smaller 5 nil))))

(ert-deftest xlib/smaller-non-number-treated-as-zero ()
  (should (= 0 (smaller "string" 5))))

(ert-deftest xlib/smaller-negative-numbers ()
  (should (= -5 (smaller -1 -5))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dash extensions

(ert-deftest xlib/-distinct-by-car-removes-duplicate-car ()
  (let ((result (-distinct-by-car '((a . 1) (a . 2)))))
    (should (= 1 (length result)))
    (should (eq 'a (caar result)))))

(ert-deftest xlib/-distinct-by-car-preserves-unique-keys ()
  (let ((result (-distinct-by-car '((a . 1) (b . 2) (c . 3)))))
    (should (= 3 (length result)))))

(ert-deftest xlib/-distinct-by-car-mixed ()
  (let ((result (-distinct-by-car '((a . 1) (b . 2) (a . 9)))))
    (should (= 2 (length result)))
    (should (assq 'a result))
    (should (assq 'b result))))

(ert-deftest xlib/-distinct-by-car-empty-list ()
  (should (equal nil (-distinct-by-car '()))))

(ert-deftest xlib/-unwind-nested-lists ()
  (should (equal '(1 2 3 4) (-unwind '((1 2) (3 4))))))

(ert-deftest xlib/-unwind-empty-list ()
  (should (equal nil (-unwind '()))))

(ert-deftest xlib/-unwind-single-level-only ()
  ;; flattens exactly one level, not recursively
  (should (equal '((1 2) (3 4)) (-unwind '(((1 2)) ((3 4)))))))

(ert-deftest xlib/-sparse-append-filters-nils ()
  (should (equal '(1 2 3) (-sparse-append '(1 nil 2) '(nil 3)))))

(ert-deftest xlib/-sparse-append-all-nil ()
  (should (equal nil (-sparse-append '(nil) '(nil nil)))))

(ert-deftest xlib/-sparse-append-no-nils ()
  (should (equal '(1 2 3) (-sparse-append '(1 2) '(3)))))

(ert-deftest xlib/-map-in-place-modifies-list ()
  (let ((lst (list 1 2 3)))
    (-map-in-place #'1+ lst)
    (should (equal '(2 3 4) lst))))

(ert-deftest xlib/-map-in-place-returns-same-list ()
  (let ((lst (list 1 2 3)))
    (should (eq lst (-map-in-place #'identity lst)))))

(ert-deftest xlib/-map-in-place-identity-preserves-values ()
  (let ((lst (list 1 2 3)))
    (-map-in-place #'identity lst)
    (should (equal '(1 2 3) lst))))

(ert-deftest xlib/-in-place-returns-count ()
  (let ((lst (list 1 2 3)))
    (should (= 3 (-in-place #'1+ lst)))))

(ert-deftest xlib/-in-place-modifies-list ()
  (let ((lst (list 10 20 30)))
    (-in-place #'1+ lst)
    (should (equal '(11 21 31) lst))))

(ert-deftest xlib/-in-place-empty-list-returns-zero ()
  (should (= 0 (-in-place #'identity nil))))

(ert-deftest xlib/-map-uniq-removes-duplicates ()
  (let ((result (-map-uniq #'identity '(1 2 1 3 2))))
    (should (= 3 (length result)))
    (should (member 1 result))
    (should (member 2 result))
    (should (member 3 result))))

(ert-deftest xlib/-map-uniq-with-transform ()
  ;; maps strings to their first character; "foo" and "fig" both → "f"
  (let ((result (-map-uniq (lambda (s) (substring s 0 1)) '("foo" "bar" "fig"))))
    (should (= 2 (length result)))))

(ert-deftest xlib/-map-uniq-all-unique ()
  (should (= 3 (length (-map-uniq #'identity '(1 2 3))))))

;;; anaphoric macros

(ert-deftest xlib/--mapc-executes-for-each-element ()
  (let ((acc nil))
    (--mapc (push it acc) '(1 2 3))
    (should (equal '(3 2 1) acc))))

(ert-deftest xlib/--mapc-empty-list ()
  (let ((ran nil))
    (--mapc (setq ran t) '())
    (should-not ran)))

(ert-deftest xlib/--flat-map-basic ()
  (should (equal '(1 2 2 3) (--flat-map (list it (1+ it)) '(1 2)))))

(ert-deftest xlib/--flat-map-empty-list ()
  (should (equal nil (--flat-map (list it) '()))))

(ert-deftest xlib/--map-in-place-modifies-via-it ()
  (let ((lst (list 1 2 3)))
    (--map-in-place (* it 2) lst)
    (should (equal '(2 4 6) lst))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ht extensions

(ert-deftest xlib/ht-get-lambda-retrieves-value ()
  (let* ((tbl (ht-create))
         (getter (ht-get-lambda tbl)))
    (ht-set tbl "key" "val")
    (should (equal "val" (funcall getter "key")))))

(ert-deftest xlib/ht-get-lambda-missing-key-returns-nil ()
  (let* ((tbl (ht-create))
         (getter (ht-get-lambda tbl)))
    (should-not (funcall getter "missing"))))

(ert-deftest xlib/ht-set-lambda-stores-value ()
  (let* ((tbl (ht-create))
         (setter (ht-set-lambda tbl)))
    (funcall setter "key" "val")
    (should (equal "val" (ht-get tbl "key")))))

(ert-deftest xlib/ht-contains-p-lambda-found ()
  (let* ((tbl (ht-create))
         (pred (ht-contains-p-lambda tbl)))
    (ht-set tbl "key" "val")
    (should (funcall pred "key"))))

(ert-deftest xlib/ht-contains-p-lambda-not-found ()
  (let* ((tbl (ht-create))
         (pred (ht-contains-p-lambda tbl)))
    (should-not (funcall pred "missing"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; option-set-p

(ert-deftest xlib/option-set-p-single-symbol-match ()
  (should (option-set-p 'foo 'foo)))

(ert-deftest xlib/option-set-p-single-symbol-no-match ()
  (should-not (option-set-p 'foo 'bar)))

(ert-deftest xlib/option-set-p-list-contains-opt ()
  (should (option-set-p 'foo '(bar foo baz))))

(ert-deftest xlib/option-set-p-list-missing-opt ()
  (should-not (option-set-p 'foo '(bar baz))))

(ert-deftest xlib/option-set-p-nil-options ()
  (should-not (option-set-p 'foo nil)))

(ert-deftest xlib/option-set-p-empty-list ()
  (should-not (option-set-p 'foo '())))

(ert-deftest xlib/option-set-p-single-element-list-match ()
  (should (option-set-p 'foo '(foo))))

(ert-deftest xlib/option-set-p-uses-eq-not-equal ()
  ;; only works with symbols (eq), not equal strings
  (should-not (option-set-p "foo" "foo")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utility macros

(ert-deftest xlib/disabled-never-executes-body ()
  (let ((ran nil))
    (disabled (setq ran t))
    (should-not ran)))

(ert-deftest xlib/disabled-with-multiple-forms ()
  (let ((count 0))
    (disabled
     (cl-incf count)
     (cl-incf count)
     (cl-incf count))
    (should (= 0 count))))

(ert-deftest xlib/pos-arg-returns-is-value ()
  (should (= 42 (pos-arg "arg" :is 42))))

(ert-deftest xlib/pos-arg-symbol-name ()
  (should (equal "hello" (pos-arg my-arg :is "hello"))))

(ert-deftest xlib/pos-arg-nil-is-value ()
  (should (eq nil (pos-arg "arg" :is nil))))

(ert-deftest xlib/pos-arg-non-string-non-symbol-name-errors ()
  (should-error (pos-arg 42 :is "val") :type 'user-error))

(ert-deftest xlib/pa-alias-works ()
  (should (= 99 (pa "x" :is 99))))

(ert-deftest xlib/with-slow-op-timer-returns-body-value ()
  (should (equal 42 (with-slow-op-timer "test" 42))))

(ert-deftest xlib/with-slow-op-timer-executes-body ()
  (let ((ran nil))
    (with-slow-op-timer "test" (setq ran t))
    (should ran)))

(ert-deftest xlib/with-slow-op-timer-returns-last-form ()
  (should (equal "last" (with-slow-op-timer "test"
                          "first"
                          "last"))))

(ert-deftest xlib/with-force-write-allows-insertion-in-read-only-buffer ()
  (with-temp-buffer
    (setq buffer-read-only t)
    (with-force-write
      (insert "hello"))
    (should (equal "hello" (buffer-string)))))

(ert-deftest xlib/with-force-write-restores-read-only ()
  (with-temp-buffer
    (setq buffer-read-only t)
    (with-force-write
      (insert "x"))
    (should buffer-read-only)))

(ert-deftest xlib/with-force-write-returns-body-value ()
  (with-temp-buffer
    (setq buffer-read-only t)
    (should (equal 99 (with-force-write 99)))))

(ert-deftest xlib/with-default-directory-sets-path ()
  (let (captured)
    (with-default-directory "/tmp"
      (setq captured default-directory))
    (should (equal "/tmp" captured))))

(ert-deftest xlib/with-default-directory-restores-original ()
  (let ((orig default-directory))
    (with-default-directory "/tmp" nil)
    (should (equal orig default-directory))))

(ert-deftest xlib/with-silence-sets-inhibit-message ()
  (let ((inhibit-message nil))
    (with-silence
      (should inhibit-message))))

(ert-deftest xlib/with-silence-sets-message-log-max-nil ()
  (let ((message-log-max 100))
    (with-silence
      (should (eq nil message-log-max)))))

(ert-deftest xlib/with-silence-restores-after-body ()
  (let ((inhibit-message nil))
    (with-silence nil)
    (should-not inhibit-message)))

(ert-deftest xlib/with-quiet-sets-inhibit-message ()
  (let ((inhibit-message nil))
    (with-quiet
      (should inhibit-message))))

(ert-deftest xlib/with-quiet-does-not-touch-message-log-max ()
  (let ((message-log-max 100))
    (with-quiet
      (should (= 100 message-log-max)))))

(ert-deftest xlib/with-prefix-arg-sets-current-prefix-arg ()
  (let (captured)
    (with-prefix-arg 4
      (setq captured current-prefix-arg))
    (should (= 4 captured))))

(ert-deftest xlib/with-prefix-arg-restores-original ()
  (let ((current-prefix-arg nil))
    (with-prefix-arg 4 nil)
    (should (eq nil current-prefix-arg))))

(ert-deftest xlib/with-prefix-arg-nil-value ()
  (let ((current-prefix-arg 4))
    (let (captured)
      (with-prefix-arg nil
        (setq captured current-prefix-arg))
      (should (eq nil captured)))))

(ert-deftest xlib/with-timer-executes-body ()
  (let ((ran nil))
    (with-timer "test" (setq ran t))
    (should ran)))

(ert-deftest xlib/compile-buffer-name-returns-callable ()
  (should (functionp (compile-buffer-name "my-buffer"))))

(ert-deftest xlib/compile-buffer-name-returned-fn-returns-name ()
  (should (equal "my-buf" (funcall (compile-buffer-name "my-buf")))))

(ert-deftest xlib/compile-buffer-name-returned-fn-ignores-optional-arg ()
  (should (equal "my-buf" (funcall (compile-buffer-name "my-buf") "ignored"))))

;;; with-toggle-once

(defvar xlib--test-toggle-state nil)

(ert-deftest xlib/with-toggle-once-body-runs-on-first-call ()
  (let ((count 0))
    (with-toggle-once xlib--test-once-basic
      (cl-incf count))
    (setq xlib--test-once-basic-toggle-state nil)
    (xlib--test-once-basic)
    (should (= 1 count))))

(ert-deftest xlib/with-toggle-once-body-does-not-run-again ()
  (let ((count 0))
    (with-toggle-once xlib--test-once-idempotent
      (cl-incf count))
    (setq xlib--test-once-idempotent-toggle-state nil)
    (xlib--test-once-idempotent)
    (xlib--test-once-idempotent)
    (xlib--test-once-idempotent)
    (should (= 1 count))))

(ert-deftest xlib/with-toggle-once-defines-a-function ()
  (with-toggle-once xlib--test-once-fn-exists nil)
  (should (fboundp 'xlib--test-once-fn-exists)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; add-one-shot-hook

(defvar xlib--test-hook-var nil
  "Scratch hook variable for add-one-shot-hook tests.")

(ert-deftest xlib/add-one-shot-hook-runs-once ()
  (setq xlib--test-hook-var nil)
  (let ((fire-count 0))
    (add-one-shot-hook
     :name "xlib-test-count"
     :hook 'xlib--test-hook-var
     :form (cl-incf fire-count))
    (run-hooks 'xlib--test-hook-var)
    (run-hooks 'xlib--test-hook-var)
    (run-hooks 'xlib--test-hook-var)
    (should (= 1 fire-count))))

(ert-deftest xlib/add-one-shot-hook-removes-itself-after-firing ()
  (setq xlib--test-hook-var nil)
  (add-one-shot-hook
   :name "xlib-test-removal"
   :hook 'xlib--test-hook-var
   :form t)
  (should xlib--test-hook-var)
  (run-hooks 'xlib--test-hook-var)
  (should-not xlib--test-hook-var))

(ert-deftest xlib/add-one-shot-hook-adds-to-hook ()
  (setq xlib--test-hook-var nil)
  (add-one-shot-hook
   :name "xlib-test-add"
   :hook 'xlib--test-hook-var
   :form t)
  (should xlib--test-hook-var))

(ert-deftest xlib/add-one-shot-hook-persist-runs-every-time ()
  (setq xlib--test-hook-var nil)
  (let ((fire-count 0))
    (add-one-shot-hook
     :name "xlib-test-persist"
     :hook 'xlib--test-hook-var
     :persist t
     :form (cl-incf fire-count))
    (run-hooks 'xlib--test-hook-var)
    (run-hooks 'xlib--test-hook-var)
    (run-hooks 'xlib--test-hook-var)
    ;; clean up
    (setq xlib--test-hook-var nil)
    (should (= 3 fire-count))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; f extensions (pure/path functions only — no fs I/O)

(ert-deftest xlib/f-make-slug-basic ()
  (should (equal "hello-world" (f-make-slug "Hello World"))))

(ert-deftest xlib/f-make-slug-special-characters ()
  (should (equal "foo-bar-baz" (f-make-slug "foo/bar_baz"))))

(ert-deftest xlib/f-make-slug-already-slug ()
  (should (equal "foo-bar" (f-make-slug "foo-bar"))))

(ert-deftest xlib/f-make-slug-all-special ()
  (should (string-match-p "^-+$" (f-make-slug "!@#$%"))))

(ert-deftest xlib/f-filename-is-p-match ()
  (should (f-filename-is-p "/some/path/foo.el" "foo.el")))

(ert-deftest xlib/f-filename-is-p-no-match ()
  (should-not (f-filename-is-p "/some/path/foo.el" "bar.el")))

(ert-deftest xlib/f-filename-is-p-different-ext ()
  (should-not (f-filename-is-p "/some/path/foo.el" "foo.elc")))

(ert-deftest xlib/f-collapse-homedir-replaces-home-prefix ()
  (let* ((home (expand-file-name "~/"))
         (path (concat home "projects/foo")))
    (should (equal "~/projects/foo" (f-collapse-homedir path)))))

(ert-deftest xlib/f-collapse-homedir-non-home-path-unchanged ()
  (should (equal "/tmp/foo/bar" (f-collapse-homedir "/tmp/foo/bar"))))

(ert-deftest xlib/f-when-file-exists-returns-path-when-file-exists ()
  (let ((tmp (make-temp-file "xlib-test-")))
    (unwind-protect
        (should (equal tmp (f-when-file-exists tmp)))
      (delete-file tmp))))

(ert-deftest xlib/f-when-file-exists-returns-nil-for-nonexistent ()
  (should-not (f-when-file-exists "/no/such/path/xlib-test-nonexistent")))

(ert-deftest xlib/f-visually-compress-path-truncates-components ()
  ;; path "/home/tychoish" with num=3 → each component truncated to 3
  (let ((result (f-visually-compress-path 3 "/home/tychoish")))
    (should (stringp result))
    ;; no component (except root "") should be longer than 3 chars
    (dolist (part (split-string result "/"))
      (should (<= (length part) 3)))))

(ert-deftest xlib/f-visually-compress-path-short-components-unchanged ()
  ;; components already <= num remain unchanged
  (let ((result (f-visually-compress-path 10 "/tmp/foo")))
    (should (string-match-p "tmp" result))
    (should (string-match-p "foo" result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; package-avalible-p

(ert-deftest xlib/package-avalible-p-returns-t-for-loaded-feature ()
  (provide 'xlib-test-fake-feature)
  (should (package-avalible-p 'xlib-test-fake-feature)))

(ert-deftest xlib/package-avalible-p-returns-nil-for-unknown-package ()
  (should-not (package-avalible-p 'xlib-test-nonexistent-package-zzz)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; approximate-project-root

(ert-deftest xlib/approximate-project-root-returns-string ()
  (should (stringp (approximate-project-root))))

(ert-deftest xlib/approximate-project-root-returns-absolute-path ()
  (should (f-absolute-p (approximate-project-root))))

(ert-deftest xlib/approximate-project-root-fallback-is-default-directory ()
  "When no project backend is active the result is default-directory."
  (let ((default-directory temporary-file-directory))
    (cl-letf (((symbol-function 'package-avalible-p) (lambda (_) nil))
              ((symbol-function 'package-installed-p) (lambda (_) nil))
              ((symbol-function 'project-current) (lambda (&rest _) nil)))
      (should (f-equal-p (expand-file-name temporary-file-directory)
                         (approximate-project-root))))))

(ert-deftest xlib/approximate-project-root-uses-project-el-when-available ()
  "When project.el reports a root it is returned."
  (let* ((expected "/tmp/fake-project/"))
    (cl-letf (((symbol-function 'package-avalible-p) (lambda (_) nil))
              ((symbol-function 'package-installed-p) (lambda (_) nil))
              ((symbol-function 'featurep)
               (lambda (sym &rest _)
                 (if (eq sym 'project) t (featurep sym))))
              ((symbol-function 'project-current) (lambda (&rest _) t))
              ((symbol-function 'project-root) (lambda (_) expected)))
      (should (equal expected (approximate-project-root))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; approximate-project-name

(ert-deftest xlib/approximate-project-name-returns-string ()
  (should (stringp (approximate-project-name))))

(ert-deftest xlib/approximate-project-name-non-empty ()
  (should (< 0 (length (approximate-project-name)))))

(ert-deftest xlib/approximate-project-name-fallback-is-directory-basename ()
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

(ert-deftest xlib/approximate-project-buffers-returns-list ()
  (should (listp (approximate-project-buffers))))

(ert-deftest xlib/approximate-project-buffers-contains-only-buffers ()
  (dolist (b (approximate-project-buffers))
    (should (bufferp b))))

(ert-deftest xlib/approximate-project-buffers-fallback-includes-current ()
  "When no project backend is active, the fallback includes the current buffer."
  (with-temp-buffer
    (let ((buf (current-buffer)))
      (cl-letf (((symbol-function 'package-avalible-p) (lambda (_) nil))
                ((symbol-function 'package-installed-p) (lambda (_) nil))
                ((symbol-function 'project-current) (lambda (&rest _) nil)))
        (should (memq buf (approximate-project-buffers)))))))

(provide 'test-xlib)
;;; test-xlib.el ends here
