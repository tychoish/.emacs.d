;;; test-gen.el --- Tests for gen.el -*- lexical-binding: t -*-

(require 'ert)
(require 'cl-lib)
(require 'generator)
(require 'seq)
(require 'gen)

;;; Helpers

(defmacro gen-test--with-counting-generator (counter-var limit-or-infinite &rest body)
  "Create a counting generator bound to `g', incrementing COUNTER-VAR on each yield.
If LIMIT-OR-INFINITE is an integer, yields integers 1..LIMIT-OR-INFINITE.
If LIMIT-OR-INFINITE is `infinite', yields forever."
  (declare (indent 2))
  `(let ((,counter-var 0))
     (let ((g (gen-wrap
               (if (eq ,limit-or-infinite 'infinite)
                   (iter-make
                    (let ((i 1))
                      (while t
                        (setq ,counter-var (1+ ,counter-var))
                        (iter-yield i)
                        (setq i (1+ i)))))
                 (iter-make
                  (let ((i 1))
                    (while (<= i ,limit-or-infinite)
                      (setq ,counter-var (1+ ,counter-var))
                      (iter-yield i)
                      (setq i (1+ i))))))
               (eq ,limit-or-infinite 'infinite))))
       ,@body)))

;;; Type & wrapper checks

(ert-deftest gen/wrap-and-predicate ()
  (let* ((raw (iter-make (iter-yield 1)))
         (wrapped (gen-wrap raw)))
    (should (gen-p wrapped))
    (should (seqp wrapped))
    (should (eq wrapped (gen-wrap wrapped)))))

(ert-deftest gen/wrap-rejects-non-function ()
  (should-error (gen-wrap 42) :type 'wrong-type-argument)
  (should-error (gen-wrap "not-a-gen") :type 'wrong-type-argument))

;;; Laziness proofs

(ert-deftest gen/lazy-map-pulls-nothing-until-consumed ()
  "seq-map pulls zero items from source until the mapped generator is consumed."
  (gen-test--with-counting-generator count 5
    (let ((mapped (seq-map (lambda (x) (* x 10)) g)))
      (should (= 0 count))
      (should (gen-p mapped))
      (should (= 0 count))
      ;; Consuming pulls on demand
      (should (equal '(10 20 30 40 50) (gen--drain mapped)))
      (should (= 5 count)))))

(ert-deftest gen/lazy-filter-pulls-nothing-until-consumed ()
  "seq-filter pulls zero items from source until consumed."
  (gen-test--with-counting-generator count 5
    (let ((filtered (seq-filter #'cl-evenp g)))
      (should (= 0 count))
      (should (gen-p filtered))
      ;; Consume
      (should (equal '(2 4) (gen--drain filtered)))
      (should (= 5 count)))))

(ert-deftest gen/pipeline-composition-is-lazy ()
  "Multi-stage pipeline (map -> filter -> take) pulls only as needed."
  (gen-test--with-counting-generator count 100
    (let* ((mapped (seq-map (lambda (x) (* x 2)) g))
           (filtered (seq-filter (lambda (x) (> x 10)) mapped))
           (taken (seq-take filtered 3)))
      (should (= 0 count))
      ;; Draining `taken' needs:
      ;; 1 -> 2 (<= 10)
      ;; 2 -> 4 (<= 10)
      ;; 3 -> 6 (<= 10)
      ;; 4 -> 8 (<= 10)
      ;; 5 -> 10 (<= 10)
      ;; 6 -> 12 (> 10) -> take 1
      ;; 7 -> 14 (> 10) -> take 2
      ;; 8 -> 16 (> 10) -> take 3 (done!)
      (should (equal '(12 14 16) (gen--drain taken)))
      (should (= 8 count)))))

;;; Short-circuiting proofs

(ert-deftest gen/short-circuit-some ()
  "seq-some stops pulling as soon as predicate succeeds."
  (gen-test--with-counting-generator count 100
    (let ((found (seq-some (lambda (x) (when (= x 3) "matched-three")) g)))
      (should (equal "matched-three" found))
      (should (= 3 count)))))

(ert-deftest gen/short-circuit-find ()
  "seq-find stops pulling as soon as match is found."
  (gen-test--with-counting-generator count 100
    (let ((found (seq-find (lambda (x) (= x 4)) g)))
      (should (= 4 found))
      (should (= 4 count)))))

(ert-deftest gen/short-circuit-every-p ()
  "seq-every-p stops pulling on first non-satisfying element."
  (gen-test--with-counting-generator count 100
    (let ((res (seq-every-p (lambda (x) (< x 3)) g)))
      (should-not res)
      ;; 1 (< 3 true), 2 (< 3 true), 3 (< 3 false, abort!)
      (should (= 3 count)))))

;;; Infinite generator termination

(ert-deftest gen/infinite-take ()
  "seq-take on an infinite generator terminates and pulls exactly N items."
  (gen-test--with-counting-generator count 'infinite
    (let ((taken (seq-take g 5)))
      (should (= 0 count))
      (should (equal '(1 2 3 4 5) (gen--drain taken)))
      (should (= 5 count)))))

;;; Exhaustion semantics

(ert-deftest gen/draining-twice-returns-empty ()
  "Calling a draining method twice gets real results first, empty second."
  (let ((g (gen-wrap (iter-make (iter-yield 1) (iter-yield 2) (iter-yield 3)))))
    (should (equal '(1 2 3) (gen--drain g)))
    (should (equal '() (gen--drain g)))
    (should (equal '() (seq-reverse g)))
    (should (= 0 (seq-length g)))))

;;; Peek buffer integrity

(ert-deftest gen/empty-p-preserves-peeked-value ()
  "seq-empty-p peeks at the value without losing it for subsequent operations."
  (let ((g (gen-wrap (iter-make (iter-yield "first") (iter-yield "second")))))
    (should-not (seq-empty-p g))
    (should-not (seq-empty-p g))
    (should (equal '("first" "second") (gen--drain g)))
    (should (seq-empty-p g))))

;;; All other seq.el operations

(ert-deftest gen/seq-do ()
  (let ((g (gen-wrap (iter-make (iter-yield 1) (iter-yield 2) (iter-yield 3))))
        (acc nil))
    (seq-do (lambda (x) (push x acc)) g)
    (should (equal '(3 2 1) acc))))

(ert-deftest gen/seq-remove ()
  (let ((g (gen-wrap (iter-make (iter-yield 1) (iter-yield 2) (iter-yield 3) (iter-yield 4)))))
    (should (equal '(1 3) (gen--drain (seq-remove #'cl-evenp g))))))

(ert-deftest gen/seq-take-while ()
  (let ((g (gen-wrap (iter-make (iter-yield 1) (iter-yield 2) (iter-yield 3) (iter-yield 1)))))
    (should (equal '(1 2) (gen--drain (seq-take-while (lambda (x) (< x 3)) g))))))

(ert-deftest gen/seq-drop ()
  (let ((g (gen-wrap (iter-make (iter-yield 1) (iter-yield 2) (iter-yield 3) (iter-yield 4)))))
    (should (equal '(3 4) (gen--drain (seq-drop g 2))))))

(ert-deftest gen/seq-drop-while ()
  (let ((g (gen-wrap (iter-make (iter-yield 1) (iter-yield 2) (iter-yield 3) (iter-yield 4)))))
    (should (equal '(3 4) (gen--drain (seq-drop-while (lambda (x) (< x 3)) g))))))

(ert-deftest gen/seq-reduce ()
  (let ((g (gen-wrap (iter-make (iter-yield 1) (iter-yield 2) (iter-yield 3) (iter-yield 4)))))
    (should (= 10 (seq-reduce #'+ g 0)))))

(ert-deftest gen/seq-length-and-count ()
  (let ((g1 (gen-wrap (iter-make (iter-yield 1) (iter-yield 2) (iter-yield 3))))
        (g2 (gen-wrap (iter-make (iter-yield 1) (iter-yield 2) (iter-yield 3) (iter-yield 4)))))
    (should (= 3 (seq-length g1)))
    (should (= 2 (seq-count #'cl-evenp g2)))))

(ert-deftest gen/seq-reverse-and-sort ()
  (let ((g1 (gen-wrap (iter-make (iter-yield 1) (iter-yield 2) (iter-yield 3))))
        (g2 (gen-wrap (iter-make (iter-yield 3) (iter-yield 1) (iter-yield 2)))))
    (should (equal '(3 2 1) (seq-reverse g1)))
    (should (equal '(1 2 3) (seq-sort #'< g2)))))

(ert-deftest gen/seq-elt-and-subseq ()
  (let ((g1 (gen-wrap (iter-make (iter-yield 10) (iter-yield 20) (iter-yield 30))))
        (g2 (gen-wrap (iter-make (iter-yield 10) (iter-yield 20) (iter-yield 30) (iter-yield 40)))))
    (should (= 20 (seq-elt g1 1)))
    (should-error (seq-elt g1 10))
    (should (equal '(20 30) (seq-subseq g2 1 3)))))

(ert-deftest gen/seq-into ()
  (let ((g1 (gen-wrap (iter-make (iter-yield 1) (iter-yield 2))))
        (g2 (gen-wrap (iter-make (iter-yield 1) (iter-yield 2)))))
    (should (equal [1 2] (seq-into g1 'vector)))
    (should (eq g2 (seq-into g2 'gen)))))

(ert-deftest gen/seq-copy ()
  (let* ((g (gen-wrap (iter-make (iter-yield 1) (iter-yield 2))))
         (copied (seq-copy g)))
    (should (gen-p copied))
    (should (equal '(1 2) (gen--drain copied)))))

(ert-deftest gen/infinite-warning ()
  (let ((g (gen-wrap (iter-make (iter-yield 1) (iter-yield 2)) t))
        (warned nil))
    (cl-letf (((symbol-function 'display-warning)
               (lambda (&rest _) (setq warned t))))
      (should (= 2 (seq-length g)))
      (should warned))))

(provide 'test-gen)
;;; test-gen.el ends here
