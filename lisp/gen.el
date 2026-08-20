;;; gen.el --- Generator iterators as lazy seq.el sequences -*- lexical-binding: t -*-

;; Author: sam kleinman (tychoish)
;; Keywords: extensions, lisp, sequences, generators
;; URL: https://github.com/tychoish/dot-emacs

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Provides `seq.el' sequence operations over `generator.el' iterators
;; (`iter-lambda', `iter-defun', `iter-make').
;;
;; Raw `generator.el' iterators are plain closures and cannot be dispatched on
;; directly by `cl-generic'.  `gen-wrap' wraps a generator iterator in
;; a `gen' struct.  Once wrapped, standard `seq.el' operations
;; (`seq-map', `seq-filter', `seq-take', etc.) produce further lazy generators
;; that pull from the source on demand, while draining operations (`seq-length',
;; `seq-reverse', `seq-reduce', `seq-into', etc.) consume elements as needed.

;;; Code:

(require 'cl-lib)
(require 'generator)
(require 'seq)

;;; Core struct & wrapper

(cl-defstruct (gen
               (:constructor gen--make)
               (:copier nil))
  "A wrapped `generator.el' iterator providing lazy, composable `seq.el' operations."
  (iter nil :documentation "The raw generator.el closure iterator.")
  (peeked nil :documentation "A cons (VALUE . t) when a value has been peeked, else nil.")
  (exhausted nil :documentation "Non-nil when the iterator has signaled `iter-end-of-sequence'.")
  (infinite nil :documentation "Non-nil when the generator is known to be infinite."))

;;;###autoload
(defun gen-wrap (iter-or-gen &optional infinite)
  "Wrap ITER-OR-GEN in a `gen' struct for `seq.el' compatibility.
If ITER-OR-GEN is already a `gen', return it unchanged (updating
its infinite flag if INFINITE is non-nil).
If ITER-OR-GEN is a raw `generator.el' iterator closure or function, wrap it.
When INFINITE is non-nil, marks the generator as infinite so draining operations
can issue a warning before attempting full consumption.

Note on infinite generators: lazy operations (`seq-map', `seq-filter',
`seq-take', `seq-take-while', `seq-drop', `seq-drop-while', etc.) are safe
on infinite generators.  Draining operations (`seq-length', `seq-reverse',
`seq-sort', `seq-into', etc.) will consume forever unless bounded."
  (cond
   ((gen-p iter-or-gen)
    (when infinite
      (setf (gen-infinite iter-or-gen) t))
    iter-or-gen)
   ((functionp iter-or-gen)
    (gen--make :iter iter-or-gen :infinite infinite))
   (t
    (signal 'wrong-type-argument (list 'functionp iter-or-gen)))))

;;; Internal iteration helpers

(defun gen--check-infinite (g)
  "Warn if G is marked as infinite before a draining operation."
  (when (and (gen-p g) (gen-infinite g))
    (display-warning 'gen
                     (format "Draining infinite generator %S; this operation may not terminate." g)
                     :warning)))

(defun gen--next (g)
  "Fetch the next value from G as a cons (VALUE . t), or nil if exhausted.
Checks the peek buffer first, then the raw generator iterator."
  (unless (gen-exhausted g)
    (if-let* ((peek (gen-peeked g)))
        (progn
          (setf (gen-peeked g) nil)
          peek)
      (condition-case nil
          (cons (iter-next (gen-iter g)) t)
        (iter-end-of-sequence
         (setf (gen-exhausted g) t)
         nil)))))

(defun gen--peek (g)
  "Peek at the next value in G without consuming it.
Returns (VALUE . t), or nil if exhausted."
  (unless (gen-exhausted g)
    (or (gen-peeked g)
        (if-let* ((next (gen--next g)))
            (setf (gen-peeked g) next)
          nil))))

(defun gen--drain (g)
  "Consume all remaining values from G and return them as a list.
Calling this exhausts G."
  (gen--check-infinite g)
  (let (acc v)
    (while (setq v (gen--next g))
      (push (car v) acc))
    (nreverse acc)))

;;; seq.el protocol implementation

(cl-defmethod seqp ((_sequence gen))
  "Return t, declaring that `gen' is a `seq.el' sequence."
  t)

;;; Draining / eager methods

(cl-defmethod seq-length ((sequence gen))
  "Return the number of remaining elements in SEQUENCE.
Consumes and exhausts SEQUENCE."
  (gen--check-infinite sequence)
  (let ((count 0))
    (while (gen--next sequence)
      (setq count (1+ count)))
    count))

(cl-defmethod seq-count (predicate (sequence gen))
  "Count the number of remaining elements in SEQUENCE for which PREDICATE returns non-nil.
Consumes and exhausts SEQUENCE."
  (gen--check-infinite sequence)
  (let ((count 0)
        v)
    (while (setq v (gen--next sequence))
      (when (funcall predicate (car v))
        (setq count (1+ count))))
    count))

(cl-defmethod seq-reduce (function (sequence gen) initial-value)
  "Reduce FUNCTION across remaining elements of SEQUENCE starting with INITIAL-VALUE.
Consumes and exhausts SEQUENCE as a direct fold."
  (gen--check-infinite sequence)
  (let ((acc initial-value)
        v)
    (while (setq v (gen--next sequence))
      (setq acc (funcall function acc (car v))))
    acc))

(cl-defmethod seq-reverse ((sequence gen))
  "Return a list of remaining elements in SEQUENCE in reverse order.
Consumes and exhausts SEQUENCE."
  (gen--check-infinite sequence)
  (let (acc v)
    (while (setq v (gen--next sequence))
      (push (car v) acc))
    acc))

(cl-defmethod seq-sort (predicate (sequence gen))
  "Return a sorted list of remaining elements in SEQUENCE according to PREDICATE.
Consumes and exhausts SEQUENCE."
  (seq-sort predicate (gen--drain sequence)))

(cl-defmethod seq-elt ((sequence gen) n)
  "Return the Nth element of SEQUENCE (0-indexed).
Consumes up to N+1 elements from SEQUENCE."
  (unless (and (integerp n) (>= n 0))
    (error "Index out of range: %s" n))
  (let ((i 0)
        v found)
    (while (and (not found) (setq v (gen--next sequence)))
      (if (= i n)
          (setq found v)
        (setq i (1+ i))))
    (if found
        (car found)
      (error "Index out of range: %s" n))))

(cl-defmethod seq-subseq ((sequence gen) start &optional end)
  "Return a list of elements of SEQUENCE from index START up to END.
Consumes elements up to END (or the whole sequence if END is nil or negative)."
  (seq-subseq (gen--drain sequence) start end))

(cl-defmethod seq-into ((sequence gen) type)
  "Convert remaining elements of SEQUENCE into TYPE.
If TYPE is `gen', returns SEQUENCE.
Otherwise drains SEQUENCE and converts to TYPE."
  (cond
   ((eq type 'gen)
    sequence)
   (t
    (seq-into (gen--drain sequence) type))))

(cl-defmethod seq-into-sequence ((sequence gen))
  "Convert SEQUENCE to a sequence.  Returns SEQUENCE unchanged."
  sequence)

(cl-defmethod seq-copy ((sequence gen))
  "Return a copy of SEQUENCE.
Note: Since generator iterators are stateful closures, `seq-copy' returns
a new `gen' wrapping the remaining elements drained into a fresh iterator."
  (let ((remaining (gen--drain sequence)))
    (gen-wrap
     (iter-make
      (dolist (item remaining)
        (iter-yield item))))))

;;; Lazy / composable methods

(cl-defmethod seq-empty-p ((sequence gen))
  "Return non-nil if SEQUENCE has no remaining elements.
Uses lookahead peek without consuming the next element."
  (null (gen--peek sequence)))

(cl-defmethod seq-do (function (sequence gen))
  "Apply FUNCTION to each remaining element of SEQUENCE for side effects.
Consumes and exhausts SEQUENCE, returning SEQUENCE."
  (let (v)
    (while (setq v (gen--next sequence))
      (funcall function (car v))))
  sequence)

(cl-defmethod seq-map (function (sequence gen))
  "Return a new lazy `gen' yielding the result of applying FUNCTION to each element.
Does not consume elements from SEQUENCE until the returned generator is consumed."
  (gen-wrap
   (iter-make
    (let (v)
      (while (setq v (gen--next sequence))
        (iter-yield (funcall function (car v))))))))

(cl-defmethod seq-filter (predicate (sequence gen))
  "Return a new lazy `gen' yielding only elements for which PREDICATE returns non-nil."
  (gen-wrap
   (iter-make
    (let (v)
      (while (setq v (gen--next sequence))
        (when (funcall predicate (car v))
          (iter-yield (car v))))))))

(cl-defmethod seq-remove (predicate (sequence gen))
  "Return a new lazy `gen' yielding elements for which PREDICATE returns nil."
  (gen-wrap
   (iter-make
    (let (v)
      (while (setq v (gen--next sequence))
        (unless (funcall predicate (car v))
          (iter-yield (car v))))))))

(cl-defmethod seq-take ((sequence gen) n)
  "Return a new lazy `gen' yielding up to N elements from SEQUENCE."
  (gen-wrap
   (iter-make
    (let ((count (max 0 n))
          (i 0)
          v)
      (while (and (< i count) (setq v (gen--next sequence)))
        (setq i (1+ i))
        (iter-yield (car v)))))))

(cl-defmethod seq-take-while (predicate (sequence gen))
  "Return a new lazy `gen' yielding elements from SEQUENCE as long as PREDICATE returns non-nil."
  (gen-wrap
   (iter-make
    (let (v done)
      (while (and (not done) (setq v (gen--next sequence)))
        (if (funcall predicate (car v))
            (iter-yield (car v))
          (setq done t)))))))

(cl-defmethod seq-drop ((sequence gen) n)
  "Return a new lazy `gen' yielding elements after skipping the first N elements."
  (gen-wrap
   (iter-make
    (let ((count (max 0 n))
          (i 0))
      (while (and (< i count) (gen--next sequence))
        (setq i (1+ i)))
      (let (v)
        (while (setq v (gen--next sequence))
          (iter-yield (car v))))))))

(cl-defmethod seq-drop-while (predicate (sequence gen))
  "Return a new lazy `gen' yielding elements after dropping initial elements satisfying PREDICATE."
  (gen-wrap
   (iter-make
    (let (v found)
      (while (and (not found) (setq v (gen--next sequence)))
        (unless (funcall predicate (car v))
          (setq found t)
          (iter-yield (car v))))
      (when found
        (while (setq v (gen--next sequence))
          (iter-yield (car v))))))))

(cl-defmethod seq-some (predicate (sequence gen))
  "Return the first non-nil result of applying PREDICATE to an element of SEQUENCE.
Short-circuits: stops pulling from SEQUENCE as soon as a match is found."
  (let (res v)
    (while (and (null res) (setq v (gen--next sequence)))
      (let ((r (funcall predicate (car v))))
        (when r
          (setq res r))))
    res))

(cl-defmethod seq-every-p (predicate (sequence gen))
  "Return non-nil if PREDICATE returns non-nil for all elements in SEQUENCE.
Short-circuits: stops pulling from SEQUENCE as soon as a non-satisfying element is found."
  (let ((all-true t)
        v)
    (while (and all-true (setq v (gen--next sequence)))
      (unless (funcall predicate (car v))
        (setq all-true nil)))
    all-true))

(cl-defmethod seq-find (predicate (sequence gen) &optional default)
  "Return the first element of SEQUENCE for which PREDICATE returns non-nil, or DEFAULT.
Short-circuits: stops pulling from SEQUENCE as soon as a match is found."
  (let (found v)
    (while (and (not found) (setq v (gen--next sequence)))
      (when (funcall predicate (car v))
        (setq found v)))
    (if found
        (car found)
      default)))

(provide 'gen)
;;; gen.el ends here
