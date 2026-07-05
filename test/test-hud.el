;;; test-hud.el --- ERT tests for hud.el -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for hud-command struct, hud-register-command, hud-reset-command-table,
;; hud--flat-entries, and hud--derive-category.

;;; Code:

(require 'ert)
(require 'hud)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hud-command struct

(ert-deftest hud-test/command-make-minimal ()
  "`make-hud-command' creates a valid struct; category is always non-nil."
  (let ((cmd (make-hud-command)))
    (should (hud-command-p cmd))
    (should (null (hud-command-command cmd)))
    (should (null (hud-command-description cmd)))
    (should (symbolp (hud-command-category cmd)))   ; derived, not nil
    (should (null (hud-command-transient-key cmd)))))

(ert-deftest hud-test/command-make-with-all-slots ()
  "All slots are stored and retrieved correctly."
  (let ((cmd (make-hud-command :command 'my-cmd
                               :description "do something"
                               :category 'my-cat
                               :transient-key "mc")))
    (should (eq 'my-cmd (hud-command-command cmd)))
    (should (equal "do something" (hud-command-description cmd)))
    (should (eq 'my-cat (hud-command-category cmd)))
    (should (equal "mc" (hud-command-transient-key cmd)))))

(ert-deftest hud-test/command-predicate ()
  "`hud-command-p' returns t for structs and nil for other values."
  (should (hud-command-p (make-hud-command)))
  (should-not (hud-command-p nil))
  (should-not (hud-command-p "string"))
  (should-not (hud-command-p '(command . foo))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hud-command constructor — category derivation

(ert-deftest hud-test/constructor-derives-category-when-nil ()
  "`make-hud-command' fills in :category when it is omitted."
  (let ((cmd (make-hud-command :command 'seq-find :description "find")))
    (should (symbolp (hud-command-category cmd)))
    (should (not (null (hud-command-category cmd))))))

(ert-deftest hud-test/constructor-keeps-explicit-category ()
  "`make-hud-command' preserves an explicit :category without modification."
  (let ((cmd (make-hud-command :command 'seq-find :category 'my-cat)))
    (should (eq 'my-cat (hud-command-category cmd)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hud--derive-category

(ert-deftest hud-test/derive-category-from-known-function ()
  "`hud--derive-category' returns a symbol based on the defining file."
  ;; `car' is defined in a C source or subr — symbol-file may return nil,
  ;; so this tests the fallback to 'hud.
  (let ((result (hud--derive-category 'car)))
    (should (symbolp result))))

(ert-deftest hud-test/derive-category-fallback-to-hud ()
  "`hud--derive-category' returns \\='hud for undefined commands."
  (should (eq 'hud (hud--derive-category 'hud-test--nonexistent-command-xyz))))

(ert-deftest hud-test/register-command-derives-category-when-omitted ()
  "Omitting :category auto-derives it; the entry lands in some category bucket."
  (let ((hud-command-table '()))
    ;; `seq-find' is a known function — symbol-file will return something
    (hud-register-command :command 'seq-find :description "find" :transient-key "sf")
    (should (= 1 (length hud-command-table)))
    (should (= 1 (length (cdr (car hud-command-table)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hud-register-command

(ert-deftest hud-test/register-command-creates-new-category ()
  "Registering into an empty table creates a new category bucket."
  (let ((hud-command-table '()))
    (hud-register-command :category 'tools
                          :command 'my-tool
                          :description "run tool"
                          :transient-key "tt")
    (should (assq 'tools hud-command-table))
    (should (= 1 (length (cdr (assq 'tools hud-command-table)))))))

(ert-deftest hud-test/register-command-appends-to-existing-category ()
  "Registering a second command in the same category appends to the bucket."
  (let ((hud-command-table '()))
    (hud-register-command :category 'tools :command 'cmd-a :description "a" :transient-key "ta")
    (hud-register-command :category 'tools :command 'cmd-b :description "b" :transient-key "tb")
    (should (= 2 (length (cdr (assq 'tools hud-command-table)))))
    (let ((entries (cdr (assq 'tools hud-command-table))))
      (should (eq 'cmd-a (hud-command-command (nth 0 entries))))
      (should (eq 'cmd-b (hud-command-command (nth 1 entries)))))))

(ert-deftest hud-test/register-command-multiple-categories ()
  "Commands in different categories create separate buckets."
  (let ((hud-command-table '()))
    (hud-register-command :category 'alpha :command 'cmd-a :description "a" :transient-key "aa")
    (hud-register-command :category 'beta  :command 'cmd-b :description "b" :transient-key "bb")
    (should (assq 'alpha hud-command-table))
    (should (assq 'beta  hud-command-table))
    (should (= 1 (length (cdr (assq 'alpha hud-command-table)))))
    (should (= 1 (length (cdr (assq 'beta  hud-command-table)))))))

(ert-deftest hud-test/register-command-stores-all-fields ()
  "All keyword arguments are stored in the resulting struct."
  (let ((hud-command-table '()))
    (hud-register-command :category 'grp
                          :command 'do-it
                          :description "do it now"
                          :transient-key "di")
    (let ((cmd (car (cdr (assq 'grp hud-command-table)))))
      (should (eq 'do-it (hud-command-command cmd)))
      (should (equal "do it now" (hud-command-description cmd)))
      (should (eq 'grp (hud-command-category cmd)))
      (should (equal "di" (hud-command-transient-key cmd))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hud-command display-name slot

(ert-deftest hud-test/constructor-display-name-defaults-to-symbol-name ()
  "When :display-name is omitted the constructor uses the command symbol name."
  (let ((cmd (make-hud-command :command 'seq-find)))
    (should (equal "seq-find" (hud-command-display-name cmd)))))

(ert-deftest hud-test/constructor-display-name-explicit ()
  "An explicit :display-name is stored as-is."
  (let ((cmd (make-hud-command :command 'seq-find :display-name "find")))
    (should (equal "find" (hud-command-display-name cmd)))))

(ert-deftest hud-test/register-command-stores-display-name ()
  "`hud-register-command' stores an explicit :display-name."
  (let ((hud-command-table '()))
    (hud-register-command :category 'grp :command 'seq-find
                          :description "find" :display-name "my-find"
                          :transient-key "f")
    (let ((cmd (car (cdr (assq 'grp hud-command-table)))))
      (should (equal "my-find" (hud-command-display-name cmd))))))

(ert-deftest hud-test/register-command-display-name-falls-back-to-symbol ()
  "When :display-name is omitted the registration uses the symbol name."
  (let ((hud-command-table '()))
    (hud-register-command :category 'grp :command 'seq-find
                          :description "find" :transient-key "f")
    (let ((cmd (car (cdr (assq 'grp hud-command-table)))))
      (should (equal "seq-find" (hud-command-display-name cmd))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hud-register-command — idempotency and uniqueness

(ert-deftest hud-test/register-command-overwrites-same-category-command ()
  "Re-registering the same category+command replaces the entry, not appends."
  (let ((hud-command-table '()))
    (hud-register-command :category 'tools :command 'cmd-a :description "original" :transient-key "ta")
    (hud-register-command :category 'tools :command 'cmd-a :description "updated"  :transient-key "ta")
    (let ((bucket (cdr (assq 'tools hud-command-table))))
      (should (= 1 (length bucket)))
      (should (equal "updated" (hud-command-description (car bucket)))))))

(ert-deftest hud-test/register-command-overwrite-preserves-order ()
  "Overwriting a command preserves its position in the bucket."
  (let ((hud-command-table '()))
    (hud-register-command :category 'tools :command 'cmd-a :description "a" :transient-key "ta")
    (hud-register-command :category 'tools :command 'cmd-b :description "b" :transient-key "tb")
    (hud-register-command :category 'tools :command 'cmd-a :description "a2" :transient-key "ta")
    (let ((bucket (cdr (assq 'tools hud-command-table))))
      (should (= 2 (length bucket)))
      (should (eq 'cmd-a (hud-command-command (nth 0 bucket))))
      (should (equal "a2" (hud-command-description (nth 0 bucket))))
      (should (eq 'cmd-b (hud-command-command (nth 1 bucket)))))))

(ert-deftest hud-test/register-command-rejects-duplicate-transient-key ()
  "Registering a command with a transient-key already in use signals `user-error'."
  (let ((hud-command-table '()))
    (hud-register-command :category 'tools :command 'cmd-a :description "a" :transient-key "tt")
    (should-error
     (hud-register-command :category 'tools :command 'cmd-b :description "b" :transient-key "tt")
     :type 'user-error)))

(ert-deftest hud-test/register-command-rejects-key-conflict-across-categories ()
  "A transient-key conflict is detected even when the categories differ."
  (let ((hud-command-table '()))
    (hud-register-command :category 'alpha :command 'cmd-a :description "a" :transient-key "xx")
    (should-error
     (hud-register-command :category 'beta :command 'cmd-b :description "b" :transient-key "xx")
     :type 'user-error)))

(ert-deftest hud-test/register-command-allows-same-key-on-overwrite ()
  "Re-registering the same category+command with the same key does not error."
  (let ((hud-command-table '()))
    (hud-register-command :category 'tools :command 'cmd-a :description "v1" :transient-key "ta")
    (hud-register-command :category 'tools :command 'cmd-a :description "v2" :transient-key "ta")
    (should (equal "v2" (hud-command-description
                         (car (cdr (assq 'tools hud-command-table))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hud-reset-command-table

(ert-deftest hud-test/reset-command-table-clears-entries ()
  "`hud-reset-command-table' empties `hud-command-table'."
  (let ((hud-command-table '()))
    (hud-register-command :category 'x :command 'foo :description "f" :transient-key "f")
    (hud-reset-command-table)
    (should (null hud-command-table))))

(ert-deftest hud-test/reset-command-table-on-empty-table ()
  "`hud-reset-command-table' is a no-op on an already empty table."
  (let ((hud-command-table '()))
    (hud-reset-command-table)
    (should (null hud-command-table))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hud--flat-entries

(ert-deftest hud-test/flat-entries-empty-table ()
  "`hud--flat-entries' returns nil for an empty table."
  (let ((hud-command-table '()))
    (should (null (hud--flat-entries)))))

(ert-deftest hud-test/flat-entries-single-category ()
  "Each entry is a cons of (category . hud-command)."
  (let ((hud-command-table '()))
    (hud-register-command :category 'tools :command 'cmd-a :description "a" :transient-key "ta")
    (hud-register-command :category 'tools :command 'cmd-b :description "b" :transient-key "tb")
    (let ((entries (hud--flat-entries)))
      (should (= 2 (length entries)))
      (should (seq-every-p (lambda (pair)
                             (and (eq 'tools (car pair))
                                  (hud-command-p (cdr pair))))
                           entries)))))

(ert-deftest hud-test/flat-entries-multiple-categories ()
  "All commands from all categories appear in the flat list."
  (let ((hud-command-table '()))
    (hud-register-command :category 'alpha :command 'cmd-a :description "a" :transient-key "aa")
    (hud-register-command :category 'beta  :command 'cmd-b :description "b" :transient-key "bb")
    (hud-register-command :category 'beta  :command 'cmd-c :description "c" :transient-key "bc")
    (let ((entries (hud--flat-entries)))
      (should (= 3 (length entries)))
      (should (seq-find (lambda (p) (eq 'alpha (car p))) entries))
      (should (= 2 (length (seq-filter (lambda (p) (eq 'beta (car p))) entries)))))))

(ert-deftest hud-test/flat-entries-category-matches-command-slot ()
  "The car of each pair matches the :category slot of the hud-command."
  (let ((hud-command-table '()))
    (hud-register-command :category 'group :command 'foo :description "foo" :transient-key "f")
    (let ((pair (car (hud--flat-entries))))
      (should (eq (car pair) (hud-command-category (cdr pair)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; :exclude-from-transient — struct slot

(ert-deftest hud-test/command-exclude-from-transient-default-nil ()
  "New commands have nil exclude-from-transient by default."
  (let ((cmd (make-hud-command :command 'foo :description "f" :transient-key "f")))
    (should (null (hud-command-exclude-from-transient cmd)))))

(ert-deftest hud-test/command-exclude-from-transient-stored ()
  "`make-hud-command' stores a non-nil :exclude-from-transient value."
  (let ((cmd (make-hud-command :command 'foo :description "f" :transient-key "f"
                               :exclude-from-transient t)))
    (should (hud-command-exclude-from-transient cmd))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; :exclude-from-transient — hud-register-command

(ert-deftest hud-test/register-command-exclude-from-transient-stored ()
  ":exclude-from-transient t is stored in the struct slot."
  (let ((hud-command-table '()))
    (hud-register-command :category 'g :command 'foo :description "f"
                          :transient-key "f" :exclude-from-transient t)
    (let ((cmd (car (cdr (assq 'g hud-command-table)))))
      (should (hud-command-exclude-from-transient cmd)))))

(ert-deftest hud-test/register-command-exclude-from-transient-default-nil ()
  "Without :exclude-from-transient the slot is nil."
  (let ((hud-command-table '()))
    (hud-register-command :category 'g :command 'foo :description "f" :transient-key "f")
    (let ((cmd (car (cdr (assq 'g hud-command-table)))))
      (should (null (hud-command-exclude-from-transient cmd))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; :exclude-from-transient — transient suffix builder

(ert-deftest hud-test/bucket-to-column-omits-excluded-commands ()
  "`hud--bucket-to-column' skips commands with exclude-from-transient set."
  (let* ((included (make-hud-command :command 'cmd-a :description "A" :category 'g
                                     :transient-key "a"))
         (excluded (make-hud-command :command 'cmd-b :description "B" :category 'g
                                     :transient-key "b" :exclude-from-transient t))
         (col (hud--bucket-to-column (cons 'g (list included excluded)))))
    ;; col is a vector: ["g" suffix-for-a]  — excluded never appears
    (should (= 2 (length col)))
    (should (equal "a" (nth 0 (aref col 1))))))

(ert-deftest hud-test/bucket-to-column-all-excluded-yields-empty-column ()
  "A bucket where all commands are excluded produces a header-only column."
  (let* ((cmd (make-hud-command :command 'foo :description "F" :category 'g
                                :transient-key "f" :exclude-from-transient t))
         (col (hud--bucket-to-column (cons 'g (list cmd)))))
    (should (= 1 (length col)))
    (should (equal "g" (aref col 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; :exclude-from-transient — ACR entry lookup still includes the item

(ert-deftest hud-test/entry-lookup-includes-exclude-from-transient ()
  "Items with :exclude-from-transient still appear in `hud--entry-lookup'."
  (let ((hud-command-table '()))
    (hud-register-command :category 'g :command 'foo :description "f"
                          :transient-key "f" :exclude-from-transient t)
    (should (= 1 (hash-table-count (hud--entry-lookup))))))

(ert-deftest hud-test/entry-lookup-exclude-from-transient-with-truthy-if ()
  "An item can be ACR-only (:exclude-from-transient) and still respect :if."
  (let ((hud-command-table '()))
    (hud-register-command :category 'g :command 'foo :description "f"
                          :transient-key "f"
                          :exclude-from-transient t
                          :if (lambda () nil))
    (should (= 0 (hash-table-count (hud--entry-lookup))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; :if and :ifapt predicates — struct slots

(ert-deftest hud-test/command-visible-inapt-default-nil ()
  "New commands have nil visible and inapt slots by default."
  (let ((cmd (make-hud-command :command 'foo :description "f" :transient-key "f")))
    (should (null (hud-command-visible cmd)))
    (should (null (hud-command-inapt cmd)))))

(ert-deftest hud-test/command-visible-inapt-stored ()
  "`make-hud-command' stores :visible and :inapt predicates."
  (let* ((pred-v (lambda () t))
         (pred-i (lambda () nil))
         (cmd (make-hud-command :command 'foo :description "f" :transient-key "f"
                                :visible pred-v :inapt pred-i)))
    (should (eq pred-v (hud-command-visible cmd)))
    (should (eq pred-i (hud-command-inapt cmd)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; :if and :ifapt predicates — hud-register-command

(ert-deftest hud-test/register-command-if-ifapt-default-nil ()
  "Without :if or :ifapt the slots default to nil."
  (let ((hud-command-table '()))
    (hud-register-command :category 'g :command 'foo :description "f" :transient-key "f")
    (let ((cmd (car (cdr (assq 'g hud-command-table)))))
      (should (null (hud-command-visible cmd)))
      (should (null (hud-command-inapt cmd))))))

(ert-deftest hud-test/register-command-stores-if-predicate ()
  ":if predicate is stored in the visible slot."
  (let ((hud-command-table '())
        (pred (lambda () t)))
    (hud-register-command :category 'g :command 'foo :description "f"
                          :transient-key "f" :if pred)
    (let ((cmd (car (cdr (assq 'g hud-command-table)))))
      (should (eq pred (hud-command-visible cmd))))))

(ert-deftest hud-test/register-command-stores-ifapt-predicate ()
  ":ifapt predicate is stored in the inapt slot."
  (let ((hud-command-table '())
        (pred (lambda () nil)))
    (hud-register-command :category 'g :command 'foo :description "f"
                          :transient-key "f" :ifapt pred)
    (let ((cmd (car (cdr (assq 'g hud-command-table)))))
      (should (eq pred (hud-command-inapt cmd))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; :if and :ifapt predicates — transient suffix builder

(ert-deftest hud-test/bucket-to-column-no-predicates ()
  "`hud--bucket-to-column' emits a plain 3-element suffix when no predicates are set."
  (let* ((cmd (make-hud-command :command 'foo :description "Foo" :category 'g
                                :transient-key "f"))
         (suffix (aref (hud--bucket-to-column (cons 'g (list cmd))) 1)))
    (should (= 3 (length suffix)))
    (should (equal "f" (nth 0 suffix)))
    (should (equal "Foo" (nth 1 suffix)))
    (should (eq 'foo (nth 2 suffix)))))

(ert-deftest hud-test/bucket-to-column-with-if-predicate ()
  "`hud--bucket-to-column' appends :if PRED when visible is set."
  (let* ((pred (lambda () t))
         (cmd (make-hud-command :command 'foo :description "Foo" :category 'g
                                :transient-key "f" :visible pred))
         (suffix (aref (hud--bucket-to-column (cons 'g (list cmd))) 1)))
    (should (= 5 (length suffix)))
    (should (eq :if (nth 3 suffix)))
    (should (eq pred (nth 4 suffix)))))

(ert-deftest hud-test/bucket-to-column-with-ifapt-predicate ()
  "`hud--bucket-to-column' appends :inapt-if PRED when inapt is set."
  (let* ((pred (lambda () nil))
         (cmd (make-hud-command :command 'foo :description "Foo" :category 'g
                                :transient-key "f" :inapt pred))
         (suffix (aref (hud--bucket-to-column (cons 'g (list cmd))) 1)))
    (should (= 5 (length suffix)))
    (should (eq :inapt-if (nth 3 suffix)))
    (should (eq pred (nth 4 suffix)))))

(ert-deftest hud-test/bucket-to-column-with-both-predicates ()
  "`hud--bucket-to-column' appends both :if and :inapt-if when both are set."
  (let* ((pred-v (lambda () t))
         (pred-i (lambda () nil))
         (cmd (make-hud-command :command 'foo :description "Foo" :category 'g
                                :transient-key "f" :visible pred-v :inapt pred-i))
         (suffix (aref (hud--bucket-to-column (cons 'g (list cmd))) 1)))
    (should (= 7 (length suffix)))
    (should (eq :if (nth 3 suffix)))
    (should (eq pred-v (nth 4 suffix)))
    (should (eq :inapt-if (nth 5 suffix)))
    (should (eq pred-i (nth 6 suffix)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; :if and :ifapt predicates — ACR entry lookup (hud--entry-lookup)

(ert-deftest hud-test/entry-lookup-includes-nil-if ()
  "`hud--entry-lookup' includes entries with nil :if (always visible)."
  (let ((hud-command-table '()))
    (hud-register-command :category 'g :command 'foo :description "f" :transient-key "f")
    (should (= 1 (hash-table-count (hud--entry-lookup))))))

(ert-deftest hud-test/entry-lookup-includes-truthy-if ()
  "`hud--entry-lookup' includes entries whose :if predicate returns non-nil."
  (let ((hud-command-table '()))
    (hud-register-command :category 'g :command 'foo :description "f"
                          :transient-key "f" :if (lambda () t))
    (should (= 1 (hash-table-count (hud--entry-lookup))))))

(ert-deftest hud-test/entry-lookup-excludes-falsy-if ()
  "`hud--entry-lookup' excludes entries whose :if predicate returns nil."
  (let ((hud-command-table '()))
    (hud-register-command :category 'g :command 'foo :description "f"
                          :transient-key "f" :if (lambda () nil))
    (should (= 0 (hash-table-count (hud--entry-lookup))))))

(ert-deftest hud-test/entry-lookup-ifapt-alone-does-not-hide ()
  ":ifapt alone does not hide an entry from the ACR selector."
  (let ((hud-command-table '()))
    (hud-register-command :category 'g :command 'foo :description "f"
                          :transient-key "f" :ifapt (lambda () t))
    (should (= 1 (hash-table-count (hud--entry-lookup))))))

(ert-deftest hud-test/entry-lookup-mixed-visibility ()
  "Only visible entries appear; entries with falsy :if are excluded."
  (let ((hud-command-table '()))
    (hud-register-command :category 'g :command 'visible-cmd :description "v"
                          :transient-key "v")
    (hud-register-command :category 'g :command 'hidden-cmd :description "h"
                          :transient-key "h" :if (lambda () nil))
    (let ((lookup (hud--entry-lookup)))
      (should (= 1 (hash-table-count lookup)))
      (should (map-elt lookup "visible-cmd"))
      (should (null (map-elt lookup "hidden-cmd"))))))

(provide 'test-hud)
;;; test-hud.el ends here
