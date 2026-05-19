;;; test-agent-shell-queue-org.el --- ERT tests for agent-shell-queue-org -*- lexical-binding: t -*-

;; Run inside a live Emacs session with the full config loaded:
;;   M-x ert RET t RET
;; or filtered:
;;   (ert "^agent-shell-queue-org/")
;;
;; Batch run (requires agent-shell and org on the load path):
;;   emacs --batch -L ~/.emacs.d/lisp \
;;     --eval '(progn (setq package-user-dir "~/.emacs.d/elpa") (package-initialize))' \
;;     -l ~/.emacs.d/test/test-agent-shell-queue-org.el \
;;     --eval '(ert-run-tests-batch-and-exit "agent-shell-queue-org/")'

(require 'ert)
(require 'cl-lib)

(defvar agent-shell-queue-org-test--load-path
  (expand-file-name "lisp" (file-name-directory
                            (directory-file-name
                             (file-name-directory
                              (or load-file-name buffer-file-name))))))

(unless (featurep 'agent-shell-queue-org)
  (add-to-list 'load-path agent-shell-queue-org-test--load-path)
  (require 'agent-shell-queue-org))

;;; Test helpers

(defun agent-shell-queue-org-test/make-item
    (id prompt &optional status background kind created dispatched completed)
  "Build a queue item for testing."
  (agent-shell-queue-item--make
   :id id
   :prompt prompt
   :status (or status 'active)
   :kind (or kind 'prompt)
   :background background
   :created (or created 1000.0)
   :dispatched dispatched
   :completed completed))

(defun agent-shell-queue-org-test/serialize-items (items-alist)
  "Serialize ITEMS-ALIST via the org format and return the string."
  (let ((agent-shell-queue--items items-alist))
    (agent-shell-queue-org--serialize)))

(defun agent-shell-queue-org-test/parse-body (org-str)
  "Extract the body from the first level-2 headline in ORG-STR.
Calls `agent-shell-queue-org--body-from-element' within the temp buffer
where the tree was parsed, so buffer positions remain valid."
  (with-temp-buffer
    (insert org-str)
    (org-mode)
    (let* ((tree (org-element-parse-buffer))
           (l1 (car (agent-shell-queue-org--headlines-of tree)))
           (l2 (when l1 (car (agent-shell-queue-org--headlines-of l1)))))
      (when l2 (agent-shell-queue-org--body-from-element l2)))))

(defun agent-shell-queue-org-test/parse-props (org-str)
  "Extract properties from the first level-2 headline in ORG-STR."
  (with-temp-buffer
    (insert org-str)
    (org-mode)
    (let* ((tree (org-element-parse-buffer))
           (l1 (car (agent-shell-queue-org--headlines-of tree)))
           (l2 (when l1 (car (agent-shell-queue-org--headlines-of l1)))))
      (when l2 (agent-shell-queue-org--props-from-element l2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Shared helper tests

(ert-deftest agent-shell-queue-org/heading-title/simple ()
  (should (equal "do the thing"
                 (agent-shell-queue-org--heading-title "do the thing"))))

(ert-deftest agent-shell-queue-org/heading-title/multiline ()
  (should (equal "first line"
                 (agent-shell-queue-org--heading-title "first line\nsecond line\nthird"))))

(ert-deftest agent-shell-queue-org/heading-title/truncate ()
  (let ((long (make-string 80 ?x)))
    (should (equal (concat (make-string 57 ?x) "...")
                   (agent-shell-queue-org--heading-title long)))))

(ert-deftest agent-shell-queue-org/heading-title/exact-boundary ()
  "60-char prompt is not truncated; 61-char prompt is."
  (should (equal (make-string 60 ?x)
                 (agent-shell-queue-org--heading-title (make-string 60 ?x))))
  (should (string-suffix-p "..."
                           (agent-shell-queue-org--heading-title (make-string 61 ?x)))))

(ert-deftest agent-shell-queue-org/heading-title/empty-string ()
  (should (equal "(empty)" (agent-shell-queue-org--heading-title ""))))

(ert-deftest agent-shell-queue-org/heading-title/nil ()
  (should (equal "(empty)" (agent-shell-queue-org--heading-title nil))))

(ert-deftest agent-shell-queue-org/heading-title/whitespace-only ()
  (should (equal "(empty)" (agent-shell-queue-org--heading-title "   \n  "))))

(ert-deftest agent-shell-queue-org/ts/number ()
  (should (equal "1705318200.5" (agent-shell-queue-org--ts 1705318200.5))))

(ert-deftest agent-shell-queue-org/ts/integer ()
  (should (equal "1000" (agent-shell-queue-org--ts 1000))))

(ert-deftest agent-shell-queue-org/ts/nil ()
  (should (equal "nil" (agent-shell-queue-org--ts nil))))

(ert-deftest agent-shell-queue-org/parse-ts/float ()
  (should (= 1705318200.5 (agent-shell-queue-org--parse-ts "1705318200.5"))))

(ert-deftest agent-shell-queue-org/parse-ts/integer ()
  (should (= 1000.0 (agent-shell-queue-org--parse-ts "1000"))))

(ert-deftest agent-shell-queue-org/parse-ts/nil-string ()
  (should (null (agent-shell-queue-org--parse-ts "nil"))))

(ert-deftest agent-shell-queue-org/parse-ts/empty ()
  (should (null (agent-shell-queue-org--parse-ts "")))
  (should (null (agent-shell-queue-org--parse-ts "  "))))

(ert-deftest agent-shell-queue-org/parse-ts/nil ()
  (should (null (agent-shell-queue-org--parse-ts nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generation — insert-item produces correct org structure

(ert-deftest agent-shell-queue-org/insert-item/property-drawer ()
  "org-set-property produces a readable property drawer."
  (let ((item (agent-shell-queue-org-test/make-item
               "q3abc" "test prompt" 'active nil 'prompt 1705318200.5)))
    (with-temp-buffer
      (org-mode)
      (insert "* bucket\n\n")
      (agent-shell-queue-org--insert-item item)
      (goto-char (point-min))
      (search-forward "** TODO")
      (org-back-to-heading)
      ;; org-entry-get converts the string "nil" to Lisp nil unless literal-nil=t
      (should (equal "q3abc"        (org-entry-get (point) "QUEUE-ID")))
      (should (equal "active"       (org-entry-get (point) "STATUS")))
      (should (equal "prompt"       (org-entry-get (point) "KIND")))
      (should (equal "nil"          (org-entry-get (point) "BACKGROUND" nil t)))
      (should (equal "1705318200.5" (org-entry-get (point) "CREATED")))
      (should (equal "nil"          (org-entry-get (point) "DISPATCHED" nil t)))
      (should (equal "nil"          (org-entry-get (point) "COMPLETED" nil t))))))

(ert-deftest agent-shell-queue-org/insert-item/background-flag ()
  "BACKGROUND is \"t\" when item has background=t."
  (let ((item (agent-shell-queue-org-test/make-item
               "qbg" "bg task" 'active t)))
    (with-temp-buffer
      (org-mode)
      (insert "* bucket\n\n")
      (agent-shell-queue-org--insert-item item)
      (goto-char (point-min))
      (search-forward "**")
      (org-back-to-heading)
      (should (equal "t" (org-entry-get (point) "BACKGROUND"))))))

(ert-deftest agent-shell-queue-org/insert-item/todo-keyword-active ()
  (let ((item (agent-shell-queue-org-test/make-item "q1" "p" 'active)))
    (with-temp-buffer
      (org-mode)
      (insert "* b\n\n")
      (agent-shell-queue-org--insert-item item)
      (should (string-match-p "\\*\\* TODO " (buffer-string))))))

(ert-deftest agent-shell-queue-org/insert-item/todo-keyword-running ()
  (let ((item (agent-shell-queue-org-test/make-item "q1" "p" 'running)))
    (with-temp-buffer
      (org-mode)
      (insert "* b\n\n")
      (agent-shell-queue-org--insert-item item)
      (should (string-match-p "\\*\\* DOING " (buffer-string))))))

(ert-deftest agent-shell-queue-org/insert-item/todo-keyword-done ()
  (let ((item (agent-shell-queue-org-test/make-item "q1" "p" 'done)))
    (with-temp-buffer
      (org-mode)
      (insert "* b\n\n")
      (agent-shell-queue-org--insert-item item)
      (should (string-match-p "\\*\\* DONE " (buffer-string))))))

(ert-deftest agent-shell-queue-org/insert-item/body-indented ()
  "Prompt body lines appear with 3-space indentation."
  (let ((item (agent-shell-queue-org-test/make-item "q1" "line one\nline two")))
    (with-temp-buffer
      (org-mode)
      (insert "* b\n\n")
      (agent-shell-queue-org--insert-item item)
      (should (string-match-p "^   line one$"  (buffer-string)))
      (should (string-match-p "^   line two$"  (buffer-string))))))

(ert-deftest agent-shell-queue-org/insert-item/org-special-in-prompt ()
  "Prompt lines starting with * or :PROPERTIES: are safely indented."
  (let ((item (agent-shell-queue-org-test/make-item
               "q1" "** not a heading\n:END: not a drawer")))
    (with-temp-buffer
      (org-mode)
      (insert "* b\n\n")
      (agent-shell-queue-org--insert-item item)
      (let ((text (buffer-string)))
        (should (string-match-p "^   \\*\\* not a heading$"   text))
        (should (string-match-p "^   :END: not a drawer$" text))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Deserialization — org-element based parsing

(ert-deftest agent-shell-queue-org/props-from-element/basic ()
  "Property drawer entries are extracted as a string-keyed alist."
  (let ((props (agent-shell-queue-org-test/parse-props
                "* bucket\n** TODO item\n   :PROPERTIES:\n   :QUEUE-ID: q1\n   :STATUS: active\n   :END:\n\n   body\n")))
    (should (equal "q1"     (cdr (assoc "QUEUE-ID" props))))
    (should (equal "active" (cdr (assoc "STATUS"   props))))))

(ert-deftest agent-shell-queue-org/props-from-element/no-bleed ()
  "Properties from a sibling headline don't appear in the first one."
  (let ((org-str
         (concat "* bucket\n"
                 "** TODO first\n"
                 "   :PROPERTIES:\n   :QUEUE-ID: q1\n   :END:\n\n   body1\n\n"
                 "** TODO second\n"
                 "   :PROPERTIES:\n   :QUEUE-ID: q2\n   :END:\n\n   body2\n")))
    (let ((props (agent-shell-queue-org-test/parse-props org-str)))
      (should (equal "q1" (cdr (assoc "QUEUE-ID" props)))))))

(ert-deftest agent-shell-queue-org/body-from-element/simple ()
  (should (equal "the prompt"
                 (agent-shell-queue-org-test/parse-body
                  "* b\n** TODO x\n   :PROPERTIES:\n   :QUEUE-ID: q1\n   :END:\n\n   the prompt\n"))))

(ert-deftest agent-shell-queue-org/body-from-element/multiline ()
  (should (equal "line one\nline two"
                 (agent-shell-queue-org-test/parse-body
                  "* b\n** TODO x\n   :PROPERTIES:\n   :QUEUE-ID: q1\n   :END:\n\n   line one\n   line two\n"))))

(ert-deftest agent-shell-queue-org/body-from-element/strips-indent ()
  "3-space indent is stripped; non-indented lines are left alone."
  (should (equal "indented\nnot"
                 (agent-shell-queue-org-test/parse-body
                  "* b\n** TODO x\n   :PROPERTIES:\n   :END:\n\n   indented\nnot\n"))))

(ert-deftest agent-shell-queue-org/body-from-element/org-content-preserved ()
  "Prompt body containing org-like text survives deserialization."
  (let* ((prompt "** not a heading\n:END: not a drawer\n[[link]]")
         (org-str
          (concat "* b\n** TODO x\n   :PROPERTIES:\n   :QUEUE-ID: q1\n   :END:\n\n"
                  (mapconcat (lambda (l) (concat "   " l)) (split-string prompt "\n") "\n")
                  "\n")))
    (should (equal prompt (agent-shell-queue-org-test/parse-body org-str)))))

(ert-deftest agent-shell-queue-org/deserialize/bucket-names ()
  "Level-1 heading text becomes the bucket name."
  (let* ((result (agent-shell-queue-org--deserialize
                  "* *test-buf*\n\n** TODO x\n   :PROPERTIES:\n   :QUEUE-ID: q1\n   :END:\n\n   p\n")))
    (should (equal "*test-buf*" (caar result)))))

(ert-deftest agent-shell-queue-org/deserialize/unassigned-bucket ()
  (let* ((result (agent-shell-queue-org--deserialize
                  "* (unassigned)\n\n** TODO x\n   :PROPERTIES:\n   :QUEUE-ID: q1\n   :END:\n\n   p\n")))
    (should (equal "(unassigned)" (caar result)))))

(ert-deftest agent-shell-queue-org/deserialize/item-fields ()
  "All item struct fields are recovered correctly."
  (let* ((result (agent-shell-queue-org--deserialize
                  (concat "* *buf*\n\n"
                          "** TODO prompt text\n"
                          "   :PROPERTIES:\n"
                          "   :QUEUE-ID: q9abc\n"
                          "   :STATUS: active\n"
                          "   :KIND: prompt\n"
                          "   :BACKGROUND: t\n"
                          "   :CREATED: 1705318200.5\n"
                          "   :DISPATCHED: nil\n"
                          "   :COMPLETED: nil\n"
                          "   :END:\n\n"
                          "   prompt text\n")))
         (item (car (cdr (car result)))))
    (should (equal "q9abc"   (agent-shell-queue-item-id item)))
    (should (equal "prompt text" (agent-shell-queue-item-prompt item)))
    (should (eq 'active      (agent-shell-queue-item-status item)))
    (should (eq 'prompt      (agent-shell-queue-item-kind item)))
    (should (eq t            (agent-shell-queue-item-background item)))
    (should (= 1705318200.5  (agent-shell-queue-item-created item)))
    (should (null            (agent-shell-queue-item-dispatched item)))
    (should (null            (agent-shell-queue-item-completed item)))))

(ert-deftest agent-shell-queue-org/deserialize/status-from-keyword ()
  "When :STATUS: property is absent, the TODO keyword is used.
The #+TODO: header must be present so org-mode recognises custom keywords."
  (let* ((result (agent-shell-queue-org--deserialize
                  (concat "#+TODO: TODO DOING WAIT HOLD DRAFT | DONE\n"
                          "* b\n\n"
                          "** DOING task\n"
                          "   :PROPERTIES:\n   :QUEUE-ID: q1\n   :END:\n\n"
                          "   task\n")))
         (item (car (cdr (car result)))))
    (should (eq 'running (agent-shell-queue-item-status item)))))

(ert-deftest agent-shell-queue-org/deserialize/multiple-buckets ()
  "Multiple level-1 headings produce multiple buckets."
  (let* ((result (agent-shell-queue-org--deserialize
                  (concat "* bucket-a\n\n"
                          "** TODO first\n   :PROPERTIES:\n   :QUEUE-ID: qa\n   :END:\n\n   first\n\n"
                          "* bucket-b\n\n"
                          "** TODO second\n   :PROPERTIES:\n   :QUEUE-ID: qb\n   :END:\n\n   second\n"))))
    (should (= 2 (length result)))
    (should (equal "bucket-a" (car (nth 0 result))))
    (should (equal "bucket-b" (car (nth 1 result))))
    (should (equal "qa" (agent-shell-queue-item-id (car (cdr (nth 0 result))))))
    (should (equal "qb" (agent-shell-queue-item-id (car (cdr (nth 1 result))))))))

(ert-deftest agent-shell-queue-org/deserialize/multiple-items-same-bucket ()
  "Multiple level-2 headings under one bucket are all recovered."
  (let* ((result (agent-shell-queue-org--deserialize
                  (concat "* buf\n\n"
                          "** TODO first\n   :PROPERTIES:\n   :QUEUE-ID: q1\n   :END:\n\n   first\n\n"
                          "** TODO second\n   :PROPERTIES:\n   :QUEUE-ID: q2\n   :END:\n\n   second\n")))
         (items (cdr (car result))))
    (should (= 2 (length items)))
    (should (equal "q1" (agent-shell-queue-item-id (nth 0 items))))
    (should (equal "q2" (agent-shell-queue-item-id (nth 1 items))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Round-trip tests — serialize then deserialize

(defmacro agent-shell-queue-org-test/round-trip (items-alist &rest checks)
  "Serialize ITEMS-ALIST, deserialize the result, bind to `rt', run CHECKS."
  (declare (indent 1))
  `(let* ((agent-shell-queue--items ,items-alist)
          (rt (agent-shell-queue-org--deserialize
               (agent-shell-queue-org--serialize))))
     ,@checks))

(ert-deftest agent-shell-queue-org/round-trip/single-item ()
  (agent-shell-queue-org-test/round-trip
      (list (cons "*test-buf*"
                  (list (agent-shell-queue-org-test/make-item
                         "q3abc" "do the thing" 'active nil 'prompt 1705318200.5))))
    (let ((item (car (cdr (car rt)))))
      (should (equal "*test-buf*"   (caar rt)))
      (should (equal "q3abc"        (agent-shell-queue-item-id item)))
      (should (equal "do the thing" (agent-shell-queue-item-prompt item)))
      (should (eq 'active           (agent-shell-queue-item-status item)))
      (should (eq nil               (agent-shell-queue-item-background item)))
      (should (= 1705318200.5       (agent-shell-queue-item-created item))))))

(ert-deftest agent-shell-queue-org/round-trip/multiline-prompt ()
  (agent-shell-queue-org-test/round-trip
      (list (cons "*buf*"
                  (list (agent-shell-queue-org-test/make-item
                         "q1" "line one\nline two\nline three"))))
    (let ((item (car (cdr (car rt)))))
      (should (equal "line one\nline two\nline three"
                     (agent-shell-queue-item-prompt item))))))

(ert-deftest agent-shell-queue-org/round-trip/background-flag ()
  (agent-shell-queue-org-test/round-trip
      (list (cons "*buf*"
                  (list (agent-shell-queue-org-test/make-item "q1" "p" 'active t))))
    (let ((item (car (cdr (car rt)))))
      (should (eq t (agent-shell-queue-item-background item))))))

(ert-deftest agent-shell-queue-org/round-trip/all-statuses ()
  "Each status symbol survives a round-trip."
  (dolist (status '(active running paused blocked draft done))
    (let* ((agent-shell-queue--items
            (list (cons "*b*"
                        (list (agent-shell-queue-org-test/make-item "q1" "p" status)))))
           (rt (agent-shell-queue-org--deserialize
                (agent-shell-queue-org--serialize)))
           (item (car (cdr (car rt)))))
      (should (eq status (agent-shell-queue-item-status item))))))

(ert-deftest agent-shell-queue-org/round-trip/timestamps ()
  "created, dispatched, and completed timestamps survive."
  (agent-shell-queue-org-test/round-trip
      (list (cons "*buf*"
                  (list (agent-shell-queue-org-test/make-item
                         "q1" "p" 'done nil 'prompt 1000.5 2000.0 3000.25))))
    (let ((item (car (cdr (car rt)))))
      (should (= 1000.5  (agent-shell-queue-item-created item)))
      (should (= 2000.0  (agent-shell-queue-item-dispatched item)))
      (should (= 3000.25 (agent-shell-queue-item-completed item))))))

(ert-deftest agent-shell-queue-org/round-trip/nil-timestamps ()
  "nil timestamps deserialize back to nil."
  (agent-shell-queue-org-test/round-trip
      (list (cons "*buf*"
                  (list (agent-shell-queue-org-test/make-item "q1" "p"))))
    (let ((item (car (cdr (car rt)))))
      (should (null (agent-shell-queue-item-dispatched item)))
      (should (null (agent-shell-queue-item-completed item))))))

(ert-deftest agent-shell-queue-org/round-trip/multiple-buckets ()
  "Multiple buckets including unassigned survive a round-trip."
  (agent-shell-queue-org-test/round-trip
      (list (cons "*buf-a*"
                  (list (agent-shell-queue-org-test/make-item "qa" "task a")))
            (cons "(unassigned)"
                  (list (agent-shell-queue-org-test/make-item "qu" "task u"))))
    (should (= 2 (length rt)))
    (should (equal "*buf-a*"     (car (nth 0 rt))))
    (should (equal "(unassigned)" (car (nth 1 rt))))
    (should (equal "qa" (agent-shell-queue-item-id (car (cdr (nth 0 rt))))))
    (should (equal "qu" (agent-shell-queue-item-id (car (cdr (nth 1 rt))))))))

(ert-deftest agent-shell-queue-org/round-trip/prompt-with-org-syntax ()
  "Prompt containing org structural text survives unchanged."
  (let ((tricky "** not a heading\n   :PROPERTIES: fake\n:END: nope\n* also not"))
    (agent-shell-queue-org-test/round-trip
        (list (cons "*buf*"
                    (list (agent-shell-queue-org-test/make-item "q1" tricky))))
      (let ((item (car (cdr (car rt)))))
        (should (equal tricky (agent-shell-queue-item-prompt item)))))))

(ert-deftest agent-shell-queue-org/round-trip/multiple-items-ordered ()
  "Item order is preserved across a round-trip."
  (agent-shell-queue-org-test/round-trip
      (list (cons "*buf*"
                  (list (agent-shell-queue-org-test/make-item "q1" "first")
                        (agent-shell-queue-org-test/make-item "q2" "second")
                        (agent-shell-queue-org-test/make-item "q3" "third"))))
    (let ((items (cdr (car rt))))
      (should (= 3 (length items)))
      (should (equal "q1" (agent-shell-queue-item-id (nth 0 items))))
      (should (equal "q2" (agent-shell-queue-item-id (nth 1 items))))
      (should (equal "q3" (agent-shell-queue-item-id (nth 2 items)))))))

(ert-deftest agent-shell-queue-org/round-trip/all-kind-values ()
  "All kind symbols survive a round-trip."
  (dolist (kind '(prompt pause compact emacs))
    (let* ((agent-shell-queue--items
            (list (cons "*b*"
                        (list (agent-shell-queue-org-test/make-item "q1" "p" 'active nil kind)))))
           (rt (agent-shell-queue-org--deserialize
                (agent-shell-queue-org--serialize)))
           (item (car (cdr (car rt)))))
      (should (eq kind (agent-shell-queue-item-kind item))))))

(provide 'test-agent-shell-queue-org)
;;; test-agent-shell-queue-org.el ends here
