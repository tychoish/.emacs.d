;;; bootstrap-fill-test.el --- Markdown glue for orgx-fill -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the markdown glue in `bootstrap.el' registered with `orgx-fill'.

;;; Code:

(require 'ert)
(require 'markdown-mode)
(require 'orgx-fill)
(require 'bootstrap)

(defconst bootstrap-fill-test--long "foo bar baz qux quux corge grault garply waldo")

(defmacro bootstrap-fill-test--with-glue (&rest body)
  "Run BODY with the markdown glue registered with orgx-fill."
  (declare (indent 0))
  `(let ((orgx-fill-suppress-functions (list #'bootstrap-markdown-fill-suppressed-p))
         (orgx-fill-handlers (list #'bootstrap-markdown-fill-folded-scalar)))
     ,@body))

(ert-deftest bootstrap-fill-markdown-frontmatter ()
  (bootstrap-fill-test--with-glue
    (with-temp-buffer
      (markdown-mode)
      (insert "---\ntitle: " bootstrap-fill-test--long "\ndesc: >\n  "
              bootstrap-fill-test--long "\nlit: |\n  " bootstrap-fill-test--long "\n---\n")
      (should (orgx-fill-buffer 30))
      (should (string-match-p (concat "^title: " bootstrap-fill-test--long "$") (buffer-string)))
      (should (string-match-p "^desc: >\n  foo bar baz qux quux corge\n  grault" (buffer-string)))
      (should (string-match-p (concat "^  " bootstrap-fill-test--long "\n---") (buffer-string))))))

(ert-deftest bootstrap-fill-markdown-heading-and-code-unchanged ()
  (bootstrap-fill-test--with-glue
    (with-temp-buffer
      (markdown-mode)
      (insert "# " bootstrap-fill-test--long "\n\n```\n" bootstrap-fill-test--long "\n```\n\n"
              bootstrap-fill-test--long "\n")
      (should (orgx-fill-buffer 30))
      (should (string-match-p (concat "^# " bootstrap-fill-test--long "$") (buffer-string)))
      (should (string-match-p (concat "^" bootstrap-fill-test--long "\n```") (buffer-string)))
      (should (string-match-p "^foo bar baz qux quux corge\ngrault" (buffer-string))))))

(provide 'bootstrap-fill-test)
;;; bootstrap-fill-test.el ends here
