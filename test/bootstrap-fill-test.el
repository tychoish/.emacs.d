;;; bootstrap-fill-test.el --- fill-paragraph skips preformatted blocks -*- lexical-binding: t; -*-

;;; Commentary:
;; Regression tests for `bootstrap-fill-suppressed-p'.

;;; Code:

(require 'ert)
(require 'org)
(require 'bootstrap)

(ert-deftest bootstrap-fill-org-src-block-unchanged ()
  (with-temp-buffer
    (org-mode)
    (insert "#+begin_src sh\nfoo bar baz qux quux corge grault garply waldo fred\n#+end_src\n")
    (let ((before (buffer-string))
          (fill-column 20))
      (goto-char 20)
      (fill-paragraph)
      (should (equal before (buffer-string))))))

(ert-deftest bootstrap-fill-org-prose-still-fills ()
  (with-temp-buffer
    (org-mode)
    (insert "foo bar baz qux quux corge grault garply waldo fred\n")
    (let ((fill-column 20))
      (goto-char 5)
      (fill-paragraph)
      (should (> (count-lines (point-min) (point-max)) 1)))))

(ert-deftest bootstrap-fill-org-heading-unchanged ()
  (with-temp-buffer
    (org-mode)
    (insert "* foo bar baz qux quux corge grault garply waldo fred\n")
    (let ((before (buffer-string))
          (fill-column 20))
      (goto-char 5)
      (fill-paragraph)
      (should (equal before (buffer-string))))))

(ert-deftest bootstrap-fill-org-keyword-and-drawer-unchanged ()
  (with-temp-buffer
    (org-mode)
    (insert "#+title: foo bar baz qux quux corge grault garply waldo\n"
            "* H\n:PROPERTIES:\n:FOO: bar baz qux quux corge grault garply waldo\n:END:\n")
    (let ((before (buffer-string))
          (fill-column 20))
      (goto-char 5)
      (fill-paragraph)
      (goto-char (point-max))
      (forward-line -2)
      (fill-paragraph)
      (should (equal before (buffer-string))))))

(ert-deftest bootstrap-fill-markdown-frontmatter ()
  (with-temp-buffer
    (markdown-mode)
    (insert "---\ntitle: foo bar baz qux quux corge grault garply\ndesc: >\n"
            "  foo bar baz qux quux corge grault garply waldo\nlit: |\n"
            "  foo bar baz qux quux corge grault garply waldo\n---\n")
    (let ((fill-column 30))
      (goto-char (point-min))
      (forward-line 1)
      (fill-paragraph)
      (forward-line 2)
      (fill-paragraph)
      (goto-char (point-min))
      (re-search-forward "^lit: |\n")
      (fill-paragraph))
    (should (string-match-p "^title: foo bar baz qux quux corge grault garply$"
                            (buffer-string)))
    (should (string-match-p "^desc: >\n  foo bar baz qux quux corge\n  grault" (buffer-string)))
    (should (string-match-p "^  foo bar baz qux quux corge grault garply waldo\n---" (buffer-string)))))

(provide 'bootstrap-fill-test)
;;; bootstrap-fill-test.el ends here
