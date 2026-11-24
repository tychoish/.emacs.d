;;; bootstrap-fill-test.el --- fill-paragraph skips preformatted blocks -*- lexical-binding: t; -*-

;;; Commentary:
;; Regression tests for `bootstrap-in-preformatted-block-p'.

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

(provide 'bootstrap-fill-test)
;;; bootstrap-fill-test.el ends here
