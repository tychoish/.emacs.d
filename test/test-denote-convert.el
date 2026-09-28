;;; test-denote-convert.el --- ERT tests for denote-convert.el -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; Unit tests for format conversion and Org datetree import workflows.

;;; Code:

(require 'ert)
(require 'denote-convert)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Datetree parsing tests

(ert-deftest denote-convert-test/datetree-day-date ()
  "Extracts YYYY-MM-DD from org datetree day headings."
  (should (equal "2024-03-15" (denote-convert--datetree-day-date "2024-03-15 Friday")))
  (should (equal "2024-12-01" (denote-convert--datetree-day-date "2024-12-01")))
  (should-not (denote-convert--datetree-day-date "No date here"))
  (should-not (denote-convert--datetree-day-date "2024-03 March")))

(ert-deftest denote-convert-test/entry-in-range-p ()
  "Tests whether a datetree entry date falls within optional date bounds."
  (let ((entry '(:date "2024-06-15" :title "Mid June")))
    (should (denote-convert--entry-in-range-p entry nil nil))
    (should (denote-convert--entry-in-range-p entry "2024-06-01" "2024-06-30"))
    (should (denote-convert--entry-in-range-p entry "2024-06-15" "2024-06-15"))
    (should-not (denote-convert--entry-in-range-p entry "2024-07-01" nil))
    (should-not (denote-convert--entry-in-range-p entry nil "2024-05-31"))))

(ert-deftest denote-convert-test/collect-datetree-entries ()
  "Scans an Org file buffer for entries under a datetree day heading."
  (let ((temp-org (make-temp-file "datetree-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file temp-org
            (insert "* 2024\n** 2024-03 March\n*** 2024-03-15 Friday\n**** First Entry :tag1:\nFirst body.\n**** Second Entry :tag2:\nSecond body.\n"))
          (with-current-buffer (find-file-noselect temp-org)
            (let ((entries (denote-convert--collect-datetree-entries)))
              (should (= 2 (length entries)))
              (should (equal "2024-03-15" (plist-get (car entries) :date)))
              (should (equal "First Entry" (plist-get (car entries) :title)))
              (should (equal '("tag1") (plist-get (car entries) :tags)))
              (should (equal "First body." (plist-get (car entries) :body)))
              (should (equal "Second Entry" (plist-get (cadr entries) :title)))))
          (kill-buffer (find-buffer-visiting temp-org)))
      (when (file-exists-p temp-org)
        (delete-file temp-org)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File type conversion tests

(ert-deftest denote-convert-test/file-type-org-to-markdown-yaml ()
  "Converts an Org note to Markdown YAML, firing the post-conversion hook."
  (let ((dir (make-temp-file "denote-convert-test-" t)))
    (unwind-protect
        (let* ((denote-directory dir)
               (org-file (denote "Test Conversion" '("project" "notes") 'org nil "2026-01-01" nil "1a"))
               hook-args)
          (let ((denote-convert-after-conversion-functions
                 (list (lambda (old-file new-file old-type new-type)
                         (setq hook-args (list old-file new-file old-type new-type))))))
            (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
              (denote-convert-file-type org-file 'markdown-yaml)))
          (should hook-args)
          (let ((new-file (nth 1 hook-args)))
            (should (file-exists-p new-file))
            (should-not (file-exists-p org-file))
            (should (string-suffix-p ".md" new-file))
            (with-temp-buffer
              (insert-file-contents new-file)
              (goto-char (point-min))
              (should (looking-at-p "---"))
              (should (re-search-forward "^title:.*Test Conversion" nil t))
              (should (re-search-forward "^signature:.*1a" nil t)))))
      (delete-directory dir t))))

(provide 'test-denote-convert)
;;; test-denote-convert.el ends here
