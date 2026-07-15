;;; test-denote-notion.el --- ERT tests for denote-notion.el -*- lexical-binding: t -*-

;;; Commentary:
;; Tests for the pure/stubbable parts of denote-notion.el: front-matter
;; get/set, tracked-p, body extraction, and the ntn process wrapper (with
;; `call-process' stubbed out — no network).

;;; Code:

(require 'ert)
(require 'denote-notion)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fixtures

(defconst test-denote-notion--md-fixture
  "---
title:      \"Example Note\"
date:       2026-01-05T17:52:00-05:00
tags:       [\"casap\", \"notion\"]
identifier: \"20260105T175200\"
signature:  \"2a8a\"
notion_tags:    [\"engineering\", \"observation\"]
notion_id:      \"2f094bf7-31a4-8081-8280-f0a225af4db2\"
notion_created: \"2026-01-22T19:54:00.000Z\"
notion_edited:  \"2026-02-23T18:18:00.000Z\"
source_url:     \"https://app.notion.com/p/example\"
---

## Body

Body content here.
")

(defconst test-denote-notion--untracked-fixture
  "---
title:      \"Untracked Note\"
date:       2026-01-05T17:52:00-05:00
tags:       [\"casap\"]
identifier: \"20260105T175201\"
signature:  \"2a8b\"
---

## Body

Untracked body.
")

(defmacro test-denote-notion--with-fixture (content &rest body)
  "Write CONTENT to a temp file, bind it as `file', and run BODY."
  (declare (indent 1))
  `(let ((file (make-temp-file "denote-notion-test" nil ".md")))
     (unwind-protect
         (progn
           (with-temp-file file (insert ,content))
           ,@body)
       (delete-file file)
       (when-let* ((buf (get-file-buffer file)))
         (with-current-buffer buf (set-buffer-modified-p nil))
         (kill-buffer buf)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-notion--frontmatter-get

(ert-deftest test-denote-notion/frontmatter-get-existing-key ()
  "Reads an existing front-matter key's value verbatim."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (should (equal (denote-notion--frontmatter-get file "notion_id")
                   "\"2f094bf7-31a4-8081-8280-f0a225af4db2\""))))

(ert-deftest test-denote-notion/frontmatter-get-missing-key ()
  "Returns nil for a key that is not present."
  (test-denote-notion--with-fixture test-denote-notion--untracked-fixture
    (should (null (denote-notion--frontmatter-get file "notion_id")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-notion--frontmatter-set

(ert-deftest test-denote-notion/frontmatter-set-replaces-existing ()
  "Replaces an existing key's value in place."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (denote-notion--frontmatter-set file "notion_edited" "2026-03-01T00:00:00.000Z")
    (should (equal (denote-notion--frontmatter-get file "notion_edited")
                   "\"2026-03-01T00:00:00.000Z\""))))

(ert-deftest test-denote-notion/frontmatter-set-appends-missing ()
  "Appends a new key when it is not already present."
  (test-denote-notion--with-fixture test-denote-notion--untracked-fixture
    (denote-notion--frontmatter-set file "notion_id" "new-page-id")
    (should (equal (denote-notion--frontmatter-get file "notion_id") "\"new-page-id\""))))

(ert-deftest test-denote-notion/frontmatter-set-formats-list-as-array ()
  "A list value is written as a JSON-array-like bracketed, quoted list."
  (test-denote-notion--with-fixture test-denote-notion--untracked-fixture
    (denote-notion--frontmatter-set file "notion_tags" '("design" "architecture"))
    (should (equal (denote-notion--frontmatter-get file "notion_tags")
                   "[\"design\", \"architecture\"]"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-notion--tracked-p

(ert-deftest test-denote-notion/tracked-p-true ()
  "A note with a non-empty notion_id is tracked."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (should (denote-notion--tracked-p file))))

(ert-deftest test-denote-notion/tracked-p-false ()
  "A note with no notion_id is not tracked."
  (test-denote-notion--with-fixture test-denote-notion--untracked-fixture
    (should-not (denote-notion--tracked-p file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-notion--body-without-front-matter

(ert-deftest test-denote-notion/body-without-front-matter-md ()
  "Strips the YAML front matter block from a markdown note."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (should (equal (denote-notion--body-without-front-matter file)
                   "## Body\n\nBody content here."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-notion--extract-page-id

(ert-deftest test-denote-notion/extract-page-id-from-bare-id ()
  "A bare page id passes through unchanged."
  (should (equal (denote-notion--extract-page-id "2e994bf731a4800d99fbf40866cc0e65")
                 "2e994bf731a4800d99fbf40866cc0e65")))

(ert-deftest test-denote-notion/extract-page-id-from-url ()
  "The 32-char hex id is extracted from a trailing Notion URL segment."
  (should (equal (denote-notion--extract-page-id
                  "https://app.notion.com/p/Example-2e994bf731a4800d99fbf40866cc0e65")
                 "2e994bf731a4800d99fbf40866cc0e65")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-notion--run / denote-notion--run-json (call-process stubbed)

(ert-deftest test-denote-notion/run-returns-exit-code-and-output ()
  "Wraps `call-process' output as (EXIT-CODE . OUTPUT-STRING)."
  (cl-letf (((symbol-function 'call-process)
             (lambda (_program _infile buffer _display &rest _args)
               (with-current-buffer buffer (insert "hello"))
               0)))
    (should (equal (denote-notion--run '("whoami")) '(0 . "hello")))))

(ert-deftest test-denote-notion/run-json-parses-successful-output ()
  "Parses JSON output into an alist on a zero exit code."
  (cl-letf (((symbol-function 'call-process)
             (lambda (_program _infile buffer _display &rest _args)
               (with-current-buffer buffer (insert "{\"page\": {\"id\": \"abc\"}}"))
               0)))
    (should (equal (map-elt (map-elt (denote-notion--run-json '("pages" "get" "abc")) 'page) 'id)
                   "abc"))))

(ert-deftest test-denote-notion/run-json-signals-on-failure ()
  "A non-zero exit code raises a `user-error' carrying the CLI output."
  (cl-letf (((symbol-function 'call-process)
             (lambda (_program _infile buffer _display &rest _args)
               (with-current-buffer buffer (insert "not found"))
               1)))
    (should-error (denote-notion--run-json '("pages" "get" "missing")) :type 'user-error)))

(provide 'test-denote-notion)
;;; test-denote-notion.el ends here
