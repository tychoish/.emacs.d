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

(defconst test-denote-notion--untracked-with-properties-fixture
  "---
title:      \"Untracked Note\"
date:       2026-01-05T17:52:00-05:00
tags:       [\"casap\"]
identifier: \"20260105T175201\"
signature:  \"2a8b\"
notion_properties: {\"Timestamp\": {\"date\": {\"start\": \"2020-01-01\"}}}
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-notion--parse-parent-arg

(ert-deftest test-denote-notion/parse-parent-arg-roundtrips ()
  "Parsing the output of `denote-notion--parent-arg' recovers the original cons."
  (should (equal (denote-notion--parse-parent-arg
                  (denote-notion--parent-arg '(database . "abc-123")))
                 '(database . "abc-123"))))

(ert-deftest test-denote-notion/parse-parent-arg-nil-for-empty ()
  "Returns nil for a nil or empty string, rather than erroring."
  (should-not (denote-notion--parse-parent-arg nil))
  (should-not (denote-notion--parse-parent-arg "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-notion--export-create
;;
;; `ntn pages create --json' returns the created page object directly at
;; its top level -- unlike `ntn pages get --json', which wraps it under a
;; `page' key alongside the converted markdown.  These fixtures are flat
;; for that reason.

(defconst test-denote-notion--create-response-json
  "{\"id\": \"new-page-id\", \"url\": \"https://app.notion.com/p/new\", \"created_time\": \"2026-03-01T00:00:00.000Z\", \"last_edited_time\": \"2026-03-01T00:00:00.000Z\", \"properties\": {}}")

(ert-deftest test-denote-notion/export-create-reads-flat-response ()
  "export-create reads id/url/timestamps from a flat (unwrapped) response
and writes them correctly, and does not write `source_url'."
  (test-denote-notion--with-fixture test-denote-notion--untracked-fixture
    (cl-letf (((symbol-function 'denote-notion--run-json)
               (lambda (&rest _)
                 (json-parse-string test-denote-notion--create-response-json
                                    :object-type 'alist :array-type 'list))))
      (denote-notion--export-create file '(database . "db-id")))
    (should (equal (denote-notion--frontmatter-get file "notion_id") "\"new-page-id\""))
    (should (equal (denote-notion--frontmatter-get file "notion_created") "\"2026-03-01T00:00:00.000Z\""))
    (should (equal (denote-notion--frontmatter-get file "notion_edited") "\"2026-03-01T00:00:00.000Z\""))
    (should-not (denote-notion--frontmatter-get file "source_url"))))

(ert-deftest test-denote-notion/export-create-parent-stores-locator-not-registry-name ()
  "notion_parent stores the parent's type:id locator, not a registry
entry's display name -- so renaming or removing that registry entry
later can't bitrot an already-created note's record of its own parent."
  (test-denote-notion--with-fixture test-denote-notion--untracked-fixture
    (let ((denote-notion-parent-registry '(("My Database" . ((database . "db-id") . nil)))))
      (cl-letf (((symbol-function 'denote-notion--run-json)
                 (lambda (&rest _)
                   (json-parse-string test-denote-notion--create-response-json
                                      :object-type 'alist :array-type 'list))))
        (denote-notion--export-create file '(database . "db-id"))))
    (should (equal (denote-notion--frontmatter-get file "notion_parent") "\"database:db-id\""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-notion--export-update
;;
;; `ntn pages edit --json' returns only a minimal confirmation object (no
;; `url'/`properties'/`last_edited_time') -- unlike `pages create', it does
;; NOT mirror the full page object, so `--export-update' re-fetches via
;; `pages get' (wrapped under a `page' key, like every other `pages get'
;; response) for everything past the content edit itself.

(defconst test-denote-notion--edit-response-json
  "{\"id\": \"tracked-id\", \"markdown\": \"Body content here.\", \"object\": \"page_markdown\", \"request_id\": \"req-1\", \"truncated\": false, \"unknown_block_ids\": []}")

(defconst test-denote-notion--get-response-json
  "{\"markdown\": {\"markdown\": \"Body content here.\"}, \"page\": {\"id\": \"tracked-id\", \"url\": \"https://app.notion.com/p/tracked\", \"last_edited_time\": \"2026-04-01T00:00:00.000Z\", \"properties\": {}}}")

(ert-deftest test-denote-notion/export-update-reads-last-edited-from-get-not-edit ()
  "export-update reads the updated `last_edited_time' from the post-edit
`pages get' response, not from `pages edit's own (minimal) response --
the latter carries no such field.  FORCE skips the conflict check, so no
pre-edit `pages get' call happens; only the edit and post-edit get calls
need stubbing here."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (cl-letf (((symbol-function 'denote-notion--run-json)
               (lambda (args)
                 (cond
                  ((member "edit" args)
                   (json-parse-string test-denote-notion--edit-response-json
                                      :object-type 'alist :array-type 'list))
                  ((member "get" args)
                   (json-parse-string test-denote-notion--get-response-json
                                      :object-type 'alist :array-type 'list))
                  (t (error "unexpected call in force path: %S" args))))))
      (should (equal (denote-notion--export-update file t)
                     "https://app.notion.com/p/tracked")))
    (should (equal (denote-notion--frontmatter-get file "notion_edited")
                   "\"2026-04-01T00:00:00.000Z\""))))

(ert-deftest test-denote-notion/export-update-resolves-properties-by-parent-locator-not-name ()
  "Default properties are found by looking the stored `notion_parent'
locator (type:id) back up in the registry, so they still resolve even
after the registry entry's display name has been renamed."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (denote-notion--frontmatter-set file "notion_parent" "database:db-id")
    (let ((denote-notion-parent-registry
           '(("Renamed Database" . ((database . "db-id") . ((Status . "Synced")))))))
      (cl-letf (((symbol-function 'denote-notion--run-json)
                 (lambda (args)
                   (cond
                    ((member "edit" args)
                     (json-parse-string test-denote-notion--edit-response-json
                                        :object-type 'alist :array-type 'list))
                    ((member "get" args)
                     (json-parse-string test-denote-notion--get-response-json
                                        :object-type 'alist :array-type 'list))
                    ((member "api" args) nil)
                    (t (error "unexpected call: %S" args))))))
        (denote-notion--export-update file t)))
    (should (equal (denote-notion--frontmatter-get file "notion_edited")
                   "\"2026-04-01T00:00:00.000Z\""))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-notion--resolve-property-sentinels

(ert-deftest test-denote-notion/resolve-property-sentinels-replaces-today ()
  "The \"<today>\" sentinel is replaced by today's date, nested anywhere in
the property-value structure."
  (cl-letf (((symbol-function 'format-time-string) (lambda (&rest _) "2026-07-16")))
    (should (equal (denote-notion--resolve-property-sentinels
                    '((Timestamp (date (start . "<today>")))))
                   '((Timestamp (date (start . "2026-07-16"))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-notion--export-create: built-in Timestamp default
;;
;; A newly created page's schema often expects a `Timestamp' date property
;; to be populated even when no registry entry or per-file
;; `notion_properties' sets it explicitly -- see
;; `denote-notion--default-export-properties'.

(ert-deftest test-denote-notion/export-create-applies-default-timestamp ()
  "Timestamp is populated with today's date on creation with no override."
  (test-denote-notion--with-fixture test-denote-notion--untracked-fixture
    (let (patched-data)
      (cl-letf (((symbol-function 'format-time-string) (lambda (&rest _) "2026-07-16"))
                ((symbol-function 'denote-notion--run-json)
                 (lambda (args)
                   (cond
                    ((member "create" args)
                     (json-parse-string test-denote-notion--create-response-json
                                        :object-type 'alist :array-type 'list))
                    ((member "api" args)
                     (setq patched-data (nth (1+ (seq-position args "--data")) args))
                     nil)
                    (t (error "unexpected call: %S" args))))))
        (denote-notion--export-create file '(database . "db-id")))
      (should patched-data)
      (should (string-match-p "\"Timestamp\"" patched-data))
      (should (string-match-p "2026-07-16" patched-data))
      (should-not (string-match-p "<today>" patched-data)))))

(ert-deftest test-denote-notion/export-create-file-properties-override-default-timestamp ()
  "A file's own `notion_properties' Timestamp value takes precedence over
the built-in default."
  (test-denote-notion--with-fixture test-denote-notion--untracked-with-properties-fixture
    (let (patched-data)
      (cl-letf (((symbol-function 'denote-notion--run-json)
                 (lambda (args)
                   (cond
                    ((member "create" args)
                     (json-parse-string test-denote-notion--create-response-json
                                        :object-type 'alist :array-type 'list))
                    ((member "api" args)
                     (setq patched-data (nth (1+ (seq-position args "--data")) args))
                     nil)
                    (t (error "unexpected call: %S" args))))))
        (denote-notion--export-create file '(database . "db-id")))
      (should (string-match-p "2020-01-01" patched-data))
      (should-not (string-match-p "<today>" patched-data)))))

(ert-deftest test-denote-notion/export-update-does-not-apply-default-timestamp ()
  "Update does not force the built-in Timestamp default on every edit --
only creation guarantees a value; an update with no properties configured
sends no PATCH at all."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (cl-letf (((symbol-function 'denote-notion--run-json)
               (lambda (args)
                 (cond
                  ((member "edit" args)
                   (json-parse-string test-denote-notion--edit-response-json
                                      :object-type 'alist :array-type 'list))
                  ((member "get" args)
                   (json-parse-string test-denote-notion--get-response-json
                                      :object-type 'alist :array-type 'list))
                  ((member "api" args)
                   (error "unexpected properties PATCH on update with no configured properties"))
                  (t (error "unexpected call: %S" args))))))
      (denote-notion--export-update file t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-notion--clean-imported-body

(ert-deftest test-denote-notion/clean-imported-body-br-to-newline ()
  "A literal <br> tag (Notion's soft-line-break artifact) becomes a real newline."
  (should (equal (denote-notion--clean-imported-body "one<br>two")
                 "one\ntwo")))

(ert-deftest test-denote-notion/clean-imported-body-br-variants ()
  "Self-closing and spaced <br/> / <br /> variants are also converted."
  (should (equal (denote-notion--clean-imported-body "one<br/>two<br />three")
                 "one\ntwo\nthree")))

(ert-deftest test-denote-notion/clean-imported-body-unescapes-brackets ()
  "Backslash-escaped brackets (CommonMark link-syntax escaping) are unescaped,
so imported prose like a bracketed label reads naturally instead of
carrying literal backslashes."
  (should (equal (denote-notion--clean-imported-body "- \\[design\\] a question")
                 "- [design] a question")))

(ert-deftest test-denote-notion/clean-imported-body-widens-block-newlines-to-paragraphs ()
  "Each single newline joining separate Notion blocks becomes a blank line,
so Markdown renders them as separate paragraphs instead of one run-on
paragraph."
  (should (equal (denote-notion--clean-imported-body "## Heading\nFirst paragraph.\nSecond paragraph.")
                 "## Heading\n\nFirst paragraph.\n\nSecond paragraph.")))

(ert-deftest test-denote-notion/clean-imported-body-soft-break-stays-single-newline ()
  "A <br> soft break within one block becomes a single newline, not a
paragraph break, even after block-level newlines are widened to blank
lines."
  (should (equal (denote-notion--clean-imported-body "One line<br>continues.\nNext paragraph.")
                 "One line\ncontinues.\n\nNext paragraph.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-notion--rich-text-plain

(ert-deftest test-denote-notion/rich-text-plain-single-segment ()
  "A single rich_text segment's plain_text is returned as-is."
  (should (equal (denote-notion--rich-text-plain '(((plain_text . "Hello"))))
                 "Hello")))

(ert-deftest test-denote-notion/rich-text-plain-concatenates-segments ()
  "Multiple rich_text segments (e.g. mixed formatting runs within one
title) are concatenated into a single string."
  (should (equal (denote-notion--rich-text-plain
                  '(((plain_text . "Hello, "))
                    ((plain_text . "world"))))
                 "Hello, world")))

(ert-deftest test-denote-notion/rich-text-plain-empty-array ()
  "An empty rich_text array (e.g. an untitled page) returns the empty string."
  (should (equal (denote-notion--rich-text-plain nil) "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-notion-pull: create-new-note path
;;
;; `properties.Name.title' is a Notion rich_text array, not a plain
;; string, so this exercises the whole create path end to end rather
;; than just the extraction helper in isolation.

(defconst test-denote-notion--get-page-response-json
  "{\"markdown\": {\"markdown\": \"one<br>two \\\\[design\\\\] done\"}, \"page\": {\"id\": \"page-id\", \"url\": \"https://app.notion.com/p/x\", \"created_time\": \"2020-05-04T10:00:00.000Z\", \"last_edited_time\": \"2020-05-04T11:00:00.000Z\", \"properties\": {\"Name\": {\"type\": \"title\", \"title\": [{\"plain_text\": \"Imported Title\"}]}, \"Tags\": {\"type\": \"multi_select\", \"multi_select\": [{\"name\": \"engineering\"}]}}}}")

(ert-deftest test-denote-notion/import-page-creates-note-with-correct-title-and-date ()
  "Importing a new page extracts the plain-text title (not the raw
rich_text array), backdates the note's identifier/date to the page's
`created_time' instead of \"now\", and cleans up the body."
  (let ((dir (make-temp-file "denote-notion-test-" t)))
    (unwind-protect
        (let ((denote-directory (list dir)))
          (cl-letf (((symbol-function 'denote-notion--run-json)
                     (lambda (&rest _)
                       (json-parse-string test-denote-notion--get-page-response-json
                                          :object-type 'alist :array-type 'list))))
            (denote-notion-pull "page-id"))
          (let ((file (car (directory-files dir t "imported-title"))))
            (should file)
            (should (string-suffix-p ".md" file))
            (should (string-prefix-p
                     (format-time-string "%Y%m%d" (date-to-time "2020-05-04T10:00:00.000Z"))
                     (file-name-nondirectory file)))
            (should (equal (denote-notion--frontmatter-get file "notion_id") "\"page-id\""))
            (with-temp-buffer
              (insert-file-contents file)
              (should (string-match-p "one\ntwo" (buffer-string)))
              (should (string-match-p "\\[design\\]" (buffer-string)))
              (should-not (string-match-p "<br>" (buffer-string))))))
      (dolist (buf (buffer-list))
        (when-let* ((f (buffer-file-name buf)))
          (when (string-prefix-p (expand-file-name dir) (expand-file-name f))
            (with-current-buffer buf (set-buffer-modified-p nil))
            (kill-buffer buf))))
      (delete-directory dir t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-notion-pull: dwim refresh with no page id

(ert-deftest test-denote-notion/import-page-nil-id-refreshes-tracked-target-file ()
  "With no PAGE-ID, TARGET-FILE's own tracked notion_id is re-pulled."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (cl-letf (((symbol-function 'denote-notion--run-json)
               (lambda (&rest _)
                 (json-parse-string test-denote-notion--get-page-response-json
                                    :object-type 'alist :array-type 'list))))
      (denote-notion-pull nil file))
    (should (equal (denote-notion--frontmatter-get file "notion_edited")
                   "\"2020-05-04T11:00:00.000Z\""))
    (with-temp-buffer
      (insert-file-contents file)
      (should (string-match-p "one\ntwo" (buffer-string))))))

(ert-deftest test-denote-notion/import-page-nil-id-errors-when-untracked ()
  "With no PAGE-ID and an untracked target, errors instead of silently
doing nothing or trying to guess a page to pull."
  (test-denote-notion--with-fixture test-denote-notion--untracked-fixture
    (should-error (denote-notion-pull nil file) :type 'user-error)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-notion--import-refresh-file: metadata sync

(ert-deftest test-denote-notion/import-refresh-syncs-notion-tags ()
  "A refresh updates `notion_tags' from the page's current Tags property,
not just the body and `notion_edited' -- the fixture starts with two
stale tags; the fetched page now has only one."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (should (equal (denote-notion--frontmatter-get file "notion_tags")
                   "[\"engineering\", \"observation\"]"))
    (cl-letf (((symbol-function 'denote-notion--run-json)
               (lambda (&rest _)
                 (json-parse-string test-denote-notion--get-page-response-json
                                    :object-type 'alist :array-type 'list))))
      (denote-notion-pull nil file))
    (should (equal (denote-notion--frontmatter-get file "notion_tags")
                   "[\"engineering\"]"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-notion--find-tracked-file / directory-wide dedup on import

(ert-deftest test-denote-notion/find-tracked-file-locates-match-anywhere ()
  "Finds the tracking file by notion_id across the whole denote directory,
not just the current buffer."
  (let ((dir (make-temp-file "denote-notion-test-" t)))
    (unwind-protect
        (let ((denote-directory (list dir)))
          (let ((f1 (expand-file-name "20260101T000000--one__tag.md" dir))
                (f2 (expand-file-name "20260101T000001--two__tag.md" dir)))
            (with-temp-file f1 (insert "---\ntitle: \"One\"\nidentifier: \"20260101T000000\"\nnotion_id: \"other-page\"\n---\n\nbody\n"))
            (with-temp-file f2 (insert "---\ntitle: \"Two\"\nidentifier: \"20260101T000001\"\nnotion_id: \"page-id\"\n---\n\nbody\n"))
            (should (equal (denote-notion--find-tracked-file "page-id") f2))
            (should-not (denote-notion--find-tracked-file "no-such-id"))))
      (delete-directory dir t))))

(ert-deftest test-denote-notion/import-page-refreshes-existing-tracked-file-instead-of-duplicating ()
  "Importing a page id already tracked by some other note (not the
current buffer, and not an explicit TARGET-FILE) refreshes that note
instead of creating a duplicate."
  (let ((dir (make-temp-file "denote-notion-test-" t)))
    (unwind-protect
        (let ((denote-directory (list dir))
              (tracked (expand-file-name "20260101T000000--already-tracked__tag.md" dir)))
          (with-temp-file tracked
            (insert "---\ntitle:      \"Already Tracked\"\nidentifier: \"20260101T000000\"\nnotion_id:  \"page-id\"\nnotion_edited: \"2019-01-01T00:00:00.000Z\"\n---\n\nold body\n"))
          (cl-letf (((symbol-function 'denote-notion--run-json)
                     (lambda (&rest _)
                       (json-parse-string test-denote-notion--get-page-response-json
                                          :object-type 'alist :array-type 'list))))
            (denote-notion-pull "page-id"))
          (should (equal (length (directory-files dir nil "\\.md\\'")) 1))
          (should (equal (denote-notion--frontmatter-get tracked "notion_edited")
                         "\"2020-05-04T11:00:00.000Z\"")))
      (dolist (buf (buffer-list))
        (when-let* ((f (buffer-file-name buf)))
          (when (string-prefix-p (expand-file-name dir) (expand-file-name f))
            (with-current-buffer buf (set-buffer-modified-p nil))
            (kill-buffer buf))))
      (delete-directory dir t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-notion--file-at-point

(ert-deftest test-denote-notion/file-at-point-prefers-denote-dash-when-loaded ()
  "Resolves via `denote-dash--file-at-point' when `denote-dash' is loaded,
so a note merely selected at point in a `denote-dash' or
sequence-hierarchy listing (which has no `buffer-file-name' of its own)
still resolves to the right file."
  (cl-letf (((symbol-function 'denote-dash--file-at-point) (lambda () "/from/denote-dash.md")))
    (should (equal (denote-notion--file-at-point) "/from/denote-dash.md"))))

(ert-deftest test-denote-notion/file-at-point-falls-back-to-buffer-file-name ()
  "Falls back to `buffer-file-name' when `denote-dash' isn't loaded."
  (should-not (fboundp 'denote-dash--file-at-point))
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (with-current-buffer (find-file-noselect file)
      (should (equal (denote-notion--file-at-point) file)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-notion--rich-text-value

(ert-deftest test-denote-notion/rich-text-value-shape ()
  "Produces a Notion rich_text array (a vector of one text-segment alist)."
  (should (equal (denote-notion--rich-text-value "Hello")
                 [((text . ((content . "Hello"))))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-notion--set-page-title

(ert-deftest test-denote-notion/set-page-title-patches-discovered-title-key ()
  "Finds the property whose type is `title' (whatever it's named) and
PATCHes it, rather than assuming a fixed key like \"Name\"."
  (let (patched-data)
    (cl-letf (((symbol-function 'denote-notion--run-json)
               (lambda (args)
                 (setq patched-data (nth (1+ (seq-position args "--data")) args))
                 nil)))
      (denote-notion--set-page-title
       "page-id"
       '((Created . ((type . "created_time")))
         (Name . ((type . "title"))))
       "My Title"))
    (should patched-data)
    (should (string-match-p "\"Name\"" patched-data))
    (should (string-match-p "My Title" patched-data))))

(ert-deftest test-denote-notion/set-page-title-noop-without-title-property ()
  "Does nothing (no PATCH call) when PROPERTIES has no `title'-typed entry."
  (cl-letf (((symbol-function 'denote-notion--run-json)
             (lambda (&rest args) (error "unexpected call: %S" args))))
    (denote-notion--set-page-title "page-id" '((Created . ((type . "created_time")))) "My Title")))

(ert-deftest test-denote-notion/set-page-title-noop-for-nil-or-empty-title ()
  "Does nothing when TITLE is nil or empty, even with a title property present."
  (cl-letf (((symbol-function 'denote-notion--run-json)
             (lambda (&rest args) (error "unexpected call: %S" args))))
    (denote-notion--set-page-title "page-id" '((Name . ((type . "title")))) nil)
    (denote-notion--set-page-title "page-id" '((Name . ((type . "title")))) "")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; denote-notion--export-create / --export-update: no title heading in
;; the exported content

(ert-deftest test-denote-notion/export-create-does-not-embed-title-in-content ()
  "The content sent to `ntn pages create' is the plain body -- no leading
H1 duplicating the page's own title."
  (test-denote-notion--with-fixture test-denote-notion--untracked-fixture
    (let (create-content)
      (cl-letf (((symbol-function 'denote-notion--run-json)
                 (lambda (args)
                   (when (member "create" args)
                     (setq create-content (nth (1+ (seq-position args "--content")) args)))
                   (json-parse-string test-denote-notion--create-response-json
                                      :object-type 'alist :array-type 'list))))
        (denote-notion--export-create file '(database . "db-id")))
      (should (equal create-content "## Body\n\nUntracked body.")))))

(ert-deftest test-denote-notion/export-update-does-not-embed-title-in-content ()
  "The content sent to `ntn pages edit' is the plain body -- no leading H1."
  (test-denote-notion--with-fixture test-denote-notion--md-fixture
    (let (edit-content)
      (cl-letf (((symbol-function 'denote-notion--run-json)
                 (lambda (args)
                   (if (member "edit" args)
                       (progn
                         (setq edit-content (nth (1+ (seq-position args "--content")) args))
                         (json-parse-string test-denote-notion--edit-response-json
                                            :object-type 'alist :array-type 'list))
                     nil))))
        (denote-notion--export-update file t))
      (should (equal edit-content "## Body\n\nBody content here.")))))

(provide 'test-denote-notion)
;;; test-denote-notion.el ends here
