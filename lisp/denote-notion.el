;;; denote-notion.el --- Two-way sync between Denote notes and Notion pages -*- lexical-binding: t; -*-

;;; Commentary:
;; Push a Denote note to Notion as a page, or pull a Notion page back into
;; a Denote note, on demand via the `npx ntn` CLI.  Both directions are
;; manual/interactive.
;;
;; Entry points:
;;   `denote-notion-push' — create or update the Notion page for the
;;                          current note
;;   `denote-notion-pull' — pull a Notion page: with an id/URL, create or
;;                          refresh; with none, dwim-refresh the current
;;                          buffer's tracked page.

;;; Code:

(require 'seq)
(require 'map)
(require 'json)
(require 'cl-lib)
(require 'denote)

(declare-function org-export-to-buffer "ox")
(declare-function annotated-completing-read "annotated-completing-read")
(declare-function denote-dash--file-at-point "denote-dash")

(defun denote-notion--file-at-point ()
  "Return the denote file implied by the current point/buffer context, or nil.
Uses `denote-dash--file-at-point' when `denote-dash' is loaded, so a note
that's merely selected at point in a `denote-dash' or sequence-hierarchy
listing resolves correctly instead of requiring the file to be the
current buffer's own visited file.  Falls back to `buffer-file-name'."
  (if (fboundp 'denote-dash--file-at-point)
      (denote-dash--file-at-point)
    (buffer-file-name)))

;;; Custom variables

(defgroup denote-notion nil
  "Two-way sync between Denote notes and Notion pages."
  :group 'denote)

(defcustom denote-notion-default-parent nil
  "Default Notion parent for a first-time export, or nil to always prompt.
When set, a cons of (TYPE . ID): TYPE is one of the symbols `page',
`database', or `data-source'; ID is that resource's id string.  Takes
priorityh over `denote-notion-parent-registry' — set this only for a
single fixed target used non-interactively (e.g. from a headless script);
leave nil to pick per-export via the registry or a raw prompt."
  :type '(choice (const :tag "Always prompt" nil)
                  (cons (choice (const page) (const database) (const data-source))
                        string))
  :group 'denote-notion)

(defcustom denote-notion-parent-registry nil
  "Alist of (NAME . (PARENT . PROPERTIES)) named Notion export targets.
NAME is a short string shown in the `denote-notion--read-parent' ACR
menu.  PARENT is a (TYPE . ID) cons — TYPE one of the symbols `page',
`database', or `data-source'; ID that resource's id string.  PROPERTIES
is an alist of default Notion property values applied to every export
under this target — same shape as `denote-notion-export-properties',
e.g. `((Timestamp (date (start . \"<today>\")))) — or nil for none.  Set
per-machine, e.g. in casap.el for a personal workspace."
  :type '(alist :key-type string
                 :value-type (cons (cons (choice (const page) (const database) (const data-source))
                                          string)
                                    (alist :key-type symbol :value-type sexp)))
  :group 'denote-notion)

(defun denote-notion--parent-arg (parent)
  "Format PARENT, a (TYPE . ID) cons, as an `ntn --parent' string.
TYPE is one of the symbols `page', `database', or `data-source'."
  (unless (memq (car parent) '(page database data-source))
    (user-error "Unknown Notion parent type: %S (want page, database, or data-source)" (car parent)))
  (format "%s:%s" (car parent) (cdr parent)))

(defun denote-notion--registry-entry-for-parent (parent)
  "Return the `denote-notion-parent-registry' entry whose PARENT cons matches.
PARENT is a (TYPE . ID) cons.  Returns (NAME . (PARENT . PROPERTIES)), or nil."
  (cl-find-if (lambda (entry) (equal (car (cdr entry)) parent))
              denote-notion-parent-registry))

(defun denote-notion--acr-select-parent ()
  "Select a (TYPE . ID) parent from `denote-notion-parent-registry' via ACR."
  (require 'annotated-completing-read)
  (let* ((table (mapcar (lambda (entry)
                           (cons (car entry) (format "%s:%s" (car (car (cdr entry))) (cdr (car (cdr entry))))))
                         denote-notion-parent-registry))
         (name (annotated-completing-read
                table :prompt "Notion export target: " :require-match t
                :category 'denote-notion-parent)))
    (car (cdr (assoc name denote-notion-parent-registry)))))

;;; ntn process wrapper

(defun denote-notion--run (args)
  "Run \"npx ntn\" with ARGS and return (EXIT-CODE . OUTPUT-STRING)."
  (with-temp-buffer
    (let ((exit-code (apply #'call-process "npx" nil (current-buffer) nil "ntn" args)))
      (cons exit-code (buffer-string)))))

(defun denote-notion--run-json (args)
  "Run \"npx ntn\" with ARGS and return the parsed JSON result.
Appends \"--json\" unless ARGS invokes the `api' subcommand — `ntn api'
always emits JSON and has no `--json' flag at all (unlike `pages
get'/`create'/`edit'), so appending it there is an unrecognized
argument, not a no-op: `ntn api's variadic trailing [INPUT]... catch-all
means a bare trailing \"--json\" is rejected outright rather than
silently absorbed. Returns nil if the process exits non-zero. Signals
a `user-error' with the CLI's own output when the exit code is
non-zero."
  (let* ((args (if (equal (car args) "api") args (append args '("--json"))))
         (result (denote-notion--run args))
         (exit-code (car result))
         (output (cdr result)))
    (unless (zerop exit-code)
      (user-error "ntn %s failed: %s" (string-join args " ") output))
    (json-parse-string output :object-type 'alist :array-type 'list)))

;;; Front matter get/set

(defun denote-notion--frontmatter-line-regexp (key)
  "Return a regexp matching a front-matter KEY:VALUE line."
  (format "^%s:[ \t]*\\(.*\\)$" (regexp-quote key)))

(defun denote-notion--frontmatter-get (file key)
  "Return the string value of front-matter KEY in FILE, or nil if absent."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (when (re-search-forward (denote-notion--frontmatter-line-regexp key) nil t)
      (string-trim (match-string 1)))))

(defun denote-notion--frontmatter-format-value (value)
  "Format VALUE the way the existing Notion-tracked notes do.
A list of strings becomes a JSON-array-like [\"a\", \"b\"]; anything else
is written as a double-quoted string."
  (if (listp value)
      (concat "[" (string-join (seq-map (lambda (v) (format "%S" v)) value) ", ") "]")
    (format "%S" value)))

(defun denote-notion--frontmatter-set (file key value)
  "Set front-matter KEY to VALUE in FILE, replacing or appending the line.
VALUE is formatted with `denote-notion--frontmatter-format-value'.  FILE
must already be visited or is visited (and saved) as part of this call."
  (let ((line (format "%-15s %s" (concat key ":") (denote-notion--frontmatter-format-value value))))
    (with-current-buffer (find-file-noselect file)
      (goto-char (point-min))
      (if (re-search-forward (denote-notion--frontmatter-line-regexp key) nil t)
          (replace-match line)
        (goto-char (point-min))
        (forward-line 1)
        (insert line "\n"))
      (save-buffer))))

(defun denote-notion--tracked-p (file)
  "Return non-nil if FILE has a non-empty notion_id front-matter value."
  (let ((id (denote-notion--frontmatter-get file "notion_id")))
    (and id (not (string-empty-p id)) (not (string= id "\"\"")))))

;;; Body extraction and conversion

(defun denote-notion--body-without-front-matter (file)
  "Return FILE's content with its Denote front matter block stripped."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (cond
     ((looking-at "^---[ \t]*$")
      (forward-line 1)
      (re-search-forward "^---[ \t]*$" nil t)
      (forward-line 1))
     (t
      (while (looking-at "^#\\+")
        (forward-line 1))))
    (skip-chars-forward "\n")
    (string-trim (buffer-substring-no-properties (point) (point-max)))))

(defun denote-notion--org-to-markdown (org-body)
  "Convert ORG-BODY (a string of Org markup) to Markdown via `ox-md'."
  (require 'ox-md)
  (with-temp-buffer
    (insert org-body)
    (org-mode)
    (let ((md-buffer (org-export-to-buffer 'md (generate-new-buffer-name "*denote-notion-md*"))))
      (unwind-protect
          (with-current-buffer md-buffer
            (string-trim (buffer-string)))
        (kill-buffer md-buffer)))))

(defun denote-notion--export-body (file)
  "Return FILE's body, converted to Markdown if FILE is an Org note."
  (let ((body (denote-notion--body-without-front-matter file)))
    (if (eq (denote-filetype-heuristics file) 'org)
        (denote-notion--org-to-markdown body)
      body)))

;;; Export

(defun denote-notion--read-parent ()
  "Prompt for a Notion parent, honoring `denote-notion-default-parent'.
Returns a (TYPE . ID) cons.  Priority: `denote-notion-default-parent',
then an ACR pick from `denote-notion-parent-registry' if non-empty,
then a raw type+id prompt."
  (or denote-notion-default-parent
      (if denote-notion-parent-registry
          (denote-notion--acr-select-parent)
        (let ((type (intern (completing-read "Notion parent type: "
                                              '("page" "database" "data-source") nil t)))
              (id (read-string "Notion parent id: ")))
          (cons type id)))))

(defun denote-notion--export-title (file)
  "Return FILE's Denote title, or nil if it has none."
  (let ((title (denote-retrieve-front-matter-title-value
                file (denote-filetype-heuristics file))))
    (and title (not (string-empty-p title)) title)))

(defun denote-notion--rich-text-value (text)
  "Return a Notion rich_text property value array for the plain string TEXT."
  (vector (list (cons 'text (list (cons 'content text))))))

(defun denote-notion--set-page-title (id properties title)
  "PATCH page ID's title property (found via PROPERTIES) to TITLE.
Every Notion page has exactly one property of type `title'; its name
varies by schema (\"Name\" is common; a bare page parent's is literally
\"title\"), so the key is discovered from PROPERTIES rather than assumed.
Does nothing if TITLE is nil or PROPERTIES has no `title'-typed entry."
  (when-let* ((title (and title (not (string-empty-p title)) title))
              (key (car (seq-find (lambda (kv) (equal (map-elt (cdr kv) 'type) "title"))
                                   properties))))
    (denote-notion--apply-properties
     id (list (cons key (list (cons 'title (denote-notion--rich-text-value title))))))))

(defun denote-notion--set-tags-from-properties (file properties)
  "Set FILE's notion_tags from Notion page PROPERTIES' Tags multi_select, if any."
  (when-let* ((tags-prop (map-elt properties 'Tags))
              (multi-select (map-elt tags-prop 'multi_select))
              (names (seq-map (lambda (tag) (map-elt tag 'name)) multi-select)))
    (denote-notion--frontmatter-set file "notion_tags" names)))

;;; Custom Notion property values

(defconst denote-notion--property-sentinels
  `(("<today>" . ,(lambda () (format-time-string "%Y-%m-%d"))))
  "Alist of (SENTINEL . NILADIC-FN) recognized in Notion property values.
Any leaf string exactly matching SENTINEL — in a note's `notion_properties'
front-matter value, or in a `denote-notion-parent-registry' entry's default
PROPERTIES — is replaced by calling NILADIC-FN.")

(defun denote-notion--merge-properties (&rest alists)
  "Merge ALISTS of Notion property values; earlier alists' keys win."
  (let (result seen)
    (dolist (alist alists)
      (dolist (kv alist)
        (unless (member (car kv) seen)
          (push (car kv) seen)
          (push kv result))))
    (nreverse result)))

(defun denote-notion--resolve-property-sentinels (value)
  "Recursively replace sentinel strings (see `denote-notion--property-sentinels') in VALUE."
  (cond
   ((stringp value)
    (if-let* ((fn (cdr (assoc value denote-notion--property-sentinels))))
        (funcall fn)
      value))
   ((and (consp value) (consp (car value)))
    (mapcar (lambda (kv) (cons (car kv) (denote-notion--resolve-property-sentinels (cdr kv)))) value))
   ((listp value)
    (mapcar #'denote-notion--resolve-property-sentinels value))
   (t value)))

(defconst denote-notion--default-export-properties
  '((Timestamp (date (start . "<today>"))))
  "Built-in default Notion property values applied to every newly created
page, before any `denote-notion-parent-registry' entry's or file's own
override — see `denote-notion--merge-properties' precedence in
`denote-notion--export-create'.  Several Casap Notion data sources expect
a `Timestamp' date property to be populated on creation; this ensures
that happens even when no registry entry or per-file `notion_properties'
has been configured to do it explicitly.  Sentinels (e.g. \"<today>\")
are resolved the same as any other property value — see
`denote-notion--apply-properties'.")

(defun denote-notion--export-properties (file)
  "Return FILE's `notion_properties' front-matter value, parsed but unresolved.
The value is a raw JSON object mapping Notion property names to Notion API
property-value objects, e.g. {\"Timestamp\": {\"date\": {\"start\": \"<today>\"}}}.
Sentinels (see `denote-notion--property-sentinels') are left unresolved
here — `denote-notion--apply-properties' resolves them once, uniformly,
regardless of which layer (file, registry entry, or built-in default)
contributed the value.  Returns nil if FILE has no `notion_properties'
line."
  (when-let* ((raw (denote-notion--frontmatter-get file "notion_properties"))
              (raw (and (not (string-empty-p raw)) raw)))
    (json-parse-string raw :object-type 'alist :array-type 'list)))

(defun denote-notion--apply-properties (id properties)
  "PATCH Notion page ID's PROPERTIES (an alist ready for JSON serialization).
Sentinels in PROPERTIES (see `denote-notion--property-sentinels') are
resolved here, just before sending — the single point every caller's
merged properties pass through, whether they came from a note's own
`notion_properties', a `denote-notion-parent-registry' entry's default,
or `denote-notion--default-export-properties'.  Resolving earlier, per
layer, would miss whichever layers didn't happen to call it."
  (when properties
    (denote-notion--run-json
     (list "api" (format "v1/pages/%s" id)
           "--data" (json-serialize
                     (list (cons 'properties (denote-notion--resolve-property-sentinels properties))))
           "-X" "PATCH"))))

(defun denote-notion--parse-parent-arg (string)
  "Parse STRING (as formatted by `denote-notion--parent-arg') back to a cons.
Returns (TYPE . ID) with TYPE interned as a symbol, or nil if STRING is
nil/empty."
  (when (and string (not (string-empty-p string)))
    (when-let* ((pos (string-search ":" string)))
      (cons (intern (substring string 0 pos)) (substring string (1+ pos))))))

(defun denote-notion--export-create (file parent)
  "Create a new Notion page for FILE under PARENT and write back tracking fields.
PARENT is a (TYPE . ID) cons; see `denote-notion-default-parent'.  The
`type:id' form of PARENT itself (not any `denote-notion-parent-registry'
entry's name) is recorded as `notion_parent', so a later
`denote-notion--export-update' resolves default properties by looking the
same locator back up in the registry (see
`denote-notion--registry-entry-for-parent') — storing the registry name
instead would bitrot the moment that name is renamed or removed from
config, since the note itself has no other record of which parent it
was actually created under.  If PARENT matches a registry entry, that
entry's default properties are merged under FILE's own
`notion_properties', which in turn take precedence over
`denote-notion--default-export-properties' (e.g. `Timestamp') — so the
built-in default always populates a value on creation unless something
more specific already provides one.

`ntn pages create --json' returns the created page object directly at
its top level (unlike `ntn pages get --json', which wraps it under a
`page' key alongside the converted markdown) — RESULT below is used as
the page object as-is."
  (let* ((registry-entry (denote-notion--registry-entry-for-parent parent))
         (content (denote-notion--export-body file))
         (page (denote-notion--run-json
                (list "pages" "create" "--parent" (denote-notion--parent-arg parent)
                      "--content" content))))
    (denote-notion--frontmatter-set file "notion_id" (map-elt page 'id))
    (denote-notion--frontmatter-set file "notion_created" (map-elt page 'created_time))
    (denote-notion--frontmatter-set file "notion_edited" (map-elt page 'last_edited_time))
    (denote-notion--frontmatter-set file "notion_parent" (denote-notion--parent-arg parent))
    (denote-notion--set-tags-from-properties file (map-elt page 'properties))
    (denote-notion--set-page-title (map-elt page 'id) (map-elt page 'properties)
                                    (denote-notion--export-title file))
    (denote-notion--apply-properties
     (map-elt page 'id)
     (denote-notion--merge-properties (denote-notion--export-properties file)
                                       (cdr (cdr registry-entry))
                                       denote-notion--default-export-properties))
    (map-elt page 'url)))

(defun denote-notion--export-update (file force)
  "Update the Notion page already tracked by FILE, or signal a conflict.
With FORCE non-nil, overwrite the remote page without comparing timestamps.

Unlike `ntn pages create --json' (a flat page object with `url',
`properties', `last_edited_time', etc.), `ntn pages edit --json' returns
only a minimal confirmation object — id/markdown/object/request_id/
truncated/unknown_block_ids, no `url' or `properties' — so everything
below the content edit re-fetches the full page object via `pages get'
rather than reading it from the edit response."
  (let* ((notion-id (denote-notion--frontmatter-get file "notion_id"))
         (id (string-trim notion-id "\"" "\"")))
    (unless force
      (let* ((remote (map-elt (denote-notion--run-json (list "pages" "get" id)) 'page))
             (remote-edited (map-elt remote 'last_edited_time))
             (stored-edited (string-trim (or (denote-notion--frontmatter-get file "notion_edited") "") "\"" "\"")))
        (when (and remote-edited stored-edited
                   (not (string-empty-p stored-edited))
                   (string> remote-edited stored-edited))
          (user-error
           "Notion page %s changed since the last sync (remote %s > stored %s); run `denote-notion-pull' (with no page id, to dwim-refresh) first, or pass force"
           id remote-edited stored-edited))))
    (let ((content (denote-notion--export-body file)))
      (denote-notion--run-json (list "pages" "edit" id "--content" content)))
    (let* ((page (map-elt (denote-notion--run-json (list "pages" "get" id)) 'page))
           (stored-parent (string-trim (or (denote-notion--frontmatter-get file "notion_parent") "") "\"" "\""))
           (registry-entry (denote-notion--registry-entry-for-parent
                             (denote-notion--parse-parent-arg stored-parent)))
           (default-properties (cdr (cdr registry-entry))))
      (denote-notion--frontmatter-set file "notion_edited" (map-elt page 'last_edited_time))
      (denote-notion--set-page-title id (map-elt page 'properties) (denote-notion--export-title file))
      (denote-notion--apply-properties
       id (denote-notion--merge-properties (denote-notion--export-properties file) default-properties))
      (map-elt page 'url))))

;;;###autoload
(defun denote-notion-push (&optional file parent force)
  "Push FILE (default the current buffer's file) to Notion as a page.
If FILE is not yet Notion-tracked, PARENT — a (TYPE . ID) cons, where TYPE
is one of the symbols `page', `database', or `data-source' — is required:
prompted interactively unless `denote-notion-default-parent' is set — and
a new page is created.  If FILE is already tracked, its Notion page is
updated, unless the remote page has changed since the last sync, in which
case a conflict is signaled; pass FORCE (or the prefix argument,
interactively) to overwrite anyway."
  (interactive (list nil nil current-prefix-arg))
  (let* ((file (or file (denote-notion--file-at-point) (user-error "No file to export")))
         (url (if (denote-notion--tracked-p file)
                  (denote-notion--export-update file force)
                (denote-notion--export-create file (or parent (denote-notion--read-parent))))))
    (message "Exported to %s" url)))

;;; Import

(defun denote-notion--extract-page-id (id-or-url)
  "Return the 32-char hex page id embedded in ID-OR-URL."
  (if (string-match "\\([0-9a-fA-F]\\{32\\}\\)\\'"
                    (replace-regexp-in-string "-" "" id-or-url))
      (match-string 1 (replace-regexp-in-string "-" "" id-or-url))
    id-or-url))

(defun denote-notion--clean-imported-body (body)
  "Clean up BODY as returned by `ntn pages get' for storage in a denote note.
Every separate Notion block (a paragraph, a heading, a list item, ...) is
joined to the next by a single newline in `ntn's Markdown, with no blank
line between them; a single newline is not a paragraph break in Markdown,
so without widening it every block runs into the next as one paragraph.
Each single newline is widened to a blank line first, then a literal
\"<br>\" tag — Notion's *soft* line break within one block — is turned
into a single newline, so it does not also become a paragraph break.
Any literal square bracket in prose is also backslash-escaped (\\[, \\])
per CommonMark convention, to stop it from being misread as link syntax
by a Markdown parser; a denote note is not read through one, so the
escaping only pollutes prose that never had it in Notion's own editor."
  (thread-last body
               (replace-regexp-in-string "\n" "\n\n")
               (replace-regexp-in-string "<br[ \t]*/?>" "\n")
               (replace-regexp-in-string (regexp-quote "\\[") "[")
               (replace-regexp-in-string (regexp-quote "\\]") "]")))

(defun denote-notion--rich-text-plain (rich-text-array)
  "Return the concatenated `plain_text' of RICH-TEXT-ARRAY.
RICH-TEXT-ARRAY is a Notion API rich_text array (as found in a `title' or
`rich_text' property value) -- a list of alists each carrying their own
`plain_text', not a plain string on its own."
  (mapconcat (lambda (segment) (or (map-elt segment 'plain_text) ""))
             rich-text-array ""))

(defun denote-notion--import-write-body (file body)
  "Replace FILE's body (everything after its front matter) with BODY."
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-min))
    (cond
     ((looking-at "^---[ \t]*$")
      (forward-line 1)
      (re-search-forward "^---[ \t]*$" nil t)
      (forward-line 1))
     (t
      (while (looking-at "^#\\+")
        (forward-line 1))))
    (skip-chars-forward "\n")
    (delete-region (point) (point-max))
    (insert body "\n")
    (save-buffer)))

(defun denote-notion--import-refresh-file (file page-id)
  "Pull PAGE-ID's current content and tracking fields into FILE.
Syncs `notion_tags' from the page's current Tags property.  denote's own
title/keywords are left alone, matching how export treats them as
user-owned once set."
  (let* ((result (denote-notion--run-json (list "pages" "get" page-id)))
         (page (map-elt result 'page))
         ;; `ntn pages get --json' nests the markdown text under its own
         ;; `markdown' object: {"markdown": {"markdown": "...", ...}, "page": {...}}.
         (body (denote-notion--clean-imported-body
                (map-elt (map-elt result 'markdown) 'markdown))))
    (denote-notion--import-write-body file body)
    (denote-notion--frontmatter-set file "notion_edited" (map-elt page 'last_edited_time))
    (denote-notion--set-tags-from-properties file (map-elt page 'properties))))

(defun denote-notion--find-tracked-file (id)
  "Return the denote file already tracking Notion page ID, or nil.
Searches every file in `denote-directory-files', not just the current
buffer or an explicit TARGET-FILE -- otherwise importing a page id
already tracked by some other, not-currently-open note creates a
duplicate rather than refreshing the note that already exists for it."
  (seq-find (lambda (f)
              (equal (string-trim (or (denote-notion--frontmatter-get f "notion_id") "") "\"" "\"")
                     id))
            (denote-directory-files)))

;;;###autoload
(defun denote-notion-pull (&optional page-id target-file)
  "Pull a Notion page into a denote note, creating or refreshing as needed.

With PAGE-ID (an id, or a Notion URL): pull that specific page.  If
TARGET-FILE, the current buffer, or any other denote note already tracks
that page's id (see `denote-notion--find-tracked-file'), replace that
file's body and refresh its tracking fields in place.  Otherwise create a
new tracked denote note from the page's properties and body — so
re-running an import on a page you already have never creates a
duplicate note, even from a buffer other than the one already tracking it.

Without PAGE-ID: refresh TARGET-FILE (or the current buffer) using its
own already-tracked `notion_id', so \"pull this specific page\" and
\"re-pull whatever I'm already looking at\" are the same command.
Errors if there's no tracked file to fall back on.

Interactively, PAGE-ID is only prompted for when the current buffer isn't
already a tracked Notion note — otherwise this dwim-refreshes the current
buffer directly, with no prompt at all."
  (interactive
   (list (let ((file (denote-notion--file-at-point)))
           (unless (and file (denote-notion--tracked-p file))
             (read-string "Notion page id or URL: ")))))
  (let ((file (or target-file (denote-notion--file-at-point))))
    (if (not page-id)
        (progn
          (unless (and file (denote-notion--tracked-p file))
            (user-error "No file to refresh: not Notion-tracked, and no page id given"))
          (let ((id (string-trim (denote-notion--frontmatter-get file "notion_id") "\"" "\"")))
            (denote-notion--import-refresh-file file id)
            (message "Refreshed %s from Notion" (file-name-nondirectory file))))
      (let* ((id (denote-notion--extract-page-id page-id))
             (target (or target-file
                         (when (and file
                                    (equal (string-trim (or (denote-notion--frontmatter-get file "notion_id") "") "\"" "\"")
                                           id))
                           file)
                         (denote-notion--find-tracked-file id))))
        (if target
            (progn
              (denote-notion--import-refresh-file target id)
              (message "Refreshed %s from Notion" (file-name-nondirectory target)))
          (let* ((result (denote-notion--run-json (list "pages" "get" id)))
                 (page (map-elt result 'page))
                 (body (denote-notion--clean-imported-body
                        (map-elt (map-elt result 'markdown) 'markdown)))
                 (properties (map-elt page 'properties))
                 ;; `Name' (a `title' property) is a Notion rich_text array,
                 ;; not a plain string -- see `denote-notion--rich-text-plain'.
                 (title (denote-notion--rich-text-plain
                         (map-elt (map-elt properties 'Name) 'title)))
                 (tags (seq-map (lambda (tag) (map-elt tag 'name))
                                (map-elt (map-elt properties 'Tags) 'multi_select)))
                 ;; Backdate the new note's identifier/date to when the
                 ;; Notion page was actually created, rather than "now" --
                 ;; otherwise an import loses the page's real authorship date.
                 (created (map-elt page 'created_time)))
            ;; `markdown-yaml' matches the front matter shape already used
            ;; by every note this tool tracks.
            (denote (if (and title (not (string-empty-p title))) title "Untitled Notion import")
                    (cons "notion" tags) 'markdown-yaml nil created)
            (let ((new-file (buffer-file-name)))
              (goto-char (point-max))
              (insert "\n" body)
              (denote-notion--frontmatter-set new-file "notion_id" id)
              (denote-notion--frontmatter-set new-file "notion_tags" tags)
              (denote-notion--frontmatter-set new-file "notion_created" (map-elt page 'created_time))
              (denote-notion--frontmatter-set new-file "notion_edited" (map-elt page 'last_edited_time))
              (save-buffer)
              (message "Imported %s" (file-name-nondirectory new-file)))))))))

(provide 'denote-notion)
;;; denote-notion.el ends here
