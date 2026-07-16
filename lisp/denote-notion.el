;;; denote-notion.el --- Two-way sync between Denote notes and Notion pages -*- lexical-binding: t; -*-

;;; Commentary:
;; Push a Denote note to Notion as a page, or pull a Notion page back into
;; a Denote note, on demand via the `npx ntn` CLI.  Both directions are
;; manual/interactive; nothing here runs on a timer or watch.
;;
;; Entry points:
;;   `denote-notion-export-post'     — create or update the Notion page for
;;                                     the current note
;;   `denote-notion-import-page'     — pull a Notion page into a new or
;;                                     existing tracked note
;;   `denote-notion-import-refresh'  — re-pull the current note's tracked page

;;; Code:

(require 'seq)
(require 'map)
(require 'json)
(require 'cl-lib)
(require 'denote)

(declare-function org-export-to-buffer "ox")
(declare-function annotated-completing-read "annotated-completing-read")

;;; Custom variables

(defgroup denote-notion nil
  "Two-way sync between Denote notes and Notion pages."
  :group 'denote)

(defcustom denote-notion-default-parent nil
  "Default Notion parent for a first-time export, or nil to always prompt.
When set, a cons of (TYPE . ID): TYPE is one of the symbols `page',
`database', or `data-source'; ID is that resource's id string.  Takes
priority over `denote-notion-parent-registry' — set this only for a
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
  "Run \"npx ntn\" with ARGS plus --json and return the parsed result.
Returns nil if the process exits non-zero.  Signals a `user-error' with
the CLI's own output when the exit code is non-zero."
  (let* ((result (denote-notion--run (append args '("--json"))))
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

(defun denote-notion--content-with-title (file body)
  "Prepend FILE's Denote title to BODY as a leading heading, if it has one.
`ntn pages create'/`edit' read a leading H1 in the Markdown content as the
page title rather than rendering it as a body block."
  (if-let* ((title (denote-notion--export-title file)))
      (concat "# " title "\n\n" body)
    body))

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

(defun denote-notion--export-properties (file)
  "Return FILE's `notion_properties' front-matter value, parsed and resolved.
The value is a raw JSON object mapping Notion property names to Notion API
property-value objects, e.g. {\"Timestamp\": {\"date\": {\"start\": \"<today>\"}}}.
Sentinels in `denote-notion--property-sentinels' (e.g. \"<today>\") are
replaced before sending.  Returns nil if FILE has no `notion_properties'
line."
  (when-let* ((raw (denote-notion--frontmatter-get file "notion_properties"))
              (raw (and (not (string-empty-p raw)) raw)))
    (denote-notion--resolve-property-sentinels
     (json-parse-string raw :object-type 'alist :array-type 'list))))

(defun denote-notion--apply-properties (id properties)
  "PATCH Notion page ID's PROPERTIES (an alist ready for JSON serialization)."
  (when properties
    (denote-notion--run-json
     (list "api" (format "v1/pages/%s" id)
           "--data" (json-serialize (list (cons 'properties properties)))
           "-X" "PATCH"))))

(defun denote-notion--export-create (file parent)
  "Create a new Notion page for FILE under PARENT and write back tracking fields.
PARENT is a (TYPE . ID) cons; see `denote-notion-default-parent'.  If
PARENT matches a `denote-notion-parent-registry' entry, that entry's name
is recorded as `notion_parent' (so `denote-notion--export-update' can
keep reapplying its default properties) and its default properties are
merged under FILE's own `notion_properties'."
  (let* ((registry-entry (denote-notion--registry-entry-for-parent parent))
         (content (denote-notion--content-with-title file (denote-notion--export-body file)))
         (result (denote-notion--run-json
                  (list "pages" "create" "--parent" (denote-notion--parent-arg parent)
                        "--content" content)))
         (page (map-elt result 'page)))
    (denote-notion--frontmatter-set file "notion_id" (map-elt page 'id))
    (denote-notion--frontmatter-set file "source_url" (map-elt page 'url))
    (denote-notion--frontmatter-set file "notion_created" (map-elt page 'created_time))
    (denote-notion--frontmatter-set file "notion_edited" (map-elt page 'last_edited_time))
    (denote-notion--set-tags-from-properties file (map-elt page 'properties))
    (when registry-entry
      (denote-notion--frontmatter-set file "notion_parent" (car registry-entry)))
    (denote-notion--apply-properties
     (map-elt page 'id)
     (denote-notion--merge-properties (denote-notion--export-properties file)
                                       (cdr (cdr registry-entry))))
    (map-elt page 'url)))

(defun denote-notion--export-update (file force)
  "Update the Notion page already tracked by FILE, or signal a conflict.
With FORCE non-nil, overwrite the remote page without comparing timestamps."
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
           "Notion page %s changed since the last sync (remote %s > stored %s); run `denote-notion-import-refresh' first, or pass force"
           id remote-edited stored-edited))))
    (let* ((content (denote-notion--content-with-title file (denote-notion--export-body file)))
           (result (denote-notion--run-json (list "pages" "edit" id "--content" content)))
           (page (map-elt result 'page))
           (parent-name (string-trim (or (denote-notion--frontmatter-get file "notion_parent") "") "\"" "\""))
           (default-properties (cdr (cdr (assoc parent-name denote-notion-parent-registry)))))
      (denote-notion--frontmatter-set file "notion_edited" (map-elt page 'last_edited_time))
      (denote-notion--apply-properties
       id (denote-notion--merge-properties (denote-notion--export-properties file) default-properties))
      (map-elt page 'url))))

;;;###autoload
(defun denote-notion-export-post (&optional file parent force)
  "Push FILE (default the current buffer's file) to Notion as a page.
If FILE is not yet Notion-tracked, PARENT — a (TYPE . ID) cons, where TYPE
is one of the symbols `page', `database', or `data-source' — is required:
prompted interactively unless `denote-notion-default-parent' is set — and
a new page is created.  If FILE is already tracked, its Notion page is
updated, unless the remote page has changed since the last sync, in which
case a conflict is signaled; pass FORCE (or the prefix argument,
interactively) to overwrite anyway."
  (interactive (list nil nil current-prefix-arg))
  (let* ((file (or file (buffer-file-name) (user-error "No file to export")))
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
  "Pull PAGE-ID's current content and tracking fields into FILE."
  (let* ((result (denote-notion--run-json (list "pages" "get" page-id)))
         (page (map-elt result 'page))
         ;; `ntn pages get --json' nests the markdown text under its own
         ;; `markdown' object: {"markdown": {"markdown": "...", ...}, "page": {...}}.
         (body (map-elt (map-elt result 'markdown) 'markdown)))
    (denote-notion--import-write-body file body)
    (denote-notion--frontmatter-set file "notion_edited" (map-elt page 'last_edited_time))))

;;;###autoload
(defun denote-notion-import-refresh (&optional file)
  "Re-pull FILE's (default current buffer's) tracked Notion page into it."
  (interactive)
  (let* ((file (or file (buffer-file-name) (user-error "No file to refresh")))
         (notion-id (or (denote-notion--frontmatter-get file "notion_id")
                        (user-error "%s is not Notion-tracked" file)))
         (id (string-trim notion-id "\"" "\"")))
    (denote-notion--import-refresh-file file id)
    (message "Refreshed %s from Notion" (file-name-nondirectory file))))

;;;###autoload
(defun denote-notion-import-page (page-id &optional target-file)
  "Pull Notion page PAGE-ID (an id, or a Notion URL, or the current tracked file).
With TARGET-FILE (or when the current buffer is already tracked with this
page id), replace that file's body and update its tracking fields.
Otherwise create a new Denote note from the page's properties and body."
  (interactive (list (read-string "Notion page id or URL: ")))
  (let* ((id (denote-notion--extract-page-id page-id))
         (target (or target-file
                     (when (and (buffer-file-name)
                                (equal (string-trim (or (denote-notion--frontmatter-get (buffer-file-name) "notion_id") "") "\"" "\"")
                                       id))
                       (buffer-file-name)))))
    (if target
        (progn
          (denote-notion--import-refresh-file target id)
          (message "Refreshed %s from Notion" (file-name-nondirectory target)))
      (let* ((result (denote-notion--run-json (list "pages" "get" id)))
             (page (map-elt result 'page))
             (body (map-elt (map-elt result 'markdown) 'markdown))
             (properties (map-elt page 'properties))
             (title (map-elt (map-elt properties 'Name) 'title))
             (tags (seq-map (lambda (tag) (map-elt tag 'name))
                            (map-elt (map-elt properties 'Tags) 'multi_select))))
        (denote (if (and title (not (string-empty-p title))) title "Untitled Notion import")
                (cons "notion" tags) 'markdown)
        (let ((file (buffer-file-name)))
          (goto-char (point-max))
          (insert "\n" body)
          (denote-notion--frontmatter-set file "notion_id" id)
          (denote-notion--frontmatter-set file "notion_tags" tags)
          (denote-notion--frontmatter-set file "notion_created" (map-elt page 'created_time))
          (denote-notion--frontmatter-set file "notion_edited" (map-elt page 'last_edited_time))
          (denote-notion--frontmatter-set file "source_url" (map-elt page 'url))
          (save-buffer)
          (message "Imported %s" (file-name-nondirectory file)))))))

(provide 'denote-notion)
;;; denote-notion.el ends here
