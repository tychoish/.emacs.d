;;; denote-dispatch.el --- Transient dispatch for Denote commands -*- lexical-binding: t; -*-

;;; Commentary:
;; Transient prefix exposing the full Denote command surface:
;; create, find, view, link, rename, sequence, filter, and convert.
;;
;; Bind via tychoish/denote-map:
;;   (bind-key "dd" #'denote-dispatch tychoish/denote-map)

;;; Code:

(require 'transient)

(declare-function org-back-to-heading "org")
(declare-function org-end-of-meta-data "org")
(declare-function org-end-of-subtree "org")
(declare-function org-get-heading "org")
(declare-function org-get-tags "org")
(declare-function org-map-entries "org")
(declare-function org-read-date "org")
(declare-function org-up-heading-safe "org")

(declare-function denote "denote")
(declare-function denote-open-or-create "denote")
(declare-function denote-link "denote")
(declare-function denote-backlinks "denote")
(declare-function denote-rename-file "denote")
(declare-function denote-dired "denote")
(declare-function denote-rename-file-using-front-matter "denote")
(declare-function consult-denote-find "consult-denote")
(declare-function consult-denote-grep "consult-denote")
(declare-function denote-journal-new-entry "denote-journal")
(declare-function denote-sequence-new-child "denote-sequence")
(declare-function denote-sequence-new-sibling "denote-sequence")
(declare-function denote-sequence-new-parent "denote-sequence")
(declare-function denote-sequence-link-to-parent "denote-sequence")
(declare-function denote-sequence-reparent "denote-sequence")
(declare-function denote-markdown-convert-links-to-markdown-format "denote-markdown")
(declare-function denote-markdown-convert-links-to-denote-format "denote-markdown")

;;;; Org datetree import
;;
;; Org datetrees have a three-level date hierarchy under which entries live:
;;
;;   * 2024
;;   ** 2024-01 January
;;   *** 2024-01-15 Monday
;;   **** Entry title          ← collected here
;;        Body content
;;
;; Only direct children of day headings (matching YYYY-MM-DD) are imported.
;; Each becomes a separate denote note with the date, heading title, org
;; tags, and body content preserved.

(defconst denote-dispatch--datetree-day-re "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}"
  "Regexp matching the YYYY-MM-DD date prefix of an org datetree day heading.")

(defun denote-dispatch--datetree-day-date (heading)
  "Return the YYYY-MM-DD string if HEADING is a datetree day heading, else nil."
  (when (string-match denote-dispatch--datetree-day-re heading)
    (match-string 0 heading)))

(defun denote-dispatch--datetree-parent-date ()
  "Return the date string if the immediate parent is a datetree day heading, else nil."
  (save-excursion
    (when (org-up-heading-safe)
      (denote-dispatch--datetree-day-date (org-get-heading t t t t)))))

(defun denote-dispatch--entry-body ()
  "Return the body of the current org heading, after the heading line and any meta-data.
Includes sub-headings; excludes the heading itself, property drawers, and planning lines."
  (save-excursion
    (org-back-to-heading t)
    (forward-line 1)
    (org-end-of-meta-data t)
    (let ((beg (point))
          (end (save-excursion (org-end-of-subtree t) (point))))
      (string-trim (buffer-substring-no-properties beg end)))))

(defun denote-dispatch--collect-datetree-entries ()
  "Return a list of entry plists from the current buffer's org datetree.
Each plist has :title :date :tags :body.  Only headings that are direct
children of day-level headings (YYYY-MM-DD ...) are collected."
  (let (entries)
    (org-map-entries
     (lambda ()
       (when-let* ((date (denote-dispatch--datetree-parent-date))
                   (title (string-trim (org-get-heading t t t t)))
                   (_ (not (string-empty-p title))))
         (push (list :title title
                     :date  date
                     :tags  (org-get-tags nil t)
                     :body  (denote-dispatch--entry-body))
               entries))))
    (nreverse entries)))

(defun denote-dispatch--import-entry (entry)
  "Create a denote note from datetree ENTRY plist.
Uses the entry's date as the note identifier, title as the note title,
org tags as denote keywords, and body as the note content."
  (let* ((title (plist-get entry :title))
         (tags  (plist-get entry :tags))
         (body  (plist-get entry :body))
         (time  (org-read-date nil t (plist-get entry :date))))
    (save-window-excursion
      (denote title tags 'org nil time)
      (when (and body (not (string-empty-p body)))
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert "\n" body)
        (save-buffer)))))

(defun denote-dispatch-import-from-datetree (file &optional from-date to-date)
  "Import org datetree entries from FILE as individual denote notes.
Only direct children of day headings are collected.  With optional
FROM-DATE and TO-DATE (YYYY-MM-DD strings), restricts import to that
inclusive range.  Interactively, prompts for the file and whether to
restrict by date range."
  (interactive
   (let* ((f (read-file-name "Import from datetree: " nil nil t nil
                             (lambda (n) (or (file-directory-p n)
                                             (string-suffix-p ".org" n)))))
          (restrict (yes-or-no-p "Restrict to a date range? "))
          (from (when restrict (org-read-date nil nil nil "From (inclusive): ")))
          (to   (when restrict (org-read-date nil nil nil "To (inclusive): "))))
     (list f from to)))
  (let* ((entries
          (with-current-buffer (find-file-noselect file)
            (denote-dispatch--collect-datetree-entries)))
         (filtered
          (seq-filter
           (lambda (e)
             (let ((d (plist-get e :date)))
               (and (or (null from-date) (not (string< d from-date)))
                    (or (null to-date)   (not (string< to-date d))))))
           entries))
         (n (length filtered)))
    (when (zerop n)
      (user-error "No datetree entries found%s"
                  (if (or from-date to-date)
                      (format " between %s and %s" from-date to-date)
                    "")))
    (unless (yes-or-no-p
             (format "Create %d denote note%s from %s? "
                     n (if (= n 1) "" "s") (file-name-nondirectory file)))
      (user-error "Import cancelled"))
    (let ((ok 0) (fail 0))
      (seq-do (lambda (entry)
                (condition-case err
                    (progn (denote-dispatch--import-entry entry) (cl-incf ok))
                  (error
                   (cl-incf fail)
                   (message "Skipped %S: %s"
                            (plist-get entry :title)
                            (error-message-string err)))))
              filtered)
      (message "Imported %d/%d note%s%s."
               ok n (if (= ok 1) "" "s")
               (if (> fail 0) (format " (%d failed)" fail) "")))))

;;;###autoload
(transient-define-prefix denote-dispatch ()
  "Dispatch Denote commands by area."
  [["Create"
    ("nc" "new note"            denote)
    ("nj" "journal entry"       denote-journal-new-entry)
    ("ns" "sequence sibling"    denote-sequence-new-sibling)
    ("nh" "sequence child"      denote-sequence-new-child)
    ("np" "sequence parent"     denote-sequence-new-parent)]
   ["Find & View"
    ("ff" "find file"           consult-denote-find)
    ("fg" "grep content"        consult-denote-grep)
    ("fo" "open or create"      denote-open-or-create)
    ("fd" "dired view"          denote-dired)
    ("fb" "backlinks"           denote-backlinks)]]
  [["Link"
    ("ll" "insert link"         denote-link)
    ("lp" "link to parent"      denote-sequence-link-to-parent)]
   ["Rename & Manage"
    ("rr" "rename file"         denote-rename-file)
    ("rf" "rename from fm"      denote-rename-file-using-front-matter)
    ("rs" "reparent sequence"   denote-sequence-reparent)]
  ["Convert"
    ("cm" "links → markdown"    denote-markdown-convert-links-to-markdown-format)
    ("cd" "links → denote"      denote-markdown-convert-links-to-denote-format)]
  ["Import"
    ("id" "from org datetree"   denote-dispatch-import-from-datetree)]])

(provide 'denote-dispatch)
;;; denote-dispatch.el ends here
