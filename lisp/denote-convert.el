;;; denote-convert.el --- File format translation and Org datetree importer for Denote -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (denote "3.0.0"))
;; Keywords: docs, denote, convenience, tools
;; URL: https://github.com/tychoish/denote-convert

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Standalone file format conversion and Org datetree import workflows
;; for Denote:
;; - Convert existing Denote notes between file types (Org, Markdown YAML,
;;   Markdown TOML, and Plain Text), rewriting frontmatter and filenames.
;; - Import Org-mode datetree journal/diary entries into atomic Denote notes,
;;   preserving timestamps, tags, and content bodies.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'denote)
(require 'org)

(defgroup denote-convert nil
  "File format translation and Org datetree importer for Denote."
  :group 'denote
  :link '(url-link "https://github.com/tychoish/denote-convert"))

(defcustom denote-convert-after-conversion-functions nil
  "Hook run after a note's file type is converted.
Each function is called with four arguments:
  (OLD-FILE NEW-FILE OLD-TYPE NEW-TYPE)."
  :group 'denote-convert
  :type 'hook)

;;; Context file resolution

(defun denote-convert--file-at-point ()
  "Return the Denote file implied by current point/buffer context, or nil."
  (cond
   ((and (fboundp 'tabulated-list-get-id)
         (derived-mode-p 'tabulated-list-mode))
    (let ((id (tabulated-list-get-id)))
      (when (and (stringp id) (file-exists-p id)) id)))
   ((derived-mode-p 'dired-mode)
    (let ((f (dired-get-filename nil t)))
      (when (and f (denote-file-has-identifier-p f)) f)))
   ((and buffer-file-name (denote-file-has-identifier-p buffer-file-name))
    buffer-file-name)))

;;;###autoload
(defun denote-convert-file-prompt (&optional prompt default-file)
  "Prompt for a Denote note file with completion.
PROMPT defaults to \"Convert note: \".  DEFAULT-FILE pre-selects a candidate."
  (let* ((files (denote-directory-files))
         (default (or default-file (denote-convert--file-at-point)))
         (choice (completing-read
                  (or prompt (if default
                                 (format "Convert note (default %s): "
                                         (file-name-nondirectory default))
                               "Convert note: "))
                  files nil t nil nil default)))
    (or choice (user-error "No note selected"))))

(defun denote-convert--target-file ()
  "Return target note file from context or user prompt."
  (or (denote-convert--file-at-point)
      (denote-convert-file-prompt)))

;;; Org datetree import

(defconst denote-convert--datetree-day-re
  "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}"
  "Regexp matching a YYYY-MM-DD date prefix in an org datetree day heading.")

(defun denote-convert--datetree-day-date (heading)
  "Return YYYY-MM-DD prefix if HEADING is a datetree day heading, else nil."
  (when (string-match denote-convert--datetree-day-re heading)
    (match-string 0 heading)))

(defun denote-convert--datetree-parent-date ()
  "Return date string if immediate parent is a datetree day heading, else nil."
  (save-excursion
    (when (org-up-heading-safe)
      (denote-convert--datetree-day-date (org-get-heading t t t t)))))

(defun denote-convert--entry-body ()
  "Return the body text of the current org heading as a trimmed string."
  (save-excursion
    (org-back-to-heading t)
    (forward-line 1)
    (string-trim
     (buffer-substring-no-properties
      (point)
      (if (re-search-forward org-heading-regexp nil t)
          (match-beginning 0)
        (point-max))))))

(defun denote-convert--collect-datetree-entries ()
  "Scan current buffer for leaf org entries under a datetree day heading."
  (let (entries)
    (org-map-entries
     (lambda ()
       (when-let* ((date (denote-convert--datetree-parent-date)))
         (let* ((title (org-get-heading t t t t))
                (tags  (org-get-tags))
                (body  (denote-convert--entry-body)))
           (push (list :date date :title title :tags tags :body body) entries))))
     nil 'file)
    (nreverse entries)))

(defun denote-convert--import-entry (entry)
  "Create a Denote note from ENTRY plist."
  (let* ((date  (plist-get entry :date))
         (title (plist-get entry :title))
         (tags  (plist-get entry :tags))
         (body  (plist-get entry :body))
         (note-path (denote title tags denote-file-type nil date)))
    (with-current-buffer (find-file-noselect note-path)
      (save-excursion
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert "\n" body)
        (save-buffer)))))

(defun denote-convert--entry-in-range-p (entry from-date to-date)
  "Return non-nil if ENTRY date falls between FROM-DATE and TO-DATE (inclusive)."
  (let ((d (plist-get entry :date)))
    (and (or (null from-date) (not (string< d from-date)))
         (or (null to-date)   (not (string< to-date d))))))

;;;###autoload
(defun denote-convert-import-from-datetree (file &optional from-date to-date)
  "Import org datetree entries from FILE as individual Denote notes.
With optional FROM-DATE and TO-DATE (YYYY-MM-DD strings), restricts
import to that inclusive date range.  Interactively, prompts for the
file and whether to apply a date restriction."
  (interactive
   (let* ((f (read-file-name "Import from datetree: " nil nil t nil
                             (lambda (n)
                                (or (file-directory-p n)
                                    (string-suffix-p ".org" n)))))
          (restrict (yes-or-no-p "Restrict to a date range? "))
          (from (when restrict (org-read-date nil nil nil "From (inclusive): ")))
          (to   (when restrict (org-read-date nil nil nil "To (inclusive): "))))
     (list f from to)))
  (let* ((entries (with-current-buffer (find-file-noselect file)
                    (denote-convert--collect-datetree-entries)))
         (filtered (seq-filter (lambda (e)
                                 (denote-convert--entry-in-range-p e from-date to-date))
                               entries))
         (n (length filtered)))
    (when (zerop n)
      (user-error "No datetree entries found%s"
                  (if (or from-date to-date)
                      (format " between %s and %s" from-date to-date)
                    "")))
    (unless (yes-or-no-p (format "Create %d denote note%s from %s? "
                                 n (if (= n 1) "" "s")
                                 (file-name-nondirectory file)))
      (user-error "Import cancelled"))
    (let ((ok 0) (fail 0))
      (dolist (entry filtered)
        (condition-case err
            (progn (denote-convert--import-entry entry) (setq ok (1+ ok)))
          (error
           (setq fail (1+ fail))
           (message "Skipped %S: %s"
                    (plist-get entry :title)
                    (error-message-string err)))))
      (message "Imported %d/%d note%s%s."
               ok n (if (= ok 1) "" "s")
               (if (> fail 0) (format " (%d failed)" fail) "")))))

;;; File type migration

(defun denote-convert--front-matter-end (file-type)
  "Return the position where FILE-TYPE's front matter block ends.
Finds the last line matching any of the title, keywords, signature,
identifier, or date key regexps for FILE-TYPE, then consumes any trailing
delimiter-only line (e.g. --- or +++) and following blank separator lines."
  (let ((end (point-min)))
    (seq-do (lambda (component)
              (save-excursion
                (goto-char (point-min))
                (when (re-search-forward
                       (funcall (denote--get-component-key-regexp-function component) file-type)
                       nil t)
                  (setq end (max end (1+ (line-end-position)))))))
            '(title keywords signature identifier date))
    (goto-char end)
    (when (looking-at "[ \t]*\\(?:-\\{3,\\}\\|\\+\\{3,\\}\\)[ \t]*\n")
      (goto-char (match-end 0)))
    (skip-chars-forward "\n")
    (point)))

;;;###autoload
(defun denote-convert-file-type (file new-file-type)
  "Migrate FILE's front matter and extension to NEW-FILE-TYPE.
Rewrites only the front matter block (title, keywords, signature,
identifier, date) in NEW-FILE-TYPE's syntax and renames the file to
match NEW-FILE-TYPE's extension.  The note body is left untouched."
  (interactive
   (list (denote-convert--target-file)
         (denote--valid-file-type (or (denote-file-type-prompt) denote-file-type))))
  (let ((old-file-type (denote-filetype-heuristics file)))
    (when (eq old-file-type new-file-type)
      (user-error "File is already of type %s" new-file-type))
    (unless (yes-or-no-p (format "Convert %s from %s to %s (front matter + extension only)? "
                                 (file-name-nondirectory file) old-file-type new-file-type))
      (user-error "Cancelled"))
    (let* ((id (or (denote-retrieve-filename-identifier file) ""))
           (date (denote-retrieve-front-matter-date-value file old-file-type))
           (title (or (denote-retrieve-title-or-filename file old-file-type) ""))
           (keywords (denote-retrieve-front-matter-keywords-value file old-file-type))
           (signature (or (denote-retrieve-filename-signature file) ""))
           (new-front-matter (denote--format-front-matter
                              title date keywords id signature new-file-type))
           (new-name (denote-format-file-name (file-name-directory file) id keywords title
                                              (denote--file-extension new-file-type) signature))
           (buf (find-file-noselect file)))
      (with-current-buffer buf
        (save-excursion
          (goto-char (point-min))
          (delete-region (point-min) (denote-convert--front-matter-end old-file-type))
          (goto-char (point-min))
          (insert new-front-matter))
        (save-buffer))
      (kill-buffer buf)
      (unless (string= (expand-file-name file) (expand-file-name new-name))
        (rename-file file new-name))
      (run-hook-with-args 'denote-convert-after-conversion-functions file new-name old-file-type new-file-type)
      (message "Converted %s -> %s" (file-name-nondirectory file) (file-name-nondirectory new-name)))))

(provide 'denote-convert)
;;; denote-convert.el ends here
