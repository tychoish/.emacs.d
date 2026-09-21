;;; denote-mcp.el --- Denote MCP service integration for mcpkit -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Keywords: tools, mcp, denote, notes

;;; Commentary:
;;
;; Provides Model Context Protocol (MCP) tools for interacting with the Denote
;; note-taking and knowledge-base system via mcpkit.el.
;;
;; Exposes discovery, reading, note creation, frontmatter syncing, Folgezettel
;; sequence hierarchy management, linking, dynamic block insertion, backdating,
;; and plan lifecycle tracking.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'mcpkit)
(require 'denote)
(require 'denote-sequence nil t)
(require 'denote-dash nil t)
(require 'denote-dash-repack nil t)
(require 'denote-org nil t)

;;; Directory & Execution Environment

(defgroup denote-mcp nil
  "Denote MCP service integration for mcpkit."
  :group 'mcpkit
  :prefix "denote-mcp-")

(defcustom denote-mcp-port 8765
  "Default TCP port for the Denote MCP service."
  :type 'integer
  :group 'denote-mcp)

(defcustom denote-mcp-executed-keyword "x"
  "Filetag applied to mark a plan note as executed/implemented."
  :type 'string
  :group 'denote-mcp)

(defun denote-mcp-get-directory ()
  "Return the primary denote directory as an absolute directory path."
  (cond
   ((and (boundp 'denote-directory) (listp denote-directory))
    (file-name-as-directory (expand-file-name (car denote-directory))))
   ((and (boundp 'denote-directory) (stringp denote-directory))
    (file-name-as-directory (expand-file-name denote-directory)))
   ((fboundp 'denote-directory)
    (file-name-as-directory (expand-file-name (denote-directory))))
   (t (file-name-as-directory (expand-file-name "~/denote/")))))

(defmacro denote-mcp-silently (&rest body)
  "Execute BODY with all interactive confirmation prompts neutralized.
Suppresses y-or-n-p prompts, local variables queries, stale-buffer revert
prompts, large-file warnings, and UTF-8 encoding prompts."
  `(cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
             ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
     (let ((enable-local-variables nil)
           (revert-without-query '(".*"))
           (large-file-warning-threshold nil)
           (coding-system-for-read 'utf-8)
           (coding-system-for-write 'utf-8)
           (denote-save-buffers t))
       ,@body)))

;;; Helper Functions

(defun denote-mcp--find-file (file-or-slug)
  "Resolve FILE-OR-SLUG to an absolute file path in denote directory.
If FILE-OR-SLUG exists on disk, return it.  Otherwise searches the denote
directory and its journal/ subdirectory for matching slug or identifier."
  (cond
   ((and (stringp file-or-slug) (file-exists-p file-or-slug))
    (expand-file-name file-or-slug))
   ((and (stringp file-or-slug) (not (string-empty-p file-or-slug)))
    (let* ((denote-dir (denote-mcp-get-directory))
           (journal-dir (expand-file-name "journal/" denote-dir))
           (pattern (regexp-quote file-or-slug)))
      (or (car (directory-files denote-dir t pattern))
          (when (file-directory-p journal-dir)
            (car (directory-files journal-dir t pattern))))))
   (t nil)))

(defun denote-mcp--parse-filename (file)
  "Parse denote FILE into a plist :id, :seq, :title, :keywords, :ext."
  (let ((base (file-name-nondirectory file)))
    (when (string-match
           (concat "^\\([0-9]\\{8\\}T[0-9]\\{6\\}\\)"
                   "\\(?:==\\([^-]+\\)\\)?--"
                   "\\([^_]+\\)"
                   "\\(?:__\\([^.]+\\)\\)?"
                   "\\.\\([a-zA-Z0-9]+\\)$")
           base)
      (list :id (match-string 1 base)
            :seq (match-string 2 base)
            :title (match-string 3 base)
            :keywords (when-let* ((kws (match-string 4 base)))
                        (split-string kws "_" t))
            :ext (match-string 5 base)))))

(defun denote-mcp--get-metadata (file)
  "Extract structured frontmatter and file metadata from FILE."
  (with-temp-buffer
    (insert-file-contents file nil 0 2000)
    (let* ((get-org (lambda (key)
                      (goto-char (point-min))
                      (if (re-search-forward (concat "^#\\+" key ":[ \t]*\\(.*\\)$") nil t)
                          (string-trim (match-string 1)) "")))
           (title (funcall get-org "title"))
           (date (funcall get-org "date"))
           (tags (funcall get-org "filetags"))
           (id (funcall get-org "identifier"))
           (sig (funcall get-org "signature"))
           (status (funcall get-org "execution_status"))
           (ext (file-name-extension file)))
      ;; Fallback to filename components if frontmatter is empty
      (let ((parsed (denote-mcp--parse-filename file)))
        (when (string-empty-p id)
          (setq id (or (plist-get parsed :id) "")))
        (when (string-empty-p sig)
          (setq sig (or (plist-get parsed :seq) "")))
        (when (string-empty-p title)
          (setq title (or (plist-get parsed :title) (file-name-base file))))
        (list :path file
              :id id
              :signature (if (string-empty-p sig) nil sig)
              :title title
              :date (if (string-empty-p date) nil date)
              :filetags (if (string-empty-p tags)
                            (or (plist-get parsed :keywords) [])
                          (split-string (string-trim tags ":" ":") ":" t))
              :execution_status (if (string-empty-p status) nil status)
              :format ext)))))

(defun denote-mcp--set-sequence (file seq)
  "Assign sequence SEQ to FILE by updating #+signature: and renaming via denote."
  (let ((path (denote-mcp--find-file file)))
    (unless (and path (file-exists-p path))
      (error "File not found: %s" file))
    (denote-mcp-silently
     (let ((buf (find-file-noselect path)))
       (with-current-buffer buf
         ;; Remove existing signature lines
         (goto-char (point-min))
         (while (re-search-forward "^#\\+signature:.*\\n?" nil t)
           (delete-region (match-beginning 0) (match-end 0)))
         ;; Insert new signature after identifier or after first header block
         (goto-char (point-min))
         (if (re-search-forward "^#\\+identifier:.*$" nil t)
             (progn (end-of-line) (insert "\n#+signature: " seq))
           (goto-char (point-min))
           (while (and (not (eobp)) (looking-at "^#\\+"))
             (forward-line 1))
           (beginning-of-line)
           (insert "#+signature: " seq "\n"))
         (save-buffer)
         (denote-rename-file-using-front-matter path)
         (buffer-file-name))))))

(defun denote-mcp--set-identifier (file new-id)
  "Change FILE's #+identifier: to NEW-ID (YYYYMMDDTHHMMSS) and rename."
  (let ((path (denote-mcp--find-file file)))
    (unless (and path (file-exists-p path))
      (error "File not found: %s" file))
    (let* ((time (encode-time (parse-time-string
                               (format "%s-%s-%sT%s:%s:%s"
                                       (substring new-id 0 4) (substring new-id 4 6) (substring new-id 6 8)
                                       (substring new-id 9 11) (substring new-id 11 13) (substring new-id 13 15)))))
           (date-str (format-time-string "[%Y-%m-%d %a %H:%M]" time)))
      (denote-mcp-silently
       (let ((buf (find-file-noselect path)))
         (with-current-buffer buf
           (goto-char (point-min))
           (if (re-search-forward "^#\\+identifier:.*$" nil t)
               (replace-match (concat "#+identifier: " new-id))
             (error "No #+identifier: found in %s" path))
           (goto-char (point-min))
           (when (re-search-forward "^#\\+date:.*$" nil t)
             (replace-match (concat "#+date:       " date-str)))
           (save-buffer)
           (denote-rename-file-using-front-matter path)
           (buffer-file-name)))))))

(defun denote-mcp--replace-links (old-id new-id &optional files)
  "Replace occurrences of OLD-ID with NEW-ID across denote files."
  (let* ((denote-dir (denote-mcp-get-directory))
         (target-files (or files
                           (directory-files denote-dir t "\\`[0-9]\\{8\\}T[0-9]\\{6\\}.*\\.\\(org\\|md\\)\\'")))
         (count 0))
    (denote-mcp-silently
     (dolist (path target-files)
       (when (file-exists-p path)
         (with-current-buffer (find-file-noselect path)
           (goto-char (point-min))
           (while (search-forward old-id nil t)
             (replace-match new-id t t)
             (cl-incf count))
           (when (buffer-modified-p)
             (when (derived-mode-p 'org-mode)
               (ignore-errors (org-element-cache-reset)))
             (save-buffer))))))
    count))

(defun denote-mcp--sync-frontmatter (&optional file)
  "Sync #+identifier: and #+signature: frontmatter to match filenames.
If FILE is given, sync only FILE; otherwise sync all notes in denote directory.
Returns a list of change descriptions."
  (let* ((denote-dir (denote-mcp-get-directory))
         (files (if file
                    (list (denote-mcp--find-file file))
                  (directory-files denote-dir t "\\`[0-9]\\{8\\}T[0-9]\\{6\\}.*\\.\\(org\\|md\\)\\'")))
         (changes nil))
    (denote-mcp-silently
     (dolist (f files)
       (when (and f (file-exists-p f))
         (let* ((base (file-name-nondirectory f))
                (parsed (denote-mcp--parse-filename f))
                (want-id (plist-get parsed :id))
                (want-sig (plist-get parsed :seq)))
           (when want-id
             (with-current-buffer (find-file-noselect f)
               (let ((file-changed nil))
                 ;; Check identifier
                 (goto-char (point-min))
                 (if (re-search-forward "^#\\+identifier:[ \t]*\\(.*\\)$" nil t)
                     (let ((cur-id (string-trim (match-string 1))))
                       (unless (equal cur-id want-id)
                         (replace-match (concat "#+identifier: " want-id))
                         (push (format "%s: identifier %s -> %s" base cur-id want-id) changes)
                         (setq file-changed t)))
                   ;; Missing identifier
                   (goto-char (point-min))
                   (insert "#+identifier: " want-id "\n")
                   (push (format "%s: inserted identifier %s" base want-id) changes)
                   (setq file-changed t))

                 ;; Check signature
                 (goto-char (point-min))
                 (let ((has-sig (re-search-forward "^#\\+signature:[ \t]*\\(.*\\)$" nil t)))
                   (cond
                    (want-sig
                     (if has-sig
                         (let ((cur-sig (string-trim (match-string 1))))
                           (unless (equal cur-sig want-sig)
                             (replace-match (concat "#+signature:  " want-sig))
                             (push (format "%s: signature %s -> %s" base cur-sig want-sig) changes)
                             (setq file-changed t)))
                       (goto-char (point-min))
                       (if (re-search-forward "^#\\+identifier:.*$" nil t)
                           (progn (end-of-line) (insert "\n#+signature:  " want-sig))
                         (insert "#+signature:  " want-sig "\n"))
                       (push (format "%s: inserted signature %s" base want-sig) changes)
                       (setq file-changed t)))
                    ((and (not want-sig) has-sig)
                     (delete-region (line-beginning-position) (min (point-max) (1+ (line-end-position))))
                     (push (format "%s: removed obsolete signature" base) changes)
                     (setq file-changed t))))

                 (when file-changed
                   (when (derived-mode-p 'org-mode)
                     (ignore-errors (org-element-cache-reset)))
                   (save-buffer)))))))))
    (nreverse changes)))

;;; Service Definition & Tool Registration

(defvar denote-mcp-service nil
  "The Denote `mcpkit-service' instance.")

;;;###autoload
(defun denote-mcp-register ()
  "Define the `denote' MCP service and register all Denote tools."
  (interactive)
  (let ((svc (or (mcpkit-get-service 'denote)
                 (mcpkit-define-service 'denote
                   :port denote-mcp-port
                   :description "Denote Note-Taking and Knowledge Base Management"))))
    (setq denote-mcp-service svc)

    ;; 1. denote_find
    (mcpkit-register-tool denote_find svc
      :description "Find notes matching a regex across title, keywords, identifier, and sequence."
      :input-schema '(:type "object"
                      :properties (:query (:type "string" :description "Search regex")
                                   :max_results (:type "integer" :description "Max results (default 50)"))
                      :required ["query"])
      (let* ((query (plist-get args :query))
             (max-count (or (plist-get args :max_results) 50))
             (denote-dir (denote-mcp-get-directory))
             (journal-dir (expand-file-name "journal/" denote-dir))
             (files (append (directory-files denote-dir t query)
                            (when (file-directory-p journal-dir)
                              (directory-files journal-dir t query))))
             (results nil))
        (dolist (f (seq-take files max-count))
          (when (file-regular-p f)
            (let ((parsed (denote-mcp--parse-filename f)))
              (when (plist-get parsed :id)
                (push (list :id (plist-get parsed :id)
                            :sequence (plist-get parsed :seq)
                            :title (or (denote-retrieve-title-value f (denote-filetype-heuristics f))
                                       (plist-get parsed :title))
                            :keywords (or (denote-retrieve-filename-keywords-as-list f) [])
                            :path f)
                      results)))))
        (nreverse results)))

    ;; 2. denote_find_by_slug
    (mcpkit-register-tool denote_find_by_slug svc
      :description "Look up a note by title slug or identifier fragment."
      :input-schema '(:type "object"
                      :properties (:slug (:type "string" :description "Unique title slug or identifier fragment"))
                      :required ["slug"])
      (let* ((slug (plist-get args :slug))
             (file (denote-mcp--find-file slug)))
        (if (not file)
            (list :found :json-false :slug slug)
          (let ((parsed (denote-mcp--parse-filename file)))
            (list :found t
                  :path file
                  :id (plist-get parsed :id)
                  :sequence (plist-get parsed :seq)
                  :title (or (denote-retrieve-title-value file (denote-filetype-heuristics file))
                             (plist-get parsed :title))
                  :keywords (or (denote-retrieve-filename-keywords-as-list file) []))))))

    ;; 3. denote_find_most_recent
    (mcpkit-register-tool denote_find_most_recent svc
      :description "Find the most recently authored note matching all specified tags (e.g. [\"agent\", \"plan\"])."
      :input-schema '(:type "object"
                      :properties (:tags (:type "array" :items (:type "string") :description "List of required tags")))
      (let* ((tags (plist-get args :tags))
             (denote-dir (denote-mcp-get-directory))
             (journal-dir (expand-file-name "journal/" denote-dir))
             (files (append (directory-files denote-dir t "\\`[0-9]\\{8\\}T[0-9]\\{6\\}")
                            (when (file-directory-p journal-dir)
                              (directory-files journal-dir t "\\`[0-9]\\{8\\}T[0-9]\\{6\\}"))))
             (matches
              (cl-remove-if-not
               (lambda (f)
                 (or (null tags)
                     (let ((filetags (denote-retrieve-filename-keywords-as-list f)))
                       (cl-every (lambda (tag) (member tag filetags)) tags))))
               files))
             (sorted (sort matches (lambda (a b) (string> (file-name-nondirectory a)
                                                          (file-name-nondirectory b)))))
             (best (car sorted)))
        (if (not best)
            (list :found :json-false)
          (let ((parsed (denote-mcp--parse-filename best)))
            (list :found t
                  :path best
                  :id (plist-get parsed :id)
                  :sequence (plist-get parsed :seq)
                  :title (denote-retrieve-title-value best (denote-filetype-heuristics best))
                  :keywords (or (denote-retrieve-filename-keywords-as-list best) []))))))

    ;; 4. denote_get_metadata
    (mcpkit-register-tool denote_get_metadata svc
      :description "Retrieve structured metadata (id, sequence, title, tags, date, execution_status) for a note."
      :input-schema '(:type "object"
                      :properties (:file_or_slug (:type "string" :description "File path or slug fragment"))
                      :required ["file_or_slug"])
      (let* ((target (plist-get args :file_or_slug))
             (file (denote-mcp--find-file target)))
        (unless file
          (error "Note not found for: %s" target))
        (denote-mcp--get-metadata file)))

    ;; 5. denote_read_note
    (mcpkit-register-tool denote_read_note svc
      :description "Safely read the text content of a denote note with UTF-8 encoding."
      :input-schema '(:type "object"
                      :properties (:file_or_slug (:type "string" :description "File path or slug fragment")
                                   :max_bytes (:type "integer" :description "Optional max bytes to read"))
                      :required ["file_or_slug"])
      (let* ((target (plist-get args :file_or_slug))
             (file (denote-mcp--find-file target))
             (max-b (plist-get args :max_bytes)))
        (unless file
          (error "Note not found for: %s" target))
        (denote-mcp-silently
         (with-temp-buffer
           (insert-file-contents file nil 0 max-b)
           (list :path file
                 :content (buffer-string)
                 :truncated (if (and max-b (> (file-attribute-size (file-attributes file)) max-b))
                                t :json-false))))))

    ;; 6. denote_create_note
    (mcpkit-register-tool denote_create_note svc
      :description "Create a new denote note with optional Folgezettel sequence and initial body."
      :input-schema '(:type "object"
                      :properties (:title (:type "string" :description "Title of the note")
                                   :keywords (:type "array" :items (:type "string") :description "List of tag strings")
                                   :sequence (:type "string" :description "Optional Folgezettel sequence (e.g. '3a1')")
                                   :content (:type "string" :description "Initial note content body")
                                   :file_type (:type "string" :enum ["org" "markdown" "text"] :description "File type (default org)")
                                   :date (:type "string" :description "Optional date string YYYY-MM-DD"))
                      :required ["title"])
      (let* ((title (plist-get args :title))
             (keywords (plist-get args :keywords))
             (seq (plist-get args :sequence))
             (content (plist-get args :content))
             (file-type (intern (or (plist-get args :file_type) "org")))
             (date-str (plist-get args :date))
             (date (when date-str (date-to-time (concat date-str " 00:00:00"))))
             (denote-dir (denote-mcp-get-directory)))
        (denote-mcp-silently
         (let ((file (save-window-excursion
                       (denote title keywords file-type denote-dir date))))
           (unless file (error "denote returned nil for %s" title))
           ;; Save initial content
           (with-current-buffer (find-file-noselect file)
             (when content
               (goto-char (point-max))
               (unless (bolp) (insert "\n"))
               (insert content))
             (basic-save-buffer))
           ;; Apply sequence if requested
           (when (and seq (not (string-empty-p seq)))
             (setq file (denote-mcp--set-sequence file seq)))
           (let ((meta (denote-mcp--get-metadata file)))
             (list :path file
                   :id (plist-get meta :id)
                   :sequence (plist-get meta :signature)
                   :title (plist-get meta :title)
                   :keywords (plist-get meta :filetags)))))))

    ;; 7. denote_append_body
    (mcpkit-register-tool denote_append_body svc
      :description "Append text content safely to an existing note without coding-system prompts."
      :input-schema '(:type "object"
                      :properties (:file_or_slug (:type "string" :description "File path or slug fragment")
                                   :content (:type "string" :description "Text content to append"))
                      :required ["file_or_slug" "content"])
      (let* ((target (plist-get args :file_or_slug))
             (content (plist-get args :content))
             (file (denote-mcp--find-file target)))
        (unless file (error "Note not found for: %s" target))
        (denote-mcp-silently
         (with-current-buffer (find-file-noselect file)
           (goto-char (point-max))
           (unless (bolp) (insert "\n"))
           (insert content)
           (when (derived-mode-p 'org-mode)
             (ignore-errors (org-element-cache-reset)))
           (basic-save-buffer)
           (list :path file :bytes_appended (length content))))))

    ;; 8. denote_rename
    (mcpkit-register-tool denote_rename svc
      :description "Update title and/or keywords of a note, updating frontmatter and renaming on disk."
      :input-schema '(:type "object"
                      :properties (:file_or_slug (:type "string" :description "File path or slug fragment")
                                   :new_title (:type "string" :description "New title string (or omit to keep)")
                                   :new_keywords (:type "array" :items (:type "string") :description "New keywords list"))
                      :required ["file_or_slug"])
      (let* ((target (plist-get args :file_or_slug))
             (new-title (plist-get args :new_title))
             (new-kws (plist-get args :new_keywords))
             (file (denote-mcp--find-file target)))
        (unless file (error "Note not found for: %s" target))
        (denote-mcp-silently
         (let* ((denote-save-buffers t)
                (new-path (denote-rename-file file
                                             (or new-title 'keep-current)
                                             (or new-kws 'keep-current)
                                             'keep-current
                                             'keep-current
                                             'keep-current)))
           (list :old_path file
                 :new_path new-path
                 :title (or new-title (denote-retrieve-title-value new-path (denote-filetype-heuristics new-path)))
                 :keywords (or (denote-retrieve-filename-keywords-as-list new-path) []))))))

    ;; 9. denote_mark_executed
    (mcpkit-register-tool denote_mark_executed svc
      :description "Mark a plan note as executed: applies 'x' filetag, cleans 'done', and updates execution status."
      :input-schema '(:type "object"
                      :properties (:file_or_slug (:type "string" :description "File path or slug fragment"))
                      :required ["file_or_slug"])
      (let* ((target (plist-get args :file_or_slug))
             (file (denote-mcp--find-file target)))
        (unless file (error "Note not found for: %s" target))
        (denote-mcp-silently
         (let* ((kw denote-mcp-executed-keyword)
                (raw (denote-retrieve-filename-keywords-as-list file)))
           (if (and (member kw raw) (not (member "done" raw)))
               (list :path file :status "already_marked")
             (let* ((clean (seq-remove (lambda (k) (member k '("done" "x"))) raw))
                    (new-kws (cons kw clean)))
               (with-current-buffer (find-file-noselect file)
                 (goto-char (point-min))
                 (when (re-search-forward "^#\\+filetags:.*" nil t)
                   (replace-match (format "#+filetags:   :%s:" (mapconcat #'identity new-kws ":"))))
                 (goto-char (point-min))
                 (when (re-search-forward "^#\\+execution_status:[ \t]*.*$" nil t)
                   (replace-match "#+execution_status: complete"))
                 (when (derived-mode-p 'org-mode)
                   (ignore-errors (org-element-cache-reset)))
                 (save-buffer))
               (let ((new-path (denote-rename-file file 'keep-current new-kws 'keep-current 'keep-current 'keep-current)))
                 (list :path new-path :status "marked" :keywords new-kws))))))))

    ;; 10. denote_sync_frontmatter
    (mcpkit-register-tool denote_sync_frontmatter svc
      :description "Synchronize note frontmatter (#+identifier:, #+signature:) to match filenames."
      :input-schema '(:type "object"
                      :properties (:file_or_slug (:type "string" :description "Optional single note to sync")))
      (let* ((target (plist-get args :file_or_slug))
             (changes (denote-mcp--sync-frontmatter target)))
        (list :changes_count (length changes)
              :changes (vconcat changes))))

    ;; 11. denote_seq_get_next
    (mcpkit-register-tool denote_seq_get_next svc
      :description "Calculate the next available child or sibling sequence for a given parent/sibling sequence."
      :input-schema '(:type "object"
                      :properties (:target_sequence (:type "string" :description "Sequence string, e.g. '3a1'"))
                      :required ["target_sequence"])
      (let* ((seq (plist-get args :target_sequence)))
        (unless (featurep 'denote-sequence)
          (require 'denote-sequence nil t))
        (let* ((has-children
                (cl-some (lambda (s) (and (not (string= s seq)) (string-prefix-p seq s)))
                         (denote-sequence-get-all-sequences-with-prefix seq)))
               (next-seq (if has-children
                             (denote-sequence-get-new 'child seq)
                           (if (< (denote-sequence-depth seq) 3)
                               (denote-sequence-get-new 'child seq)
                             (denote-sequence-get-new 'sibling seq))))
               (rel (if (string-prefix-p seq next-seq) "child" "sibling")))
          (list :target_sequence seq
                :next_sequence next-seq
                :relation rel))))

    ;; 12. denote_seq_set
    (mcpkit-register-tool denote_seq_set svc
      :description "Assign sequence signature to a note and rename file to match."
      :input-schema '(:type "object"
                      :properties (:file_or_slug (:type "string" :description "File path or slug fragment")
                                   :sequence (:type "string" :description "New sequence signature (e.g. '3a1b')"))
                      :required ["file_or_slug" "sequence"])
      (let* ((target (plist-get args :file_or_slug))
             (seq (plist-get args :sequence))
             (new-path (denote-mcp--set-sequence target seq)))
        (list :path new-path :sequence seq)))

    ;; 13. denote_seq_graft
    (mcpkit-register-tool denote_seq_graft svc
      :description "Batch reassign sequences safely ordered in reverse depth to avoid slot collisions."
      :input-schema '(:type "object"
                      :properties (:pairs (:type "array"
                                           :items (:type "object"
                                                   :properties (:slug (:type "string")
                                                                :new_sequence (:type "string"))
                                                   :required ["slug" "new_sequence"])))
                      :required ["pairs"])
      (let* ((pairs (plist-get args :pairs))
             (resolved
              (cl-loop for pair across pairs
                       for slug = (plist-get pair :slug)
                       for new-seq = (plist-get pair :new_sequence)
                       for file = (denote-mcp--find-file slug)
                       for cur-seq = (when file
                                       (plist-get (denote-mcp--parse-filename file) :seq))
                       collect (list :file file :cur_seq cur-seq :new_seq new-seq :slug slug)))
             (sorted (sort resolved
                           (lambda (a b)
                             (string> (or (plist-get a :cur_seq) "")
                                      (or (plist-get b :cur_seq) "")))))
             (results nil))
        (dolist (entry sorted)
          (let ((f (plist-get entry :file))
                (nseq (plist-get entry :new_seq))
                (slug (plist-get entry :slug)))
            (if f
                (let ((new-file (denote-mcp--set-sequence f nseq)))
                  (push (list :slug slug :new_path new-file :sequence nseq) results))
              (push (list :slug slug :error "Slug not found") results))))
        (list :modified (vconcat (nreverse results)))))

    ;; 14. denote_seq_reparent
    (mcpkit-register-tool denote_seq_reparent svc
      :description "Reparent a note (or subtree) under a new parent note."
      :input-schema '(:type "object"
                      :properties (:source_slug (:type "string" :description "Source note slug")
                                   :new_parent_slug (:type "string" :description "New parent note slug")
                                   :recursive (:type "boolean" :description "Whether to reparent child notes recursively"))
                      :required ["source_slug" "new_parent_slug"])
      (let* ((src (plist-get args :source_slug))
             (parent (plist-get args :new_parent_slug))
             (recursive (plist-get args :recursive))
             (src-file (denote-mcp--find-file src))
             (parent-file (denote-mcp--find-file parent)))
        (unless src-file (error "Source note not found: %s" src))
        (unless parent-file (error "Parent note not found: %s" parent))
        (denote-mcp-silently
         (let ((denote-sequence-scheme 'alphanumeric))
           (if (and recursive (fboundp 'denote-dash-reparent-recursive))
               (progn
                 (denote-dash-reparent-recursive src-file parent-file)
                 (list :status "reparented_recursive" :source src-file :new_parent parent-file))
             (let* ((parent-seq (plist-get (denote-mcp--parse-filename parent-file) :seq))
                    (new-path (denote-sequence-reparent src-file parent-seq)))
               (list :status "reparented" :source (or new-path src-file) :parent_sequence parent-seq)))))))

    ;; 15. denote_seq_tree
    (mcpkit-register-tool denote_seq_tree svc
      :description "List all notes organized in Folgezettel sequence hierarchy."
      :input-schema '(:type "object"
                      :properties (:root_sequence (:type "string" :description "Optional root sequence prefix")))
      (let* ((root (plist-get args :root_sequence))
             (denote-dir (denote-mcp-get-directory))
             (files (directory-files denote-dir t "\\`[0-9]\\{8\\}T[0-9]\\{6\\}==.*\\.\\(org\\|md\\)\\'"))
             (notes nil))
        (dolist (f files)
          (let ((parsed (denote-mcp--parse-filename f)))
            (when-let* ((seq (plist-get parsed :seq)))
              (when (or (null root) (string-prefix-p root seq))
                (push (list :sequence seq
                            :id (plist-get parsed :id)
                            :title (or (denote-retrieve-title-value f (denote-filetype-heuristics f))
                                       (plist-get parsed :title))
                            :path f)
                      notes)))))
        (let ((sorted (sort notes (lambda (a b) (string< (plist-get a :sequence)
                                                         (plist-get b :sequence))))))
          (list :count (length sorted) :notes (vconcat sorted)))))

    ;; 16. denote_verify
    (mcpkit-register-tool denote_verify svc
      :description "Validate sequence integrity, check for missing parent stubs, and report all sequences."
      :input-schema '(:type "object"
                      :properties (:quiet (:type "boolean" :description "If true, skip full sequence list")))
      (let* ((changes (denote-mcp--sync-frontmatter))
             (denote-dir (denote-mcp-get-directory))
             (files (directory-files denote-dir nil "^[0-9]\\{8\\}T[0-9]\\{6\\}.*\\.\\(org\\|md\\)$"))
             (seqs (sort (cl-loop for f in files
                                  when (string-match "==\\([^-]+\\)--" f)
                                  collect (match-string 1 f))
                         #'string<))
             (missing nil))
        ;; Check for implied parent gaps
        (dolist (s seqs)
          (when (> (length s) 1)
            (let ((parent-seq (substring s 0 (1- (length s)))))
              (unless (member parent-seq seqs)
                (push (format "Missing parent sequence: %s (implied by %s)" parent-seq s) missing)))))
        (list :valid (if missing :json-false t)
              :sequenced_count (length seqs)
              :missing_parents (vconcat (nreverse missing))
              :frontmatter_changes (vconcat changes)
              :sequences (if (plist-get args :quiet) [] (vconcat seqs)))))

    ;; 17. denote_link_string
    (mcpkit-register-tool denote_link_string svc
      :description "Construct a canonical [[denote:ID][Title]] link without sequence signatures."
      :input-schema '(:type "object"
                      :properties (:target_slug (:type "string" :description "Target note slug or identifier")
                                   :link_text (:type "string" :description "Optional override link display text"))
                      :required ["target_slug"])
      (let* ((target (plist-get args :target_slug))
             (file (denote-mcp--find-file target)))
        (unless file (error "Target note not found: %s" target))
        (let* ((parsed (denote-mcp--parse-filename file))
               (id (plist-get parsed :id))
               (title (or (plist-get args :link_text)
                          (denote-retrieve-title-value file (denote-filetype-heuristics file))
                          (plist-get parsed :title))))
          (list :link (format "[[denote:%s][%s]]" id title)
                :id id
                :title title))))

    ;; 18. denote_insert_dblock
    (mcpkit-register-tool denote_insert_dblock svc
      :description "Insert and update an Org dynamic block (denote-backlinks, denote-links, denote-sequence) in a note."
      :input-schema '(:type "object"
                      :properties (:file_or_slug (:type "string" :description "Target note slug or file")
                                   :block_type (:type "string" :description "Block type: denote-backlinks, denote-links, denote-sequence")
                                   :params (:type "object" :description "Parameters plist or key-value map"))
                      :required ["file_or_slug" "block_type"])
      (let* ((target (plist-get args :file_or_slug))
             (btype (plist-get args :block_type))
             (params (plist-get args :params))
             (file (denote-mcp--find-file target)))
        (unless file (error "Target note not found: %s" target))
        (denote-mcp-silently
         (with-current-buffer (find-file-noselect file)
           (goto-char (point-max))
           (unless (bolp) (insert "\n"))
           (insert "\n#+BEGIN: " btype)
           (when (listp params)
             (cl-loop for (k v) on params by #'cddr do
                      (insert " " (if (keywordp k) (substring (symbol-name k) 1) (symbol-name k)) " "
                              (cond ((stringp v) (concat "\"" v "\""))
                                    ((null v) "nil")
                                    (t (format "%s" v))))))
           (insert "\n#+END:\n")
           (re-search-backward "^#\\+BEGIN:" nil t)
           (org-dblock-update)
           (save-buffer)
           (list :path file :block_type btype :updated t)))))

    ;; 19. denote_redate
    (mcpkit-register-tool denote_redate svc
      :description "Backdate note identifier and rewrite all links repo-wide in one call."
      :input-schema '(:type "object"
                      :properties (:file_or_slug (:type "string" :description "Target note slug or file")
                                   :new_identifier (:type "string" :description "New identifier YYYYMMDDTHHMMSS"))
                      :required ["file_or_slug" "new_identifier"])
      (let* ((target (plist-get args :file_or_slug))
             (new-id (plist-get args :new_identifier))
             (file (denote-mcp--find-file target)))
        (unless file (error "Target note not found: %s" target))
        (let* ((parsed (denote-mcp--parse-filename file))
               (old-id (plist-get parsed :id))
               (new-path (denote-mcp--set-identifier file new-id))
               (links-fixed 0))
          (when (and new-path old-id (not (string= old-id new-id)))
            (setq links-fixed (denote-mcp--replace-links old-id new-id)))
          (list :new_path new-path
                :old_identifier old-id
                :new_identifier new-id
                :links_updated links-fixed))))

    svc))

(provide 'denote-mcp)
;;; denote-mcp.el ends here
