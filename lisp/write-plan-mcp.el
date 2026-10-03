;;; write-plan-mcp.el --- Write-Plan MCP service integration for mcpkit -*- lexical-binding: t; -*-

;; Author: Tychoish
;; Keywords: tools, mcp, denote, planning

;;; Commentary:
;;
;; Provides Model Context Protocol (MCP) tools for creating denote plan and
;; report notes, linking TODO tasks in agentic.org, and answering questions
;; in plan notes via mcpkit.
;;
;; Tools are registered at top-level on the `write-plan' service upon loading.
;; Services are started only when explicitly requested via `mcpkit-start-service'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'denote)
(require 'mcpkit)
(require 'org-capture-mcp nil t)

(defgroup write-plan-mcp nil
  "Write-plan MCP service integration."
  :group 'mcpkit
  :prefix "write-plan-mcp-")

(defcustom write-plan-mcp-port 8765
  "Default TCP port for the write-plan MCP service."
  :type 'integer
  :group 'write-plan-mcp)

;;; Service Definition & Top-Level Tool Registration

(defvar write-plan-mcp-service
  (or (mcpkit-get-service 'write-plan)
      (mcpkit-define-service 'write-plan
        :port write-plan-mcp-port
        :description "Plan and Report Note Creation and Lifecycle Management"))
  "The write-plan `mcpkit-service' instance.")

(defun write-plan-mcp--directory ()
  "Return denote directory for plan notes."
  (cond
   ((and (boundp 'denote-directory) (listp denote-directory))
    (file-name-as-directory (expand-file-name (car denote-directory))))
   ((and (boundp 'denote-directory) (stringp denote-directory))
    (file-name-as-directory (expand-file-name denote-directory)))
   ((fboundp 'denote-directory)
    (file-name-as-directory (expand-file-name (denote-directory))))
   (t (file-name-as-directory (expand-file-name "~/denote/")))))

(defun write-plan-mcp--extract-id (file-path)
  "Extract denote ID from FILE-PATH."
  (let ((base (file-name-nondirectory file-path)))
    (when (string-match "\\([0-9]\\{8\\}T[0-9]\\{6\\}\\)" base)
      (match-string 1 base))))

;; 1. write_plan_create_plan
(mcpkit-register-tool 'write_plan_create_plan 'write-plan
  :description "Create a new denote plan note with initial frontmatter, insert content, and capture TODO to agentic.org."
  :input-schema '(:type "object"
                  :properties (:title (:type "string" :description "Title of the plan note")
                               :content (:type "string" :description "Markdown or Org content body for the plan")
                               :file_type (:type "string" :description "File type: 'org' (default) or 'markdown-yaml'")
                               :sequence_or_parent (:type "string" :description "Optional Folgezettel sequence or parent note"))
                  :required ["title" "content"])
  (let* ((title (plist-get args :title))
         (content (plist-get args :content))
         (ft-str (plist-get args :file_type))
         (ft (if (and ft-str (string-prefix-p "markdown" ft-str)) 'markdown-yaml 'org))
         (seq-parent (plist-get args :sequence_or_parent))
         (denote-directory (write-plan-mcp--directory))
         (denote-templates (cons '(plan . "#+execution_status: proposed\n#+started:\n#+finished:\n\n")
                                 (assq-delete-all 'plan denote-templates))))
    (save-window-excursion
      (denote title '("agent" "plan") ft nil nil 'plan)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert content)
      (save-buffer)
      (let* ((file-path (buffer-file-name))
             (id (write-plan-mcp--extract-id file-path)))
        ;; If sequence requested and denote-sequence loaded, sequence it
        (when (and seq-parent (fboundp 'dso-set))
          (ignore-errors
            (setq file-path (dso-set file-path seq-parent))))
        ;; Capture TODO link to agentic.org
        (when (and file-path (file-exists-p file-path))
          (ignore-errors
            (let ((link-str (format "[[denote:%s][%s]]" (or id (file-name-base file-path)) title)))
              (when (fboundp 'org-capture-mcp--run)
                (org-capture-mcp--run (org-capture-mcp--target '("att" "at")) (concat "TODO " link-str) nil)))))
        (list :status "ok"
              :path file-path
              :id (or id "")
              :title title)))))

;; 2. write_plan_create_report
(mcpkit-register-tool 'write_plan_create_report 'write-plan
  :description "Create a new denote report note, insert content, and save."
  :input-schema '(:type "object"
                  :properties (:title (:type "string" :description "Title of the report note")
                               :content (:type "string" :description "Content body of the report")
                               :file_type (:type "string" :description "File type: 'org' (default) or 'markdown-yaml'")
                               :sequence_or_parent (:type "string" :description "Optional Folgezettel sequence or parent note"))
                  :required ["title" "content"])
  (let* ((title (plist-get args :title))
         (content (plist-get args :content))
         (ft-str (plist-get args :file_type))
         (ft (if (and ft-str (string-prefix-p "markdown" ft-str)) 'markdown-yaml 'org))
         (seq-parent (plist-get args :sequence_or_parent))
         (denote-directory (write-plan-mcp--directory)))
    (save-window-excursion
      (denote title '("agent" "report") ft nil nil nil)
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert content)
      (save-buffer)
      (let* ((file-path (buffer-file-name))
             (id (write-plan-mcp--extract-id file-path)))
        (when (and seq-parent (fboundp 'dso-set))
          (ignore-errors
            (setq file-path (dso-set file-path seq-parent))))
        (list :status "ok"
              :path file-path
              :id (or id "")
              :title title)))))

;; 3. write_plan_answer_question
(mcpkit-register-tool 'write_plan_answer_question 'write-plan
  :description "Mark a question heading as answered in a plan note, logging response and state transition in :LOGBOOK:."
  :input-schema '(:type "object"
                  :properties (:file_or_slug (:type "string" :description "Plan note file path, denote ID, or title slug")
                               :question_title (:type "string" :description "Text of question heading to match")
                               :answer_text (:type "string" :description "Answer content to record")
                               :new_state (:type "string" :description "Optional new todo keyword (default 'ANSWERED')"))
                  :required ["file_or_slug" "question_title" "answer_text"])
  (let* ((target (plist-get args :file_or_slug))
         (question-title (plist-get args :question_title))
         (answer-text (plist-get args :answer_text))
         (state (or (plist-get args :new_state) "ANSWERED"))
         (file (cond
                ((file-exists-p target) target)
                ((fboundp 'denote-get-path-by-id) (denote-get-path-by-id target))
                (t (let ((files (directory-files (write-plan-mcp--directory) t (regexp-quote target))))
                     (car-safe files))))))
    (unless (and file (file-exists-p file))
      (user-error "Could not find plan note for target %s" target))
    (with-current-buffer (find-file-noselect file)
      (save-excursion
        (goto-char (point-min))
        (unless (re-search-forward (concat "^\\*+\\s-+\\(?:QUESTION\\|TODO\\|INPROGRESS\\|BLOCKED\\)?\\s-*"
                                           (regexp-quote question-title))
                                   nil t)
          (user-error "Question heading %s not found in %s" question-title file))
        (org-back-to-heading t)
        (when (looking-at "^\\(\\*+\\)\\s-+\\(?:[A-Z]+\\s-+\\)?\\(.*\\)$")
          (replace-match (concat "\\1 " state " \\2")))
        (let* ((timestamp (format-time-string "[%Y-%m-%d %a %H:%M]"))
               (log-entry (format "- State \"%-12s\" from \"QUESTION\"   %s \\\\\n  %s\n"
                                  state timestamp answer-text))
               (drawer-pos (save-excursion
                             (forward-line 1)
                             (if (looking-at-p "^:LOGBOOK:")
                                 (point)
                               (let ((bound (save-excursion (outline-next-heading) (point))))
                                 (when (re-search-forward "^:LOGBOOK:" bound t)
                                   (match-beginning 0)))))))
          (if drawer-pos
              (save-excursion
                (goto-char drawer-pos)
                (forward-line 1)
                (insert log-entry))
            (save-excursion
              (forward-line 1)
              (insert ":LOGBOOK:\n" log-entry ":END:\n")))))
      (save-buffer)
      (list :status "ok"
            :file file
            :question question-title
            :state state))))

;;;###autoload
(defun write-plan-mcp-register ()
  "Ensure `write-plan-mcp-service' is registered in `mcpkit-registry' and return it."
  (interactive)
  (unless (mcpkit-get-service 'write-plan)
    (mcpkit-register-service write-plan-mcp-service))
  write-plan-mcp-service)

(provide 'write-plan-mcp)
;;; write-plan-mcp.el ends here
