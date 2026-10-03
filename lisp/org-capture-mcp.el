;;; org-capture-mcp.el --- Org-capture MCP service integration for mcpkit -*- lexical-binding: t; -*-

;; Author: Tychoish
;; Keywords: tools, mcp, org, capture

;;; Commentary:
;;
;; Provides Model Context Protocol (MCP) tools for interacting with Org-mode
;; capture targets (tasks, journal, clock-in) via mcpkit.
;;
;; Tools are registered at top-level on the `org-capture' service upon loading.
;; Services are started only when explicitly requested via `mcpkit-start-service'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'org-capture)
(require 'mcpkit)

(defgroup org-capture-mcp nil
  "Org-capture MCP service integration."
  :group 'mcpkit
  :prefix "org-capture-mcp-")

(defcustom org-capture-mcp-port 8765
  "Default TCP port for the org-capture MCP service."
  :type 'integer
  :group 'org-capture-mcp)

;;; Service Definition & Top-Level Tool Registration

(defvar org-capture-mcp-service
  (or (mcpkit-get-service 'org-capture)
      (mcpkit-define-service 'org-capture
        :port org-capture-mcp-port
        :description "Org Capture Management and Integration"))
  "The org-capture `mcpkit-service' instance.")

(defun org-capture-mcp--target (keys)
  "Return target spec matching any of KEYS in `org-capture-templates'."
  (if-let* ((tpl (seq-some (lambda (k) (assoc k org-capture-templates)) keys)))
      (nth 3 tpl)
    (if-let* ((default-entry (assoc "t" org-capture-templates)))
        (nth 3 default-entry)
      (list 'file (expand-file-name "~/org/agentic.org")))))

(defun org-capture-mcp--run (target heading body-str &optional clock-in-p)
  "Insert HEADING and BODY-STR at TARGET via `org-capture'."
  (let* ((timestamp (format-time-string "[%Y-%m-%d %a %H:%M]"))
         (content (concat "** " heading "\n"
                          ":PROPERTIES:\n:CREATED:  " timestamp "\n:END:\n"
                          (if (and body-str (not (string-empty-p body-str)))
                              (concat body-str "\n")
                            "")))
         (org-capture-templates
          `(("_" "" entry ,target ,content
             :immediate-finish t :kill-buffer t :prepend t
             :clock-in ,(if clock-in-p t nil)))))
    (save-window-excursion
      (org-capture nil "_"))))

;; 1. org_capture_task
(mcpkit-register-tool 'org_capture_task 'org-capture
  :description "Capture a task heading and optional body to the Tasks section of agentic.org."
  :input-schema '(:type "object"
                  :properties (:heading (:type "string" :description "Task heading (with or without TODO)")
                               :body (:type "string" :description "Optional task body text"))
                  :required ["heading"])
  (let* ((raw-heading (plist-get args :heading))
         (heading (if (string-prefix-p "TODO " raw-heading) raw-heading (concat "TODO " raw-heading)))
         (body (plist-get args :body))
         (target (org-capture-mcp--target '("att" "at"))))
    (org-capture-mcp--run target heading body)
    (list :status "ok" :target "task" :heading heading)))

;; 2. org_capture_journal
(mcpkit-register-tool 'org_capture_journal 'org-capture
  :description "Capture a journal entry heading and optional body to today's Journal datetree in agentic.org."
  :input-schema '(:type "object"
                  :properties (:heading (:type "string" :description "Journal entry heading")
                               :body (:type "string" :description "Optional journal entry body text"))
                  :required ["heading"])
  (let* ((heading (plist-get args :heading))
         (body (plist-get args :body))
         (target (org-capture-mcp--target '("ajj" "aj"))))
    (org-capture-mcp--run target heading body)
    (list :status "ok" :target "journal" :heading heading)))

;; 3. org_capture_clock_in
(mcpkit-register-tool 'org_capture_clock_in 'org-capture
  :description "Capture a task heading and body to agentic.org and immediately clock into it."
  :input-schema '(:type "object"
                  :properties (:heading (:type "string" :description "Task heading to clock in")
                               :body (:type "string" :description "Optional body text"))
                  :required ["heading"])
  (let* ((raw-heading (plist-get args :heading))
         (heading (if (string-prefix-p "TODO " raw-heading) raw-heading (concat "TODO " raw-heading)))
         (body (plist-get args :body))
         (target (org-capture-mcp--target '("att" "at"))))
    (org-capture-mcp--run target heading body t)
    (list :status "ok" :target "clock-in" :heading heading)))

;;;###autoload
(defun org-capture-mcp-register ()
  "Ensure `org-capture-mcp-service' is registered in `mcpkit-registry' and return it."
  (interactive)
  (unless (mcpkit-get-service 'org-capture)
    (mcpkit-register-service org-capture-mcp-service))
  org-capture-mcp-service)

(provide 'org-capture-mcp)
;;; org-capture-mcp.el ends here
