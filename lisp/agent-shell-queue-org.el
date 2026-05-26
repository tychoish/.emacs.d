;;; agent-shell-queue-org.el --- Org-mode storage format for agent-shell-queue -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Keywords: tools, agent-shell

;; This file is not part of GNU Emacs

;;; Commentary:
;; Org-mode serialization format for agent-shell-queue.
;;
;; Top-level headings are queue buckets (buffer names or "(unassigned)").
;; Second-level headings are individual items.  All metadata lives in a
;; property drawer managed by org-set-property; the heading title is the
;; first line of the prompt, truncated to 60 chars for human readability.
;; The full prompt is the subtree body with 3-space indentation so that
;; org structural punctuation inside the prompt remains inert.
;;
;; To activate:
;;   (require 'agent-shell-queue-org)
;;   (setq agent-shell-queue-serialization-format 'org)
;; Set the state file path to a .org path for clarity:
;;   (setq agent-shell-queue-state-file-function
;;         (lambda () (tychoish/conf-state-path "agent-shell-queue.org")))

;;; Code:

(require 'seq)
(require 'agent-shell-queue)
(require 'org)
(require 'org-element)

;;; Status / keyword mapping

(defconst agent-shell-queue-org--status-keyword
  '((active  . "TODO")
    (running . "DOING")
    (paused  . "WAIT")
    (blocked . "HOLD")
    (draft      . "DRAFT")
    (done       . "DONE")
    (aborted    . "ABORTED")
    (incomplete . "INCOMPLETE"))
  "Alist mapping queue status symbols to org TODO keyword strings.")

(defconst agent-shell-queue-org--keyword-status
  (seq-map (lambda (it) (cons (cdr it) (car it))) agent-shell-queue-org--status-keyword)
  "Reverse alist: org TODO keyword strings to queue status symbols.")

;;; Shared helpers

(defun agent-shell-queue-org--heading-title (prompt)
  "Return a short heading label from the first line of PROMPT."
  (let* ((first (car (split-string (or prompt "") "\n")))
         (trimmed (string-trim first)))
    (cond ((string-empty-p trimmed) "(empty)")
          ((> (length trimmed) 60) (concat (substring trimmed 0 57) "..."))
          (t trimmed))))

(defun agent-shell-queue-org--ts (val)
  "Return numeric VAL as a string, or \"nil\" when VAL is nil."
  (if (numberp val) (number-to-string val) "nil"))

(defun agent-shell-queue-org--parse-ts (str)
  "Parse a float timestamp from STR; return nil for \"nil\", blank, or missing."
  (and str
       (not (equal str "nil"))
       (not (string-empty-p (string-trim str)))
       (string-to-number str)))

;;; Serialization — org-insert based

(defun agent-shell-queue-org--insert-item (item)
  "Append a level-2 org subtree for ITEM at end of the current org buffer.
The buffer must already be in `org-mode'.  Uses `org-set-property' to
build the property drawer so org manages drawer formatting."
  (let* ((status (agent-shell-queue-item-status item))
         (keyword (or (cdr (assq status agent-shell-queue-org--status-keyword)) "TODO"))
         (title (agent-shell-queue-org--heading-title
                 (agent-shell-queue-item-prompt item)))
         (prompt (or (agent-shell-queue-item-prompt item) "")))
    (goto-char (point-max))
    (insert (format "** %s %s\n" keyword title))
    (org-set-property "QUEUE-ID"   (agent-shell-queue-item-id item))
    (org-set-property "STATUS"     (symbol-name status))
    (org-set-property "KIND"       (symbol-name (or (agent-shell-queue-item-kind item) 'prompt)))
    (org-set-property "BACKGROUND" (if (agent-shell-queue-item-background item) "t" "nil"))
    (org-set-property "CREATED"    (agent-shell-queue-org--ts
                                    (agent-shell-queue-item-created item)))
    (org-set-property "DISPATCHED" (agent-shell-queue-org--ts
                                    (agent-shell-queue-item-dispatched item)))
    (org-set-property "COMPLETED"  (agent-shell-queue-org--ts
                                    (agent-shell-queue-item-completed item)))
    (goto-char (point-max))
    (insert "\n")
    (dolist (line (split-string prompt "\n"))
      (insert "   " line "\n"))
    (insert "\n")))

(defun agent-shell-queue-org--serialize ()
  "Serialize `agent-shell-queue--items' to an org-mode string."
  (with-temp-buffer
    (org-mode)
    (insert "#+TITLE: Agent Shell Queue\n")
    (insert "#+STARTUP: overview\n")
    (insert "#+TODO: TODO DOING WAIT HOLD DRAFT | DONE ABORTED\n\n")
    (dolist (pair agent-shell-queue--items)
      (goto-char (point-max))
      (insert (format "* %s\n\n" (car pair)))
      (dolist (item (cdr pair))
        (agent-shell-queue-org--insert-item item)))
    (buffer-string)))

;;; Deserialization — org-element based

(defun agent-shell-queue-org--section-of (hl)
  "Return the section element that is a direct child of HL, or nil."
  (--first (eq (org-element-type it) 'section) (org-element-contents hl)))

(defun agent-shell-queue-org--drawer-of (section)
  "Return the property-drawer element that is a direct child of SECTION, or nil."
  (--first (eq (org-element-type it) 'property-drawer) (org-element-contents section)))

(defun agent-shell-queue-org--props-from-element (hl)
  "Return a string-keyed alist of property drawer entries from headline HL.
Uses direct child access rather than `org-element-map' to avoid recursion into
paragraph text nodes, which are strings in Emacs 29+ and would crash the walker."
  (let (result)
    (when-let* ((section (agent-shell-queue-org--section-of hl))
                (drawer (agent-shell-queue-org--drawer-of section)))
      (dolist (child (org-element-contents drawer))
        (when (eq (org-element-type child) 'node-property)
          (push (cons (org-element-property :key child)
                      (or (org-element-property :value child) ""))
                result))))
    result))

(defun agent-shell-queue-org--body-from-element (hl)
  "Extract the prompt body text from queue item headline HL.
Uses buffer positions from the parsed element, so must be called in the
same buffer where HL was parsed.  Strips 3-space indentation added by
`agent-shell-queue-org--insert-item'."
  (let* ((section (agent-shell-queue-org--section-of hl)))
    (if (not section)
        ""
      (let* ((drawer (agent-shell-queue-org--drawer-of section))
             (start (or (and drawer (org-element-property :end drawer))
                        (org-element-property :contents-begin section)))
             (end (org-element-property :contents-end section))
             (raw (if (and start end (< start end))
                      (buffer-substring-no-properties start end)
                    "")))
        (string-trim
         (mapconcat (lambda (line)
                      (if (string-prefix-p "   " line) (substring line 3) line))
                    (split-string raw "\n")
                    "\n"))))))

(defun agent-shell-queue-org--item-from-element (item-hl)
  "Build a queue item struct from level-2 org-element headline ITEM-HL.
Must be called in the buffer where ITEM-HL was parsed."
  (let* ((props (agent-shell-queue-org--props-from-element item-hl))
         (prop (lambda (k) (cdr (assoc k props))))
         (kw (or (org-element-property :todo-keyword item-hl) "TODO"))
         (status-str (funcall prop "STATUS"))
         (status (if status-str
                     (intern status-str)
                   (or (cdr (assoc kw agent-shell-queue-org--keyword-status))
                       'active))))
    (agent-shell-queue-item--make
     :id (or (funcall prop "QUEUE-ID") (agent-shell-queue--gen-id))
     :prompt (agent-shell-queue-org--body-from-element item-hl)
     :status status
     :kind (intern (or (funcall prop "KIND") "prompt"))
     :background (equal (funcall prop "BACKGROUND") "t")
     :created (agent-shell-queue-org--parse-ts (funcall prop "CREATED"))
     :dispatched (agent-shell-queue-org--parse-ts (funcall prop "DISPATCHED"))
     :completed (agent-shell-queue-org--parse-ts (funcall prop "COMPLETED")))))

(defun agent-shell-queue-org--headlines-of (element)
  "Return the direct headline children of ELEMENT as a list."
  (seq-filter (lambda (it) (eq (org-element-type it) 'headline)) (org-element-contents element)))

(defun agent-shell-queue-org--deserialize (str)
  "Deserialize STR (org format) into an items alist via org-element.
Level-1 headlines become bucket names; level-2 headlines become items.
Uses direct child access rather than `org-element-map' to avoid Emacs 29+
string-in-paragraph-contents issues during tree walking."
  (with-temp-buffer
    (insert str)
    (org-mode)
    (let ((tree (org-element-parse-buffer)))
      (-map (lambda (bucket-hl)
              (cons (string-trim (org-element-property :raw-value bucket-hl))
                    (-map #'agent-shell-queue-org--item-from-element
                          (agent-shell-queue-org--headlines-of bucket-hl))))
            (agent-shell-queue-org--headlines-of tree)))))

;;; Registration

(cl-defmethod agent-shell-queue--serialize-items ((_format (eql org)) items)
  (let ((agent-shell-queue--items items))
    (agent-shell-queue-org--serialize)))

(cl-defmethod agent-shell-queue--deserialize-items ((_format (eql org)) string)
  (agent-shell-queue-org--deserialize string))

(cl-defmethod agent-shell-queue-format-file-extension ((_format (eql org)))
  ".org")

(provide 'agent-shell-queue-org)
;;; agent-shell-queue-org.el ends here
