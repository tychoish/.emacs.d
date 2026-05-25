;;; agent-shell-menu.el --- ACR menus and transient prefixes for agent-shell -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Keywords: tools, agent-shell
;; Version: 0.1.0
;; URL: https://github.com/tychoish/dot-emacs
;; Package-Requires: ((emacs "29.1") (transient "0.4") (annotated-completing-read "0.1") (agent-shell "0.1"))

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; ACR-based interactive menus for agent-shell sessions.  Provides transient
;; prefix menus `agent-shell-global-menu' and `agent-shell-session-menu' for
;; navigating and controlling agent sessions.  Covers permission resolution,
;; action selection, command selection, and collapse control.  Setup and
;; keybinding infrastructure lives in `agent-shell-setup.el'.

;;; Code:

(require 'cl-lib)
(require 'annotated-completing-read)
(require 'transient)
(require 'agent-shell)
(require 'agent-shell-setup)

(declare-function agent-shell-viewport--shell-buffer "agent-shell-viewport")
(declare-function agent-shell-queue--format-age "agent-shell-queue")

;; Suppress byte-compiler warnings for agent-shell-queue commands
;; referenced in action alists and transient menus loaded lazily.
(declare-function agent-shell-queue-buffer-open "agent-shell-queue")
(declare-function agent-shell-queue-edit-task "agent-shell-queue")
(declare-function agent-shell-queue-enqueue "agent-shell-queue")
(declare-function agent-shell-queue-capture "agent-shell-queue")
(declare-function agent-shell-queue-enqueue-clear "agent-shell-queue")
(declare-function agent-shell-queue-capture-unassigned "agent-shell-queue")
(declare-function agent-shell-queue-capture-from-region "agent-shell-queue")
(declare-function agent-shell-queue-capture-from-context "agent-shell-queue")
(declare-function agent-shell-queue-capture-from-clipboard "agent-shell-queue")
(declare-function agent-shell-queue-pause "agent-shell-queue")
(declare-function agent-shell-queue-resume "agent-shell-queue")
(declare-function agent-shell-queue-raw-edit "agent-shell-queue")
(declare-function agent-shell-queue-import "agent-shell-queue")
(declare-function agent-shell-queue-fork-session "agent-shell-queue")
(declare-function agent-shell-queue-insert-fork-before "agent-shell-queue")
(declare-function agent-shell-queue-insert-fork-after "agent-shell-queue")
(declare-function agent-shell-queue-release-pending-fork "agent-shell-queue")

(defvar agent-shell-action-alist
  '(("submit" . shell-maker-submit)
    ("interrupt" . agent-shell-interrupt)
    ("jump to end (prompt)" . end-of-buffer)
    ("compose in viewport" . agent-shell-prompt-compose)
    ("goto last interaction" . agent-shell-goto-last-interaction)
    ("jump to permission row" . agent-shell-jump-to-latest-permission-button-row)
    ("next permission button" . agent-shell-next-permission-button)
    ("previous permission button" . agent-shell-previous-permission-button)
    ("next item" . agent-shell-next-item)
    ("previous item" . agent-shell-previous-item)
    ("other buffer (viewport)" . agent-shell-other-buffer)
    ("switch agent-shell" . agent-shell-switch-buffer)
    ("send region" . agent-shell-send-region)
    ("send file" . agent-shell-send-file)
    ("yank (DWIM)" . agent-shell-yank-dwim)
    ("queue request" . agent-shell-queue-enqueue)
    ("queue capture" . agent-shell-queue-capture)
    ("capture unassigned" . agent-shell-queue-capture-unassigned)
    ("capture from region" . agent-shell-queue-capture-from-region)
    ("capture from clipboard" . agent-shell-queue-capture-from-clipboard)
    ("capture from context" . agent-shell-queue-capture-from-context)
    ("queue clear" . agent-shell-queue-enqueue-clear)
    ("cycle session mode" . agent-shell-cycle-session-mode)
    ("set session mode" . agent-shell-set-session-mode)
    ("set session model" . agent-shell-set-session-model)
    ("copy session id" . agent-shell-copy-session-id)
    ("open transcript" . agent-shell-open-transcript)
    ("collapse menu" . agent-shell-select-collapse)
    ("queue review" . agent-shell-queue-buffer-open))
  "Alist of (LABEL . COMMAND) for `agent-shell-select-action'.")

;;; Key binding

(defmacro agent-shell-mode-key (key fn)
  "Define `agent-shell-output-key-KEY' and bind it in `agent-shell-mode-map'.
In the output area, or while the shell is busy, calls FN interactively.
Self-inserts KEY only when at the idle prompt.
Also binds FN directly in `agent-shell-viewport-view-mode-map'."
  (let* ((key-str (if (stringp key) key (symbol-name key)))
         (name (intern (concat "agent-shell-output-key-" key-str)))
         (char (pcase key-str
                 ("TAB" ?\t)
                 ((pred (lambda (s) (= 1 (length s)))) (aref key-str 0)))))
    `(progn
       (defun ,name ()
         ,(format "In output or busy: `%s'. Self-insert at idle prompt." fn)
         (interactive)
         (if (and (not (shell-maker-busy)) (shell-maker-point-at-last-prompt-p))
             ,(if char `(self-insert-command 1 ,char) '(ignore))
           (call-interactively #',fn)))
       (define-key agent-shell-mode-map (kbd ,key-str) #',name)
       (with-eval-after-load 'agent-shell-viewport
         (define-key agent-shell-viewport-view-mode-map (kbd ,key-str) #',fn)))))

;;; Buffer/session management

(defun agent-shell--buffer-annotation (buf)
  "Build an annotation string describing the agent-shell BUF."
  (with-current-buffer buf
    (let* ((status (agent-shell-status))
	   (state agent-shell--state)
	   (used (map-nested-elt state '(:usage :context-used)))
	   (size (map-nested-elt state '(:usage :context-size)))
	   (last (map-elt state :last-activity-time))
	   (cwd (abbreviate-file-name (or default-directory ""))))
      (mapconcat 'identity
		 (seq-remove #'null
		  (list (format "[%s]" status)
			cwd
			(when (and (numberp used) (numberp size) (> size 0))
			  (format "ctx %.0f%%" (* 100.0 (/ (float used) size))))
			(when last
			  (format "%s ago"
				  (agent-shell-queue--format-age (time-since last))))))
		 " · "))))

(defun agent-shell-extras--pick-buffer (prompt)
  "Select an agent-shell buffer via PROMPT using status/cwd/context annotations."
  (let ((bufs (or (agent-shell-buffers) (user-error "No live agent-shell buffers"))))
    (get-buffer
     (annotated-completing-read
      (seq-map (lambda (buf) (cons (buffer-name buf) (agent-shell--buffer-annotation buf)))
	       bufs)
      :prompt prompt
      :category 'agent-shell-buffer
      :require-match t
      :history 'agent-shell-extras--pick-buffer))))

;;;###autoload
(defun agent-shell-switch-buffer ()
  "Switch to an agent-shell buffer with status, cwd, context, and age annotations."
  (interactive)
  (switch-to-buffer (agent-shell-extras--pick-buffer "agent-shell => ")))

;;; Permission resolution

(defun agent-shell--permission-buttons ()
  "Return a list of (LABEL . POSITION) for each pending permission button.
LABEL is the visible button text trimmed of surrounding brackets/whitespace.
POSITION is buffer position of the button's start."
  (let (out)
    (save-excursion
      (goto-char (point-min))
      (let (match)
	(while (setq match (text-property-search-forward 'button 'permission t))
	  (let* ((beg (prop-match-beginning match))
		 (end (prop-match-end match))
		 (text (buffer-substring-no-properties beg end))
		 (label (string-trim text "[][ \t\n\r]+" "[][ \t\n\r]+")))
	    (push (cons label beg) out)))))
    (nreverse out)))

(defun agent-shell--permission-action-at (position)
  "Return the RET command bound on the permission button at POSITION."
  (when-let ((keymap (get-text-property position 'keymap)))
    (lookup-key keymap (kbd "RET"))))

(defun agent-shell--permission-button-action (pos)
  "Return an interactive command that activates the permission button at POS."
  (lambda ()
    (interactive)
    (when-let ((cmd (agent-shell--permission-action-at pos)))
      (save-excursion
	(goto-char pos)
	(call-interactively cmd)))))

;;;###autoload
(defun agent-shell-resolve-permission ()
  "Resolve a pending permission prompt via `annotated-completing-read'."
  (interactive)

  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Not in an agent-shell buffer"))

  (unless (agent-shell--permission-pending-p)
    (user-error "No pending permission request in this buffer"))

  (let* ((label (annotated-completing-read
		 (or (agent-shell--permission-buttons)
		     (user-error "No permission buttons found in this buffer"))
                 :prompt "permission => "
                 :category 'agent-shell-permission
                 :require-match t
                 :history 'agent-shell-resolve-permission))
         (pos (cdr (assoc label buttons)))
         (cmd (or (and pos (agent-shell--permission-action-at pos))
                  (user-error "No action attached to permission button"))))

    (save-excursion
      (goto-char pos)
      (call-interactively cmd))))

;;; Action menu

;;;###autoload
(defun agent-shell-select-action ()
  "Pick a common agent-shell action and run it via `call-interactively'.
When a permission request is pending, permission responses are spliced into the menu."
  (interactive)
  (when-let* ((table (thread-last (when (and (derived-mode-p 'agent-shell-mode)
					     (agent-shell--permission-pending-p))
					    (agent-shell--permission-buttons))
				  (mapcar (lambda (b)
					    (cons (format "permission: %s" (car b))
						  (agent-shell--permission-button-action (cdr b)))))
				  (append agent-shell-action-alist)
				  (seq-filter (lambda (entry) (commandp (cdr entry))))
				  (seq-map (lambda (entry)
					    (cons (car entry)
						  (or (car (split-string (or (documentation (cdr entry)) "") "\n")) ""))))))
	      (label (annotated-completing-read table
		      :prompt "agent-shell action =>"
		      :category 'agent-shell-action
		      :require-match t
		      :history 'agent-shell-select-action))
	      (cmd (cdr (assoc label table)))
	      (_ (commandp cmd)))
    (call-interactively cmd)))

;;; Project session switching

(defun agent-shell-extras--same-project-buffers ()
  "Return live agent-shell buffers sharing the current buffer's project directory."
  (let ((dir default-directory))
    ;; TODO refactor this to use seq-filter and (with-current-buffer) (simplification)
    (cl-remove-if-not
     (lambda (b)
       (and (not (eq b (current-buffer)))
            (equal (buffer-local-value 'default-directory b) dir)))
     (agent-shell-buffers))))

;;;###autoload
(defun agent-shell-switch-project-session ()
  "Switch to another agent-shell session in the same project directory."
  (interactive)
  (switch-to-buffer
   (get-buffer (annotated-completing-read
		(agent-shell--buffer-annotation (or (agent-shell-extras--same-project-buffers)
						    (user-error "No other agent-shell sessions for this project")))
                :prompt "project session =>"
                :category 'agent-shell-buffer
                :require-match t
                :history 'agent-shell-switch-project-session))))

;;; Transient menus

;;;###autoload
(transient-define-prefix agent-shell-global-menu ()
  "Global agent-shell operations."
  [["Sessions"
    (";" "Switch to session" agent-shell-switch-buffer)
    ("," "Manager toggle" agent-shell-manager-toggle)
    ("b" "Find buffer" agent-shell-manager-find-buffer)]
   ["Create"
    ("n" "New shell session" agent-shell-new-shell)
    ("t" "New temp shell" agent-shell-new-temp-shell)
    ("h" "Resume session (hydrate)" agent-shell-resume-session)]
   ["Queue"
    ("q" "Open queue" agent-shell-queue-buffer-open)
    ("e" "Enqueue prompt" agent-shell-queue-enqueue)
    ("E" "Edit task" agent-shell-queue-edit-task)]
   ["Capture"
    ("w" "Compose (write)" agent-shell-queue-capture)
    ("u" "Unassigned capture" agent-shell-queue-capture-unassigned)
    ("r" "From region" agent-shell-queue-capture-from-region)
    ("y" "From clipboard" agent-shell-queue-capture-from-clipboard)
    ("c" "From context" agent-shell-queue-capture-from-context)]])

;;;###autoload
(transient-define-prefix agent-shell-session-menu ()
  "Actions for the current agent-shell session."
  [["Navigate"
    ("g" "Last interaction" agent-shell-goto-last-interaction)
    ("P" "Jump to permissions" agent-shell-jump-to-latest-permission-button-row)
    ("n" "Next permission button" agent-shell-next-permission-button)
    ("p" "Prev permission button" agent-shell-previous-permission-button)]
   ["Act"
    ("a" "Action menu" agent-shell-select-action)
    ("R" "Resolve permission" agent-shell-resolve-permission)
    ("i" "Interrupt" agent-shell-interrupt)
    ("/" "Command menu" agent-shell-select-command)]
   ["Queue"
    ("q" "Open queue" agent-shell-queue-buffer-open)
    ("e" "Enqueue prompt" agent-shell-queue-enqueue)
    ("E" "Edit task" agent-shell-queue-edit-task)]
   ["Capture"
    ("w" "Compose (write)" agent-shell-queue-capture)
    ("u" "Unassigned capture" agent-shell-queue-capture-unassigned)
    ("r" "From region" agent-shell-queue-capture-from-region)
    ("y" "From clipboard" agent-shell-queue-capture-from-clipboard)
    ("z" "From context" agent-shell-queue-capture-from-context)]
   ["Mode"
    ("c" "Cycle session mode" agent-shell-cycle-session-mode)
    ("M" "Set session mode" agent-shell-set-session-mode)
    ("v" "Set session model" agent-shell-set-session-model)]
   ["Session"
    ("f" "Fork session" agent-shell-fork)
    ("o" "Other session (project)" agent-shell-switch-project-session)
    ("I" "Copy session ID" agent-shell-copy-session-id)
    ("T" "Open transcript" agent-shell-open-transcript)]
   ["Fork Queue"
    ("Gf" "Fork queue from here" agent-shell-queue-fork-session)
    ("Gb" "Insert fork task before" agent-shell-queue-insert-fork-before)
    ("Ga" "Insert fork task after" agent-shell-queue-insert-fork-after)
    ("Gr" "Release pending-fork items" agent-shell-queue-release-pending-fork)]])

;;; Command menu

;;;###autoload
(defun agent-shell-select-command ()
  "Insert one of the agent's advertised `/' commands at the prompt."
  (interactive)
  (let* ((shell (or (cond
		     ((derived-mode-p 'agent-shell-mode) (current-buffer))
		     ((agent-shell-viewport--shell-buffer)))
		    (user-error "not in an agent-shell or viewport buffer")))
	 (commands (with-current-buffer shell
		     (map-elt agent-shell--state :available-commands))))
    (unless commands
      (user-error "no agent slash-commands advertised in %s" (buffer-name shell)))
    (agent-shell-insert :text (concat "/" (annotated-completing-read
					   (seq-map (lambda (c)
						     (cons (map-elt c 'name)
							   (or (map-elt c 'description) "")))
						   commands)
					   :prompt "agent /command => "
					   :category 'agent-shell-slash-command
					   :require-match t
					   :history 'agent-shell-select-command) " ")
			:shell-buffer shell
			:submit nil)))

;;; Collapse menu

(defun agent-shell--blocks-in-buffer ()
  "Return one entry per distinct fragment block in the buffer.
Each entry is `((:start . POS) (:state . STATE))'.  Plain-text entries
created via `agent-shell-ui-update-text' (no `:collapsed' key) are skipped."
  (let ((seen (make-hash-table :test 'equal))
	(pos (point-min))
	out)
    (while pos
      (when-let* ((state (get-text-property pos 'agent-shell-ui-state))
		  (id (map-elt state :qualified-id))
		  ((assq :collapsed state))
		  ((not (map-elt seen id))))
	(map-put! seen id t)
	(push (list (cons :start pos) (cons :state state)) out))
      (setq pos (next-single-property-change pos 'agent-shell-ui-state)))
    (nreverse out)))

(defun agent-shell--block-category (qualified-id)
  "Classify QUALIFIED-ID into a coarse block category string."
  (cond
   ((string-match-p "agent_thought_chunk\\'" qualified-id) "thinking")
   ((string-match-p "agent_message_chunk\\'" qualified-id) "agent message")
   ((string-match-p "user_message_chunk\\'" qualified-id)  "user message")
   ((string-suffix-p "-plan" qualified-id)                 "plan")
   ((string-prefix-p "bootstrapping-" qualified-id)        "session info")
   (t                                                       "tool call")))

(cl-defun agent-shell--set-collapse (target &key category)
  "Force `:collapsed' = TARGET on every toggleable block.
When CATEGORY is non-nil, only affect blocks matching that category."
  (save-mark-and-excursion
    (dolist (block (agent-shell--blocks-in-buffer))
      (let* ((state (map-elt block :state))
	     (id (map-elt state :qualified-id))
	     (collapsed (map-elt state :collapsed)))
	(when (and (not (eq (and collapsed t) (and target t)))
		   (or (null category)
		       (equal category (agent-shell--block-category id))))
	  (goto-char (map-elt block :start))
	  (agent-shell-ui-toggle-fragment-at-point))))))

;;;###autoload
(defun agent-shell-select-collapse ()
  "Pick a collapse action via `annotated-completing-read'.
Offers bulk expand/collapse, per-category toggles, and entries to flip the
three expand-by-default customization variables."
  (interactive)
  (unless (or (derived-mode-p 'agent-shell-mode)
	      (derived-mode-p 'agent-shell-viewport-view-mode))
    (user-error "Not in an agent-shell buffer"))
  (let* ((by-cat (make-hash-table :test #'equal))
	 (table (make-hash-table :test #'equal))
	 (toggles '(("~ thinking: expand-by-default"
		     . agent-shell-thought-process-expand-by-default)
		    ("~ tool call: expand-by-default"
		     . agent-shell-tool-use-expand-by-default)
		    ("~ user message: expand-by-default"
		     . agent-shell-user-message-expand-by-default))))
    (dolist (b (agent-shell--blocks-in-buffer))
      (let* ((state (map-elt b :state))
	     (cat (agent-shell--block-category (map-elt state :qualified-id)))
	     (entry (or (map-elt by-cat cat) (cons 0 0))))
	(cl-incf (car entry))
	(when (map-elt state :collapsed) (cl-incf (cdr entry)))
	(map-put! by-cat cat entry)))
    (map-put! table "+ expand all" "show every collapseable block")
    (map-put! table "+ collapse all" "hide every collapseable block")
    (map-put! table "~ set all: collapse by default"
              (if (and (not (symbol-value 'agent-shell-thought-process-expand-by-default))
                       (not (symbol-value 'agent-shell-tool-use-expand-by-default))
                       (not (symbol-value 'agent-shell-user-message-expand-by-default)))
                  "already collapsed by default"
                "set thinking, tool calls, and user messages to collapse by default"))
    (map-put! table "~ set all: expand by default"
              (if (and (symbol-value 'agent-shell-thought-process-expand-by-default)
                       (symbol-value 'agent-shell-tool-use-expand-by-default)
                       (symbol-value 'agent-shell-user-message-expand-by-default))
                  "already expanded by default"
                "set thinking, tool calls, and user messages to expand by default"))
    (dolist (cat (sort (map-keys by-cat) #'string<))
      (let* ((entry (map-elt by-cat cat))
	     (total (car entry))
	     (n-collapsed (cdr entry))
	     (state-str (cond ((zerop n-collapsed) "all expanded")
			      ((= n-collapsed total) "all collapsed")
			      (t (format "%d/%d collapsed" n-collapsed total)))))
	(map-put! table cat (format "%d block%s · %s"
				    total (if (= total 1) "" "s") state-str))))
    (dolist (toggle toggles)
      (map-put! table (car toggle)
	        (if (symbol-value (cdr toggle)) "expanded by default" "collapsed by default")))
    (let ((choice (annotated-completing-read table
					     :prompt "agent-shell collapse: "
					     :category 'agent-shell-collapse
					     :require-match t
					     :history 'agent-shell-select-collapse)))
      (cond
       ((equal choice "+ expand all")   (agent-shell--set-collapse nil))
       ((equal choice "+ collapse all") (agent-shell--set-collapse t))
       ((equal choice "~ set all: collapse by default")
        (set 'agent-shell-thought-process-expand-by-default nil)
        (set 'agent-shell-tool-use-expand-by-default nil)
        (set 'agent-shell-user-message-expand-by-default nil)
        (message "All block types set to collapse by default"))
       ((equal choice "~ set all: expand by default")
        (set 'agent-shell-thought-process-expand-by-default t)
        (set 'agent-shell-tool-use-expand-by-default t)
        (set 'agent-shell-user-message-expand-by-default t)
        (message "All block types set to expand by default"))
       ((assoc choice toggles)
	(let ((var (cdr (assoc choice toggles))))
	  (set var (not (symbol-value var)))
	  (message "%s → %s" var (if (symbol-value var) "expanded" "collapsed"))))
       (t
	(let ((entry (map-elt by-cat choice)))
	  (agent-shell--set-collapse (< (cdr entry) (car entry))
				     :category choice)))))))

(provide 'agent-shell-menu)

;;; agent-shell-menu.el ends here
