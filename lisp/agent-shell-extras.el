;;; agent-shell-extras.el --- Custom extensions for agent-shell -*- lexical-binding: t -*-

;; Author: tycho garen
;; Maintainer: tychoish
;; Keywords: tools, agent-shell

;; This file is not part of GNU Emacs

;;; Code:

(require 'cl-lib)
(require 'ht)
(require 'annotated-completing-read)
(require 's)
(require 'dash)
(require 'transient)
(require 'agent-shell)

(declare-function agent-shell-viewport--shell-buffer "agent-shell-viewport")
(declare-function shell-maker-point-at-last-prompt-p "shell-maker")
(declare-function tychoish/conf-state-path "tychoish-bootstrap")

;; Suppress byte-compiler warnings for agent-shell-queue variables and commands
;; referenced in action alists and transient menus loaded lazily.
(defvar agent-shell-queue-serialization-format)
(defvar agent-shell-queue-state-file-function)
(defvar agent-shell-queue-pick-buffer-function)
(declare-function agent-shell-queue-open "agent-shell-queue")
(declare-function agent-shell-queue-enqueue "agent-shell-queue")
(declare-function agent-shell-queue-capture "agent-shell-queue")
(declare-function agent-shell-queue-enqueue-clear "agent-shell-queue")

;;; agent-shell-queue integration (lazy — no hard require)

(defun agent-shell-extras--queue-state-file ()
  "Queue state file under the per-instance agent-shell state directory."
  (let ((ext (pcase agent-shell-queue-serialization-format
               ('json "json")
               ('yaml "yaml")
               (_ "el"))))
    (expand-file-name (concat "queue." ext)
                      (tychoish/conf-state-path "agent-shell"))))

(with-eval-after-load 'agent-shell-queue
  (setq agent-shell-queue-state-file-function #'agent-shell-extras--queue-state-file)
  (setq agent-shell-queue-pick-buffer-function #'agent-shell-extras--pick-buffer))

(setq agent-shell-buffer-name-format
      (lambda (agent-name project-name)
        (format "*%s-%s*"
                (downcase (replace-regexp-in-string "\\s-+" "-" (string-trim agent-name)))
                (downcase (replace-regexp-in-string "\\s-+" "-" (string-trim project-name))))))

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
    ("queue clear" . agent-shell-queue-enqueue-clear)
    ("cycle session mode" . agent-shell-cycle-session-mode)
    ("set session mode" . agent-shell-set-session-mode)
    ("set session model" . agent-shell-set-session-model)
    ("copy session id" . agent-shell-copy-session-id)
    ("open transcript" . agent-shell-open-transcript)
    ("collapse menu" . agent-shell-collapse-menu)
    ("queue review" . agent-shell-queue-open))
  "Alist of (LABEL . COMMAND) for `agent-shell-action-menu'.")

;;; Mode key

(defmacro agent-shell-mode-key (key fn)
  "Define `agent-shell-output-key-KEY' and bind it in `agent-shell-mode-map'.
In the output section calls FN interactively; self-inserts KEY at the prompt."
  (let* ((key-str (if (stringp key) key (symbol-name key)))
         (name (intern (concat "agent-shell-output-key-" key-str)))
         (char (pcase key-str
                 ("TAB" ?\t)
                 ((pred (lambda (s) (= 1 (length s)))) (aref key-str 0)))))
    `(progn
       (defun ,name ()
         ,(format "In output: `%s'. Self-insert at prompt." fn)
         (interactive)
         (if (shell-maker-point-at-last-prompt-p)
             ,(if char `(self-insert-command 1 ,char) '(ignore))
           (call-interactively #',fn)))
       (define-key agent-shell-mode-map (kbd ,key-str) #',name))))

;;; Completion setup

(defun agent-shell-corfu-setup ()
  "Configure corfu auto-completion for agent-shell buffers."
  (corfu-mode +1)
  (setq-local corfu-auto-prefix 2)
  (setq-local completion-at-point-functions
	      (append (remq t completion-at-point-functions)
		      (list #'cape-dabbrev))))

;;; Buffer/session management

(defun agent-shell--format-age (delta)
  "Format DELTA, a time-value, as a short relative age (e.g. \"3m\", \"2h\")."
  (let ((s (float-time delta)))
    (cond ((< s 60) (format "%ds" (truncate s)))
	  ((< s 3600) (format "%dm" (truncate (/ s 60))))
	  ((< s 86400) (format "%dh" (truncate (/ s 3600))))
	  (t (format "%dd" (truncate (/ s 86400)))))))

(defun agent-shell--buffer-annotation (buf)
  "Build an annotation string describing the agent-shell BUF."
  (with-current-buffer buf
    (let* ((status (agent-shell-status))
	   (state agent-shell--state)
	   (used (map-nested-elt state '(:usage :context-used)))
	   (size (map-nested-elt state '(:usage :context-size)))
	   (last (map-elt state :last-activity-time))
	   (cwd (abbreviate-file-name (or default-directory ""))))
      (s-join " · "
	      (-non-nil
	       (list (format "[%s]" status)
		     cwd
		     (when (and (numberp used) (numberp size) (> size 0))
		       (format "ctx %.0f%%" (* 100.0 (/ (float used) size))))
		     (when last
		       (format "%s ago"
			       (agent-shell--format-age (time-since last))))))))))

(defun agent-shell-extras--pick-buffer (prompt)
  "Select an agent-shell buffer via PROMPT using status/cwd/context annotations."
  (let ((table (ht-create)))
    (dolist (buf (or (agent-shell-buffers) (user-error "No live agent-shell buffers")))
      (ht-set table (buffer-name buf) (agent-shell--buffer-annotation buf)))
    (get-buffer (annotated-completing-read table
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
  (let ((buttons (or (agent-shell--permission-buttons)
                     (user-error "No permission buttons found in this buffer")))
        (table (ht-create)))
    (dolist (b buttons)
      (ht-set table (car b) (format "pos %d" (cdr b))))
    (let* ((label (annotated-completing-read table
                                             :prompt "permission => "
                                             :category 'agent-shell-permission
                                             :require-match t
                                             :history 'agent-shell-resolve-permission))
           (pos   (cdr (assoc label buttons)))
           (cmd   (or (and pos (agent-shell--permission-action-at pos))
                      (user-error "No action attached to permission button"))))
      (save-excursion
        (goto-char pos)
        (call-interactively cmd)))))

;;; Action menu

;;;###autoload
(defun agent-shell-action-menu ()
  "Pick a common agent-shell action and run it via `call-interactively'.
When a permission request is pending, permission responses are spliced into the menu."
  (interactive)
  (let* ((perm-entries
	  (when (and (derived-mode-p 'agent-shell-mode)
		     (agent-shell--permission-pending-p))
	    (mapcar (lambda (b)
		      (cons (format "permission: %s" (car b))
			    (agent-shell--permission-button-action (cdr b))))
		    (agent-shell--permission-buttons))))
	 (alist (append perm-entries agent-shell-action-alist))
	 (table (ht-create)))
    (dolist (entry alist)
      (when (commandp (cdr entry))
	(ht-set table (car entry)
		(or (car (split-string (or (documentation (cdr entry)) "") "\n")) ""))))
    (when-let* ((label (annotated-completing-read table
			  :prompt "agent-shell action =>"
			  :category 'agent-shell-action
			  :require-match t
			  :history 'agent-shell-action-menu))
		(cmd (cdr (assoc label alist)))
		((commandp cmd)))
      (call-interactively cmd))))

;;; Project session switching

(defun agent-shell-extras--same-project-buffers ()
  "Return live agent-shell buffers sharing the current buffer's project directory."
  (let ((dir default-directory))
    (cl-remove-if-not
     (lambda (b)
       (and (not (eq b (current-buffer)))
            (equal (buffer-local-value 'default-directory b) dir)))
     (agent-shell-buffers))))

;;;###autoload
(defun agent-shell-switch-project-session ()
  "Switch to another agent-shell session in the same project directory."
  (interactive)
  (let* ((bufs (or (agent-shell-extras--same-project-buffers)
                   (user-error "No other agent-shell sessions for this project")))
         (table (ht-create)))
    (dolist (buf bufs)
      (ht-set table (buffer-name buf) (agent-shell--buffer-annotation buf)))
    (switch-to-buffer
     (get-buffer (annotated-completing-read table
                                            :prompt "project session => "
                                            :category 'agent-shell-buffer
                                            :require-match t
                                            :history 'agent-shell-switch-project-session)))))

;;; Transient menus

;;;###autoload
(transient-define-prefix agent-shell-global-menu ()
  "Global agent-shell operations."
  [["Sessions"
    ("s" "Switch session"   agent-shell-switch-buffer)
    ("m" "Manager toggle"   agent-shell-manager-toggle)
    ("f" "Find buffer"      agent-shell-manager-find-buffer)]
   ["Create"
    ("n" "New shell"        agent-shell-new-shell)
    ("t" "New temp shell"   agent-shell-new-temp-shell)
    ("r" "Resume session"   agent-shell-resume-session)]
   ["Queue"
    ("q" "Open queue"       agent-shell-queue-open)
    ("e" "Enqueue prompt"   agent-shell-queue-enqueue)]])

;;;###autoload
(transient-define-prefix agent-shell-session-menu ()
  "Actions for the current agent-shell session."
  [["Navigate"
    ("g" "Last interaction"        agent-shell-goto-last-interaction)
    ("P" "Jump to permissions"     agent-shell-jump-to-latest-permission-button-row)
    ("n" "Next permission button"  agent-shell-next-permission-button)
    ("p" "Prev permission button"  agent-shell-previous-permission-button)]
   ["Act"
    ("a" "Action menu"             agent-shell-action-menu)
    ("R" "Resolve permission"      agent-shell-resolve-permission)
    ("i" "Interrupt"               agent-shell-interrupt)
    ("/" "Command menu"            agent-shell-command-menu)]
   ["Queue"
    ("q" "Open queue"              agent-shell-queue-open)
    ("e" "Enqueue prompt"          agent-shell-queue-enqueue)]
   ["Mode"
    ("c" "Cycle session mode"      agent-shell-cycle-session-mode)
    ("M" "Set session mode"        agent-shell-set-session-mode)
    ("v" "Set session model"       agent-shell-set-session-model)]
   ["Session"
    ("f" "Fork session"            agent-shell-fork)
    ("o" "Other session (project)" agent-shell-switch-project-session)
    ("I" "Copy session ID"         agent-shell-copy-session-id)
    ("T" "Open transcript"         agent-shell-open-transcript)]])

;;; Command menu

;;;###autoload
(defun agent-shell-command-menu ()
  "Insert one of the agent's advertised `/' commands at the prompt."
  (interactive)
  (let* ((shell (or (cond
		      ((derived-mode-p 'agent-shell-mode) (current-buffer))
		      ((agent-shell-viewport--shell-buffer)))
		     (user-error "not in an agent-shell or viewport buffer")))
	 (commands (with-current-buffer shell
		     (map-elt agent-shell--state :available-commands)))
	 (table (ht-create)))
    (unless commands
      (user-error "no agent slash-commands advertised in %s" (buffer-name shell)))
    (seq-do (lambda (c)
	      (ht-set table (map-elt c 'name) (or (map-elt c 'description) "")))
	    commands)
    (agent-shell-insert :text (concat "/" (annotated-completing-read
					   table
					   :prompt "agent /command => "
					   :category 'agent-shell-slash-command
					   :require-match t
					   :history 'agent-shell-command-menu) " ")
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
		  ((not (gethash id seen))))
	(puthash id t seen)
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
(defun agent-shell-collapse-menu ()
  "Pick a collapse action via `annotated-completing-read'.
Offers bulk expand/collapse, per-category toggles, and entries to flip the
three expand-by-default customization variables."
  (interactive)
  (unless (or (derived-mode-p 'agent-shell-mode)
	      (derived-mode-p 'agent-shell-viewport-view-mode))
    (user-error "Not in an agent-shell buffer"))
  (let* ((by-cat (ht-create))
	 (table (ht-create))
	 (toggles '(("~ thinking: expand-by-default"
		     . agent-shell-thought-process-expand-by-default)
		    ("~ tool call: expand-by-default"
		     . agent-shell-tool-use-expand-by-default)
		    ("~ user message: expand-by-default"
		     . agent-shell-user-message-expand-by-default))))
    (dolist (b (agent-shell--blocks-in-buffer))
      (let* ((state (map-elt b :state))
	     (cat (agent-shell--block-category (map-elt state :qualified-id)))
	     (entry (or (ht-get by-cat cat) (cons 0 0))))
	(cl-incf (car entry))
	(when (map-elt state :collapsed) (cl-incf (cdr entry)))
	(ht-set! by-cat cat entry)))
    (ht-set! table "+ expand all"   "show every collapseable block")
    (ht-set! table "+ collapse all" "hide every collapseable block")
    (dolist (cat (sort (ht-keys by-cat) #'string<))
      (let* ((entry (ht-get by-cat cat))
	     (total (car entry))
	     (n-collapsed (cdr entry))
	     (state-str (cond ((zerop n-collapsed) "all expanded")
			      ((= n-collapsed total) "all collapsed")
			      (t (format "%d/%d collapsed" n-collapsed total)))))
	(ht-set! table cat (format "%d block%s · %s"
				   total (if (= total 1) "" "s") state-str))))
    (dolist (toggle toggles)
      (ht-set! table (car toggle)
	       (if (symbol-value (cdr toggle)) "expanded by default" "collapsed by default")))
    (let ((choice (annotated-completing-read table
					     :prompt "agent-shell collapse: "
					     :category 'agent-shell-collapse
					     :require-match t
					     :history 'agent-shell-collapse-menu)))
      (cond
       ((equal choice "+ expand all")   (agent-shell--set-collapse nil))
       ((equal choice "+ collapse all") (agent-shell--set-collapse t))
       ((assoc choice toggles)
	(let ((var (cdr (assoc choice toggles))))
	  (set var (not (symbol-value var)))
	  (message "%s → %s" var (if (symbol-value var) "expanded" "collapsed"))))
       (t
	(let ((entry (ht-get by-cat choice)))
	  (agent-shell--set-collapse (< (cdr entry) (car entry))
				     :category choice)))))))

(provide 'agent-shell-extras)

;;; agent-shell-extras.el ends here
