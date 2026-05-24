;;; agent-shell-setup.el --- Setup and keybinding helpers for agent-shell -*- lexical-binding: t -*-

;;; Commentary:
;; Completion configuration, mode keybinding macro, and global initialization
;; for agent-shell and agent-shell-queue buffers.

;;; Code:

(require 'agent-shell)
(require 'agent-shell-queue)

(declare-function corfu-mode "corfu")
(declare-function cape-dabbrev "cape")
(declare-function shell-maker-point-at-last-prompt-p "shell-maker")
(declare-function shell-maker-busy "shell-maker")

(setq agent-shell-buffer-name-format
      (lambda (agent-name project-name)
        (let* ((raw (string-trim project-name))
               (base (file-name-nondirectory (directory-file-name raw)))
               (stripped (replace-regexp-in-string "\\`[./]+" "" base))
               (slug (downcase (replace-regexp-in-string "\\s-+" "-"
                                                         (if (string-empty-p stripped) base stripped)))))
          (format "*%s-%s*"
                  (car (split-string (downcase (string-trim agent-name))))
                  slug))))

;;; Mode key

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

;;; Completion setup

(defun agent-shell-corfu-setup ()
  "Configure corfu auto-completion for agent-shell buffers."
  (corfu-mode +1)
  (setq-local corfu-auto-prefix 2)
  (setq-local completion-at-point-functions
              (cons #'cape-dabbrev (remq t completion-at-point-functions))))

(defun agent-shell-queue-capture-corfu-setup ()
  "Configure corfu and dabbrev completion for agent-shell-queue capture/edit buffers."
  (corfu-mode +1)
  (setq-local corfu-auto-prefix 2)
  (setq-local completion-at-point-functions
              (append '(cape-dabbrev agent-shell-queue-capture--slash-command-capf)
                      (remq t completion-at-point-functions))))

(defun agent-shell-queue-capture--slash-command-capf ()
  "Complete agent slash commands after / in capture buffers with a live target."
  (when-let* ((shell-buf (and (boundp 'agent-shell-queue--capture-target)
                              (buffer-live-p agent-shell-queue--capture-target)
                              agent-shell-queue--capture-target))
              (commands (with-current-buffer shell-buf
                          (map-elt agent-shell--state :available-commands)))
              ((not (seq-empty-p commands))))
    (save-excursion
      (let ((end (point)))
        (when (re-search-backward "/" (line-beginning-position) t)
          (list (1+ (point)) end
                (mapcar (lambda (c) (map-elt c 'name)) commands)
                :annotation-function
                (lambda (name)
                  (let ((cmd (seq-find (lambda (c) (equal (map-elt c 'name) name)) commands)))
                    (concat "  " (or (and cmd (map-elt cmd 'description)) ""))))
                :exclusive 'no))))))


(provide 'agent-shell-setup)
;;; agent-shell-setup.el ends here
