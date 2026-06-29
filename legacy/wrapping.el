(advice-add 'set-fill-column :around #'ad:set-fill-column-locally)

(defun ad:set-fill-column-locally (f &rest arg)
  (let ((had-default (default-boundp 'fill-column))
	(previous-default (default-value 'fill-column))
	(new-value (apply f arg)))
    (when had-default
      (setq-default fill-column previous-default))
    (setq-local fill-column new-value)))

(defun bootstrap--vfc-fill-column-watcher (_sym _newval op where)
  "Re-apply visual-fill-column-mode when fill-column is set in an active buffer."
  (when (and (eq op 'set) (buffer-live-p where))
    (with-current-buffer where
      (when (bound-and-true-p visual-fill-column-mode)
        (visual-fill-column-mode 0)))))

(add-variable-watcher 'fill-column #'bootstrap--vfc-fill-column-watcher)

(defconst bootstrap--vfc-heading-patterns
  '((org-mode      . "^\\*+\\s-")
    (markdown-mode . "^#+\\s-"))
  "Alist mapping major-mode symbols to heading regexp patterns.")

(defconst bootstrap--vfc-wrap-prefix
  (propertize " " 'display '(space :width 9999))
  "wrap-prefix value that pushes continuation lines off-screen.
A fixed large integer is used; (frame-width) would be more precise during
export but adds complexity with no interactive benefit.")

(defun bootstrap--vfc-jit-lock (start end)
  "Apply heading truncation wrap-prefix in the region START to END."
  (let ((pattern (cdr (assq major-mode bootstrap--vfc-heading-patterns))))
    (when pattern
      (save-excursion
        (goto-char start)
        (beginning-of-line)
        (while (< (point) end)
          (when (looking-at pattern)
            (put-text-property (line-beginning-position)
			       (line-end-position)
			       'wrap-prefix bootstrap--vfc-wrap-prefix))
          (forward-line 1))))))

(define-minor-mode bootstrap-vfc-heading-truncation-mode
  "Simulate truncation on heading lines when visual-fill-column is active."
  :lighter nil
  (if bootstrap-vfc-heading-truncation-mode
      (progn
        (jit-lock-register #'bootstrap--vfc-jit-lock)
        (jit-lock-refontify))
    (jit-lock-unregister #'bootstrap--vfc-jit-lock)
    (with-silent-modifications
      (remove-text-properties (point-min) (point-max) '(wrap-prefix nil)))))

(add-hook 'visual-fill-column-mode-hook
          (lambda ()
            (when (assq major-mode bootstrap--vfc-heading-patterns)
              (bootstrap-vfc-heading-truncation-mode
	       (if (bound-and-true-p visual-fill-column-mode) 1 -1)))))
