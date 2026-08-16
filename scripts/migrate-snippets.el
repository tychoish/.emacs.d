;;; migrate-snippets.el --- Migrate YASnippet templates to Tempel format -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'subr-x)

(defun tempel-migrate--count-placeholders (body)
  "Scan BODY and return an alist of (index . count) for YASnippet placeholders."
  (let ((start 0)
        (counts nil))
    (while (string-match "\\$\\(?:\\([0-9]+\\)\\|{\\([0-9]+\\)\\(?:[:#]\\([^}]*\\)\\)?}\\)" body start)
      (let* ((idx-str (or (match-string 1 body) (match-string 2 body)))
             (idx (string-to-number idx-str)))
        (setq counts (cons (cons idx (1+ (or (cdr (assoc idx counts)) 0)))
                           (assoc-delete-all idx counts))))
      (setq start (match-end 0)))
    counts))

(defun tempel-migrate--parse-body (body counts)
  "Parse BODY string into a list of Tempel syntax elements using placeholder COUNTS."
  (let ((pos 0)
        (len (length body))
        (elements nil)
        (seen-indices nil))
    (while (< pos len)
      (let* ((next-placeholder (string-match "\\$\\(?:\\([0-9]+\\)\\|{\\([0-9]+\\)\\(?:[:#]\\([^}]*\\)\\)?}\\)" body pos))
             (next-selected (string-match "\\$yas-selected-text\\|\\${yas-selected-text}" body pos))
             (next-backtick (string-match "`\\([^`]+\\)`" body pos))
             ;; Find the earliest match
             (earliest-match nil)
             (earliest-pos len)
             (match-type nil))
        (when (and next-placeholder (< next-placeholder earliest-pos))
          (setq earliest-pos next-placeholder
                earliest-match next-placeholder
                match-type 'placeholder))
        (when (and next-selected (< next-selected earliest-pos))
          (setq earliest-pos next-selected
                earliest-match next-selected
                match-type 'selected))
        (when (and next-backtick (< next-backtick earliest-pos))
          (setq earliest-pos next-backtick
                earliest-match next-backtick
                match-type 'backtick))

        ;; Add literal text before match
        (when (> earliest-pos pos)
          (let ((literal (substring body pos earliest-pos)))
            (push literal elements)))

        (if (null earliest-match)
            (setq pos len)
          (cond
           ((eq match-type 'selected)
            (push 'r elements)
            (setq pos (match-end 0)))

           ((eq match-type 'backtick)
            (let* ((elisp-str (match-string 1 body))
                   (elisp-form (condition-case nil
                                   (car (read-from-string elisp-str))
                                 (error elisp-str))))
              (push elisp-form elements))
            (setq pos (match-end 0)))

           ((eq match-type 'placeholder)
            (let* ((idx-str (or (match-string 1 body) (match-string 2 body)))
                   (idx (string-to-number idx-str))
                   (default-val (match-string 3 body))
                   (count (or (cdr (assoc idx counts)) 0)))
              (cond
               ((= idx 0)
                (push 'q elements))
               ((> count 1)
                (let ((var-sym (intern (format "v%d" idx))))
                  (if (member idx seen-indices)
                      (push (list 's var-sym) elements)
                    (push idx seen-indices)
                    (if default-val
                        (push (list 'p default-val var-sym) elements)
                      (push (list 'p var-sym) elements)))))
               (t
                (if default-val
                    (push (list 'p default-val) elements)
                  (push 'p elements)))))
            (setq pos (match-end 0)))))))
    (nreverse elements)))

(defun tempel-migrate--parse-file (file-path)
  "Parse YASnippet file at FILE-PATH and return a Tempel template form."
  (with-temp-buffer
    (insert-file-contents file-path)
    (goto-char (point-min))
    (let ((key nil)
          (name nil)
          (body-start nil)
          (lines nil))
      ;; Parse metadata headers
      (while (and (not body-start) (not (eobp)))
        (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
          (cond
           ((string-prefix-p "# --" line)
            (setq body-start (line-beginning-position 2)))
           ((string-match "^#[[:blank:]]*key:[[:blank:]]*\\(\\S.+\\)" line)
            (setq key (match-string 1 line)))
           ((string-match "^#[[:blank:]]*name:[[:blank:]]*\\(\\S.+\\)" line)
            (setq name (match-string 1 line)))
           ((not (string-prefix-p "#" line))
            ;; No "# --" and reached first non-comment line
            (setq body-start (line-beginning-position))))
          (forward-line 1)))

      (unless body-start
        (setq body-start (point-min)))

      (let* ((body (buffer-substring-no-properties body-start (point-max)))
             (file-base (file-name-base file-path))
             (final-key (or key file-base))
             (final-name (or name file-base))
             (counts (tempel-migrate--count-placeholders body))
             (elements (tempel-migrate--parse-body body counts)))
        (append (list (intern final-key))
                elements
                (when (and final-name (not (string= final-name final-key)))
                  (list :doc final-name)))))))

(defun yas-to-tempel-migrate-all (&optional snippets-dir output-file)
  "Migrate all YASnippets in SNIPPETS-DIR to Tempel template format in OUTPUT-FILE."
  (interactive)
  (let* ((snippets-dir (or snippets-dir (expand-file-name "snippets" user-emacs-directory)))
         (output-file (or output-file (expand-file-name "templates.migrated" user-emacs-directory)))
         (modes (when (file-directory-p snippets-dir)
                  (directory-files snippets-dir nil "^[a-zA-Z0-9].*-mode$")))
         (total-migrated 0)
         (errors nil))
    (unless (file-directory-p snippets-dir)
      (error "Snippets directory does not exist: %s" snippets-dir))

    (with-temp-file output-file
      (insert ";;; -*- mode: lisp-data -*-\n\n")
      (dolist (mode modes)
        (let* ((mode-dir (expand-file-name mode snippets-dir))
               (snippet-files (directory-files mode-dir t "^[a-zA-Z0-9].*"))
               (migrated-forms nil))
          (dolist (file snippet-files)
            (when (file-regular-p file)
              (condition-case err
                  (let ((form (tempel-migrate--parse-file file)))
                    (push form migrated-forms)
                    (cl-incf total-migrated))
                (error
                 (push (format "Error migrating %s: %s" file (error-message-string err)) errors)))))
          (when migrated-forms
            (insert (format "\n;; %s\n" mode))
            (insert (format "%s\n" mode))
            (dolist (form (nreverse migrated-forms))
              (let ((print-escape-newlines t)
                    (print-quoted t)
                    (print-length nil)
                    (print-level nil))
                (insert "  " (pp-to-string form)))))))

      (message "Successfully migrated %d snippets to %s" total-migrated output-file)
      (when errors
        (message "\nWarnings/Errors during migration:")
        (dolist (err errors)
          (message "  %s" err))))
    t))

(provide 'migrate-snippets)
