;;; deduplicate-templates.el --- Compare and deduplicate migrated snippets against tempel-collection -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'subr-x)
(require 'tempel)

(defun tempel-deduplicate--load-collection-templates (collection-dir)
  "Load all templates from the tempel-collection eld files in COLLECTION-DIR.
Returns an alist of (mode . ((key . elements)))."
  (let ((eld-files (directory-files collection-dir t "\\.eld\\'"))
        (collection-data nil))
    (dolist (file eld-files)
      (condition-case nil
          (let* ((file-content (tempel--file-read file))
                 (mode-name (file-name-base file)))
            ;; Normalize mode name (e.g. "org" to "org-mode")
            (unless (string-suffix-p "-mode" mode-name)
              (setq mode-name (concat mode-name "-mode")))
            (let ((mode-sym (intern mode-name))
                  (templates nil))
              (dolist (item file-content)
                (let ((modes (car item))
                      (tpls (cddr item)))
                  (when (or (eq modes mode-sym)
                            (and (listp modes) (memq mode-sym modes)))
                    (dolist (tpl tpls)
                      (push (cons (car tpl) (cdr tpl)) templates)))))
              (when templates
                (push (cons mode-sym templates) collection-data))))
        (error nil)))
    collection-data))

(defun tempel-deduplicate--load-migrated-templates (migrated-file)
  "Load migrated templates from MIGRATED-FILE.
Returns an alist of (mode . ((key . elements)))."
  (with-temp-buffer
    (insert-file-contents migrated-file)
    (goto-char (point-min))
    (let ((migrated-data nil)
          (current-mode nil)
          (current-templates nil))
      (while (not (eobp))
        (let ((form (condition-case nil
                        (read (current-buffer))
                      (error nil))))
          (when form
            (cond
             ((symbolp form)
              ;; It's a mode symbol, e.g. 'org-mode
              (when current-mode
                (push (cons current-mode (nreverse current-templates)) migrated-data)
                (setq current-templates nil))
              (setq current-mode form))
             ((listp form)
              ;; It's a template definition, e.g. '(key ...)
              (when current-mode
                (push (cons (car form) (cdr form)) current-templates)))))))
      (when current-mode
        (push (cons current-mode (nreverse current-templates)) migrated-data))
      (nreverse migrated-data))))

(defun tempel-deduplicate-all (&optional migrated-file collection-dir output-file)
  "Compare MIGRATED-FILE with COLLECTION-DIR templates, deduplicate, and write to OUTPUT-FILE."
  (interactive)
  (let* ((migrated-file (or migrated-file (expand-file-name "templates.migrated" user-emacs-directory)))
         (output-file (or output-file (expand-file-name "templates" user-emacs-directory)))
         ;; Locate tempel-collection directory in elpa/
         (elpa-dir (expand-file-name "elpa" user-emacs-directory))
         (coll-dir (or collection-dir
                       (let ((base-dir (car (directory-files elpa-dir t "^tempel-collection-[0-9].*"))))
                         (when base-dir
                           (expand-file-name "templates" base-dir)))))
         (collection-templates (tempel-deduplicate--load-collection-templates coll-dir))
         (migrated-templates (tempel-deduplicate--load-migrated-templates migrated-file))
         (custom-templates nil)
         (exact-duplicates 0)
         (customized-duplicates 0)
         (unique-templates 0)
         (comparison-report nil))
    
    (unless (file-exists-p migrated-file)
      (error "Migrated file does not exist: %s" migrated-file))
    (unless (and coll-dir (file-directory-p coll-dir))
      (error "Tempel-collection directory not found: %s" coll-dir))
    
    (dolist (mode-group migrated-templates)
      (let* ((mode (car mode-group))
             (tpls (cdr mode-group))
             (coll-tpls (cdr (assoc mode collection-templates)))
             (mode-custom nil))
        (dolist (tpl tpls)
          (let* ((key (car tpl))
                 (elements (cdr tpl))
                 (coll-match (assoc key coll-tpls)))
            (cond
             ((null coll-match)
              ;; Unique to migrated custom templates
              (push tpl mode-custom)
              (cl-incf unique-templates)
              (push (format "  [UNIQUE] %s: %s" mode key) comparison-report))
             ((equal elements (cdr coll-match))
              ;; Exact duplicate of tempel-collection template
              (cl-incf exact-duplicates)
              (push (format "  [DUPLICATE] %s: %s (discarded - identical to collection)" mode key) comparison-report))
             (t
              ;; Customized duplicate (same key, different contents)
              (push tpl mode-custom)
              (cl-incf customized-duplicates)
              (push (format "  [CUSTOMIZED] %s: %s (retained - different from collection)" mode key) comparison-report)))))
        (when mode-custom
          (push (cons mode (nreverse mode-custom)) custom-templates))))
    
    ;; Write the deduplicated custom templates to OUTPUT-FILE
    (with-temp-file output-file
      (insert ";;; -*- mode: lisp-data -*-\n\n")
      (dolist (mode-group (nreverse custom-templates))
        (let ((mode (car mode-group))
              (tpls (cdr mode-group)))
          (insert (format "%s\n" mode))
          (dolist (tpl tpls)
            (let ((form (cons (car tpl) (cdr tpl)))
                  (print-escape-newlines t)
                  (print-quoted t)
                  (print-length nil)
                  (print-level nil))
              (insert "  " (pp-to-string form))))
          (insert "\n"))))
    
    ;; Print summary
    (message "\n=== DEDUPLICATION COMPARISON SUMMARY ===")
    (message "  Unique custom templates:      %d" unique-templates)
    (message "  Customized key templates:     %d" customized-duplicates)
    (message "  Identical duplicate templates: %d (discarded)" exact-duplicates)
    (message "  Total custom templates saved:  %d to %s" (+ unique-templates customized-duplicates) output-file)
    (message "\n=== DETAILED COMPARISON REPORT ===")
    (dolist (line (nreverse comparison-report))
      (message "%s" line))
    t))

(provide 'deduplicate-templates)
