;;; Config analysis

(defun bootstrap-core-use-package-sizes ()
  "Return (PACKAGE-NAME . LINE-COUNT) pairs for every top-level use-package
block in tychoish-core.el, sorted by LINE-COUNT descending."
  (let ((file (expand-file-name "lisp/tychoish-core.el" user-emacs-directory))
        results)
    (with-temp-buffer
      (set-syntax-table emacs-lisp-mode-syntax-table)
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward "^(use-package \\([^ \t\n]+\\)" nil t)
        (let* ((name (match-string-no-properties 1))
               (start (match-beginning 0)))
          (goto-char start)
          (condition-case nil
              (progn
                (forward-sexp 1)
                (push (cons name (count-lines start (point))) results))
            (scan-error
             (forward-line 1))))))
    (sort results (lambda (a b) (> (cdr a) (cdr b))))))

;;;###autoload
(defun bootstrap-core-use-package-sizes-report ()
  "Display use-package blocks from tychoish-core.el sorted by line count."
  (interactive)
  (let* ((results (bootstrap-core-use-package-sizes))
         (buf (get-buffer-create "*use-package-sizes*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%-40s %s\n" "Package" "Lines"))
        (insert (make-string 48 ?-) "\n")
        (seq-do (lambda (entry)
                  (insert (format "%-40s %d\n" (car entry) (cdr entry))))
                results)
        (goto-char (point-min)))
      (special-mode))
    (pop-to-buffer buf)))
