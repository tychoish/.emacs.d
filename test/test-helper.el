;;; test-helper.el --- ERT test infrastructure -*- lexical-binding: t; -*-

;;; Commentary:
;; Loaded by ert-runner before any test files.
;; Adds lisp/ and elpa/* to load-path so test files can require local modules.

;;; Code:

(let* ((test-file (or load-file-name buffer-file-name))
       (test-dir (file-name-directory test-file))
       (root (file-name-directory (directory-file-name test-dir))))
  (add-to-list 'load-path (expand-file-name "lisp" root))
  (dolist (dir (directory-files (expand-file-name "elpa" root) t "\\`[^.]"))
    (when (file-directory-p dir)
      (add-to-list 'load-path dir))))

(provide 'test-helper)
;;; test-helper.el ends here
