;;; test-helper.el --- ERT test infrastructure -*- lexical-binding: t; -*-

;;; Commentary:
;; Loaded by ert-runner before any test files.
;; Adds lisp/ to load-path so test files can require local modules directly.

;;; Code:

(let ((root (file-name-directory
             (directory-file-name
              (file-name-directory (or load-file-name buffer-file-name))))))
  (add-to-list 'load-path (expand-file-name "lisp" root)))

(provide 'test-helper)
;;; test-helper.el ends here
