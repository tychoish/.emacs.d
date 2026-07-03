;;; legacy/denote.el --- Denote-menu configuration (not loaded) -*- lexical-binding: t; -*-

;;; Commentary:
;; denote-menu 1.4.0 is not compatible with Denote 4.x without the shims
;; below.  This block is parked here until denote-menu is updated or replaced.
;; To re-enable, move the use-package block back into tychoish-core.el and
;; restore the denote-dispatch "fl" suffix for list-notes.

;;; Code:

(use-package denote-menu
  :ensure t
  :after denote
  :commands (denote-menu-list-notes list-denotes)
  :config
  ;; denote-menu 1.4.0 was written for the old YYYYMMDDTHHMMSS identifier
  ;; format. Denote 4.x uses YYYY-MM-DD.HHMMSS, which breaks date parsing
  ;; and the "-"-separated unique identifier (hyphens now appear in IDs).
  (defun denote-menu-date (path)
    "Return human readable date from denote PATH identifier."
    (let* ((id (denote-retrieve-filename-identifier path))
           (parts (split-string id "[T.]" t))
           (date-str (car parts))
           (time-str (cadr parts))
           (date-parts (split-string date-str "-")))
      (if (> (length date-parts) 1)
          (format "%s-%s-%s %s:%s"
                  (nth 0 date-parts) (nth 1 date-parts) (nth 2 date-parts)
                  (substring time-str 0 2) (substring time-str 2 4))
        (format "%s-%s-%s %s:%s"
                (substring date-str 0 4) (substring date-str 4 6)
                (substring date-str 6 8)
                (substring time-str 0 2) (substring time-str 2 4)))))

  (defun denote-menu--path-to-unique-identifier (path)
    "Convert PATH to a unique identifier for `tabulated-list-entries'."
    (format "%s|%s"
            (denote-retrieve-filename-identifier path)
            (file-name-extension path)))

  (defun denote-menu--entries-to-paths ()
    "Return list of file paths present in the *Denote* buffer."
    (mapcar (lambda (entry)
              (let* ((parts (split-string (car entry) "|"))
                     (id (car parts))
                     (ext (cadr parts)))
                (denote-menu-get-path-by-id id ext)))
            (funcall tabulated-list-entries)))

  (defun denote-menu--entries-to-filenames ()
    "Return list of file names present in the *Denote* buffer."
    (mapcar (lambda (entry)
              (let* ((parts (split-string (car entry) "|"))
                     (id (car parts))
                     (ext (cadr parts)))
                (file-name-nondirectory (denote-menu-get-path-by-id id ext))))
            (funcall tabulated-list-entries)))

  (defun denote-menu-list-notes ()
    "Display list of Denote files in variable `denote-directory'."
    (interactive)
    (let* ((short-name (file-name-nondirectory (directory-file-name denote-directory)))
           (denote-menu-buffer-name (format "*[%s]*" short-name))
           (local-denote-directory denote-directory))
      (if (get-buffer denote-menu-buffer-name)
          (pop-to-buffer-same-window denote-menu-buffer-name)
        (let ((buffer (get-buffer-create denote-menu-buffer-name)))
          (with-current-buffer buffer
            (setq buffer-file-coding-system 'utf-8)
            (setq denote-menu-current-regex denote-menu-initial-regex)
            (denote-menu-mode)
            (setq-local denote-directory local-denote-directory)
            (denote-menu-update-entries))
          (pop-to-buffer-same-window buffer)))))

  (add-hook 'denote-menu-mode-hook
            (lambda () (setq mode-name "denote-menu"))))

(provide 'legacy/denote)
;;; legacy/denote.el ends here
