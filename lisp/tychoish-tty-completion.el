;;; tychoish-tty-completion.el --- consult-based TTY completion-at-point -*- lexical-binding: t; -*-

;;; Commentary:
;; Routes completion-at-point through consult-completion-in-region for TTY
;; frames, keeping corfu for GUI frames sharing the same daemon.  Enhances
;; vertico annotations with aligned type info and inline doc strings.
;;
;; Load this file to activate.  Unload with M-x unload-feature to revert.
;; Corfu auto-popup is suppressed in TTY frames; use manual C-<tab> there.

;;; Code:

(require 'consult)

(defun tychoish--tty-candidate-doc (candidate doc-fn)
  "Return single-line doc for CANDIDATE via DOC-FN (:company-doc-buffer)."
  (ignore-errors
    (when-let* ((result (funcall doc-fn candidate))
                (buf (if (stringp result) (get-buffer result) result))
                (_ (buffer-live-p buf)))
      (with-current-buffer buf
        (thread-last
          (buffer-substring-no-properties (point-min) (point-max))
          string-trim
          (replace-regexp-in-string "[\n\r\t]+" " ")
          (replace-regexp-in-string "[[:space:]]+" " ")
          (truncate-string-to-width 80 0 nil "…"))))))

(defun tychoish-tty-completion-in-region (start end collection &optional predicate)
  "Consult-based completion-in-region for TTY with aligned type+doc annotations."
  (let* ((extra completion-extra-properties)
         (metadata (completion-metadata
                    (buffer-substring-no-properties start end)
                    collection predicate))
         (orig-aff-fn (or (plist-get extra :affixation-function)
                          (completion-metadata-get metadata 'affixation-function)))
         (orig-ann-fn (or (plist-get extra :annotation-function)
                          (completion-metadata-get metadata 'annotation-function)))
         (doc-fn (plist-get extra :company-doc-buffer)))
    (let ((completion-extra-properties
           (plist-put (copy-sequence extra)
                      :affixation-function
                      (lambda (candidates)
                        (seq-map
                         (lambda (item)
                           (let* ((candidate (nth 0 item))
                                  (prefix (nth 1 item))
                                  (suffix (string-trim (nth 2 item)))
                                  (doc (when doc-fn
                                         (tychoish--tty-candidate-doc candidate doc-fn)))
                                  (new-suffix
                                   (cond
                                    ((and (not (string-empty-p suffix)) doc)
                                     (format "  %-28s  %s"
                                             suffix
                                             (propertize doc 'face 'completions-annotations)))
                                    ((not (string-empty-p suffix))
                                     (format "  %s" suffix))
                                    (doc
                                     (format "  %s"
                                             (propertize doc 'face 'completions-annotations)))
                                    (t ""))))
                             (list candidate prefix new-suffix)))
                         (cond
                          (orig-aff-fn
                           (funcall orig-aff-fn candidates))
                          (orig-ann-fn
                           (seq-map (lambda (c)
                                      (list c "" (or (funcall orig-ann-fn c) "")))
                                    candidates))
                          (t
                           (seq-map (lambda (c) (list c "" "")) candidates))))))))
      (consult-completion-in-region start end collection predicate))))

(defun ad:completion-at-point-tty-dispatch (orig-fn)
  "In TTY frames, route `completion-at-point' through consult."
  (if (display-graphic-p)
      (funcall orig-fn)
    (let ((completion-in-region-function #'tychoish-tty-completion-in-region))
      (funcall orig-fn))))

(advice-add 'completion-at-point :around #'ad:completion-at-point-tty-dispatch)

(defun ad:corfu--auto-complete-tty-guard (orig-fn)
  "Suppress corfu auto-popup in TTY frames."
  (when (display-graphic-p)
    (funcall orig-fn)))

(advice-add 'corfu--auto-complete :around #'ad:corfu--auto-complete-tty-guard)

(provide 'tychoish-tty-completion)
;;; tychoish-tty-completion.el ends here
