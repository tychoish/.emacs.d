  ;; magit-20260609+ uses `thread$' from cond-let which requires cond-let>=0.3;
  ;; the installed cond-let doesn't define cond-let--thread$ yet.  Redefine the
  ;; one affected function using nested calls until cond-let catches up.
  ;; `magit--with-refresh-cache' (vendored, not ours) expands to `incf', which
  ;; triggers an obsolete-alias warning at this call site; suppressed since
  ;; there's no local fix for a third-party macro's expansion.
  (with-suppressed-warnings ((obsolete incf))
    (defun magit-config-get-from-cached-list (key)
      (gethash
       (replace-regexp-in-string "[^.]+\\'" #'downcase
         (replace-regexp-in-string "\\`[^.]+" #'downcase key t t)
         t t)
       (magit--with-refresh-cache (cons (magit-toplevel) 'config)
         (let ((configs (make-hash-table :test #'equal)))
           (dolist (conf (magit-git-items "config" "--list" "-z"))
             (let* ((nl-pos (cl-position ?\n conf))
                    (key (substring conf 0 nl-pos))
                    (val (if nl-pos (substring conf (1+ nl-pos)) "")))
               (puthash key (nconc (gethash key configs) (list val)) configs)))
           configs)))))

