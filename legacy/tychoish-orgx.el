;;;###autoload
(cl-defun tychoish/org-capture-add-routine-templates
    (&key name (key "") (path (concat (f-make-slug name) ".org")))

  (when (string-equal "r" key)
    (user-error "cannot define routine (loops) %s org-capture-templates with key `r'" name))

  (when-let* ((description (format "%s routines <%s>" name (file-name-nondirectory path)))
	      (_ (not (string-equal "" key))))
    (add-to-list 'org-capture-templates (list (concat "r" key) description))
    (add-to-list 'org-capture-templates (list (concat key "r") description)))

  (thread-last '(("1d" .  "Daily")
                 ("1w" .  "Weekly")
                 ("4w" . "Monthly")
                 ("12w" . "Quarterly")
                 ("26w" . "Half Yearly")
                 ("52w" . "Yearly"))
               (seq-mapcat
                (lambda (it)
                  (let* ((interval (car it))
                         (heading (cdr it))
                         (interval-key (downcase (substring-no-properties heading 0 1))))
                    (thread-last (list (concat key "r" interval-key)
                                       (concat "r" key interval-key))
                                 (seq-map (lambda (prefix)
                                            (cons prefix (cons interval heading))))))))
               (seq-do
                (lambda (it)
                  (let* ((prefix (car it))
                         (interval (cadr it))
                         (heading (cddr it))
                         (lower-heading (downcase heading))
                         (menu-name (format "routine (%s; %s)" name lower-heading)))
                    (add-to-list
                     'org-capture-templates
                     (list prefix menu-name
                           'entry (list 'file+olp path "Loops" heading)
                           (concat "* %(~title~)\nSCHEDULED: <%(org-read-date nil nil \"++" interval "\") ++" interval ">\n%?")
                           :prepend t
                           :kill-buffer t
                           :empty-lines-after 1)))))))
