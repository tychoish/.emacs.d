lisp/(use-package consult-mu
  :commands (consult-mu)
  :init
  (setq consult-mu-maxnum 200)
  (setq consult-mu-preview-key 'any)
  (setq consult-mu-mark-previewed-as-read nil)
  (setq consult-mu-mark-viewed-as-read t)
  (setq consult-mu-use-wide-reply t)
  (setq consult-mu-headers-template 'tychoish/consult-mu-headers-template)

  (setq consult-mu-saved-searches-dynamics '("#flag:unread"))
  (setq consult-mu-saved-searches-async '("#flag:unread"))

  (defun consult-mu-bookmark ()
    "Select `consult-mu' initial query from mu4e-bookmarks."
    (interactive)
    (let* ((bookmarks (map-into
                       (seq-map (lambda (bm) (cons (plist-get bm :name) bm)) mu4e-bookmarks)
                       '(hash-table :test equal)))
           (annotation-table (map-into
                              (seq-map (lambda (bm)
					 (cons (plist-get bm :name)
                                               (format "[%s] %s"
                                                       (char-to-string (plist-get bm :key))
                                                       (plist-get bm :query))))
                                       mu4e-bookmarks)
                              '(hash-table :test equal)))
           (selection (annotated-completing-read
                       annotation-table
                       :prompt "mu4e query =>> "
                       :category 'consult-mu)))
      (consult-mu (plist-get (map-elt bookmarks selection) :query))))
  :config
  (with-slow-op-timer
    "<mail.el> consult-mu extensions"
    (add-to-list 'load-path (expand-file-name "external/consult-mu/extras/" user-emacs-directory))
    (require 'consult-mu-compose)
    (require 'consult-mu-contacts)
    (require 'consult-mu-embark)
    (setq consult-mu-compose-use-dired-attachment 'in-dired)
    (setq consult-mu-compose-preview-key "M-o")
    (setq consult-mu-contacts-ignore-case-fold-search t)
    (setq consult-mu-contacts-ignore-list '("^.*no.*reply.*"))
    ;; the order of the following remains important:
    (setq consult-mu-embark-attach-file-key "C-a")
    (require 'consult-mu-compose-embark))

  (defun tychoish/consult-mu-headers-template ()
    (concat "%f" (number-to-string
		  (floor (* (frame-width) 0.15)))
	    "%s" (number-to-string (floor (* (frame-width) 0.5)))
	    "%d13" "%g" "%x")))
