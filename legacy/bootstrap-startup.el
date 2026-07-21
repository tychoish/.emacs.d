
(defun ad:suppress-message (f &rest arg)
  (with-silence
    (apply f arg)))


(when (fboundp 'emacs-repository-branch-git)
  (advice-add 'emacs-repository-branch-git :around #'ad:suppress-message))

(when (fboundp 'emacs-repository-version-git)
  (advice-add 'emacs-repository-version-git :around #'ad:suppress-message))

(defun emacs-repository-version-git (_dir)
  "Noop definition of function to speed up startup" "")

(defun emacs-repository-get-version (&optional _dir _ext)
  "Noop definition of function to speed up startup" "")
