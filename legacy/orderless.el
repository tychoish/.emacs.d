(use-package orderless
  :ensure t
  :after (vertico)
  :defer t
  :config
  ;; Orderless's own behavior knobs only; cross-cutting `completion-styles' and
  ;; `completion-category-overrides' are owned by the completion-flavor system.
  (setq orderless-component-separator #'orderless-escapable-split)
  (setq orderless-matching-styles
	'(orderless-literal orderless-prefixes orderless-initialism orderless-regexp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; completion flavor -- switch between ordereless / prescient / hybrid at runtime.

(defvar bootstrap-completion-flavor 'hybrid
  "Currently active completion flavor.
One of `hybrid', `orderless', `prescient'.  Set by the
`bootstrap-completion-use-*' commands; do not setq directly.")

(defvar bootstrap-completion-flavors
  '((hybrid bootstrap-completion-use-hybrid
	    "orderless filter + prescient sort (frecency)")
    (orderless bootstrap-completion-use-orderless
	       "pure orderless filter; default sort, no frecency")
    (prescient bootstrap-completion-use-prescient
	       "prescient filter + sort (frecency)"))
  "Alist of (NAME ACTIVATOR DESCRIPTION) for completion flavors.
ACTIVATOR is the interactive command that installs the flavor.")

(defvar bootstrap-completion--applying nil
  "Re-entry guard for the `bootstrap-completion-use-*' functions.
Cycling `vertico-prescient-mode' / `corfu-prescient-mode' fires their
mode hooks, which can re-invoke a flavor function (e.g. via the
startup one-shot hook below).  The guard makes the inner call a no-op.")

(defun bootstrap-completion--set-category-overrides (kind)
  "Set `completion-category-overrides' for KIND (`orderless' or `prescient')."
  (setq completion-category-overrides
	(pcase kind
	  ('orderless '((file (styles basic partial-completion))
			(consult-grep (styles basic))
			(buffer (styles orderless basic))
			(command (styles orderless basic))
			(symbol (styles orderless basic))))
	  ('prescient '((file (styles basic partial-completion))
			(consult-grep (styles basic)))))))

(defun bootstrap-completion--reload-prescient-mode (mode-symbol)
  "Cycle MODE-SYMBOL off and back on so it picks up new `*-enable-*' values.
`vertico-prescient' and `corfu-prescient' read the filtering/sorting flags
only at mode activation, so changing the variables alone has no effect
on an already-enabled mode."
  (when (fboundp mode-symbol)
    (when (symbol-value mode-symbol)
      (funcall mode-symbol -1))
    (funcall mode-symbol 1)))

(defmacro bootstrap-completion--with-guard (&rest body)
  "Run BODY with `bootstrap-completion--applying' bound non-nil.
If already non-nil (we are re-entering from a prescient mode hook),
BODY is skipped."
  (declare (indent defun))
  `(unless bootstrap-completion--applying
     (let ((bootstrap-completion--applying t))
       ,@body)))

(defun bootstrap-completion-use-hybrid ()
  "Install the hybrid flavor: orderless filters, prescient sorts."
  (interactive)
  (bootstrap-completion--with-guard
    (setq completion-styles '(orderless basic))
    (bootstrap-completion--set-category-overrides 'orderless)
    (when (boundp 'vertico-prescient-enable-filtering)
      (setq vertico-prescient-enable-filtering nil
	    vertico-prescient-enable-sorting   t))
    (when (boundp 'corfu-prescient-enable-filtering)
      (setq corfu-prescient-enable-filtering nil
	    corfu-prescient-enable-sorting   t))
    (setq completion-preview-sort-function #'prescient-completion-sort)
    (bootstrap-completion--reload-prescient-mode 'vertico-prescient-mode)
    (bootstrap-completion--reload-prescient-mode 'corfu-prescient-mode)
    (setq bootstrap-completion-flavor 'hybrid)
    (message "completion: orderless filter + prescient sort")))

(defun bootstrap-completion-use-orderless ()
  "Install pure orderless; prescient disabled (no frecency)."
  (interactive)
  (bootstrap-completion--with-guard
    (setq completion-styles '(orderless basic))
    (bootstrap-completion--set-category-overrides 'orderless)
    (setq completion-preview-sort-function nil)
    (when (boundp 'vertico-prescient-enable-filtering)
      (setq vertico-prescient-enable-filtering nil))
    (when (boundp 'corfu-prescient-enable-filtering)
      (setq corfu-prescient-enable-filtering nil))
    (when (fboundp 'vertico-prescient-mode) (vertico-prescient-mode -1))
    (when (fboundp 'corfu-prescient-mode)   (corfu-prescient-mode -1))
    (setq bootstrap-completion-flavor 'orderless)
    (message "completion: pure orderless")))

(defun bootstrap-completion-use-prescient ()
  "Install prescient for both filter and sort; orderless inert."
  (interactive)
  (bootstrap-completion--with-guard
    (setq completion-styles '(basic partial-completion emacs22))
    (bootstrap-completion--set-category-overrides 'prescient)
    (when (boundp 'vertico-prescient-enable-filtering)
      (setq vertico-prescient-enable-filtering t
	    vertico-prescient-enable-sorting   t))
    (when (boundp 'corfu-prescient-enable-filtering)
      (setq corfu-prescient-enable-filtering t
	    corfu-prescient-enable-sorting   t))
    (setq completion-preview-sort-function #'prescient-completion-sort)
    (bootstrap-completion--reload-prescient-mode 'vertico-prescient-mode)
    (bootstrap-completion--reload-prescient-mode 'corfu-prescient-mode)
    (setq bootstrap-completion-flavor 'prescient)
    (message "completion: prescient filter + sort")))

(defun bootstrap-completion-select-flavor ()
  "Pick a completion flavor via `annotated-completing-read'."
  (interactive)
  (let* ((name (annotated-completing-read
                (seq-map (lambda (entry)
                           (cons (symbol-name (car entry))
                                 (concat (if (eq (car entry) bootstrap-completion-flavor) "[active] " "")
                                         (nth 2 entry))))
                         bootstrap-completion-flavors)
		:prompt "completion flavor => "
		:category 'bootstrap-completion-flavor
		:require-match t))
	 (entry (assq (intern name) bootstrap-completion-flavors)))
    (when entry (funcall (nth 1 entry)))))
