(setq use-package-compute-statistics t)

(defun with-hook-timing (inner &rest args)
  (mapc (lambda (it)
          (with-slow-op-timer (format "<hook> %s" it)
            (funcall inner it)))
        args))

(advice-add 'run-hooks :around 'with-hook-timing)
(advice-add 'run-hooks-with-args :around 'with-hook-timing)


(cl-defmacro which-key-customize (new-text &key map key form)
  "Register a which-key annotation, deferred until which-key is loaded.

NEW-TEXT is the replacement label (a string, or a cons (LABEL . COMMAND)
for keymap-based replacements that also bind a prefix command).
:KEY  — key sequence string; required unless :FORM is used.
:MAP  — keymap (symbol or quoted symbol); uses the keymap-scoped API when
        available, falling back to the global-key variant otherwise.
:FORM — arbitrary expression; mutually exclusive with NEW-TEXT, :KEY, and :MAP.

All constraints are validated at macro-expansion time."
  (declare (indent 1))
  (cond
   (form
    (when key
      (user-error "which-key-customize: :form is mutually exclusive with :key"))
    (when map
      (user-error "which-key-customize: :form is mutually exclusive with :map"))
    (when new-text
      (user-error "which-key-customize: :form is mutually exclusive with new-text")))
   (t
    (unless new-text
      (user-error "which-key-customize: new-text is required when :form is not provided"))
    (unless key
      (user-error "which-key-customize: :key is required when :form is not provided"))
    (unless (stringp key)
      (user-error "which-key-customize: :key must be a string literal, got: %S" key))))
  (cond
   (form `(with-eval-after-load 'which-key ,form))
   (map `(with-eval-after-load 'which-key
           (if (and ,map (fboundp 'which-key-add-keymap-based-replacements))
               (which-key-add-keymap-based-replacements ,map ,key ,new-text)
             (which-key-add-key-based-replacements ,key ,new-text))))
   (t `(with-eval-after-load 'which-key
         (which-key-add-key-based-replacements ,key ,new-text)))))
