(setq use-package-compute-statistics t)

(defun with-hook-timing (inner &rest args)
  (mapc (lambda (it)
          (with-slow-op-timer (format "<hook> %s" it)
            (funcall inner it)))
        args))

(advice-add 'run-hooks :around 'with-hook-timing)
(advice-add 'run-hooks-with-args :around 'with-hook-timing)

