;;; hud-modeline.el --- Lightweight modeline -*- lexical-binding: t; -*-

;;; Commentary:
;; Lightweight modeline replacing doom-modeline.
;; Uses nerd-icons (terminal and GUI), integrates flycheck, eglot, vc, projectile.
;;
;; Segments are stored as alists: ((KEY . FUNCTION) ...).  Keys are symbols
;; used for lookup and override; only the function values are called during
;; rendering.
;;
;; Public segment management API:
;;   `hud-modeline-add-segment'        — add a new segment, optionally after a key
;;   `hud-modeline-remove-segment'     — remove a segment entirely
;;   `hud-modeline-override-segment'   — replace an existing segment's function
;;   `hud-modeline-disable-segment'    — suppress a segment without removing it
;;   `hud-modeline-reset-segment'      — restore one segment to its default
;;   `hud-modeline-reset-all-segments' — restore all segments to defaults
;;
;; External packages and use-package blocks should use these functions rather
;; than modifying `hud-modeline-left-segments' / `hud-modeline-right-segments'
;; directly.

;;; Code:

(require 'subr-x)
(require 'map)
(require 'seq)

(declare-function flycheck-count-errors "flycheck")
(declare-function nerd-icons-codicon "nerd-icons")
(declare-function nerd-icons-octicon "nerd-icons")
(declare-function nerd-icons-icon-for-buffer "nerd-icons")
(declare-function projectile-project-name "projectile")
(declare-function projectile-project-root "projectile")
(declare-function mouse-major-mode-menu "mouse")
(declare-function mouse-buffer-menu "mouse")

;;;; Customization

(defgroup hud-modeline nil
  "Lightweight custom modeline."
  :group 'mode-line)

(defcustom hud-modeline-icons t
  "Show nerd-icons in the mode line."
  :type 'boolean
  :group 'hud-modeline)

(defcustom hud-modeline-mode-max-length 12
  "Maximum characters for major mode name before truncation."
  :type 'integer
  :group 'hud-modeline)

(defcustom hud-modeline-show-buffer-size t
  "Show total buffer size (chars) in the mode line."
  :type 'boolean
  :group 'hud-modeline)

(defcustom hud-modeline-misc-max-width nil
  "Default maximum display-column width for the misc segment, or nil for no cap.
Can be overridden per-registration by passing :max-width to the segment constructor.
See `hud-modeline--misc-segment'."
  :type '(choice integer (const :tag "Unlimited" nil))
  :group 'hud-modeline)

;;;; Faces

(defface hud-modeline-modified
  '((t :inherit warning :weight bold :underline nil))
  "Face for modified buffer indicator."
  :group 'hud-modeline)

(defface hud-modeline-read-only
  '((t :inherit shadow :underline nil))
  "Face for read-only buffer indicator."
  :group 'hud-modeline)

(defface hud-modeline-vc
  '((t :inherit success :underline nil))
  "Face for vc branch name."
  :group 'hud-modeline)

(defface hud-modeline-error
  '((t :inherit error :weight bold :underline nil))
  "Face for flycheck error count."
  :group 'hud-modeline)

(defface hud-modeline-warning
  '((t :inherit warning :weight bold :underline nil))
  "Face for flycheck warning count."
  :group 'hud-modeline)

(defface hud-modeline-lsp
  '((t :inherit mode-line-emphasis :underline nil))
  "Face for active LSP (eglot) indicator."
  :group 'hud-modeline)

(defface hud-modeline-buffer-name
  '((t :inherit mode-line-buffer-id :underline nil))
  "Face for the buffer name in the mode line."
  :group 'hud-modeline)

(defface hud-modeline-position
  '((t :inherit shadow :underline nil))
  "Face for line/column position."
  :group 'hud-modeline)

;;;; Segment keymaps

(defvar hud-modeline--debug-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'toggle-debug-on-error)
    map)
  "Keymap for the debug-on-error indicator.")

(defvar hud-modeline--buffer-name-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'next-buffer)
    (define-key map [mode-line mouse-3] #'mouse-buffer-menu)
    map)
  "Keymap for the buffer name segment.")

(defvar hud-modeline--major-mode-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'mouse-major-mode-menu)
    (define-key map [mode-line mouse-2] #'describe-mode)
    (define-key map [mode-line mouse-3] #'mouse-major-mode-menu)
    map)
  "Keymap for the major mode name.")

;;;; Icon cache

(defvar-local hud-modeline--icon-cache nil
  "Cached nerd-icon for the current buffer's major mode.")

(defun hud-modeline--invalidate-icon ()
  "Invalidate the icon cache when the major mode changes."
  (setq hud-modeline--icon-cache nil))

(defun hud-modeline--icon ()
  "Return file-type icon string or nil."
  (when (and hud-modeline-icons (fboundp 'nerd-icons-icon-for-buffer))
    (or hud-modeline--icon-cache
        (setq hud-modeline--icon-cache (nerd-icons-icon-for-buffer)))))

;;;; Internal segment helpers
;;
;; These are called by the public segment functions below.  They are not
;; intended to appear directly in a segments alist.

(defun hud-modeline--buffer-state ()
  "Return icon or text for modified/read-only state, nil when clean."
  (cond
   (buffer-read-only
    (if (and hud-modeline-icons (fboundp 'nerd-icons-octicon))
        (nerd-icons-octicon "nf-oct-lock" :face 'hud-modeline-read-only)
      (propertize "RO" 'face 'hud-modeline-read-only)))
   ((buffer-modified-p)
    (if (and hud-modeline-icons (fboundp 'nerd-icons-octicon))
        (nerd-icons-octicon "nf-oct-pencil" :face 'hud-modeline-modified)
      (propertize "!" 'face 'hud-modeline-modified)))
   (t nil)))

(defun hud-modeline--vc ()
  "Return vc branch string with icon, or nil."
  (when-let* ((raw vc-mode)
              (branch (string-trim
                       (replace-regexp-in-string
                        "\\`[[:space:]]*[A-Za-z]+[-:]" "" raw))))
    (unless (string-empty-p branch)
      (let ((icon (when (and hud-modeline-icons (fboundp 'nerd-icons-octicon))
                    (nerd-icons-octicon "nf-oct-git_branch" :face 'hud-modeline-vc))))
        (if icon
            (concat icon " " (propertize branch 'face 'hud-modeline-vc))
          (propertize branch 'face 'hud-modeline-vc))))))

(defun hud-modeline--major-mode ()
  "Return the major mode name as a string.
Handles delight's 3-element (INHIBIT-VAR ORIGINAL LIGHTER) form, which the
C mode-line renderer resolves correctly but `format-mode-line' (Lisp) does
not: `format-mode-line' silently drops the ELSE/LIGHTER branch, returning
empty string.  This function reads the lighter directly.

Must be called OUTSIDE any `format-mode-line' invocation: delight's advice
around `format-mode-line' binds `delight-mode-name-inhibit' to a non-nil
value to suppress lighters in non-mode-line contexts.  Calling here, before
`format-mode-line', sees the variable as void and returns the lighter."
  (let* ((raw mode-name)
         (name (cond
                ((stringp raw) raw)
                ((and (listp raw) (= (length raw) 3))
                 (if (and (symbolp (car raw))
                          (boundp (car raw))
                          (symbol-value (car raw)))
                     (nth 1 raw)
                   (nth 2 raw)))
                (t (format-mode-line raw)))))
    (if (and (stringp name) (> (length name) hud-modeline-mode-max-length))
        (concat (substring name 0 (1- hud-modeline-mode-max-length)) "…")
      (or name ""))))

;;;; Segment functions
;;
;; Each function takes no arguments and returns a string or nil.
;; nil means the segment contributes nothing to the rendered mode line.
;; These are the values stored in the segments alists.

(defun hud-modeline--state-segment ()
  "Combine buffer state and file icon into a prefix segment."
  (let ((state (hud-modeline--buffer-state))
        (icon (hud-modeline--icon)))
    (cond
     ((and state icon) (concat state " " icon " "))
     (state (concat state " "))
     (icon (concat icon " "))
     (t ""))))

(defun hud-modeline--buffer-name ()
  "Return buffer name relative to project root when available."
  (propertize
   (if-let* ((fname (buffer-file-name))
             (root (and (bound-and-true-p projectile-mode)
                        (fboundp 'projectile-project-root)
                        (projectile-project-root))))
       (file-relative-name fname root)
     (buffer-name))
   'face 'hud-modeline-buffer-name
   'local-map hud-modeline--buffer-name-keymap
   'mouse-face 'mode-line-highlight
   'help-echo "mouse-1: next buffer\nmouse-3: buffer menu"))

(defun hud-modeline--separator ()
  "Return a single space separator."
  " ")

(defun hud-modeline--debug-segment ()
  "Return a bug icon with trailing space when `debug-on-error' is active."
  (when debug-on-error
    (let ((icon (if (and hud-modeline-icons (fboundp 'nerd-icons-codicon))
                    (nerd-icons-codicon "nf-cod-bug" :face 'hud-modeline-error)
                  (propertize "bug" 'face 'hud-modeline-error))))
      (concat (propertize icon
                          'local-map hud-modeline--debug-keymap
                          'mouse-face 'mode-line-highlight
                          'help-echo "debug-on-error active\nmouse-1: toggle")
              " "))))

(defun hud-modeline--mode-segment ()
  "Render mode brackets: %[( MAJOR MINOR %n )%].
Calls `hud-modeline--major-mode' before entering `format-mode-line' to
ensure delight lighters are resolved before delight's advice activates.
The major mode name is clickable: mouse-1/3 shows the mode menu, mouse-2 describes it."
  (let* ((major (hud-modeline--major-mode))
         (major-display (if (stringp major)
                            (propertize major
                                        'local-map hud-modeline--major-mode-keymap
                                        'mouse-face 'mode-line-highlight
                                        'help-echo "mouse-1: mode menu\nmouse-2: describe mode")
                          major)))
    (format-mode-line
     `("%[" "(" ,major-display ("" mode-line-process) ("" minor-mode-alist) "%n" ")" "%]"))))

(defun hud-modeline--flycheck ()
  "Return flycheck error/warning counts with icons or nil."
  (when (and (bound-and-true-p flycheck-mode)
             flycheck-current-errors)
    (let* ((counts (flycheck-count-errors flycheck-current-errors))
           (errs (or (alist-get 'error counts) 0))
           (warns (or (alist-get 'warning counts) 0)))
      (when (or (> errs 0) (> warns 0))
        (concat
         (when (> errs 0)
           (concat
            (if (and hud-modeline-icons (fboundp 'nerd-icons-codicon))
                (nerd-icons-codicon "nf-cod-error" :face 'hud-modeline-error)
              (propertize "✖" 'face 'hud-modeline-error))
            (propertize (number-to-string errs) 'face 'hud-modeline-error)
            " "))
         (when (> warns 0)
           (concat
            (if (and hud-modeline-icons (fboundp 'nerd-icons-codicon))
                (nerd-icons-codicon "nf-cod-warning" :face 'hud-modeline-warning)
              (propertize "▲" 'face 'hud-modeline-warning))
            (propertize (number-to-string warns) 'face 'hud-modeline-warning)
            " ")))))))

(defun hud-modeline--eglot ()
  "Return eglot's own mode-line format string, or nil when eglot is inactive.
Renders the same content eglot would show in mode-line-misc-info so that
callers who suppress eglot's misc-info entry still get the full status."
  (when (and (bound-and-true-p eglot--managed-mode)
             (fboundp 'eglot--mode-line-format))
    (when-let* ((s (format-mode-line '(eglot--managed-mode eglot--mode-line-format)))
                (_ (not (string-empty-p (string-trim s)))))
      (concat s " "))))

(defun hud-modeline--vc-segment ()
  "Return vc branch string with trailing space, or nil."
  (when-let* ((branch (hud-modeline--vc)))
    (concat branch " ")))

(defun hud-modeline--position-segment ()
  "Return formatted line:column position with trailing space."
  (concat (format-mode-line '(:propertize "%l:%c" face hud-modeline-position))
          " "))

(defun hud-modeline--buffer-size ()
  "Return propertized buffer char count or nil when disabled."
  (when hud-modeline-show-buffer-size
    (propertize (file-size-human-readable (buffer-size)) 'face 'shadow)))

(cl-defun hud-modeline--misc-segment (&key (max-width hud-modeline-misc-max-width))
  "Return misc mode-line info, or nil when empty.
With :max-width N truncates to N display columns with a trailing ellipsis.
Defaults to `hud-modeline-misc-max-width' (nil = no cap).
Use a lambda wrapper when registering with a non-default width:
  (lambda () (hud-modeline--misc-segment :max-width 40))"
  (let ((misc (format-mode-line 'mode-line-misc-info)))
    (unless (string-empty-p (string-trim misc))
      (if (and max-width (> (string-width misc) max-width))
          (truncate-string-to-width misc max-width nil nil "…")
        misc))))

(defun hud-modeline--projectile-segment ()
  "Return current project name with a leading separator, or nil.
Shown on the right side when `projectile-mode' is active and the buffer
belongs to a known project."
  (when (and (bound-and-true-p projectile-mode)
             (fboundp 'projectile-project-name))
    (when-let* ((name (projectile-project-name))
                (_ (not (equal name "-"))))
      (concat "| " (propertize name 'face 'hud-modeline-vc) " "))))

;;;; Compilation progress

(defvar hud-modeline--compilation-finished nil
  "Non-nil after at least one compilation has completed this session.")
(defvar hud-modeline--compilation-exit-ok nil
  "Non-nil when the last compilation exited cleanly.")
(defvar hud-modeline--compilation-errors 0
  "Error count from the last finished compilation.")
(defvar hud-modeline--compilation-warnings 0
  "Warning count from the last finished compilation.")

(defun hud-modeline--on-compilation-start (_proc)
  "Force a mode-line redraw when a compilation process starts."
  (force-mode-line-update t))

(defun hud-modeline--on-compilation-finish (buf status)
  "Record compilation result from BUF and STATUS for mode-line display."
  (setq hud-modeline--compilation-finished t
        hud-modeline--compilation-exit-ok (string-prefix-p "finished" status)
        hud-modeline--compilation-errors
        (with-current-buffer buf
          (or (bound-and-true-p compilation-num-errors-found) 0))
        hud-modeline--compilation-warnings
        (with-current-buffer buf
          (or (bound-and-true-p compilation-num-warnings-found) 0)))
  (force-mode-line-update t))

(defun hud-modeline--compilation-segment ()
  "Show compilation running spinner or last exit status.
While a compilation is running shows a spinner with process count.
After completion shows error/warning counts or a check mark on clean exit.
Returns nil when no compilation has run this session."
  (cond
   ((bound-and-true-p compilation-in-progress)
    (let ((n (length compilation-in-progress)))
      (propertize (if (> n 1) (format "⟳%d " n) "⟳ ")
                  'face 'hud-modeline-warning
                  'help-echo "Compilation in progress")))
   (hud-modeline--compilation-finished
    (cond
     ((> hud-modeline--compilation-errors 0)
      (propertize (format "✖%d " hud-modeline--compilation-errors)
                  'face 'hud-modeline-error
                  'help-echo "Last compilation: errors"))
     ((> hud-modeline--compilation-warnings 0)
      (propertize (format "▲%d " hud-modeline--compilation-warnings)
                  'face 'hud-modeline-warning
                  'help-echo "Last compilation: warnings"))
     (hud-modeline--compilation-exit-ok
      (propertize "✓ " 'face 'hud-modeline-vc
                  'help-echo "Last compilation succeeded"))))))

;;;; Segment registries

(defconst hud-modeline--default-left-segments
  '((state . hud-modeline--state-segment)
    (debug . hud-modeline--debug-segment)
    (buffer-name . hud-modeline--buffer-name)
    (separator . hud-modeline--separator)
    (modes . hud-modeline--mode-segment))
  "Default left mode-line segment alist.")

(defconst hud-modeline--default-right-segments
  '((position . hud-modeline--position-segment)
    (buffer-size . hud-modeline--buffer-size)
    (misc . hud-modeline--misc-segment))
  "Default right mode-line segment alist (core only).
Integration segments — compilation, flycheck, eglot, vc, projectile — are
registered after the defvars via `hud-modeline-add-segment'.")

(defvar hud-modeline-left-segments
  (copy-alist hud-modeline--default-left-segments)
  "Left mode-line segments alist: ((KEY . FUNCTION) ...).
Keys are symbols used for lookup and override.  Functions take no
arguments and return a string or nil.  Nil disables the segment.
Modify via `hud-modeline-override-segment', `hud-modeline-disable-segment',
`hud-modeline-reset-segment', or `hud-modeline-reset-all-segments'.")

(defvar hud-modeline-right-segments
  (copy-alist hud-modeline--default-right-segments)
  "Right mode-line segments alist.  See `hud-modeline-left-segments'.")

;;;; Segment management API

(defun hud-modeline--defaults-for (var)
  "Return the default segment alist for user segments variable VAR."
  (cond
   ((eq var 'hud-modeline-left-segments) hud-modeline--default-left-segments)
   ((eq var 'hud-modeline-right-segments) hud-modeline--default-right-segments)
   (t (user-error "Unknown segments variable: %S" var))))

(defun hud-modeline-override-segment (var key fn)
  "In segments alist VAR, set segment KEY to function FN.
VAR must be `hud-modeline-left-segments' or `hud-modeline-right-segments'.
KEY is the symbol identifying the segment.  FN is called with no arguments
and must return a string or nil."
  (setf (map-elt (symbol-value var) key) fn))

(defun hud-modeline-disable-segment (var key)
  "Disable segment KEY in segments alist VAR.
Sets the segment function to nil; the segment contributes nothing to
the rendered mode line.  Use `hud-modeline-reset-segment' to re-enable."
  (hud-modeline-override-segment var key nil))

(defun hud-modeline-reset-segment (var key)
  "Reset segment KEY in segments alist VAR to its default function."
  (hud-modeline-override-segment var key
    (map-elt (hud-modeline--defaults-for var) key)))

(defun hud-modeline-reset-all-segments (var)
  "Reset all segments in VAR to their defaults."
  (set var (copy-alist (hud-modeline--defaults-for var))))

(defun hud-modeline-add-segment (var key fn &optional position ref-key)
  "Add segment KEY with function FN to segments alist VAR.
POSITION is :after (default) or :before, relative to REF-KEY.
When REF-KEY is nil, :after appends to the end and :before prepends to the front.
No-op if KEY is already in VAR — use `hud-modeline-override-segment' to update."
  (unless (assq key (symbol-value var))
    (let* ((new-pair (cons key fn))
           (pos (or position :after))
           (segments (symbol-value var)))
      (cond
       ((and ref-key (assq ref-key segments))
        (set var (seq-mapcat (lambda (pair)
                               (if (eq (car pair) ref-key)
                                   (if (eq pos :before)
                                       (list new-pair pair)
                                     (list pair new-pair))
                                 (list pair)))
                             segments)))
       ((eq pos :before)
        (set var (cons new-pair segments)))
       (t
        (set var (append segments (list new-pair))))))))

(defun hud-modeline-remove-segment (var key)
  "Remove segment KEY from segments alist VAR entirely.
Use `hud-modeline-disable-segment' to keep the slot but suppress output."
  (set var (assq-delete-all key (symbol-value var))))

;; Register integration segments using the same public API external callers use.
;; Each goes before `position' so they appear left of the position/size/misc block.
(hud-modeline-add-segment 'hud-modeline-right-segments 'compilation
                          #'hud-modeline--compilation-segment :before 'position)
(hud-modeline-add-segment 'hud-modeline-right-segments 'flycheck
                          #'hud-modeline--flycheck :after 'compilation)
(hud-modeline-add-segment 'hud-modeline-right-segments 'vc
                          #'hud-modeline--vc-segment :after 'flycheck)
(hud-modeline-add-segment 'hud-modeline-right-segments 'projectile
                          #'hud-modeline--projectile-segment :after 'vc)
;; eglot is intentionally absent from the defaults: eglot manages its own
;; mode-line-misc-info entry, which the misc segment renders without truncation.
;; To use a dedicated segment instead, add from the eglot use-package block:
;;   (hud-modeline-add-segment 'hud-modeline-right-segments 'eglot
;;                             #'hud-modeline--eglot :after 'flycheck)

;;;; Rendering

(defun hud-modeline--render-segments (segments)
  "Concatenate results of all non-nil segment functions in SEGMENTS.
Iterates SEGMENTS in alist order; keys are ignored.  Disabled entries
(nil function) and functions returning nil are silently skipped."
  (thread-last segments
    (seq-map #'cdr)
    (seq-filter #'identity)
    (seq-map #'funcall)
    (seq-filter #'stringp)
    (apply #'concat)))

(defun hud-modeline--left ()
  "Render the left side of the mode-line."
  (hud-modeline--render-segments hud-modeline-left-segments))

(defun hud-modeline--right ()
  "Render the right side of the mode-line."
  (hud-modeline--render-segments hud-modeline-right-segments))

(defun hud-modeline--pad (s prefix)
  "Ensure S has a single space at the PREFIX end (t=left, nil=right)."
  (if prefix
      (if (string-prefix-p " " s) s (concat " " s))
    (if (string-suffix-p " " s) s (concat s " "))))

(defun hud-modeline--cols (s)
  "Return display width of S in columns.
Uses pixel-based measurement in GUI Emacs (Emacs 29+) for accuracy with
characters whose Unicode width is 1 but render as 2 columns (nerd-icons
glyphs, ambiguous-width symbols).  Falls back to `string-width' in terminal
and batch contexts."
  (if (and (fboundp 'string-pixel-width) (display-graphic-p))
      (let ((char-w (frame-char-width)))
        (if (> char-w 0)
            (ceiling (/ (string-pixel-width s) (float char-w)))
          (string-width s)))
    (string-width s)))

(defun hud-modeline--render ()
  "Render full mode-line with right-aligned right side.
Truncates the right side when lhs + rhs would exceed the window width."
  (let* ((lhs (hud-modeline--pad (hud-modeline--left) t))
         (rhs-raw (hud-modeline--pad (hud-modeline--right) nil))
         (window-width (window-body-width))
         (lhs-cols (hud-modeline--cols lhs))
         (rhs-raw-cols (hud-modeline--cols rhs-raw))
         (rhs-max (max 0 (- window-width lhs-cols 1)))
         (rhs (if (> rhs-raw-cols rhs-max)
                  (truncate-string-to-width rhs-raw rhs-max nil nil "…")
                rhs-raw))
         (rhs-cols (if (eq rhs rhs-raw) rhs-raw-cols (hud-modeline--cols rhs)))
         (fill (max 1 (- window-width lhs-cols rhs-cols))))
    (concat lhs (make-string fill ?\s) rhs)))

;;;; Mode

(defvar hud-modeline-format '(:eval (hud-modeline--render))
  "Mode-line format for `hud-modeline-mode'.")

(defvar hud-modeline--saved-format nil
  "Saved `mode-line-format' before `hud-modeline-mode' activation.")

(defconst hud-modeline--mode-line-faces
  '(mode-line mode-line-active mode-line-inactive mode-line-highlight)
  "Standard mode-line faces whose underline hud-modeline suppresses.")

(defvar hud-modeline--saved-underlines nil
  "Alist of (FACE . underline-value) saved before hud-modeline-mode activation.")

(defun hud-modeline--suppress-underlines ()
  "Save and clear :underline on standard mode-line faces."
  (setq hud-modeline--saved-underlines
        (seq-map (lambda (face)
                   (cons face (face-attribute face :underline nil t)))
                 hud-modeline--mode-line-faces))
  (seq-do (lambda (face)
            (set-face-attribute face nil :underline nil))
          hud-modeline--mode-line-faces))

(defun hud-modeline--restore-underlines ()
  "Restore saved :underline values on standard mode-line faces."
  (seq-do (lambda (pair)
            (set-face-attribute (car pair) nil :underline (cdr pair)))
          hud-modeline--saved-underlines)
  (setq hud-modeline--saved-underlines nil))

;;;###autoload
(define-minor-mode hud-modeline-mode
  "Lightweight global modeline."
  :global t
  :group 'hud-modeline
  (if hud-modeline-mode
      (progn
        (setq hud-modeline--saved-format (default-value 'mode-line-format))
        (setq-default mode-line-format hud-modeline-format)
        (hud-modeline--suppress-underlines)
        (add-hook 'after-change-major-mode-hook #'hud-modeline--invalidate-icon)
        (add-hook 'compilation-start-hook #'hud-modeline--on-compilation-start)
        (add-hook 'compilation-finish-functions #'hud-modeline--on-compilation-finish))
    (remove-hook 'after-change-major-mode-hook #'hud-modeline--invalidate-icon)
    (remove-hook 'compilation-start-hook #'hud-modeline--on-compilation-start)
    (remove-hook 'compilation-finish-functions #'hud-modeline--on-compilation-finish)
    (hud-modeline--restore-underlines)
    (setq-default mode-line-format hud-modeline--saved-format)))

(provide 'hud-modeline)
;;; hud-modeline.el ends here
