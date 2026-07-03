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
;;   `hud-modeline-add-segment'        — register a segment; :fn SYMBOL or :body FORM
;;   `hud-modeline-remove-segment'     — remove a segment entirely
;;   `hud-modeline-override-segment'   — replace an existing segment's function
;;   `hud-modeline-disable-segment'    — suppress a segment without removing it
;;   `hud-modeline-reset-segment'      — restore one segment to its default
;;   `hud-modeline-reset-all-segments'  — restore all segments to defaults
;;   `hud-modeline-move-segment'        — move a segment between left and right blocks
;;   `hud-modeline-set-segment-action'  — attach a mouse-1 command to any segment
;;
;; External packages and use-package blocks should use these functions rather
;; than modifying `hud-modeline-left-segments' / `hud-modeline-right-segments'
;; directly.

;;; Code:

(require 'subr-x)
(require 'map)
(require 'seq)

(declare-function anzu--reset-status "anzu")
(declare-function flycheck-count-errors "flycheck")
(declare-function sprite-list-menu "sprite")
(declare-function nerd-icons-codicon "nerd-icons")
(declare-function nerd-icons-octicon "nerd-icons")
(declare-function nerd-icons-icon-for-buffer "nerd-icons")
(declare-function projectile-project-name "projectile")
(declare-function projectile-project-root "projectile")
(declare-function projectile-dispatch "projectile")
(declare-function mouse-major-mode-menu "mouse")
(declare-function mouse-buffer-menu "mouse")
(declare-function annotated-completing-read "annotated-completing-read")

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

(defcustom hud-modeline-box-style nil
  "Mode-line box style as a list of features to enable (or nil for no box).
Any combination of:
  `raised'   — beveled raised-button appearance
  `padded'   — extra pixel padding inside the mode-line border
  `bordered' — thin flat border line around the mode-line"
  :type '(set (const :tag "Raised" raised)
              (const :tag "Padded" padded)
              (const :tag "Bordered" bordered))
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
  '((t :inherit mode-line-emphasis :underline nil))
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
  '((t :underline nil))
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
  "Cached nerd-icon for the current buffer's major mode.
The symbol `none' means icon lookup was attempted and failed or returned nil;
any other non-nil value is the cached icon string.")

(defun hud-modeline--invalidate-icon ()
  "Invalidate the icon cache when the major mode changes."
  (setq hud-modeline--icon-cache nil))

(defun hud-modeline--icon ()
  "Return file-type icon string or nil."
  (when (and hud-modeline-icons (fboundp 'nerd-icons-icon-for-buffer))
    (cond
     ((eq hud-modeline--icon-cache 'none) nil)
     (hud-modeline--icon-cache hud-modeline--icon-cache)
     (t
      (let ((icon (condition-case nil
                      (nerd-icons-icon-for-buffer)
                    (error nil))))
        (setq hud-modeline--icon-cache (or icon 'none))
        icon)))))

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
  "Return buffer name, preferring explicit renames over project-relative path.
For file-backed buffers explicitly renamed (e.g. by `denote-rename-buffer-mode'),
shows the first two space-separated words of the title portion (text before
the first \" <\" keyword marker) for a compact modeline display."
  (propertize
   (if-let* ((fname (buffer-file-name))
             (_ (string= (buffer-name) (file-name-nondirectory fname)))
             (root (and (bound-and-true-p projectile-mode)
                        (fboundp 'projectile-project-root)
                        (projectile-project-root))))
       (file-relative-name fname root)
     (let* ((name (buffer-name))
            (fpath (buffer-file-name))
            (renamed (and fpath
                          (not (string= name (file-name-nondirectory fpath))))))
       (if renamed
           (string-join
            (seq-take
             (split-string (or (car (split-string name " <" t)) name) " " t)
             3)
            " ")
         name)))
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

(defun hud-modeline--eglot ()
  "Return eglot's own mode-line format string, or nil when eglot is inactive.
Renders the same content eglot would show in mode-line-misc-info so that
callers who suppress eglot's misc-info entry still get the full status."
  (when (and (bound-and-true-p eglot--managed-mode)
             (fboundp 'eglot--mode-line-format))
    (when-let* ((s (format-mode-line '(eglot--managed-mode eglot--mode-line-format)))
                (_ (not (string-empty-p (string-trim s)))))
      (concat s " "))))

(defun hud-modeline--position-segment ()
  "Return formatted line:column position with trailing space."
  (concat (format-mode-line '(:propertize "%l:%c" face hud-modeline-position))
          " "))

(defun hud-modeline--anzu ()
  "Return anzu search match index string, or nil when anzu is inactive.
Reads anzu state variables directly so the segment works without anzu
prepending anything to `mode-line-format'.  Set `anzu-cons-mode-line-p'
to nil in the anzu use-package config to suppress anzu's own insertion."
  (when (and (bound-and-true-p anzu--state)
             (not (bound-and-true-p iedit-mode)))
    (let ((here anzu--current-position)
          (total anzu--total-matched))
      (propertize
       (cond
        ((eq anzu--state 'replace-query)
         (format "%d replace" (bound-and-true-p anzu--cached-count)))
        ((eq anzu--state 'replace)
         (format "%d/%d" here total))
        ((bound-and-true-p anzu--overflow-p)
         (format "%s+" total))
        (t
         (format "%s/%d" here total)))
       'face 'hud-modeline-warning))))

(defun hud-modeline--buffer-size ()
  "Return propertized buffer char count with trailing space, or nil when disabled."
  (when hud-modeline-show-buffer-size
    (concat (file-size-human-readable (buffer-size)) " ")))

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

;;;; Segment registries

(defconst hud-modeline--default-left-segments
  '((buffer-size . hud-modeline--buffer-size)
    (state . hud-modeline--state-segment)
    (buffer-name . hud-modeline--buffer-name)
    (separator . hud-modeline--separator)
    (modes . hud-modeline--mode-segment)
    (position . hud-modeline--position-segment))
  "Default left mode-line segment alist.")

(defconst hud-modeline--default-right-segments
  '((misc . hud-modeline--misc-segment))
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

;;;; Segment actions

(defvar hud-modeline--action-keymaps nil
  "Alist mapping segment key symbol to its mouse-1 keymap.")

(defun hud-modeline-set-segment-action (key command)
  "Attach COMMAND to mouse-1 clicks on segment KEY.
Creates and caches a sparse keymap; segment bodies that call
`hud-modeline--action-map' pick it up automatically.
Replaces any previously registered action for KEY."
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] command)
    (setf (map-elt hud-modeline--action-keymaps key) map)))

(defun hud-modeline--action-map (key)
  "Return the cached mouse-1 keymap for segment KEY, or nil."
  (map-elt hud-modeline--action-keymaps key))

(defun hud-modeline--register-segment (var key fn place-after place-before)
  "Insert KEY/FN into VAR relative to PLACE-AFTER or PLACE-BEFORE on first call; always update FN.
Insertion order is fixed after first registration; re-evaluation updates the function
in place so reload picks up body changes without moving the segment."
  (unless (assq key (symbol-value var))
    (let* ((new-pair (cons key fn))
           (segments (symbol-value var))
           (ref (or place-after place-before)))
      (cond
       ((and ref (assq ref segments))
        (set var (seq-mapcat (lambda (pair)
                               (if (eq (car pair) ref)
                                   (if place-before
                                       (list new-pair pair)
                                     (list pair new-pair))
                                 (list pair)))
                             segments)))
       (place-before
        (set var (cons new-pair segments)))
       (t
        (set var (append segments (list new-pair)))))))
  (setf (map-elt (symbol-value var) key) fn))

(cl-defmacro hud-modeline-add-segment (&key segment-block key fn body place-after place-before)
  "Register segment KEY in SEGMENT-BLOCK with optional positional placement.
Supply :fn SYMBOL for a pre-defined function, or :body FORM to generate a named
function `hud-modeline--KEY-segment' inline.  Use :place-after KEY or
:place-before KEY to control insertion order; the two are mutually exclusive.
Re-evaluation always updates the function without changing insertion order."
  (declare (debug t))
  (cond
   ((and place-after place-before)
    (error "hud-modeline-add-segment: :place-after and :place-before are mutually exclusive"))
   ((and fn body)
    (error "hud-modeline-add-segment: supply :fn or :body, not both"))
   (body
    (let* ((key-sym (if (and (consp key) (eq (car key) 'quote))
                        (cadr key)
                      (error "hud-modeline-add-segment: key must be a quoted symbol")))
           (fn-sym (intern (format "hud-modeline--%s-segment"
                                   (symbol-name key-sym)))))
      `(progn
         (defun ,fn-sym () ,body)
         (hud-modeline--register-segment ,segment-block ,key #',fn-sym ,place-after ,place-before))))
   (fn
    `(hud-modeline--register-segment ,segment-block ,key ,fn ,place-after ,place-before))
   (t
    (error "hud-modeline-add-segment: :fn or :body is required"))))

(defun hud-modeline-remove-segment (var key)
  "Remove segment KEY from segments alist VAR entirely.
Use `hud-modeline-disable-segment' to keep the slot but suppress output."
  (set var (assq-delete-all key (symbol-value var))))

(defun hud-modeline-move-segment (from-var to-var key &optional place-after place-before)
  "Move segment KEY from FROM-VAR to TO-VAR, preserving its function.
Optional PLACE-AFTER or PLACE-BEFORE (quoted symbols) control insertion
order in TO-VAR; they are mutually exclusive.  Signals `user-error' if KEY
is not present in FROM-VAR."
  (when (and place-after place-before)
    (user-error "hud-modeline-move-segment: place-after and place-before are mutually exclusive"))
  (if-let* ((pair (assq key (symbol-value from-var))))
      (progn
        (set from-var (assq-delete-all key (symbol-value from-var)))
        (hud-modeline--register-segment to-var key (cdr pair) place-after place-before))
    (user-error "hud-modeline-move-segment: segment %S not found in %S" key from-var)))

;; Register integration segments using the same public API external callers use.
;; Each goes before `position' so they appear left of the position/size/misc block.
(hud-modeline-add-segment
    :segment-block 'hud-modeline-right-segments
    :key 'anzu
    :place-after 'misc
    :fn #'hud-modeline--anzu)

(hud-modeline-add-segment
    :segment-block 'hud-modeline-right-segments
    :key 'instance
    :place-after 'projectile
    :body (when (and (featurep 'sprite) (boundp 'sprite-instance-id) sprite-instance-id)
            (concat (propertize (concat "[" sprite-instance-id "]")
                                'local-map (hud-modeline--action-map 'instance)
                                'mouse-face 'mode-line-highlight
                                'help-echo "mouse-1: sprite list")
                    " ")))

(hud-modeline-add-segment
    :segment-block 'hud-modeline-right-segments
    :place-after 'anzu
    :key 'compilation
    :body (cond
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

(hud-modeline-add-segment
    :segment-block 'hud-modeline-right-segments
    :place-after 'compilation
    :key 'flycheck
    :body (when (and (bound-and-true-p flycheck-mode)
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

(hud-modeline-add-segment
    :segment-block 'hud-modeline-right-segments
    :place-after 'flycheck
    :key 'vc
    :body (when-let* ((branch (hud-modeline--vc)))
            (if-let* ((km (hud-modeline--action-map 'vc)))
                (concat (propertize branch
                                    'local-map km
                                    'mouse-face 'mode-line-highlight
                                    'help-echo "mouse-1: vc dispatch")
                        " ")
              (concat branch " "))))

(hud-modeline-add-segment
    :segment-block 'hud-modeline-right-segments
    :key 'projectile
    :place-after 'vc
    :body (when (and (bound-and-true-p projectile-mode)
                     (fboundp 'projectile-project-name))
            (when-let* ((name (projectile-project-name))
                        (_ (not (equal name "-"))))
              (concat (when vc-mode "| ")
                      (propertize name
                                  'face 'mode-line-buffer-id
                                  'local-map (hud-modeline--action-map 'projectile)
                                  'mouse-face 'mode-line-highlight
                                  'help-echo "mouse-1: projectile dispatch")
                      " "))))

;; Move debug to the right side, first in the right block; safe to call on reload.
(hud-modeline-remove-segment 'hud-modeline-left-segments 'debug)
(hud-modeline-remove-segment 'hud-modeline-right-segments 'debug)

(hud-modeline-add-segment
    :segment-block 'hud-modeline-right-segments
    :key 'debug
    :place-before 'misc
    :fn #'hud-modeline--debug-segment)

;; Default actions for built-in segments.
(hud-modeline-set-segment-action 'instance #'sprite-list-menu)
(hud-modeline-set-segment-action 'projectile #'projectile-dispatch)

;; eglot is intentionally absent from the defaults: eglot manages its own
;; mode-line-misc-info entry, which the misc segment renders without truncation.
;; To use a dedicated segment instead, add from the eglot use-package block:
;;   (hud-modeline-add-segment
;;     :segment-block 'hud-modeline-right-segments :key 'eglot
;;     :fn #'hud-modeline--eglot :place-after 'flycheck)

;;;; Rendering

(defun hud-modeline--render-segments (segments)
  "Concatenate segment function results with exactly one space between each.
Iterates SEGMENTS in alist order; keys are ignored.  Disabled entries
(nil function) and nil/non-string returns are silently skipped.
Each segment string is trimmed before joining so no segment can introduce
leading or trailing padding that would create double spaces."
  (string-join
   (thread-last segments
     (seq-map #'cdr)
     (seq-filter #'identity)
     (seq-map #'funcall)
     (seq-filter #'stringp)
     (seq-map #'string-trim)
     (seq-remove #'string-empty-p))
   " "))

(defun hud-modeline--pad (s prefix)
  "Ensure S has a single space at the PREFIX end (t=left, nil=right)."
  (if prefix
      (if (string-prefix-p " " s) s (concat " " s))
    (if (string-suffix-p " " s) s (concat s " "))))

(defun hud-modeline--left ()
  "Render the left side of the mode-line with a leading space."
  (hud-modeline--pad (hud-modeline--render-segments hud-modeline-left-segments) t))

(defun hud-modeline--right ()
  "Render the right side of the mode-line with a trailing space."
  (hud-modeline--pad (hud-modeline--render-segments hud-modeline-right-segments) nil))

;;;; Mode

(defconst hud-modeline-format
  '((:eval (hud-modeline--left))
    mode-line-format-right-align
    (:eval (hud-modeline--right)))
  "Mode-line format for `hud-modeline-mode'.
A list of constructs so packages like anzu that prepend to `mode-line-format'
via `cons' get a proper list rather than tearing apart the first element.
Right-alignment is provided by `mode-line-format-right-align'.")

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

(defvar hud-modeline--saved-boxes nil
  "Alist of (FACE . box-value) saved before box style activation.")

(defun hud-modeline--box-spec ()
  "Return a :box attribute value computed from `hud-modeline-box-style', or nil."
  (when hud-modeline-box-style
    (let* ((padded (memq 'padded hud-modeline-box-style))
           (bordered (memq 'bordered hud-modeline-box-style))
           (raised (memq 'raised hud-modeline-box-style))
           (line-width (cond (padded 4) (bordered 1) (t -2)))
           (style (when raised 'released-button)))
      (if style
          (list :line-width line-width :style style)
        (list :line-width line-width)))))

(defun hud-modeline--apply-box ()
  "Save and apply the box spec from `hud-modeline-box-style' to mode-line faces."
  (when-let* ((spec (hud-modeline--box-spec)))
    (setq hud-modeline--saved-boxes
          (seq-map (lambda (face)
                     (cons face (face-attribute face :box nil t)))
                   hud-modeline--mode-line-faces))
    (seq-do (lambda (face)
              (set-face-attribute face nil :box spec))
            hud-modeline--mode-line-faces)))

(defun hud-modeline--restore-boxes ()
  "Restore saved :box values on mode-line faces."
  (seq-do (lambda (pair)
            (set-face-attribute (car pair) nil :box (cdr pair)))
          hud-modeline--saved-boxes)
  (setq hud-modeline--saved-boxes nil))

(defconst hud-modeline--box-style-options
  '(("none" nil "No box styling")
    ("raised" (raised) "Beveled raised-button appearance")
    ("bordered" (bordered) "Thin flat border line")
    ("padded" (padded) "Extra pixel padding")
    ("raised + padded" (raised padded) "Raised button with extra padding")
    ("raised + bordered" (raised bordered) "Raised button with border line")
    ("bordered + padded" (bordered padded) "Border with extra padding")
    ("raised + bordered + padded" (raised bordered padded) "All features"))
  "Preset combinations for `hud-modeline-set-box-style'.")

(defun hud-modeline-set-box-style ()
  "Interactively set `hud-modeline-box-style' and apply it immediately."
  (interactive)
  (let* ((table (seq-map (lambda (entry)
                           (cons (car entry) (nth 2 entry)))
                         hud-modeline--box-style-options))
         (choice (annotated-completing-read
                  table
                  :prompt "Mode-line box style: "
                  :require-match t
                  :category 'hud-modeline))
         (value (nth 1 (assoc choice hud-modeline--box-style-options))))
    (setq hud-modeline-box-style value)
    (when (bound-and-true-p hud-modeline-mode)
      (when hud-modeline--saved-boxes
        (hud-modeline--restore-boxes))
      (when hud-modeline-box-style
        (hud-modeline--apply-box)))))

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
        (when hud-modeline-box-style
          (hud-modeline--apply-box))
        (add-hook 'after-change-major-mode-hook #'hud-modeline--invalidate-icon)
        (add-hook 'compilation-start-hook #'hud-modeline--on-compilation-start)
        (add-hook 'compilation-finish-functions #'hud-modeline--on-compilation-finish))
    (remove-hook 'after-change-major-mode-hook #'hud-modeline--invalidate-icon)
    (remove-hook 'compilation-start-hook #'hud-modeline--on-compilation-start)
    (remove-hook 'compilation-finish-functions #'hud-modeline--on-compilation-finish)
    (when hud-modeline--saved-boxes
      (hud-modeline--restore-boxes))
    (hud-modeline--restore-underlines)
    (setq-default mode-line-format hud-modeline--saved-format)))

(provide 'hud-modeline)
;;; hud-modeline.el ends here
