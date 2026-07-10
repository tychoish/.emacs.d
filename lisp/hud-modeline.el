;;; hud-modeline.el --- Lightweight modeline -*- lexical-binding: t; -*-

;;; Commentary:
;; Lightweight modeline replacing doom-modeline.
;; Uses nerd-icons (terminal and GUI), integrates flycheck, eglot, vc, projectile.
;;
;; Segments are stored as alists: ((KEY . hud-modeline-segment) ...).  Each value
;; is a `hud-modeline-segment' struct carrying the rendering function, display
;; name, description, and an enabled flag.  Only enabled segments with non-nil
;; function returns contribute to the rendered mode line.
;;
;; Public segment management API:
;;   `hud-modeline-add-segment'        — register a segment; :fn SYMBOL or :body FORM
;;   `hud-modeline-remove-segment'     — remove a segment entirely
;;   `hud-modeline-override-segment'   — replace an existing segment's function
;;   `hud-modeline-disable-segment'    — suppress a segment without removing it
;;   `hud-modeline-enable-segment'     — re-enable a suppressed segment
;;   `hud-modeline-toggle-segment'     — interactively toggle visibility via ACR
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
  "Maximum display-column width for the misc segment, or nil for no cap."
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

(defface hud-modeline-buffer-name
  '((t :inherit mode-line-buffer-id :underline nil))
  "Face for the buffer name in the mode line."
  :group 'hud-modeline)

(defface hud-modeline-position
  '((t :underline nil))
  "Face for line/column position."
  :group 'hud-modeline)

;;;; Segment struct

(cl-defstruct (hud-modeline-segment
               (:copier nil)
               (:constructor hud-modeline--make-segment
                 (&key fn name description (enabled t))))
  "A hud-modeline segment: rendering function, display metadata, and visibility state."
  fn          ; () → string|nil — called each mode-line render
  name        ; string or nil  — display name for the ACR picker
  description ; string or nil  — annotation for the ACR picker
  enabled)    ; non-nil = visible; nil = suppressed without removal

;;;; Icon cache

(defvar-local hud-modeline--icon-cache nil
  "Cached nerd-icon for the current buffer's major mode.
The symbol `none' means icon lookup was attempted and failed or returned nil;
any other non-nil value is the cached icon string.")

(defun hud-modeline--invalidate-icon ()
  "Invalidate the icon cache when the major mode changes."
  (setq hud-modeline--icon-cache nil))

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

(defun hud-modeline-clear-compilation-status ()
  "Clear the compilation status indicator in the mode-line."
  (interactive)
  (setq hud-modeline--compilation-finished nil
        hud-modeline--compilation-errors 0
        hud-modeline--compilation-warnings 0)
  (force-mode-line-update t))

;;;; Segment registries

(defvar hud-modeline-left-segments nil
  "Left mode-line segments alist: ((KEY . hud-modeline-segment) ...).
Keys are symbols for lookup.  Each value is a `hud-modeline-segment' struct;
only segments with non-nil enabled field contribute to rendering.
Modify via `hud-modeline-override-segment', `hud-modeline-disable-segment',
`hud-modeline-enable-segment', or `hud-modeline-remove-segment'.")

(defvar hud-modeline-right-segments nil
  "Right mode-line segments alist.  See `hud-modeline-left-segments'.")

;;;; Segment management API

(defun hud-modeline-override-segment (var key fn)
  "In segments alist VAR, replace the rendering function for segment KEY with FN."
  (when-let* ((seg (map-elt (symbol-value var) key)))
    (setf (hud-modeline-segment-fn seg) fn)))

(defun hud-modeline-disable-segment (var key)
  "Disable segment KEY in VAR; it contributes nothing to the mode-line.
Use `hud-modeline-enable-segment' to re-enable."
  (when-let* ((seg (map-elt (symbol-value var) key)))
    (setf (hud-modeline-segment-enabled seg) nil)))

(defun hud-modeline-enable-segment (var key)
  "Enable segment KEY in VAR."
  (when-let* ((seg (map-elt (symbol-value var) key)))
    (setf (hud-modeline-segment-enabled seg) t)))

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

(defun hud-modeline--register-segment (var key segment place-after place-before)
  "Insert KEY/SEGMENT into VAR at PLACE-AFTER or PLACE-BEFORE on first call; update on reload.
SEGMENT must be a `hud-modeline-segment' struct.  On reload (key already present
and value is already a struct), fn/name/description are updated in-place so the
enabled state is preserved across file reloads.  If the existing value is not a
struct (stale format from a prior session), it is replaced in-place with SEGMENT."
  (if-let* ((existing (map-elt (symbol-value var) key)))
      (if (hud-modeline-segment-p existing)
          (setf (hud-modeline-segment-fn          existing) (hud-modeline-segment-fn          segment)
                (hud-modeline-segment-name        existing) (hud-modeline-segment-name        segment)
                (hud-modeline-segment-description existing) (hud-modeline-segment-description segment))
        (setf (map-elt (symbol-value var) key) segment))
    (let* ((new-pair (cons key segment))
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
        (set var (append segments (list new-pair))))))))

(cl-defmacro hud-modeline-add-segment (&key segment-block key fn body name description
                                            place-after place-before)
  "Register segment KEY in SEGMENT-BLOCK with optional positional placement.
Supply :fn SYMBOL for a pre-defined function, or :body FORM to generate a named
function `hud-modeline--KEY-segment' inline.  :name and :description are shown
in `hud-modeline-toggle-segment'.  Use :place-after or :place-before (mutually
exclusive) to control insertion order.  Re-evaluation updates fn/name/description
without changing position or resetting the enabled flag."
  (declare (debug t))
  (cond
   ((and place-after place-before)
    (error "hud-modeline-add-segment: :place-after and :place-before are mutually exclusive"))
   ((and fn body)
    (error "hud-modeline-add-segment: supply :fn or :body, not both"))
   ((not (or fn body))
    (error "hud-modeline-add-segment: :fn or :body is required"))
   (body
    (let* ((key-sym (if (and (consp key) (eq (car key) 'quote))
                        (cadr key)
                      (error "hud-modeline-add-segment: key must be a quoted symbol")))
           (fn-sym (intern (format "hud-modeline--%s-segment"
                                   (symbol-name key-sym)))))
      `(progn
         (defun ,fn-sym () ,body)
         (hud-modeline--register-segment ,segment-block ,key
           (hud-modeline--make-segment :fn #',fn-sym :name ,name :description ,description)
           ,place-after ,place-before))))
   (fn
    `(hud-modeline--register-segment ,segment-block ,key
       (hud-modeline--make-segment :fn ,fn :name ,name :description ,description)
       ,place-after ,place-before))))

(defun hud-modeline-remove-segment (var key)
  "Remove segment KEY from segments alist VAR entirely.
Use `hud-modeline-disable-segment' to keep the slot but suppress output."
  (set var (assq-delete-all key (symbol-value var))))

(defun hud-modeline-move-segment (from-var to-var key &optional place-after place-before)
  "Move segment KEY from FROM-VAR to TO-VAR, preserving its struct (including enabled state).
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

;;;; Core segment registration

(hud-modeline-add-segment
 :segment-block 'hud-modeline-left-segments
 :key 'buffer-size
 :name "Buffer Size"
 :description "Character count of the current buffer"
 :body (when hud-modeline-show-buffer-size
         (concat (file-size-human-readable (buffer-size)) " ")))

(hud-modeline-add-segment
 :segment-block 'hud-modeline-left-segments
 :key 'state
 :name "State"
 :description "Modified/read-only indicator"
 :body (cond
        (buffer-read-only
         (if (and hud-modeline-icons (fboundp 'nerd-icons-octicon))
             (nerd-icons-octicon "nf-oct-lock" :face 'hud-modeline-read-only)
           (propertize "RO" 'face 'hud-modeline-read-only)))
        ((buffer-modified-p)
         (if (and hud-modeline-icons (fboundp 'nerd-icons-octicon))
             (nerd-icons-octicon "nf-oct-pencil" :face 'hud-modeline-modified)
           (propertize "!" 'face 'hud-modeline-modified)))
        (t nil)))

;; hud-modeline--icon-cache: nil=not yet checked, 'none=failed/absent, STRING=cached icon
(hud-modeline-add-segment
 :segment-block 'hud-modeline-left-segments
 :key 'icon
 :name "Icon"
 :description "File-type icon (nerd-icons)"
 :body (when (and hud-modeline-icons
                  (fboundp 'nerd-icons-icon-for-buffer))
         (cond
          ((eq hud-modeline--icon-cache 'none) nil)
          (hud-modeline--icon-cache hud-modeline--icon-cache)
          (t
           (let ((icon (condition-case nil
                           (nerd-icons-icon-for-buffer)
                         (error nil))))
             (setq hud-modeline--icon-cache (or icon 'none))
             icon)))))

(hud-modeline-add-segment
 :segment-block 'hud-modeline-left-segments
 :key 'buffer-name
 :name "Buffer Name"
 :description "Buffer name or project-relative path"
 :body (propertize
        (if-let* ((fname (buffer-file-name))
                  (_ (string= (buffer-name) (file-name-nondirectory fname)))
                  (root (and (bound-and-true-p projectile-mode)
                             (fboundp 'projectile-project-root)
                             (projectile-project-root))))
            (file-relative-name fname root)
          (buffer-name))
        'face 'hud-modeline-buffer-name
        'local-map (let ((map (make-sparse-keymap)))
		     (define-key map [mode-line mouse-1] #'next-buffer)
		     (define-key map [mode-line mouse-3] #'mouse-buffer-menu)
		     map)
        'mouse-face 'mode-line-highlight
        'help-echo "mouse-1: next buffer\nmouse-3: buffer menu"))

(hud-modeline-add-segment
 :segment-block 'hud-modeline-left-segments
 :key 'separator
 :name "Separator"
 :description "Visual spacer between buffer name and modes"
 :body " ")

(hud-modeline-add-segment
 :segment-block 'hud-modeline-left-segments
 :key 'modes
 :name "Modes"
 :description "Major and minor mode indicators"
 :body (let* ((major (hud-modeline--major-mode))
              (major-display
               (if (stringp major)
                   (propertize major
                               'local-map (let ((map (make-sparse-keymap)))
					    (define-key map [mode-line mouse-1] #'mouse-major-mode-menu)
					    (define-key map [mode-line mouse-2] #'describe-mode)
					    (define-key map [mode-line mouse-3] #'mouse-major-mode-menu)
					    map)
                               'mouse-face 'mode-line-highlight
                               'help-echo "mouse-1: mode menu\nmouse-2: describe mode")
                 major)))
         (format-mode-line
          `("%[" "(" ,major-display
            ("" mode-line-process) ("" minor-mode-alist) "%n" ")" "%]"))))

(hud-modeline-add-segment
 :segment-block 'hud-modeline-left-segments
 :key 'position
 :name "Position"
 :description "Line and column number"
 :body (concat (format-mode-line '(:propertize "%l:%c" face hud-modeline-position))
               " "))

;; per-registration :max-width override no longer supported; use hud-modeline-misc-max-width
(hud-modeline-add-segment
 :segment-block 'hud-modeline-right-segments
 :key 'misc
 :name "Misc Info"
 :description "Miscellaneous mode-line info (battery, flyspell, etc.); excludes eglot, which has its own segment"
 :body (let ((misc (format-mode-line
                    (seq-remove (lambda (entry)
                                  (and (consp entry)
                                       (eq (car entry) 'eglot--managed-mode)))
                                mode-line-misc-info))))
         (unless (string-empty-p (string-trim misc))
           (if (and hud-modeline-misc-max-width
                    (> (string-width misc) hud-modeline-misc-max-width))
               (truncate-string-to-width misc hud-modeline-misc-max-width nil nil "…")
             misc))))

;;;; Integration segment registration

;; Set anzu-cons-mode-line-p to nil in anzu config to suppress anzu's own insertion
(hud-modeline-add-segment
 :segment-block 'hud-modeline-right-segments
 :key 'anzu
 :name "Anzu"
 :description "Search match index (set anzu-cons-mode-line-p nil)"
 :place-after 'misc
 :body (when (and (bound-and-true-p anzu--state)
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

(hud-modeline-add-segment
 :segment-block 'hud-modeline-right-segments
 :key 'instance
 :name "Instance"
 :description "Sprite instance identifier"
 :place-after 'projectile
 :body (when (and (featurep 'sprite)
		  (boundp 'sprite-instance-id)
		  sprite-instance-id)
         (concat (propertize (concat "[" sprite-instance-id "]")
                             'local-map (hud-modeline--action-map 'instance)
                             'mouse-face 'mode-line-highlight
                             'help-echo "mouse-1: sprite list")
                 " ")))

(hud-modeline-add-segment
 :segment-block 'hud-modeline-right-segments
 :place-after 'anzu
 :key 'compilation
 :name "Compilation"
 :description "Build status indicator"
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
 :name "Flycheck"
 :description "Error and warning counts"
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
 :name "VC"
 :description "Version-control branch"
 :body (when-let* ((raw vc-mode)
                   (bare (string-trim
                          (replace-regexp-in-string
                           "\\`[[:space:]]*[A-Za-z]+[-:]" "" raw)))
                   (_ (not (string-empty-p bare))))
         (let* ((icon (when (and hud-modeline-icons (fboundp 'nerd-icons-octicon))
                        (nerd-icons-octicon "nf-oct-git_branch" :face 'hud-modeline-vc)))
                (branch (if icon
                            (concat icon " " (propertize bare 'face 'hud-modeline-vc))
                          (propertize bare 'face 'hud-modeline-vc))))
           (concat
            (if-let* ((km (hud-modeline--action-map 'vc)))
                (propertize branch
                            'local-map km
                            'mouse-face 'mode-line-highlight
                            'help-echo "mouse-1: vc dispatch")
              branch)
            " "))))

(hud-modeline-add-segment
 :segment-block 'hud-modeline-right-segments
 :key 'projectile
 :name "Project"
 :description "Current project name"
 :place-after 'vc
 :body (when-let* ((_ (and (bound-and-true-p projectile-mode)
                           (fboundp 'projectile-project-name)))
                   (name (projectile-project-name))
                   (_ (not (equal name "-"))))
         (concat (when vc-mode "| ")
                 (propertize name
                             'face 'mode-line-buffer-id
                             'local-map (hud-modeline--action-map 'projectile)
                             'mouse-face 'mode-line-highlight
                             'help-echo "mouse-1: projectile dispatch")
                 " ")))

;; Enable only when eglot's misc-info entry is suppressed to avoid duplication
(hud-modeline-add-segment
 :segment-block 'hud-modeline-right-segments
 :key 'eglot
 :name "Eglot"
 :description "LSP status (disable eglot misc-info entry to avoid duplication)"
 :place-after 'flycheck
 :body (when-let* ((_ (and (bound-and-true-p eglot--managed-mode)
			   (fboundp 'eglot--mode-line-format)))
		   (s (format-mode-line '(eglot--managed-mode eglot--mode-line-format)))
                   (_ (not (string-empty-p (string-trim s)))))
         (concat s " ")))

(defun hud-modeline--raise-backtrace ()
  "Display the *Backtrace* buffer if one is currently active."
  (interactive)
  (if-let* ((buf (get-buffer "*Backtrace*")))
      (pop-to-buffer buf)
    (message "No *Backtrace* buffer is currently active")))

(defun hud-modeline--debug-flag-icon (icon-name fallback face help-echo)
  "Return a propertized mode-line indicator for an active debug flag.
ICON-NAME is a nerd-icons codicon name, FALLBACK the plain-text substitute
when icons are disabled, FACE the icon/text face, and HELP-ECHO the
tooltip text.  Clicking with mouse-1 raises the *Backtrace* buffer via
`hud-modeline--raise-backtrace'."
  (let ((icon (if (and hud-modeline-icons (fboundp 'nerd-icons-codicon))
                  (nerd-icons-codicon icon-name :face face)
                (propertize fallback 'face face))))
    (propertize icon
                'local-map (let ((map (make-sparse-keymap)))
                            (define-key map [mode-line mouse-1] #'hud-modeline--raise-backtrace)
                            map)
                'mouse-face 'mode-line-highlight
                'help-echo help-echo)))

(hud-modeline-add-segment
 :segment-block 'hud-modeline-right-segments
 :key 'debug
 :name "Debug"
 :description "Active when debug-on-error or debug-on-quit is set"
 :place-before 'misc
 :body (when-let* ((parts (delq nil
                                (list
                                 (when debug-on-error
                                   (hud-modeline--debug-flag-icon
                                    "nf-cod-bug" "bug" 'hud-modeline-error
                                    "debug-on-error active\nmouse-1: raise *Backtrace*"))
                                 (when debug-on-quit
                                   (hud-modeline--debug-flag-icon
                                    "nf-cod-debug_pause" "quit" 'hud-modeline-warning
                                    "debug-on-quit active\nmouse-1: raise *Backtrace*"))))))
         (concat (string-join parts " ") " ")))

;; Default actions for built-in segments.
(hud-modeline-set-segment-action 'instance #'sprite-list-menu)
(hud-modeline-set-segment-action 'projectile #'projectile-dispatch)

;;;; Rendering

(defun hud-modeline--render-segments (segments)
  "Concatenate enabled segment results with exactly one space between each.
Iterates SEGMENTS alist order; keys are ignored.  Segments with enabled=nil
are skipped; nil/non-string function returns are silently skipped.
Each string is trimmed before joining to prevent double spaces."
  (string-join
   (thread-last segments
     (seq-map    #'cdr)
     (seq-filter #'hud-modeline-segment-p)
     (seq-filter #'hud-modeline-segment-enabled)
     (seq-map    #'hud-modeline-segment-fn)
     (seq-filter #'identity)
     (seq-map    #'funcall)
     (seq-filter #'stringp)
     (seq-map    #'string-trim)
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

(defun hud-modeline-toggle-segment ()
  "Interactively toggle a segment's visibility using `annotated-completing-read'.
Candidates are the segment keys; annotations show [on]/[off], name, and description."
  (interactive)
  (let* ((all (append
               (seq-map (lambda (p) (list (car p) 'hud-modeline-left-segments  (cdr p)))
                        hud-modeline-left-segments)
               (seq-map (lambda (p) (list (car p) 'hud-modeline-right-segments (cdr p)))
                        hud-modeline-right-segments)))
         (table (seq-map
                 (lambda (entry)
                   (let* ((key (car entry))
                          (seg (nth 2 entry)))
                     (cons (symbol-name key)
                           (format "[%s] %s%s"
                                   (if (hud-modeline-segment-enabled seg) "on " "off")
                                   (if (hud-modeline-segment-name seg)
                                       (concat (hud-modeline-segment-name seg) " — ")
                                     "")
                                   (or (hud-modeline-segment-description seg) "")))))
                 all))
         (choice (annotated-completing-read
                  table
                  :prompt "Toggle segment: "
                  :require-match t
                  :category 'hud-modeline))
         (entry (seq-find (lambda (e) (string= (symbol-name (car e)) choice)) all))
         (var (nth 1 entry))
         (key (car entry))
         (seg (nth 2 entry)))
    (if (hud-modeline-segment-enabled seg)
        (hud-modeline-disable-segment var key)
      (hud-modeline-enable-segment var key))
    (force-mode-line-update t)))

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
