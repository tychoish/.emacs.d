;;; sprite.el --- Manage subordinate Emacs daemon instances -*- lexical-binding: t; -*-

;; Author: Sam Kleinman
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (seq "2.24") (transient "0.4"))
;; Keywords: tools, daemon, processes

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; sprite.el manages "sprite" Emacs daemons that are children of a
;; parent Emacs instance.  Each sprite runs as a named emacs --daemon
;; subprocess and is addressable via emacsclient.
;;
;; Sprite names follow the pattern "<parent>.<idx>.<unique-name>", where
;; idx is the lowest integer not already in use for that parent.
;;
;; Entry points:
;;   `sprites-list'              — open the overview buffer
;;   `sprites-create'            — spawn a new sprite daemon
;;   `sprites-eval'              — evaluate a form in a sprite
;;   `sprites-get-next'          — get the next available sprite
;;   `sprites-get-or-create-next' — get or create a sprite
;;

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'seq)
(require 'subr-x)
(require 'map)
(require 'tabulated-list)
(require 'transient)

(declare-function tychoish/conf-state-path "tychoish-bootstrap")
(declare-function tychoish/resolve-instance-id "tychoish-bootstrap")

;;;; Identity

(defconst sprite--state-subdir "sprites"
  "Subdirectory name under the parent's state path where sprite state lives.")

(defvar sprite/cli-instance-id nil
  "CLI-specified sprite instance name; set from command-line args via --id.")

(defun sprite/cli-resolve-id ()
  "Parse --id=NAME or --id NAME from `command-line-functions' context.
Sets `sprite/cli-instance-id' when found.  Mirrors `cli/resolve-id' in init.el."
  (or (when (string-equal "--id" argi)
        (setq sprite/cli-instance-id (pop argv)))
      (when (and (> (length argi) 5)
                 (or (string-prefix-p "--id=" argi)
                     (string-prefix-p "--id " argi)))
        (setq sprite/cli-instance-id (substring argi 5)))))

(defun sprite--format-full-name (parent idx unique-name)
  "Format PARENT, IDX, and UNIQUE-NAME into a sprite full name."
  (format "%s.%d.%s" parent idx unique-name))

(defun sprite--parse-full-name (full-name)
  "Parse FULL-NAME into (PARENT IDX UNIQUE-NAME) or nil if malformed.
All three segments must be non-empty and contain no dots."
  (when (string-match "^\\([^.]+\\)\\.\\([0-9]+\\)\\.\\([^.]+\\)$" full-name)
    (list (match-string 1 full-name)
          (string-to-number (match-string 2 full-name))
          (match-string 3 full-name))))

(defun sprite--full-name-p (name)
  "Return t if NAME matches the sprite full-name pattern <parent>.<idx>.<unique>."
  (and (stringp name)
       (string-match-p "^[^.]+\\.[0-9]+\\.[^.]+$" name)
       t))

(defun sprite--parent-letter (parent-id)
  "Return the first character of PARENT-ID as a single-char string."
  (substring parent-id 0 1))

(defun sprite--mode-line-id (full-name)
  "Return a mode-line display string for FULL-NAME.
For sprite names, abbreviates the parent to its first letter.
For top-level names, returns FULL-NAME unchanged."
  (if-let* ((parts (sprite--parse-full-name full-name)))
    (format "%s.%d.%s"
            (sprite--parent-letter (car parts))
            (cadr parts)
            (caddr parts))
    full-name))

(defun sprite--mode-line-string ()
  "Return abbreviated mode-line string if running as a sprite, or nil."
  (when-let* ((id (tychoish/resolve-instance-id))
              ((sprite--full-name-p id)))
    (sprite--mode-line-id id)))

(defun sprite-state-directory (&optional full-name)
  "Return the state directory for sprite FULL-NAME, or the sprites root if nil.
The sprites root is nested under the parent instance's state path."
  (let ((base (file-name-as-directory
               (tychoish/conf-state-path sprite--state-subdir))))
    (if full-name
        (file-name-concat base full-name)
      base)))

(defun sprite-conf-state-path (name &optional full-name)
  "Return path for NAME within FULL-NAME's state directory.
If FULL-NAME is nil, returns path under the sprites root."
  (file-name-concat (sprite-state-directory full-name) name))

;;;; Struct and registry

(cl-defstruct (sprite (:constructor sprite--make) (:copier nil))
  "Registry entry for a managed sprite daemon."
  name
  idx
  parent
  unique-name
  pid
  state-dir
  spawned-by
  start-time
  last-contact)

(defvar sprite--registry (make-hash-table :test #'equal)
  "In-memory registry mapping full-name strings to `sprite' structs.")

(defun sprite--registry-get (name)
  "Return the sprite struct for NAME, or nil."
  (gethash name sprite--registry))

(defun sprite--registry-put (s)
  "Store sprite struct S in the registry under its name."
  (puthash (sprite-name s) s sprite--registry))

(defun sprite--registry-remove (name)
  "Remove the registry entry for NAME."
  (remhash name sprite--registry))

(defun sprite--registry-all ()
  "Return a list of all sprite structs in the registry."
  (map-values sprite--registry))

(defvar sprite--registry-saved nil
  "Serialized registry for savehist persistence.
A list of plists; populated by `sprite--registry-serialize'.")

(defun sprite--struct-to-plist (s)
  "Convert sprite struct S to a plist for serialization."
  (list :name (sprite-name s)
        :idx (sprite-idx s)
        :parent (sprite-parent s)
        :unique-name (sprite-unique-name s)
        :pid (sprite-pid s)
        :state-dir (sprite-state-dir s)
        :spawned-by (sprite-spawned-by s)
        :start-time (sprite-start-time s)
        :last-contact (sprite-last-contact s)))

(defun sprite--plist-to-struct (plist)
  "Reconstruct a sprite struct from PLIST."
  (sprite--make
   :name (plist-get plist :name)
   :idx (plist-get plist :idx)
   :parent (plist-get plist :parent)
   :unique-name (plist-get plist :unique-name)
   :pid (plist-get plist :pid)
   :state-dir (plist-get plist :state-dir)
   :spawned-by (plist-get plist :spawned-by)
   :start-time (plist-get plist :start-time)
   :last-contact (plist-get plist :last-contact)))

(defun sprite--registry-serialize ()
  "Serialize the registry to `sprite--registry-saved' for savehist."
  (setq sprite--registry-saved
        (seq-map #'sprite--struct-to-plist (sprite--registry-all))))

(defun sprite--registry-deserialize ()
  "Restore the registry hash table from `sprite--registry-saved'."
  (clrhash sprite--registry)
  (seq-do (lambda (plist)
            (sprite--registry-put (sprite--plist-to-struct plist)))
          (or sprite--registry-saved nil)))

(defun sprite--discover-state-dirs ()
  "Return full-name strings for all sprite state directories on disk."
  (when (file-directory-p (sprite-state-directory))
    (thread-last (directory-files (sprite-state-directory) nil nil t)
      (seq-filter #'sprite--full-name-p))))

(defun sprite--decommissioned-p (full-name)
  "Return t if the DECOMMISSIONED marker file exists for FULL-NAME."
  (file-exists-p
   (file-name-concat (sprite-state-directory full-name) "DECOMMISSIONED")))

(defun sprite--write-decommissioned-file (full-name)
  "Write the DECOMMISSIONED marker file for FULL-NAME.  Returns t on success."
  (condition-case _
      (progn
        (write-region "" nil
                      (file-name-concat (sprite-state-directory full-name)
                                        "DECOMMISSIONED"))
        t)
    (error nil)))

(defun sprite--allocated-indices (parent)
  "Return list of idx integers in use for PARENT, from disk state directories."
  (thread-last (sprite--discover-state-dirs)
    (seq-map #'sprite--parse-full-name)
    (seq-filter (lambda (parts)
                  (and parts
                       (equal parent (car parts))
                       (not (sprite--decommissioned-p
                              (sprite--format-full-name
                               (car parts) (cadr parts) (caddr parts)))))))
    (seq-map #'cadr)))

(defun sprite--next-idx (parent)
  "Return the lowest non-negative integer index not in use for PARENT."
  (let ((used (sprite--allocated-indices parent))
        (n 0))
    (while (member n used)
      (setq n (1+ n)))
    n))

(defun sprite--make-from-state-dir (full-name)
  "Create a minimal sprite struct from a discovered state directory name FULL-NAME."
  (if-let* ((parts (sprite--parse-full-name full-name)))
    (sprite--make :name full-name
                  :idx (cadr parts)
                  :parent (car parts)
                  :unique-name (caddr parts)
                  :state-dir (sprite-state-directory full-name))
    (error "Cannot parse sprite name: %s" full-name)))

(defun sprite--discover-and-sync-registry ()
  "Reconcile the registry with the filesystem.
Adds discovered sprites not in the registry.  Does not remove entries."
  (seq-do (lambda (name)
            (unless (sprite--registry-get name)
              (sprite--registry-put (sprite--make-from-state-dir name))))
          (sprite--discover-state-dirs)))

;;;; Lifecycle

(defun sprite--resolve-name (name)
  "Resolve NAME to a full sprite name.
NAME may be a full name (returned as-is) or a unique-name matched in the registry.
Signals `user-error' if NAME cannot be resolved."
  (cond
   ((sprite--registry-get name) name)
   (t
    (if-let* ((found (seq-find (lambda (s)
                                 (equal name (sprite-unique-name s)))
                               (sprite--registry-all))))
      (sprite-name found)
      (user-error "No sprite found with name: %s" name)))))

(defun sprite--server-file (full-name)
  "Return the server identifier for FULL-NAME sprite.
Used as the value of EMACS_SERVER_FILE when calling emacsclient."
  full-name)

(defun sprite--ensure-state-dir (full-name)
  "Create the state directory for FULL-NAME if it does not exist.
Returns the state directory path."
  (let ((dir (sprite-state-directory full-name)))
    (make-directory dir t)
    dir))

(defun sprite--spawn (full-name)
  "Low-level: start an Emacs daemon named FULL-NAME.
Returns the sprite struct with `start-time' set.  Updates the registry."
  (let ((s (or (sprite--registry-get full-name)
               (sprite--make-from-state-dir full-name))))
    (let ((proc (start-process "sprite-daemon" nil "emacs"
                               (concat "--daemon=" full-name))))
      (setf (sprite-pid s) (process-id proc))
      (setf (sprite-start-time s) (current-time))
      (sprite--registry-put s)
      s)))

(defun sprite--running-p (full-name)
  "Return t if the sprite FULL-NAME appears to be live."
  (when-let* ((s (sprite--registry-get full-name))
              (pid (sprite-pid s)))
    (= 0 (condition-case _
             (signal-process pid 0)
           (error 1)))))

(defun sprite--wait-for-server (full-name &optional timeout-secs)
  "Poll until sprite FULL-NAME's server is accepting connections.
Times out after TIMEOUT-SECS seconds (default 10).  Returns t on success."
  (let ((deadline (time-add (current-time) (or timeout-secs 10)))
        (ready nil))
    (while (and (not ready) (time-less-p (current-time) deadline))
      (when (= 0 (call-process "emacsclient" nil nil nil
                               "--socket-name" full-name
                               "--eval" "t"))
        (setq ready t))
      (unless ready
        (sleep-for 0.5)))
    ready))

(defun sprites-create (unique-name)
  "Spawn a new sprite daemon with UNIQUE-NAME under the current instance.
The current instance's ID becomes the parent.
Returns the new sprite struct."
  (interactive "sSprite unique name: ")
  (let* ((parent (tychoish/resolve-instance-id))
         (idx (sprite--next-idx parent))
         (full-name (sprite--format-full-name parent idx unique-name)))
    (when (sprite--decommissioned-p full-name)
      (user-error "Sprite %s is decommissioned and cannot be restarted" full-name))
    (sprite--ensure-state-dir full-name)
    (let ((s (sprite--spawn full-name)))
      (setf (sprite-spawned-by s)
            (buffer-name (current-buffer)))
      (sprite--registry-put s)
      s)))

(defun sprites-stop (name)
  "Send (kill-emacs) to sprite NAME via emacsclient."
  (interactive (list (completing-read "Stop sprite: "
                                      (seq-map #'sprite-name (sprite--registry-all)))))
  (let ((full-name (sprite--resolve-name name)))
    (call-process "emacsclient" nil nil nil
                  "--socket-name" full-name
                  "--eval" "(kill-emacs)")))

(defun sprites-restart (name)
  "Restart sprite NAME: stop it then create a new daemon with the same unique-name."
  (interactive (list (completing-read "Restart sprite: "
                                      (seq-map #'sprite-name (sprite--registry-all)))))
  (let* ((full-name (sprite--resolve-name name))
         (s (sprite--registry-get full-name)))
    (when (sprite--decommissioned-p full-name)
      (user-error "Cannot restart decommissioned sprite: %s" full-name))
    (sprites-stop full-name)
    (sprite--ensure-state-dir full-name)
    (sprite--spawn full-name)
    (when s
      (setf (sprite-start-time (sprite--registry-get full-name)) (current-time)))))

(defun sprites-decommission (name)
  "Decommission sprite NAME: stop it and write the DECOMMISSIONED marker."
  (interactive (list (completing-read "Decommission sprite: "
                                      (seq-map #'sprite-name (sprite--registry-all)))))
  (let ((full-name (sprite--resolve-name name)))
    (condition-case _ (sprites-stop full-name) (error nil))
    (sprite--ensure-state-dir full-name)
    (unless (sprite--write-decommissioned-file full-name)
      (error "Failed to write DECOMMISSIONED file for %s" full-name))
    (when-let* ((s (sprite--registry-get full-name)))
      (setf (sprite-last-contact s) (current-time)))))

;;;; Communication

(defun sprite--log-buffer-name (name)
  "Return the name of the log buffer for sprite NAME."
  (format "*sprite:%s*" name))

(defun sprite--log (name direction content)
  "Log a message to the sprite NAME log buffer.
DIRECTION is the symbol `sent' or `received'.  CONTENT is a string."
  (with-current-buffer (get-buffer-create (sprite--log-buffer-name name))
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (format "[%s] %s: %s\n"
                      (format-time-string "%H:%M:%S")
                      direction
                      content)))))

(cl-defmacro with-sprite (name &rest body)
  "Execute BODY with EMACS_SERVER_FILE set to the server identifier for NAME.
Returns the value of the last form in BODY."
  (declare (indent 1))
  (let ((gname (make-symbol "name")))
    `(let* ((,gname ,name)
            (process-environment
             (cons (concat "EMACS_SERVER_FILE=" (sprite--server-file ,gname))
                   process-environment)))
       ,@body)))

(defun sprites-eval (name form)
  "Evaluate FORM in the sprite NAME via emacsclient.
Returns the read result, or nil on failure.  Logs to the sprite's log buffer."
  (sprite--log name 'sent (format "%S" form))
  (let ((result
         (with-sprite name
           (condition-case err
               (with-temp-buffer
                 (let ((exit-code (call-process "emacsclient" nil t nil
                                                "--eval" (format "%S" form))))
                   (when (= exit-code 0)
                     (condition-case _
                         (read (buffer-string))
                       (error (string-trim (buffer-string)))))))
             (error
              (sprite--log name 'error (error-message-string err))
              nil)))))
    (when result
      (sprite--log name 'received (format "%S" result))
      (when-let* ((s (sprite--registry-get name)))
        (setf (sprite-last-contact s) (current-time))))
    result))

(defun sprites-open-log (name)
  "Switch to the communication log buffer for sprite NAME."
  (interactive (list (completing-read "Sprite log: "
                                      (seq-map #'sprite-name (sprite--registry-all)))))
  (pop-to-buffer (sprite--log-buffer-name name)))

;;;; Overview buffer

(defun sprite--format-uptime (seconds)
  "Format SECONDS as a human-readable uptime string.
Returns \"?\" when SECONDS is nil."
  (cond
   ((null seconds) "?")
   ((< seconds 60) (format "%ds" (round seconds)))
   ((< seconds 3600) (format "%dm" (round (/ seconds 60))))
   ((< seconds 86400) (format "%dh" (round (/ seconds 3600))))
   (t (format "%dd" (round (/ seconds 86400))))))

(defun sprite--query-buffer-count (full-name)
  "Try to get the buffer count from sprite FULL-NAME.  Returns integer or nil."
  (condition-case _
      (with-sprite full-name
        (with-temp-buffer
          (when (= 0 (call-process "emacsclient" nil t nil
                                   "--eval" "(length (buffer-list))"))
            (string-to-number (string-trim (buffer-string))))))
    (error nil)))

(defun sprite--for-current-parent-p (s)
  "Return t if sprite S belongs to the current parent instance."
  (equal (sprite-parent s) (tychoish/resolve-instance-id)))

(defun sprite--build-list-entry (s)
  "Build a `tabulated-list' entry for sprite struct S."
  (let* ((name (sprite-name s))
         (uptime (when (sprite-start-time s)
                   (float-time (time-since (sprite-start-time s)))))
         (last-seen (when (sprite-last-contact s)
                      (float-time (time-since (sprite-last-contact s)))))
         (buffers (or (sprite--query-buffer-count name) "?"))
         (spawned-by (or (sprite-spawned-by s) "")))
    (list s
          (vector
           (number-to-string (sprite-idx s))
           name
           (sprite--format-uptime uptime)
           (if (numberp buffers) (number-to-string buffers) "?")
           (sprite--format-uptime last-seen)
           spawned-by))))

(defvar sprite-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "c") #'sprites-list-create)
    (define-key map (kbd "d") #'sprites-list-decommission)
    (define-key map (kbd "r") #'sprites-list-restart)
    (define-key map (kbd "s") #'sprites-list-stop)
    (define-key map (kbd "o") #'sprites-list-open-log)
    (define-key map (kbd "f") #'sprites-list-open-frame)
    (define-key map (kbd "g") #'sprites-list-refresh)
    (define-key map (kbd "?") #'sprite-list-menu)
    map)
  "Keymap for `sprite-list-mode'.")

(define-derived-mode sprite-list-mode tabulated-list-mode "Sprites"
  "Major mode for viewing and managing sprite daemons.

\\{sprite-list-mode-map}"
  (setq tabulated-list-format
        (vector
         '("#"          4  t)
         '("Name"      30  t)
         '("Uptime"    10  nil)
         '("Buffers"    7  nil)
         '("Last Seen" 12  nil)
         '("Spawned By" 20 nil)))
  (setq tabulated-list-sort-key '("Name" . nil))
  (tabulated-list-init-header))

(defun sprites-list-refresh ()
  "Refresh the sprites overview buffer."
  (interactive)
  (sprite--discover-and-sync-registry)
  (setq tabulated-list-entries
        (thread-last (sprite--registry-all)
          (seq-filter (lambda (s)
                        (and (sprite--for-current-parent-p s)
                             (not (sprite--decommissioned-p (sprite-name s))))))
          (seq-map #'sprite--build-list-entry)))
  (tabulated-list-print t))

(defun sprites-list ()
  "Open the sprites overview buffer."
  (interactive)
  (let ((buf (get-buffer-create "*Sprites*")))
    (with-current-buffer buf
      (sprite-list-mode)
      (sprites-list-refresh))
    (pop-to-buffer buf)))

(defun sprites-list--sprite-at-point ()
  "Return the sprite struct at point, or signal `user-error'."
  (or (tabulated-list-get-id)
      (user-error "No sprite at point")))

(defun sprites-list-create ()
  "Create a new sprite from the overview buffer."
  (interactive)
  (call-interactively #'sprites-create)
  (sprites-list-refresh))

(defun sprites-list-decommission ()
  "Decommission the sprite at point."
  (interactive)
  (let ((name (sprite-name (sprites-list--sprite-at-point))))
    (when (yes-or-no-p (format "Decommission sprite %s? " name))
      (sprites-decommission name)
      (sprites-list-refresh))))

(defun sprites-list-restart ()
  "Restart the sprite at point."
  (interactive)
  (sprites-restart (sprite-name (sprites-list--sprite-at-point)))
  (sprites-list-refresh))

(defun sprites-list-stop ()
  "Stop the sprite at point."
  (interactive)
  (sprites-stop (sprite-name (sprites-list--sprite-at-point)))
  (sprites-list-refresh))

(defun sprites-list-open-log ()
  "Open the communication log for the sprite at point."
  (interactive)
  (sprites-open-log (sprite-name (sprites-list--sprite-at-point))))

(defun sprites-list-open-frame ()
  "Open a new Emacs frame connected to the sprite at point."
  (interactive)
  (let ((name (sprite-name (sprites-list--sprite-at-point))))
    (start-process "sprite-frame" nil "emacsclient"
                   "-c" "--socket-name" name)))

(transient-define-prefix sprite-list-menu ()
  "Actions for the sprite overview buffer."
  [["Sprite"
    ("c" "Create"       sprites-list-create)
    ("d" "Decommission" sprites-list-decommission)
    ("r" "Restart"      sprites-list-restart)
    ("s" "Stop"         sprites-list-stop)]
   ["Navigate"
    ("o" "Open log"     sprites-list-open-log)
    ("f" "New frame"    sprites-list-open-frame)
    ("g" "Refresh"      sprites-list-refresh)
    ("q" "Quit"         quit-window)]])

;;;; Fleet API

(defcustom sprite-busy-threshold 30
  "Seconds since last-contact after which a sprite is considered available.
A sprite is \"busy\" if it was contacted within this many seconds."
  :type 'integer
  :group 'sprite)

(defcustom sprite-max-count 4
  "Maximum number of sprites `sprites-get-or-create-next' will spawn."
  :type 'integer
  :group 'sprite)

(defun sprites-resolve-list ()
  "Return list of sprite structs accessible to this instance.
Includes direct children.  If this instance is itself a sprite (worker),
also includes sibling sprites from the same parent."
  (let ((own-id (tychoish/resolve-instance-id)))
    (if (sprite--full-name-p own-id)
      (let ((parts (sprite--parse-full-name own-id)))
        (thread-last (sprite--registry-all)
          (seq-filter (lambda (s)
                        (and (not (sprite--decommissioned-p (sprite-name s)))
                             (or (equal own-id (sprite-parent s))
                                 (equal (car parts) (sprite-parent s))))))))
      (thread-last (sprite--registry-all)
        (seq-filter (lambda (s)
                      (and (equal own-id (sprite-parent s))
                           (not (sprite--decommissioned-p (sprite-name s))))))))))

(defun sprites-controller-p ()
  "Return t if this instance has at least one non-decommissioned sprite."
  (not (null (thread-last (sprite--registry-all)
               (seq-filter (lambda (s)
                              (and (equal (tychoish/resolve-instance-id)
                                         (sprite-parent s))
                                   (not (sprite--decommissioned-p
                                          (sprite-name s))))))))))

(defun sprites-worker-p ()
  "Return t if this instance is itself a sprite (has a parent)."
  (sprite--full-name-p (tychoish/resolve-instance-id)))

(defun sprites-available-p ()
  "Return t if any accessible sprites are configured."
  (not (null (sprites-resolve-list))))

(defun sprite--not-busy-p (s)
  "Return t if sprite S has not been contacted recently."
  (or (null (sprite-last-contact s))
      (> (float-time (time-since (sprite-last-contact s)))
         sprite-busy-threshold)))

(defun sprites-get-next ()
  "Return the struct of the next available sprite, or nil.
\"Available\" means running and not recently contacted."
  (seq-find (lambda (s)
              (and (sprite--running-p (sprite-name s))
                   (sprite--not-busy-p s)))
            (sprites-resolve-list)))

(defun sprites-get-or-create-next ()
  "Return the next available sprite, creating one if none are free.
Signals `user-error' if `sprite-max-count' would be exceeded."
  (or (sprites-get-next)
      (let ((active-count (length (sprites-resolve-list))))
        (if (>= active-count sprite-max-count)
            (user-error "All %d sprites are busy and max-count (%d) reached"
                        active-count sprite-max-count)
          (sprites-create (format "worker-%d" active-count))))))

;;;; Mode-line and savehist wiring

(defun sprite--on-savehist-load ()
  "Restore the sprite registry after savehist loads."
  (sprite--registry-deserialize))

(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables 'sprite--registry-saved)
  (add-hook 'savehist-save-hook #'sprite--registry-serialize)
  (add-hook 'savehist-mode-hook #'sprite--on-savehist-load))

(provide 'sprite)
;;; sprite.el ends here
