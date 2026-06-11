;;; sprite.el --- Manage subordinate Emacs daemon instances -*- lexical-binding: t; -*-

;; Author: Sam Kleinman
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (seq "2.24") (transient "0.4") (annotated-completing-read "0.1"))
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
;;   `sprite-list'              — open the overview buffer
;;   `sprite-create'            — spawn a new sprite daemon
;;   `with-sprite'               — evaluate a form in a sprite
;;   `sprite-get-next'          — get the next available sprite
;;   `sprite-get-or-create-next' — get or create a sprite
;;

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'seq)
(require 'subr-x)
(require 'map)
(require 'tabulated-list)
(require 'transient)

(declare-function annotated-completing-read "annotated-completing-read")

;;;; Instance identity and state paths

(defconst sprite--conf-state-directory "state"
  "Name of the state subdirectory under `user-emacs-directory'.")

(defconst sprite--state-subdir "sprite"
  "Subdirectory name under the state path where sprite state lives.")

(defvar sprite-instance-id nil
  "Name of the running Emacs instance.
Set by `sprite-set-up-instance-name' at startup.")

(defvar sprite--system-name-cached nil
  "Cached value of the output of `system-name' for use in system management contexts")

(defvar sprite-cli-instance-id nil
  "CLI-specified instance name; set from --id command-line arguments.")

(defun sprite-resolve-instance-id ()
  "Return the current Emacs instance ID.
Resolution order: daemon name, `sprite-cli-instance-id',
`sprite-instance-id', then \"solo\"."
  (let ((daemon (daemonp)))
    (or (when (eq daemon t) "primary")
        (when (stringp daemon)
	  (if server-use-tcp
	      (setenv "EMACS_SERVER_FILE" daemon)
	    (setenv "EMACS_SOCKET_FILE" (expand-file-name daemon server-socket-dir)))
          daemon)
        sprite-cli-instance-id
        sprite-instance-id
        "solo")))

(defun sprite-instance-name ()
  "Return the current Emacs instance name, initialising it if needed.
Caches the result in `sprite-instance-id'.  This is the preferred public
accessor; call `sprite-resolve-instance-id' only when the raw resolution
chain must be re-evaluated."
  (unless sprite-instance-id
    (setq sprite-instance-id (sprite-resolve-instance-id)))
  sprite-instance-id)

(defun sprite-system-name ()
  (or sprite--system-name-cached (setq sprite--system-name-cached (system-name))))

(defun sprite-conf-host-and-instance ()
  "Return (HOSTNAME INSTANCE-ID) for state-path construction."
  (list (if (eq system-type 'darwin)
            (car (string-split (sprite-system-name) "\\."))
          (system-name))
        (sprite-instance-name)))

(defun sprite-state-file-prefix (name)
  "Return the instance-scoped filename prefix for NAME.
Produces HOSTNAME-INSTANCE-NAME[-USERNAME] where USERNAME is included
only when running as root or under a symlinked `user-emacs-directory'."
  (pcase-let ((`(,host ,instance) (sprite-conf-host-and-instance)))
    (string-join
     (seq-filter #'identity
                 (list host instance name
                       (when (or (equal "root" user-login-name)
                                 (file-symlink-p user-emacs-directory))
                         user-login-name)))
     "-")))

(defun sprite-state-path (name)
  "Return the full state-directory path for NAME, scoped to this host and instance."
  (file-name-concat
   user-emacs-directory
   sprite--conf-state-directory
   (sprite-state-file-prefix name)))

;;;; Identity

(defun sprite-cli-resolve-id ()
  "Parse --id=NAME or --id NAME from `command-line-functions' context.
Sets `sprite-cli-instance-id' when found."
  (cond
   ((string-equal "--id" argi)
    (setq sprite-cli-instance-id (pop argv)))
   ((and (> (length argi) 5)
         (or (string-prefix-p "--id=" argi)
             (string-prefix-p "--id " argi)))
    (setq sprite-cli-instance-id (substring argi 5)))))

(add-to-list 'command-line-functions #'sprite-cli-resolve-id)

(defun sprite--format-full-name (parent idx unique-name)
  "Format PARENT, IDX, and UNIQUE-NAME into a sprite full name."
  (format "%s.%d.%s" parent idx unique-name))

(defun sprite--parse-full-name (full-name)
  "Parse FULL-NAME into (PARENT IDX UNIQUE-NAME) or nil if malformed.
All three segments must be non-empty and contain no dots."
  (when-let* ((parts (string-split full-name "\\."))
              ((= (length parts) 3))
              ((seq-every-p (lambda (p) (not (string-empty-p p))) parts))
              ((string-match-p "^[0-9]+$" (nth 1 parts))))
    (list (nth 0 parts)
          (string-to-number (nth 1 parts))
          (nth 2 parts))))

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
  "Return mode-line display string for the current Emacs instance.
Always returns a non-nil string: abbreviated for sprite instances,
the raw instance id for top-level instances."
  (sprite--mode-line-id (sprite-instance-name)))

(defun sprite-state-directory (&optional full-name)
  "Return the state directory for sprite instance FULL-NAME, or the sprite root if nil."
  (let ((base (file-name-as-directory (sprite-state-path sprite--state-subdir))))
    (if full-name
        (file-name-concat base full-name)
      base)))

(defun sprite-conf-state-path (name &optional full-name)
  "Return path for NAME within FULL-NAME's state directory.
If FULL-NAME is nil, returns path under the sprite root."
  (file-name-concat (sprite-state-directory full-name) name))

(defun sprite--live-p (s)
  "Return t if sprite S is not decommissioned."
  (not (sprite--decommissioned-p (sprite-name s))))

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
  last-contact
  running-status)

(defvar sprite--registry (make-hash-table :test #'equal)
  "In-memory registry mapping full-name strings to `sprite' structs.")

(defun sprite--registry-get (name)
  "Return the sprite struct for NAME, or nil."
  (map-elt sprite--registry name))

(defun sprite--registry-put (s)
  "Store sprite struct S in the registry under its name."
  (setf (map-elt sprite--registry (sprite-name s)) s))

(defun sprite--registry-remove (name)
  "Remove the registry entry for NAME."
  (map-delete sprite--registry name))

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
  (setq sprite--registry (make-hash-table :test #'equal))
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
    (pcase-let ((`(,parent ,idx ,unique-name) parts))
      (sprite--make :name full-name
                    :idx idx
                    :parent parent
                    :unique-name unique-name
                    :state-dir (sprite-state-directory full-name)))
    (error "Cannot parse sprite name: %s" full-name)))

(defun sprite--discover-and-sync-registry ()
  "Reconcile the registry with the filesystem.
Adds discovered sprite not in the registry.  Does not remove entries."
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

(defun sprite--ensure-state-dir (full-name)
  "Create the state directory for FULL-NAME if it does not exist.
Returns the state directory path."
  (let ((dir (sprite-state-directory full-name)))
    (make-directory dir t)
    dir))

(defun sprite--spawn (full-name)
  "Low-level: start an Emacs daemon named FULL-NAME.
Returns the sprite struct with `start-time' set.  Updates the registry."
  (let* ((s (or (sprite--registry-get full-name)
                (sprite--make-from-state-dir full-name)))
         (log-buf (get-buffer-create (sprite--log-buffer-name full-name)))
         (proc (start-process "sprite-daemon" log-buf
                              (expand-file-name invocation-name invocation-directory)
                              (concat "--daemon=" full-name))))
    (setf (sprite-pid s) (process-id proc))
    (setf (sprite-start-time s) (current-time))
    (sprite--registry-put s)
    s))

(defun sprite--running-p (full-name)
  "Return t if the sprite FULL-NAME appears to be live.
Checks whether the log buffer's associated process is still alive with a
matching PID first; falls back to an emacsclient ping."
  (or (when-let* ((buf (get-buffer (sprite--log-buffer-name full-name)))
                  (proc (get-buffer-process buf))
                  ((process-live-p proc))
                  (s (sprite--registry-get full-name))
                  (pid (sprite-pid s))
                  ((= pid (process-id proc))))
        t)
      (not (null (with-sprite full-name t :no-log t)))))

(defun sprite--wait-for-server (full-name &optional timeout-secs)
  "Poll until sprite FULL-NAME's server is accepting connections.
Times out after TIMEOUT-SECS seconds (default 10).  Returns t on success."
  (let ((deadline (time-add (current-time) (or timeout-secs 10)))
        ready)
    (while (and (not ready) (time-less-p (current-time) deadline))
      (when (with-sprite full-name t :no-log t)
        (setq ready t))
      (unless ready
        (sleep-for 0.5)))
    ready))

(defun sprite-create (unique-name)
  "Spawn a new sprite daemon with UNIQUE-NAME under the current instance.
The current instance's ID becomes the parent.
Returns the new sprite struct."
  (interactive "sSprite unique name: ")
  (let* ((parent (sprite-instance-name))
         (idx (sprite--next-idx parent))
         (full-name (sprite--format-full-name parent idx unique-name)))
    (when (sprite--decommissioned-p full-name)
      (user-error "Sprite %s is decommissioned and cannot be restarted" full-name))
    (sprite--ensure-state-dir full-name)
    (let ((s (sprite--spawn full-name)))
      (setf (sprite-spawned-by s)
            (buffer-name (current-buffer)))
      (sprite--registry-put s)
      (unless (sprite--wait-for-server full-name sprite-startup-timeout)
        (user-error "Sprite %s did not become ready within %ds"
                    full-name sprite-startup-timeout))
      s)))

(defun sprite--completing-read (prompt)
  "Read a sprite name from the registry using annotated completion.
Each candidate is annotated with its index and uptime."
  (annotated-completing-read
   (seq-map (lambda (s)
              (cons (sprite-name s)
                    (format "idx:%d uptime:%s"
                            (sprite-idx s)
                            (if (sprite-start-time s)
                                (sprite--format-uptime (float-time (time-since (sprite-start-time s))))
                              "?"))))
            (sprite--registry-all))
   :prompt prompt
   :require-match t))

(defun sprite-stop (name)
  "Send (kill-emacs) to sprite NAME via emacsclient."
  (interactive (list (sprite--completing-read "Stop sprite: ")))
  (with-sprite (sprite--resolve-name name) (kill-emacs)))
(defun sprite-restart (name)
  "Restart sprite NAME: stop it then create a new daemon with the same unique-name."
  (interactive (list (sprite--completing-read "Restart sprite: ")))
  (let* ((full-name (sprite--resolve-name name))
         (s (sprite--registry-get full-name)))
    (when (sprite--decommissioned-p full-name)
      (user-error "Cannot restart decommissioned sprite: %s" full-name))
    (sprite-stop full-name)
    (sprite--ensure-state-dir full-name)
    (sprite--spawn full-name)
    (when s
      (setf (sprite-start-time (sprite--registry-get full-name)) (current-time)))))

(defun sprite-decommission (name)
  "Decommission sprite NAME: stop it and write the DECOMMISSIONED marker."
  (interactive (list (sprite--completing-read "Decommission sprite: ")))
  (let ((full-name (sprite--resolve-name name)))
    (condition-case _ (sprite-stop full-name) (error nil))
    (sprite--ensure-state-dir full-name)
    (unless (sprite--write-decommissioned-file full-name)
      (error "Failed to write DECOMMISSIONED file for %s" full-name))
    (when-let* ((s (sprite--registry-get full-name)))
      (setf (sprite-last-contact s) (current-time)))))

;;;; Communication

(defconst sprite--log-time-format "%H:%M:%S"
  "Format string for timestamps in sprite log buffers.")

(defun sprite--log-buffer-name (name)
  "Return the name of the log buffer for sprite NAME."
  (format "*sprite:%s*" name))

(defun sprite--log (name direction content)
  "Log a message to the sprite NAME log buffer.
DIRECTION is the symbol `sent' or `received'.  CONTENT is a string."
  (with-current-buffer (get-buffer-create (sprite--log-buffer-name name))
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert (propertize (format "%s" direction) 'face 'bold))
      (insert (format " [%s]: %s\n"
                      (format-time-string sprite--log-time-format)
                      content)))))

(defun sprite--call (name form &optional buffer)
  "The single site where emacsclient is invoked for eval-based communication.
Evaluates FORM in sprite NAME; captures output in BUFFER when non-nil.
Returns the emacsclient exit code."
  (call-process "emacsclient" nil buffer nil
                "--socket-name" name "--eval" (format "%S" form)))

(defun sprite--call-and-read (name form)
  "Invoke emacsclient against NAME evaluating FORM; return the read result.
Returns the read Lisp value on success, nil if the call fails or output is
unreadable.  Process errors propagate to the caller."
  (with-temp-buffer
    (when (= 0 (sprite--call name form (current-buffer)))
      (condition-case _ (read (buffer-string))
        (error nil)))))

(cl-defmacro with-sprite (name form &key no-log)
  "Evaluate FORM in sprite NAME via emacsclient.
Returns the read result, or nil if the connection fails.
Unless NO-LOG is non-nil, logs the exchange and updates last-contact."
  (declare (indent 1))
  (let ((gname (make-symbol "name"))
        (gresult (make-symbol "result")))
    `(let ((,gname ,name))
       ,(unless no-log `(sprite--log ,gname 'sent (format "%S" ',form)))
       (let ((,gresult
              (condition-case ,(if no-log '_ 'err)
                  (sprite--call-and-read ,gname ',form)
                (error
                 ,(unless no-log
                    `(sprite--log ,gname 'error (error-message-string err)))
                 nil))))
         ,(unless no-log
            `(when ,gresult
               (sprite--log ,gname 'received (format "%S" ,gresult))
               (when-let* ((s (sprite--registry-get ,gname)))
                 (setf (sprite-last-contact s) (current-time)))))
         ,gresult))))

(defun sprite-open-log (name)
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
  (when-let* ((n (with-sprite full-name (length (buffer-list)) :no-log t))
              ((numberp n)))
    n))

(defun sprite--for-current-parent-p (s)
  "Return t if sprite S belongs to the current parent instance."
  (equal (sprite-parent s) (sprite-instance-name)))

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

(defun sprite--build-parent-entry ()
  "Build a `tabulated-list' entry representing the current parent instance."
  (let* ((name (sprite-instance-name))
         (uptime (when (boundp 'before-init-time)
                   (float-time (time-since before-init-time))))
         (buffers (length (buffer-list))))
    (list (sprite--make :name name
                        :idx nil
                        :parent nil
                        :unique-name name)
          (vector
           "-"
           (propertize name 'face 'bold)
           (sprite--format-uptime uptime)
           (number-to-string buffers)
           "(self)"
           "system"))))

(defconst sprite--list-buffer-name "*sprite-list*"
  "Name of the sprite overview buffer.")

(defvar sprite-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "c") #'sprite-list-create)
    (define-key map (kbd "d") #'sprite-list-decommission)
    (define-key map (kbd "r") #'sprite-list-restart)
    (define-key map (kbd "s") #'sprite-list-stop)
    (define-key map (kbd "p") #'sprite-list-check)
    (define-key map (kbd "o") #'sprite-list-open-log)
    (define-key map (kbd "f") #'sprite-list-open-frame)
    (define-key map (kbd "g") #'sprite-list-refresh)
    (define-key map (kbd "?") #'sprite-list-menu)
    (define-key map (kbd "m") #'sprite-list-menu)
    map)
  "Keymap for `sprite-list-mode'.")

(define-derived-mode sprite-list-mode tabulated-list-mode "sprite"
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

(defun sprite-list-refresh ()
  "Refresh the sprite overview buffer."
  (interactive)
  (sprite--discover-and-sync-registry)
  (setq tabulated-list-entries
        (cons (sprite--build-parent-entry)
              (thread-last (sprite--registry-all)
                (seq-filter (lambda (s)
                              (and (sprite--for-current-parent-p s)
                                   (sprite--live-p s))))
                (seq-map #'sprite--build-list-entry))))
  (tabulated-list-print t))

(defun sprite-list ()
  "Open the sprite overview buffer."
  (interactive)
  (with-current-buffer (get-buffer-create sprite--list-buffer-name)
    (unless (derived-mode-p 'sprite-list-mode)
      (sprite-list-mode))
    (sprite-list-refresh)
    (pop-to-buffer (current-buffer))))

(defun sprite-list--sprite-at-point-p ()
  "Return non-nil if there is a managed sprite at point (not the parent row)."
  (when-let* ((s (tabulated-list-get-id)))
    (not (null (sprite-idx s)))))

(defun sprite-list--log-exists-at-point-p ()
  "Return non-nil if a log buffer exists for the sprite at point."
  (when-let* ((s (tabulated-list-get-id)))
    (get-buffer (sprite--log-buffer-name (sprite-name s)))))

(defun sprite-list--known-dead-p ()
  "Return non-nil if the sprite at point has been checked and is not running."
  (when-let* ((s (tabulated-list-get-id))
              ((sprite-idx s)))
    (eq (sprite-running-status s) 'dead)))

(defun sprite-list--can-open-frame-p ()
  "Return non-nil if a frame can be opened for the sprite at point.
True when point is on a sprite row that is not known to be dead."
  (and (sprite-list--sprite-at-point-p)
       (not (sprite-list--known-dead-p))))

(defun sprite-list--sprite-at-point ()
  "Return the managed sprite struct at point, or signal `user-error'.
Signals an error if point is on the parent-instance row."
  (if-let* ((s (tabulated-list-get-id))
            ((sprite-idx s)))
    s
    (user-error "No sprite at point")))

(defun sprite-list-create ()
  "Create a new sprite from the overview buffer."
  (interactive)
  (call-interactively #'sprite-create)
  (sprite-list-refresh))

(defun sprite-list-decommission ()
  "Decommission the sprite at point."
  (interactive)
  (when-let* ((name (sprite-name (sprite-list--sprite-at-point)))
              ((yes-or-no-p (format "Decommission sprite %s? " name))))
    (sprite-decommission name)
    (sprite-list-refresh)))

(defun sprite-list-restart ()
  "Restart the sprite at point."
  (interactive)
  (sprite-restart (sprite-name (sprite-list--sprite-at-point)))
  (sprite-list-refresh))

(defun sprite-list-stop ()
  "Stop the sprite at point."
  (interactive)
  (sprite-stop (sprite-name (sprite-list--sprite-at-point)))
  (sprite-list-refresh))

(defun sprite-list-open-log ()
  "Open the communication log for the sprite at point."
  (interactive)
  (sprite-open-log (sprite-name (sprite-list--sprite-at-point))))

(defun sprite-list-check ()
  "Verify whether the sprite at point is reachable via emacsclient.
Updates the sprite's running status and refreshes the list."
  (interactive)
  (let* ((s (sprite-list--sprite-at-point))
         (name (sprite-name s))
         (running (sprite--running-p name)))
    (setf (sprite-running-status s) (if running 'running 'dead))
    (sprite-list-refresh)
    (message "Sprite %s: %s" name (if running "running" "not running"))))

(defun sprite-list-open-frame ()
  "Open a new Emacs frame connected to the sprite at point."
  (interactive)
  (let ((name (sprite-name (sprite-list--sprite-at-point))))
    (with-environment-variables (("DISPLAY" (or (getenv "DISPLAY") ":0"))
				 ("TERM" nil))
    (start-process
     (format "sprite-frame-%s" name)
     (get-buffer-create (sprite--log-buffer-name name))
     "emacsclient" "--no-wait" "--create-frame" "--server-file" name))))

(transient-define-prefix sprite-list-menu ()
  "Actions for the sprite overview buffer."
  [["Sprite"
    ("c" "Create"       sprite-list-create)
    ("d" "Decommission" sprite-list-decommission
     :inapt-if-not sprite-list--sprite-at-point-p)
    ("r" "Restart"      sprite-list-restart
     :inapt-if-not sprite-list--sprite-at-point-p)
    ("s" "Stop"         sprite-list-stop
     :inapt-if-not sprite-list--sprite-at-point-p)
    ("p" "Check/ping"   sprite-list-check
     :inapt-if-not sprite-list--sprite-at-point-p)]
   ["Navigate"
    ("o" "Open log"     sprite-list-open-log
     :inapt-if-not sprite-list--log-exists-at-point-p)
    ("f" "New frame"    sprite-list-open-frame
     :inapt-if-not sprite-list--can-open-frame-p)
    ("g" "Refresh"      sprite-list-refresh)
    ("q" "Quit"         quit-window)]])

;;;; Fleet API

(defcustom sprite-active-threshold 30
  "Seconds since last-contact after which a sprite is considered available.
A sprite is \"active\" if it was contacted within this many seconds."
  :type 'integer
  :group 'sprite)

(defcustom sprite-max-count 4
  "Maximum number of sprite `sprite-get-or-create-next' will spawn."
  :type 'integer
  :group 'sprite)

(defcustom sprite-startup-timeout 10
  "Seconds to wait for a newly spawned sprite server to accept connections."
  :type 'integer
  :group 'sprite)

(defun sprite-worker-p ()
  "Return t if this instance is itself a sprite (has a parent)."
  (sprite--full-name-p (sprite-instance-name)))

(defun sprite-resolve-list ()
  "Return list of sprite structs accessible to this instance.
Includes direct children.  If this instance is itself a sprite (worker),
also includes sibling sprite from the same parent."
  (let ((own-id (sprite-instance-name)))
    (if (sprite-worker-p)
      (let ((parts (sprite--parse-full-name own-id)))
        (thread-last (sprite--registry-all)
          (seq-filter (lambda (s)
                        (and (sprite--live-p s)
                             (or (equal own-id (sprite-parent s))
                                 (equal (car parts) (sprite-parent s))))))))
      (thread-last (sprite--registry-all)
        (seq-filter (lambda (s)
                      (and (equal own-id (sprite-parent s))
                           (sprite--live-p s))))))))

(defun sprite-controller-p ()
  "Return t if this instance has at least one non-decommissioned sprite."
  (not (null (thread-last (sprite--registry-all)
               (seq-filter (lambda (s)
                              (and (equal (sprite-instance-name) (sprite-parent s))
                                   (sprite--live-p s))))))))

(defun sprite-available-p ()
  "Return t if any accessible sprite are configured."
  (not (null (sprite-resolve-list))))

(defun sprite--available-p (s)
  "Return t if sprite S has not been contacted recently."
  (or (null (sprite-last-contact s))
      (> (float-time (time-since (sprite-last-contact s)))
         sprite-active-threshold)))

(defun sprite-get-next ()
  "Return the struct of the next available sprite, or nil.
\"Available\" means running and not recently contacted."
  (seq-find (lambda (s)
              (and (sprite--running-p (sprite-name s))
                   (sprite--available-p s)))
            (sprite-resolve-list)))

(defun sprite-get-or-create-next ()
  "Return the next available sprite, creating one if none are free.
Signals `user-error' if `sprite-max-count' would be exceeded."
  (or (sprite-get-next)
      (let ((active-count (length (sprite-resolve-list))))
        (if (>= active-count sprite-max-count)
            (user-error "All %d sprite are busy and max-count (%d) reached"
                        active-count sprite-max-count)
          (sprite-create (format "worker-%d" active-count))))))

;;;; Mode-line and savehist wiring

(with-eval-after-load 'savehist
  (add-to-list 'savehist-additional-variables 'sprite--registry-saved)
  (add-hook 'savehist-save-hook #'sprite--registry-serialize)
  (add-hook 'savehist-mode-hook #'sprite--registry-deserialize))

(provide 'sprite)
;;; sprite.el ends here
