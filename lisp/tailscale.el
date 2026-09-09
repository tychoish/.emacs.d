;;; tailscale.el --- Tailscale CLI integration and network control -*- lexical-binding: t; -*-

;; Author: Tychoish
;; Keywords: comm, hardware, network, tools
;; Version: 1.0.0

;;; Commentary:
;; Tailscale network status inspection, node IP copying, account switching,
;; file sending (Taildrop), and daemon connect/disconnect controls.

;;; Code:

(require 'cl-lib)
(require 'json)

(defgroup tailscale nil
  "Tailscale network management and inspection."
  :group 'network
  :group 'tools)

(defcustom tailscale-cli-executable "tailscale"
  "Path to the `tailscale' CLI executable."
  :type 'string
  :group 'tailscale)

(defun tailscale--exec (&rest args)
  "Execute `tailscale' CLI with ARGS synchronously and return output string."
  (unless (executable-find tailscale-cli-executable t)
    (user-error "Executable `%s' not found" tailscale-cli-executable))
  (with-output-to-string
    (with-current-buffer standard-output
      (apply #'call-process tailscale-cli-executable nil t nil args))))

(defun tailscale-status-json ()
  "Query `tailscale status --json' and return parsed alist."
  (let ((output (tailscale--exec "status" "--json")))
    (when (and output (not (string-empty-p output)))
      (ignore-errors (json-parse-string output :object-type 'alist :array-type 'list)))))

(defun tailscale-nodes ()
  "Return list of node alists for self and peers from `tailscale status --json'."
  (let* ((data (tailscale-status-json))
         (self (map-elt data 'Self))
         (peers (map-elt data 'Peer))
         (result nil))
    (when self
      (push self result))
    (when (hash-table-p peers)
      (maphash (lambda (_k peer) (push peer result)) peers))
    (nreverse result)))

(defun tailscale-status ()
  "Display interactive Tailscale network status buffer."
  (interactive)
  (let ((buf (get-buffer-create "*Tailscale Status*"))
        (data (tailscale-status-json)))
    (unless data
      (user-error "Failed to retrieve Tailscale status"))
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (let* ((self (map-elt data 'Self))
             (dns-suffix (map-elt data 'MagicDNSSuffix))
             (current-tailnet (map-elt (map-elt data 'CurrentTailnet) 'Name))
             (peers (map-elt data 'Peer)))
        (insert "= Tailscale Network Status =\n\n")
        (when current-tailnet
          (insert (format "Tailnet:      %s\n" current-tailnet)))
        (when dns-suffix
          (insert (format "MagicDNS:     %s\n" dns-suffix)))
        (when self
          (insert (format "Local Host:   %s (%s)\n"
                          (map-elt self 'HostName)
                          (string-join (map-elt self 'TailscaleIPs) ", "))))
        (insert "\n-- Peers --\n\n")
        (if (hash-table-p peers)
            (maphash
             (lambda (_k peer)
               (let ((name (map-elt peer 'HostName))
                     (ips (string-join (map-elt peer 'TailscaleIPs) ", "))
                     (online (map-elt peer 'Online))
                     (os (map-elt peer 'OS))
                     (dns (map-elt peer 'DNSName)))
                 (insert (format "%-20s  %-10s  %-8s  %s\n"
                                 name
                                 (if online "online" "offline")
                                 (or os "")
                                 ips))))
             peers)
          (insert "No peers found.\n")))
      (tailscale-mode))
    (display-buffer buf)))

(defun tailscale-connect ()
  "Connect Tailscale daemon (`tailscale up')."
  (interactive)
  (message "[tailscale] Connecting...")
  (let ((output (tailscale--exec "up")))
    (message "[tailscale] %s" (if (string-empty-p output) "Connected" output))))

(defun tailscale-disconnect ()
  "Disconnect Tailscale daemon (`tailscale down')."
  (interactive)
  (message "[tailscale] Disconnecting...")
  (let ((output (tailscale--exec "down")))
    (message "[tailscale] %s" (if (string-empty-p output) "Disconnected" output))))

(defun tailscale-copy-ip ()
  "Select a Tailscale node interactively and copy its IP to the kill ring."
  (interactive)
  (let* ((nodes (tailscale-nodes))
         (choices (mapcar (lambda (n)
                            (cons (format "%s (%s)" (map-elt n 'HostName)
                                          (car (map-elt n 'TailscaleIPs)))
                                  (car (map-elt n 'TailscaleIPs))))
                          nodes))
         (selected (completing-read "Node IP to copy: " choices nil t))
         (ip (cdr (assoc selected choices))))
    (when ip
      (kill-new ip)
      (message "[tailscale] Copied IP %s to kill ring" ip))))

(defun tailscale-file-send (file target-node)
  "Send FILE to TARGET-NODE via Taildrop (`tailscale file cp')."
  (interactive
   (list (read-file-name "File to send via Taildrop: ")
         (completing-read "Target Tailnet Node: "
                          (mapcar (lambda (n) (symbol-name (map-elt n 'HostName)))
                                  (seq-filter (lambda (n) (eq (map-elt n 'Online) t))
                                              (tailscale-nodes))))))
  (unless (file-exists-p file)
    (user-error "File `%s' does not exist" file))
  (message "[tailscale] Sending %s to %s..." (file-name-nondirectory file) target-node)
  (make-process
   :name "tailscale-file-send"
   :buffer "*tailscale-file-send*"
   :command (list tailscale-cli-executable "file" "cp" (expand-file-name file) (format "%s:" target-node))
   :sentinel (lambda (proc _event)
               (when (memq (process-status proc) '(exit signal))
                 (if (= (process-exit-status proc) 0)
                     (message "[tailscale] Sent %s to %s via Taildrop" (file-name-nondirectory file) target-node)
                   (message "[tailscale] Failed to send file via Taildrop"))))))

(defvar-keymap tailscale-mode-map
  :doc "Keymap for `tailscale-mode'."
  "g" #'tailscale-status
  "c" #'tailscale-connect
  "d" #'tailscale-disconnect
  "y" #'tailscale-copy-ip
  "f" #'tailscale-file-send)
(define-derived-mode tailscale-mode special-mode "Tailscale"
  "Major mode for inspecting Tailscale network status."
  (setq buffer-read-only t))

(provide 'tailscale)
;;; tailscale.el ends here
