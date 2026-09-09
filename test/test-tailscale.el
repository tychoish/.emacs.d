;;; test-tailscale.el --- Tests for tailscale integration -*- lexical-binding: t; no-byte-compile: t; -*-

;;; Commentary:
;; ERT unit tests for tailscale.el.

;;; Code:

(require 'ert)
(require 'cl-lib)

(load (expand-file-name "tailscale"
                        (expand-file-name "../lisp" (file-name-directory (or load-file-name buffer-file-name)))))

(defvar test-tailscale-mock-json
  "{\n  \"MagicDNSSuffix\": \"tuna-ionian.ts.net\",\n  \"CurrentTailnet\": {\"Name\": \"user@example.com\"},\n  \"Self\": {\"HostName\": \"laptop\", \"TailscaleIPs\": [\"100.93.226.33\"]},\n  \"Peer\": {\n    \"node1\": {\"HostName\": \"server1\", \"Online\": true, \"OS\": \"linux\", \"TailscaleIPs\": [\"100.80.39.36\"]},\n    \"node2\": {\"HostName\": \"phone\", \"Online\": false, \"OS\": \"android\", \"TailscaleIPs\": [\"100.106.219.33\"]}\n  }\n}"
  "Sample JSON status output for tests.")

(ert-deftest test-tailscale-status-json-parsing ()
  "Test parsing of `tailscale status --json' output."
  (cl-letf (((symbol-function 'tailscale--exec)
             (lambda (&rest args)
               (should (equal args '("status" "--json")))
               test-tailscale-mock-json)))
    (let ((data (tailscale-status-json)))
      (should (equal "tuna-ionian.ts.net" (map-elt data 'MagicDNSSuffix)))
      (should (equal "user@example.com" (map-elt (map-elt data 'CurrentTailnet) 'Name)))
      (should (equal "laptop" (map-elt (map-elt data 'Self) 'HostName))))))

(ert-deftest test-tailscale-nodes-extraction ()
  "Test extraction of self and peer node list."
  (cl-letf (((symbol-function 'tailscale--exec)
             (lambda (&rest _) test-tailscale-mock-json)))
    (let ((nodes (tailscale-nodes)))
      (should (= 3 (length nodes)))
      (should (equal "laptop" (map-elt (car nodes) 'HostName)))
      (should (equal "server1" (map-elt (nth 1 nodes) 'HostName)))
      (should (equal "phone" (map-elt (nth 2 nodes) 'HostName))))))

(ert-deftest test-tailscale-status-buffer-creation ()
  "Test `tailscale-status' populates status buffer."
  (cl-letf (((symbol-function 'tailscale--exec)
             (lambda (&rest _) test-tailscale-mock-json)))
    (tailscale-status)
    (let ((buf (get-buffer "*Tailscale Status*")))
      (should buf)
      (with-current-buffer buf
        (goto-char (point-min))
        (should (search-forward "Tailnet:      user@example.com" nil t))
        (should (search-forward "MagicDNS:     tuna-ionian.ts.net" nil t))
        (should (search-forward "server1" nil t))
        (should (eq major-mode 'tailscale-mode))))))

(ert-deftest test-tailscale-connect-and-disconnect ()
  "Test connect and disconnect trigger CLI commands."
  (let (executed-args)
    (cl-letf (((symbol-function 'tailscale--exec)
               (lambda (&rest args) (setq executed-args args) "")))
      (tailscale-connect)
      (should (equal '("up") executed-args))
      (tailscale-disconnect)
      (should (equal '("down") executed-args)))))

(ert-deftest test-tailscale-copy-ip ()
  "Test selecting node IP copies IP to kill ring."
  (cl-letf (((symbol-function 'tailscale--exec)
             (lambda (&rest _) test-tailscale-mock-json))
            ((symbol-function 'completing-read)
             (lambda (_prompt choices &rest _) (car (car choices)))))
    (tailscale-copy-ip)
    (should (equal "100.93.226.33" (current-kill 0)))))

(ert-deftest test-tailscale-exec-missing-binary ()
  "Signaling error when tailscale binary is missing."
  (let ((tailscale-cli-executable "nonexistent-tailscale-binary"))
    (should-error (tailscale--exec "status") :type 'user-error)))

(provide 'test-tailscale)
;;; test-tailscale.el ends here
