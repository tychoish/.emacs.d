;;; ollama-tailnet-vars.el --- Variables and host registry for ollama-tailnet -*- lexical-binding: t; -*-

;; Author: Tychoish
;; Keywords: hypermedia, tools, ai

;;; Commentary:
;; Variables, customization groups, preset profiles, and host registry structures for
;; Ollama tailnet LLM orchestration and gptel integration.

;;; Code:

(require 'cl-lib)

(defgroup ollama-tailnet nil
  "Ollama tailnet LLM orchestration and gptel integration."
  :group 'tools
  :group 'convenience)

(defcustom ollama-tailnet-fallback-strategy :prompt
  "Fallback strategy when a target tailnet node is unreachable.
Options:
`:prompt'     - Prompt user interactively via minibuffer to pick a reachable host.
`:auto-local' - Fall back automatically to local laptop Ollama instance.
`:error'      - Raise an immediate error."
  :type '(choice (const :tag "Prompt user" :prompt)
                 (const :tag "Auto fallback to local" :auto-local)
                 (const :tag "Raise error" :error))
  :group 'ollama-tailnet)

(defcustom ollama-tailnet-preset-profiles
  '((:derrida-server
     :name derrida
     :host "derrida.tailnet-name.ts.net:11434"
     :default-model "gemma4:27b"
     :models ("gemma4:27b" "gemma4:32b" "mistral:latest")
     :local-p nil)
    (:laptop-offline
     :name laptop
     :host "127.0.0.1:11434"
     :default-model "gemma4:2b"
     :models ("gemma4:2b" "gemma4:e4b" "mistral:7b")
     :local-p t))
  "Preset profiles for tailnet server and laptop endpoints."
  :type '(alist :key-type symbol :value-type plist)
  :group 'ollama-tailnet)

(cl-defstruct (ollama-tailnet-host (:constructor ollama-tailnet-host-create))
  "Structure representing a tailnet Ollama host node."
  (name nil :type symbol)
  (address "" :type string)
  (default-model "" :type string)
  (models nil :type list)
  (local-p nil :type boolean))

(defvar ollama-tailnet-hosts (make-hash-table :test 'eq)
  "Hash table mapping host name symbols to `ollama-tailnet-host' structures.")

(defun ollama-tailnet-register-host (name &rest props)
  "Register a tailnet node NAME (symbol or string) with PROPS plist.

Recognized PROPS:
`:host' or `:address' - Host string (e.g., \"derrida.tailnet-name.ts.net:11434\" or \"127.0.0.1:11434\")
`:default-model'     - String default model name (e.g., \"gemma4:27b\")
`:models'            - List of model strings (e.g., '(\"gemma4:27b\" \"gemma4:32b\"))
`:local-p'           - Boolean indicating if host is local (default: auto-detected)"
  (let* ((sym (if (symbolp name) name (intern (format "%s" name))))
         (addr (or (plist-get props :host) (plist-get props :address) "127.0.0.1:11434"))
         (def-mod (or (plist-get props :default-model) "gemma4:2b"))
         (raw-mods (or (plist-get props :models) (list def-mod)))
         (mods (delete-dups (cons def-mod raw-mods)))
         (local-p (if (plist-member props :local-p)
                      (plist-get props :local-p)
                    (or (string-prefix-p "127.0.0.1" addr)
                        (string-prefix-p "localhost" addr))))
         (host-struct (ollama-tailnet-host-create
                       :name sym
                       :address addr
                       :default-model def-mod
                       :models mods
                       :local-p local-p)))
    (puthash sym host-struct ollama-tailnet-hosts)
    host-struct))

(defun ollama-tailnet-get-host (name)
  "Retrieve registered `ollama-tailnet-host' structure by NAME (symbol or string)."
  (when name
    (let ((sym (if (symbolp name) name (intern (format "%s" name)))))
      (gethash sym ollama-tailnet-hosts))))

(defun ollama-tailnet-list-hosts ()
  "Return a list of all registered `ollama-tailnet-host' structures."
  (let (hosts)
    (maphash (lambda (_v host) (push host hosts)) ollama-tailnet-hosts)
    (nreverse hosts)))

(defun ollama-tailnet-clear-hosts ()
  "Clear all registered host structures."
  (clrhash ollama-tailnet-hosts))

(defun ollama-tailnet-setup-laptop-presets ()
  "Register standard laptop and server profiles from `ollama-tailnet-preset-profiles'."
  (interactive)
  (dolist (item ollama-tailnet-preset-profiles)
    (let* ((props (cdr item))
           (name (plist-get props :name)))
      (apply #'ollama-tailnet-register-host name props)))
  (message "[ollama-tailnet] Laptop and server presets initialized"))

(defun ollama-tailnet-host-reachable-p (host-name-or-struct)
  "Check if HOST-NAME-OR-STRUCT is reachable on its network port within 1s timeout."
  (let* ((host-struct (if (ollama-tailnet-host-p host-name-or-struct)
                          host-name-or-struct
                        (ollama-tailnet-get-host host-name-or-struct)))
         (addr (if host-struct (ollama-tailnet-host-address host-struct)
                 (format "%s" host-name-or-struct)))
         (parts (split-string addr ":"))
         (host (car parts))
         (port (if (cdr parts) (string-to-number (cadr parts)) 11434)))
    (if (or (string-equal host "127.0.0.1") (string-equal host "localhost"))
        t
      (condition-case nil
          (let ((proc (make-network-process
                       :name "ollama-reachability-probe"
                       :host host
                       :service port
                       :nowait nil
                       :timeout 1)))
            (when (processp proc)
              (delete-process proc)
              t))
        (error nil)))))

(provide 'ollama-tailnet-vars)
;;; ollama-tailnet-vars.el ends here
