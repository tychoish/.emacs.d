;;; ollama-tailnet.el --- Ollama Gemma 4 Host and Gptel Tailnet Orchestration -*- lexical-binding: t; -*-

;; Author: Tychoish
;; Version: 1.0.0
;; Keywords: hypermedia, tools, ai, tailscale, systemd, gptel
;; Package-Requires: ((emacs "28.1") (gptel "0.9.0"))

;;; Commentary:
;; Systemd configuration and gptel backend orchestration for deploying
;; and querying Ollama LLM services hosting Gemma 4 and Mistral models
;; across a Tailscale tailnet.
;;
;; Quickstart:
;;
;;   (require 'ollama-tailnet)
;;
;;   ;; Register tailnet hosts with explicit models and default model alias
;;   (ollama-tailnet-register-host 'derrida
;;     :host "derrida.tailnet-name.ts.net:11434"
;;     :default-model "gemma4:27b"
;;     :models '("gemma4:27b" "gemma4:32b" "mistral:latest"))
;;
;;   (ollama-tailnet-register-host 'laptop
;;     :host "127.0.0.1:11434"
;;     :default-model "gemma4:2b"
;;     :models '("gemma4:2b" "gemma4:e4b" "mistral:7b"))
;;
;;   ;; Or use default laptop preset configuration
;;   (ollama-tailnet-setup-laptop-presets)
;;
;;   ;; Set gptel default backend to derrida's default model alias
;;   (ollama-tailnet-set-gptel-backend 'derrida/default)
;;
;; Interactive commands:
;;   `M-x ollama-tailnet-status'          - View status of all tailnet Ollama nodes
;;   `M-x ollama-tailnet-pull-model'      - Download/pull models to a target host
;;   `M-x ollama-tailnet-service-restart' - Restart systemd ollama.service on a host
;;   `M-x ollama-tailnet-service-status'  - Inspect systemd ollama.service status
;;   `M-x ollama-tailnet-set-gptel-backend' - Set gptel backend to a host-model spec

;;; Code:

(require 'cl-lib)
(require 'gptel)
(require 'ollama-tailnet-vars)
(require 'ollama-tailnet-gptel)
(require 'ollama-tailnet-control)

(defun ollama-tailnet-bind-hud-keys ()
  "Bind `ollama-tailnet' commands to `hud-robot-ollama-map' if available."
  (when (boundp 'hud-robot-ollama-map)
    (keymap-set hud-robot-ollama-map "s" #'ollama-tailnet-status)
    (keymap-set hud-robot-ollama-map "p" #'ollama-tailnet-pull-model)
    (keymap-set hud-robot-ollama-map "r" #'ollama-tailnet-service-restart)
    (keymap-set hud-robot-ollama-map "t" #'ollama-tailnet-service-status)
    (keymap-set hud-robot-ollama-map "b" #'ollama-tailnet-set-gptel-backend)))

;; Bind HUD keys at load time if map exists
(ollama-tailnet-bind-hud-keys)

(provide 'ollama-tailnet)
;;; ollama-tailnet.el ends here
