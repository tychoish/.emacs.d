;;; setup-core.el -- contains all major use-package forms -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "30.1"))

;;; Commentary:

;; Provides my collection of use-package forms and related
;; configuration for loading and configuring all Emacs packages.

;; This configuration optimizes for lazy-loading so that configuration
;; only loads when called directly or a mode is activated.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; INTERNAL

(declare-function setup-cmdr-register-commands "setup-cmdr")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ROBOTS (AI) Integration

(use-package gptel
  :ensure t
  :defer t
  :config
  (defvar gemini-api-key nil)
  (defvar anthropic-api-key nil)
  (defvar openai-api-key nil)

  (require 'cl-lib)

  (cl-defun make-gptel-set-up-backend-functions (&key name model backend key api-key)
    "Register buffer-local and session-default backend switchers for MODEL as NAME."
    (declare (indent defun))
    (let ((local-function-symbol (intern (format "gptel-set-backend-%s" name)))
          (default-function-symbol (intern (format "gptel-set-backend-default-%s" name))))
      (defalias local-function-symbol
        (lambda ()
          (interactive)
          (setq-local gptel-model model)
          (when api-key
            (setq-local gptel-api-key (if (functionp api-key) api-key (lambda () api-key))))
          (setq-local gptel-backend backend)
          (message "[gptel] set backend to %s for the local buffer" name))
        (format "Set LLM backend for the current buffer to `%s'" model))

      (defalias default-function-symbol
        (lambda ()
          (interactive)
          (setq-default gptel-model model)
          (when api-key
            (setq-default gptel-api-key (if (functionp api-key) api-key (lambda () api-key))))
          (setq-default gptel-backend backend)
          (message "[gptel] set default backend to %s" name))
        (format "Set the default LLM backend for the current session to `%s'" model))

      (keymap-set gptel-mode-map (format "C-c r a m %s" (upcase key)) default-function-symbol)
      (keymap-set gptel-mode-map (format "C-c r a m %s" (downcase key)) local-function-symbol)
      (keymap-set hud-robot-gptel-set-default-model-map (downcase key) default-function-symbol)
      (when (boundp 'cmdr-mode-map)
        (keymap-set cmdr-mode-map (format "C-c r g m %s" (downcase key)) default-function-symbol))))

  (keymap-set gptel-mode-map "C-c m" #'gptel-menu)

  (setq gptel-include-reasoning 'ignore)

  (make-gptel-set-up-backend-functions
   :name "sonnet-5"
   :key "s"
   :model 'claude-sonnet-5
   :backend (gptel-make-anthropic "claude-sonnet" :key anthropic-api-key :stream t)
   :api-key anthropic-api-key)

  (make-gptel-set-up-backend-functions
   :name "gemini-flash"
   :key "f"
   :model 'gemini-flash-latest
   :backend (gptel-make-gemini "gemini" :key gemini-api-key :stream t))

  (make-gptel-set-up-backend-functions
   :name "copilot"
   :key "c"
   :model 'gpt-5-mini
   :backend (gptel-make-gh-copilot "copilot"))

  (make-gptel-set-up-backend-functions
   :name "gpt-5.6-terra"
   :key "m"
   :backend (gptel-make-openai "openai" :key openai-api-key)
   :model 'gpt-5.6-terra
   :api-key openai-api-key)

  (gptel-set-backend-default-gemini-flash)
  (require 'gptel-integrations)
  (gptel-mcp-mode 1))

(use-package cmdr
  :config
  (require 'setup-cmdr)
  (setup-cmdr-register-commands)
  :commands (cmdr-menu cmdr-dispatch))

(provide 'setup-core)
;;; setup-core.el ends here
