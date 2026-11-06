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

(eval-when-compile
  (require 'xtd-macro))

(defvar local-notes-directory nil
  "Directory where notes (org, roam, deft, etc.) are stored.")

(defvar user-org-directories nil
  "Additional directories that may contain org files.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; writing (english) configuration

(use-package denote
  :ensure t
  :defer t
  :commands (denote-dired denote-directories)
  :config

  ;; site configuration (paths)
  (when (default-boundp 'denote-directory)
    (setq denote-directory '()))
  (add-to-list 'denote-directory (file-name-concat (or local-notes-directory (expand-file-name "~/notes")) "denote"))
  (setq denote-journal-directory (file-name-concat (or local-notes-directory (expand-file-name "~/notes")) "denote" "journal"))
  (setq denote-explore-network-directory (file-name-concat (or local-notes-directory (expand-file-name "~/notes")) "explore"))

  ;; configuration/settings
  (setq denote-dash-default-filter '(not "journal"))
  (setq denote-review-insert-after "date")
  (setq denote-known-keywords '("project" "reference" "journal" "idea" "writing" "migration" "agent" "plan" "singing"))
  (setq denote-file-type 'org)
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-save-buffers t)
  (setq denote-prompts '(title keywords file-type))
  (setq denote-journal-title-format nil)
  (setq denote-sequence-scheme 'alphanumeric)
  (setq denote-explore-network-format "graphviz")
  (setq denote-explore-network-graphviz-filetype "svg")
  (add-to-list 'display-buffer-alist
               '("\\`\\*denote-sequence-hierarchy "
                 (display-buffer-reuse-window display-buffer-pop-up-window)))
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'denote--title-history)
    (add-to-list 'savehist-additional-variables 'denote--keywords-history))

  (setq denote-rename-buffer-function #'denote-dash-rename-buffer)

  (denote-rename-buffer-mode 1)

  (defun ad:denote-link-ol-get-heading--org-only (fn &rest args)
    "Only call FN in `org-mode' buffers.
`denote-link-ol-store' calls this unconditionally, even from
`markdown-mode' buffers, which makes `org-get-heading' warn via
`org-element-at-point' about running outside an Org buffer."
    (when (derived-mode-p 'org-mode)
      (apply fn args)))
  (advice-add 'denote-link-ol-get-heading :around
              #'ad:denote-link-ol-get-heading--org-only)

  (setq denote-link-description-format #'denote-dash-link-description-format))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ROBOTS (AI) Integration

(use-package gptel
  :ensure t
  :defer t
  :functions (gptel-make-anthropic gptel-make-gh-copilot gptel-make-gemini)
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
      (keymap-set hud-robot-gptel-set-default-model-map (downcase key) default-function-symbol)))

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

(provide 'setup-core)
;;; setup-core.el ends here
