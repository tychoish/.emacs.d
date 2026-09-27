;;; hud-mode.el --- global minor mode holding tycho's custom keybindings -*- lexical-binding: t; -*-

;; Author: tychoish
;; Maintainer: tychoish
;; Version: 1.0-pre
;; Package-Requires: ((emacs "29.1"))
;; Keywords: internal maint emacs convenience

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; A single global minor mode (`hud-mode') that owns every custom prefix
;; keymap and general-purpose editing/window/buffer command that used to
;; live directly in `global-map' via bootstrap.el.  Enabling `hud-mode'
;; (done once, from bootstrap's late-enable-modes) makes `hud-mode-map'
;; active everywhere; disabling it removes all of these bindings without
;; touching `global-map' itself.
;;
;; This file intentionally does not depend on bootstrap.el -- keymaps here
;; may reference commands still defined in bootstrap.el by symbol (that's
;; fine, keymaps just store symbols), but nothing here calls a bootstrap.el
;; function at load time.

;;; Code:

(require 'seq)
(eval-when-compile
  (require 'cl-lib))

(declare-function which-key-add-keymap-based-replacements "which-key")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration & scheduling macros (relocated from xtd-macro.el)

(defvar hud-slow-op-reporting debug-on-error
  "When non-nil, report operation execution durations exceeding `hud-slow-op-threshold'.")

(defvar hud-slow-op-threshold 0.01
  "Threshold in seconds above which operations are timed and logged.")

(defmacro hud-with-slow-op-timer (name &rest body)
  "Send a message if the BODY operation of NAME takes longer than `hud-slow-op-threshold'."
  (declare (indent defun) (debug t))
  `(if (not (or (and (boundp 'slow-op-reporting) slow-op-reporting)
                hud-slow-op-reporting))
       (progn ,@body)
     (let* ((time (current-time))
	    (return-value (let ((inhibit-message t)) ,@body))
	    (duration (time-to-seconds (time-since time)))
            (thresh (if (boundp 'slow-op-threshold) slow-op-threshold hud-slow-op-threshold)))
       (when (> duration thresh)
	 (let ((inhibit-message t))
	   (message "[op]: %s: %.06fs" ,name duration)))
       return-value)))

(defun hud--resolve-hooks (hook)
  "Resolve HOOK to a list of hook variable symbols.
HOOK may be a symbol, a list of symbols, or the sentinel
`after-first-frame-created'."
  (let ((hook (if (eq hook 'after-first-frame-created)
		  (if (daemonp)
		      'server-after-make-frame-hook
		    'window-setup-hook)
		hook)))
    (cond ((symbolp hook)
	   (list hook))
	  ((and (listp hook) (seq-every-p #'symbolp hook))
	   hook)
	  (:else
	   (user-error "must have a symbol, list of symbols, or `after-first-frame-created' for hook: %S" hook)))))

(defun hud--build-symbol-name (&rest parts)
  "Join PARTS into a single hyphen-separated string suitable for a symbol name."
  (replace-regexp-in-string
   "-\\{3,\\}" "-"
   (mapconcat #'identity
	      (thread-last
		parts
		(seq-filter #'stringp)
		(seq-map (lambda (elem) (replace-regexp-in-string "[ \t\n\r]+" " " elem)))
		(seq-map #'string-trim)
		(seq-map (lambda (elem) (replace-regexp-in-string "[=+_'\"\\/ ]+" "-" elem)))
		(seq-remove #'string-empty-p))
	      "-")))

(cl-defmacro add-lazy-init (&key name operation (delay 1))
  "Execute OPERATION in an idle timer DELAY seconds after Emacs becomes idle."
  (unless (and name operation)
    (user-error "add-lazy-init requires :name and :operation"))
  `(run-with-idle-timer
    ,delay nil
    (lambda ()
      (hud-with-slow-op-timer ,name
	(funcall ,operation)))))

(cl-defmacro add-one-shot-hook
    (&key name hook function result body form operation
	  ;; flags and options; with defaults
	  (args nil) (local nil) (persist nil) (count 1) (depth 0) (make-unique nil) (cleanup nil) (idle-timer nil))
  "Register a self-removing hook function named NAME on HOOK."
  (unless hook
    (user-error "add-one-shot-hook requires :hook"))
  (let* ((unique-tag (or (when make-unique (gensym "hook-"))
			 (make-symbol "hook")))
	 (count-tag (cond (persist "perpetual")
			  ((not (numberp count)) (user-error "must specify hook limited count as a number %d" count))
			  ((eq count 1) "one-shot")
			  (:else (format "run-%d-times" count))))
	 (cleanup-symbol (intern (hud--build-symbol-name "one-shot" count-tag name (symbol-name unique-tag))))
	 (hook-form (if (symbolp hook) `',hook hook))
	 (hook-display (cond ((and (consp hook) (eq (car hook) 'quote))
			      (cadr hook))
			     ((or (symbolp hook) (seq-every-p #'symbolp hook))
			      hook)
			     (:else
			      "a computed hook expression")))
	 (timer-name (format "<one-shot-hook> %s" name))
	 (call-args (seq-remove (lambda (arg) (memq arg '(&optional &rest))) args))
	 (resolved-form
	  (or (cond (form
		     form)
		    (body
		     `,@body)
		    (result
		     `,(eval result))
		    (operation
		     (if args
			 `(funcall ,operation ,@call-args)
		       `(funcall ,operation)))
		    ((symbolp function)
		     (if args
			 `(funcall ',function ,@call-args)
		       `(funcall ',function)))
		    ((listp function)
		     function))
	      (user-error "could not resolve the hook function from input for %s" name)))
	 (cleanup-expr
	  (if (or make-unique cleanup)
	      `(unintern ',cleanup-symbol obarray)
	    t))
	 (run-count-var (intern (concat (symbol-name cleanup-symbol) "--run-count"))))

    `(progn
       (defvar ,run-count-var 0
	 ,(format "Number of times the one-shot hook function `%s' has fired." cleanup-symbol))

       (defun ,cleanup-symbol ,args
	 ,(format "Self-removing hook function for `%s', registered on %S.
Generated by `add-one-shot-hook'; removes itself from the hook after
%s." name hook-display (if (eq count 1) "one run" (format "%d runs" count)))
	 ,@(if idle-timer
	       `((run-with-idle-timer ,idle-timer nil
				      (lambda ()
					(hud-with-slow-op-timer ,timer-name ,resolved-form)))
		 (cl-incf ,run-count-var)
		 (when (and (not ,persist) (>= ,run-count-var ,count))
		   (seq-do (lambda (h) (remove-hook h ',cleanup-symbol ,local))
			   (hud--resolve-hooks ,hook-form))
		   ,cleanup-expr))
	     `((hud-with-slow-op-timer ,timer-name
		 ,resolved-form
		 (cl-incf ,run-count-var)
		 (when (and (not ,persist) (>= ,run-count-var ,count))
		   (seq-do (lambda (h) (remove-hook h ',cleanup-symbol ,local))
			   (hud--resolve-hooks ,hook-form))
		   ,cleanup-expr)))))

       (seq-do (lambda (h) (add-hook h ',cleanup-symbol ,depth ,local))
	       (hud--resolve-hooks ,hook-form)))))

(cl-defmacro create-toggle-functions (value &optional &key short-name local keymap key)
  "Define turn-on, turn-off, and toggle interactive commands for variable VALUE.
Use SHORT-NAME to override the generated name. LOCAL makes commands use `setq-local'.
Optionally bind the toggle to KEY in KEYMAP."
  (let* ((name (or short-name (symbol-name value)))
	 (suffix (when local "local"))
	 (ops (list
	       `(,(intern (hud--build-symbol-name "turn-on" name suffix)) t)
	       `(,(intern (hud--build-symbol-name "turn-off" name suffix)) nil)
	       `(,(intern (hud--build-symbol-name "toggle" name suffix)) (not ,value))))
	 (setter (if local 'setq-local 'setq)))

    (when (and keymap (not key))
      (user-error "must define both keymap and a key"))

    `(progn
       ,@(seq-map (lambda (op) `(defun ,(car op) ()
				   (interactive)
				   (,setter ,value ,(cadr op))))
		  ops)
       ,(when keymap
	  `(keymap-set ,keymap ,key #',(car (nth 2 ops)))))))

(cl-defmacro make-read-extended-command-for-prefix (prefix &optional &key bind-map bind-key key-alias)
  "Define an interactive command that runs `execute-extended-command' filtered to PREFIX.
Only commands whose names begin with PREFIX are offered for completion.
Optionally bind the command to BIND-KEY in BIND-MAP with KEY-ALIAS as the which-key label."
  (declare (indent defun))
  (unless (setq prefix (when-let* ((_ prefix)
				   (trimmed (string-trim prefix))
				   (_ (not (string-empty-p trimmed))))
			    trimmed))
    (user-error "cannot build predicate function for '%s'" prefix))

  (let* ((predicate-name (format "read-extended-command-for-%s-prefix-p" prefix))
	 (predicate-symbol (intern predicate-name))
	 (user-command-name (format "execute-extended-%s-command" prefix))
	 (user-command-symbol (intern user-command-name)))
    `(prog1
	 (defun ,user-command-symbol ()
	   ,(format "Read extentend command but filtered for only those beginning with prefix `%s'." prefix)
	   (interactive)
	   (let ((read-extended-command-predicate #',predicate-symbol))
	     (with-suppressed-warnings ((interactive-only execute-extended-command))
	       (execute-extended-command nil))))

       (defun ,predicate-symbol (command _)
	 ,(format "Predicate for `read-extended-command-predicate' to filter commands returning only those that start with the prefix `%s'" prefix)
	 (string-prefix-p ,prefix (symbol-name command)))
       ,(when bind-key
	  `(progn
	     (keymap-set ,(or bind-map 'global-map) ,bind-key #',user-command-symbol)
	     (with-eval-after-load 'which-key
	       (which-key-add-keymap-based-replacements ,(or bind-map 'global-map) ,bind-key ,(or key-alias (format "%s-commands" prefix)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; keymap definitions -- top level C-c <> maps

(defvar-keymap hud-core-map
  :name "core"
  :doc "Core commands under C-c t (hud-mode).")

(keymap-set hud-core-map "k" #'execute-extended-clipboard-command)
(keymap-set hud-core-map "p" #'toggle-electric-pair-inhibition)
(keymap-set hud-core-map "e" #'toggle-electric-pair-eagerness)
(keymap-set hud-core-map "j" #'journalctl)
(keymap-set hud-core-map "m" #'hud-dispatch)
(keymap-set hud-core-map "," #'hud-select)

(defvar-keymap hud-display-map
  :name "display"
  :doc "Display commands under C-c f (hud-mode).")

(keymap-set hud-display-map "=" #'text-scale-increase)
(keymap-set hud-display-map "-" #'text-scale-decrease)
(keymap-set hud-display-map "0" #'text-scale-reset)
(keymap-set hud-display-map "h" #'auto-fill-mode)
(keymap-set hud-display-map "s" #'visual-line-mode)

(defvar-keymap hud-kill-map
  :name "kill"
  :doc "Kill/delete commands under C-c k (hud-mode).")

(keymap-set hud-kill-map "s" #'backward-kill-sentence)
(keymap-set hud-kill-map "p" #'backward-kill-paragraph)
(keymap-set hud-kill-map "f" #'backward-kill-sexp)
(keymap-set hud-kill-map "d" #'delete-region)
(keymap-set hud-kill-map "w" #'delete-trailing-whitespace)

(defvar-keymap hud-web-browser-map
  :name "web-browser"
  :doc "Web browser commands under C-c w (hud-mode).")

(keymap-set hud-web-browser-map "d" #'browse-url-generic)
(keymap-set hud-web-browser-map "e" #'browse-url)
(keymap-set hud-web-browser-map "f" #'browse-url-firefox)
(keymap-set hud-web-browser-map "c" #'browse-url-chrome)
(keymap-set hud-web-browser-map "g" #'eww-search-words)

(defvar-keymap hud-docs-map
  :name "docs"
  :doc "Documentation commands under C-c h (hud-mode).")

(keymap-set hud-docs-map "s" #'hud-describe-symbol-dwim)
(keymap-set hud-docs-map "v" #'describe-variable)
(keymap-set hud-docs-map "q" #'kill-eldoc-and-help-buffers)
(keymap-set hud-docs-map "j" #'jump-to-elisp-help)
(keymap-set hud-docs-map "e" #'eldoc)
(keymap-set hud-docs-map "b" #'eldoc-doc-buffer)

(defvar-keymap hud-ecclectic-grep-map
  :name "grep"
  :doc "Grep commands under C-c g (hud-mode).")

(keymap-set hud-ecclectic-grep-map "o" #'occur)
(keymap-set hud-ecclectic-grep-map "g" #'grep)

(defvar-keymap hud-ide-map
  :name "ide"
  :doc "IDE/language commands under C-c l (hud-mode).")

(keymap-set hud-ide-map "m" #'imenu)
(keymap-set hud-ide-map "c" #'xref-find-references)
(keymap-set hud-ide-map "d" #'xref-find-definitions)
(keymap-set hud-ide-map "p" #'xref-go-back)
(keymap-set hud-ide-map "n" #'xref-go-forward)
(keymap-set hud-ide-map "o" #'xref-find-definitions-other-window)

(defvar-keymap hud-completion-map
  :name "completion"
  :doc "Completion commands under C-c . (hud-mode).")

(keymap-set hud-completion-map "TAB" #'completion-at-point)
(keymap-set hud-completion-map "." #'completion-at-point)
(keymap-set hud-completion-map "p" #'completion-at-point)
(keymap-set hud-completion-map "/" #'dabbrev-expand)
(keymap-set hud-completion-map "c" #'dabbrev-completion)
(keymap-set hud-completion-map "i" #'tempel-insert)
(keymap-set hud-completion-map "s" #'tempel-complete)
(keymap-set hud-completion-map "x" #'tempel-expand)
(keymap-set hud-completion-map "v" #'tempel-open-custom-file)

(defvar-keymap hud-shell-map
  :name "shell"
  :doc "Shell commands under C-c s (hud-mode).")

(keymap-set hud-shell-map "m" #'eshell)

(defvar-keymap hud-denote-map
  :name "denote"
  :doc "Denote commands under C-c d (hud-mode).")

(defvar-keymap hud-robot-map
  :name "robot"
  :doc "Robot/agent commands under C-c r (hud-mode).")

(defvar-keymap hud-anzu-map
  :name "anzu"
  :doc "Anzu commands under C-c q (hud-mode).")

(keymap-set hud-anzu-map "r" #'anzu-query-replace)
(keymap-set hud-anzu-map "e" #'anzu-query-replace-regexp)

(defvar-keymap hud-mail-map
  :name "mail"
  :doc "Mail commands under C-c m (hud-mode).")

(keymap-set hud-mail-map "m" #'mu4e)
(keymap-set hud-mail-map "d" #'mu4e-search-maildir)
(keymap-set hud-mail-map "b" #'mu4e-search-bookmark)
(keymap-set hud-mail-map "c" #'mu4e-compose-new)

(defvar-keymap hud-magit-map
  :name "magit"
  :doc "Magit commands under C-x g (hud-mode).")

(keymap-set hud-magit-map "s" #'magit-status)
(keymap-set hud-magit-map "f" #'magit-branch)
(keymap-set hud-magit-map "b" #'magit-blame)

(make-read-extended-command-for-prefix "magit"
  :bind-map hud-magit-map
  :bind-key "x")

(defvar-keymap hud-docker-map
  :name "docker"
  :doc "Docker commands under C-x d (hud-mode).")

(keymap-set hud-docker-map "d" #'docker)
(keymap-set hud-docker-map "c" #'docker-containers)
(keymap-set hud-docker-map "i" #'docker-images)
(keymap-set hud-docker-map "v" #'docker-volumes)
(keymap-set hud-docker-map "m" #'docker-contexts)
(keymap-set hud-docker-map "p" #'docker-compose)

(defvar-keymap orgx-global-map
  :name "org"
  :doc "Org commands under C-c o (hud-mode).")

(keymap-set orgx-global-map "4" #'org-agenda)
(keymap-set orgx-global-map "k" #'org-capture)
(keymap-set orgx-global-map "s" #'org-save-all-org-buffers)

;; nested keymaps
(defvar-keymap orgx-link-map
  :name "org-link"
  :doc "Org link commands under C-c o l (hud-mode).")

(keymap-set orgx-link-map "s" #'org-store-link)
(keymap-set orgx-link-map "a" #'org-id-store-link)
(keymap-set orgx-link-map "i" #'org-insert-link)
(keymap-set orgx-link-map "n" #'org-annotate-file)

(defvar-keymap hud-display-opacity-map
  :name "opacity"
  :doc "Opacity commands under C-c f o (hud-mode).")

(defvar-keymap hud-buffer-control-map
  :name "buffer-control"
  :doc "Buffer control commands under C-x C-b (hud-mode).")

(keymap-set hud-buffer-control-map "d" #'kill-buffers-in-directory)
(keymap-set hud-buffer-control-map "<SPC>" #'revert-buffer-quick)
(keymap-set hud-buffer-control-map "m" #'kill-buffers-matching-mode)
(keymap-set hud-buffer-control-map "h" #'bury-buffer)
(keymap-set hud-buffer-control-map "r" #'revbufs)
(keymap-set hud-buffer-control-map "b" #'switch-to-buffer)
(keymap-set hud-buffer-control-map "n" #'switch-to-buffer-other-window)

(defvar-keymap hud-blogging-map
  :name "blogging"
  :doc "Blogging commands under C-c t b (hud-mode).")

(defvar-keymap hud-theme-map
  :name "theme"
  :doc "Theme commands under C-c t t (hud-mode).")

(keymap-set hud-theme-map "r" #'disable-all-themes)
(keymap-set hud-theme-map "d" #'bootstrap-load-dark-theme)
(keymap-set hud-theme-map "l" #'bootstrap-load-light-theme)

(defvar-keymap hud-whitespace-map
  :name "whitespace"
  :doc "Whitespace commands under C-c t w (hud-mode).")

(defvar-keymap hud-ecclectic-grep-project-map
  :name "project-grep"
  :doc "Project grep commands under C-c g p (hud-mode).")

(keymap-set hud-ecclectic-grep-project-map "f" #'find-grep)

(defvar-keymap hud-ecclectic-rg-map
  :name "+ripgrep"
  :doc "Ripgrep commands under C-c g r (hud-mode).")

(defvar-keymap hud-consult-search-map
  :name "consult-search"
  :doc "Consult search commands under C-c g s (hud-mode).")

(defvar-keymap hud-eglot-global-map
  :name "eglot"
  :doc "Eglot commands under C-c l l (hud-mode).")

(defvar-keymap hud-robot-agent-shell-map
  :name "agent-shell"
  :doc "Agent shell commands under C-c r s (hud-mode).")

(defvar-keymap hud-shell-eat-map
  :name "shell-eat"
  :doc "Shell eat commands under C-c s e (hud-mode).")

(keymap-set hud-shell-eat-map "e" #'eat)
(keymap-set hud-shell-eat-map "o" #'eat-other-window)
(keymap-set hud-shell-eat-map "p" #'eat-project)
(keymap-set hud-shell-eat-map "P" #'eat-project-other-window)

(make-read-extended-command-for-prefix "eat"
  :bind-map hud-shell-eat-map
  :bind-key "m")

(defvar-keymap hud-denote-sequence-map
  :name "denote-sequence"
  :doc "Denote sequence commands under C-c d s (hud-mode).")

(defvar-keymap hud-denote-org-map
  :name "denote-org"
  :doc "Denote org commands under C-c d o (hud-mode).")

(keymap-set hud-denote-org-map "l" #'denote-org-link-to-heading)
(keymap-set hud-denote-org-map "b" #'denote-org-backlinks-for-heading)
(keymap-set hud-denote-org-map "x" #'denote-org-extract-org-subtree)
(keymap-set hud-denote-org-map "d" #'denote-org-dblock-insert-links)
(keymap-set hud-denote-org-map "p" #'denote-org-dblock-insert-backlinks)
(keymap-set hud-denote-org-map "f" #'denote-org-dblock-insert-files)

(defvar-keymap hud-denote-explore-map
  :name "denote-explore"
  :doc "Denote explore commands under C-c d e (hud-mode).")

(defvar-keymap hud-denote-review-map
  :name "denote-review"
  :doc "Denote review commands under C-c d c (hud-mode).")

(defvar-keymap hud-denote-hierarchy-map
  :name "denote-hierarchy"
  :doc "Denote sequence hierarchy view commands under C-c d h (hud-mode).")

(defvar-keymap hud-consult-mode-map
  :name "consult"
  :doc "Consult commands under C-c C-; (hud-mode).")

(keymap-set hud-consult-mode-map "s" #'tempel-insert)

(defvar-keymap hud-smerge-map
  :name "smerge"
  :doc "Smerge commands under C-x g m (hud-mode).")

(defvar-keymap hud-robot-gptel-map
  :name "gptel"
  :doc "GPtel commands under C-c r g (hud-mode).")

(keymap-set hud-robot-gptel-map "g" #'gptel)
(keymap-set hud-robot-gptel-map "r" #'gptel-rewrite)
(keymap-set hud-robot-gptel-map "m" #'gptel-menu)
(keymap-set hud-robot-gptel-map "a" #'gptel-agent)
(keymap-set hud-robot-gptel-map "w" #'gptel-aibo-summon)

(make-read-extended-command-for-prefix "gptel"
  :bind-map hud-robot-gptel-map
  :bind-key "x")

(make-read-extended-command-for-prefix "gptel-set-backend"
  :bind-map hud-robot-gptel-map
  :bind-key "b")


(defvar-keymap hud-robot-gptel-set-default-model-map
  :name "gptel-set-default-model"
  :doc "GPtel model selection under C-c r g m (hud-mode).")

(defvar-keymap hud-robot-ollama-map
  :name "ollama"
  :doc "Ollama commands under C-c r o (hud-mode).")

(defvar-keymap hud-robot-ollama-tailnet-map
  :name "ollama-tailnet"
  :doc "Ollama tailnet orchestration under C-c r o t (hud-mode).")

(keymap-set hud-robot-ollama-tailnet-map "s" #'ollama-tailnet-status)
(keymap-set hud-robot-ollama-tailnet-map "p" #'ollama-tailnet-pull-model)
(keymap-set hud-robot-ollama-tailnet-map "r" #'ollama-tailnet-service-restart)
(keymap-set hud-robot-ollama-tailnet-map "t" #'ollama-tailnet-service-status)
(keymap-set hud-robot-ollama-tailnet-map "b" #'ollama-tailnet-set-gptel-backend)
(keymap-set hud-robot-ollama-tailnet-map "l" #'ollama-tailnet-models)
(keymap-set hud-robot-ollama-tailnet-map "S" #'ollama-tailnet-search-model)

(defvar-keymap hud-robot-network-map
  :name "network"
  :doc "Tailscale network control under C-c r n (hud-mode).")

;; the mode's own container map -- populated below
(defvar-keymap hud-mode-map
  :name "hud"
  :doc "Global keybindings for hud-mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; hud-mode-map wiring -- everything attaches here instead of global-map

;; Baseline extended-command prefixes
(make-read-extended-command-for-prefix "clipboard"
  :bind-map hud-mode-map
  :bind-key "C-x x c")

(make-read-extended-command-for-prefix "tailscale"
  :bind-map hud-robot-network-map
  :bind-key "x")

(make-read-extended-command-for-prefix "smerge"
  :bind-map hud-smerge-map
  :bind-key "x")

(make-read-extended-command-for-prefix "docker"
  :bind-key "x"
  :bind-map hud-docker-map)

(create-toggle-functions
 hud-modeline-icons
 :keymap hud-theme-map
 :key "i")

(create-toggle-functions
 hud-modeline-show-buffer-size
 :keymap hud-theme-map
 :key "s")

(create-toggle-functions slow-op-reporting)
(create-toggle-functions electric-pair-inhibition)
(create-toggle-functions electric-pair-eagerness)


(keymap-set minibuffer-local-map "C-g" #'hud-super-abort-minibuffers)
(keymap-set minibuffer-local-map "C-l" #'backward-kill-word)
(keymap-set minibuffer-local-map "C-c a" #'marginalia-cycle)

(keymap-set orgx-global-map "l" (cons "org-link" orgx-link-map))

(keymap-set hud-blogging-map "m" #'hud-insert-date)
(keymap-set hud-display-map "o" (cons "opacity" hud-display-opacity-map))
(keymap-set hud-buffer-control-map "k" #'kill-this-buffer)
(keymap-set hud-display-opacity-map "=" #'hud-opacity-increase)
(keymap-set hud-display-opacity-map "-" #'hud-opacity-decrease)
(keymap-set hud-display-opacity-map "0" #'hud-opacity-reset)
(keymap-set hud-docs-map "h" #'help)
(keymap-set hud-docs-map "a" #'mark-whole-buffer)

(keymap-set hud-core-map "b" (cons "blogging" hud-blogging-map))
(keymap-set hud-core-map "t" (cons "theme" hud-theme-map))
(keymap-set hud-core-map "w" (cons "whitespace" hud-whitespace-map))

(keymap-set hud-ecclectic-grep-map "p" (cons "project-grep" hud-ecclectic-grep-project-map))
(keymap-set hud-ecclectic-grep-map "r" (cons "+ripgrep" hud-ecclectic-rg-map))
(keymap-set hud-ecclectic-grep-map "s" (cons "consult-search" hud-consult-search-map))

(keymap-set hud-magit-map "m" (cons "smerge" hud-smerge-map))
(keymap-set hud-ide-map "l" (cons "eglot" hud-eglot-global-map))
(keymap-set hud-shell-map "e" (cons "shell-eat" hud-shell-eat-map))
(keymap-set hud-robot-map "s" (cons "agent-shell" hud-robot-agent-shell-map))
(keymap-set hud-robot-map "g" (cons "gptel" hud-robot-gptel-map))
(keymap-set hud-robot-gptel-map "m" (cons "gptel-set-default-model" hud-robot-gptel-set-default-model-map))
(keymap-set hud-robot-map "o" (cons "ollama" hud-robot-ollama-map))
(keymap-set hud-robot-ollama-map "t" (cons "tailnet" hud-robot-ollama-tailnet-map))
(keymap-set hud-robot-map "p" (cons "workflows" #'agent-shell-workflow-select))
(keymap-set hud-robot-map "m" (cons "workflow-menu" #'agent-shell-workflow-dispatch-menu))
(keymap-set hud-robot-map "n" (cons "network" hud-robot-network-map))
(keymap-set hud-robot-network-map "s" #'tailscale-status)
(keymap-set hud-robot-network-map "c" #'tailscale-connect)
(keymap-set hud-robot-network-map "d" #'tailscale-disconnect)
(keymap-set hud-robot-network-map "y" #'tailscale-copy-ip)
(keymap-set hud-robot-network-map "f" #'tailscale-file-send)

(keymap-set hud-denote-map "o" (cons "denote-org" hud-denote-org-map))
(keymap-set hud-denote-map "s" (cons "denote-sequence" hud-denote-sequence-map))
(keymap-set hud-denote-map "e" (cons "denote-explore" hud-denote-explore-map))
(keymap-set hud-denote-map "c" (cons "denote-review" hud-denote-review-map))
(keymap-set hud-denote-map "h" (cons "denote-hierarchy" hud-denote-hierarchy-map))

(keymap-set hud-smerge-map "n" #'smerge-vc-next-conflict)
(keymap-set hud-smerge-map "k" #'smerge-kill-current)
(keymap-set hud-smerge-map "s" #'smerge-start-session)
(keymap-set hud-smerge-map "t" #'smerge-keep-current)

(keymap-set hud-mode-map "C-c t" (cons "core" hud-core-map))
(keymap-set hud-mode-map "C-c f" (cons "display" hud-display-map))
(keymap-set hud-mode-map "C-c k" (cons "kill" hud-kill-map))
(keymap-set hud-mode-map "C-c w" (cons "web-browser" hud-web-browser-map))
(keymap-set hud-mode-map "C-c g" (cons "grep" hud-ecclectic-grep-map))
(keymap-set hud-mode-map "C-c ." (cons "completion" hud-completion-map))
(keymap-set hud-mode-map "C-c h" (cons "docs" hud-docs-map))
(keymap-set hud-mode-map "C-c l" (cons "ide" hud-ide-map))
(keymap-set hud-mode-map "C-c s" (cons "shell" hud-shell-map))
(keymap-set hud-mode-map "C-c r" (cons "robot" hud-robot-map))
(keymap-set hud-mode-map "C-c q" (cons "anzu" hud-anzu-map))
(keymap-set hud-mode-map "C-x g" (cons "magit" hud-magit-map))
(keymap-set hud-mode-map "C-x d" (cons "docker" hud-docker-map))
(keymap-set hud-mode-map "C-c d" (cons "denote" hud-denote-map))
(keymap-set hud-mode-map "C-c o" (cons "org" orgx-global-map))
(keymap-set hud-mode-map "C-c m" (cons "mail" hud-mail-map))
(keymap-set hud-mode-map "C-x C-b" (cons "buffer-control" hud-buffer-control-map))
(keymap-set hud-mode-map "C-c C-;" (cons "consult" hud-consult-mode-map))

(keymap-set hud-mode-map "M-." #'xref-find-definitions)
(keymap-set hud-mode-map "M-/" #'dabbrev-completion)
(keymap-set hud-mode-map "C-M-/" #'dabbrev-expand)

(keymap-set hud-mode-map "M-h" #'windmove-left)
(keymap-set hud-mode-map "M-j" #'windmove-down)
(keymap-set hud-mode-map "M-k" #'windmove-up)
(keymap-set hud-mode-map "M-l" #'windmove-right)
(keymap-set hud-mode-map "S-<left>" #'windmove-left)
(keymap-set hud-mode-map "S-<down>" #'windmove-down)
(keymap-set hud-mode-map "S-<up>" #'windmove-up)
(keymap-set hud-mode-map "S-<right>" #'windmove-right)

(keymap-set hud-mode-map "C-x ." #'hud-dispatch)
(keymap-set hud-mode-map "C-x ," #'hud-select)

;; general bindings that used to go straight into global-map
(keymap-set hud-mode-map "C-x l" #'goto-line)
(keymap-set hud-mode-map "C-x f" #'find-file)
(keymap-set hud-mode-map "C-x h" #'mark-whole-buffer) ;; default
(keymap-set hud-mode-map "C-x m" #'execute-extended-command)
(keymap-set hud-mode-map "C-x C-m" #'execute-extended-command)
(keymap-set hud-mode-map "C-x C-f" #'find-file)
(keymap-set hud-mode-map "C-x C-x" #'exchange-point-and-mark)
(keymap-set hud-mode-map "C-x C-r" #'recentf)
(keymap-set hud-mode-map "C-x C-n" #'count-words)
(keymap-set hud-mode-map "C-c i" #'indent-region)
(keymap-set hud-mode-map "C-c c" #'comment-region)
(keymap-set hud-mode-map "C-c u w" #'upcase-word)
(keymap-set hud-mode-map "C-c u t" #'upcase-initials-region)
(keymap-set hud-mode-map "C-c u r" #'upcase-region)
(keymap-set hud-mode-map "C-c C-f" #'set-fill-column)
(keymap-set hud-mode-map "C-c C-p" #'set-mark-command)
(keymap-set hud-mode-map "C-c C-r" #'rename-buffer)
(keymap-set hud-mode-map "C-z" #'undo)
(keymap-set hud-mode-map "C-w" #'kill-region)
(keymap-set hud-mode-map "C-h" #'backward-kill-word)
(keymap-set hud-mode-map "C-<backspace>" #'backward-kill-word)
(keymap-set hud-mode-map "C-<tab>" #'completion-at-point)
(keymap-set hud-mode-map "M-<SPC>" #'set-mark-command)
(keymap-set hud-mode-map "M-X" #'execute-extended-command-for-buffer)
(keymap-set hud-mode-map "s-c" #'clipboard-kill-ring-save) ;; (CUA/macOS) copy
(keymap-set hud-mode-map "s-v" #'clipboard-yank)            ;; (CUA/macOS) paste
(keymap-set hud-mode-map "s-x" #'clipboard-kill-region)     ;; (CUA/macOS) cut
(keymap-set hud-mode-map "s-h" #'hud-increase-window-left)
(keymap-set hud-mode-map "s-j" #'hud-increase-window-down)
(keymap-set hud-mode-map "s-k" #'hud-increase-window-up)
(keymap-set hud-mode-map "s-l" #'hud-increase-window-right)
(keymap-set hud-mode-map "s-<left>" #'hud-increase-window-left)
(keymap-set hud-mode-map "s-<down>" #'hud-increase-window-down)
(keymap-set hud-mode-map "s-<up>" #'hud-increase-window-up)
(keymap-set hud-mode-map "s-<right>" #'hud-increase-window-right)
(keymap-set hud-mode-map "M-<up>" #'move-text-up)
(keymap-set hud-mode-map "M-<down>" #'move-text-down)
(keymap-set hud-mode-map "<mouse-2>" #'clipboard-yank)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; window management

(defun hud-increase-window-up () (interactive) (enlarge-window 1 nil))
(defun hud-increase-window-down () (interactive) (enlarge-window -1 nil))
(defun hud-increase-window-left () (interactive) (enlarge-window 1 t))
(defun hud-increase-window-right () (interactive) (enlarge-window -1 t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; frame management

(defvar-local hud--buffer-home-frame nil
  "Frame where this buffer was first shown; nil means no frame restriction.")

(defun hud--record-home-frame ()
  "Record the selected frame as this buffer's home frame (first visit only)."
  (unless hud--buffer-home-frame
    (setq-local hud--buffer-home-frame (selected-frame))))

(defun hud--frame-buffer-predicate (buf)
  "Unified predicate controlling next-buffer and switch-to-buffer candidates."
  (let* ((current (current-buffer))
         (current-file (buffer-file-name current))
         (current-readonly (buffer-local-value 'buffer-read-only current)))
    (with-current-buffer buf
      (and
       ;; Rule 1: frame-sticky buffers stay on their home frame
       (or (null hud--buffer-home-frame)
           (eq hud--buffer-home-frame (selected-frame)))
       ;; Rules 2 & 3: file-based navigation restrictions
       (cond
        ;; Not in a file buffer — no restriction
        ((null current-file) t)
        ;; In a read-only file (reference buffer) — only writable files
        (current-readonly (and (buffer-file-name) (not buffer-read-only)))
        ;; In a writable file — only file buffers (any)
        (t (buffer-file-name)))))))

(defun hud--install-buffer-predicate (frame)
  "Install the unified buffer predicate on FRAME."
  (set-frame-parameter frame 'buffer-predicate
                        #'hud--frame-buffer-predicate))

;;;###autoload
(defun hud-run-current-major-mode-hooks (&optional buffer)
  "Run all mode-hooks for the current major mode."
  (interactive)
  (with-current-buffer (or (when (bufferp buffer) buffer)
			   (when (and (stringp buffer) (get-buffer buffer)) buffer)
			   (current-buffer))
    (apply #'run-mode-hooks (seq-keep (lambda (it) (intern-soft (format "%s-hook" it))) (derived-mode-all-parents major-mode)))))

;; display-buffer: reference (read-only file) buffers reuse an existing
;; read-only window rather than displacing a writable project-file window
(defun hud--readonly-file-buffer-p (buf _action)
  "Return non-nil if BUF is a read-only file-visiting buffer."
  (with-current-buffer buf
    (and buffer-read-only (buffer-file-name))))

(defun hud--reuse-readonly-file-window (buffer _alist)
  "Action: display BUFFER in an existing read-only file window if one exists."
  (when-let* ((win (seq-find
                    (lambda (w)
                      (and (not (eq w (selected-window)))
                           (with-current-buffer (window-buffer w)
                             (and buffer-read-only (buffer-file-name)))))
                    (window-list nil 'nomini))))
    (set-window-buffer win buffer)
    win))

(defun contextual-menubar (&optional frame)
  "Display the menubar in FRAME (default: selected frame) if on a graphical display, but hide it if in terminal."
  (interactive)
  (set-frame-parameter frame 'menu-bar-lines (if (display-graphic-p frame) 1 0)))

(defun frame-unset-background-for-tty (frame)
  ;; https://stackoverflow.com/questions/19054228/emacs-disable-theme-background-color-in-terminal
  ;;
  ;; The sentinel strings "unspecified-bg"/"unspecified-fg" stick; the symbol
  ;; `unspecified' does not -- Emacs's default-face realization silently
  ;; resolves it back to the frame's concrete background-color parameter on
  ;; the next redisplay, undoing the effect a moment after it's set.
  (unless (display-graphic-p frame)
    (set-face-background 'default "unspecified-bg" frame)
    (set-face-foreground 'default "unspecified-fg" frame)))

(defun current-frame-unset-background-for-tty ()
  "Reset the background on the current frame, but only if its a TTY frame."
  (interactive)
  (frame-unset-background-for-tty (selected-frame)))

(defun ad:unset-background-for-tty-frames-after-theme-enable (&rest _theme)
  "Re-apply the TTY background unset to all TTY frames after a theme is enabled.
Themes set an explicit `default' face background, which clobbers the
unspecified background `frame-unset-background-for-tty' set for TTY frames."
  (seq-do #'frame-unset-background-for-tty (frame-list)))

(defun frame-enable-xterm-mouse-for-tty (frame)
  "Enable xterm-mouse-mode when FRAME is a tty frame."
  (unless (display-graphic-p frame)
    (xterm-mouse-mode 1)))

(defun current-frame-enable-xterm-mouse-for-tty ()
  "Enable xterm-mouse-mode if the current frame is a tty."
  (frame-enable-xterm-mouse-for-tty (selected-frame)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; editing -- text editing, experience, and manipulation

;; move-text  -- use arrow keys to move whole

(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (when (> (point) (mark))
      (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(declare-function org-metaup "org")
(declare-function org-metadown "org")

;;;###autoload
(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line arg lines down.
In `org-mode', defers to `org-metadown' instead, which moves the
subtree at point rather than just the current line."
  (interactive "*p")
  (if (derived-mode-p 'org-mode)
      (org-metadown arg)
    (move-text-internal arg)))

;;;###autoload
(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line arg lines up.
In `org-mode', defers to `org-metaup' instead, which moves the subtree
at point rather than just the current line."
  (interactive "*p")
  (if (derived-mode-p 'org-mode)
      (org-metaup arg)
    (move-text-internal (- arg))))

;; word-wrapping  --

(defalias 'turn-on-hard-wrap 'turn-off-soft-wrap)
(defalias 'turn-off-hard-wrap 'turn-on-soft-wrap)
(defalias 'toggle-soft-wrap 'toggle-on-soft-wrap)
(defalias 'toggle-hard-wrap 'toggle-off-soft-wrap)

;;;###autoload
(defun turn-on-soft-wrap ()
  (interactive)
  (let ((was-hard-wrapping auto-fill-function))
    (auto-fill-mode -1)
    (visual-line-mode 1)
    (when was-hard-wrapping
      (hud-show-wrapping-mode))))

;;;###autoload
(defun turn-off-soft-wrap ()
  (interactive)
  (let ((was-soft-wrapping (not auto-fill-function)))
    (visual-line-mode -1)
    (auto-fill-mode 1)
    (when was-soft-wrapping
      (hud-show-wrapping-mode))))

;;;###autoload
(defun toggle-word-wrap (&optional arg)
  (interactive)
  (when arg
    (user-error "ambiguous argument to `toggle-word-wrap'"))
  (if auto-fill-function
      (turn-on-soft-wrap)
    (turn-on-hard-wrap)))

(defun hud-show-wrapping-mode ()
  (let ((buf (current-buffer))
	(wrapping-mode (if auto-fill-function
                           "hard"
                         "soft")))
    (message "wrapping mode `%s' for %s <%s>"
	     wrapping-mode
	     (buffer-local-value 'major-mode buf)
	     (buffer-name buf))))

;;;###autoload
(defun unfill-region (begin end)
  "Remove all linebreaks in a region but leave paragraphs
  indented text (quotes,code) and lines starting with an asterix (lists) intakt."
  (interactive "r")
  (replace-regexp-in-region "\\([^\n]\\)\n\\([^ *\n]\\)" "\\1 \\2" begin end))

;; tab width --

(defmacro hud-set-tab-width (num)
  (unless (integerp num)
    (signal 'wrong-type-argument (list 'integerp num)))
  (unless (< num 32)
    (warn "INVALID cannot create tab width hook function to >= 32 (%s)" num))

  (let ((generated-name (intern (format "hud-set-local-tab-width-%d" num))))
    `(defun ,generated-name ()
       (set-tab-width ,num))))

;;;###autoload
(defun set-tab-width (num-spaces)
  (interactive "nTab width: ")
  (setq-local tab-width num-spaces))

(defun font-lock-show-tabs ()
  "Return a font-lock style keyword for tab characters."
  '(("\t" 0 'trailing-whitespace prepend)))

(defun font-lock-width-keyword (width)
  "Return a font-lock style keyword for strings beyond WIDTH that use `font-lock-warning-face'."
  `((,(format "^%s\\(.+\\)" (make-string width ?.))
     (1 font-lock-warning-face t))))

;; line manipulation

;;;###autoload
(defun uniquify-region-lines (beg end)
  "Remove duplicate adjacent lines between BEG and END."
  (interactive "*r")
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
      (replace-match "\\1"))))

;;;###autoload
(defun uniquify-buffer-lines ()
  "Remove duplicate adjacent lines in the current buffer."
  (interactive)
  (uniquify-region-lines (point-min) (point-max)))

;; files and notes

;;;###autoload
(defun hud-insert-date ()
  "Insert date string."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

;;;###autoload
(defun jump-to-elisp-help ()
  (interactive)
  (apropos-documentation (symbol-name (intern-soft (thing-at-point 'symbol)))))

(declare-function consult-eglot-symbols "consult-eglot")

;;;###autoload
(defun hud-describe-symbol-dwim (prefix)
  "Look up symbol at point contextually.
With PREFIX arg, always use `describe-symbol'.
Otherwise: use `slime-describe-symbol' if slime is connected,
`consult-eglot-symbols' if in an eglot-managed buffer,
or `describe-symbol' as fallback."
  (interactive "P")
  (cond
   (prefix
    (call-interactively #'describe-symbol))
   ((and (boundp 'slime-describe-symbol)
         (boundp 'slime-connected-p)
         (slime-connected-p))
    (call-interactively #'slime-describe-symbol))
   ((and (boundp 'eglot-current-server)
         (boundp 'consult-eglot-symbols)
         (eglot-current-server))
    (consult-eglot-symbols))
   (t
    (call-interactively #'describe-symbol))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; display

;;;###autoload
(defun text-scale-reset ()
  (interactive)
  (text-scale-set 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; opacity

(defun djcb-opacity-modify (&optional dec)
  "Modify frame transparency by 5% steps."
  (let* ((raw (frame-parameter nil 'alpha))
         (current (cond
                   ((null raw) 1.0)
                   ((floatp raw) raw)
                   (t (/ raw 100.0))))
         (next (if dec (- current 0.025) (+ current 0.025))))
    (when (and (>= next 0.2) (<= next 1.0))
      (modify-frame-parameters nil (list (cons 'alpha next))))))

;;;###autoload
(defun hud-opacity-increase ()
  (interactive)
  (djcb-opacity-modify))

;;;###autoload
(defun hud-opacity-decrease ()
  (interactive)
  (djcb-opacity-modify t))

;;;###autoload
(defun hud-opacity-reset ()
  (interactive)
  (modify-frame-parameters nil '((alpha . 0.95))))

(defvar-keymap hud-opacity-repeat-map
  :name "opacity-repeat"
  :doc "Opacity repeat map for repeating opacity changes."
  :repeat t)

(keymap-set hud-opacity-repeat-map "=" #'hud-opacity-increase)
(keymap-set hud-opacity-repeat-map "-" #'hud-opacity-decrease)
(keymap-set hud-opacity-repeat-map "0" #'hud-opacity-reset)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; extra buffer management tools

(declare-function annotated-completing-read-directory "annotated-completing-read")
(declare-function f-ancestor-of-p "f")

(defun hud--minimize-path (path)
  "Replace the expanded home directory prefix in PATH with `~/'."
  (string-replace (expand-file-name "~/") "~/" path))

;;;###autoload
(defun save-all-buffers ()
  "Save all unsaved buffers without prompting."
  (interactive)
  (save-some-buffers t t))

;;;###autoload
(defun kill-eldoc-and-help-buffers ()
  "Kills all eldoc and help buffers"
  (interactive)
  (kill-matching-buffers "\\*Help\\*\\|\\*eldoc.*\\*" nil t))

(defun buffers-matching-path (regexp &optional internal-too)
  (seq-filter (lambda (buf)
                (when-let* ((name (buffer-file-name buf)))
                  (and (not (string-equal name ""))
                       (or internal-too (/= (aref name 0) ?\s))
                       (string-match regexp name))))
              (buffer-list)))

(defun buffers-matching-mode (mode)
  (seq-filter (lambda (buf) (with-current-buffer buf (eq major-mode mode)))
              (buffer-list)))

;;;###autoload
(defun kill-buffers-in-directory (&optional directory)
  "Kill all buffers in `directory'. When not defined, a directory can be selected interactively."
  (interactive)

  (unless directory
    (setq directory (annotated-completing-read-directory)))

  (let ((killed (thread-last (buffer-list)
			     (seq-filter #'buffer-file-name)
			     (seq-filter (lambda (buf) (f-ancestor-of-p directory (buffer-file-name buf))))
			     (seq-map (lambda (buf) (cons (buffer-file-name buf) (kill-buffer buf))))
			     (seq-filter #'cdr)
			     (seq-map (lambda (c) (hud--minimize-path (car c)))))))

    (if (called-interactively-p 'any)
	(message "killed %d buffers in subdirectory %s: '%S'" (length killed) (hud--minimize-path directory) (string-join killed ", "))
      killed)))

(defalias 'kill-buffers-matching-name 'kill-matching-buffers)

;;;###autoload
(defun force-kill-buffers-matching-path (regexp)
  (interactive "sKill buffers visiting a path matching this regular expression: \n")
  (kill-buffers-matching-path regexp t t))

;;;###autoload
(defun kill-buffers-matching-path (regexp &optional internal-too no-ask)
  "Kill buffers whose name matches the specified REGEXP.
Ignores buffers whose name starts with a space, unless optional
prefix argument INTERNAL-TOO is non-nil.  Asks before killing
each buffer, unless NO-ASK is non-nil."
  (interactive "sKill buffers visiting a path matching this regular expression: \n")
  (let ((killed (thread-last
		  (buffers-matching-path regexp internal-too)
		  (seq-map (lambda (buf)
			     (cons (buffer-file-name buf)
				   (funcall (if no-ask #'kill-buffer #'kill-buffer-ask) buf))))
		  (seq-filter #'cdr)
		  (seq-map #'car))))

    (if (called-interactively-p 'any)
	(message "killed %d buffers matching '%S'" (length killed) (string-join killed ", "))
      killed)))

;;;###autoload
(defun kill-all-reference-and-source-buffers ()
  "Kill all buffers for files in external (upstream) sources, likely opened
by jump-to-definition."
  (interactive)
  (let ((killed (thread-last
		  '("/usr/share/emacs/.*" "/usr/lib/go/.*" ".*/src/emacs.*/src/.*")
		  (append (cons package-user-dir package-directory-list))
		  (seq-map #'force-kill-buffers-matching-path)
		  (seq-filter 'identity)
                  (seq-filter #'stringp)
		  (seq-map #'hud--minimize-path))))
    (if (called-interactively-p 'any)
	(message "killed %s refrence/source buffers [%s]" (length killed) (string-join killed ", "))
      killed)))

;;;###autoload
(defun kill-buffers-matching-mode (mode)
  "Kill all buffers matching the symbol defined by MODE.
Returns the number of buffers killed."
  (interactive
   (list (intern
          (completing-read
           "mode: " ;; prompt
           obarray  ;; collection
           (lambda (symbol) (string-suffix-p "-mode" (symbol-name symbol)))
           t nil nil major-mode))))
  (let* ((buffers (buffers-matching-mode mode))
	 (count (length buffers)))
    (message "killing all buffers (%d) with mode \"%s\"" count mode)
    (mapc #'kill-buffer buffers)
    count))

;;;###autoload
(defun kill-buffers-visiting-missing-files ()
  "Kill buffers visiting files that no longer exist on disk.
Prompts before killing each buffer.  Returns the list of killed file paths
when called non-interactively."
  (interactive)
  (let ((killed (thread-last (buffer-list)
                             (seq-filter (lambda (buf)
                                           (when-let* ((file (buffer-file-name buf)))
                                             (not (file-exists-p file)))))
                             (seq-map (lambda (buf)
                                        (cons (buffer-file-name buf) (kill-buffer-ask buf))))
                             (seq-filter #'cdr)
                             (seq-map #'car))))
    (if (called-interactively-p 'any)
        (message "killed %d buffers visiting missing files%s"
                 (length killed)
                 (if killed
                     (format ": %s" (string-join (seq-map #'hud--minimize-path killed) ", "))
                   ""))
      killed)))

;;;###autoload
(defun kill-buffer-and-delete-file (&optional buffer)
  "Kill BUFFER (default the current buffer), then delete the file it visits.
Errors when BUFFER is not visiting a file.  The buffer is killed
before the file is deleted, so declining Emacs's built-in \"buffer
modified; kill anyway?\" prompt, or its \"save and then kill\" choice,
leaves the file on disk.  Never prompts when called non-interactively."
  (interactive)
  (let* ((buffer (or buffer (current-buffer)))
         (file (buffer-file-name buffer)))
    (unless file
      (user-error "Buffer '%s' is not visiting a file" (buffer-name buffer)))
    (if (called-interactively-p 'any)
        (unless (kill-buffer buffer)
          (user-error "Aborted: '%s' was not killed" (buffer-name buffer)))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buffer)))
    (delete-file file)
    (when (called-interactively-p 'any)
      (message "Deleted %s" (hud--minimize-path file)))))

(defun hud-super-abort-minibuffers ()
  (interactive)
  (if (not (minibuffer-selected-window))
      (keyboard-quit)
    (abort-minibuffers)
    (minibuffer-keyboard-quit))
  (when (minibuffer-selected-window)
    (move-beginning-of-line nil)
    (kill-line)
    (abort-minibuffers)))

;;;###autoload
(defun pin-buffer-to-window-toggle ()
  "pin buffer to window, most useful in keeping chat buffers under control"
  (interactive)
  (let* ((buf (current-buffer))
	 (window (selected-window))
	 (current-state (window-dedicated-p window))
	 (buf-name (buffer-name buf)))

    (set-window-dedicated-p window (not current-state))

    (if current-state
	(message "pinned %s to window" buf-name)
      (message "unpinned %s from window" buf-name))))

;;;###autoload
(defun buffer-line-count (&optional buf)
  "Return the number of lines in the specified buffer (name or buffer), defaulting to the current buffer."
  (car (buffer-line-statistics buf)))

;;;###autoload
(defun buffer-directory (buf)
  "Return the `default-directory' of the provide buffer."
  (when (bufferp buf)
    (with-current-buffer buf
      (let ((file-name (buffer-file-name buf)))
	(cond ((null file-name) nil)
	      ((file-directory-p file-name) file-name)
	      ((file-regular-p file-name) (file-name-directory file-name))
	      (t default-directory))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; kill ring

(defvar clean-kill-ring-filters '(string-blank-p))
(defvar clean-kill-ring-prevent-duplicates t)

(defun clean-kill-ring-filter-catch-p (string)
  "T if STRING satisfies at least one of `clean-kill-ring-filters'."
  (let ((s (substring-no-properties string)))
    (and (seq-some (lambda (filter) (funcall filter s))
                   clean-kill-ring-filters)
         t)))

;;;###autoload
(defun clean-kill-ring-clean (&optional remove-dups)
  "Remove `kill-ring' members that satisfy one of`clean-kill-ring-filters'.

If REMOVE-DUPS or `clean-kill-ring-prevent-duplicates' is non-nil, or if called
interactively then remove duplicate items from the `kill-ring'."
  (interactive (list t))
  (let ((cleaned (seq-remove #'clean-kill-ring-filter-catch-p kill-ring)))
    (setq kill-ring
          (if (or remove-dups clean-kill-ring-prevent-duplicates)
              (delete-dups cleaned)
            cleaned))))

(defun ad:kill-new-reject-empty (string &optional _replace)
  "Prevent empty STRING from being added to the kill ring."
  (not (string-empty-p string)))

(advice-add 'kill-new :before-while #'ad:kill-new-reject-empty)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the hooks

(add-hook 'after-make-frame-functions #'frame-unset-background-for-tty)
(add-hook 'after-make-frame-functions #'hud--install-buffer-predicate)
(add-hook 'server-after-make-frame-hook #'current-frame-unset-background-for-tty)
(add-hook 'window-setup-hook #'current-frame-unset-background-for-tty)
(add-hook 'enable-theme-functions #'ad:unset-background-for-tty-frames-after-theme-enable)

(add-hook 'after-make-frame-functions #'frame-enable-xterm-mouse-for-tty)
(add-hook 'server-after-make-frame-hook #'current-frame-enable-xterm-mouse-for-tty)
(add-hook 'window-setup-hook #'current-frame-enable-xterm-mouse-for-tty)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; the mode

;;;###autoload
(define-minor-mode hud-mode
  "Global minor mode holding all of tycho's custom keybindings.
See `hud-mode-map'."
  :global t
  :group 'convenience
  :keymap hud-mode-map)

(provide 'hud-mode)
;;; hud-mode.el ends here
