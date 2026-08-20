;;; elpaish-recipes.el --- Package recipes for the Tychoish ecosystem -*- lexical-binding: t; -*-

;; Author: tychoish
;; Keywords: maintenance, tools, elpa, package

;;; Commentary:
;; Declarative package recipes for packages maintained across the tychoish
;; ecosystem.  Provides automatic local-or-remote path resolution so builds
;; execute seamlessly on the developer workstation or in clean CI runners.

;;; Code:

(require 'builder-elpa)

(defun elpaish-recipe-path (local-path remote-url)
  "Return LOCAL-PATH if it exists as a directory, otherwise REMOTE-URL."
  (let* ((expanded (expand-file-name local-path))
         (clean-rel (and (stringp local-path)
                         (string-remove-prefix "~/.emacs.d/" local-path)))
         (local-emacs-d (and clean-rel
                             (boundp 'user-emacs-directory)
                             (expand-file-name clean-rel user-emacs-directory)))
         (local-rel (and clean-rel
                         (expand-file-name clean-rel default-directory))))
    (cond
     ((and (stringp expanded) (file-directory-p expanded))
      expanded)
     ((and local-emacs-d (file-directory-p local-emacs-d))
      local-emacs-d)
     ((and local-rel (file-directory-p local-rel))
      local-rel)
     (t remote-url))))

;;;###autoload
(defun elpaish-recipes-register-all ()
  "Register all Tychoish ecosystem package recipes in `builder-elpa-registry'."
  (interactive)

  ;; 1. annotated-completing-read
  (builder-elpa-register-package
   'annotated-completing-read
   (elpaish-recipe-path "~/src/annotated-completing-read"
                        "https://github.com/tychoish/annotated-completing-read.git")
   :branch "main"
   :files '("annotated-completing-read.el")
   :test-dir "test"
   :summary "Annotated completing-read interface with aligned annotations"
   :url "https://github.com/tychoish/annotated-completing-read"
   :keywords '("convenience" "completion" "matching"))

  ;; 2. agent-shell-queue
  (builder-elpa-register-package
   'agent-shell-queue
   (elpaish-recipe-path "~/.emacs.d/external/agent-shell-queue"
                        "https://github.com/tychoish/agent-shell-queue.git")
   :branch "main"
   :files '("agent-shell-queue.el"
            "agent-shell-queue-org.el"
            "agent-shell-queue-db.el"
            "agent-shell-queue-persistence.el"
            "agent-shell-menu.el")
   :test-dir "test"
   :preflight-skip '(ert)
   :summary "Emacs queue manager for AI agent tasks"
   :url "https://github.com/tychoish/agent-shell-queue"
   :keywords '("tools" "convenience"))

  ;; 3. magit-dash
  (builder-elpa-register-package
   'magit-dash
   (elpaish-recipe-path "~/.emacs.d/external/magit-dash"
                        "https://github.com/tychoish/magit-dash.git")
   :branch "main"
   :files '("magit-dash.el"
            "magit-dash-gh.el"
            "magit-dash-gh-pr.el"
            "magit-dash-gh-actions.el"
            "magit-dash-gh-ci.el"
            "magit-dash-open.el"
            "magit-dash-submodules.el"
            "magit-dash-timer.el")
   :test-dir "test"
   :summary "Personal multi-repository dashboard for Magit and GitHub"
   :url "https://github.com/tychoish/magit-dash"
   :keywords '("tools" "vc" "git"))

  ;; 4. sprite
  (builder-elpa-register-package
   'sprite
   (elpaish-recipe-path "~/.emacs.d/external/sprite"
                        "https://github.com/tychoish/sprite.git")
   :branch "main"
   :files '("sprite.el"
            "sprite-direct.el"
            "sprite-fleet.el"
            "sprite-future.el"
            "sprite-heartbeat.el"
            "sprite-list.el"
            "sprite-session.el")
   :test-dir "test"
   :preflight-skip '(ert)
   :summary "Fast ephemeral Emacs child-daemon manager"
   :url "https://github.com/tychoish/sprite"
   :keywords '("processes" "tools"))

  ;; 5. agent-shell-notifications
  (builder-elpa-register-package
   'agent-shell-notifications
   (elpaish-recipe-path "~/.emacs.d/external/agent-shell-notifications"
                        "https://github.com/zackattackz/agent-shell-notifications.git")
   :branch "main"
   :files '("agent-shell-notifications.el"
            "agent-shell-notifications-knockknock.el"
            "agent-shell-notifications-libnotify.el")
   :preflight-skip '(byte-compile)
   :summary "Notification routing for agent shell sessions"
   :url "https://github.com/zackattackz/agent-shell-notifications"
   :keywords '("tools" "notifications"))

  ;; 6. xtdlib
  (builder-elpa-register-package
   'xtdlib
   (elpaish-recipe-path "~/.emacs.d/external/xtdlib"
                        "https://github.com/tychoish/xtdlib.el")
   :branch "main"
   :files '("xtdlib.el"
            "xtd-dash.el"
            "xtd-f.el"
            "xtd-ht.el"
            "xtd-macro.el"
            "xtd-project.el"
            "xtd-s.el")
   :summary "Extended standard library and macros for Emacs Lisp"
   :url "https://github.com/tychoish/xtdlib"
   :keywords '("extensions" "lisp"))

  ;; 7. xlib
  (builder-elpa-register-package
   'xlib
   (elpaish-recipe-path "~/src/xlib.el"
                        "https://github.com/tychoish/xlib.el.git")
   :branch "main"
   :files '("xlib.el")
   :test-dir "test"
   :summary "Extended elisp utility library"
   :url "https://github.com/tychoish/xlib.el"
   :keywords '("extensions" "utility"))

  ;; 8. elpaish-keyring
  (builder-elpa-register-package
   'elpaish-keyring
   (elpaish-recipe-path "~/.emacs.d/lisp"
                        "https://github.com/tychoish/elpaish.git")
   :branch "main"
   :files '("elpaish-keyring.el")
   :preflight-skip t
   :summary "GPG keyring and trust anchors for ELPAish package archives"
   :url "https://github.com/tychoish/elpaish"
   :keywords '("package" "security" "maintenance" "elpa"))

  (message "Registered %d ELPAish recipes." (hash-table-count builder-elpa-registry)))

;; Automatically register recipes when loaded
(elpaish-recipes-register-all)

(provide 'elpaish-recipes)
;;; elpaish-recipes.el ends here
