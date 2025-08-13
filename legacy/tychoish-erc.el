(use-package znc
  :ensure t
  :commands (znc-all))

(use-package erc
  :ensure t
  :commands (erc)
  ;; :bind (("C-c e r" . reset-erc-track-mode)
  ;;        ("C-c e n" . tychoish-next-erc-buffer)
  ;;        ("C-c <SPC>" . erc-next-channel-buffer)
  ;;        ("C-c C-<SPC>" . erc-next-channel-buffer)
  ;;        ("C-c e k" . kill-all-erc-buffers)
  ;;        ("C-c e b" . ido-erc-buffer)
  ;;        ([backtab] . erc-button-url-previous)
  ;;        ("C-c C-d" . erc-truncate-buffer))
  :config
  (add-hook 'erc-mode-hook (lambda ()
                             (make-local-variable 'erc-fill-column)
                             (visual-line-mode 1)
                             (auto-fill-mode 0)))

  (add-hook 'erc-insert-pre-hook
            (lambda (s)
              (when (erc-foolish-content s)
                (setq erc-insert-this nil))))

  (setopt erc-foolish-content '("No such nick/channel"))
  (setopt erc-modules '(stamp
                      completion
                      autojoin
                      irccontrols
                      list
                      match
                      menu
                      move-to-prompt
                      netsplit
                      networks
                      noncommands
                      readonly
                      ring
                      spelling
                      track))

  (setopt erc-ignore-list '("*@*facebook" "&bitlbee"))
  (setopt erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                  "324" "329" "332" "333" "353" "477"))
  (setopt erc-hide-list '("MODE" "JOIN" "PART"))

  (setopt erc-current-nick-highlight-type 'nick)
  (setopt erc-insert-timestamp-function 'erc-insert-timestamp-left)
  (setopt erc-keywords '("\\berc[-a-z]*\\b" "\\bemms[-a-z]*\\b"))

  (setopt erc-timestamp-format "[%H:%M] ")
  (setopt erc-fill-prefix "        ")
  (setopt erc-fill-column 80)

  (setopt erc-auto-query 'bury)
  (setopt erc-format-query-as-channel-p t)
  (setopt erc-header-line-format nil)
  (setopt erc-input-line-position -2)
  (setopt erc-interpret-controls-p 'remove)
  (setopt erc-interpret-mirc-color t)
  (setopt erc-join-buffer 'bury)
  (setopt erc-kill-buffer-on-part t)
  (setopt erc-kill-queries-on-quit t)
  (setopt erc-kill-server-buffer-on-quit t)
  (setopt erc-nicklist-use-icons nil)
  (setopt erc-server-303-functions nil)
  (setopt erc-server-coding-system '(utf-8 . utf-8))
  (setopt erc-timestamp-format "%H:%M ")
  (setopt erc-track-enable-keybindings t)
  (setopt erc-track-exclude-server-buffer t)
  (setopt erc-track-faces-priority-list nil)
  (setopt erc-track-priority-faces-only nil)
  (setopt erc-track-use-faces nil)
  (setopt erc-truncate-buffer-on-save t)
  (setopt erc-max-buffer-size (* 10 1000))

  (add-hook 'erc-insert-post-hook 'erc-truncate-buffer)

  (setopt erc-prompt (lambda ()
                     (if erc-network
                         (concat "[" (symbol-name erc-network) "]")
                       (concat "[" (car erc-default-recipients) "]"))))

  (add-hook 'window-configuration-change-hook
            (lambda ()
              (save-excursion
                (walk-windows
                 (lambda (w)
                   (let ((buffer (window-buffer w)))
                     (set-buffer buffer)
                     (when (eq major-mode 'erc-mode)
                       (setq erc-fill-column (- (window-width w) 2)))))))))

  (defun reset-erc-track-mode ()
    (interactive)
    (setq erc-modified-channels-alist nil)
    (erc-modified-channels-update)
    (erc-track-switch-buffer 1)
    (erc-track-switch-buffer -1))

  (defun erc-foolish-content (msg)
    "Check whether MSG is foolish."
    (erc-list-match erc-foolish-content msg))

  (defun erc-button-url-previous ()
    "Go to the previous URL button in this buffer."
    (interactive)
    (let* ((point (point))
           (found (catch 'found
                    (while (setq point (previous-single-property-change point 'erc-callback))
                      (when (eq (get-tbext-property point 'erc-callback) 'browse-url)
                        (throw 'found point))))))
      (if found
          (goto-char found)
        (error "No previous URL button"))))

  (defun kill-all-erc-buffers ()
    "Kill all erc buffers."
    (interactive)
    (save-excursion
      (let((count 0))
        (dolist(buffer (buffer-list))
          (set-buffer buffer)
          (when (equal major-mode 'erc-mode)
            (setq count (1+ count))
            (kill-buffer buffer)))
        (message "Killed %i erc buffer(s)." count ))))

  (defvar tychoish-erc-disable-connection-status nil
    "When t, disable setting =mode-line-process= with the connection status")

  (defun erc-custom-modeline (buffer)
    (with-current-buffer buffer
      (if tychoish-erc-disable-connection-status
          (setq mode-line-process '())
        (let ((process-status (cond ((and (erc-server-process-alive)
                                          (not erc-server-connected))
                                     ":C")
                                    ((erc-server-process-alive)
                                     ":A")
                                    (t
                                     ":X"))))
          (setq mode-line-process (list process-status))))))

  (advice-add 'erc-update-mode-line-buffer :after #'erc-custom-modeline)

  (defun tychoish-erc-update-modelines ()
    (interactive)
    (dolist (buffer (buffer-list))
      (set-buffer buffer)
      (when (equal major-mode 'erc-mode)
        (erc-custom-modeline buffer))))

  (defvar erc-channels-to-visit nil
    "Channels that have not yet been visited by erc-next-channel-buffer")
  (defun erc-next-channel-buffer ()
    "Switch to the next unvisited channel. See erc-channels-to-visit"
    (interactive)
    (when (null erc-channels-to-visit)
      (setq erc-channels-to-visit
            (remove (current-buffer) (erc-channel-list nil))))
    (let ((target (pop erc-channels-to-visit)))
      (if target
          (switch-to-buffer target))))

  (defun tychoish-next-erc-buffer ()
    "Switch to an IRC buffer, or run `erc-select'.
    When called repeatedly, cycle through the buffers."
    (interactive)
    (let ((buffers (and (fboundp 'erc-buffer-list)
                        (erc-buffer-list))))
      (when (eq (current-buffer) (car buffers))
        (bury-buffer)
        (setq buffers (cdr buffers)))))

  (defun ido-erc-buffer nil
    "Switch to ERC buffer using IDO to choose which one, or start ERC
if not already started."
    (interactive)
    (let (final-list (list ))
      (dolist (buf (buffer-list) final-list)
        (if (equal 'erc-mode (with-current-buffer buf major-mode))
            (setq final-list (append (list (buffer-name buf)) final-list))))
      (if final-list
          (switch-to-buffer (ido-completing-read "Buffer: " final-list))
        (call-interactively 'erc))))

  (setopt erc-track-priority-faces-only (remove "&bitlbee" erc-channel-list))
  (global-emojify-mode 1)
  (erc-update-modules)
  (erc-add-scroll-to-bottom)
  (erc-timestamp-mode 1)
  (erc-notifications-mode 0)
  (erc-fill-mode 1)
  (erc-spelling-mode 1)
  (erc-track-mode 1))

(use-package erc-hl-nicks
  :ensure t
  :after (erc))

(provide 'tychoish-erc)
