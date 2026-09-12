;;; ollama-tailnet-test.el --- Tests for ollama-tailnet -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT unit tests for ollama-tailnet registry, gptel backend resolution,
;; fallback strategies, laptop preset profiles, and orchestration control functions.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ollama-tailnet-vars)
(require 'ollama-tailnet-gptel)
(require 'ollama-tailnet-control)
(require 'ollama-tailnet)

(defvar hud-robot-ollama-tailnet-map)

(ert-deftest ollama-tailnet-test-host-registration ()
  "Test registering and retrieving tailnet hosts."
  (ollama-tailnet-clear-hosts)
  (let ((h1 (ollama-tailnet-register-host 'derrida
              :host "derrida.tailnet:11434"
              :default-model "gemma4:27b"
              :models '("gemma4:27b" "gemma4:32b")))
        (h2 (ollama-tailnet-register-host 'laptop
              :host "127.0.0.1:11434"
              :default-model "gemma4:2b"
              :models '("gemma4:2b" "mistral:7b"))))
    (should (equal (ollama-tailnet-host-name h1) 'derrida))
    (should (equal (ollama-tailnet-host-address h1) "derrida.tailnet:11434"))
    (should (equal (ollama-tailnet-host-default-model h1) "gemma4:27b"))
    (should-not (ollama-tailnet-host-local-p h1))
    (should (ollama-tailnet-host-local-p h2))
    (should (equal (length (ollama-tailnet-list-hosts)) 2))))

(ert-deftest ollama-tailnet-test-model-resolution ()
  "Test resolving host-model specs to host struct and model name."
  (ollama-tailnet-clear-hosts)
  (ollama-tailnet-register-host 'derrida
    :host "derrida.tailnet:11434"
    :default-model "gemma4:27b"
    :models '("gemma4:27b" "gemma4:32b"))
  (ollama-tailnet-register-host 'laptop
    :host "127.0.0.1:11434"
    :default-model "gemma4:2b")

  ;; Explicit model spec string
  (let ((res (ollama-tailnet-resolve-model "derrida/gemma4:32b")))
    (should (equal (ollama-tailnet-host-name (car res)) 'derrida))
    (should (equal (cdr res) "gemma4:32b")))

  ;; Default model spec string
  (let ((res (ollama-tailnet-resolve-model "derrida/default")))
    (should (equal (ollama-tailnet-host-name (car res)) 'derrida))
    (should (equal (cdr res) "gemma4:27b")))

  ;; Symbol spec
  (let ((res (ollama-tailnet-resolve-model 'laptop/default)))
    (should (equal (ollama-tailnet-host-name (car res)) 'laptop))
    (should (equal (cdr res) "gemma4:2b"))))

(ert-deftest ollama-tailnet-test-fallback-strategies ()
  "Test fallback strategies for unknown or unreachable host specs."
  (ollama-tailnet-clear-hosts)
  (ollama-tailnet-register-host 'laptop
    :host "127.0.0.1:11434"
    :default-model "gemma4:2b"
    :local-p t)

  ;; Test :auto-local fallback
  (let ((ollama-tailnet-fallback-strategy :auto-local))
    (let ((res (ollama-tailnet-resolve-model "unknown-host/gemma4:2b")))
      (should (equal (ollama-tailnet-host-name (car res)) 'laptop))
      (should (equal (cdr res) "gemma4:2b"))))

  ;; Test :error fallback
  (let ((ollama-tailnet-fallback-strategy :error))
    (should-error (ollama-tailnet-resolve-model "unknown-host/gemma4:2b"))))

(ert-deftest ollama-tailnet-test-gptel-backend-caching ()
  "Test gptel backend struct generation and caching."
  (ollama-tailnet-clear-hosts)
  (ollama-tailnet-clear-backend-cache)
  (ollama-tailnet-register-host 'derrida
    :host "derrida.tailnet:11434"
    :default-model "gemma4:27b")

  (let ((b1 (ollama-tailnet-get-gptel-backend "derrida/default"))
        (b2 (ollama-tailnet-get-gptel-backend "derrida/gemma4:27b")))
    (should (eq b1 b2))
    (should b1)))

(ert-deftest ollama-tailnet-test-laptop-presets ()
  "Test laptop presets initialization."
  (ollama-tailnet-clear-hosts)
  (ollama-tailnet-setup-laptop-presets)
  (let ((derrida (ollama-tailnet-get-host 'derrida))
        (laptop (ollama-tailnet-get-host 'laptop)))
    (should derrida)
    (should laptop)
    (should (equal (ollama-tailnet-host-default-model derrida) "gemma4:27b"))
    (should (equal (ollama-tailnet-host-default-model laptop) "gemma4:2b"))))

(ert-deftest ollama-tailnet-test-systemctl-cmd ()
  "Test systemctl command construction for local and remote hosts."
  (ollama-tailnet-clear-hosts)
  (let ((h-local (ollama-tailnet-register-host 'laptop :host "127.0.0.1:11434" :local-p t))
        (h-remote (ollama-tailnet-register-host 'derrida :host "derrida.tailnet:11434" :local-p nil)))
    (should (equal (ollama-tailnet--systemctl-cmd h-local "restart" "ollama")
                   '("systemctl" "--user" "restart" "ollama")))
    (should (equal (ollama-tailnet--systemctl-cmd h-remote "restart" "ollama")
                   '("ssh" "derrida.tailnet" "systemctl" "--user" "restart" "ollama")))))

(ert-deftest ollama-tailnet-test-hud-binding ()
  "Test HUD keymap binding function."
  (let ((hud-robot-ollama-tailnet-map (make-sparse-keymap)))
    (ollama-tailnet-bind-hud-keys)
    (should (keymap-lookup hud-robot-ollama-tailnet-map "s"))
    (should (keymap-lookup hud-robot-ollama-tailnet-map "p"))
    (should (keymap-lookup hud-robot-ollama-tailnet-map "r"))
    (should (keymap-lookup hud-robot-ollama-tailnet-map "t"))
    (should (keymap-lookup hud-robot-ollama-tailnet-map "b"))))

(provide 'ollama-tailnet-test)
;;; ollama-tailnet-test.el ends here
