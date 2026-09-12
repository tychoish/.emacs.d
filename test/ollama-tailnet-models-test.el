;;; ollama-tailnet-models-test.el --- Tests for ollama-tailnet-models -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT unit tests for ollama-tailnet-models' pure data-shaping functions:
;; /api/tags parsing, digest normalization/comparison, entry building, and
;; library-search HTML scraping.  Network-facing functions
;; (`ollama-tailnet-list-models-for-host', `ollama-tailnet-remote-manifest-digest',
;; `ollama-tailnet-search-library') are not exercised here.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'ollama-tailnet-models)

(ert-deftest ollama-tailnet-models-test-parse-tags-response ()
  "ollama-tailnet--parse-tags-response builds one model struct per entry, tagging the host."
  (let* ((data '((models . (((name . "llama3:8b")
                             (size . 4700000000)
                             (modified_at . "2026-01-01T00:00:00Z")
                             (digest . "sha256:abc123"))
                            ((name . "mistral:7b")
                             (size . 4100000000)
                             (modified_at . "2026-02-01T00:00:00Z")
                             (digest . "sha256:def456"))))))
         (models (ollama-tailnet--parse-tags-response data 'derrida)))
    (should (= (length models) 2))
    (should (equal (ollama-tailnet-model-host (car models)) 'derrida))
    (should (equal (ollama-tailnet-model-name (car models)) "llama3:8b"))
    (should (equal (ollama-tailnet-model-size (car models)) 4700000000))
    (should (equal (ollama-tailnet-model-digest (cadr models)) "sha256:def456"))))

(ert-deftest ollama-tailnet-models-test-parse-tags-response-empty ()
  "ollama-tailnet--parse-tags-response returns nil for an empty models list."
  (should (null (ollama-tailnet--parse-tags-response '((models . nil)) 'derrida))))

(ert-deftest ollama-tailnet-models-test-split-model-name-with-tag ()
  "ollama-tailnet--split-model-name splits an explicit tag."
  (should (equal (ollama-tailnet--split-model-name "llama3:8b") '("llama3" . "8b"))))

(ert-deftest ollama-tailnet-models-test-split-model-name-default-tag ()
  "ollama-tailnet--split-model-name defaults to \"latest\" with no tag."
  (should (equal (ollama-tailnet--split-model-name "llama3") '("llama3" . "latest"))))

(ert-deftest ollama-tailnet-models-test-normalize-digest-strips-prefix ()
  "ollama-tailnet--normalize-digest strips a leading \"sha256:\"."
  (should (equal (ollama-tailnet--normalize-digest "sha256:abc123") "abc123"))
  (should (equal (ollama-tailnet--normalize-digest "abc123") "abc123"))
  (should (null (ollama-tailnet--normalize-digest nil))))

(ert-deftest ollama-tailnet-models-test-digests-differ-p ()
  "ollama-tailnet--digests-differ-p compares normalized digests, and is nil when either is missing."
  (should (ollama-tailnet--digests-differ-p "sha256:aaa" "sha256:bbb"))
  (should-not (ollama-tailnet--digests-differ-p "sha256:aaa" "aaa"))
  (should-not (ollama-tailnet--digests-differ-p nil "sha256:bbb"))
  (should-not (ollama-tailnet--digests-differ-p "sha256:aaa" nil)))

(ert-deftest ollama-tailnet-models-test-check-model-update-sets-upgradeable ()
  "ollama-tailnet-check-model-update sets upgradeable-p from a mocked remote digest."
  (let ((model (ollama-tailnet-model--make :host 'derrida :name "llama3:8b"
                                           :digest "sha256:aaa")))
    (cl-letf (((symbol-function 'ollama-tailnet-remote-manifest-digest)
               (lambda (_name) "sha256:bbb")))
      (ollama-tailnet-check-model-update model)
      (should (eq (ollama-tailnet-model-upgradeable-p model) t))
      (should (equal (ollama-tailnet-model-remote-digest model) "sha256:bbb")))))

(ert-deftest ollama-tailnet-models-test-check-model-update-up-to-date ()
  "ollama-tailnet-check-model-update reports nil (up to date) when digests match."
  (let ((model (ollama-tailnet-model--make :host 'derrida :name "llama3:8b"
                                           :digest "sha256:aaa")))
    (cl-letf (((symbol-function 'ollama-tailnet-remote-manifest-digest)
               (lambda (_name) "sha256:aaa")))
      (ollama-tailnet-check-model-update model)
      (should-not (ollama-tailnet-model-upgradeable-p model)))))

(ert-deftest ollama-tailnet-models-test-check-model-update-unknown-on-error ()
  "ollama-tailnet-check-model-update sets `unknown' when the digest lookup errors."
  (let ((model (ollama-tailnet-model--make :host 'derrida :name "someuser/custom"
                                           :digest "sha256:aaa")))
    (cl-letf (((symbol-function 'ollama-tailnet-remote-manifest-digest)
               (lambda (_name) (user-error "unsupported"))))
      (ollama-tailnet-check-model-update model)
      (should (eq (ollama-tailnet-model-upgradeable-p model) 'unknown)))))

(ert-deftest ollama-tailnet-models-test-format-size ()
  "ollama-tailnet--format-size renders bytes with an appropriate unit."
  (should (equal (ollama-tailnet--format-size 500) "500 B"))
  (should (equal (ollama-tailnet--format-size 4700000000) "4.7 GB"))
  (should (equal (ollama-tailnet--format-size nil) "-")))

(ert-deftest ollama-tailnet-models-test-status-string-reflects-upgradeable-p ()
  "ollama-tailnet--status-string renders the three upgradeable-p states distinctly."
  (let ((up (ollama-tailnet-model--make :upgradeable-p t))
        (ok (ollama-tailnet-model--make :upgradeable-p nil))
        (unk (ollama-tailnet-model--make :upgradeable-p 'unknown)))
    (should (equal (ollama-tailnet--status-string up) "update avail"))
    (should (equal (ollama-tailnet--status-string ok) "up to date"))
    (should (equal (ollama-tailnet--status-string unk) "unknown"))))

(ert-deftest ollama-tailnet-models-test-build-entry-shape ()
  "ollama-tailnet--build-entry produces a (model . [host model size modified status]) entry."
  (let* ((model (ollama-tailnet-model--make :host 'derrida :name "llama3:8b"
                                            :size 4700000000
                                            :modified "2026-01-01T00:00:00Z"))
         (entry (ollama-tailnet--build-entry model)))
    (should (eq (car entry) model))
    (should (equal (aref (cadr entry) 0) "derrida"))
    (should (equal (aref (cadr entry) 1) "llama3:8b"))
    (should (equal (aref (cadr entry) 2) "4.7 GB"))))

(ert-deftest ollama-tailnet-models-test-parse-library-search ()
  "ollama-tailnet--parse-library-search extracts unique (name . summary) pairs from HTML."
  (let* ((html (concat
                "<a href=\"/library/llama3\" class=\"group w-full\">"
                "<h2><span>llama3</span></h2>"
                "<p class=\"max-w-lg break-words text-neutral-800 text-md\">"
                "Meta Llama 3</p></a>"
                "<a href=\"/library/llama3\">dup</a>"
                "<a href=\"/library/mistral\"><p class=\"max-w-lg text-md\">Mistral models</p></a>"))
         (results (ollama-tailnet--parse-library-search html)))
    (should (equal (length results) 2))
    (should (equal (assoc "llama3" results) '("llama3" . "Meta Llama 3")))
    (should (equal (assoc "mistral" results) '("mistral" . "Mistral models")))))

(ert-deftest ollama-tailnet-models-test-parse-library-search-no-matches ()
  "ollama-tailnet--parse-library-search returns nil for HTML with no library links."
  (should (null (ollama-tailnet--parse-library-search "<html><body>no models here</body></html>"))))

(ert-deftest ollama-tailnet-models-test-models-mode-derived-from-tabulated-list-mode ()
  "ollama-tailnet-models-mode is derived from tabulated-list-mode."
  (with-temp-buffer
    (ollama-tailnet-models-mode)
    (should (derived-mode-p 'tabulated-list-mode))))

(ert-deftest ollama-tailnet-models-test-model-at-point-signals-when-empty ()
  "ollama-tailnet--model-at-point signals a user-error with no model at point."
  (with-temp-buffer
    (ollama-tailnet-models-mode)
    (should-error (ollama-tailnet--model-at-point) :type 'user-error)))

(provide 'ollama-tailnet-models-test)
;;; ollama-tailnet-models-test.el ends here
