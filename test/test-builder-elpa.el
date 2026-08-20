;;; test-builder-elpa.el --- ERT Tests for ELPAish multi-track builder -*- lexical-binding: t; no-byte-compile: t; -*-

;; Author: tychoish
;; Keywords: test, elpa, package

;;; Code:

(require 'ert)
(require 'builder-elpa)
(require 'elpaish-recipes)
(require 'package)
(require 'url)

(defmacro builder-elpa-test-with-temp-env (&rest body)
  "Execute BODY within an isolated temporary registry, work, and output directory."
  `(let* ((temp-dir (make-temp-file "builder-elpa-test-" t))
          (builder-elpa-output-dir (expand-file-name "public/" temp-dir))
          (builder-elpa-work-dir (expand-file-name "repos/" temp-dir))
          (builder-elpa-registry (make-hash-table :test 'equal))
          (builder-elpa-sign-packages nil)
          (builder-elpa-run-preflight nil))
     (unwind-protect
         (progn ,@body)
       (when (process-live-p builder-elpa-server-process)
         (builder-elpa-stop-server))
       (delete-directory temp-dir t))))

(defun builder-elpa-test-create-dummy-pkg (dir name version summary &optional reqs)
  "Create a dummy package file in DIR."
  (make-directory dir t)
  (let ((file (expand-file-name (concat name ".el") dir)))
    (with-temp-file file
      (insert (format ";;; %s.el --- %s -*- lexical-binding: t; -*-\n" name summary))
      (when version
        (insert ";; Version: " version "\n"))
      (when reqs
        (insert ";; Package-Requires: " (format "%S" reqs) "\n"))
      (insert ";; Keywords: test, tools\n")
      (insert ";; URL: https://github.com/tychoish/" name "\n\n")
      (insert "(provide '" name ")\n")
      (insert (format ";;; %s.el ends here\n" name)))))

;;; Tests

(ert-deftest builder-elpa-test-registration ()
  "Test package registration with symbol, string, and recipe attributes."
  (builder-elpa-test-with-temp-env
   (builder-elpa-register-package 'pkg-a "/path/to/a"
                                  :summary "Package A"
                                  :url "https://github.com/test/pkg-a"
                                  :keywords '("convenience"))
   (builder-elpa-register-package "pkg-b" "/path/to/b"
                                  :branch "develop"
                                  :files '("*.el" "src/*.el")
                                  :test-dir "test"
                                  :preflight-skip '(checkdoc))

   (let ((recipe-a (gethash "pkg-a" builder-elpa-registry))
         (recipe-b (gethash "pkg-b" builder-elpa-registry)))
     (should recipe-a)
     (should (equal (builder-elpa-recipe-name recipe-a) "pkg-a"))
     (should (equal (builder-elpa-recipe-branch recipe-a) "main"))
     (should (equal (builder-elpa-recipe-summary recipe-a) "Package A"))
     (should (equal (builder-elpa-recipe-keywords recipe-a) '("convenience")))

     (should recipe-b)
     (should (equal (builder-elpa-recipe-branch recipe-b) "develop"))
     (should (equal (builder-elpa-recipe-files recipe-b) '("*.el" "src/*.el")))
     (should (equal (builder-elpa-recipe-test-dir recipe-b) "test"))
     (should (equal (builder-elpa-recipe-preflight-skip recipe-b) '(checkdoc))))))

(ert-deftest builder-elpa-test-pure-date-version ()
  "Test pure UTC date versioning on elpaish snapshot track."
  (builder-elpa-test-with-temp-env
   (let ((pkg-dir (expand-file-name "date-pkg" temp-dir)))
     (builder-elpa-test-create-dummy-pkg pkg-dir "date-pkg" "0.1.0" "Date Test")
     (let ((ver (builder-elpa--get-snapshot-version pkg-dir)))
       (should (stringp ver))
       ;; Should match pure date format YYYYMMDD.HHMMSS (no header version prefix)
       (should (string-match-p "\\`[0-9]\\{8\\}\\.[0-9]\\{6\\}\\'" ver))
       (should (version-to-list ver))))))

(ert-deftest builder-elpa-test-stable-semver-tag-filtering ()
  "Test clean semver tag resolution and pre-release tag exclusion on elpaish-stable."
  ;; Test clean semver predicate
  (should (builder-elpa--stable-tag-p "v1.2.3"))
  (should (builder-elpa--stable-tag-p "1.0.0"))
  (should (builder-elpa--stable-tag-p "v2.1.0.4"))
  (should-not (builder-elpa--stable-tag-p "v1.2.0-rc1"))
  (should-not (builder-elpa--stable-tag-p "v2.0.0-beta.2"))
  (should-not (builder-elpa--stable-tag-p "v0.9.0-alpha"))
  (should-not (builder-elpa--stable-tag-p "v1.0.0-dev"))
  (should-not (builder-elpa--stable-tag-p "untagged-commit"))

  ;; Test clean semver string
  (should (equal (builder-elpa--clean-semver-string "v1.2.3") "1.2.3"))
  (should (equal (builder-elpa--clean-semver-string "1.2.3") "1.2.3")))

(ert-deftest builder-elpa-test-staging-version-derivation ()
  "Test pre-release tag normalization and git describe versioning on elpaish-staging."
  ;; Pre-release tags normalized for version-to-list
  (should (equal (builder-elpa--normalize-staging-version "v1.2.0-rc1") "1.2.0.rc1"))
  (should (equal (builder-elpa--normalize-staging-version "1.2.0-beta2") "1.2.0.beta2"))
  (should (equal (builder-elpa--normalize-staging-version "v2.0.0-pre1") "2.0.0.pre1"))
  ;; Ensure all normalized versions parse cleanly into version lists
  (should (version-to-list (builder-elpa--normalize-staging-version "v1.2.0-rc1")))
  (should (version-to-list (builder-elpa--normalize-staging-version "1.2.0-beta2")))
  (should (version-to-list (builder-elpa--normalize-staging-version "v2.0.0-pre1"))))

(ert-deftest builder-elpa-test-in-memory-version-injection ()
  "Test in-memory ;; Version: header injection without modifying source files."
  (with-temp-buffer
    (insert ";;; foo.el --- Test -*- lexical-binding: t; -*-\n\n;; Author: Test\n(provide 'foo)\n")
    (builder-elpa--inject-version-header "20260817.143022")
    (should (search-backward ";; Version: 20260817.143022" nil t)))
  (with-temp-buffer
    (insert ";;; foo.el --- Test -*- lexical-binding: t; -*-\n;; Version: 1.0.0\n;; Author: Test\n")
    (builder-elpa--inject-version-header "3.1.4")
    (should (search-backward ";; Version: 3.1.4" nil t))))

(ert-deftest builder-elpa-test-single-file-package-build ()
  "Test building single-file package on elpaish track."
  (builder-elpa-test-with-temp-env
   (let ((pkg-dir (expand-file-name "single-pkg" temp-dir)))
     (builder-elpa-test-create-dummy-pkg pkg-dir "single-pkg" nil "Single File Test" '((emacs "27.1")))
     (builder-elpa-register-package 'single-pkg pkg-dir)
     (let* ((recipe (gethash "single-pkg" builder-elpa-registry))
            (dest (builder-elpa-build-package recipe 'elpaish)))
       (should dest)
       (should (file-exists-p dest))
       (should (string-suffix-p ".el" dest))
       (should (builder-elpa-recipe-built-version-elpaish recipe))
       ;; Verify built artifact has injected version header
       (with-temp-buffer
         (insert-file-contents dest)
         (should (search-forward ";; Version:" nil t)))))))

(ert-deftest builder-elpa-test-multi-file-tar-build ()
  "Test multi-file package tar packaging and <pkg>-pkg.el descriptor generation."
  (builder-elpa-test-with-temp-env
   (let ((pkg-dir (expand-file-name "multi-pkg" temp-dir)))
     (builder-elpa-test-create-dummy-pkg pkg-dir "multi-pkg" "1.0.0" "Multi File Test" '((emacs "28.1") (seq "2.0")))
     (with-temp-file (expand-file-name "multi-pkg-extra.el" pkg-dir)
       (insert ";;; multi-pkg-extra.el -*- lexical-binding: t; -*-\n(provide 'multi-pkg-extra)\n"))
     (builder-elpa-register-package 'multi-pkg pkg-dir
                                    :files '("*.el")
                                    :url "https://github.com/tychoish/multi-pkg"
                                    :keywords '("tools" "convenience"))
     (let* ((recipe (gethash "multi-pkg" builder-elpa-registry))
            (dest (builder-elpa-build-package recipe 'elpaish)))
       (should dest)
       (should (file-exists-p dest))
       (should (string-suffix-p ".tar" dest))
       (should (eq (builder-elpa-recipe-built-type recipe) 'tar))))))

(ert-deftest builder-elpa-test-archive-contents-generation ()
  "Test multi-track `archive-contents' generation."
  (builder-elpa-test-with-temp-env
   (let ((pkg1 (expand-file-name "pkg1" temp-dir))
         (pkg2 (expand-file-name "pkg2" temp-dir)))
     (builder-elpa-test-create-dummy-pkg pkg1 "pkg1" "1.0.0" "First Package")
     (builder-elpa-test-create-dummy-pkg pkg2 "pkg2" "2.0.0" "Second Package")
     (builder-elpa-register-package 'pkg1 pkg1)
     (builder-elpa-register-package 'pkg2 pkg2)

     (builder-elpa-build-package (gethash "pkg1" builder-elpa-registry) 'elpaish)
     (builder-elpa-build-package (gethash "pkg2" builder-elpa-registry) 'elpaish)

     (let ((ac-file (builder-elpa-generate-archive-contents 'elpaish)))
       (should (file-exists-p ac-file))
       (with-temp-buffer
         (insert-file-contents ac-file)
         (let ((data (read (current-buffer))))
           (should (eq (car data) 1))
           (should (assoc 'pkg1 (cdr data)))
           (should (assoc 'pkg2 (cdr data)))
           (let ((entry (cdr (assoc 'pkg1 (cdr data)))))
             (should (vectorp entry))
             ;; Entry: [VER REQS SUMMARY KIND EXTRAS]
             (should (listp (aref entry 0)))
             (should (stringp (aref entry 2)))
             (should (eq (aref entry 3) 'single)))))))))

(ert-deftest builder-elpa-test-html-indexes-and-landing-page ()
  "Test generation of track catalogs and top-level landing page."
  (builder-elpa-test-with-temp-env
   (let ((pkg-dir (expand-file-name "catalog-pkg" temp-dir)))
     (builder-elpa-test-create-dummy-pkg pkg-dir "catalog-pkg" "1.0.0" "Catalog Package")
     (builder-elpa-register-package 'catalog-pkg pkg-dir)
     (builder-elpa-build-package (gethash "catalog-pkg" builder-elpa-registry) 'elpaish)

     (builder-elpa-generate-github-index 'elpaish)
     (builder-elpa-generate-top-index)

     (let ((track-index (expand-file-name "elpaish/index.html" builder-elpa-output-dir))
           (top-index (expand-file-name "index.html" builder-elpa-output-dir)))
       (should (file-exists-p track-index))
       (should (file-exists-p top-index))
       (with-temp-buffer
         (insert-file-contents track-index)
         (should (search-forward "catalog-pkg" nil t)))
       (with-temp-buffer
         (insert-file-contents top-index)
         (should (search-forward "elpaish (Snapshots)" nil t))
         (should (search-forward "elpaish-stable (Releases)" nil t))
         (should (search-forward "elpaish-staging (Pre-release)" nil t)))))))

(ert-deftest builder-elpa-test-build-all-multi-track ()
  "Test building all tracks with `builder-elpa-build-all`."
  (builder-elpa-test-with-temp-env
   (let ((pkg-dir (expand-file-name "all-pkg" temp-dir)))
     (builder-elpa-test-create-dummy-pkg pkg-dir "all-pkg" "1.0.0" "All Track Test")
     (builder-elpa-register-package 'all-pkg pkg-dir)

     (builder-elpa-build-all 'all)

     ;; Snapshot track should exist
     (should (file-exists-p (expand-file-name "elpaish/archive-contents" builder-elpa-output-dir)))
     (should (file-exists-p (expand-file-name "elpaish/index.html" builder-elpa-output-dir)))
     ;; Staging track should exist
     (should (file-exists-p (expand-file-name "elpaish-staging/archive-contents" builder-elpa-output-dir)))
     (should (file-exists-p (expand-file-name "elpaish-staging/index.html" builder-elpa-output-dir)))
     ;; Top index should exist
     (should (file-exists-p (expand-file-name "index.html" builder-elpa-output-dir))))))

(ert-deftest builder-elpa-test-preflight-gate-quarantine ()
  "Test that packages failing preflight validation are quarantined and omitted."
  (builder-elpa-test-with-temp-env
   (let ((pkg-dir (expand-file-name "broken-pkg" temp-dir))
         (builder-elpa-run-preflight t))
     (make-directory pkg-dir t)
     ;; Create package with unbalanced parentheses to trigger check-parens failure
     (with-temp-file (expand-file-name "broken-pkg.el" pkg-dir)
       (insert ";;; broken-pkg.el --- Broken -*- lexical-binding: t; -*-\n")
       (insert "(defun broken (x (missing-close-paren)\n")
       (insert "(provide 'broken-pkg)\n"))

     (builder-elpa-register-package 'broken-pkg pkg-dir)
     (let* ((recipe (gethash "broken-pkg" builder-elpa-registry))
            (built (builder-elpa-build-package recipe 'elpaish)))
       ;; Build should fail and return nil due to preflight quarantine
       (should-not built)
       (should-not (builder-elpa-recipe-built-version-elpaish recipe))))))

(ert-deftest builder-elpa-test-timer-controls ()
  "Test starting and stopping auto-build background timer."
  (unwind-protect
      (progn
        (builder-elpa-start-auto-build "3600")
        (should (timerp builder-elpa-timer))
        (builder-elpa-stop-auto-build)
        (should-not builder-elpa-timer)
        (builder-elpa-start-auto-build "5 mins" t)
        (should (timerp builder-elpa-timer))
        (builder-elpa-stop-auto-build))
    (builder-elpa-stop-auto-build)))

(ert-deftest builder-elpa-test-status-ui ()
  "Test status buffer creation and multi-track column population."
  (builder-elpa-test-with-temp-env
   (let ((pkg-dir (expand-file-name "ui-pkg" temp-dir)))
     (builder-elpa-test-create-dummy-pkg pkg-dir "ui-pkg" "0.5.0" "UI Test")
     (builder-elpa-register-package 'ui-pkg pkg-dir)

     (builder-elpa-status)
     (let ((buf (get-buffer "*builder-elpa-status*")))
       (should buf)
       (with-current-buffer buf
         (should (eq major-mode 'builder-elpa-status-mode))
         (should (search-forward "ui-pkg" nil t))))
     (when (get-buffer "*builder-elpa-status*")
       (kill-buffer "*builder-elpa-status*")))))

(ert-deftest builder-elpa-test-local-preview-server-and-install ()
  "Test local HTTP preview server, package-archives fetching, and installation."
  (builder-elpa-test-with-temp-env
   (let* ((pkg-dir (expand-file-name "preview-pkg" temp-dir))
          (test-port 18889))
     (builder-elpa-test-create-dummy-pkg pkg-dir "preview-pkg" "1.0.0" "Preview Server Test")
     (builder-elpa-register-package 'preview-pkg pkg-dir)
     (builder-elpa-build-all 'all)

     (unwind-protect
         (progn
           (builder-elpa-serve-local test-port builder-elpa-output-dir)
           (should (process-live-p builder-elpa-server-process))

           ;; Verify HTTP request to preview server
           (let ((url-buf (url-retrieve-synchronously (format "http://127.0.0.1:%d/elpaish/archive-contents" test-port) t t 5)))
             (should url-buf)
             (with-current-buffer url-buf
               (goto-char (point-min))
               (should (search-forward "200 OK" nil t))
               (should (search-forward "preview-pkg" nil t))
               (kill-buffer url-buf))))
       (builder-elpa-stop-server)))))
(ert-deftest builder-elpa-test-gpg-signing-pipeline ()
  "Test GPG signing key/passphrase resolution and signature generation."
  (builder-elpa-test-with-temp-env
   (let ((builder-elpa-sign-packages t)
         (builder-elpa-gpg-key "TESTKEY123")
         (builder-elpa-gpg-passphrase "SECRET123"))
     (should (equal (builder-elpa--get-signing-key) "TESTKEY123"))
     (should (equal (builder-elpa--get-signing-passphrase) "SECRET123"))

     ;; Test env fallback when custom vars are nil
     (let ((builder-elpa-gpg-key nil)
           (builder-elpa-gpg-passphrase nil))
       (setenv "ELPAISH_SIGNING_KEY" "ENVKEY456")
       (setenv "ELPAISH_GPG_PASSPHRASE" "ENVPASS456")
       (unwind-protect
           (progn
             (should (equal (builder-elpa--get-signing-key) "ENVKEY456"))
             (should (equal (builder-elpa--get-signing-passphrase) "ENVPASS456")))
         (setenv "ELPAISH_SIGNING_KEY" nil)
         (setenv "ELPAISH_GPG_PASSPHRASE" nil)))

     ;; Test mock signing execution
     (let ((dummy-file (expand-file-name "test.el" temp-dir)))
       (with-temp-file dummy-file (insert ";; test"))
       (cl-letf (((symbol-function 'builder-elpa--sign-with-gpg-cli)
                  (lambda (file sig-file _key _pass)
                    (with-temp-file sig-file (insert "MOCK SIG"))
                    0)))
         (builder-elpa--sign-file dummy-file)
         (should (file-exists-p (concat dummy-file ".sig"))))))))

(ert-deftest builder-elpa-test-key-rotation-and-revocation ()
  "Test subkey rotation and emergency revocation certificate generation."
  (builder-elpa-test-with-temp-env
   (let ((gpg-bin (executable-find "gpg")))
     (if (not gpg-bin)
         (ert-skip "GPG binary not found in PATH")
       (cl-letf (((symbol-function 'call-process)
                  (lambda (_program &optional _infile _destination _display &rest args)
                    (when (member "--gen-revoke" args)
                      (let ((out-file (cadr (member "--output" args))))
                        (when out-file
                          (make-directory (file-name-directory out-file) t)
                          (with-temp-file out-file (insert "MOCK REV")))))
                    0))
                 ((symbol-function 'call-process-region) (lambda (&rest _) 0)))
         ;; Test rotation
         (builder-elpa-rotate-keys :master-key-id "MASTERKEY" :output-dir builder-elpa-output-dir)
         ;; Test revocation publishing
         (builder-elpa-revoke-key "MASTERKEY" builder-elpa-output-dir)
         (should (file-exists-p (expand-file-name "elpaish.rev.asc" builder-elpa-output-dir))))))))

(ert-deftest builder-elpa-test-preflight-skip-options ()
  "Test preflight gate skipping specific checks and skipping all."
  (builder-elpa-test-with-temp-env
   (let ((pkg-dir (expand-file-name "skip-pkg" temp-dir)))
     (builder-elpa-test-create-dummy-pkg pkg-dir "skip-pkg" "1.0.0" "Skip Test")
     ;; Add invalid docstring to trigger checkdoc warning
     (with-temp-buffer
       (insert ";;; skip-pkg.el --- invalid checkdoc -*- lexical-binding: t; -*-\n\n(defun skip-pkg-foo ()\n  \"Missing period in docstring\"\n  nil)\n\n(provide 'skip-pkg)\n;;; skip-pkg.el ends here\n")
       (write-region (point-min) (point-max) (expand-file-name "skip-pkg.el" pkg-dir)))

     ;; Test skipping checkdoc & package-lint explicitly
     (let ((res (run-checks-package pkg-dir :skip-checks '(checkdoc package-lint))))
       (should (plist-get res :passed)))

     ;; Test skipping all checks
     (let ((res (run-checks-package pkg-dir :skip-checks t)))
       (should (plist-get res :passed))))))
(ert-deftest builder-elpa-test-staging-version-edge-cases ()
  "Test edge cases in version normalization and staging version derivation."
  (should (equal (builder-elpa--normalize-staging-version "v1.2.0-4-gabcdef") "1.2.0.4"))
  (should (equal (builder-elpa--normalize-staging-version "1.2.0-rc.1") "1.2.0.rc1"))
  (should (version-to-list (builder-elpa--normalize-staging-version "v1.2.0-4-gabcdef")))
  (should (version-to-list (builder-elpa--normalize-staging-version "1.2.0-rc.1"))))

(ert-deftest builder-elpa-test-http-server-edge-cases ()
  "Test HTTP preview server MIME types and 404/400/HEAD responses."
  (should (equal (builder-elpa--http-mime-type "foo.sig") "application/pgp-signature"))
  (should (equal (builder-elpa--http-mime-type "foo.asc") "application/pgp-keys"))
  (should (equal (builder-elpa--http-mime-type "foo.gpg") "application/pgp-keys"))
  (should (equal (builder-elpa--http-mime-type "foo.tar") "application/x-tar"))
  (should (equal (builder-elpa--http-mime-type "foo.html") "text/html; charset=utf-8"))

  (builder-elpa-test-with-temp-env
   (let ((doc-root temp-dir))
     (with-temp-file (expand-file-name "test.html" doc-root) (insert "<h1>Hello</h1>"))

     ;; 200 OK
     (let ((res (builder-elpa--handle-http-request "GET /test.html HTTP/1.1\r\n" doc-root)))
       (should (string-prefix-p "HTTP/1.1 200 OK" (car res)))
       (should (equal (cdr res) "<h1>Hello</h1>")))

     ;; HEAD request (200 OK headers, empty body)
     (let ((res (builder-elpa--handle-http-request "HEAD /test.html HTTP/1.1\r\n" doc-root)))
       (should (string-prefix-p "HTTP/1.1 200 OK" (car res)))
       (should (equal (cdr res) "")))

     ;; 404 Not Found
     (let ((res (builder-elpa--handle-http-request "GET /nonexistent.el HTTP/1.1\r\n" doc-root)))
       (should (string-prefix-p "HTTP/1.1 404 Not Found" (car res))))

     ;; 400 Bad Request
     (let ((res (builder-elpa--handle-http-request "POST /test.html HTTP/1.1\r\n" doc-root)))
       (should (string-prefix-p "HTTP/1.1 400 Bad Request" (car res)))))))

(ert-deftest builder-elpa-test-stable-track-omission ()
  "Test omitting packages without clean semver tag from stable track."
  (builder-elpa-test-with-temp-env
   (let ((pkg-dir (expand-file-name "untagged-pkg" temp-dir)))
     (builder-elpa-test-create-dummy-pkg pkg-dir "untagged-pkg" "1.0.0" "Untagged Test")
     (let ((recipe (builder-elpa-register-package 'untagged-pkg pkg-dir)))
       ;; No clean semver git tag present
       (should-not (builder-elpa-derive-version recipe 'elpaish-stable))
       (should-not (builder-elpa-build-package recipe 'elpaish-stable))
       (let ((ac-file (builder-elpa-generate-archive-contents 'elpaish-stable)))
         (with-temp-buffer
           (insert-file-contents ac-file)
           (let ((data (read (current-buffer))))
             (should-not (assoc 'untagged-pkg (cdr data))))))))))

(ert-deftest builder-elpa-test-pkg-descriptor-generation ()
  "Test multi-file file collection and <pkg>-pkg.el descriptor generation."
  (builder-elpa-test-with-temp-env
   (let ((pkg-dir (expand-file-name "desc-pkg" temp-dir)))
     (make-directory pkg-dir t)
     (with-temp-file (expand-file-name "desc-pkg.el" pkg-dir) (insert ";;; desc-pkg.el -- Desc -*- lexical-binding: t -*-"))
     (with-temp-file (expand-file-name "desc-pkg-aux.el" pkg-dir) (insert ";;; desc-pkg-aux.el -- Aux -*- lexical-binding: t -*-"))
     (with-temp-file (expand-file-name "desc-pkg-tests.el" pkg-dir) (insert ";;; desc-pkg-tests.el -- Test -*- lexical-binding: t -*-"))

     ;; Verify file collection excludes test file
     (let ((files (builder-elpa--collect-files pkg-dir '("*.el") "desc-pkg")))
       (should (member "desc-pkg.el" files))
       (should (member "desc-pkg-aux.el" files))
       (should-not (member "desc-pkg-tests.el" files)))

     ;; Verify descriptor generation
     (let ((dest (expand-file-name "desc-pkg-pkg.el" temp-dir)))
       (builder-elpa--generate-pkg-file dest "desc-pkg" "1.2.3" "Desc Summary" '((emacs "27.1")) "https://example.com" '("tools"))
       (should (file-exists-p dest))
       (with-temp-buffer
         (insert-file-contents dest)
         (should (search-forward "define-package" nil t))
         (should (search-forward "desc-pkg" nil t))
         (should (search-forward "1.2.3" nil t)))))))

(provide 'test-builder-elpa)
;;; test-builder-elpa.el ends here
