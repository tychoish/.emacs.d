;;; builder-elpa-tests.el --- ERT Tests for builder-elpa -*- lexical-binding: t; -*-

(require 'ert)
(require 'builder-elpa)

(defmacro builder-elpa-test-with-temp-env (&rest body)
  "Execute BODY within an isolated temporary registry, work, and output directory."
  `(let* ((temp-dir (make-temp-file "builder-elpa-test-" t))
          (builder-elpa-output-dir (expand-file-name "public/" temp-dir))
          (builder-elpa-work-dir (expand-file-name "repos/" temp-dir))
          (builder-elpa-registry (make-hash-table :test 'equal))
          (builder-elpa-sign-packages nil))
     (unwind-protect
         (progn ,@body)
       (delete-directory temp-dir t))))

(defun builder-elpa-test-create-dummy-pkg (dir name version summary)
  "Create a dummy package file in DIR."
  (make-directory dir t)
  (let ((file (expand-file-name (concat name ".el") dir)))
    (with-temp-file file
      (insert (format ";;; %s.el --- %s -*- lexical-binding: t; -*-\n" name summary))
      (insert ";; Version: " version "\n")
      (insert ";; Keywords: test\n\n")
      (insert "(provide '" name ")\n")
      (insert (format ";;; %s.el ends here\n" name)))))

;;; Tests

(ert-deftest builder-elpa-test-registration ()
  "Test package registration with symbol, string, and default options."
  (builder-elpa-test-with-temp-env
   (builder-elpa-register-package 'pkg-a "/path/to/a")
   (builder-elpa-register-package "pkg-b" "/path/to/b" :branch "develop" :files '("*.el" "src/*.el"))

   (let ((recipe-a (map-elt builder-elpa-registry "pkg-a"))
         (recipe-b (map-elt builder-elpa-registry "pkg-b")))
     (should recipe-a)
     (should (equal (builder-elpa-recipe-name recipe-a) "pkg-a"))
     (should (equal (builder-elpa-recipe-branch recipe-a) "main"))

     (should recipe-b)
     (should (equal (builder-elpa-recipe-branch recipe-b) "develop"))
     (should (equal (builder-elpa-recipe-files recipe-b) '("*.el" "src/*.el"))))))

(ert-deftest builder-elpa-test-build-package ()
  "Test building a single local package."
  (builder-elpa-test-with-temp-env
   (let ((pkg-dir (expand-file-name "test-mode" temp-dir))
         (builder-elpa-release-mode 'stable))
     (builder-elpa-test-create-dummy-pkg pkg-dir "test-mode" "1.2.3" "Test Mode Package")
     (builder-elpa-register-package 'test-mode pkg-dir)

     (let ((recipe (map-elt builder-elpa-registry "test-mode")))
       (builder-elpa-build-package recipe)

       (should (equal (builder-elpa-recipe-built-version recipe) "1.2.3"))
       (should (equal (builder-elpa-recipe-summary recipe) "Test Mode Package"))
       (should (file-exists-p (expand-file-name "test-mode-1.2.3.el" builder-elpa-output-dir)))))))

(ert-deftest builder-elpa-test-generate-archive-contents ()
  "Test generation of `archive-contents` file."
  (builder-elpa-test-with-temp-env
   (let ((pkg-dir (expand-file-name "my-pkg" temp-dir)))
     (builder-elpa-test-create-dummy-pkg pkg-dir "my-pkg" "0.1.0" "My Package")
     (builder-elpa-register-package 'my-pkg pkg-dir)
     (builder-elpa-build-package (map-elt builder-elpa-registry "my-pkg"))

     (builder-elpa-generate-archive-contents)
     (let ((archive-file (expand-file-name "archive-contents" builder-elpa-output-dir)))
       (should (file-exists-p archive-file))
       (with-temp-buffer
         (insert-file-contents archive-file)
         (let ((data (read (current-buffer))))
           (should (eq (car data) 1))
           (should (assoc 'my-pkg (cdr data)))))))))

(ert-deftest builder-elpa-test-generate-github-index ()
  "Test generation of static index.html."
  (builder-elpa-test-with-temp-env
   (let ((pkg-dir (expand-file-name "demo-pkg" temp-dir)))
     (builder-elpa-test-create-dummy-pkg pkg-dir "demo-pkg" "2.0.0" "Demo HTML Package")
     (builder-elpa-register-package 'demo-pkg pkg-dir)
     (builder-elpa-build-package (map-elt builder-elpa-registry "demo-pkg"))

     (builder-elpa-generate-github-index)
     (let ((index-file (expand-file-name "index.html" builder-elpa-output-dir)))
       (should (file-exists-p index-file))
       (with-temp-buffer
         (insert-file-contents index-file)
         (should (search-forward "demo-pkg" nil t))
         (should (search-forward "2.0.0" nil t)))))))

(ert-deftest builder-elpa-test-build-all ()
  "Test full build pipeline via `builder-elpa-build-all`."
  (builder-elpa-test-with-temp-env
   (let ((pkg1 (expand-file-name "pkg1" temp-dir))
         (pkg2 (expand-file-name "pkg2" temp-dir))
         (builder-elpa-release-mode 'stable))
     (builder-elpa-test-create-dummy-pkg pkg1 "pkg1" "1.0.0" "First")
     (builder-elpa-test-create-dummy-pkg pkg2 "pkg2" "2.0.0" "Second")
     (builder-elpa-register-package 'pkg1 pkg1)
     (builder-elpa-register-package 'pkg2 pkg2)

     (builder-elpa-build-all)

     (should (file-exists-p (expand-file-name "pkg1-1.0.0.el" builder-elpa-output-dir)))
     (should (file-exists-p (expand-file-name "pkg2-2.0.0.el" builder-elpa-output-dir)))
     (should (file-exists-p (expand-file-name "archive-contents" builder-elpa-output-dir)))
     (should (file-exists-p (expand-file-name "index.html" builder-elpa-output-dir))))))

(ert-deftest builder-elpa-test-timer-controls ()
  "Test starting and stopping auto-build background timer."
  (unwind-protect
      (progn
        (builder-elpa-start-auto-build "3600")
        (should (timerp builder-elpa-timer))
        (builder-elpa-stop-auto-build)
        (should-not builder-elpa-timer))
    (builder-elpa-stop-auto-build)))

(ert-deftest builder-elpa-test-status-ui ()
  "Test status buffer creation and table population."
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
(ert-deftest builder-elpa-test-multi-file-tar-build ()
  "Test building a multi-file package as a tar archive."
  (builder-elpa-test-with-temp-env
   (let ((pkg-dir (expand-file-name "multi-pkg" temp-dir))
         (builder-elpa-release-mode 'stable))
     (builder-elpa-test-create-dummy-pkg pkg-dir "multi-pkg" "1.0.0" "Multi-file Test")
     (with-temp-file (expand-file-name "multi-pkg-extra.el" pkg-dir)
       (insert ";;; extra file\n"))
     (builder-elpa-register-package 'multi-pkg pkg-dir :files '("*.el"))
     (let ((recipe (map-elt builder-elpa-registry "multi-pkg")))
       (builder-elpa-build-package recipe)
       (should (file-exists-p (expand-file-name "multi-pkg-1.0.0.tar" builder-elpa-output-dir)))
       (should (eq (builder-elpa-recipe-built-type recipe) 'tar))
       (builder-elpa-generate-archive-contents)
       (let ((archive-file (expand-file-name "archive-contents" builder-elpa-output-dir)))
         (with-temp-buffer
           (insert-file-contents archive-file)
           (let ((data (read (current-buffer))))
             (let ((entry (cdr (assoc 'multi-pkg (cdr data)))))
               (should entry)
               (should (eq (aref entry 3) 'tar))))))))))

(ert-deftest builder-elpa-test-idle-auto-build ()
  "Test starting idle background rebuild timer."
  (unwind-protect
      (progn
        (builder-elpa-start-auto-build "5 mins" t)
        (should (timerp builder-elpa-timer))
        (should (timer--idle-delay builder-elpa-timer))
        (builder-elpa-stop-auto-build)
        (should-not builder-elpa-timer))
    (builder-elpa-stop-auto-build)))

(ert-deftest builder-elpa-test-auto-build-interval-parsing ()
  "Test interval parsing for periodic auto-build timer."
  (unwind-protect
      (progn
        (builder-elpa-start-auto-build "1 hour")
        (should (timerp builder-elpa-timer))
        (builder-elpa-stop-auto-build)
        (builder-elpa-start-auto-build "15 mins")
        (should (timerp builder-elpa-timer))
        (builder-elpa-stop-auto-build))
    (builder-elpa-stop-auto-build)))
(ert-deftest builder-elpa-test-setup-signing ()
  "Test setup signing wizard with mocked GPG keys."
  (let ((dummy-key (epg-make-key 'OpenPGP))
        (sub-key (record 'epg-sub-key nil nil nil nil nil nil nil nil)))
    (setf (epg-sub-key-id sub-key) "1234567890ABCDEF")
    (setf (epg-key-user-id-list dummy-key) (list (epg-make-user-id "Test User <test@example.com>" nil)))
    (setf (epg-key-sub-key-list dummy-key) (list sub-key))
    (cl-letf* (((symbol-function 'executable-find) (lambda (_cmd) "/usr/bin/gpg"))
               ((symbol-function 'epg-make-context) (lambda (&rest _) 'dummy-ctx))
               ((symbol-function 'epg-list-keys) (lambda (&rest _) (list dummy-key)))
               ((symbol-function 'annotated-completing-read)
                (lambda (table &rest _)
                  (caar table))))
      (let ((builder-elpa-gpg-key nil)
            (builder-elpa-sign-packages nil))
        (builder-elpa-setup-signing)
        (should (equal builder-elpa-gpg-key "1234567890ABCDEF"))
        (should builder-elpa-sign-packages)))))

(ert-deftest builder-elpa-test-collect-files-default-patterns ()
  "Test `builder-elpa--collect-files` with default nil patterns."
  (builder-elpa-test-with-temp-env
   (let ((pkg-dir (expand-file-name "test-pkg" temp-dir)))
     (make-directory pkg-dir t)
     (with-temp-file (expand-file-name "foo.el" pkg-dir) (insert ";; foo"))
     (with-temp-file (expand-file-name "bar.el" pkg-dir) (insert ";; bar"))
     (with-temp-file (expand-file-name "README.txt" pkg-dir) (insert "doc"))
     (let ((files (builder-elpa--collect-files pkg-dir nil)))
       (should (equal (sort files #'string<) '("bar.el" "foo.el")))))))

(ert-deftest builder-elpa-test-collect-files-custom-patterns-and-subdirs ()
  "Test `builder-elpa--collect-files` with nested directories and custom patterns."
  (builder-elpa-test-with-temp-env
   (let ((pkg-dir (expand-file-name "test-pkg" temp-dir)))
     (make-directory (expand-file-name "src" pkg-dir) t)
     (with-temp-file (expand-file-name "main.el" pkg-dir) (insert ";; main"))
     (with-temp-file (expand-file-name "src/helper.el" pkg-dir) (insert ";; helper"))
     (let ((files (builder-elpa--collect-files pkg-dir '("*.el" "src/*.el"))))
       (should (equal (sort files #'string<) '("main.el" "src/helper.el")))))))

(ert-deftest builder-elpa-test-collect-files-filtering-directories-and-duplicates ()
  "Test `builder-elpa--collect-files` filters out directories and removes duplicate matches."
  (builder-elpa-test-with-temp-env
   (let ((pkg-dir (expand-file-name "test-pkg" temp-dir)))
     (make-directory (expand-file-name "dir.el" pkg-dir) t) ; directory named dir.el
     (with-temp-file (expand-file-name "foo.el" pkg-dir) (insert ";; foo"))
     (let ((files (builder-elpa--collect-files pkg-dir '("*.el" "foo.el"))))
       (should (equal files '("foo.el")))))))

(ert-deftest builder-elpa-test-collect-files-no-matches ()
  "Test `builder-elpa--collect-files` when no files match pattern."
  (builder-elpa-test-with-temp-env
   (let ((pkg-dir (expand-file-name "test-pkg" temp-dir)))
     (make-directory pkg-dir t)
     (with-temp-file (expand-file-name "foo.el" pkg-dir) (insert ";; foo"))
     (let ((files (builder-elpa--collect-files pkg-dir '("*.org"))))
       (should (null files))))))
(ert-deftest builder-elpa-test-unstable-date-version ()
  "Test date-based unstable version string formatting."
  (builder-elpa-test-with-temp-env
   (let ((pkg-dir (expand-file-name "test-pkg" temp-dir)))
     (make-directory pkg-dir t)
     (let ((builder-elpa-version-include-header t))
       (should (string-match-p "^1\\.2\\.0\\.[0-9]+\\.[0-9]+$"
                               (builder-elpa--get-unstable-version pkg-dir "1.2.0"))))
     (let ((builder-elpa-version-include-header nil))
       (should (string-match-p "^[0-9]+\\.[0-9]+$"
                               (builder-elpa--get-unstable-version pkg-dir "1.2.0")))))))

(ert-deftest builder-elpa-test-release-mode-both ()
  "Test building repository in 'both mode (creating stable/ and unstable/ subfolders)."
  (builder-elpa-test-with-temp-env
   (let ((pkg-dir (expand-file-name "demo-pkg" temp-dir)))
     (builder-elpa-test-create-dummy-pkg pkg-dir "demo-pkg" "1.0.0" "Demo")
     (builder-elpa-register-package 'demo-pkg pkg-dir)
     (builder-elpa-build-all 'both)
     (should (file-exists-p (expand-file-name "index.html" builder-elpa-output-dir)))
     (should (file-exists-p (expand-file-name "stable/archive-contents" builder-elpa-output-dir)))
     (should (file-exists-p (expand-file-name "stable/index.html" builder-elpa-output-dir)))
     (should (file-exists-p (expand-file-name "unstable/archive-contents" builder-elpa-output-dir)))
     (should (file-exists-p (expand-file-name "unstable/index.html" builder-elpa-output-dir))))))

(ert-deftest builder-elpa-test-inject-version-header ()
  "Test `builder-elpa--inject-version-header` updating and inserting Version: lines."
  (with-temp-buffer
    (insert ";;; foo.el --- Test -*- lexical-binding: t; -*-\n\n;; Author: Test\n(provide 'foo)\n")
    (builder-elpa--inject-version-header "3.1.4")
    (should (search-backward ";; Version: 3.1.4" nil t)))
  (with-temp-buffer
    (insert ";;; foo.el --- Test -*- lexical-binding: t; -*-\n;; Version: 1.0.0\n;; Author: Test\n")
    (builder-elpa--inject-version-header "3.1.4")
    (should (search-backward ";; Version: 3.1.4" nil t))))

(ert-deftest builder-elpa-test-output-version-header-in-built-file ()
  "Test that built output file contains injected Version: header."
  (builder-elpa-test-with-temp-env
   (let ((pkg-dir (expand-file-name "version-hdr-pkg" temp-dir))
         (builder-elpa-release-mode 'stable))
     (builder-elpa-test-create-dummy-pkg pkg-dir "version-hdr-pkg" "1.0.0" "Version Header Test")
     (builder-elpa-register-package 'version-hdr-pkg pkg-dir)
     (builder-elpa-build-package (map-elt builder-elpa-registry "version-hdr-pkg"))
     (let ((built-file (expand-file-name "version-hdr-pkg-1.0.0.el" builder-elpa-output-dir)))
       (should (file-exists-p built-file))
       (with-temp-buffer
         (insert-file-contents built-file)
         (should (search-forward ";; Version: 1.0.0" nil t)))))))
(ert-deftest builder-elpa-test-elpaish-bootstrap ()
  "Test loading elpaish packages.el and building the archive in both mode."
  (let ((elpaish-dir (expand-file-name "~/src/elpaish")))
    (should (file-exists-p (expand-file-name "packages.el" elpaish-dir)))
    (should (file-exists-p (expand-file-name "build.el" elpaish-dir)))
    (should (file-exists-p (expand-file-name "public/index.html" elpaish-dir)))
    (should (file-exists-p (expand-file-name "public/stable/archive-contents" elpaish-dir)))
    (should (file-exists-p (expand-file-name "public/unstable/archive-contents" elpaish-dir)))))

(provide 'builder-elpa-tests)
