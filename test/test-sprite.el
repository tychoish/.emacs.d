;;; test-sprite.el --- ERT tests for sprite.el -*- lexical-binding: t -*-

;; Run inside a live Emacs session:
;;   (ert "^sprite/")
;;
;; Batch run:
;;   emacs --batch -L ~/.emacs.d/lisp \
;;     --eval '(progn (setq package-user-dir "~/.emacs.d/elpa") (package-initialize))' \
;;     -l ~/.emacs.d/test/test-sprite.el \
;;     --eval '(ert-run-tests-batch-and-exit "sprite/")'

;;; Commentary:
;;
;; Unit tests for sprite.el.  All tests that touch the registry or
;; filesystem use let-bindings or temp dirs so they are fully isolated.
;;

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'map)

;; Stub tychoish functions before loading sprite so declarations resolve.
(unless (fboundp 'tychoish/conf-state-path)
  (defun tychoish/conf-state-path (name)
    (expand-file-name (concat "state/" name) temporary-file-directory)))

(unless (fboundp 'tychoish/resolve-instance-id)
  (defun tychoish/resolve-instance-id () "work"))

(require 'sprite)

;;;; Helpers

(defmacro sprite-test/with-registry (&rest body)
  "Run BODY with a fresh, isolated sprite registry."
  `(let ((sprite--registry (make-hash-table :test #'equal)))
     ,@body))

(defmacro sprite-test/with-temp-state-dir (&rest body)
  "Run BODY with sprite-state-directory redirected to a temp directory."
  `(let ((tmpdir (make-temp-file "sprite-test-" t)))
     (unwind-protect
         (cl-letf (((symbol-function 'sprite-state-directory)
                    (lambda (&optional full-name)
                      (if full-name
                          (file-name-concat tmpdir full-name)
                        (file-name-as-directory tmpdir)))))
           ,@body)
       (delete-directory tmpdir t))))

;;;; Identity: sprite--format-full-name

(ert-deftest sprite/format-full-name-basic ()
  (should (equal "work.0.render" (sprite--format-full-name "work" 0 "render"))))

(ert-deftest sprite/format-full-name-idx-increments ()
  (should (equal "work.3.render" (sprite--format-full-name "work" 3 "render"))))

(ert-deftest sprite/format-full-name-parent-variety ()
  (should (equal "solo.1.analysis" (sprite--format-full-name "solo" 1 "analysis"))))

;;;; Identity: sprite--parse-full-name

(ert-deftest sprite/parse-full-name-valid ()
  (should (equal '("work" 0 "render") (sprite--parse-full-name "work.0.render"))))

(ert-deftest sprite/parse-full-name-idx-preserved ()
  (should (equal '("work" 3 "analysis") (sprite--parse-full-name "work.3.analysis"))))

(ert-deftest sprite/parse-full-name-no-dots-returns-nil ()
  (should (null (sprite--parse-full-name "notaname"))))

(ert-deftest sprite/parse-full-name-only-one-dot-returns-nil ()
  (should (null (sprite--parse-full-name "work.render"))))

(ert-deftest sprite/parse-full-name-nonnumeric-idx-returns-nil ()
  (should (null (sprite--parse-full-name "work.abc.render"))))

(ert-deftest sprite/parse-full-name-too-many-parts-returns-nil ()
  "Names with more than one dot in the unique-name portion are still valid."
  ;; Pattern allows any non-dot prefix for the unique-name segment with 2+ chars
  (should (null (sprite--parse-full-name "a.1.b.c.d"))))

;;;; Identity: sprite--full-name-p

(ert-deftest sprite/full-name-p-true-for-valid ()
  (should (sprite--full-name-p "work.0.render"))
  (should (sprite--full-name-p "solo.12.analysis")))

(ert-deftest sprite/full-name-p-false-for-top-level ()
  (should-not (sprite--full-name-p "work"))
  (should-not (sprite--full-name-p "solo")))

(ert-deftest sprite/full-name-p-false-for-partial ()
  (should-not (sprite--full-name-p "work.render"))
  (should-not (sprite--full-name-p "work.abc.render")))

(ert-deftest sprite/full-name-p-false-for-empty ()
  (should-not (sprite--full-name-p ""))
  (should-not (sprite--full-name-p nil)))

;;;; Identity: sprite--parent-letter

(ert-deftest sprite/parent-letter-returns-first-char ()
  (should (equal "w" (sprite--parent-letter "work")))
  (should (equal "s" (sprite--parent-letter "solo")))
  (should (equal "p" (sprite--parent-letter "primary"))))

;;;; Identity: sprite--mode-line-id

(ert-deftest sprite/mode-line-id-sprite-abbreviates-parent ()
  (should (equal "w.0.render" (sprite--mode-line-id "work.0.render"))))

(ert-deftest sprite/mode-line-id-top-level-unchanged ()
  (should (equal "work" (sprite--mode-line-id "work")))
  (should (equal "solo" (sprite--mode-line-id "solo"))))

(ert-deftest sprite/mode-line-id-higher-idx ()
  (should (equal "s.5.analysis" (sprite--mode-line-id "solo.5.analysis"))))

;;;; Identity: sprite--mode-line-string

(ert-deftest sprite/mode-line-string-nil-for-top-level ()
  (cl-letf (((symbol-function 'tychoish/resolve-instance-id) (lambda () "work")))
    (should (null (sprite--mode-line-string)))))

(ert-deftest sprite/mode-line-string-abbreviated-for-sprite ()
  (cl-letf (((symbol-function 'tychoish/resolve-instance-id)
             (lambda () "work.0.render")))
    (should (equal "w.0.render" (sprite--mode-line-string)))))

;;;; Registry

(ert-deftest sprite/registry-put-and-get ()
  (sprite-test/with-registry
    (let ((s (sprite--make :name "work.0.test" :idx 0 :parent "work")))
      (sprite--registry-put s)
      (should (equal s (sprite--registry-get "work.0.test"))))))

(ert-deftest sprite/registry-get-missing-returns-nil ()
  (sprite-test/with-registry
    (should (null (sprite--registry-get "nonexistent")))))

(ert-deftest sprite/registry-all-returns-list ()
  (sprite-test/with-registry
    (should (null (sprite--registry-all)))
    (sprite--registry-put (sprite--make :name "work.0.a" :idx 0 :parent "work"))
    (should (= 1 (length (sprite--registry-all))))))

(ert-deftest sprite/registry-remove ()
  (sprite-test/with-registry
    (sprite--registry-put (sprite--make :name "work.0.test" :idx 0 :parent "work"))
    (sprite--registry-remove "work.0.test")
    (should (null (sprite--registry-get "work.0.test")))))

(ert-deftest sprite/registry-multiple-entries ()
  (sprite-test/with-registry
    (sprite--registry-put (sprite--make :name "work.0.a" :idx 0 :parent "work"))
    (sprite--registry-put (sprite--make :name "work.1.b" :idx 1 :parent "work"))
    (should (= 2 (length (sprite--registry-all))))))

;;;; Serialization

(ert-deftest sprite/serialize-deserialize-roundtrip ()
  "Serializing and deserializing preserves all registry entries."
  (sprite-test/with-registry
    (let ((s1 (sprite--make :name "work.0.a" :idx 0 :parent "work" :unique-name "a"))
          (s2 (sprite--make :name "work.1.b" :idx 1 :parent "work" :unique-name "b"))
          (sprite--registry-saved nil))
      (sprite--registry-put s1)
      (sprite--registry-put s2)
      (sprite--registry-serialize)
      (should (= 2 (length sprite--registry-saved)))
      ;; Restore into a fresh registry
      (clrhash sprite--registry)
      (sprite--registry-deserialize)
      (let ((restored (sprite--registry-get "work.0.a")))
        (should (sprite-p restored))
        (should (equal "work.0.a" (sprite-name restored)))
        (should (= 0 (sprite-idx restored))))
      (should (sprite--registry-get "work.1.b")))))

(ert-deftest sprite/serialize-empty-registry ()
  (sprite-test/with-registry
    (let ((sprite--registry-saved nil))
      (sprite--registry-serialize)
      (should (null sprite--registry-saved)))))

;;;; State discovery

(ert-deftest sprite/discover-state-dirs-returns-valid-names ()
  (sprite-test/with-temp-state-dir
    (make-directory (file-name-concat (sprite-state-directory) "work.0.render"))
    (make-directory (file-name-concat (sprite-state-directory) "work.1.analysis"))
    (make-directory (file-name-concat (sprite-state-directory) "not-a-sprite-name"))
    (let ((found (sprite--discover-state-dirs)))
      (should (member "work.0.render" found))
      (should (member "work.1.analysis" found))
      (should-not (member "not-a-sprite-name" found)))))

(ert-deftest sprite/discover-state-dirs-empty-when-no-dir ()
  (cl-letf (((symbol-function 'sprite-state-directory)
             (lambda (&optional _) "/nonexistent-sprite-state-dir/")))
    (should (null (sprite--discover-state-dirs)))))

(ert-deftest sprite/decommissioned-p-no-file ()
  (sprite-test/with-temp-state-dir
    (make-directory (file-name-concat (sprite-state-directory) "work.0.render"))
    (should-not (sprite--decommissioned-p "work.0.render"))))

(ert-deftest sprite/decommissioned-p-file-present ()
  (sprite-test/with-temp-state-dir
    (let ((dir (file-name-concat (sprite-state-directory) "work.0.render")))
      (make-directory dir)
      (write-region "" nil (file-name-concat dir "DECOMMISSIONED"))
      (should (sprite--decommissioned-p "work.0.render")))))

(ert-deftest sprite/write-decommissioned-file-creates-marker ()
  (sprite-test/with-temp-state-dir
    (make-directory (file-name-concat (sprite-state-directory) "work.0.render"))
    (should (sprite--write-decommissioned-file "work.0.render"))
    (should (sprite--decommissioned-p "work.0.render"))))

;;;; Index allocation

(ert-deftest sprite/next-idx-empty-parent ()
  (cl-letf (((symbol-function 'sprite--allocated-indices) (lambda (_) nil)))
    (should (= 0 (sprite--next-idx "work")))))

(ert-deftest sprite/next-idx-fills-gap ()
  (cl-letf (((symbol-function 'sprite--allocated-indices) (lambda (_) '(0 2))))
    (should (= 1 (sprite--next-idx "work")))))

(ert-deftest sprite/next-idx-sequential ()
  (cl-letf (((symbol-function 'sprite--allocated-indices) (lambda (_) '(0 1 2))))
    (should (= 3 (sprite--next-idx "work")))))

(ert-deftest sprite/allocated-indices-excludes-decommissioned ()
  "Indices from decommissioned sprites are not returned."
  (sprite-test/with-temp-state-dir
    (let ((active-dir (file-name-concat (sprite-state-directory) "work.0.active"))
          (decomm-dir (file-name-concat (sprite-state-directory) "work.1.gone")))
      (make-directory active-dir)
      (make-directory decomm-dir)
      (write-region "" nil (file-name-concat decomm-dir "DECOMMISSIONED"))
      (let ((indices (sprite--allocated-indices "work")))
        (should (member 0 indices))
        (should-not (member 1 indices))))))

;;;; Resolve name

(ert-deftest sprite/resolve-name-full-name-passthrough ()
  (sprite-test/with-registry
    (sprite--registry-put (sprite--make :name "work.0.foo" :idx 0
                                        :parent "work" :unique-name "foo"))
    (should (equal "work.0.foo" (sprite--resolve-name "work.0.foo")))))

(ert-deftest sprite/resolve-name-unique-name-lookup ()
  (sprite-test/with-registry
    (sprite--registry-put (sprite--make :name "work.0.foo" :idx 0
                                        :parent "work" :unique-name "foo"))
    (should (equal "work.0.foo" (sprite--resolve-name "foo")))))

(ert-deftest sprite/resolve-name-error-on-unknown ()
  (sprite-test/with-registry
    (should-error (sprite--resolve-name "unknown") :type 'user-error)))

;;;; Communication

(ert-deftest sprite/log-buffer-name-format ()
  (should (equal "*sprite:work.0.render*"
                 (sprite--log-buffer-name "work.0.render"))))

(ert-deftest sprite/log-creates-buffer ()
  (let ((buf-name (sprite--log-buffer-name "work.0.test-log-creates")))
    (when (get-buffer buf-name)
      (kill-buffer buf-name))
    (unwind-protect
        (progn
          (sprite--log "work.0.test-log-creates" 'sent "hello")
          (should (get-buffer buf-name)))
      (when (get-buffer buf-name)
        (kill-buffer buf-name)))))

(ert-deftest sprite/log-records-direction-and-content ()
  (let ((buf-name (sprite--log-buffer-name "work.0.test-log-content")))
    (when (get-buffer buf-name)
      (kill-buffer buf-name))
    (unwind-protect
        (progn
          (sprite--log "work.0.test-log-content" 'sent "my-form")
          (let ((text (with-current-buffer buf-name (buffer-string))))
            (should (string-match-p "sent" text))
            (should (string-match-p "my-form" text))))
      (when (get-buffer buf-name)
        (kill-buffer buf-name)))))

(ert-deftest sprite/with-sprite-binds-env ()
  "with-sprite sets EMACS_SERVER_FILE in process-environment."
  (cl-letf (((symbol-function 'sprite--server-file) (lambda (n) n)))
    (let ((captured nil))
      (with-sprite "work.0.render"
        (setq captured (getenv "EMACS_SERVER_FILE")))
      (should (equal "work.0.render" captured)))))

(ert-deftest sprite/with-sprite-returns-body-value ()
  (cl-letf (((symbol-function 'sprite--server-file) (lambda (n) n)))
    (should (= 42 (with-sprite "work.0.render"
                    (+ 1 41))))))

(ert-deftest sprite/with-sprite-does-not-pollute-env ()
  "EMACS_SERVER_FILE is not set after with-sprite exits."
  (let ((before (getenv "EMACS_SERVER_FILE")))
    (cl-letf (((symbol-function 'sprite--server-file) (lambda (n) n)))
      (with-sprite "work.0.render" t))
    (should (equal before (getenv "EMACS_SERVER_FILE")))))

;;;; Overview buffer

(ert-deftest sprite/format-uptime-nil ()
  (should (equal "?" (sprite--format-uptime nil))))

(ert-deftest sprite/format-uptime-seconds ()
  (should (equal "30s" (sprite--format-uptime 30.0))))

(ert-deftest sprite/format-uptime-minutes ()
  (should (equal "5m" (sprite--format-uptime 300.0))))

(ert-deftest sprite/format-uptime-hours ()
  (should (equal "2h" (sprite--format-uptime 7200.0))))

(ert-deftest sprite/format-uptime-days ()
  (should (equal "3d" (sprite--format-uptime (* 3 86400.0)))))

(ert-deftest sprite/format-uptime-boundary-59s ()
  (should (equal "59s" (sprite--format-uptime 59.0))))

(ert-deftest sprite/format-uptime-boundary-60s ()
  (should (equal "1m" (sprite--format-uptime 60.0))))

(ert-deftest sprite/list-mode-derived-from-tabulated-list ()
  (should (get 'sprite-list-mode 'derived-mode-parent)))

(ert-deftest sprite/build-list-entry-structure ()
  "build-list-entry returns (STRUCT VECTOR) with 6 columns."
  (cl-letf (((symbol-function 'sprite--query-buffer-count) (lambda (_) nil))
            ((symbol-function 'sprite--decommissioned-p) (lambda (_) nil)))
    (let ((s (sprite--make :name "work.0.render" :idx 0
                           :parent "work" :unique-name "render"
                           :spawned-by "scratch")))
      (let* ((entry (sprite--build-list-entry s))
             (id (car entry))
             (vec (cadr entry)))
        (should (sprite-p id))
        (should (= 6 (length vec)))
        (should (equal "0" (aref vec 0)))
        (should (equal "work.0.render" (aref vec 1)))
        (should (equal "scratch" (aref vec 5)))))))

(ert-deftest sprite/build-list-entry-uptime-nil-when-no-start ()
  (cl-letf (((symbol-function 'sprite--query-buffer-count) (lambda (_) nil)))
    (let ((s (sprite--make :name "work.0.render" :idx 0
                           :parent "work" :unique-name "render")))
      (let ((vec (cadr (sprite--build-list-entry s))))
        (should (equal "?" (aref vec 2)))))))

(ert-deftest sprite/build-list-entry-buffers-unknown ()
  (cl-letf (((symbol-function 'sprite--query-buffer-count) (lambda (_) nil)))
    (let ((s (sprite--make :name "work.0.render" :idx 0
                           :parent "work" :unique-name "render")))
      (let ((vec (cadr (sprite--build-list-entry s))))
        (should (equal "?" (aref vec 3)))))))

;;;; Transient key validation

(ert-deftest sprite/transient-no-duplicate-keys ()
  (require 'test-helper nil t)
  (when (fboundp 'transient-test/collect-keys)
    (let ((keys (transient-test/collect-keys 'sprite-list-menu)))
      (should (null (transient-test/duplicate-keys keys))))))

(ert-deftest sprite/transient-no-prefix-conflicts ()
  (require 'test-helper nil t)
  (when (fboundp 'transient-test/collect-keys)
    (let ((keys (transient-test/collect-keys 'sprite-list-menu)))
      (should (null (transient-test/key-prefix-conflicts keys))))))

;;;; Fleet API

(ert-deftest sprite/controller-p-true-when-has-children ()
  (sprite-test/with-registry
    (cl-letf (((symbol-function 'tychoish/resolve-instance-id) (lambda () "work"))
              ((symbol-function 'sprite--decommissioned-p) (lambda (_) nil)))
      (sprite--registry-put (sprite--make :name "work.0.render"
                                          :idx 0 :parent "work"))
      (should (sprites-controller-p)))))

(ert-deftest sprite/controller-p-false-when-empty ()
  (sprite-test/with-registry
    (cl-letf (((symbol-function 'tychoish/resolve-instance-id) (lambda () "work")))
      (should-not (sprites-controller-p)))))

(ert-deftest sprite/controller-p-false-when-all-decommissioned ()
  (sprite-test/with-registry
    (cl-letf (((symbol-function 'tychoish/resolve-instance-id) (lambda () "work"))
              ((symbol-function 'sprite--decommissioned-p) (lambda (_) t)))
      (sprite--registry-put (sprite--make :name "work.0.render"
                                          :idx 0 :parent "work"))
      (should-not (sprites-controller-p)))))

(ert-deftest sprite/worker-p-true-for-sprite-name ()
  (cl-letf (((symbol-function 'tychoish/resolve-instance-id)
             (lambda () "work.0.render")))
    (should (sprites-worker-p))))

(ert-deftest sprite/worker-p-false-for-top-level ()
  (cl-letf (((symbol-function 'tychoish/resolve-instance-id) (lambda () "work")))
    (should-not (sprites-worker-p))))

(ert-deftest sprite/available-p-false-when-no-sprites ()
  (sprite-test/with-registry
    (cl-letf (((symbol-function 'tychoish/resolve-instance-id) (lambda () "work"))
              ((symbol-function 'sprite--decommissioned-p) (lambda (_) nil)))
      (should-not (sprites-available-p)))))

(ert-deftest sprite/available-p-true-when-has-sprites ()
  (sprite-test/with-registry
    (cl-letf (((symbol-function 'tychoish/resolve-instance-id) (lambda () "work"))
              ((symbol-function 'sprite--decommissioned-p) (lambda (_) nil)))
      (sprite--registry-put (sprite--make :name "work.0.render"
                                          :idx 0 :parent "work"))
      (should (sprites-available-p)))))

(ert-deftest sprite/get-next-returns-available-sprite ()
  (sprite-test/with-registry
    (let ((s (sprite--make :name "work.0.render" :idx 0 :parent "work")))
      (sprite--registry-put s)
      (cl-letf (((symbol-function 'tychoish/resolve-instance-id) (lambda () "work"))
                ((symbol-function 'sprite--running-p) (lambda (_) t))
                ((symbol-function 'sprite--decommissioned-p) (lambda (_) nil)))
        (let ((result (sprites-get-next)))
          (should (sprite-p result))
          (should (equal "work.0.render" (sprite-name result))))))))

(ert-deftest sprite/get-next-nil-when-all-busy ()
  (sprite-test/with-registry
    (let ((s (sprite--make :name "work.0.render" :idx 0 :parent "work"
                           :last-contact (current-time))))
      (sprite--registry-put s)
      (cl-letf (((symbol-function 'tychoish/resolve-instance-id) (lambda () "work"))
                ((symbol-function 'sprite--running-p) (lambda (_) t))
                ((symbol-function 'sprite--decommissioned-p) (lambda (_) nil))
                (sprite-busy-threshold 3600))
        (should (null (sprites-get-next)))))))

(ert-deftest sprite/get-next-skips-non-running ()
  (sprite-test/with-registry
    (let ((s (sprite--make :name "work.0.render" :idx 0 :parent "work")))
      (sprite--registry-put s)
      (cl-letf (((symbol-function 'tychoish/resolve-instance-id) (lambda () "work"))
                ((symbol-function 'sprite--running-p) (lambda (_) nil))
                ((symbol-function 'sprite--decommissioned-p) (lambda (_) nil)))
        (should (null (sprites-get-next)))))))

(ert-deftest sprite/not-busy-p-true-when-no-contact ()
  (let ((s (sprite--make :name "work.0.render" :idx 0 :parent "work")))
    (should (sprite--not-busy-p s))))

(ert-deftest sprite/not-busy-p-false-when-recent-contact ()
  (let ((s (sprite--make :name "work.0.render" :idx 0 :parent "work"
                         :last-contact (current-time))))
    (let ((sprite-busy-threshold 3600))
      (should-not (sprite--not-busy-p s)))))

(ert-deftest sprite/not-busy-p-true-when-old-contact ()
  (let ((s (sprite--make :name "work.0.render" :idx 0 :parent "work"
                         :last-contact (time-subtract (current-time) 120))))
    (let ((sprite-busy-threshold 30))
      (should (sprite--not-busy-p s)))))

(ert-deftest sprite/get-or-create-errors-at-max ()
  (sprite-test/with-registry
    (cl-letf (((symbol-function 'tychoish/resolve-instance-id) (lambda () "work"))
              ((symbol-function 'sprite--running-p) (lambda (_) t))
              ((symbol-function 'sprite--decommissioned-p) (lambda (_) nil))
              (sprite-busy-threshold 3600))
      (let ((sprite-max-count 2))
        (sprite--registry-put (sprite--make :name "work.0.a" :idx 0 :parent "work"
                                            :last-contact (current-time)))
        (sprite--registry-put (sprite--make :name "work.1.b" :idx 1 :parent "work"
                                            :last-contact (current-time)))
        (should-error (sprites-get-or-create-next) :type 'user-error)))))

(ert-deftest sprite/resolve-list-includes-children-for-controller ()
  (sprite-test/with-registry
    (cl-letf (((symbol-function 'tychoish/resolve-instance-id) (lambda () "work"))
              ((symbol-function 'sprite--decommissioned-p) (lambda (_) nil)))
      (sprite--registry-put (sprite--make :name "work.0.render" :idx 0
                                          :parent "work"))
      (let ((list (sprites-resolve-list)))
        (should (= 1 (length list)))
        (should (equal "work.0.render" (sprite-name (car list))))))))

(ert-deftest sprite/resolve-list-includes-siblings-for-worker ()
  "A sprite (worker) sees sibling sprites from the same parent."
  (sprite-test/with-registry
    (cl-letf (((symbol-function 'tychoish/resolve-instance-id)
               (lambda () "work.0.render"))
              ((symbol-function 'sprite--decommissioned-p) (lambda (_) nil)))
      (sprite--registry-put (sprite--make :name "work.0.render" :idx 0
                                          :parent "work"))
      (sprite--registry-put (sprite--make :name "work.1.analysis" :idx 1
                                          :parent "work"))
      (let ((list (sprites-resolve-list)))
        (should (= 2 (length list)))))))

(ert-deftest sprite/resolve-list-excludes-decommissioned ()
  (sprite-test/with-registry
    (cl-letf (((symbol-function 'tychoish/resolve-instance-id) (lambda () "work"))
              ((symbol-function 'sprite--decommissioned-p)
               (lambda (name) (string= name "work.0.gone"))))
      (sprite--registry-put (sprite--make :name "work.0.gone" :idx 0
                                          :parent "work"))
      (sprite--registry-put (sprite--make :name "work.1.active" :idx 1
                                          :parent "work"))
      (let ((list (sprites-resolve-list)))
        (should (= 1 (length list)))
        (should (equal "work.1.active" (sprite-name (car list))))))))

;;;; Savehist wiring

(ert-deftest sprite/savehist-variable-registered ()
  "sprite--registry-saved is in savehist-additional-variables after loading."
  (when (featurep 'savehist)
    (should (member 'sprite--registry-saved savehist-additional-variables))))

(provide 'test-sprite)
;;; test-sprite.el ends here
