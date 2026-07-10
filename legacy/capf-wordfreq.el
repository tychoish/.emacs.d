;;; capf-wordfreq.el --- Capf backend for human language texts -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Johannes Mueller

;; Author: Johannes Mueller <github@johannes-mueller.org>
;; URL: https://github.com/johannes-mueller/capf-wordfreq.el
;; Version: 0.1.0
;; Keywords: capf, completion, convenience, matching
;; Package-Requires: ((emacs "28.1")

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation version 2. <https://www.gnu.org/licenses/>

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

(require 'subr-x)

;;;###autoload
(defconst capf-wordfreq-language-alist
  '(("afrikaans" . "af")
    ("arabic" . "ar")
    ("bulgarian" . "bg")
    ("bengali" . "bn")
    ("breton" . "br")
    ("bosnian" . "bs")
    ("catalan" . "ca")
    ("czech" . "cs")
    ("danish" . "da")
    ("german" . "de")
    ("greek" . "el")
    ("english" . "en")
    ("esperanto" . "eo")
    ("spanish" . "es")
    ("estonian" . "et")
    ("basque" . "eu")
    ("persian" . "fa")
    ("finnish" . "fi")
    ("french" . "fr")
    ("galician" . "gl")
    ("hebrew" . "he")
    ("hindi" . "hi")
    ("croatian" . "hr")
    ("hungarian" . "hu")
    ("armenian" . "hy")
    ("indonesian" . "id")
    ("icelandic" . "is")
    ("italian" . "it")
    ("japanese" . "ja")
    ("georgian" . "ka")
    ("kazakh" . "kk")
    ("korean" . "ko")
    ("lithuanian" . "lt")
    ("latvian" . "lv")
    ("macedonian" . "mk")
    ("malayalam" . "ml")
    ("malay" . "ms")
    ("dutch" . "nl")
    ("norwegian" . "no")
    ("polish" . "pl")
    ("portuguese" . "pt")
    ("brasileiro" .  "pt_br")
    ("romanian" . "ro")
    ("russian" . "ru")
    ("sinhala" . "si")
    ("slovak" . "sk")
    ("slovenian" . "sl")
    ("albanian" . "sq")
    ("serbian" . "sr")
    ("swedish" . "sv")
    ("tamil" . "ta")
    ("telugu" . "te")
    ("thai" . "th")
    ("tagalog" . "tl")
    ("turkish" . "tr")
    ("ukrainian" . "uk")
    ("urdu" . "ur")
    ("vietnamese" . "vi")))

(defun capf-wordfreq--proposal-list ()
  "Get the friendly names of the languages."
  (mapcar #'car capf-wordfreq-language-alist))

(defun capf-wordfreq--iso-code (language)
  "Get the iso code of LANGUAGE."
  (cdr (assoc language capf-wordfreq-language-alist)))

(defvar-local capf-wordfreq--begin nil)
(defvar-local capf-wordfreq--cands nil)

(defun capf-wordfreq--default-path ()
  "Set up the default for `capf-wordfreq-path'."
  (concat (file-name-as-directory user-emacs-directory) "wordfreq-dicts"))

(defcustom capf-wordfreq-path (capf-wordfreq--default-path)
  "Path where the dictionary files reside.

The dictionary files are expected to have the name <language>.txt
where <language> is the contents of `ispell-local-dictionary' in
the current buffer."
  :type 'string
  :group 'capf)

(defcustom capf-wordfreq-minimal-candidate-length 0
  "The minimal length of the candidates"
  :type 'integer
  :group 'capf)

(defcustom capf-wordfreq-langauge "english"
  "The minimal length of the candidates"
  :type (cons 'choice (mapcar (lambda (lang) `(const ,(car lang))) capf-wordfreq-language-alist))
  :group 'capf)

(defun capf-wordfreq--dictionary ()
  "Determine the path of the word list file."
  (when-let* ((dct-file (concat (file-name-as-directory capf-wordfreq-path) capf-wordfreq-langauge ".txt")))
    (or (when (file-exists-p dct-file) (expand-file-name dct-file))
	(when-let* ((default (capf-wordfreq--default-path)))
	  (when (file-exists-p default) default)))))

(defun capf-wordfreq--enforce-exact-prefix (cand prefix)
  (concat prefix (substring cand (length prefix) nil)))

(defvar-local capf-wordfreq--msg-fragment "")

(defun capf-wordfreq--process-msg (msg)
  (let* ((last-message (car (last (split-string (string-trim-right msg) "[\n]"))))
         (fields (split-string last-message))
         (beg (string-to-number (car fields)))
         (prefix (car (cdr fields)))
         (cands (mapcar
                 (lambda (cand) (capf-wordfreq--enforce-exact-prefix cand prefix))
                 (cddr fields))))
    (setq capf-wordfreq--msg-fragment "")
    (setq capf-wordfreq--begin (when cands beg)
          capf-wordfreq--cands cands)))

(defun capf-wordfreq--return-buffer-filter (_proc msg)
  (if (string-suffix-p "\n" msg)
      (capf-wordfreq--process-msg (concat capf-wordfreq--msg-fragment msg))
    (setq capf-wordfreq--msg-fragment (concat capf-wordfreq--msg-fragment msg))))


(defun capf-wordfreq--external-process-observer (&rest _)
  (if-let* ((dict-file (capf-wordfreq--dictionary))
            (bounds (bounds-of-thing-at-point 'word))
            (beg (car bounds))
            (end (cdr bounds))
            (prefix (buffer-substring-no-properties beg end)))
      (process-send-string
       (capf-wordfreq--start-external-process)
       (format "%s %s %s\n" beg dict-file prefix))
    (setq capf-wordfreq--begin nil
          capf-wordfreq--cands nil)))


(defconst capf-wordfreq--script "
while read -r line; do
    split=($line)
    beg=${split[0]}
    dictfile=${split[1]}
    prefix=${split[2]}
    grep_command='grep -i ^\"$prefix\" $dictfile | tr \"\\n\" \" \"'
    words=\$(eval $grep_command)
    echo \"$beg $prefix $words\"
done
")

(defvar capf-wordfreq--external-process nil)
(defvar capf-wordfreq--begin nil)
(defvar capf-wordfreq--cands nil)

(defun capf-wordfreq--start-external-process ()
  (unless (capf-wordfreq--external-process-live-p)
    (ignore-errors (delete-process capf-wordfreq--external-process))
    (setq capf-wordfreq--external-process
          (make-process :name "capf-wordfreq-external"
                        :command `("/bin/bash" "-c" ,capf-wordfreq--script)
                        :noquery t
                        :stderr "*external-stderr*"
                        :filter #'capf-wordfreq--return-buffer-filter)))
  capf-wordfreq--external-process)


(defun capf-wordfreq--external-process-live-p ()
  (process-live-p capf-wordfreq--external-process))


(defun capf-wordfreq--enforce-min-length (cands)
  (seq-filter
   (lambda (cand) (>= (length cand) capf-wordfreq-minimal-candidate-length))
   cands))


;;;###autoload
(defun capf-wordfreq-completion-at-point-function ()
  "The completion at point function."
  (if (and capf-wordfreq--begin capf-wordfreq--cands)
      (list capf-wordfreq--begin (point) (capf-wordfreq--enforce-min-length capf-wordfreq--cands))
    (add-hook 'after-change-functions #'capf-wordfreq--external-process-observer nil 'local)
    '()))

;;;###autoload
(defun capf-wordfreq-download-list ()
  "Download a wordlist from FrequentWords and process it for use.

The language can be chosen from a completion list.  If the full
wordlist for the chosen language is so big, that there is a
shorter version of 50k words available, you will be prompted to
choose the short version.  Probably it is a good idea to choose
the short version as the full versions can be quite huge and
introduce latency to the completion proposals."
  (interactive)
  (let* ((language (completing-read "Choose language: " (capf-wordfreq--proposal-list)))
         (lang-code (capf-wordfreq--iso-code language))
         (kind-str (when (and (capf-wordfreq--probe-50k lang-code)
                              (capf-wordfreq--prompt-fetch-short))
                     "50k" "full")))
    (setq capf-wordfreq--word-list-buffer
          (url-retrieve (capf-wordfreq--dict-url lang-code kind-str)
                        #'capf-wordfreq--list-retrieved-callback
                        `(,language)
                        :inhibit-cookies))))

(defconst capf-wordfreq--frequency-word-url-prefix
  "https://raw.githubusercontent.com/johannes-mueller/FrequencyWords/master/content/2018/")

(defun capf-wordfreq--dict-url (lang-code kind)
  "Setup the file path for the language LANG-CODE.
KIND is either \"full\" or \"50k\"."
    (concat capf-wordfreq--frequency-word-url-prefix
            lang-code "/"
            lang-code "_"
            kind ".txt"))

(defun capf-wordfreq--probe-50k (lang-code)
  "Test if a 50k version for language LANG-CODE is available."
  (let ((url-request-method "HEAD"))
    (with-current-buffer (url-retrieve-synchronously
                          (capf-wordfreq--dict-url lang-code "50k")
                          :inhibit-cookies)
      (goto-char (point-min))
      (let ((status-code
             (nth 1 (split-string (car (split-string (buffer-string) "\n")) " "))))
        (kill-current-buffer)
        (not (equal status-code "404"))))))

(defun capf-wordfreq--drop-http-response-header ()
  "Delete the http response header from received word list."
  (goto-char (point-min))
  (re-search-forward "^$")
  (forward-char)
  (delete-region (point-min) (point)))

(defun capf-wordfreq--drop-frequency-values ()
  "Delete the frequency value after each word in the word list."
  (goto-char (point-min))
  (while (re-search-forward "\s[0-9]+$" nil t)
    (replace-match "" nil nil)))

(defun capf-wordfreq--prompt-fetch-short ()
  "Prompt if the user wants the short version of the word list."
  (y-or-n-p "Use reduced length 50k words? "))

(defun capf-wordfreq--list-retrieved-callback (response language)
  "Process the downloaded word list and save it to the appropriate place.

RESPONSE the http response from `url-retrieve', LANGUAGE the
language of the word list."
  (when (eq (car response) :error)
    (user-error "Fetching the word list failed, sorry.
Either a problem with your net connection or something has changed with
the word list source.  Consider filing an issue"))
  (with-current-buffer capf-wordfreq--word-list-buffer
    (capf-wordfreq--drop-http-response-header)
    (capf-wordfreq--drop-frequency-values)
    (unless (file-directory-p capf-wordfreq-path)
      (make-directory capf-wordfreq-path))
    (write-file (concat (file-name-as-directory capf-wordfreq-path)
                        language ".txt"))
    (kill-current-buffer)
    (setq capf-wordfreq--word-list-buffer nil)))


(provide 'capf-wordfreq)
;;; capf-wordfreq.el ends here
