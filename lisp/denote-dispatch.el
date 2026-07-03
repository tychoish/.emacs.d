;;; denote-dispatch.el --- Transient dispatch for Denote commands -*- lexical-binding: t; -*-

;;; Commentary:
;; Transient prefix exposing the full Denote command surface:
;; create, find, view, link, rename, sequence, filter, and convert.
;;
;; Bind via tychoish/denote-map:
;;   (bind-key "dd" #'denote-dispatch tychoish/denote-map)

;;; Code:

(require 'transient)

(declare-function denote "denote")
(declare-function denote-open-or-create "denote")
(declare-function denote-link "denote")
(declare-function denote-backlinks "denote")
(declare-function denote-rename-file "denote")
(declare-function denote-dired "denote")
(declare-function denote-rename-file-using-front-matter "denote")
(declare-function consult-denote-find "consult-denote")
(declare-function consult-denote-grep "consult-denote")
(declare-function denote-journal-new-entry "denote-journal")
(declare-function denote-sequence-new-child "denote-sequence")
(declare-function denote-sequence-new-sibling "denote-sequence")
(declare-function denote-sequence-new-parent "denote-sequence")
(declare-function denote-sequence-link-to-parent "denote-sequence")
(declare-function denote-sequence-reparent "denote-sequence")
(declare-function denote-markdown-convert-links-to-markdown-format "denote-markdown")
(declare-function denote-markdown-convert-links-to-denote-format "denote-markdown")

;;;###autoload
(transient-define-prefix denote-dispatch ()
  "Dispatch Denote commands by area."
  [["Create"
    ("nc" "new note"            denote)
    ("nj" "journal entry"       denote-journal-new-entry)
    ("ns" "sequence sibling"    denote-sequence-new-sibling)
    ("nh" "sequence child"      denote-sequence-new-child)
    ("np" "sequence parent"     denote-sequence-new-parent)]
   ["Find & View"
    ("ff" "find file"           consult-denote-find)
    ("fg" "grep content"        consult-denote-grep)
    ("fo" "open or create"      denote-open-or-create)
    ("fd" "dired view"          denote-dired)
    ("fb" "backlinks"           denote-backlinks)]]
  [["Link"
    ("ll" "insert link"         denote-link)
    ("lp" "link to parent"      denote-sequence-link-to-parent)]
   ["Rename & Manage"
    ("rr" "rename file"         denote-rename-file)
    ("rf" "rename from fm"      denote-rename-file-using-front-matter)
    ("rs" "reparent sequence"   denote-sequence-reparent)]
  ["Convert"
    ("cm" "links → markdown"    denote-markdown-convert-links-to-markdown-format)
    ("cd" "links → denote"      denote-markdown-convert-links-to-denote-format)]])

(provide 'denote-dispatch)
;;; denote-dispatch.el ends here
