;;; "Compiled" snippets and support files for `org-mode'  -*- lexical-binding:t -*-
;;; Snippet definitions:
;;;
(yas-define-snippets 'org-mode
		     '((">vi"
			"[[${1:link of the video}][file:${2:link of the image}]"
			"video" nil nil nil
			"/home/tychoish/.emacs.d/snippets/org-mode/video"
			nil nil)
		       (">verse" "#+begin_verse\n$0\n#+end_verse"
			"verse" nil nil nil
			"/home/tychoish/.emacs.d/snippets/org-mode/verse"
			nil nil)
		       ("uml" "#+begin_uml\n$1\n#+end_uml" "uml" nil
			nil nil
			"/home/tychoish/.emacs.d/snippets/org-mode/uml"
			nil nil)
		       ("project"
			"#+TITLE:prime.tychoish.projects\n#+CATEGORY: ${1:category}\n* Inbox\n* Tasks\n* Notes\n* Loops\n** Daily\n** Weekly\n** Quarterly\n** Yearly\n* Journal\n"
			"sasha chua inspired project tracking file"
			nil nil nil
			"/home/tychoish/.emacs.d/snippets/org-mode/tracker"
			nil nil)
		       (">ti" "#+title: $0" "title" nil nil nil
			"/home/tychoish/.emacs.d/snippets/org-mode/title"
			nil nil)
		       (">ta"
			"#+caption: ${1: caption of the table}\n|${2:column 1} | ${3: column 2} |\n|--------------+----------------|\n"
			"table" nil nil nil
			"/home/tychoish/.emacs.d/snippets/org-mode/table"
			nil nil)
		       (">src" "#+BEGIN_SRC $1\n  $0\n#+END_SRC\n"
			"src" nil nil nil
			"/home/tychoish/.emacs.d/snippets/org-mode/src"
			nil nil)
		       ("set" "#+setupfile: $0" "setup" nil nil nil
			"/home/tychoish/.emacs.d/snippets/org-mode/setup"
			nil nil)
		       (">quote" "#+begin_quote\n$0\n#+end_quote"
			"quote" nil nil nil
			"/home/tychoish/.emacs.d/snippets/org-mode/quote"
			nil nil)
		       ("py" "#+begin_src python\n$0\n#+end_src"
			"python" nil nil nil
			"/home/tychoish/.emacs.d/snippets/org-mode/python"
			nil nil)
		       (">op"
			"#+options: h:${1:1} num:${2:t||nil} toc:${3:t||nil}$0"
			"options" nil nil nil
			"/home/tychoish/.emacs.d/snippets/org-mode/options"
			nil nil)
		       (">li" "[[${1:link}][${2:description}]]\n"
			"link" nil nil nil
			"/home/tychoish/.emacs.d/snippets/org-mode/link"
			nil nil)
		       (">lan" "#+language: ${1:en}" "language" nil
			nil nil
			"/home/tychoish/.emacs.d/snippets/org-mode/language"
			nil nil)
		       (">ke" "#+keywords: $0" "keywords" nil nil nil
			"/home/tychoish/.emacs.d/snippets/org-mode/keywords"
			nil nil)
		       ("ipy"
			"#+begin_src ipython :session ${1:session01} :file ${2:$$(concat (make-temp-name \"./ipython-\") \".png\")} :exports ${3:both}\n$0\n#+end_src"
			"ipython" nil nil nil
			"/home/tychoish/.emacs.d/snippets/org-mode/ipython"
			nil nil)
		       (">i" "#+include: $0" "include" nil nil nil
			"/home/tychoish/.emacs.d/snippets/org-mode/include"
			nil nil)
		       (">im"
			"#+caption: ${1:caption of the image}\n[[file:${2:image_path}]]$0"
			"image" nil nil nil
			"/home/tychoish/.emacs.d/snippets/org-mode/image"
			nil nil)
		       (">ht" "#+html:$1" "html" nil nil nil
			"/home/tychoish/.emacs.d/snippets/org-mode/html"
			nil nil)
		       (">fn"
			"[fn:${1:note id}] $0\n\n[fn${1:note id}]"
			"footnote" nil nil nil
			"/home/tychoish/.emacs.d/snippets/org-mode/fn"
			nil nil)
		       ("fig"
			"#+caption: ${1:caption}\n#+attr_latex: ${2:scale=0.75}\n#+label: fig:${3:label}$0"
			"figure" nil nil nil
			"/home/tychoish/.emacs.d/snippets/org-mode/figure"
			nil nil)
		       (">ex"
			"#+begin_export ${1:type}\n$0\n#+end_export"
			"export" nil nil nil
			"/home/tychoish/.emacs.d/snippets/org-mode/export"
			nil nil)
		       (">e" "#+begin_example\n$0\n#+end_example"
			"example" nil nil nil
			"/home/tychoish/.emacs.d/snippets/org-mode/exampleblock"
			nil nil)
		       ("entry"
			"#+begin_html\n---\nlayout: ${1:default}\ntitle: ${2:title}\n---\n#+end_html\n"
			"entry" nil nil nil
			"/home/tychoish/.emacs.d/snippets/org-mode/entry"
			nil nil)
		       ("emb"
			"src_${1:lang}${2:[${3:where}]}{${4:code}}"
			"embedded" nil nil nil
			"/home/tychoish/.emacs.d/snippets/org-mode/embedded"
			nil nil)
		       (">em" "#+email: $0" "email" nil nil nil
			"/home/tychoish/.emacs.d/snippets/org-mode/email"
			nil nil)
		       ("emacs-lisp_"
			"#+begin_src emacs-lisp :tangle yes\n$0\n#+end_src"
			"emacs-lisp" nil nil nil
			"/home/tychoish/.emacs.d/snippets/org-mode/emacs-lisp"
			nil nil)
		       ("elisp"
			"#+begin_src emacs-lisp :tangle yes\n$0\n#+end_src"
			"elisp" nil nil nil
			"/home/tychoish/.emacs.d/snippets/org-mode/elisp"
			nil nil)
		       ("dot"
			"#+begin_src dot :file ${1:file} :cmdline -t${2:pdf} :exports none :results silent\n$0\n#+end_src\n[[file:${3:path}]]"
			"dot" nil nil nil
			"/home/tychoish/.emacs.d/snippets/org-mode/dot"
			nil nil)
		       ("desc" "#+description: $0" "description" nil
			nil nil
			"/home/tychoish/.emacs.d/snippets/org-mode/description"
			nil nil)
		       (">da" "#+date: ${1:year}:${2:month}:${3:day}"
			"date" nil nil nil
			"/home/tychoish/.emacs.d/snippets/org-mode/date"
			nil nil)
		       (">c" "#+begin_center\n$0\n#+end_center"
			"center" nil nil nil
			"/home/tychoish/.emacs.d/snippets/org-mode/center"
			nil nil)
		       (">au" "#+author: $0" "author" nil nil nil
			"/home/tychoish/.emacs.d/snippets/org-mode/author"
			nil nil)
		       (">afn" "[fn:: ${1:note}] $0\n" "footnote" nil
			nil nil
			"/home/tychoish/.emacs.d/snippets/org-mode/afn"
			nil nil)))


;;; Do not edit! File generated at Fri Aug  1 09:43:16 2025
