;;; "Compiled" snippets and support files for `markdown-mode'  -*- lexical-binding:t -*-
;;; Snippet definitions:
;;;
(yas-define-snippets 'markdown-mode
		     '(("hugo"
			"---\ntitle: \"${1:title}\"\ntags: [\"${2:}\"]\ncategories: [\"${3:software}\"]\ndate: `(format-time-string \"%Y-%m-%d\")`\nauthor: tychoish\nmarkup: rst\n---\n\n$0\n"
			"hugo" nil nil nil
			"/home/tychoish/.emacs.d/snippets/markdown-mode/hugo"
			nil nil)
		       ("`" "\\`${1:Code}\\` $0\n" "Inline Code" nil
			nil nil
			"/home/tychoish/.emacs.d/snippets/markdown-mode/`"
			nil nil)
		       ("__" "**${1:Text}** $0\n" "Strong" nil nil nil
			"/home/tychoish/.emacs.d/snippets/markdown-mode/__"
			nil nil)
		       ("-" "- ${1:Text}\n-$0\n"
			"\"Unordered List Hypen\"" nil nil nil
			"/home/tychoish/.emacs.d/snippets/markdown-mode/-"
			nil nil)
		       ("+" "+ ${1:Text}\n+$0\n"
			"\"Unordered List Plus\"" nil nil nil
			"/home/tychoish/.emacs.d/snippets/markdown-mode/+"
			nil nil)))


;;; Do not edit! File generated at Fri Aug  1 09:43:16 2025
