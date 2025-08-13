;;; "Compiled" snippets and support files for `shell-script-mode'  -*- lexical-binding:t -*-
;;; Snippet definitions:
;;;
(yas-define-snippets 'shell-script-mode
		     '(("here"
			"<<-${2:'${1:TOKEN}'}\n	$0\n${1/['\"`](.+)['\"`]/$1/}"
			"Here Document" nil nil nil
			"/home/tychoish/.emacs.d/snippets/shell-script-mode/here document (here).yasnippet"
			nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'shell-script-mode
		     '(("temp"
			"${1:TMPFILE}=\"$(mktemp -t ${2:`echo \"${TM_FILENAME%.*}\" | sed -e 's/[^a-zA-Z]/_/g' -e 's/^$/untitled/'`})\"\n${3:${4/(.+)/trap \"/}${4:rm -f '\\$${1/.*\\s//}'}${4/(.+)/\" 0               # EXIT\n/}${5/(.+)/trap \"/}${5:rm -f '\\$${1/.*\\s//}'; exit 1}${5/(.+)/\" 2       # INT\n/}${6/(.+)/trap \"/}${6:rm -f '\\$${1/.*\\s//}'; exit 1}${6/(.+)/\" 1 15    # HUP TERM\n/}}\n"
			"Tempfile" nil ("Idioms") nil
			"/home/tychoish/.emacs.d/snippets/shell-script-mode/Idioms/Tempfile.yasnippet"
			nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'shell-script-mode
		     '(("while"
			"while ${2:[[ ${1:condition} ]]}; do\n	${0:#statements}\ndone"
			"while … done" nil ("Loops") nil
			"/home/tychoish/.emacs.d/snippets/shell-script-mode/Loops/while … (done).yasnippet"
			nil nil)
		       ("until"
			"until ${2:[[ ${1:condition} ]]}; do\n	${0:#statements}\ndone"
			"until … done" nil ("Loops") nil
			"/home/tychoish/.emacs.d/snippets/shell-script-mode/Loops/until … (done).yasnippet"
			nil nil)
		       ("forin"
			"for ${1:i}${2/.+/ in /}${2:words}; do\n	${0:#statements}\ndone"
			"for … in … done" nil ("Loops") nil
			"/home/tychoish/.emacs.d/snippets/shell-script-mode/Loops/for … in … done (forin).yasnippet"
			nil nil)
		       ("for"
			"for (( i = 0; i < ${1:10}; i++ )); do\n	${0:#statements}\ndone"
			"for … done" nil ("Loops") nil
			"/home/tychoish/.emacs.d/snippets/shell-script-mode/Loops/for ... done (for).yasnippet"
			nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'shell-script-mode
		     '(("if"
			"if ${2:[[ ${1:condition} ]]}; then\n	${0:#statements}\nfi"
			"if … fi" nil ("Tests") nil
			"/home/tychoish/.emacs.d/snippets/shell-script-mode/Tests/if ... then (if).yasnippet"
			nil nil)
		       ("elif"
			"elif ${2:[[ ${1:condition} ]]}; then\n	${0:#statements}"
			"elif …" nil ("Tests") nil
			"/home/tychoish/.emacs.d/snippets/shell-script-mode/Tests/elif .. (elif).yasnippet"
			nil nil)
		       ("case"
			"case ${1:word} in\n	${2:pattern} )\n		$0;;\nesac"
			"case … esac" nil ("Tests") nil
			"/home/tychoish/.emacs.d/snippets/shell-script-mode/Tests/case .. esac (case).yasnippet"
			nil nil)))


;;; Do not edit! File generated at Fri Aug  1 09:43:17 2025
