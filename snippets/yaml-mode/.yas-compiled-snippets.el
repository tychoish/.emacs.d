;;; "Compiled" snippets and support files for `yaml-mode'  -*- lexical-binding:t -*-
;;; Snippet definitions:
;;;
(yas-define-snippets 'yaml-mode
		     '(("rt"
			"name: \":method:\\`sh.${1:object}\\`\"\nfile: /reference/method/sh.${2:name}\ndescription: \"${3:description}\"\n---\n$0"
			"rt" nil nil nil
			"/home/tychoish/.emacs.d/snippets/yaml-mode/rt"
			nil nil)
		       ("key" "${1:key}: ${2:value}$0" "key: value"
			nil nil nil
			"/home/tychoish/.emacs.d/snippets/yaml-mode/key"
			nil nil)))


;;; Do not edit! File generated at Fri Aug  1 09:43:17 2025
