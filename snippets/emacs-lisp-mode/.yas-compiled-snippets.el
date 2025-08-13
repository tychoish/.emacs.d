;;; "Compiled" snippets and support files for `emacs-lisp-mode'  -*- lexical-binding:t -*-
;;; Snippet definitions:
;;;
(yas-define-snippets 'emacs-lisp-mode
		     '(("yonp" "(yes-or-no-p \"PROMPT$0 \")"
			"y-or-n-p" nil nil nil
			"/home/tychoish/.emacs.d/snippets/emacs-lisp-mode/y-or-n-p"
			nil nil)
		       ("while" "(while $0)" "while" nil nil nil
			"/home/tychoish/.emacs.d/snippets/emacs-lisp-mode/while"
			nil nil)
		       ("set" "(set $0 )" "set" nil nil nil
			"/home/tychoish/.emacs.d/snippets/emacs-lisp-mode/set"
			nil nil)
		       ("rmhk"
			"(remove-hook '${1:Hook} '${2:Function})"
			"(remove-hook ... ...)" nil nil nil
			"/home/tychoish/.emacs.d/snippets/emacs-lisp-mode/rmhk"
			nil nil)
		       ("push" "(push $0 )" "push" nil nil nil
			"/home/tychoish/.emacs.d/snippets/emacs-lisp-mode/push"
			nil nil)
		       ("progn" "(progn $0)" "progn" nil nil nil
			"/home/tychoish/.emacs.d/snippets/emacs-lisp-mode/progn"
			nil nil)
		       ("print" "(print $0)" "print" nil nil nil
			"/home/tychoish/.emacs.d/snippets/emacs-lisp-mode/print"
			nil nil)
		       ("lt" "(let (${1:varlist})\n  ${0:body})"
			"(let ... ...)" nil nil nil
			"/home/tychoish/.emacs.d/snippets/emacs-lisp-mode/lt"
			nil nil)
		       ("length" "(length $0)" "length" nil nil nil
			"/home/tychoish/.emacs.d/snippets/emacs-lisp-mode/length"
			nil nil)
		       ("lai" "(lambda () (interactive) ($0))"
			"(lambda () (interactive) (...))" nil nil nil
			"/home/tychoish/.emacs.d/snippets/emacs-lisp-mode/lai"
			nil nil)
		       ("function" "(function $0 )" "function" nil nil
			nil
			"/home/tychoish/.emacs.d/snippets/emacs-lisp-mode/function"
			nil nil)
		       ("funcall" "(funcall $0)" "funcall" nil nil nil
			"/home/tychoish/.emacs.d/snippets/emacs-lisp-mode/funcall"
			nil nil)
		       ("evalal"
			"(eval-after-load ${1:file-name}\n  ${0:form})"
			"(eval-after-load ... ...)" nil nil nil
			"/home/tychoish/.emacs.d/snippets/emacs-lisp-mode/evalal"
			nil nil)
		       ("dolist" "(dolist $0 )" "dolist" nil nil nil
			"/home/tychoish/.emacs.d/snippets/emacs-lisp-mode/dolist"
			nil nil)
		       ("dlhkd"
			"(dolist (hooked (list\n               ${1:mode-list}\n               ))\n  (add-hook '${0:mode-hook} hooked))\n"
			"(dolist (hooked (list ...)) (add-hook '... hooked))"
			nil nil nil
			"/home/tychoish/.emacs.d/snippets/emacs-lisp-mode/dlhkd"
			nil nil)
		       ("dlhk"
			"(dolist (hook (list\n               ${1:mode-list}\n               ))\n  (add-hook hook '${0:mode-hook}))"
			"(dolist (hook (list ... )) (add-hook hook '...))"
			nil nil nil
			"/home/tychoish/.emacs.d/snippets/emacs-lisp-mode/dlhk"
			nil nil)
		       ("dlaa"
			"(dolist (${1:elt-cons} '(${2:elt-cons-list}))\n  (add-to-alist '${0:alist} $1))\n \n"
			"(dolist (... (...)) (add-to-alist '... ...))"
			nil nil nil
			"/home/tychoish/.emacs.d/snippets/emacs-lisp-mode/dlaa"
			nil nil)
		       ("deft"
			"(defun test ()\n  \"Just for test\"        \n  (interactive)\n  $0\n)\n"
			"(defun test () \"Just for test\" (interactive) ...)"
			nil nil nil
			"/home/tychoish/.emacs.d/snippets/emacs-lisp-mode/deft"
			nil nil)
		       ("defmm"
			"(define-minor-mode ${1:mode-name}\n  \"${2:Document}\"\n  :init-value ${3:init-value}\n  :lighter \"${4:highlight-name}\"\n  :keymap ${5:keymap}\n  :group '${6:group})"
			"(define-minor-mode ... ... ... ... ... ...)"
			nil nil nil
			"/home/tychoish/.emacs.d/snippets/emacs-lisp-mode/defmm"
			nil nil)
		       ("defgp"
			"(defgroup ${1:Group-Name} ${2:Group-value}\n  \"${3:Group-doc}\"\n  $0)"
			"(defgroup ... ... ... ...)" nil nil nil
			"/home/tychoish/.emacs.d/snippets/emacs-lisp-mode/defgp"
			nil nil)
		       ("deffc"
			"(defface ${1:face}\n  ${2:spec}\n  \"${3:doc}\"\n  ${0:args})\n"
			"(defface ... ... ... ...)" nil nil nil
			"/home/tychoish/.emacs.d/snippets/emacs-lisp-mode/deffc"
			nil nil)
		       ("apply" "(apply $0 )" "apply" nil nil nil
			"/home/tychoish/.emacs.d/snippets/emacs-lisp-mode/apply"
			nil nil)
		       ("append" "(append $0 )" "append" nil nil nil
			"/home/tychoish/.emacs.d/snippets/emacs-lisp-mode/append"
			nil nil)
		       ("addusk"
			"(lazy-unset-key '(\"${1:key-list}\")$0)\n"
			"(lazy-unset-key ... ...)" nil nil nil
			"/home/tychoish/.emacs.d/snippets/emacs-lisp-mode/addusk"
			nil nil)
		       ("addul"
			"(unless ${1:conditional}\n  ${0:body})"
			"(unless ... ...)" nil nil nil
			"/home/tychoish/.emacs.d/snippets/emacs-lisp-mode/addul"
			nil nil)
		       ("addtl"
			"(add-to-list ${1:list-var} ${0:element})"
			"(add-to-list ... ...)" nil nil nil
			"/home/tychoish/.emacs.d/snippets/emacs-lisp-mode/addtl"
			nil nil)
		       ("addse"
			"(setq ${1:variable-name} ${0:variable-value})"
			"(setq ... ...)" nil nil nil
			"/home/tychoish/.emacs.d/snippets/emacs-lisp-mode/addse"
			nil nil)
		       ("addrq" "(require '${0:library-name})"
			"(require '...)" nil nil nil
			"/home/tychoish/.emacs.d/snippets/emacs-lisp-mode/addrq"
			nil nil)
		       ("addpr" "(provide '${0:library-name})"
			"(provide '...)" nil nil nil
			"/home/tychoish/.emacs.d/snippets/emacs-lisp-mode/addpr"
			nil nil)
		       ("addif" "(if ${1:conditional}\n    ${2:then})"
			"(if ... ... ...)" nil nil nil
			"/home/tychoish/.emacs.d/snippets/emacs-lisp-mode/addif"
			nil nil)
		       ("addic" "((= c ?${1:char}) (${0:command}))"
			"((= c ...) (...))" nil nil nil
			"/home/tychoish/.emacs.d/snippets/emacs-lisp-mode/addic"
			nil nil)
		       ("addguk"
			"(global-unset-key (kbd ${0:key-value}))"
			"(global-unset-key ...)" nil nil nil
			"/home/tychoish/.emacs.d/snippets/emacs-lisp-mode/addguk"
			nil nil)
		       ("addcll"
			";; ${1:Date}\n;;  * ${2:Name}:\n;;      * $0\n;;\n"
			"Add change logs with details" nil nil nil
			"/home/tychoish/.emacs.d/snippets/emacs-lisp-mode/addcll"
			nil nil)
		       ("addcl"
			";; `(format-time-string \"%Y/%m/%d\")`\n;;  * `user-full-name`:\n;;      * $0\n;;\n"
			"Add change logs" nil nil nil
			"/home/tychoish/.emacs.d/snippets/emacs-lisp-mode/addcl"
			nil nil)
		       ("addc" ";;; ### $1 ###\n;;; --- $2\n$0"
			";;; ### ... ### ..." nil nil nil
			"/home/tychoish/.emacs.d/snippets/emacs-lisp-mode/addc"
			nil nil)
		       ("adda" "(ad-deactivate '${1:Function})\n"
			"(ad-deactivate ...)" nil nil nil
			"/home/tychoish/.emacs.d/snippets/emacs-lisp-mode/adda"
			nil nil)
		       ("ada" "(ad-activate '${1:Function})\n"
			"(ad-activate ...)" nil nil nil
			"/home/tychoish/.emacs.d/snippets/emacs-lisp-mode/ada"
			nil nil)))


;;; Do not edit! File generated at Fri Aug  1 09:43:16 2025
