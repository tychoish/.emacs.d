;;; "Compiled" snippets and support files for `lisp-mode'  -*- lexical-binding:t -*-
;;; Snippet definitions:
;;;
(yas-define-snippets 'lisp-mode
		     '(("typecase"
			"(typecase ${1:key-form}\n  (${2:match} ${3:result})${4:\n  (t ${5:otherwise})})"
			"typecase" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/typecase"
			nil nil)
		       ("switch"
			"(cond (${1:case1} (${2:do-this}))\n      (${3:case2} (${4:do-this}))     \n      (t ${5:default}))\n$0\n"
			"switch" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/switch"
			nil nil)
		       ("rsc" "(run-shell-command $0)"
			"(run-shell-command ...)" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/rsc"
			nil nil)
		       ("mitlic"
			";;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-\n;;;\n;;; ${1:description}\n;;;\n;;; Copyright © ${2:`(format-time-string \"%Y\")`} `user-full-name` <`user-mail-address`>\n;;;\n;;; Permission is hereby granted, free of charge, to any person obtaining a\n;;; copy of this software and associated documentation files (the\n;;; \"Software\"), to deal in the Software without restriction, including\n;;; without limitation the rights to use, copy, modify, merge, publish,\n;;; distribute, sublicense, and/or sell copies of the Software, and to\n;;; permit persons to whom the Software is furnished to do so, subject to\n;;; the following conditions:\n;;;\n;;; The above copyright notice and this permission notice shall be included\n;;; in all copies or substantial portions of the Software.\n;;;\n;;; THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS\n;;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF\n;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND\n;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE\n;;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION\n;;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION\n;;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.\n\n$0"
			"MIT License Header" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/mitlic"
			nil nil)
		       ("mapcar" "(mapcar ${1:fnc} ${2:list})\n$0"
			"mapcar" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/mapcar"
			nil nil)
		       ("mapc" "(mapc ${1:fnc} ${2:list})\n$0" "mapc"
			nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/mapc"
			nil nil)
		       ("lt" "(let (${1:varlist})\n  ${0:body})"
			"(let ... ...)" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/lt"
			nil nil)
		       ("let" "(let ((${1:var} ${2:val}))\n  $0)\n"
			"let" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/let"
			nil nil)
		       ("labels"
			"(labels ((${1:name} (${2:args})${3:\n  \"${4:doc}\"}\n  ${5:body}))\n  $0)"
			"labels" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/labels"
			nil nil)
		       ("in-package" "(in-package #:${1:package})\n$0"
			"in-package" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/in-package"
			nil nil)
		       ("if" "(if ${1:test} ${2:then}${3: else})\n"
			"if" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/if"
			nil nil)
		       ("gnugpl"
			";;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; -*-\n;;;\n;;; ${1:description}\n;;;\n;;; Copyright © ${2:`(format-time-string \"%Y\")`} `user-full-name` <`user-mail-address`>\n;;;\n;;; ${3:This program$(prog1 yas-text (fill-paragraph))} is free software:\n;;; you can redistribute it and/or modify it under the terms of the GNU\n;;; General Public License as published by the Free Software Foundation,\n;;; either version 3 of the License, or (at your option) any later version.\n;;;\n;;; ${3:$(prog1 yas-text (fill-paragraph))} is distributed in the hope that\n;;; it will be useful, but WITHOUT ANY WARRANTY; without even the implied\n;;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the\n;;; GNU General Public License for more details.\n;;;\n;;; You should have received a copy of the GNU General Public License along\n;;; with this program. If not, see <http://www.gnu.org/licenses/>.\n\n$0"
			"GNU GPL 3 Header" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/gnugpl"
			nil nil)
		       ("format" "(format ${1:nil} ${2:str} $0)"
			"format" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/format"
			nil nil)
		       ("flet"
			"(flet ((${1:name} (${2:args})${3:\n  \"${4:doc}\"}\n  ${5:body}))\n  $0)"
			"flet" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/flet"
			nil nil)
		       ("evalal"
			"(eval-after-load ${1:file-name}\n  ${0:form})"
			"(eval-after-load ... ...)" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/evalal"
			nil nil)
		       ("etypecase"
			"(etypecase ${1:key-form}\n  (${2:match} ${3:result}))"
			"etypecase" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/etypecase"
			nil nil)
		       ("ecase"
			"(ecase ${1:key-form}\n  (${2:match} ${3:result}))"
			"ecase" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/ecase"
			nil nil)
		       ("dotimes"
			"(dotimes (${1:var} ${2:count}${3: result})\n  $0)"
			"dotimes" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/dotimes"
			nil nil)
		       ("dolist"
			"(dolist (${1:var} ${2:list}${3: result})\n  $0)"
			"dolist" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/dolist"
			nil nil)
		       ("do*"
			"(do* (${1:vars})\n     (${2:end-test-form}${3: result})\n  $0)"
			"do*" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/do_"
			nil nil)
		       ("do"
			"(do (${1:vars})\n    (${2:end-test-form}${3: result})\n  $0)"
			"do" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/do"
			nil nil)
		       ("dlhkd"
			"(dolist (hooked (list\n               ${1:mode-list}\n               ))\n  (add-hook '${0:mode-hook} 'hooked))\n"
			"(dolist (hooked (list ...)) (add-hook '... 'hooked))"
			nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/dlhkd"
			nil nil)
		       ("dlhk"
			"(dolist (hook (list\n               ${1:mode-list}\n               ))\n  (add-hook hook '${0:mode-hook}))"
			"(dolist (hook (list ... )) (add-hook hook '...))"
			nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/dlhk"
			nil nil)
		       ("dbind"
			"(destructuring-bind (${1:vars}) ${2:value}\n  $0)"
			"destructuring-bind" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/destructuring-bind"
			nil nil)
		       ("defvr"
			"(defvar ${1:variable-name} ${2:variable-varlue}\n  \"${0:document}\")"
			"(defvar ... ... \"...\")" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/defvr"
			nil nil)
		       ("defvar"
			"(defvar *${1:name}*${2: nil}${3:\n \"${4:doc}\"})\n$0"
			"defvar" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/defvar"
			nil nil)
		       ("defun"
			"(defun ${1:name} (${2:args})${3:\n  \"${4:doc}\"}\n  $0)"
			"defun" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/defun"
			nil nil)
		       ("deftype"
			"(deftype ${1:name} (${2:args})${3:\n  \"${4:doc}\"}\n  $0)"
			"deftype" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/deftype"
			nil nil)
		       ("deft"
			"(defun test ()\n  \"Just for test\"        \n  (interactive)\n  $0\n)\n"
			"(defun test () \"Just for test\" (interactive) ...)"
			nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/deft"
			nil nil)
		       ("defsystem"
			"(asdf:defsystem :${1:system}${2:\n  :version      \"${3:0.1.0}\"}${4:\n  :description  \"${5:description}\"}${6:\n  :author       \"${7:`user-full-name` <`user-mail-address`>}\"}${8:\n  :serial       t}${10:\n  :license      \"${11:GNU GPL, version 3}\"}${12:\n  :components   (${13:(:file \"file.lisp\")})}${14:\n  :depends-on   (${15:#:alexandria})})\n$0"
			"defsystem" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/defsystem"
			nil nil)
		       ("defstruct"
			"(defstruct ${1:name}${2:\n  \"${3:doc}\"}\n  ($0))"
			"defstruct" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/defstruct"
			nil nil)
		       ("defsc"
			"(define-stumpwm-command ${1:name} ()\n  $0)"
			"(define-stumpwm-command ... ...)" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/defsc"
			nil nil)
		       ("defpr"
			"(defparameter ${1:name} ${0:value})\n"
			"(defparameter ... ...)" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/defpr"
			nil nil)
		       ("defparameter"
			"(defparameter *${1:name}* ${2:nil}${3:\n \"${4:doc}\"})\n$0"
			"defparameter" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/defparameter"
			nil nil)
		       ("defpackage"
			"(defpackage   :${1:package}${2:\n  (:nicknames ${3:nicks})}${4:\n  (:use       ${5:packages})}${6:\n  (:shadow    ${7:packages})}${8:\n  (:export    ${9:packages})}${10:\n  (:documentation \"${11:doc}\")})\n$0"
			"defpackage" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/defpackage"
			nil nil)
		       ("defmethod"
			"(defmethod ${1:name} (${2:args})\n  $0)"
			"defmethod" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/defmethod"
			nil nil)
		       ("defmacro"
			"(defmacro ${1:name} (${2:args }${3:&body body})${4:\n  \"${5:doc}\"}\n  $0)"
			"defmacro" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/defmacro"
			nil nil)
		       ("define-symbol-macro"
			"(define-symbol-macro ${1:name} ${2:expansion})\n$0"
			"define-symbol-macro" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/define-symbol-macro"
			nil nil)
		       ("define-condition"
			"(define-condition ${1:name} (${2:parents})\n  ($0)${3:\n  (:documentation \"${4:doc}\")})"
			"define-condition" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/define-condition"
			nil nil)
		       ("define-compiler-macro"
			"(define-compiler-macro ${1:name} (${2:args})\n  $0)"
			"define-compiler-macro" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/define-compiler-macro"
			nil nil)
		       ("defgeneric"
			"(defgeneric ${1:name} (${2:args})${3:\n  (:documentation \"${4:doc}\")})\n$0"
			"defgeneric" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/defgeneric"
			nil nil)
		       ("deffu"
			"(defun ${1:Function Name} ($2)\n  \"${3:Function document}\"\n  $0)"
			"(defun ... (...) \"...\" ...)" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/deffu"
			nil nil)
		       ("deffc"
			"(defface ${1:face}\n  ${2:spec}\n  \"${3:doc}\"\n  ${0:args})\n"
			"(defface ... ... ... ...)" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/deffc"
			nil nil)
		       ("defconstant"
			"(defconstant +${1:name}+ ${2:nil}${3:\n \"${4:doc}\"})\n$0"
			"defconstant" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/defconstant"
			nil nil)
		       ("defcm"
			"(defcustom ${1:symbol} ${2:value}\n  \"${3:doc}\"\n  ${0:body})\n"
			"(defcustom ... ... ... ...)" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/defcm"
			nil nil)
		       ("defclass"
			"(defclass ${1:name} (${2:parents})\n  ($0)${3:\n  (:documentation \"${4:doc}\")})"
			"defclass" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/defclass"
			nil nil)
		       ("defav"
			"(defadvice ${1:function-name} (${2:args})\n  \"${3:advice-document}\"\n  (${0:advice-body})\n)"
			"(defadvice ... ... ...)" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/defav"
			nil nil)
		       ("defas" "(defalias ${1:symbol} ${0:define})"
			"(defalias ... ...)" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/defas"
			nil nil)
		       ("ctypecase"
			"(ctypecase ${1:key-form}\n  (${2:match} ${3:result}))"
			"ctypecase" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/ctypecase"
			nil nil)
		       ("cond"
			"(cond (${1:test} ${2:then})\n      (t ${3:else}))"
			"cond" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/cond"
			nil nil)
		       ("ccase"
			"(ccase ${1:key-form}\n  (${2:match} ${3:result}))"
			"ccase" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/ccase"
			nil nil)
		       ("case"
			"(case ${1:key-form}\n  (${2:match} ${3:result})${4:\n  (t ${5:otherwise})})"
			"case" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/case"
			nil nil)
		       ("assert"
			"(assert ${1:assertion} (${2:vars-to-change})\n  \"${3:string}\"\n  ${4:mentioned-vars})"
			"assert" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/assert"
			nil nil)
		       ("addul"
			"(unless ${1:conditional}\n  ${0:body})"
			"(unless ... ...)" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/addul"
			nil nil)
		       ("addtl"
			"(add-to-list ${1:list-var} ${0:element})"
			"(add-to-list ... ...)" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/addtl"
			nil nil)
		       ("addt"
			";;;;;;;;;;;;;;;;;;;;;;;;;;;;;; $1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n$0\n"
			";;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ... ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;"
			nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/addt"
			nil nil)
		       ("addse"
			"(setf ${1:variable-name} ${0:variable-value})"
			"(setq ... ...)" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/addse"
			nil nil)
		       ("addrq" "(require '${0:library-name})"
			"(require '...)" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/addrq"
			nil nil)
		       ("addpr" "(provide '${0:library-name})"
			"(provide '...)" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/addpr"
			nil nil)
		       ("addlt"
			";;;;;;;;;;;;;;;;;;;; $1 ;;;;;;;;;;;;;;;;;;;;\n$0\n"
			";;;;;;;;;;;;;;;;;;;; ... ;;;;;;;;;;;;;;;;;;;;"
			nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/addlt"
			nil nil)
		       ("addlk"
			"(define-key ${1:some-mode-map} (kbd \"${2:some-key}\") ${0:some-command})"
			"(define-key ... (kbd \"...\") ...)" nil nil
			nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/addlk"
			nil nil)
		       ("addip" "(in-package :${0:library-name})\n"
			"(in-package :...)" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/addip"
			nil nil)
		       ("addif" "(if ${1:conditional}\n    ${2:then})"
			"(if ... ... ...)" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/addif"
			nil nil)
		       ("addic" "((= c ?${1:char}) (${0:command}))"
			"((= c ...) (...))" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/addic"
			nil nil)
		       ("addhk" "(add-hook ${1:hook} ${0:function})"
			"(add-hook ... ...)" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/addhk"
			nil nil)
		       ("addguk"
			"(global-unset-key (kbd ${0:key-value}))"
			"(global-unset-key ...)" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/addguk"
			nil nil)
		       ("addgk"
			"(global-set-key (kbd \"${1:some-key}\") ${0:some-command})"
			"(global-set-key (kbd \"...\") ...)" nil nil
			nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/addgk"
			nil nil)
		       ("addc" ";;; ### $1 ###\n;;; --- $2\n$0"
			";;; ### ... ### ..." nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/addc"
			nil nil)
		       ("addbku"
			"(basic-unset-key-list ${1:keymap} '(\"${0:key-list}\"))"
			"(basic-unset-key-list ... ...)" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/addbku"
			nil nil)
		       ("addbk" "(basic-set-key-alist ${0:rest})"
			"(basic-set-key-alist ...)" nil nil nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/addbk"
			nil nil)
		       ("addal"
			"(autoload '${1:function-name} \"${2:file-name}\" \"${3:document}\" ${0:interactive})"
			"(autoload '... \"...\" \"...\" ...)" nil nil
			nil
			"/home/tychoish/.emacs.d/snippets/lisp-mode/addal"
			nil nil)))


;;; Do not edit! File generated at Fri Aug  1 09:43:16 2025
