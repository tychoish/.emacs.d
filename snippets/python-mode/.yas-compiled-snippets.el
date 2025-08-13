;;; "Compiled" snippets and support files for `python-mode'  -*- lexical-binding:t -*-
;;; Snippet definitions:
;;;
(yas-define-snippets 'python-mode
		     '(("wtime" "with Timer($1):\n    $0\n" "wtime"
			nil nil nil
			"/home/tychoish/.emacs.d/snippets/python-mode/wtime"
			nil nil)
		       ("try.exceptfinally"
			"try:\n	${1:pass}\nexcept ${2:Exception}, ${3:e}:\n	${4:raise $3}\nfinally:\n	${5:pass}"
			"try.exceptfinally" nil nil nil
			"/home/tychoish/.emacs.d/snippets/python-mode/try.exceptfinally"
			nil nil)
		       ("try.exceptelsefinally"
			"try:\n	${1:pass}\nexcept ${2:Exception}, ${3:e}}:\n	${4:raise}\nelse:\n	${5:pass}\nfinally:\n	${6:pass}"
			"try.exceptelsefinally" nil nil nil
			"/home/tychoish/.emacs.d/snippets/python-mode/try.exceptelsefinally"
			nil nil)
		       ("try.exceptelse"
			"try:\n	${1:pass}\nexcept ${2:Exception}, ${3:e}:\n	${4:raise $3}\nelse:\n	${5:pass}"
			"try.exceptelse" nil nil nil
			"/home/tychoish/.emacs.d/snippets/python-mode/try.exceptelse"
			nil nil)
		       ("try.except"
			"try:\n	${1:pass}\nexcept ${2:Exception}, ${3:e}:\n	${4:raise $3}"
			"try.except" nil nil nil
			"/home/tychoish/.emacs.d/snippets/python-mode/try.except"
			nil nil)
		       ("tr" "import pdb; pdb.set_trace()" "trace" nil
			("debug") nil
			"/home/tychoish/.emacs.d/snippets/python-mode/trace"
			nil nil)
		       ("timer"
			"import time\nclass Timer():\n    def __init__(self, name=None):\n        if name is None:\n            self.name = 'task'\n        else:\n            self.name = name\n    def __enter__(self):\n        self.start = time.time()\n    def __exit__(self, *args):\n        print( 'time elapsed for {0} was: {1}'.format(self.name, str(time.time() - self.start)) )\n"
			"timer class" nil nil nil
			"/home/tychoish/.emacs.d/snippets/python-mode/timer"
			nil nil)
		       ("testcase"
			"class ${1:TestCase}(${2:unittest.TestCase}):\n    $0\n"
			"unittest.TestCase normal" nil nil nil
			"/home/tychoish/.emacs.d/snippets/python-mode/testcase"
			nil nil)
		       ("propsg"
			"def _set_${1:foo}(self, value):\n    self._$1 = value\n\ndef _get_$1(self):\n    return self._$1\n\n$1 = property(_get_$1, _set_$1)\n\n$0"
			"_get_foo ... _set_foo ... foo=property(...)"
			nil nil nil
			"/home/tychoish/.emacs.d/snippets/python-mode/propsg"
			nil nil)
		       ("propg"
			"def _get_${1:foo}(self):\n    return self._$1\n\n$1 = property(_get_$1)\n\n$0"
			"_get_foo ... foo=property(...)" nil nil nil
			"/home/tychoish/.emacs.d/snippets/python-mode/propg"
			nil nil)
		       ("prop.fun"
			"def ${1:foo}():\n   doc = \"\"\"${2:Doc string}\"\"\"\n   def fget(self):\n       return self._$1\n   def fset(self, value):\n       self._$1 = value\n   def fdel(self):\n       del self._$1\n   return locals()\n$1 = property(**$1())\n\n$0\n"
			"prop" nil nil
			((yas/indent-line 'fixed)
			 (yas/wrap-around-region 'nil))
			"/home/tychoish/.emacs.d/snippets/python-mode/prop.fun"
			nil nil)
		       ("pprint"
			"def pprint(doc):\n    import json\n\n    print(json.dumps(doc, indent=3))\n"
			"pprint" nil nil nil
			"/home/tychoish/.emacs.d/snippets/python-mode/pprint"
			nil nil)
		       ("param" ":param $1: $0" "param"
			'force-in-comment nil nil
			"/home/tychoish/.emacs.d/snippets/python-mode/param"
			nil nil)
		       ("itr" "import ipdb; ipdb.set_trace()"
			"ipdb trace" nil ("debug") nil
			"/home/tychoish/.emacs.d/snippets/python-mode/ipdbdebug"
			nil nil)
		       ("init-logging"
			"import logging\nimport os.path\n\nlogger = logging.getLogger(os.path.basename(__file__))\n"
			"init-logging" nil nil nil
			"/home/tychoish/.emacs.d/snippets/python-mode/init-logging"
			nil nil)
		       ("defm"
			"def ${1:name}(self, $2):\n    \"\"\"$3\n    ${2:$\n    (let* ((indent\n            (concat \"\\n\" (make-string (current-column) 32)))\n           (args\n            (mapconcat\n             '(lambda (x)\n                (if (not (string= (nth 0 x) \"\"))\n                    (concat \"- \" (char-to-string 96) (nth 0 x)\n                            (char-to-string 96) \":\")))\n             (mapcar\n              '(lambda (x)\n                 (mapcar\n                  '(lambda (x)\n                     (replace-regexp-in-string \"[[:blank:]]*$\" \"\"\n                      (replace-regexp-in-string \"^[[:blank:]]*\" \"\" x)))\n                  x))\n              (mapcar '(lambda (x) (split-string x \"=\"))\n                      (split-string text \",\")))\n             indent)))\n      (if (string= args \"\")\n          (make-string 3 34)\n        (mapconcat\n         'identity\n         (list \"\" \"Arguments:\" args (make-string 3 34))\n         indent)))\n    }\n    $0"
			"defm" nil nil nil
			"/home/tychoish/.emacs.d/snippets/python-mode/defm"
			nil nil)
		       ("def"
			"def ${1:name}($2):\n    \"\"\"$3\n    ${2:$\n      (let* \n        ((indent\n            (concat \"\\n\" (make-string (current-column) 32)))\n           (args\n            (mapconcat\n             '(lambda (x)\n                (if (not (string= (nth 0 x) \"\"))\n                    (concat \"- \" (char-to-string 96) (nth 0 x)\n                            (char-to-string 96) \":\")))\n             (mapcar\n              '(lambda (x)\n                 (mapcar\n                  '(lambda (x)\n                     (replace-regexp-in-string \"[[:blank:]]*$\" \"\"\n                      (replace-regexp-in-string \"^[[:blank:]]*\" \"\" x)))\n                  x))\n              (mapcar '(lambda (x) (split-string x \"=\"))\n                      (split-string text \",\")))\n             indent)))\n      (if (string= args \"\")\n          (make-string 3 34)\n        (mapconcat\n         'identity\n         (list \"\" \"Arguments:\" args (make-string 3 34))\n         indent)))\n    }\n    $0"
			"def" nil nil nil
			"/home/tychoish/.emacs.d/snippets/python-mode/def"
			nil nil)
		       ("class"
			"class ${1:ClassName}(${2:object}):\n    \"\"\"$3\n    \"\"\"\n\n    def __init__(self, $4):\n        \"\"\"$5\n        ${4:$\n        (let* ((indent\n                (concat \"\\n\" (make-string (current-column) 32)))\n               (args\n                (mapconcat\n                 '(lambda (x)\n                    (if (not (string= (nth 0 x) \"\"))\n                        (concat \"- \" (char-to-string 96) (nth 0 x)\n                                (char-to-string 96) \":\")))\n                 (mapcar\n                  '(lambda (x)\n                     (mapcar\n                      (lambda (x)\n                        (replace-regexp-in-string \"[[:blank:]]*$\" \"\"\n                         (replace-regexp-in-string \"^[[:blank:]]*\" \"\" x))) x))\n                  (mapcar '(lambda (x) (split-string x \"=\"))\n                          (split-string text \",\")))\n                 indent)))\n          (if (string= args \"\")\n              (make-string 3 34)\n            (mapconcat\n             'identity\n             (list \"\" \"Arguments:\" args (make-string 3 34))\n             indent)))\n        }\n        ${4:$\n        (mapconcat\n         '(lambda (x)\n            (if (not (string= (nth 0 x) \"\"))\n                (concat \"self._\" (nth 0 x) \" = \" (nth 0 x))))\n         (mapcar\n          '(lambda (x)\n             (mapcar\n              '(lambda (x)\n                 (replace-regexp-in-string \"[[:blank:]]*$\" \"\"\n                  (replace-regexp-in-string \"^[[:blank:]]*\" \"\" x)))\n              x))\n          (mapcar '(lambda (x) (split-string x \"=\"))\n                  (split-string text \",\")))\n         (concat \"\\n\" (make-string (current-column) 32)))\n        }\n        $0"
			"class" nil nil nil
			"/home/tychoish/.emacs.d/snippets/python-mode/class"
			nil nil)
		       ("__" "__${init}__" "__...__" nil nil nil
			"/home/tychoish/.emacs.d/snippets/python-mode/__"
			nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'python-mode
		     '(("defs"
			"def ${1:mname}(self${2/([^,])?.*/(?1:, )/}${2:arg}):\n	${3:pass}"
			"New Method" nil nil nil
			"/home/tychoish/.emacs.d/snippets/python-mode/Declarations/New Method.yasnippet"
			nil nil)
		       ("def"
			"def ${1:fname}(${2:`if [ \"$TM_CURRENT_LINE\" != \"\" ]\n				# poor man's way ... check if there is an indent or not\n				# (cuz we would have lost the class scope by this point)\n				then\n					echo \"self\"\n				fi`}):\n	${3/.+/\"\"\"/}${3:docstring for $1}${3/.+/\"\"\"\\n/}${3/.+/\\t/}${0:pass}"
			"New Function" nil nil nil
			"/home/tychoish/.emacs.d/snippets/python-mode/Declarations/New Function.yasnippet"
			nil nil)
		       ("class"
			"class ${1:ClassName}(${2:object}):\n	${3/.+/\"\"\"/}${3:docstring for $1}${3/.+/\"\"\"\\n/}${3/.+/\\t/}def __init__(self${4/([^,])?(.*)/(?1:, )/}${4:arg}):\n		${5:super($1, self).__init__()}\n${4/(\\A\\s*,\\s*\\Z)|,?\\s*([A-Za-z_][a-zA-Z0-9_]*)\\s*(=[^,]*)?(,\\s*|$)/(?2:\\t\\tself.$2 = $2\\n)/g}		$0"
			"New Class" nil nil nil
			"/home/tychoish/.emacs.d/snippets/python-mode/Declarations/New Class.yasnippet"
			nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'python-mode
		     '(("ifmain"
			"if __name__ == '__main__':\n	${1:main()}$0"
			"if __name__ == '__main__'" nil nil nil
			"/home/tychoish/.emacs.d/snippets/python-mode/Idioms/if __name__ == '__main__'.yasnippet"
			nil nil)
		       ("tryef"
			"try:\n	${1:pass}\nexcept ${2:Exception}, ${3:e}:\n	${4:raise $3}\nfinally:\n	${5:pass}"
			"Try/Except/Finally" nil nil nil
			"/home/tychoish/.emacs.d/snippets/python-mode/Idioms/Try:Except:Finally.yasnippet"
			nil nil)
		       ("tryeef"
			"try:\n	${1:pass}\nexcept${2: ${3:Exception}, ${4:e}}:\n	${5:raise}\nelse:\n	${6:pass}\nfinally:\n	${7:pass}"
			"Try/Except/Else/Finally" nil nil nil
			"/home/tychoish/.emacs.d/snippets/python-mode/Idioms/Try:Except:Else:Finally.yasnippet"
			nil nil)
		       ("tryee"
			"try:\n	${1:pass}\nexcept ${2:Exception}, ${3:e}:\n	${4:raise $3}\nelse:\n	${5:pass}"
			"Try/Except/Else" nil nil nil
			"/home/tychoish/.emacs.d/snippets/python-mode/Idioms/Try:Except:Else.yasnippet"
			nil nil)
		       ("try"
			"try:\n	${1:pass}\nexcept ${2:Exception}, ${3:e}:\n	${4:raise $3}"
			"Try/Except" nil nil nil
			"/home/tychoish/.emacs.d/snippets/python-mode/Idioms/Try:Except.yasnippet"
			nil nil)
		       ("property"
			"def ${1:foo}():\n    doc = \"${2:The $1 property.}\"\n    def fget(self):\n        ${3:return self._$1}\n    def fset(self, value):\n        ${4:self._$1 = value}\n    def fdel(self):\n        ${5:del self._$1}\n    return locals()\n$1 = property(**$1())$0"
			"New Property" nil nil nil
			"/home/tychoish/.emacs.d/snippets/python-mode/Idioms/New Property.yasnippet"
			nil nil)))


;;; Snippet definitions:
;;;
(yas-define-snippets 'python-mode
		     '(("." "self." "self" nil nil nil
			"/home/tychoish/.emacs.d/snippets/python-mode/Insert/self.yasnippet"
			nil nil)
		       ("__" "__${1:init}__" "__magic__" nil nil nil
			"/home/tychoish/.emacs.d/snippets/python-mode/Insert/__magic__.yasnippet"
			nil nil)))


;;; Do not edit! File generated at Fri Aug  1 09:43:16 2025
