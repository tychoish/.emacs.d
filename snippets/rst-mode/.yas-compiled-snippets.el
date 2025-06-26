;;; Compiled snippets and support files for `rst-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'rst-mode
		     '(("t" "${1:$(make-string (string-width yas-text) ?\\=)}\n${1:Title}\n${1:$(make-string (string-width yas-text) ?\\=)}\n\n$0\n" "Document title" nil nil nil "/home/tychoish/.emacs.d/snippets/rst-mode/t" nil nil)
		       ("sec" "${1:Section}\n${1:$(make-string (string-width yas-text) ?\\-)}\n\n$0\n" "Section title" nil nil nil "/home/tychoish/.emacs.d/snippets/rst-mode/sec" nil nil)
		       ("pre" "\\``${1:Code}\\`` $0\n" "Inline Literals" nil nil nil "/home/tychoish/.emacs.d/snippets/rst-mode/pre" nil nil)
		       ("p" "\\``${1:Code}\\`` $0\n" "Inline Literals" nil nil nil "/home/tychoish/.emacs.d/snippets/rst-mode/p" nil nil)
		       ("op" ".. operator:: $1\n   $0\n" "Operator Directive" nil nil nil "/home/tychoish/.emacs.d/snippets/rst-mode/op" nil nil)
		       ("ol" "${1:1}. ${2:Text}\n${1:$(number-to-string (1+ (string-to-number text)))}. $0\n" "Ordered List" nil nil nil "/home/tychoish/.emacs.d/snippets/rst-mode/ol" nil nil)
		       ("note" "[#$1]_\n\n.. [#$1] $0\n\n" "note" nil nil nil "/home/tychoish/.emacs.d/snippets/rst-mode/note" nil nil)
		       ("link" "\\`${1:Link Text} <${2:URL}>\\`_ $0\n" "Link" nil nil nil "/home/tychoish/.emacs.d/snippets/rst-mode/link" nil nil)
		       ("limit" ".. _limit-$1:\n\n.. limit:: $2\n\n   $0\n" "insert limit template" nil nil nil "/home/tychoish/.emacs.d/snippets/rst-mode/limit" nil nil)
		       ("issue" ":issue:\\`${1:Link Text}\\`$0\n" "Issue Link" nil nil nil "/home/tychoish/.emacs.d/snippets/rst-mode/issue" nil nil)
		       ("img" ".. image:: ${1:Image Path}\n   :alt: ${2:Alt Text}\n   :target: ${3:Link Target}\n$1\n" "Image" nil nil nil "/home/tychoish/.emacs.d/snippets/rst-mode/img" nil nil)
		       ("h4" "${1:Header 4}\n${1:$(make-string (string-width yas-text) ?\\`)}\n$0\n" "Header 4 (`) (Doc Baker Specific)" nil nil nil "/home/tychoish/.emacs.d/snippets/rst-mode/h4" nil nil)
		       ("h3" "${1:Header 3}\n${1:$(make-string (string-width yas-text) ?\\~)}\n$0\n" "Header 3 (~) (Doc Baker Specific)" nil nil nil "/home/tychoish/.emacs.d/snippets/rst-mode/h3" nil nil)
		       ("h2" "${1:Header 2}\n${1:$(make-string (string-width yas-text) ?\\-)}\n$0\n" "Header 2 (-)" nil nil nil "/home/tychoish/.emacs.d/snippets/rst-mode/h2" nil nil)
		       ("h1" "${1:Header 1}\n${1:$(make-string (string-width yas-text) ?\\=)}\n$0\n" "Header 1 (=)" nil nil nil "/home/tychoish/.emacs.d/snippets/rst-mode/h1" nil nil)
		       ("fn" "[#$1]_\n\n.. [#$1] $0\n\n" "footnote" nil nil nil "/home/tychoish/.emacs.d/snippets/rst-mode/fn" nil nil)
		       ("doc" ":doc:\\`$1\\`$0\n" "doc" nil nil nil "/home/tychoish/.emacs.d/snippets/rst-mode/doc" nil nil)
		       ("dd" ".. default-domain:: $1\n   $0\n" "default domain" nil nil nil "/home/tychoish/.emacs.d/snippets/rst-mode/dd" nil nil)
		       ("cmd" ".. command:: $1\n   $0\n" "Command Directive" nil nil nil "/home/tychoish/.emacs.d/snippets/rst-mode/cmd" nil nil)
		       ("chap" "${1:Chapter}\n${1:$(make-string (string-width text) ?\\=)}\n\n$0" "Chapter title" nil nil nil "/home/tychoish/.emacs.d/snippets/rst-mode/chap" nil nil)
		       ("cbsh" ".. code-block:: sh\n   $0\n" "sh code block" nil nil nil "/home/tychoish/.emacs.d/snippets/rst-mode/cbsh" nil nil)
		       ("cbpy" ".. code-block:: python\n   $0\n" "python code block" nil nil nil "/home/tychoish/.emacs.d/snippets/rst-mode/cbpy" nil nil)
		       ("cbjs" ".. code-block:: javascript\n   $0\n" "javascript code block" nil nil nil "/home/tychoish/.emacs.d/snippets/rst-mode/cbjs" nil nil)
		       ("``" "\\``${1:Code}\\`` $0\n" "Inline Literals" nil nil nil "/home/tychoish/.emacs.d/snippets/rst-mode/``" nil nil)
		       ("__" "**${1:Text}** $0\n" "Strong" nil nil nil "/home/tychoish/.emacs.d/snippets/rst-mode/__" nil nil)
		       ("_" "_${1:Text}_ $0\n" "Emphasis" nil nil nil "/home/tychoish/.emacs.d/snippets/rst-mode/_" nil nil)
		       ("-" "- ${1:Text}\n-$0\n" "Unordered List" nil nil nil "/home/tychoish/.emacs.d/snippets/rst-mode/-" nil nil)
		       ("**" "**${1:Text}** $0\n" "Strong" nil nil nil "/home/tychoish/.emacs.d/snippets/rst-mode/**" nil nil)
		       ("*" "*${1:Text}* $0\n" "Emphasis" nil nil nil "/home/tychoish/.emacs.d/snippets/rst-mode/*" nil nil)))


;;; Do not edit! File generated at Mon Dec 30 15:17:58 2024
