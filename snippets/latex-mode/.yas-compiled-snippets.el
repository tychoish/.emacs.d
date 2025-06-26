;;; Compiled snippets and support files for `latex-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'latex-mode
		     '(("letter" "\\documentclass[12pt]{letter}\n\n\\signature{$0}\n\\address{\n%Sender's Address use \\\\ for line breaks\n\n% Optional Package\n\n\\usepackage{fullpage}\n\n% Packages required by code\n\n% Packages always used\n\\usepackage{ucs}\n\\usepackage[utf8x]{inputenc}\n\\usepackage{hyperref}\n\\usepackage{xspace}\n\\usepackage[usenames,dvipsnames]{color}\n\\hypersetup{colorlinks=true,urlcolor=blue}\n\n\\begin{document} \n\\begin{letter}{\n%Address of recepiant\n	}\n\n\\opening{\n%Specific Opening Here\n}\n\n$1\n%Body of the letter\n\n\\closing{\n%Specific Closing\n}\n\\end{letter}\n\\end{document}" "letter" nil nil nil "/home/tychoish/.emacs.d/snippets/latex-mode/letter" nil nil)
		       ("apa" "\\documentclass[man]{apa}\n% Packages required to support encoding\n\\usepackage{ucs}\n\\usepackage[utf8x]{inputenc}\n% Packages required by code\n\\title{}\n\\shorttitle{%header title$}\n\\author{$1}\n\\affiliation{Webster University}\n\\rightheader{Running head: title}\n% Packages always used\n\\usepackage{hyperref}\n\\usepackage{xspace}\n%\\usepackage{apacite}\n\\usepackage{natbib}\n\\bibliographystyle{apa-good}\n\n\\begin{document}\n\\maketitle \n\n%body of paper goes here\n$0\n\n\\newpage\n\\bibliography{AC.references}\n\\end{document}\n" "apa" nil nil nil "/home/tychoish/.emacs.d/snippets/latex-mode/apa" nil nil)))


;;; Do not edit! File generated at Mon Dec 30 15:17:58 2024
