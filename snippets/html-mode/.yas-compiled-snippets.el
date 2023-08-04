;;; Compiled snippets and support files for `html-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'html-mode
		     '(("ul.id" "<ul id=\"$1\">\n  $0\n</ul>" "<ul id=\"...\">...</ul>" nil
			("list")
			nil "/home/tychoish/.emacs.d/snippets/html-mode/ul.id" nil nil)
		       ("ul.class" "<ul class=\"$1\">\n  $0\n</ul>" "<ul class=\"...\">...</ul>" nil
			("list")
			nil "/home/tychoish/.emacs.d/snippets/html-mode/ul.class" nil nil)
		       ("ul" "<ul>\n  $0\n</ul>" "<ul>...</ul>" nil
			("list")
			nil "/home/tychoish/.emacs.d/snippets/html-mode/ul" nil nil)
		       ("tycho-section" "<div class='section'>\n  <h1 id='research'>$1</h1>\n  <p>$0</p>\n</div>\n" "tycho section" nil nil nil "/home/tychoish/.emacs.d/snippets/html-mode/tycho-section" nil nil)
		       ("tr" "<tr>\n  $0\n</tr>" "<tr>...</tr>" nil
			("table")
			nil "/home/tychoish/.emacs.d/snippets/html-mode/tr" nil nil)
		       ("title" "<title>$1</title>" "<title>...</title>" nil nil nil "/home/tychoish/.emacs.d/snippets/html-mode/title" nil nil)
		       ("td" "<td$1>$2</td>" "<td>...</td>" nil
			("table")
			nil "/home/tychoish/.emacs.d/snippets/html-mode/td" nil nil)
		       ("table" "<table width=\"$1\" cellspacing=\"$2\" cellpadding=\"$3\" border=\"$4\">\n  $0\n</table>" "<table ...>...</table>" nil
			("table")
			nil "/home/tychoish/.emacs.d/snippets/html-mode/table" nil nil)
		       ("style" "<style type=\"text/css\" media=\"${1:screen}\">\n  $0\n</style>" "<style type=\"text/css\" media=\"...\">...</style>" nil nil nil "/home/tychoish/.emacs.d/snippets/html-mode/style" nil nil)
		       ("strong" "<strong>$1</strong>\n" "<strong>...</strong>" nil nil nil "/home/tychoish/.emacs.d/snippets/html-mode/strong" nil nil)
		       ("span.id" "<span id=\"$1\">$2</span>" "<span id=\"...\">...</span>" nil nil nil "/home/tychoish/.emacs.d/snippets/html-mode/span.id" nil nil)
		       ("span.class" "<span class=\"$1\">$2</span>" "<span class=\"...\">...</span>" nil nil nil "/home/tychoish/.emacs.d/snippets/html-mode/span.class" nil nil)
		       ("span" "<span>$1</span>" "<span>...</span>" nil nil nil "/home/tychoish/.emacs.d/snippets/html-mode/span" nil nil)
		       ("quote" "<blockquote>\n  $1\n</blockquote>" "<blockquote>...</blockquote>" nil nil nil "/home/tychoish/.emacs.d/snippets/html-mode/quote" nil nil)
		       ("pre" "<pre>\n  $0\n</pre>" "<pre>...</pre>" nil nil nil "/home/tychoish/.emacs.d/snippets/html-mode/pre" nil nil)
		       ("p" "<p>$1</p>" "<p>...</p>" nil nil nil "/home/tychoish/.emacs.d/snippets/html-mode/p" nil nil)
		       ("ol.id" "<ol id=\"$1\">\n  $0\n</ol>" "<ol id=\"...\">...</ol>" nil
			("list")
			nil "/home/tychoish/.emacs.d/snippets/html-mode/ol.id" nil nil)
		       ("ol.class" "<ol class=\"$1\">\n  $0\n</ol>" "<ol class=\"...\">...</ol>" nil
			("list")
			nil "/home/tychoish/.emacs.d/snippets/html-mode/ol.class" nil nil)
		       ("ol" "<ol>\n  $0\n</ol>" "<ol>...</ol>" nil
			("list")
			nil "/home/tychoish/.emacs.d/snippets/html-mode/ol" nil nil)
		       ("li.class" "<li class=\"$1\">$2</li>" "<li class=\"...\">...</li>" nil
			("list")
			nil "/home/tychoish/.emacs.d/snippets/html-mode/li.class" nil nil)
		       ("li" "<li>$1</li>" "<li>...</li>" nil
			("list")
			nil "/home/tychoish/.emacs.d/snippets/html-mode/li" nil nil)
		       ("input" "<input type=\"$1\" name=\"$2\" value=\"$3\" />" "<input ... />" nil nil nil "/home/tychoish/.emacs.d/snippets/html-mode/input" nil nil)
		       ("img" "<img src=\"$1\" class=\"$2\" alt=\"$3\" />" "<img src=\"...\" class=\"...\" alt=\"...\" />" nil nil nil "/home/tychoish/.emacs.d/snippets/html-mode/img" nil nil)
		       ("href" "<a href=\"$1\">$2</a>" "<a href=\"...\">...</a>" nil nil nil "/home/tychoish/.emacs.d/snippets/html-mode/href" nil nil)
		       ("hr" "<hr />\n" "<hr />" nil nil nil "/home/tychoish/.emacs.d/snippets/html-mode/hr" nil nil)
		       ("head" "<head>\n  $0\n</head>" "<head>...</head>" nil nil nil "/home/tychoish/.emacs.d/snippets/html-mode/head" nil nil)
		       ("h6" "<h6>$1</h6>" "<h6>...</h6>" nil
			("header")
			nil "/home/tychoish/.emacs.d/snippets/html-mode/h6" nil nil)
		       ("h5" "<h5>$1</h5>" "<h5>...</h5>" nil
			("header")
			nil "/home/tychoish/.emacs.d/snippets/html-mode/h5" nil nil)
		       ("h4" "<h4>$1</h4>" "<h4>...</h4>" nil
			("header")
			nil "/home/tychoish/.emacs.d/snippets/html-mode/h4" nil nil)
		       ("h3" "<h3>$1</h3>" "<h3>...</h3>" nil
			("header")
			nil "/home/tychoish/.emacs.d/snippets/html-mode/h3" nil nil)
		       ("h2" "<h2>$1</h2>" "<h2>...</h2>" nil
			("header")
			nil "/home/tychoish/.emacs.d/snippets/html-mode/h2" nil nil)
		       ("h1" "<h1>$1</h1>" "<h1>...</h1>" nil
			("header")
			nil "/home/tychoish/.emacs.d/snippets/html-mode/h1" nil nil)
		       ("em" "<em>$1</em>\n" "<em>...</em>" nil nil nil "/home/tychoish/.emacs.d/snippets/html-mode/em" nil nil)
		       ("doctype.xhml1" "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Frameset//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-frameset.dtd\">" "DocType XHTML 1.0 frameset" nil
			("meta")
			nil "/home/tychoish/.emacs.d/snippets/html-mode/doctype.xhml1" nil nil)
		       ("dl.id" "<dl id=\"$1\">\n    $0\n</dl>" "<dl> ... </dl>" nil
			("list")
			nil "/home/tychoish/.emacs.d/snippets/html-mode/dl.id" nil nil)
		       ("div.id-class" "<div id=\"$1\" class=\"$2\">\n  $0\n</div>" "<div id=\"...\" class=\"...\">...</div>" nil nil nil "/home/tychoish/.emacs.d/snippets/html-mode/div.id-class" nil nil)
		       ("div.id" "<div id=\"$1\">\n  $0\n</div>" "<div id=\"...\">...</div>" nil nil nil "/home/tychoish/.emacs.d/snippets/html-mode/div.id" nil nil)
		       ("div.class" "<div class=\"$1\">\n  $0\n</div>" "<div class=\"...\">...</div>" nil nil nil "/home/tychoish/.emacs.d/snippets/html-mode/div.class" nil nil)
		       ("div" "<div$1>$0</div>" "<div...>...</div>" nil nil nil "/home/tychoish/.emacs.d/snippets/html-mode/div" nil nil)
		       ("code.class" "<code class=\"$1\">\n  $0\n</code>" "<code class=\"...\">...</code>" nil nil nil "/home/tychoish/.emacs.d/snippets/html-mode/code.class" nil nil)
		       ("code" "<code>\n  $0\n</code>" "<code>...</code>" nil nil nil "/home/tychoish/.emacs.d/snippets/html-mode/code" nil nil)
		       ("br" "<br />\n" "<br />" nil nil nil "/home/tychoish/.emacs.d/snippets/html-mode/br" nil nil)
		       ("body" "<body$1>\n  $0\n</body>" "<body>...</body>" nil nil nil "/home/tychoish/.emacs.d/snippets/html-mode/body" nil nil)))


;;; Do not edit! File generated at Sun Jul  9 00:25:25 2023
