=====================================
``ext`` -- Emacs Extensions Directory
=====================================

Background
----------

Although the distinction is arbitrary, the ``ext`` directory holds "packages"
rather than configuration: discrete bits of functionality that could
(theoretically) live in their own repositories or end up in MELPA. Many of
these are old pieces of lisp unearthed from the internet in the pre-MELPA days,
and the headers contain attribution and licensing information. 

All files in this directory should be loaded and configured using
``use-package`` forms in the ``.emacs.d/conf/programming.el`` file, and while
these will get loaded after ``.emacs.d/conf/local-functions.el`` and can use
these functions, my tendency has been to reduce the move things from
``local-functions`` into their own packages, when that makes sense.

Overview
--------

- ``blogging`` are a collection of functions and helpers that I use for
  editing and producing my blog. I believe I got them from `metajack
  <https://github.com/metajack>`_, once upon a time, though I think I've
  edited them beyond recognition: they've certainly lasted longer than the
  publishing tools that I've used.

- ``notify`` is a simpler version of `alert
  <https://github.com/jwiegley/alert>`_ that isn't as good, but I found it
  easier to modify, and so I continue to use it. 

- ``revbufs`` reverts all buffers in an intelligent way, and I've not found a
  reasonable replacement, and I use it all of the time. 
  
- ``helm-slime``, ``EEC-yank``, and ``flycheck-aspell`` all do what you'd
  expect and I don't use them much, but they're not in MELPA.
