=================================
``lisp/`` -- Emacs Lisp Directory
=================================

Background
----------

Although the distinction is arbitrary, the ``ext`` directory holds "packages"
rather than configuration: discrete bits of functionality that could
(theoretically) live in their own repositories or end up in MELPA. Many of
these are old pieces of lisp unearthed from the internet in the pre-MELPA days,
and the headers contain attribution and licensing information. 

All files in this directory should be loaded and configured using
``use-package`` forms in the ``.emacs.d/tychoish-core.el`` file.

Overview
--------

External
~~~~~~~~

- ``revbufs`` reverts all buffers in an intelligent way, and I've not found a
  reasonable replacement, and I use it all of the time. 
  
- ``helm-slime``, ``erc-yank``, and ``flycheck-aspell`` all do what you'd
  expect and I don't use them much, but they're not in MELPA.

Internal
~~~~~~~~

The packages with the ``tychoish-`` prefix are largely independent slices of
configuration and functionality.

- ``core`` holds all ``use-package`` forms that handle the entire
  configuration.
  
- ``setuputils`` defines functions used during the initial setup of the
  configuration.

- ``theme`` provides tools for managing and configuring themes.

- ``editing`` collects configuration and several minor operations related to
  text editing and manipulation.

- ``grep`` provides find-grep like functionality using ``git grep``.

- ``blogging`` are a collection of functions and helpers that I use for
  editing and producing my blog. I believe I got them from `metajack
  <https://github.com/metajack>`_, once upon a time, though I think I've
  edited them beyond recognition: they've certainly lasted longer than the
  publishing tools that I've used.

