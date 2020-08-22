======================
Tycho Emacs Config Kit
======================

Background
----------

I've been using Emacs for a long time, and I suspect I will be for a while
yet. This repository contains all of that configuration, packaged up in a way
that anyone can use, learn from, and get started using very quickly. There are
a lot of `starter kits <https://github.com/emacs-tw/awesome-emacs#starter-kit>`_
for emacs, and this isn't quite that; although if you're newer to Emacs and want
to bootstrap the configuration process, or if you've been using Emacs a bit for a
while but aren't particularly fond of your configuration, this might be a good
place to start.

Fair warning, where starter kits are flashy and opinionated, this
configuration strives for a *exceptionally* minimal and lightweight experience
that's also fully usable and feature complete as your primary text-editing and
software development toolkit.

Goals
-----

From a high level, the goals are:

- **exceptionally fast start times**: on my (not very new) computers, Emacs
  sessions readily start in about a second, and while I've included systemd
  service files for managing daemons, being able to start ephemeral sessions
  and have Emacs associated with opening specific file types is useful.

- **full support for console-mode Emacs**: while I mostly use Emacs in GUI mode,
  I've always been envious of vim user's ability to operate contently in the
  console. The configuration omits some features which are annoying on the
  console, makes it possible to use themes in Emacs daemons without impacting
  the experience on the console.

- **daemon-first and multi-daemon support without compromises**: the Emacs
  daemon, allows you to have a single Emacs session and open different frames
  that have access to the same state, and persist even if all frames
  close. This configuration provides support for saving this state--even
  across restarts of the daemon using a reasonable ``desktop``, ``session``, and
  ``recentf`` config, but also omits these features for ephemeral
  sessions. Also daemon-related state files are saved in ``~/.emacs.d``
  folders with names specific to the daemon instance so you can have some
  degree of isolation between environments.

- **visually minimal**: I turned everything off, there are no menus, and Emacs
  doesn't talk to you about anything.

- **great initial experience**: I've provided mechanisms for automatically
  loading your own arbitrary configuration, and attempted to document my own
  configuration as much as possible; however, I've tried to put good defaults
  in place and made it super easy to get started without any required
  customization.

- **support all of my common workflows**: By day I write a lot of Go, with
  some other programming in Common Lisp, TypeScript/JavaScript, Python, Shell,
  and whatever else comes up. I also write a bunch of English in ``org-mode``
  and ``rst-mode``. I also read email in emacs. All of these use cases should
  be well supported.

Features
~~~~~~~~

This is not an exhaustive list:

- compellingly fast load times. My goal is to keep 3-10 year old computers
  starting emacs in well under 2 seconds.

- `helm <https://github.com/emacs-helm/helm>`_ for everything, and lots of
  convent bindings to put helm menus at your finger tips.

- red/green friendly colorblind friendly themes (accessible via
  ``tychoish-load-dark-theme`` and ``tychoish-load-light-theme``,) but not
  enabled by default. They are the `modus themes
  <https://gitlab.com/protesilaos/modus-themes>`_ .

- good support for long-lived emacs sessions, both with running multiple
  daemons on a single system, as well as configuration to support simple
  session saving and automatic restoration.

- rich editing support: `yasnippets <https://github.com/joaotavora/yasnippet>`_
  for text snippets, `company-mode <https://github.com/company-mode>`_
  for automatic completion as you type, and `lsp-mode
  <https://github.com/emacs-lsp/lsp-mode>`_ for all of the rich IDE-like
  features.

Documentation
-------------

Dependencies
~~~~~~~~~~~~

Install Emacs in whatever way makes sense for you and your system. My
preferences are:

- **Emacs 27.1 or greater**. The latest version of Emacs has very fast JSON
  parsing, which makes using `lsp-mode <https://github.com/emacs-lsp/lsp-mode/>`_
  compelling.

- **Lucid**. This is a bit odd, but there's a long standing GK/Emacs issue
  where GTK Emacs daemons crash if the X11 server restarts. It's not a big
  deal, and totally irrelevant if you're not planning to use the daemon mode,
  but it's there.

When I've used macOS in the past I've always installed emacs using homebrew,
though I'm not sure what the state of the art is there: ideally building
something with the cocoa bits is nice.

Other dependencies, all optional, depending on your goals:

- If you want to use LSP mode (you probably do) you will probably want to
  select and install language servers with packages from your operating
  system. On Arch I have ``clang``, ``typescript-language-server-bin``,
  ``bash-language-server``, ``dockerfile-language-server-bin``,
  ``python-langauge-server``. I also install ``gopls`` using ``go get``.

- To read email, I install ``mu`` (which includes ``mu4e``) from arch
  packages.

Installation
~~~~~~~~~~~~

Basically you want this repository to be your ``~/.emacs.d`` directory. I'd go
about it by, first moving your existing ``~/.emacs.d/`` out of the way: ::

   mv ~/.emacs.d/ ~/emacs.d.archive

Then I'd fork this repository in Github, so you can make your own
modifications, and clone it: ::

   git clone git@github.com:tychoish/.emacs.d.git ~/.emacs.d/

I've created a repository that just has a clone of my ``elpa`` (package)
directory as a submodule. This is optional, but it will be *quicker* and
any bugs you run into I'll probably be hitting as well: ::

   cd ~/.emacs.d/
   git submodule update --init

In the future you can just pull/merge from the upstream to get updates, and
run ``git submodule update`` to update ``elpa``.

Congrats, you should be good to go!

Use
~~~

Invocation
``````````

Once you're installed, ``emacs`` should just read the tychoish
configuration. I would start by using ``emacs`` (GUI) and ``emacs -nw``
(console) to start ``emacs``, or just by selecting it from the menus in your
desktop environment. Once you've gotten used to things, you can configure the
daemon mode as follows:

If you're on Linux, and want to use the ``systemd --user`` instance, which is
like your own user-specific systemd instance, begin by using the following
command to ensure that ``systemd --user`` instance starts on boot and doesn't
wait for you to login: ::

   sudo loginctl enable-linger $(whoami)

Then copy the relevant service file into the ``~/.config/systemd/user/``
directory: ::

   cp ~/.emacs.d/emacs@.service ~/.config/systemd/user/

Then run the following configuration to reload the ``systemd`` instance: ::

   systemctl --user daemon-reload
   systemctl --user daemon-reexec

Now you can use the following commands, to start two
``emacs`` daemons: ::

   systemctl --user start emacs@personal
   systemctl --user start emacs@work

The following command will ensure that the daemons start when your system
reboots: ::

   systemctl --user enable emacs@personal
   systemctl --user enable emacs@work

Now you can start the ``emacsclient`` which opens Emacs frames attached to the
specified daemon. I keep the following aliases in my shell, and bound to
keybindings in my window manager as well: ::

   alias e='emacsclient --server-file=personal --no-wait'
   alias ew='emacsclient --server-file=personal --create-frame --no-wait'
   alias et='emacsclient --server-file=personal --tty'

   alias we='emacsclient --server-file=work --no-wait'
   alias wew='emacsclient --server-file=work --create-frame --no-wait'
   alias wet='emacsclient --server-file=work --tty'

Modify these commands to use whatever daemon names you selected above. The
``e`` option opens a specific file in the most recent frame you've used,
``ew`` creates a new frame optionally opening a file, and ``et`` opens a
console window optionally opening a file.

Keybindings
```````````

Most of the keybindings are defined in package specific configuration in
``conf/programming.el``, but there are also many general ones defined in
``conf/settings.el``. In daemon/GUI-mode, the `which-key
<https://github.com/justbur/emacs-which-key>`_ makes these discoverable. I
often use helm menus to find hints about keybindings. Some broad themes:

- ``C-c h <mnemonic>`` provide entry-points into helm-based
  functionality. ``a`` for appropos is great for finding documentation of
  symbols for emacs lisp; ``s`` gets at ``swoop`` which is a buffer search
  tool (also ``C-c M-s``)``; ``m`` opens system man pages. Other helm
  keybindings:

  - ``C-x m``, ``C-x C-m`` are alternates for ``M-x`` and do similar
    things. ``M-<spc>`` opens a ``helm-mini`` that searches across buffers,
    files, and commands.

  - ``C-c a p`` does a helm-ag search in the current project. ``C-c r p`` does
    the same thing but with ripgrep. ``C-c a s`` and ``C-c r s`` do the same
    thing but starting at the directory of the current file.

- ``C-x g s`` opens the ``magit-status`` buffer for the current repository.

- ``C-c t <mnemonic>`` (``t`` for tycho) provide entry-points into functions that
  I've written or cases where I want quicker access to something that isn't
  bound by default:

  - ``C-c t t <d|l|e>`` for disable, load, enable theme.
  - ``C-c t b <mnemonic>`` for functions related to blog posting.

- ``C-c g <mnemonic>`` for grep/git-grep/ag/rg helpers for searching
  for strings in directories projects.

- ``C-c w <mnemonic>`` for ``browse-url`` functionality opening links in
  various browsers ``e`` is for ``WWW`` and ``c`` is for chromium.

- ``C-c l`` is the prefix for all ``lsp-mode`` keybindings, I'm particularly
  fond of ``C-c l s s`` to start an lsp session on a file.

- ``C-c f =`` and ``C-c f -`` to increase the text size in a specific buffer.

- ``C-c f f`` enables flycheck, and all flycheck keybindings are under ``C-c
  f``, so "open list of flycheck buffers" ``C-c f l``.

- ``C-c t c`` runs ``make build`` at the top-level of the current file in a
  unique compile buffer, switching to that buffer if its open, use ``C`` in
  compile-mode to change the compile command. I use this for managing most
  background processes.

- ``C-c .`` and ``C-c C .`` open a company completion window at the current
  point on demand. These open automatically many times, but it's nice to be
  able to call them up. ``C-c s s`` opens a company window for inserting
  asnippets explicitly, which are intentionally the lowest priority
  completions.

Customization
`````````````

The process of "making my config public and reusable" mostly centered on
pulling all of the assumptions about the way I organize my files and paths,
and avoid hardcoding things in most places. The result is that there isn't
much customization that you should need to do; however, the following variable
should probably be set differently: ::

    (setq local-notes-directory (expand-file-name "~/notes")))

The ``local-notes-directory`` is the top level directory underwhich ``org-mode``,
``deft`` and ``roam`` directories are stored in this configuration.

For any further customization, create or link files in the
``~/.emacs.d/user/`` directory and they'll be loaded after my config finishes
loading. These files should have matching file names and feature declarations,
as in a ``(provide 'file)`` form at the bottom for ``file.el``. These files
can use elisp that's provided elsewhere in the config, but you should rely on
any specific initialization order for these files. The code that loads them
also prints the load times so you can see if you're loosing too much time
loading this code: having lots of files here can really impact your load time,
one or two files is fine, but more could be a problem.

I keep work-specific configuration here (just to keep it separate from the
rest of the configuration,) as well as configuration that's super specific to
my machines or personal use (e.g email.) Consider some of the following
configuration:

- Setup my org capture templates, using a function defined elsewhere: ::

    ;; (tychoish-add-org-capture-template <prefix-key> "file-name")
    ;; using the empty key creates a shortcuts in a file at the top level
    ;; (templates starting with n, t, r, j )
    (tychoish-add-org-capture-template "m" "meta")
    (tychoish-add-org-capture-template "w" "writing")
    (tychoish-add-org-capture-template "b" "blog")
    (tychoish-add-org-capture-template "" "organizer")

  This modifies data in a list, so calling this function in the reverse
  priority is ideal.

- Setup different fonts on different machines: ::

    (when (gui-p)
     (let ((sys (system-name)))
       (cond
	((equal "deleuze" sys) (tychoish-font-setup "Source Code Pro" 11))
	((equal "derrida" sys) (tychoish-font-setup "Source Code Pro" 10))
	((equal "bakhtin" sys) (tychoish-font-setup "Source Code Pro" 10)))))

  I use different font sizes on my laptop ``deleuze`` but at the very least,
  if you do nothing else I'd use one of the following forms somewhere: ::

    (when (gui-p)
      (tychoish-font-setup "Source Code Pro" 10))

    (when (gui-p)
      (tychoish-font-setup "Inconsolata-g" 11))

    (when (gui-p)
      (tychoish-font-setup "Consolas" 11))

  ``tychoish-font-setup`` also works interactively.

- For using ``mu4e`` to manage email, in a multi-account setup, I have a few
  functions that I define here for changing my originating email address/mu
  database, which are bound to keys, but the fundamentals are: ::

     (defun tychoish-mail-setup-personal ()
	(interactive)
	(setq mu4e-get-mail-command "fetchmail -a")
	(setq mu4e-user-mail-address-list '("tycho@example.org" "tycho@example.net"))
	(tychoish-set-up-email (expand-file-name "~/mail") "tycho garen" "tycho@example.net"))

     (global-set-key (kbd "C-c m a") ''tychoish-mail-setup-personal)

     (defun tychoish-mail-setup-work ()
	(interactive)
	(setq mu4e-get-mail-command "fetchmail -a")
	(setq mu4e-user-mail-address-list '("sam@example.com"))
	(tychoish-set-up-email (expand-file-name "~/mail") "sam tycho garen" "sam@example.com"))

     (global-set-key (kbd "C-c m a") ''tychoish-mail-setup-work)

     (let ((daemon (daemonp)))
	 (cond
	   ((equal daemon "personal") (tychoish-mail-personal))
	   ((equal daemon "work") (tychoish-mail-work))))

Enjoy! Happy hacking!

Participation
-------------

If you like this, and find it useful, you don't need to do anything! Just
enjoy! If you discover a bug, or have a feature request, please feel free to
open an issue or submit a pull request!
