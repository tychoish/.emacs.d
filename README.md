quantity <[across names]>quantity <[across names]># Tycho Emacs Config Kit

## Background

I've been using Emacs for a long time, and I suspect I will be for a
while yet. This repository contains all of that configuration, packaged
up in a way that anyone can use, learn from, and get started using very
quickly. There are a lot of [starter
kits] for emacs,
and this isn't quite that; although if you're newer to Emacs and want
to bootstrap the configuration process, or if you've been using Emacs a
bit for a while but aren't particularly fond of your configuration,
this might be a good place to start.

Fair warning, where starter kits are flashy and opinionated, this
configuration strives for a *exceptionally* minimal and lightweight
experience that's also fully usable and feature complete as your
primary text-editing and software development toolkit.

[starter kits]:https://github.com/emacs-tw/awesome-emacs#starter-kit)

## Goals

From a high level, the goals are:

- **exceptionally fast start times**: on my (not very new) computers,
  Emacs sessions readily start in about a second, and while I've
  included systemd service files for managing daemons, being able to
  start ephemeral sessions and have Emacs associated with opening
  specific file types is useful.

- **full support for console-mode Emacs**: while I mostly use Emacs in
  GUI mode, I've always been envious of vim user's ability to
  operate contently in the console. The configuration omits some
  features which are annoying on the console, makes it possible to use
  themes in Emacs daemons without impacting the experience on the
  console.

- **daemon-first and multi-daemon support without compromises**: the
  Emacs daemon, allows you to have a single Emacs session and open
  different frames that have access to the same state, and persist
  even if all frames close. This configuration provides support for
  saving this state\--even across restarts of the daemon using a
  reasonable `desktop`, `session`, and `recentf` config, but also
  omits these features for ephemeral sessions. Also daemon-related
  state files are saved in `~/.emacs.d` folders with names specific to
  the daemon instance so you can have some degree of isolation between
  environments.

- **visually minimal**: I turned everything off, there are no menus,
  and Emacs doesn't talk to you about anything.

- **great initial experience**: I've provided mechanisms for
  automatically loading your own arbitrary configuration, and
  attempted to document my own configuration as much as possible;
  however, I've tried to put good defaults in place and made it super
  easy to get started without any required customization.

- **support all of my common workflows**: By day I write a lot of Go,
  with some other programming in Common Lisp, TypeScript/JavaScript,
  Python, Shell, and whatever else comes up. I also write a bunch of
  English in `org-mode`, `rst-mode` and `markdown-mode`. I also read
  email in emacs. All of these use cases should be well supported.

### Features

This is not an exhaustive list:

- compellingly fast load times. My goal is to keep 3-10 year old computers
  starting emacs in well under 2 seconds. Current timing (on my systems) is
  around 750ms (and often less,) for GUI sessions and less than 400ms for
  terminal sessions.

- a compelling (and roughly equivalent) terminal experience _and_ GUI
  experience.

- superior completion experience. While I've historically been a
  [helm] user (and that configuration is
  still in the tree in the `legacy/` directory), in mid 2025, I switched to
  using the [vertico] + [consult] + [corfu] + [cape] + [embark] +
  [marginalia]. Though there are more pieces, but having a less monolithic
  approach makes the whole system a bit simpler, and also easy to customize more
  directly.

- use the red/green friendly colorblind friendly [modus themes]--now included
  with emacs--by the default.

- good support for long-lived emacs sessions, both with running multiple daemons
  on a single system, as well as configuration to support simple session saving
  and automatic restoration of sessions. Add support for an `--id` CLI flag to
  be able to maintain multiple distinct emacs sessions on a single system,
  either as named daemons or just default from the command line.

- solid configurations for other configurations: [yasnippet] for text expansion,
  and [eglot] for getting rich IDE-like support language servers, `tree-sitter`
  for syntax handling, and more recently I've been exploring various ways to
  integrate LLM-derived coding tools. Historically I've used more third-party
  packages for this kind of functionality (like [lsp-mode], `company-mode`, and
  others), but I've tended toward using increasingly minimal configurations and
  very thin components.

[lsp-mode]:https://github.com/emacs-lsp/lsp-mode
[helm]:https://github.com/emacs-helm/helm
[eglot]:https://www.gnu.org/software/emacs/manual/html_mono/eglot.html
[yasnippet]:https://github.com/joaotavora/yasnippet
[modus themes]:https://gitlab.com/protesilaos/modus-themes
[vertico]:https://github.com/minad/vertico
[consult]:https://github.com/minad/consult
[corfu]:https://github.com/minad/corfu
[cape]:https://github.com/minad/cape
[embark]:https://github.com/oantolin/embark
[marginalia]:https://github.com/minad/marginalia

## Documentation

### Dependencies

Install Emacs in whatever way makes sense for you and your system. My
preferences are:

- **Emacs 30 or greater**. There's not a lot of reason to hang out with old
  versions.

- **Lucid**. This is a bit odd, but there's a long standing GTK/Emacs
  issue where GTK Emacs daemons crash if the X11 server restarts.
  It's not a big deal, and totally irrelevant if you're not planning
  to use the daemon mode, but it's there.

When I've used macOS in the past I've always installed emacs using homebrew and
getting a build with a native (coca? do we still call it that?)  toolkit. The
last time I tried this (Aug 2025), it worked well.

Other dependencies, all optional, depending on your goals:

- If you want to use LSP mode (you probably do) you will probably want
  to select and install language servers with packages from your
  operating system. On Arch I have `clang`,
  `typescript-language-server-bin`, `bash-language-server`,
  `dockerfile-language-server-bin`, `python-langauge-server`. I also
  install `gopls` using `go get`.

- To read email, I install `mu` (which includes `mu4e`) from the arch
  packages.

### Installation

Basically you want this repository to be your `~/.emacs.d` directory.
I'd go about it by, first moving your existing `~/.emacs.d/` out of the
way:

```bash
 mv ~/.emacs.d/ ~/emacs.d.archive
```

Then clone the repository:

```bash
git clone --recurse-submodules git@github.com:tychoish/.emacs.d.git ~/.emacs.d/
```

And that's it! the `--recursive-submodules` option clones an `elpa`
submodule, which it totally optional, but means that you won't have to
download all of the packages on the first time.

In more advanced setups, you can choose to break this apart:

```bash
git clone git@github.com:tychoish/.emacs.d.git ~/.emacs.d/
```

I've created a repository that just has a clone of my `elpa` (package)
directory as a submodule. This is optional, but it will be *quicker* and
any bugs you run into I'll probably be hitting as well:

```bash
cd ~/.emacs.d/
git submodule update --init
```

In the future you can just pull/merge from the upstream to get updates,
and run `git submodule update` to update `elpa`.

Read the customization section for more information. It's possible to
put your own config in the `users` subdirectory, but you might also want
to fork this repository and make your own modifications, or have more
control over how you track the mainline.

**If you're not running emacs 30** (or whatever version I'm running these days)
the `elpa` checkout *might not work*. In the past I've committed major-version
branches for different versions of emacs. You can omit the
`--recursive-submodules` option or remove the `elpa` directory entirely: Emacs
will take *much* longer to start the first time as it downloads and builds all
of the third-party packages it depends on.

### Use

#### Invocation

Once you're installed, `emacs` should just read the tychoish
configuration. I would start by using `emacs` (GUI) and `emacs -nw`
(console) to start `emacs`, or just by selecting it from the menus in
your desktop environment. Once you've gotten used to things, you can
configure the daemon mode as follows:

If you're on Linux, and want to use the `systemd --user` instance,
which is like your own user-specific systemd instance, begin by using
the following command to ensure that `systemd --user` instance starts on
boot and doesn't wait for you to login:

```bash
sudo loginctl enable-linger $(whoami)
```
Then copy the relevant service file into the `~/.config/systemd/user/`
directory:

```bash
cp ~/.emacs.d/emacs@.service ~/.config/systemd/user/
```

Then run the following configuration to reload the `systemd` instance:

```bash
systemctl --user daemon-reload
systemctl --user daemon-reexec
```

Now you can use the following commands, to start two `emacs` daemons:

```bash
systemctl --user start emacs@personal
systemctl --user start emacs@work
```

The following command will ensure that the daemons start when your
system reboots:

```bash
systemctl --user enable emacs@personal
systemctl --user enable emacs@work
```

Now you can start the `emacsclient` which opens Emacs frames attached to
the specified daemon. I keep the following aliases in my shell, and
bound to keybindings in my window manager as well:

```bash
alias e='emacsclient --server-file=personal --no-wait'
alias ew='emacsclient --server-file=personal --create-frame --no-wait'
alias et='emacsclient --server-file=personal --tty'

alias we='emacsclient --server-file=work --no-wait'
alias wew='emacsclient --server-file=work --create-frame --no-wait'
alias wet='emacsclient --server-file=work --tty'
```

Modify these commands to use whatever daemon names you selected above.
The `e` option opens a specific file in the most recent frame you've
used, `ew` creates a new frame optionally opening a file, and `et` opens
a console window optionally opening a file.

#### Keybindings

Most of the keybindings are defined in package specific configuration in
`lisp/tychoish-coreprogramming.el`. In daemon/GUI-mode, the
[which-key](https://github.com/justbur/emacs-which-key) makes these
discoverable. I often use helm menus to find hints about keybindings.
Some broad themes:

- `C-x g s` opens the `magit-status` buffer for the current
  repository.
- `C-c t <...>` (`t` for tycho) provide entry-points into functions that I've
  written or cases where I want quicker access to something that isn't bound by
  default:
  - `C-c t t <d|l|e>` for disable, load, enable theme.
  - `C-c t b <...>` for functions related to blog posting.
- `C-c g <...>` for grep/git-grep/ag/rg helpers for searching for
  strings in directories projects. I tend to prefer `rg` these days and have
  commands for doing both incremental search (with consult) and in compile
  buffers (as with `find-grep`.

- `C-c w <>` for `browse-url` functionality opening links in
  various browsers `e` is for `eww` and `c` is for chromium.

- `C-c l` is the prefix for all `eglot` keybindings, I'm particularly fond of
  `C-c l s s` to start an lsp session on a file.
- `C-c f =` and `C-c f -` to increase the text size in a specific
  buffer.
- `C-c f f` enables flycheck, and all flycheck keybindings are under
  `C-c f`, so \"open list of flycheck buffers\" `C-c f l`.
- `C-c t c` runs build at the top-level of the current project, providing the
  ability to manage a few different compile buffers, to be able to run test,
  lint, build (and other) in parallel. For many languages it will also suggest
  commands to run. Inside of a compile buffer the `C` key will change the
  compile command. I use this for managing most background processes.
- `C-c .` provides access to specific completion suggestions. This should happen
  automatically as you type, but you can look for a specific kind of completion
  or to open a company completion window at the current point on demand. These
  open automatically many times, but it's nice to be able to call them up
  specifically.

#### Customization

The process of "making my config public and reusable" mostly centered
on pulling all of the assumptions about the way I organize my files and
paths, and avoid hardcoding things in most places. The result is that
there isn't much customization that you should need to do; however, the
following variable should probably be set differently:

```emacs
(setq local-notes-directory (expand-file-name "~/notes")))
```

The `local-notes-directory` is the top level directory underwhich
`org-mode`, `deft` and `roam` directories are stored in this
configuration.

For any further customization, create or link files in the
`~/.emacs.d/user/` directory and they'll be loaded after my config
finishes loading. These files should have matching file names and
feature declarations, as in a `(provide 'file)` form at the bottom for
`file.el`. These files can use elisp that's provided elsewhere in the
config, but you should rely on any specific initialization order for
these files. The code that loads them also prints the load times so you
can see if you're loosing too much time loading this code: having lots
of files here can really impact your load time, one or two files is
fine, but more could be a problem.

I keep work-specific configuration here (just to keep it separate from
the rest of the configuration,) as well as configuration that's super
specific to my machines or personal use (e.g email.) Consider some of
the following configuration:

- Setup my org capture templates, using a function defined elsewhere:

```emacs
;; (tychoish-org-add-project-file-capture-templates "file-name" :prefix <char>)
;; using the empty key creates a shortcuts in a file at the top level
;; (templates starting with n, t, r, j )
(tychoish-org-add-project-file-capture-templates "meta" "m")
(tychoish-org-add-project-file-capture-templates "writing" "w")
(tychoish-org-add-project-file-capture-templates "blog" "b")
(tychoish-org-add-project-file-capture-templates "organizer" "")
```

  This modifies data in a list, so calling this function in the
  reverse priority is ideal.

- Setup different fonts on different machines:

```emacs
(when (gui-p)
 (let ((sys (system-name)))
   (cond
    ((equal "deleuze" sys) (tychoish-setup-font "Source Code Pro" 11))
	((equal "derrida" sys) (tychoish-setup-font "Source Code Pro" 10))
	((equal "bakhtin" sys) (tychoish-setup-font "Source Code Pro" 10)))))
```

  I use different font sizes on my laptop `deleuze` but at the very
  least, if you do nothing else I'd use one of the following forms
  somewhere:

```emacs
(when (gui-p)
   (tychoish-setup-font "Source Code Pro" 10))

(when (gui-p)
   (tychoish-setup-font "Inconsolata-g" 11))

(when (gui-p)
   (tychoish-setup-font "Consolas" 11))
```

  `tychoish-setup-font` also works interactively.

- For using `mu4e` to manage email, in a multi-account setup. I wrote a macro,
  `tychoish-define-mail-account` to add a new account.  I have a few
  functions that I define here for changing my originating email address/mu
  database, which are bound to keys, but the basics are:

```emacs
(tychoish-define-mail-account
 :name "tycho"
 :address "garen@tychoish.com"
 :key "g"
 :id "tychoish"
 :instances '("hud")
 :command "fetchmail")
```

This binds `C-m g` to a function that changes the `mu4e` configuration to use this
account. The `C-m a` opens a menu where you can select an account.

Enjoy! Happy hacking!

## Participation

If you like this, and find it useful, you don't need to do anything!
Just enjoy! If you discover a bug, or have a feature request, please
feel free to open an issue or submit a pull request!

I'm not particularly sure what direction development will take, but
I'm interested in the following areas:

- Improving the way that console mode interacts with themes.

- Increasing or maintaining the current level with regards to startup
  time.

- Continue to improve development experience for specific languages (including
  English!) and other workflows, integration with external tools (e.g. like
  coding agents).
