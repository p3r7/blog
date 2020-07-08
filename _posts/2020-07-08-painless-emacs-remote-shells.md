---
layout: post
title: Painless Emacs remote shells
description: Human-friendly spawning of remote shells
summary: A human-friendlier command
tags: [emacs]
---

This post is part of a series about shells in Emacs:
- [Painless Emacs shell commands](/2020/01/19/painless-emacs-shell-commands)
- [Painless Emacs interactive shells](/2020/01/21/painless-emacs-interactive-shells)
- [Emacs shell interpreter configuration](/2020/07/07/emacs-remote-shell-interpreter-conf)
- [Painless Emacs remote shells](/2020/07/08/painless-emacs-remote-shells) (current)


## Remote interactive shells

In a [previous post](/2020/01/21/painless-emacs-interactive-shells) we dealt with spawning interactive shells.

In it, we defined a new function `friendly-shell`, an objectively clearer version of `shell`.

It can be used to easilly define commands for spawning local and remote shells with an arbitrary shell interpreter.

But it can be quite cumbersome to have to define a command for each and every remote connection.

Ideally we'd want a command that prompts for a remote path and spawn a shell there.


## Prompting for a remote path

The [remote path format](https://www.gnu.org/software/emacs/manual/html_node/tramp/File-name-syntax.html#File-name-syntax) understood by Emacs looks like this:

    /<method>:[<user>[%<domain>]@]<host>[%<port>][:<localname>]

The `/<method>:` prefix is mandatory, required for Emacs to detect that the path is remote.

`<method>` can have a default value (configured with `tramp-default-method` and `tramp-default-method-alist`) but the user still need to type in the prefix `/-:` to use it.

In the context of a command explicitely prompting for a remote path, it's quite cumbersome for the user to have to type in this `/<method>:` prefix.

We want a light wrapper that also supports this more permissive format:

    [<user>[%<domain>]@]<host>[%<port>][:<localname>]

That wrapper is function `friendly-tramp-path-dissect` provided by package [friendly-tramp-path](https://github.com/p3r7/friendly-tramp-path).


## Default remote interpreter configuration

Introduced in Emacs 26.2, connection-local vars allow to have a per host interpreter configuration.

Package [friendly-shell](https://github.com/p3r7/friendly-shell) comes with its own, more versatile, implementation of connection-local vars.

Both options are explained in details in post: [Emacs shell interpreter configuration](/2020/07/07/emacs-remote-shell-interpreter-conf).


## Putting it all together

```emacs-lisp
(require 'friendly-tramp-path)
(require 'with-shell-interpreter)
(require 'prf-shell)

(cl-defun friendly-remote-shell (&key path
                                      interpreter interpreter-args
                                      command-switch
                                      w32-arg-quote)
  (interactive)
  (let* ((path (or path (read-string "Host: ")))
         (path (with-shell-interpreter--normalize-path path))
         (vec (friendly-tramp-path-dissect path))
         (path (tramp-make-tramp-file-name (tramp-file-name-method vec)
                                           (tramp-file-name-user vec) (tramp-file-name-domain vec)
                                           (tramp-file-name-host vec) (tramp-file-name-port vec)
                                           (tramp-file-name-localname vec))))
    (friendly-shell :path path
                    :interpreter interpreter
                    :interpreter-args interpreter-args
                    :command-switch command-switch
                    :w32-arg-quote w32-arg-quote)))
```

And _voilà_! Now to connect to a remote server we just has to type:

    M-x friendly-remote-shell <RET>
    pi@raspberry <RET>

The code for `friendly-remote-shell` can be found in package [friendly-remote-shell](https://github.com/p3r7/friendly-shell).
