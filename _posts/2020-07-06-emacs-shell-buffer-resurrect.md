---
layout: post
title: Magically resurect Emacs shell buffers
description: Using shx to resurrect shell buffers magically
summary: When the process dies, the buffer resurrects it
tags: [emacs]
---


This article is part of a multi-post series auto-killing and resurrecting interactive shell buffers:
 - [Auto-close Emacs interpreter buffers](2020/05/13/emacs-comint-buffer-auto-close)
 - [Magically resurrect Emacs shell buffers](2020/07/06/emacs-shell-buffer-resurrect)


## Quitting a shell

On some terminal emulators, when the spawned process dies, the terminal window does not close.

And pressing `<Return>` can bring the process back from the dead[^1].

This is notably the case when using [KiTTY](http://www.9bis.net/kitty/#!index.md)[^2] to access a remote server (via SSH).

But Emacs doesn't work exactly that way.

The buffer stays open but is basically useless.


## shx to the rescue

Package [shx.el](https://github.com/riscy/shx-for-emacs) offer many goodies enhancing the user experience of interactive shell in Emacs.

One of its little-known feature is the ability to resurrect shell buffers!

It works right out of the box, you just have to add this to your config[^3]:

```emacs-lisp
(use-package shx
  :disabled
  :hook (shell-mode . shx-mode)
```

And once a shell buffer is dead, just press `<Return>` to resurrect its shell!


## Non-default interpreters

So let's assumed that you've spawn a shell with a non-default interpreter (`zsh` instead of `bash`):

```emacs-lisp
(let ((explicit-shell-file-name "zsh"))
  (shell))
```

If the process dies and after attempting to respawn it with shx, we'll end up with a `bash` process.

In fact this is because shx doesn't have any knowledge of the interpreter being used for the shell.

This is not a limitation in the package but of Emacs itself. More precisely, shell-mode does not keep track of the connection parameters in any buffer-local variable.


## Static local interpreter declaration

For the previous example, if we would always use `zsh` as our local interpreter, we can just:

```emacs-lisp
(setq explicit-shell-file-name "/bin/zsh"
      shell-file-name          "/bin/zsh"
      explicit-zsh-args        "-i"
      shell-command-switch     "-c")
```


## Static remote interpreter declaration

Emacs 26.2 comes with a new feature that can help us customize remote connection interpreters.

This feature is connection-local vars, i.e. custom var values for a given context (in this case TRAMP connections). It's akin to buffer-local vars.

Let's say that we want to launch a `zsh` shell on a remote server:

```emacs-lisp
(defun my-zsh-on-rapsi ()
  "Connect to my Raspberry Pi using zsh."
  (interactive)
  (let ((default-directory "/ssh:pi@raspberry:~")
        (current-prefix-arg '(4))       ; don't prompt for interpreter
        (explicit-shell-file-name "zsh"))
    (shell)))
```

If we would always like to connect to this server with the `zsh` interpreter, we could instead rely on the following connection-local vars definition:

```emacs-lisp
(connection-local-set-profile-variables
 'zsh                                   ; this is an alias, you can name it as you like
 '((explicit-shell-file-name . "/bin/zsh")
   ;; you can set any var here but it only get useful w/ TRAMP-related vars, e.g.:
   (explicit-bash-args . ("-i"))
   ;; ...
   ))

(connection-local-set-profiles
 '(
   :application tramp
   :protocol "ssh"
   :user "pi"
   :machine "raspberry")
 'zsh)                                  ; use same alias as before
```

This way, when resurrecting a shell buffer for this connection, Emacs would find that the desired interpreter is `zsh`.

We can even choose to use `zsh` for all remote ssh connections, `nil` acting as a wild card[^4]:

```emacs-lisp
(connection-local-set-profiles
 '(
   :application tramp
   :protocol "ssh"
   :user nil                            ; any user
   :machine nil)                        ; any host
 'zsh)                                  ; use same alias as before
```


## Dynamic interpreter retrieval

But what if we don't want to limit ourselves to one interpreter per remote host (e.g. defining several commands like `my-zsh-on-rapsi` for different interpreter)?

And what about a non-default local interpreter shells?

That's were [friendly-shell](https://github.com/p3r7/friendly-shell)[^5] shines.

When invoking a shell with `friendly-shell`, e.g.:

```emacs-lisp
(defun my-zsh-on-raspi ()
  (interractive)
  (prf-shell :path "/ssh:pi@raspberry:/~" :interpreter "zsh"))
```

... the interpreter-related vars have their values preserved as buffer-local vars.

This way, shx can find them back and dynamically spawned buffer can be resurrected with the right interpreter with no additional trick.


## Notes

[^1]: Spawn a new one in fact, but for the user it appears as if it was the same being resurected.

[^2]: A fork of PuTTY that I highly recommend, not to be confused w/ [kitty](https://sw.kovidgoyal.net/kitty/).

[^3]: Assuming you're using `use-package`. Otherwise, see [shx' README](https://github.com/riscy/shx-for-emacs/blob/master/README.org) for generic instructions.

[^4]: Propper regexps would have been a better design decision...

[^5]: Previously introduced in post [Painless Emacs interactive shells](2020/01/21/painless-emacs-interactive-shells).
