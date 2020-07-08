---
layout: post
title: Magically resurect Emacs shell buffers
description: Using shx to resurrect shell buffers magically
summary: When the process dies, the buffer resurrects it
tags: [emacs]
---


This article is part of a multi-post series about how to better deal with interactive shell buffers in Emacs:
 - [Auto-close Emacs interpreter buffers](/2020/05/13/emacs-comint-buffer-auto-close)
 - [Magically resurrect Emacs shell buffers](/2020/07/06/emacs-shell-buffer-resurrect) (current)


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

Everything works fine for static declaration of interpreters (read [Emacs shell interpreter configuration](/2020/07/07/painless-emacs-remote-shells) for more details) but as soon as we start to bind things dynamically Emacs looses track.


## A solution

That's were [friendly-shell](https://github.com/p3r7/friendly-shell)[^4] shines.

When invoking a shell with `friendly-shell`, e.g.:

```emacs-lisp
(defun my-zsh-on-raspi ()
  (interractive)
  (friendly-shell :path "/ssh:pi@raspberry:/~" :interpreter "zsh"))
```

... the interpreter-related vars have their values preserved as buffer-local vars!

This way, shx can find them back and dynamically spawned buffer can be resurrected with the right interpreter with no additional trick.


## Notes

[^1]: Spawn a new one in fact, but for the user it appears as if it was the same being resurected.

[^2]: A fork of PuTTY that I highly recommend, not to be confused w/ [kitty](https://sw.kovidgoyal.net/kitty/).

[^3]: Assuming you're using `use-package`. Otherwise, see [shx' README](https://github.com/riscy/shx-for-emacs/blob/master/README.org) for generic instructions.

[^4]: Previously introduced in post [Painless Emacs interactive shells](2020/01/21/painless-emacs-interactive-shells).
