---
layout: post
title: Emacs shell interpreter configuration
description: How to configure default shell interpreter
summary: Local and remote
tags: [emacs]
---

This post is part of a series about shells in Emacs:
- [Painless Emacs shell commands](/2020/01/19/painless-emacs-shell-commands)
- [Painless Emacs interactive shells](/2020/01/21/painless-emacs-interactive-shells)
- [Emacs shell interpreter configuration](/2020/07/07/emacs-remote-shell-interpreter-conf)
- [Painless Emacs remote shells](/2020/07/08/painless-emacs-remote-shells)


## Interactivity

In this series of posts we discuss spawning shells from within Emacs.

If the functions we previously introduced can be used programmatically, their main use-case is to be called directly by the user with `M-x`, i.e. as _commands_.

In this case, the user is not prompted for the shell interpreter to use.

This post explores the different ways to configure the interpreter to be used.


## Custom wrapper commands

One strategy is to define commands for specific connection / interpreter combinations:

```emacs-lisp
;; native
(defun my-zsh-on-rapsi ()
  (interactive)
  (let ((default-directory "/ssh:pi@raspberry:~")
        (current-prefix-arg '(4))       ; don't prompt for interpreter
        (explicit-shell-file-name "zsh"))
    (shell)))

;; using `friendly-shell'
(defun my-zsh-on-raspi ()
  (interactive)
  (friendly-shell :path "/ssh:pi@raspberry:/~" :interpreter "zsh"))
```

This is convenient if we can want to be able to spawn several shells with different interpreters for a same connection string (`:path`).


## Default local interpreter declaration

The default interpreter can be customized like so:


```emacs-lisp
(setq explicit-shell-file-name "/bin/zsh"
      shell-file-name          "/bin/zsh"
      explicit-zsh-args        "-i"
      shell-command-switch     "-c")
```


## Static remote connection interpreter declaration (native)

Emacs 26.2 comes with a new feature that can help us customize remote connection interpreters.

This feature is connection-local vars, i.e. custom var values for a given context (in this case TRAMP connections). It's akin to buffer-local vars.

Let's say that we always want to connect to a server with the `zsh` interpreter:


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

We can even choose to use `zsh` for all remote ssh connections, `nil` acting as a wild card:

```emacs-lisp
(connection-local-set-profiles
 '(
   :application tramp
   :protocol "ssh"
   :user nil                            ; any user
   :machine nil)                        ; any host
 'zsh)
```


## Better static remote connection interpreter declaration

Connection-local vars are a great idea but the implementation is far from ideal.

Indeed, the 2-step declaration is cumbersome and a declaration cannot be undone unless we restart the Emacs process[^1].

More importantly, instead of using `nil` as a wildcard, proper regexps would have been a better design decision.

In an enterprise setting, when dealing with thousands of VMs / containers, you may want to have configurations based on host naming conventions or IP ranges.

That's why the [friendly-shell*](https://github.com/p3r7/friendly-shell) packages[^2] now come with its custom implementation of connection-local vars.

The previous example becomes:

```emacs-lisp
(add-to-list with-shell-interpreter-connection-local-vars '(".*@rasp.*") . ((explicit-shell-file-name . "/bin/zsh")
                                                                            (explicit-bash-args . ("-i"))
                                                                            (shell-command-switch . "-c")))
```

This way every connection with any user to any host starting with the string `"rasp"` would use the zsh interpreter.

This configuration only gets applied to the `friendly-shell*` family of commands.

The syntax of the configuration is heavily inspired by [this original idea](https://github.com/riscy/shx-for-emacs/issues/16#issuecomment-586771357) from [@riscy](https://github.com/riscy).

If you want to use those commands with the native connection-local vars implementation instead, put this in your init:

```emacs-lisp
(setq with-shell-interpreter-connection-local-vars-implem 'native)
```


## Notes

[^1]: Read the source of `hack-connection-local-variables` and see how `connection-local-variables-alist` is set to understand why.

[^2]: Since [version 0.2.3 of with-shell-interpreter](https://github.com/p3r7/with-shell-interpreter/releases/tag/0.2.3).
