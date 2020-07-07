---
layout: post
title: Painless Emacs interactive shells
description: Making a better API for Emacs shells
summary: More implicit into explicit
tags: [emacs]
---

![drake](/assets/img/drake-friendly-shell.png)


This post is part of a series about shells in Emacs:
 - [Painless Emacs shell commands](/2020/01/19/painless-emacs-shell-commands)
 - [Painless Emacs interactive shells](/2020/01/21/painless-emacs-interactive-shells)


## Recap

On the [previous post](/2020/01/19/painless-emacs-shell-commands), we discussed how to make more explicit functions to call single shell command.

Now, let's get into more juicy stuff with interactive shells.


## Emacs Interactive Shells API

For spawning shell, the main command is: **shell** `(& buffer)`.

Like for single shell commands, there is some hidden vars we can manipulate to change the behavior.

Here are the important vars we want to manipulate.

| var                                            |                                                                    |
| --                                             | --                                                                 |
| _default-directory_                            | location from which to launch shell                                |
| _explicit-shell-file-name_ / _shell-file-name_ | shell interpreter exec (e.g. bash ...)                             |
| _explicit-\<INTERPRETER\>-args_                | startup arguments for invoking the shell interactively (e.g. `-i`) |

Note that `shell-command-switch` is not listed of no use for interactive shells.

Instead, we have the new var `explicit-<INTEPRETER>-args` that allows passing a list of commands to the shell before it becomes interactive.

You might have spotted it, but in the previous post function `eval-with-shell-interpreter` already handled this use-case.


## Reusing our Wrappers

Hence, we can reuse the wrapper `with-shell-interpreter` that we defined previously.

There is just a small annoyance: for remote connections, `shell` prompts systematically for the remote interpreter path.

To disable this behaviour, we have to call it with the _default prefix argument_ (i.e. `C-u M-x shell`), of programmatically by let-binding `current-prefix-arg` to `'(4)`.

In this case, `explicit-shell-file-name` (or `shell-file-name` as a fallback) is used.

This gives, for example:

```emacs-lisp
(defun my/zsh-local ()
  (interractive)
  (with-shell-interpreter
    :path "~"
    :interpreter "zsh"
    :form
    (shell)))

(defun my/bash-on-raspi ()
  (interractive)
  (with-shell-interpreter
   :path "/ssh:pi@raspi:/~"
   :interpreter "bash"
   :form
   (let ((current-prefix-arg '(4)))     ; don't prompt for remote interperter path
     (shell))))
```


## Another wrapper

That's still quite cumbersome.

So let's create another derived helper to prevent repetition.

{::options parse_block_html="true" /}
<details><summary markdown="span">Click to toggle</summary>
```emacs-lisp
;; ------------------------------------------------------------------------
;; MAIN

(cl-defun friendly-shell (&key path interpreter interpreter-args command-switch)
  "Create a shell at given PATH, using given INTERPRETER binary."
  (interactive)

  (with-shell-interpreter
    :form
    (let* ((path (or path default-directory))
           (is-remote (file-remote-p path))
           (interpreter (or interpreter
                            (if is-remote
                                with-shell-interpreter-default-remote
                              shell-file-name)))
           (interpreter (prf/tramp/path/normalize interpreter))
           (shell-buffer-basename (friendly-shell--generate-buffer-name is-remote interpreter path))
           (shell-buffer-name (generate-new-buffer-name shell-buffer-name))
           (current-prefix-arg '(4))
           (comint-process-echoes t))
      (shell shell-buffer-name))
    :path path
    :interpreter interpreter
    :interpreter-args interpreter-args))

;; ------------------------------------------------------------------------
;; HELPERS: BUFFER NAME

(defun friendly-shell--generate-buffer-name (is-remote interpreter path)
  (if is-remote
      (friendly-shell--generate-buffer-name-remote interpreter path)
    (friendly-shell--generate-buffer-name-local interpreter path)))

(defun friendly-shell--generate-buffer-name-local (&optional interpreter _path)
  (if interpreter
      (prf-with-interpreter--get-interpreter-name interpreter)
    "shell"))

(defun friendly-shell--generate-buffer-name-remote (intepreter path)
  (let ((vec (tramp-dissect-file-name path)))
    (friendly-shell--generate-buffer-name-remote-from-vec vec)))

(defun friendly-shell--generate-buffer-name-remote-from-vec (vec)
  (let (user host)
    (concat
     (tramp-file-name-user vec) "@" (tramp-file-name-host vec))))
```
</details>
{::options parse_block_html="false" /}

Please note that we force `comint-process-echoes` to `t` to ensure that directory tracking works properly.

Directory tracking (_ditrack_ for short) is the Emacs capability to keep track of current directory when doing a `cd`.

Also, we embarked functions to help make shell buffer names more explicit.

Our rewritten commands become:

```emacs-lisp
(defun my/zsh-local ()
  (interractive)
  (friendly-shell :path "~" :interpreter "zsh"))

(defun my/bash-on-raspi ()
  (interractive)
  (friendly-shell :path "/ssh:pi@raspi:/~" :interpreter "bash"))
```

The code for `friendly-shell` can be found in package [friendly-shell](https://github.com/p3r7/friendly-shell).
