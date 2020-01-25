---
layout: post
title: Painless Emacs shell commands
description: Making a better API for Emacs shell commands
summary: Turning implicit into explicit
tags: [emacs]
---

This article is part of a multi-post series about shells in Emacs:
 - [Painless Emacs shell commands](/2020/01/19/painless-emacs-shell-commands)
 - [Painless Emacs interactive shells](2020/01/21/painless-emacs-interactive-shells)


## Emacs as a terminal emulator

Emacs can act as a powerful [terminal emulator](https://www.gnu.org/software/emacs/manual/html_node/emacs/Shell.html).

It can spawning interactive shells (with shell-mode and term-mode) and execute single shell commands.

This post will focus on single shell commands.

For interactive shells, see [next post](2020/01/21/painless-emacs-interactive-shells).

>In this article, there are two meanings for the word "command":
> - shell commands, we will refer to them with **shell commands**
> - interactive Emacs functions, we will refer to them with **_commands_**


## Emacs Single Shell Command API

The main _commands_ are:

| function                                                       | execution   | return value                      | spawned buffers   |
| --                                                             | :--:        | --                                | --                |
| _shell-command-to-string_ `(command)`                          | synchronous | stdout                            |                   |
| _shell-command_ `(command & output-buffer error-buffer)`       | synchronous | return code                       | stdout and stderr |
| _async-shell-command_ `(command & output-buffer error-buffer)` | synchronous | _window_ containing output-buffer | stdout and stderr |

As you can see, the three of those only accept a limited number of arguments.

But more are supported implicitly via various variables.

Here is a list of the most relevant ones:

| var                                            | description                                  |
| :--                                            | :--                                          |
| _default-directory_                            | location from which to launch shell          |
| _explicit-shell-file-name_ / _shell-file-name_ | shell interpreter exec (e.g. bash ...)       |
| _shell-command-switch_                         | switch argument to use for running a command |

So, by redefining the value of those vars we can change the behavior of the functions.

Specifically:

- by having the `default-directory` be on a remote server (via TRAMP), we can open a shell on this remote server
- by changing the other vars, we can spawn a shell from non-default interpreters (fish, zsh, ksh...)

To prevent having to redefine those values, We can let-bind those vars for the duration of the execution (dynamic binding).

```emacs-lisp
(defun my/uname-local ()
  (interactive)
  (let ((default-directory "~")
        (explicit-shell-file-name "fish"))
    (message "Launching \"uname -a\" locally")
    (message (shell-command-to-string "uname -a"))))

(defun my/uname-on-raspi ()
  (interactive)
  (let ((default-directory "/ssh:pi@raspi:/~")
        (explicit-shell-file-name "bash"))
    (message "Launching \"uname -a\" on Raspberry Pi")
    (message (shell-command-to-string "uname -a"))))
```

That's pretty cool, but I'm not too fond of this hidden part of API.


## Making the Implicit Explicit

Thankfully, it's pretty straightforward to map implicit parameters to explicit ones and define a helper function.

As we want those params to be optional, it's more convenient define them as keywords.

{::options parse_block_html="true" /}
<details><summary markdown="span">Click to toggle</summary>
```emacs-lisp
;; ------------------------------------------------------------------------
;; VARS

(defvar prf-default-remote-shell-interpreter "/bin/bash")
(defvar prf-default-remote-shell-interpreter-args '("-c" "export EMACS=; export TERM=dumb; stty echo; bash"))
(defvar prf-default-remote-shell-interpreter-command-swith "-c")


;; ------------------------------------------------------------------------
;; HELPER

(defun with-shell-interpreter--normalize-path (path)
  "Normalize path, converting \\ into /."
  ;; REVIEW: shouldn't we just useinstead `convert-standard-filename'
  ;; or even `executable-find'?
  (subst-char-in-string ?\\ ?/ path))


(defun with-shell-interpreter--get-interpreter-name (interpreter)
  (file-name-nondirectory interpreter))


;; ------------------------------------------------------------------------
;; MAIN

(cl-defun eval-with-shell-interpreter (&key form path
                                            interpreter interpreter-args command-switch)
  (unless path
    (setq path default-directory))
  (unless (file-exists-p path)
    (error "Path %s doesn't seem to exist" path))

  (let* ((func
          (if (functionp form) form
            ;; Try to use the "current" lexical/dynamic mode for `form'.
            (eval `(lambda () ,form) lexical-binding)))
         (is-remote (file-remote-p path))
         (interpreter (or interpreter
                          (if is-remote
                              prf-default-remote-shell-interpreter
                            shell-file-name)))
         (interpreter (with-shell-interpreter--normalize-path interpreter))
         (interpreter-name (with-shell-interpreter--get-interpreter-name interpreter))
         (explicit-interpreter-args-var (intern (concat "explicit-" interpreter-name "-args")))
         (interpreter-args (or interpreter-args (when is-remote prf-default-remote-shell-interpreter-args)))
         (command-switch (or command-switch
                             (if is-remote
                                 prf-default-remote-shell-interpreter-command-swith
                               shell-command-switch)))
         (default-directory path)
         (shell-file-name interpreter)
         (explicit-shell-file-name interpreter)
         (shell-command-switch command-switch))
    (cl-progv
        (list explicit-interpreter-args-var)
        (list (or interpreter-args
                  (when (boundp explicit-interpreter-args-var)
                    (symbol-value explicit-interpreter-args-var))))
      (funcall func))))
```
</details>
{::options parse_block_html="false" /}

Note that we are defining `prf-default-remote-shell-interpreter` to have a default interpreter different from local `shell-file-name`[^1].

This allows rewriting the `my/uname-local` example with:

```emacs-lisp
(defun my/uname-local ()
  (interactive)
  (eval-with-shell-interpreter
   :path "~"
   :interpreter "fish"
   :form
   '(progn
      (message "Launching \"uname -a\" locally")
      (message (shell-command-to-string "uname -a")))))
```

That's pretty cool, but having to quote _:form_ and wrap it in a `progn` is kinda cumbersome.

A macro wrapper to the rescue:

```emacs-lisp
(defmacro with-shell-interpreter (&rest args)
  (declare (indent 1) (debug t))
  `(eval-with-shell-interpreter
    :form (lambda () ,(plist-get args :form))
    :path ,(plist-get args :path)
    :interpreter ,(plist-get args :interpreter)
    :interpreter-args ,(plist-get args :interpreter-args)
    :command-switch ,(plist-get args :command-switch)))
```

Which allows us to rewrite it like so:

```emacs-lisp
(defun my/uname-local ()
  (interactive)
  (with-shell-interpreter
   :path "~"
   :interpreter "fish"
   :form
   (message "Launching \"uname -a\" locally")
   (message (shell-command-to-string "uname -a"))))
```

The code for `with-shell-interpreter` can be found in package [with-shell-interpreter](https://github.com/p3r7/with-shell-interpreter).


## Even better

Let's just spin off our own version of `shell-command-to-string`.

```emacs-lisp
(cl-defun prf/shell-command-to-string (command &key path interpreter command-switch)
  "Call CMD w/ `shell-command-to-string' on host and location described by PATH"
  (with-shell-interpreter
   :form (shell-command-to-string command)
   :path path
   :interpreter interpreter
   :command-switch command-switch))
```

Our example command becomes:

```emacs-lisp
(defun my/uname-local ()
  (interactive)
  (message "Launching \"uname -a\" locally")
  (prf/shell-command-to-string "uname -a"
                               :path "~"
                               :interpreter "fish"))
```

The code for `prf/shell-command-to-string` can be found in package [prf-shell-command](https://github.com/p3r7/prf-shell).


# Notes

[^1]: Indeed, we might want an exotic one locally (e.g. zsh) but would want a safer option for remote servers. Also, under Microsoft Windows, `shell-file-name` defaults to _cmdproxy.exe_ which is OK for local shells but sucks for remote ones...
