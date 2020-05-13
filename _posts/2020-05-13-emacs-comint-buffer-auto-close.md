---
layout: post
title: Auto-close Emacs interpreter buffers
description: Using a process sentinel to auto-kill Emacs comint buffer
summary: When the process dies, so does the buffer
tags: [emacs]
---


## Quitting a shell

On any other terminal emulator, typing `exit` or `C-d` would kill the shell process **and** the terminal window.

But Emacs doesn't work this way.

When you exit a shell process, your shell buffer stays open:

```
$ ^D
exit

Process shell finished
```

And you'd have to `C-x k`-it (`kill-this-buffer`) manually.

This can be annoying and we might want to change that.

But first we need to delve a bit in Emacs internals.


## Emacs bestiary: shell, comint & process

The Emacs implementation of dealing with terminal emulation is divided in three API layers[^1]:

 - the _shell_ layer (dealt with previously on this blog: [part 1](/2020/01/19/painless-emacs-shell-commands) and [2](2020/01/21/painless-emacs-interactive-shells))
 - the _comint_ layer
 - the (inferior) _process_ layer

The _process_ layer deals with spawning inferior processes, be it synchronously (`call-process` and `process-file` functions) or asynchronously (`make-process` and `start-process`).

The _comint_ is primarily comprised of a major mode (`comint-mode`) for interacting with spawned processes. specifically suited for interpreters (and dealing with user input).

Finally, the _shell_ layer is the higher-level set of APIs built on top of comint and specifically suited for interacting with shell interpreters.


## Sentinels

The _process_ layer offers to bind a _sentinel_ to each spawned process, a callback function that gets run every time the process changes state.

Notably, it runs when the process dies.

So by crafting the appropriate sentinel, we can have the buffer close automatically when we exit its running process:

```emacs-lisp
(defun my-kill-buffer-sentinel (process output)
  "Process sentinel to auto kill associated buffer once PROCESS dies."
  (unless (process-live-p process)
    (kill-buffer (process-buffer process))))

(let ((current-prefix-arg '(4)) ;; don't prompt for interpreter
      (shell-buffer (shell)))   ;; spawn shell and retrieve buffer
  (let ((process (get-buffer-process shell-buffer)))
    (set-process-sentinel process #'my-kill-buffer-sentinel)))
```


## Generalizing

The previous example works but has 2 limitations:

- some mode spawn comint buffers with important code in their associated sentinels, we don't want to override it
- me somewhat manually bind the sentinel to the shell buffer

First let's deal with the first issue:

```emacs-lisp
;; -*- lexical-binding: t; -*-

(require 'dash)

(defun add-my-kill-on-exit-sentinel ()
  "Replace current process sentinel with a new sentinel composed of the current one and `my-kill-buffer-sentinel'."

  (let* ((process (get-buffer-process (current-buffer)))
         (og-sentinel (process-sentinel process))
         (sentinel-list (-remove #'null
                                 (list og-sentinel #'my-kill-buffer-sentinel)))
         (combined-sentinel
          (lambda (process line)
            (--each sentinel-list
              (funcall it process line)))))
    (setf (process-sentinel process) combined-sentinel)))
```

Calling `add-my-kill-on-exit-sentinel` creates a new sentinel from the one already bound to the buffer plus our `my-kill-buffer-sentinel`. Hence the original sentinel isn't replaced but instead enriched.

Now for the second issue, let's automatically call `add-my-kill-on-exit-sentinel` on any new comint buffer:

```emacs-lisp
(defvar my-kill-on-exit-comint-hook-has-run nil
  "Whether or not `kill-on-exit-comint-hook' has run or not.
We need this buffer-local var to prevent the hook from running
   several times, as can happen for example when calling `shell'.")

(defun my-async-funcall (function &optional buffer args delay)
  "Run FUNCTION with ARGS in the buffer after a short DELAY."
  (run-at-time (or delay 0.2) nil
               `(lambda ()
                  (with-current-buffer ,buffer ,(cons function args)))))

(defun kill-on-exit-comint-hook ()
  (unless my-kill-on-exit-comint-hook-has-run
    (setq-local my-kill-on-exit-comint-hook-has-run t)
    (my-async-funcall #'add-my-kill-on-exit-sentinel (current-buffer))))

(add-hook 'comint-mode-hook #'kill-on-exit-comint-hook)
```

The trig calling `add-my-kill-on-exit-sentinel` by registering a new hook callback to the `comint-mode-hook`.

The trick here is to wait for the comint derived modes to register their custom sentinel before enriching it. That's why we use `my-async-funcall`.

Finally, `comint-mode-hook` can get triggered several times for a same buffer. To ensure we only apply our changes once, we define the state variable `my-kill-on-exit-comint-hook-has-run` with a buffer-local value.


## Notes

[^1]: Welp, there is also _term-mode_, a different beast volountarily omitted in the sake of conciseness.
