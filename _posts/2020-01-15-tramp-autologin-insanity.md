---
layout: post
title: Better TRAMP autologin
description: Using SSH keys with tramps
summary: Using SSH keys with tramps
tags: [emacs]
---


# Introduction

For those not in the know, [TRAMP](https://www.gnu.org/software/tramp/) is one of Emacs' killer apps. It's a package that allows to interact with remote systems through a [variety](https://www.gnu.org/software/tramp/#Inline-methods) [of](https://www.gnu.org/software/tramp/#External-methods) [protocols](https://www.gnu.org/software/tramp/#GVFS-based-methods).

Such interactions include opening shells and browsing remote file trees as if they were locally mounted. You can even bounce across several machines like if it was nothing (it's called [multi-hops](https://www.gnu.org/software/emacs/manual/html_node/tramp/Ad_002dhoc-multi_002dhops.html)).

But there is one thing can break these otherwise seemless interactions: login prompts.


# The Authinfo way

To get rid of them, emacs offers natively support for Gnus Authinfo and .netrc files via its `auth-source` package:

```emacs-lisp
(require 'dash)

(use-package auth-source
  :demand
  :no-require t
  :config
  (setq auth-sources (-filter #'file-exists-p '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc"))))
```

Now you can just create a `~/.authinfo` like this:

```
machine raspi login pi password raspberry
```

And then `C-x f /ssh:pi@raspi:/` and it will connect automatically, yey!

...Except that sucks.


# SSH keys to the rescue

Indeed passwords suck. Big time. Even more so when you have a gazillion of remote hosts to connect to.

SSH keys (aka identity files, aka certificates) are a way smarter (and in most cases safer) way to proceed.

Given you have a master key for connecting to all of your machines, just have to drop into your `~/.ssh/config`:

```
Host *
    User eigen
    IdentityFile ~/.ssh/eigen-identity
```

And you could connect to any server for which this key is known for user `eigen`.


# The PuTTY dilemna

If you're on Windows, you're most likely using PuTTY/plink as an SSH alternative.

TRAMP, in all its glory, supports PuTTY (use the `/pscp:` method, that's the one you're looking for).

PuTTY has an equivalent way to set a default key, via a [Default Setting](https://the.earth.li/~sgtatham/putty/0.73/htmldoc/Chapter4.html#config-saving) or through [Pageant](https://the.earth.li/~sgtatham/putty/0.73/htmldoc/Chapter9.html).

The former only works if you're saving each and every of your hosts as a connection profile and access it with `/plinkx:<PROFILE>:`.

The latter only seems to load when lauching putty.exe (GUI interface).

So here comes time for ~~adventure~~ insanity.


# Insanity (or elisp-bind all the things)

Another cross-platform solution is to do this in pure elisp.

The trick is to enrich `tramp-methods` with an additionnal args corresponding to the identity file option (`-i`).

Thus we need some utils to alter those method definitions.

{::options parse_block_html="true" /}

<details><summary markdown="span">Click to reveal!</summary>
```emacs-lisp

;; ------------------------------------------------------------------------
;; DEPS

(require 'tramp)
(require 'dash)

;; ------------------------------------------------------------------------
;; TRAMP METHODS ARGS

(defun prf/tramp/method/def/some-args/with-cert (some-args cert-arg cert)
  "Returns enriched tramp def SOME-ARGS with certificate arg.
SOME-ARGS can be of type `tramp-login-args' or `tramp-copy-args'"
  (let ((args-type (car some-args))
        (args (car (cdr some-args))))
    (add-to-list 'args `(,cert-arg ,(concat "\"" cert "\"")))
    `(,args-type ,args)))

(defun prf/tramp/method/def/with-cert-in-some-args (tramp-method-def args-type cert-arg cert)
  "Returns copy of TRAMP-METHOD-DEF with certificate arg added to ARGS-TYPE.
ARGS-TYPE can be `tramp-login-args' or `tramp-copy-args'."
  (let ((method-name (car tramp-method-def))
        (method-def-args (cdr tramp-method-def)))
    (cons method-name
          (-map-when
           (lambda (e) (equal (car e) args-type))
           (lambda (e) (prf/tramp/method/def/args/with-cert e cert-arg cert))
           method-def-args))))

;; ------------------------------------------------------------------------
;; TRAMP METHODS

(defun prf/tramp/method/def/with-cert-in-args (tramp-method-def cert-arg cert)
  "Returns copy of TRAMP-METHOD-DEF enriched with certificate arg.
Certificate arg gets added to both 'tramp-login-args and 'tramp-copy-args."
  (-> tramp-method-def
      (prf/tramp/method/def/with-cert-in-some-args 'tramp-login-args cert-arg cert)
      (prf/tramp/method/def/with-cert-in-some-args 'tramp-copy-args cert-arg cert)))
```
</details>

{::options parse_block_html="false" /}

Then we can override the method definitions:

```emacs-lisp
;; PuTTY
(let ((cert-path "~/my-cert.ppk")
      (putty-methods '("pscp" "plink" "plinkx" "psftp")))
  (setq tramp-methods
        (-map-when
         (lambda (e) (member (car e) putty-methods))
         (lambda (e) (prf/tramp/method/def/with-cert-in-args e "-i" cert-path))
         tramp-methods)))

;; SSH
(let ((cert-path "~/.ssh/id_dsa")
      (ssh-methods '("ssh" "sshx")))
  (setq tramp-methods
        (-map-when
         (lambda (e) (member (car e) ssh-methods))
         (lambda (e) (prf/tramp/method/def/with-cert-in-args e "-i" cert-path))
         tramp-methods)))
```

The beauty of this is that if your key is not known to the remote host, it would still prompt you for a password without failing.

The code can be found in package `prf-tramp-method` avaialble at [p3r7/prf-tramp](https://github.com/p3r7/prf-tramp).
