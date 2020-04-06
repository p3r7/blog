---
layout: post
title: Overriding part of an Emacs theme
description: Defining Emacs theme overrides
summary: Because I like most of it but not all
tags: [emacs]
---


## Introduction

Like most sane people, I don't use Emacs' default theme but instead rely on custom themes.

Through a thoughtful choice of colors, the venerable editor can feel like a modern beast (or a retro oddity if that's what you're into).

I [cycle through several themes](https://github.com/p3r7/prf-theme) but my current favorite is [chocolate](https://github.com/SavchenkoValeriy/emacs-chocolate-theme). I def' recommend it: very easy on the eyes and yummy 🍫.

Other favorites are [white-sand](https://github.com/mswift42/white-sand-theme), [dracula](https://github.com/dracula/emacs) and for a more retro vibe [comidia](https://github.com/emacs-jp/replace-colorthemes).


## A matter of taste

I really love the themes I use. At least 95% of them.

But there are several discrepancies between them that annoy me:
 - whether variable pitch is on for outlines
 - whether different outline levels have different colors
 - mode-line look: either "box" or "flat"
 - active buffer mode-line not standing out sufficiently
 - face `form-feed-line` (for `^L` character) not of the same color as comments
 - `show-paren-*` faces colors not standing out
 - `region` color too close to `hl-line` and / or `cursor`
 - ...

Yeah, I'm a picky dude.

The way I used to deal with this was by forking the themes and maintaining my own versions.

But this was cumbersome and I had the feeling there was a better way.


## Enters Spacemacs

I stumbled on [this blog post about beautifying org-mode](https://lepisma.xyz/2017/10/28/ricing-org-mode/) on [Abhinav Tushar's blog](https://lepisma.xyz/) and realized that Spacemacs has _"native"_ theme override capabilities.

Once I discovered this I decided to ~~switch to Spacemacs~~ delve into Spacemacs source code to find out how it's done.

It appears that the outer part of Spacemacs' code is divided into special kinds of packages called [layers](https://www.spacemacs.org/layers/LAYERS.html) (as opposed to the [core part](https://github.com/syl20bnr/spacemacs/tree/master/core)).

The theme override code is provided by the [theming layer](https://www.spacemacs.org/layers/+themes/theming/README.html) ([code](https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Bthemes/theming)) and the trick seems to be to call `custom-set-faces`.


## Porting to vanilla Emacs 🍦

I could have called `custom-set-faces` directly from my init.el but got a better idea.

Why not reuse the same format as Spacemacs for defining theme overrides?

And why not just simply port the whole layer to vanilla Emacs as regular package?

As it turns out, it was pretty trivial to do.

Introducing new package: [space-theming](https://github.com/p3r7/space-theming).

Also please note for Emacs 27 users, you might have to set the following:

```emacs-lisp
(setq custom--inhibit-theme-enable nil)
```

## Bonus example

As a bonus here is my setup for `chocolate-theme`:

```emacs-lisp
(setq space-theming-modifications
      '((chocolate
         ;; strawberry active region
         (region :background "#C77497" :foreground "black")

         (show-paren-match :background "white" :foreground "black")
         (show-paren-mismatch :background "red" :foreground "white")

         ;; active / inactivate buffer mode-line
         (mode-line :background "#594A3B") ; chocolate-dark-yellow
         (mode-line-inactive :background "#2b241d") ; darker derivative of chocolate-dark-yellow

         (fringe :background "#2b241d") ; same as mode-line-inactive

         ;; ^L are comments
         (form-feed-line :strike-through "#705B5F") ;; :foreground of `font-lock-comment-delimiter-face'

         ;; babel
         (org-block :inherit default)
         (org-block-begin-line :foreground "#594A3B")
         (org-block-end-line :foreground "#594A3B")

         ;; dired
         (dired-directory :foreground "#EAEAFE") ; chocolate-hue-2

         ;; bookmarks
         (bmkp-local-directory :foreground "#45AFBD")
         (bmkp-remote-file :foreground "#C55D67")))
```

![chocolate theme overrides](/assets/img/emacs-chocolate-theme-overrides.png)

You can see my customization on the left VS the default faces on the right.

Please note:

 - it's way clearer that the bottom buffer is the active one
 - the region went from _"I'm a ninja and want to look the same as a mode-line"_ to _"Hey look at me!"_.
 - the unobtrusive form feed in the top buffer
