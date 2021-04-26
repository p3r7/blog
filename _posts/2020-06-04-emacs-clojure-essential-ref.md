---
layout: post
title: cider-doc to Clojure, The Essential Reference
description: A new package to jump from within Emacs to definition of Clojure symbol in book Clojure, The Essential Reference
summary: Jump to a more exhaustive core lib documentation
tags: [clojure,emacs]
---


## The Clojure Standard Library

Clojure's standard library (aka the Core Library) is well thought-of and a pleasure to use.

But it's pretty massive with around [700 functions and macros provided](https://clojuredocs.org/quickref)[^1], some of them having several (potentially tricky) use-cases and ways to combine them.

Moreover their documentation tend to be very concise, working more like little refreshers.

A step up is to use [ClojureDocs](https://clojuredocs.org/) and its user-provided examples.


## A proper reference

I stumbled upon [Clojure, The Essential Reference](https://www.manning.com/books/clojure-the-essential-reference)[^2], a book that aims at being a proper reference to the Core Library, with exhaustive descriptions and examples.

It has become my go-to reference for getting the specificities of core functions and macros.


## Jump to documentation from within Emacs

Emacs and [CIDER](https://cider.mx/) provide convenience commands to jump to the documentation of a symbol:

 - `cider-doc`: Show the docstring of the symbol (+ related symbols)
 - `cider-clojuredocs` and `cider-clojuredocs-web`: Same but use ClojureDocs as a source,

I quickly wanted to have a similar capability with _Clojure, The Essential Reference_, thus I started writing a package.

Sadly I didn't find any programmatic symbol index I could parse. So I decided to make one with semi-automated parsing of the book's _Table of Content_.

Introducing new packages [clojure-essential-ref](https://melpa.org/#/clojure-essential-ref) / [clojure-essential-ref](https://melpa.org/#/clojure-essential-ref-nov) (optional epub support):

[![p3r7/clojure-essential-ref - GitHub](https://gh-card.dev/repos/p3r7/clojure-essential-ref.svg){:style="max-width: var(--img-width-gh-card)"}](https://github.com/p3r7/clojure-essential-ref)

I you find anything missing, don't hesitate to suggest corrections with tickets or PRs.


## Notes

[^1]: For comparison, C's provides [half of that](https://www.ibm.com/support/knowledgecenter/ssw_ibm_i_73/rtref/stalib.htm).

[^2]: It's not free. A limited amount of content can be freely consulted each day in the linked online version, though.
