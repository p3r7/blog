---
layout: post
title: Custom Org-mode link abbrevs
description: Shorter Org-mode link abbrevs
summary: The less I type, the merrier
tags: [emacs,elisp,productivity]
---


## Org-mode links

One of the many selling point of using Org-mode as a personal wiki (aka _digital garden_, see [previous post](/2021/09/15/org-roam)) is its [_(hyper)link_ feature](https://orgmode.org/manual/Hyperlinks.html).

As we've seen previously, we can define _internal_ links between Org-file (and/or their outlines), but also [_external_ links](https://orgmode.org/manual/External-Links.html) targeting external files & sytems. Those can either point to stuff openable in Emacs itself (e.g. link to files, to a shell command...) or external applications if configured as such (e.g. web pages).


## URL abbrevs

Conveniently, pasted URL are directly resolved as `http(s)` links.

As usually, we may have a bunch of links to a few websites, it can be convenient to be able to have those in a shorter form.

Conveniently, Org-mode support [link abbreviations](https://orgmode.org/manual/Link-Abbreviations.html).

For example, we may define our list of recurrent linked website.

```elisp
(setq org-link-abbrev-alist '(("gh" . "https://github.com/")
                              ("gl" . "https://gitlab.com/")
                              ("hn" . "https://news.ycombinator.com/item?id=")
                              ("lines" . "https://llllllll.co/t/")
                              ;; [...]
                              ("thing" . "https://www.thingiverse.com/thing:")))
```

One would just have to type `[[gh:emacs-mirror/emacs]]` instead of `https://github.com/emacs-mirror/emacs` to insert a link to Emacs' github mirror.


## Even faster & shorter URL abbrevs

The above solution is nice if we use `org-insert-link` to insert links.

But I personally find this command cumbersome. It prompts me 3 times (link type, actual link, description).

I want to go faster and just having to type `gh:emacs-mirror/emacs` to get a valid link.

One way to achieve this is to declare [custom link types](https://orgmode.org/manual/Adding-Hyperlink-Types.html).

The solution becomes:

```elisp
(require 'dash)

(setq my-org-link-abbrev-alist '(("gh" . "https://github.com/")
                                 ;; [...]
                                 ("thing" . "https://www.thingiverse.com/thing:")))

(--each my-org-link-abbrev-alist
  (let* ((link-prefix (car it))
         (browse-fn `(lambda (e)
                       (let ((org-link-abbrev-alist my-org-link-abbrev-alist))
                         (browse-url (org-link-expand-abbrev (concat ,link-prefix ":" e)))))))
    (org-link-set-parameters link-prefix :follow browse-fn)))
```

Please note that we renamed `org-link-abbrev-alist` to `my-org-link-abbrev-alist` to prevent having duplicated prefix entries if we ever want to call `org-insert-link`.


## Auto-shortening abbrev'ed links

Most of the time, I just copy/paste an URL into an Org buffer.

Wouldn't it be convenient if it would automagically shorten it if it correspond to a known abbrev?

Thankfully, this is relatively trivial by _advising_ `org-yank`.

```elisp
(require 's)

(defun my-org-link-apply-prefix (txt)
  "Rework link TXT, swapping prefix w/ shorted one if matches
`my-org-link-abbrev-alist'."
  (let ((prfx (--some (and (s-starts-with? (cdr it) txt)
                           (not (string= (cdr it) txt)) it)
                       my-org-link-abbrev-alist)))
    (if prfx
        (s-replace (cdr prfx) (concat (car prfx) ":") txt)
      txt)))

(defadvice org-yank (around prf/org-yank-prefix-link activate)
  "Advice around `org-yank' that will auto-compact current entry in `kill-ring'
if it matches `my-org-link-abbrev-alist'."
  (let* ((kill (or (and kill-ring (current-kill 0)) ""))
         (new-kill (my-org-link-apply-prefix kill)))
    (unless (s-blank? new-kill)
      (kill-new new-kill t))
    ad-do-it))
```

Please not that `my-org-link-apply-prefix` only covers "basic" prefix abbrevs. It doesn't support formatted abbrevs (`%s`, `%h`) nor formatting using a custom function (`%(<CUSTOM-FN>)` for which we'd need the inverse function).


## Conclusion

As always, this shows how flexible Emacs is and how expressive Elisp can be (once you are used to its quirks).

We showcased a very basic example of custom links. One could really go crazy with those.

For more in-depth examples, check out [this article](https://kitchingroup.cheme.cmu.edu/blog/2016/11/04/New-link-features-in-org-9/) from the _The Kitchin Research Group_ blog. They wrote [a bunch of advanced articles about Org](https://kitchingroup.cheme.cmu.edu/blog/category/orgmode/), notably about _Babel_ (Org's JupyterLab equivalent).
