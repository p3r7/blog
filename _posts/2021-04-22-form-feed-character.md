---
layout: post
title: The venerable form feed character
description: The old form feed character is still relevant to this day
summary: And its modern uses
tags: [emacs]
---

![old_printer](/assets/gif/old-printer.gif){:style="max-width: var(--img-width-small)"}


## Introduction

The form feed character (aka `FF`, ASCII `0x0C`, Unicode `\f`, `^L` in caret notation) is an artifact from the past.

A past where printers where simpler beasts and relied on a this special invisible (control) character to be told of page breaks.

At a time when people printed their source codes, those characters were commonly used to delimit code sections, forcing a page break for easier reading.


## Comments as section delimiters

It's quite common, especially with programming languages that don't enforce code segmentation (non OOP-centric) to split a source file in section delimited by comments.

Your typical Python script usually looks something like:

```py
## ------------------------------------------------------------------------
## CONFIG

#[...]


## ------------------------------------------------------------------------
## HELPER FNS

#[...]


## ------------------------------------------------------------------------
## MAIN

#[...]

```

See all those delimiters made of dashes?

Those lines are generally 75 characters wide to fit under the 80 character width a lot of editors/terminal windows have by default.

This allows displaying the whole line (with no wrap) under most circumstances.

People generally use extensions / keyboard shortcuts to insert those quickly.


## Form feed characters as an alternative

Using a line composed of a single `FF` character instead provides many benefits:
- on supported software, it gets displayed as a solid horizontal line taking the whole window width
- it's detected as a comment so no need to prefix it with a commenting character (e.g. `#` for Python)
- it can be typed quickly (`C-q-l` in Emacs)

In fact form feed characters can be found in many Clojure, Elisp, C and Python source codes.


## Form feed delimiter in Emacs

Emacs' source code features an extensive use of the `FF` character.

The output of `compilation-mode` uses this character (typically when updating several package).

Strangely, by default, Emacs display those characters in caret notation (`^L`), which is not super user-friendly.

Thankfully, either package [form-feed](https://depp.brause.cc/form-feed/) or [page-break-lines](https://github.com/purcell/page-break-lines) allows displaying them as a proper horizontal delimiter.

![ff_char_emacs](/assets/img/ff-char-emacs.png)


## Form feed delimiter in other editors

[UltraEdit](https://www.ultraedit.com/) render those appropriately natively.

[@Alhadis](https://github.com/Alhadis) made an [package](https://atom.io/packages/form-feeds) to bring this feature to Atom.

Sadly, nothing seem to exist for IntelliJ IDEA, Sublime Text nor VSCode.


## Form feed delimiter on GitHub

GitHub doesn't style those `FF` line section delimiters.

Thankfully it was trivial to make a [userscript](https://en.wikipedia.org/wiki/Userscript) to enable this support.

Introducing: [p3r7/gh-userscript-form-feed-line](https://github.com/p3r7/gh-userscript-form-feed-line)

![ff_char_github](/assets/img/ff-char-github.png)

The code is small enough to be shared. The trick is to replace `FF` lines with `<hr/>` html elements:

```js
$(document).ready(function(){
    const form_feed="";
    $("td.js-file-line:contains('" + form_feed + "')").html('<hr/>');
    $("td.js-file-line > hr").css('margin', '10px auto auto 0');
});
```


## In Conclusion

Using a form feed character as a section delimiter is an elegant solution.

Sadly, few people seem to know about it and few software support it.

So if you want portability, you'd have to stick with the comment\-line\-made\-of\-(dashes|underscores|whatever) solution.

If you work on an ecosystem that fully support those, such as Emacs packages, I encourage you to try it.

In any case, it's sad that "modern" editors don't support this "old" yet still relevant feature.

Open issues / PRs. Create plugins/extensions. Spread the word.
