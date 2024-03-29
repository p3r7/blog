---
layout: post
title: Emacs is no text editor
description: An attempt to define Emacs
summary: ... and that's OK
tags: [emacs]
---

... and that's OK.


## The Start of a Journey

![first blade](/assets/img/first_blade.jpg){:style="max-width: 450px"}

You also might have gone through a phase of extensive search for the ULTIMATE VERY BESTEST TOOL for your first / next / hypothetical programming project.

To only find yourself at the middle of a battlefield.

Solutions are enumerated and compared. Everywhere you look, everybody seems to argue there is only one truth but not consensus is in sight.

You feel like you cannot proceed further until [the conflict is resolved](/assets/img/highlander.jpg).

As far away as you look Emacs seems to be mentioned and compared against other "editors".

But I dare say it's unfair and reductive to consider Emacs as such.


## The misconception

You'll see it everywhere. This statement.

>Emacs is an [extensible\|free\|customizable\|bloated] text editor.

On blog posts, forums, Linux package descriptions, [Wikipedia](https://en.wikipedia.org/wiki/Emacs), [emacs.sexy](https://emacs.sexy/) and even on the [official site](https://www.gnu.org/software/emacs/).

But the latter quickly rectifies with:

>At its core is an interpreter for Emacs Lisp, a dialect of the Lisp programming language with extensions to support text editing.

Yeah, that seems more correct. But it resumes the implementation and not the function.

Everybody knows this Vim hooligan proverb:

>Emacs is a great operating system, lacking only a decent editor.

And even though it's humoristic (thus by design slightly incorrect) I'm more OK with this definition than with the "text editor" one.


## The whys & hows

I guess that the main reason why people state that Emacs is (primarily) a text editor is to make it easier to grasp.

![emacs could be anything](/assets/img/emacs_anything.jpg){:style="max-width: 300px"}

In fact it was not conceived as a text editor with extensibility as a feature.

Instead, __Emacs was conceived as an extensible environment with text editing as a feature.__

Even if this was originally a means to an end[^1], this important design decision has allowed Emacs to evolve into something broader.


## Proposal for a definition

Let's attempt to define Emacs' essence:

>Emacs is a generic user-centric text manipulation environment.

<center><p><em style="color:gray;">“Wow, that looks like the definition of a text editor!”</em></p></center>

OK, let's explain this gibberish.

_User-centric_ means Emacs is built around user interactions. It needs user input (_commands_) to perform any actions[^2] and the results of those actions are presented to the user (via side-effects).

Even something as simple as moving the cursor is modelized as a _command_ side-effecting the operation[^3].

Please also note that by _text_ we target any _character string_, not necessarily "human-understandable words". A more explicit term would be _textual data_.

User-driven actions consist of and are built upon the _manipulation_ of said textual data.

Manipulation is a very broad term, and that's because Emacs is very broad in its capabilities.

Indeed, it relies on _generic_ APIs to interact with text. This abstraction allows it to interact with anything that "speaks" _textual data_ (files, HTTP APIs, databases...).

In other words:

>Emacs is a generic Man-Machine Interface for anything text.

<center><p><em style="color:gray;">"Wow, that still looks like the definition of an editor!"</em></p></center>

Yes, but that's also the the definition of a [task planner](https://orgmode.org/), [file browser](https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired.html), [terminal emulator](https://www.gnu.org/software/emacs/manual/html_node/emacs/Shell.html), [email client](https://www.djcbsoftware.nl/code/mu/mu4e.html), [diffing tool](https://www.gnu.org/software/emacs/manual/html_mono/ediff.html)[^4], [remote server access tool](https://www.gnu.org/software/tramp/) (SSH, FTP...), [git frontend](https://magit.vc/), [HTTP client](https://github.com/pashky/restclient.el) / [server](https://github.com/skeeto/emacs-web-server)...

And Emacs is all of that. Not because it's a *bloated* editor, but because it's a **generic tool** for everything text-related, including editing.


## A bit of nuance

This is not a glorification of Emacs.

Emacs also has it quirks that sometimes makes it painful to work with.

Some APIs are showing their age (in their design and naming conventions) and programming in elisp can genuinely feel like retro-computing.

Having a global variable/function context is user-friendly, but the lack of parallel threading is painful when the interface freezes[^5].

Our expectation of what an editor should be has evolved quite a bit over time (from text editor, to code editor to programming environment).

Emacs has managed to stay competitive thanks to its native adaptability.

With no expertise, I assume that newer kids (such as Sublime, VSCode...) got designed from the ground with levels of abstractions that make them closer to Emacs in design than older traditional text editors.

The ability to override and extend core functionalities (_"they told me i could be anything"_) is maybe the only thing that keeps Emacs a different beast, but is a nightmare from a security and stability standpoint.


## Closing thoughts

Emacs is my main MMI for textual data.

I've invested countless hours tailoring it to my needs but have saved oh-so-many more reducing otherwise repetitive tasks to a minimal amount of keystrokes performed at the speed of thought.

It has terrible defaults.

Other tools can be seen as blades that gets polished through configuration and plugins.

For Emacs a better analogy would be a rough piece of steel you'd have to extract from a stone, forge and tailor to your exact needs.

You'd have to learn its lingo and idiosyncrasies.
You'd have to [learn Elisp](https://github.com/p3r7/awesome-elisp).
You will certainly find yourself more efficient using a collection of other tools.

But at the end of the journey you may end up with your very own Excalibur.

And this is, I believe, how Emacs should get advertised instead of as being only an "editor".

![excalibur](/assets/img/excalibur.png){:style="max-width: 450px"}


## Similar & complementary thoughts

[Irreal](https://irreal.org/blog/) blog [picked up on this article](https://irreal.org/blog/?p=8809) with an extremelly well put analysis. They share a similar point of view (_"[Emacs is] a Lisp interpreter specialized for dealing with text that has an editor as one of its built-in applications"_) and make an important clarification regarding Emacs original vision and design decisions:

>[...] I don’t think it’s true [...] that Emacs was designed ab initio “as an extensible environment with text editing as a feature”. That claim is trivially false if you take the statement literally. After all, Emacs was originally implemented as a set of macros on top of the TECO editor. But even later, stand-alone versions of Emacs were not conceived as general purpose environments the way Emacs is often thought of today.

[Karl Voit](https://karl-voit.at/) made a similar statement [back in 2015](https://karl-voit.at/2015/10/23/Emacs-is-not-just-an-editor/), presenting Emacs as a _"LISP interpreter and thus [...] very capable highly dynamic platform"_. One other key point is that _"comparing Emacs to an editor is [...] unfair and inadequate [...]. Those stupid editor flame-wars are not worth the time at all."_.

[Two Wrongs blog](https://two-wrongs.com/) features [a post back in 2018](https://two-wrongs.com/why-you-should-buy-into-the-emacs-platform) that makes the same statement but with other words: _"Emacs is a platform for developing end-user applications with a text-based core"_.

[Mario Lang](https://blind.guru/author/mario-lang.html) wrote [a post on emacs-devel](https://lists.gnu.org/archive/html/emacs-devel/2020-09/msg00286.html) praising Emacs for its _modality-independency_, i.e. the decoupling of the functionalities (such as editing) from how they get interacted with. This aspect makes Emacs a stellar tool in term of accessibility.

[Vladimir Dikan](https://www.ivorysiegetower.com/) made a similar point in his [Ode to Emacs](https://www.ivorysiegetower.com/posts/an-ode-to-emacs.html) post about Emacs being a _"textual UI tool"_ and how useful it can be to _"unify[...] the text-processing tasks"_.

[Bozhidar Batsov](https://batsov.com/) sums it up in his [Why Emacs: Redux](https://batsov.com/articles/2021/11/16/why-emacs-redux/) post:

>Emacs is not really an editor either! I believe that Emacs is the ultimate editor building material. The out-of-the-box experience is kind of basic and somewhat weird, but I don’t think that anyone should be using Emacs like this. You take this simple foundation, you shape and mold it to your taste and preferences and you end up with the best possible editor for you and you alone. That’s the reason why you should consider using Emacs.

[Ashton Wiersdorf](https://lambdaland.org/) makes an interesting point of how [Emacs doesn't really contradict the UNIX philosophy](https://lambdaland.org/posts/2022-11-07_unix_philosophy/): it's just an alternative _"tool forge"_ (i.e. _platform_) to the command line, packages being akin to CLI programs (i.e. _apps_).


## Notes

[^1]: The "end" being to build a text editing tool. Furthermore this vision was not present unil later Emacs iterations. See the [Irreal blog response to this article](#similar--complementary-thoughts) for the whole picture.
[^2]: Not true as modern Emacs can do stuff independently of user input. But the implementation is often hackish (timers...). Furthermore, doing stuff programmatically rely on user-centric APIs (temp buffers, moving point and mark around...).
[^3]: Just do `C-h k` following by any arrow key and see for yourself.
[^4]: You might want to take a look at [ztree](https://github.com/fourier/ztree) in complement to ediff for recursive directory diffing.
[^5]: See the ["No Threading" page on the wiki](https://www.emacswiki.org/emacs/NoThreading)
