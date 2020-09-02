---
layout: post
title: Emacs in odd places
description: Emacs found in unfamiliar situations
summary: Does it run Emacs?
tags: [emacs,retrocomputing]
---

## Unfamiliar situations

I find it amusing spotting familiar software in equally unfamiliar situations.

One of those situations is when it [makes a cameo in fiction](https://ilikeinterfaces.com/), with examples such as [nmap](https://nmap.org/movies/) and the [inclusion of Emacs & eshell in Tron Legacy](https://boingboing.net/2011/04/06/how-emacs-got-into-t.html).

Another unfamiliar situation is when the software runs on an unexpected hardware or OS.

>Can it run Doom?

It's common sense that almost every device with a bare minimum of computing power [can run Doom](https://itrunsdoom.tumblr.com/)[^1].


## Emacs on odd hardware & software

Emacs has in essence less potential than Doom due to its comparatively hefty requirements (especially in its modern versions).

But that just forces us to take other approaches:

 - Use an early Emacs version compatible with the device
 - Use a fork/clone with low system requirements: [uEmacs](https://github.com/torvalds/uemacs) or [JOVE](https://en.wikipedia.org/wiki/JOVE)
 - Use our odd hardware as a dumb terminal connecting to a remote modern version of Emacs

The first approach is basically called [retrocomputing](/tag/retrocomputing/) and can be painfully cumbersome. It's only reserved to the most stubborn internet archive dvelvers.

The thirds approach works surprisingly well as Emacs kept good support for terminal rendering[^2].

Actually this goes further as the [GUI rendering is built on top of the TUI codebase](https://www.facebook.com/notes/daniel-colascione/buttery-smooth-emacs/10155313440066102).

>Whoever made Emacs into a native X11 program [...] pretended the GUI was a text terminal.
>  -- Daniel Colascione


## Emacs on the Go

Another appeal of exploring alternative hardware is the perspective of having an Emacs instance on the go.

Nowadays the best approach is to have an Android device with Emacs installed through [Termux](https://termux.com/) and a bluetooth keyboard.

Most people use a smartphone. If the screen feels too small, one can use a tablet[^4].

As far as Org-mode is concerned, we nowadays have decent applications such as [Orgzly](http://www.orgzly.com/) (Android), [beorg](https://beorgapp.com/) (iOS) and [organice](https://organice.200ok.ch/) and [org-web](https://org-web.org/) (web, mobile-friendly).

Back in 2007, smartphones (as we know them) where in their infancy and I was in dire need for a solution.

At the time, the only possibility was to find an [UMPC](https://en.wikipedia.org/wiki/Ultra-mobile_PC). Without much money in the bank, I settled for an old (even for the time) HP Jornada 680, bought secondhand for not much.

It ran a custom version of Windows CE (in German) but a distribution of Linux ([Jlime](https://web.archive.org/web/20160301064554/http://jlime.com/wiki/home)) could be run on it.

Emacs wasn't available in its repositories but I was able to compile it after fetching all of its dependencies.

And here I was, with my little [data island](https://en.wikipedia.org/wiki/Data_island) in my backpack.

![emacs_on_hp_jornada_680](/assets/img/emacs_on_hp_jornada_680.jpg){:style="max-width: 450px"}


## Emacs on a actual dumb terminal

Even the most recent versions of Emacs still have [stellar support for hardware terminals](https://www.gnu.org/software/emacs/manual/html_node/emacs/Terminal-Init.html).

I don't know who (apart from a few enthusiasts like me) still access Emacs through those devices.

There are some configuration specificities (addressed in a latter post) but things mostly just work.

![emacs_on_dec_vt320](/assets/img/emacs_on_dec_vt320.jpg){:style="max-width: 450px"}


## Other examples spotted in the wild

#### Esotheric OS

Emacs under [Haiku](https://depot.haiku-os.org/#!/pkg/emacs/haikuports/26/2/-/-/1/x86_64?bcguid=bc157-URXX) ([screenshot](https://imgur.com/GCa6CFK)).


#### Niche Linux-based smartphones

Emacs on the [Purism Librem 5](https://twitter.com/puri_sm/status/1144394797517402114).

Emacs on the [PinePhone](https://social.pixie.town/@theonefreeman/103623829020543902)

Emacs under [Jolla SailfishOS](https://www.reddit.com/r/unixporn/comments/1wwpe3/gnu_emacs_in_sailfish_os/).

Emacs (GUI version) under [Ubuntu Touch](https://imgur.com/ZGWH6Nm).


#### Older phones

Emacs 23 running natively on the [Nokia N900](https://talk.maemo.org/showthread.php?s=c5e3e5b32f8fe6d2a08d76503164c9d5&t=37241) ([scrrenshot](https://talk.maemo.org/attachment.php?s=c5e3e5b32f8fe6d2a08d76503164c9d5&attachmentid=7364&stc=1&d=1266616914))[^3].

Emacs accessed from SSH on an [old Nokia phone](http://archive.eglug.org/node/27).


#### PDAs and UMPCs

Emacs on the [GeminiPDA](https://twitter.com/komecha/status/1021370513707347969) (Android-based).

Emacs on the [Psion 5mx](http://muru.com/linux/psion/kernel/).

Emacs 21 running natively on the [Sharp Zaurus SL-C1000](https://web.archive.org/web/20160426190322/http://pda.sukareruhito.com/2007/05/zaurus_slc1000emacshowm.html)[^5].

UEmacs fork [Ng](https://web.archive.org/web/20060610210357/http://tillanosoft.com/ce/ngj.html) on the [SoftBank X01HT](https://web.archive.org/web/20150514125016/http://pda.sukareruhito.com/2007/05/x01htemacsng_for_win32micro_em.html).

Emacs accessed from SSH under [Palm OS 5](http://www.sealiesoftware.com/pssh/).


#### Single board computers

Emacs on the [Pocket C.H.I.P.](https://www.reddit.com/r/emacs/comments/ajwtcb/emacs_on_the_go/).

Emacs on the [Noodle Pi](https://twitter.com/noodle_pi/status/996427835630747648).


#### Smartwatches

Emacs on a [Pebble watch](https://www.reddit.com/r/emacs/comments/7oqr5s/i_must_have_emacs_on_everything_even_my_smartwatch/) (this one is actually just a "face", not the real deal).


## Notes

[^1]: No, not this [Doom](https://github.com/hlissner/doom-emacs), the [actual game](https://en.wikipedia.org/wiki/Doom_(1993_video_game)).

[^2]: I assume a good percentage of the user base don't use the graphical version, prefering to _live in the terminal_.

[^3]: For more N900 goodness: [Nokia N900 flickr group](https://www.flickr.com/photos/n900user/).

[^4]: I personally use an iPad connecting to my Android phone with [Blink shell](https://blink.sh/).

[^5]: The [wiki has a page dedicated to the Zauraus line](https://www.emacswiki.org/emacs/EmacsForZaurus). See also [this more generic page about PDAs](https://www.emacswiki.org/emacs/Emacs_and_EmacsWikiMode_on_PDAs).
