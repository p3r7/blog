---
layout: post
title: Play AI Dungeon like it's 1980
description: Playing AI Dungeon on a hardware terminal
summary: On an actual hardware terminal
tags: [vt320,retrocomputing]
---


## Introduction

So I got myself a hardware terminal a year hand a half from now.

A [DEC VT320](https://terminals-wiki.org/wiki/index.php/DEC_VT320), more precisely (more on that on a later post).

I've been fooling with it for a while, using it like you would use any terminal emulator (only slower and with no UTF8), accessing BBSes, reading newsgroup...

... and playing text adventure games (such as Zork[^1]).


## AI Dungeon

And then came AI Dungeon.

You've played it. I've played it[^2]. It seems everybody has played it.

And I quickly realized that this curiosity of a game was not deemed to be played in a [Jupyter notebook](https://colab.research.google.com/github/nickwalton/AIDungeon/blob/master/AIDungeon_2.ipynb) nor in a [web app simulating a terminal](https://play.aidungeon.io/).

No.

It needs to be played through a hardware terminal like in the good ol' days.


## Solution #1

The game is [open source](https://github.com/AIDungeon/AIDungeon).

So one way would be to just clone the project and run it locally.

Except...

>To play the game locally, it is recommended that you have an nVidia GPU with 12 GB or more of memory, and CUDA installed. If you do not have such a GPU, each turn can take a couple of minutes or more for the game to compose its response.

Crap, I don't even have a graphics card.


## Solution #2

Well, the web app version ([play.aidungeon.io](https://play.aidungeon.io/)) is a web app, so it relies on a backend to do the heavy lifting.

So if the API is well designed, it would be super easy to just make a CLI client to it.

And tanksfully that was the case and so that's exactly what I did.

Ta-dam!

![emacs could be anything](/assets/img/ai-dungeon-vt320.jpg)

Find the code at: [https://github.com/Eigenbahn/ai-dungeon-cli](https://github.com/Eigenbahn/ai-dungeon-cli).


## Closing thoughts

If you do not have access to a hardware terminal, you could still experience the thing with terminal emulators that emulate the rendering of CRT displays: [cool-retro-term](https://github.com/Swordfish90/cool-retro-term) or [Cathode.app](https://apps.apple.com/us/app/cathode/id499233976?mt=12)[^3].

Also, as you might have heard, hosting AI Dungeon costs a lot of money.

This CLI client relies on the same infrastructure as the online version ([play.aidungeon.io](https://play.aidungeon.io/)).

So don't hesitate to [help support the hosting fees](https://aidungeon.io/) to keep the game up and running.


## Notes

[^1]: I highly recommend [Trinity](https://ifdb.tads.org/viewgame?id=j18kjz80hxjtyayw) from the same publisher ([Infocom](https://en.wikipedia.org/wiki/Infocom)).
[^2]: Both "classic" and "2".
[^3]: Mac OS-only.
