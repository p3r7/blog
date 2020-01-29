---
layout: post
title: Move files fast in GNOME
description: A nautilus custom script to move files fast
summary: With a nautilus custom script
tags: [linux,productivity]
---


## The problem

I tend do download lots of crap from the internet.

Mainly pictures... Of various stuff... Ranging from furniture, old computer hardware, world fair memorabilias, old japanese toys...

I was basically doing a local Pinterest before it existed.

I also like photography and will construct small catalogs of my fav pictures from people I admire.

Needless to say that my download folder can quickly turn into a real mess.

If not taken care of regularly, sorting files can be a real mess.

So I quickly developped a solution to make this repetitive task less challenging.


## Nautilus scripts

I found out that we could define custom scripts to be run in Nautilus, GNOME's file explorer.

Those custom script can be launched through a right-click menu (meh) or bound to keyboard shortcuts (yay!).

We just have to refer to `$NAUTILUS_SCRIPT_SELECTED_URIS` in our shell script to get the list of selected files.

We now just have to have our script _mv_ this list of files to a chosen location.


## Prompting for user input

I realized that I tend to only move stuff to a lmited number of folder. So I don't want to be prompted everytime to enter a complete path.

What I want, instead, is to refer to one of my favorite folders. So I want a whitelist of them, and I want to be able to select one of them quickly.

To be as effiscient as possible, we associate alias to each of those location.

Enters [_dmenu_](https://tools.suckless.org/dmenu/), which is a graphical tool from [suckless.org](https://suckless.org/) which by default acts as a minimal keyboard-driven graphical program launcher.

But by playing with its option we can use it to as a list picker.


## The solution

The result is the script [_dmenu\_move_](https://github.com/p3r7/dmenu_move) and its accompagnying buddy _dmenu\_go_.

Here is a short demo of the two in action:

![demo](/assets/img/dmenu_move_demo.gif)

Enjoy!
