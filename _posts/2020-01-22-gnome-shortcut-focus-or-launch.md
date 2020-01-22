---
layout: post
title: Focus or launch under GNOME 3
description: A script to focus on program or launch it
summary: Or any over DE / WM under X server
tags: [linux]
---


## The problem

After a few years using Windows 10 as my main OS, I went back to using Ubuntu[^1].

Under Windows, I'm using [AutoHotkey](https://www.autohotkey.com/) to define a bunch of custom keyboard shortcuts to switch between different program windows.

I usually bind a single key to focus to a program window or launch it if not already running.

For example, for Google Chrome:

```
#n::
IfWinExist ahk_exe chrome.exe
{
	WinGet, id, list, ahk_exe chrome.exe
	Loop, %id%
	{
	    this_id := id%A_Index%
		WinActivate, ahk_id %this_id%
	}
	return
}
else
	Run C:\Program Files\Google\Chrome\Application\chrome.exe
	WinWait ahk_exe chrome.exe
	WinActivate
return
```

Under Linux, I managed to have a similar behavior but at the time I was using [xmonad](https://xmonad.org/).

Now, I'm sticking (for now) with the default DE that is GNOME 3.

Much to my disappointment, the latter does not support out of the box defining shortcuts with this "focus or launch" behavior.


## The solution

I googled around, but didn't find any solution that satisfied my need.

Specifically, I wanted:

 - to consider each virtual desktop independently
 - to dissociate the window class / program name from the launch command[^2]

At first I tried using [AutoKey](https://github.com/autokey/autokey) but found it kinda unstable.

Ultimately I ended up with a small script relying on _wmctrl_ and _xdtotool_.

_wmctrl_ is used to determine the list of windows for current desktop.

_xdotool_ is used to list the windows for a given class.

I used python for the convenience of parsing string and manipulating lists.

You can find it alongside detailed instructions on [the dedicated GitHub project](https://github.com/p3r7/focus-or-open-program).


## Limitations

It works as long as we're running X server.

But we'll all know that we'll ultimately switch to Wayland.

When that happens, we'll need another solution.


## Notes

[^1]: I still dual boot as there are several programs I rely on, such as Capture One...
[^2]: This is necessary for launching stuff with arguments or that would spawn programs with a different name (e.g.emacsclient spawning emacs).
