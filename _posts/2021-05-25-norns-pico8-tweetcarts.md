---
layout: post
title: Running tweetcarts on norns
description: Making an adaptation layer to run PICO-8 tweetcarts on norns
summary: By making a PICO-8 adaptation layer
tags: [music,monome]
---

![norns_p8_ghosts](/assets/gif/norns_p8_ghosts.gif){:style="max-width: var(--img-width-small)"}


## _monome_ _norns_

As stated in a [previous post](/2021/05/10/norns-community), I got myself a _monome_ _norns_.

Just to recap, it's a programmable instrument for which anyone can make small apps (_scripts_) in Lua.

After playing a bit with it, I was struck by the similarities it shared with another platform.


## _PICO-8_

_PICO-8_ is a _fantasy console_, a software platform allowing anyone to make small games (_carts_) in (you guessed it) Lua.

It can be seen as an homage to the homebrew programming scene during the _home computer era_ (late 70s / early 80s), when people used to share small games written in [BASIC](https://en.wikipedia.org/wiki/BASIC)[^1].

So both _PICO-8_ and _norns_ revolves around the idea of a community sharing small apps. The main difference is that one targets video games while the other focuses on music.

They both have rather limited capabilities and APIs, but this is by design: from constraints creativity flourishes.

One particular phenomenon that emerged with _PICO-8_ are [_tweetcarts_](https://twitter.com/hashtag/tweetcart?lang=en), people posting demos of code fitting in a single tweet.

These show that you don't need a super fancy graphical lib to do some impressive stuff, not too dissimilar to the [demoscene](https://en.wikipedia.org/wiki/Demoscene).

To be honest, I follow a few profiscient _tweetcart_ creators[^2] and now a fair amount of my twitter feed is populated by those.


## The graphical libs

_PICO-8_ and _norns_ graphical libs are pretty close to each other.

|             | _PICO-8_                       | _norns_                         |
|-------------|--------------------------------|---------------------------------|
| screen size | 128x128                        | 128x64                          |
| palette     | 16 colors (+16 secret)         | 16 grey gradients               |
| shapes      | point, line, circle, rectangle | point, line, arc, curve, circle |
| 3D support  | no                             | no                              |

It became apparent to me that with a rather minimal adaptation layer, I could run _tweetcarts_ on _norns_.

Hopefully this could help the people from one community be inspired by the work provided by a like-minded other.

People could quickly steal animation ideas from _tweetcart_ codes, remix them to their liking into their interactive music-making _scripts_.


## _p8_

In practice it took a bit more effort than I expected.

One mildly complex aspect was the conversion of _PICO-8_ color palette to _norns_' greyscale values.

It would have been pretty straightforward if the indexes of the color palette could not be readdressed at run time (`pal`, `palt`) with some special range of values to access a secrete palette of colors.

In addition to its own graphical API, _PICO-8_ also provides its own set of core functions that behave differently from the default Lua ones.

I got stuck on trying to replicate its `atan2` function, and eventually stole the implementation from [@benjamin_soule](https://www.lexaloffle.com/bbs/?uid=9308)'s [PAT Shooter](https://www.lexaloffle.com/bbs/?pid=10183) that predated its introduction in the API.

I was also missing the _table_ manipulation functions (`foreach`, `all`, `add` and `del`). That's when I discovered [picolove](https://github.com/picolove/picolove), another adaptation layer project allowing to run PICO-8 games under the [LÖVE](https://love2d.org/) game framework. So I mostly copy-pasted those with minor tweaks to accomodate _norns_' handling of memory.

Another limitation was _norns_' API inability to do the `pget` functionality (get value of drawn pixel).

Thankfully, Sam ([@csboling](https://norns.community/en/authors/csboling)) got inspired by the idea and came up with the `screen.peek` function. _They_ even came as far as to [document the whole process](https://monome.org/docs/norns/extending/), so kudos to _them_.

Finally, I managed to get something working against most _tweetcarts_ I was throwing at it:

[![p3r7/p8 - GitHub](https://gh-card.dev/repos/p3r7/p8.svg){:style="max-width: var(--img-width-gh-card)"}](https://github.com/p3r7/p8)

You can see its [_norns.community_ page](https://norns.community/en/authors/eigen/p8) and discuss about it on its [_lines_ thread](https://llllllll.co/t/p8-pico-8-wrapper-lib/37947).


## Examples

The [example](ghosts.lua) at the top of the article comes from [this tweetcart](https://twitter.com/Alexis_Lessard/status/1322164958008905728) by [@Alexis_Lessard](https://twitter.com/Alexis_Lessard).

The project comes bundled with a few others:

[confetti.lua](confetti.lua) ([original tweetcart](https://twitter.com/user/status/1324156597569048578) by [@von_rostock](https://twitter.com/von_rostock)).

![norns_p8_confetti](/assets/gif/norns_p8_confetti.gif){:style="max-width: var(--img-width-small)"}

[manga_effect.lua](manga_effect.lua) ([original tweetcart](https://twitter.com/user/status/1309354303933616131) by [@kadoyan](https://twitter.com/kadoyan)).

![norns_p8_manga-effect](/assets/gif/norns_p8_manga-effect.gif){:style="max-width: var(--img-width-small)"}

[tree.lua](tree.lua) ([original tweetcart](https://twitter.com/user/status/1319781601425952768) by [@Alexis_Lessard](https://twitter.com/Alexis_Lessard)).

![norns_p8_tree](/assets/gif/norns_p8_tree.gif){:style="max-width: var(--img-width-small)"}

[pumpkin.lua](tree.lua) ([original tweetcart](https://twitter.com/user/status/1322693583623884803) by [@von_rostock](https://twitter.com/von_rostock)).

![norns_p8_pumpkin](/assets/gif/norns_p8_pumpkin.gif){:style="max-width: var(--img-width-small)"}

[cube.lua](tree.lua) ([original code](https://gist.github.com/neauoire/200d97396805dda71154) by [@neauoire of 100 rabbits](https://twitter.com/hundredrabbits)).

![norns_p8_cube](/assets/gif/norns_p8_cube.gif){:style="max-width: var(--img-width-small)"}


## Notes

[^1]: See [this small documentary](https://www.youtube.com/watch?v=n79SYpEVMgM) about a the UK scene.

[^2]: People such as [@2DArray](https://twitter.com/2DArray), [@Alexis_Lessard](https://twitter.com/Alexis_Lessard), [@kadoyan](https://twitter.com/kadoyan), [@TRASEVOL_DOG](https://twitter.com/TRASEVOL_DOG), [@von_rostock](https://twitter.com/von_rostock) and [zep himself](https://twitter.com/lexaloffle)
