---
layout: post
title: Cataloging norns scripts
description: A tale of cataloging monome norns scripts
summary: Or how to get to work with awesome people
tags: [music,monome]
---

![norns](https://monome.org/images/norns-front-small.jpg){:style="max-width: var(--img-width-small)"}


## _monome_ _norns_

Pictured above is [_norns_](https://monome.org/docs/norns/), a ~~sound computer~~ ~~music framework~~ many-faced instrument[^1], from the folks at [_monome_](https://monome.org/).

It runs _scripts_, little music-making apps, that anyone can develop and share through it package manager (_maiden_) and/or the community forum ([_lines_](https://llllllll.co/)).


## Cataloging _norns_ _scripts_

Before deciding to acquire my own _norns_, I did some research to see what it was all about.

The official documentation (at the time[^2]) was rather elusive, leaning more towards poetry than a technical reference.

I finally took the plunge but even after playing a bit with it, I still had a hard time pinpointing what I got my hands on.

As a matter of fact, _norns_ can be many things, each _script_ being mostly limited by its creator's imagination.

For my own use, as I usually do with any subject matter[^3], I wrote down some notes as I got things figured out.

As I arrived pretty late in _norns_' history, I was astonished by the shear number of _scripts_ available.

To try them out and easily go back to them, I started cataloging those _scripts_.

It quickly became apparent that this effort could benefit to others, so I converted my notes to markdown (for easier collaboration) and made them public:

[![p3r7/awesome-monome-norns - GitHub](https://gh-card.dev/repos/p3r7/awesome-monome-norns.svg){:style="max-width: var(--img-width-gh-card)"}](https://github.com/p3r7/awesome-monome-norns)

The [reaction was pretty positive](https://llllllll.co/t/index-of-norns-related-resources/37007), and it sparked discussion with long time monome collaborators such as Sam Boling ([post](https://llllllll.co/t/index-of-norns-related-resources/37007/16)) and Dan Derks.


## Early developments of _norns.community_

Still, it appeared that the official docs + the _lines_ forum weren't enough.

My effort got well received but wasn't flexible enough to gain collaborative traction[^4].

As first theorized by Dan in [this post](https://llllllll.co/t/index-of-norns-related-resources/37007/3), there was a need for more.


Overseen by Brian (@tehn), Dan and Tyler Etters started playing seriously with the idea of a wiki so that knowledge between _script_ creators and users could be shared.

Before that, the forum was used for that purpose but it was tedious to find back technical information buried deep between more general conversation.

A wiki looked indeed like the best tool to create a shared memory, a common _documentary heritage_.

The idea was to use _awesome-monome-norns_ as a baseline for the structure but to make it more _script_ author-centered, notably allowing them to have space to document each of their creations.

At that time I got consulted by Dan on my views on how to structure information and that I could, if I wanted, be involved in the project in the future.

I shared with him some views I had gathered from my experience in the industry[^5] but didn't really gave a clear feedback about being more involved.


## _norns.gallery_

I realized that one thing that was clearly missing from previous efforts (official doc, _awesome-monome-norns_) was a gallery view of all available _scripts_.

Indeed, they relied on lists or tables to present them. But all those _scripts_ have a unique visual identity that is often more recognizable than their names.

I also wanted to address a common complaint I had with _awesome-monome-norns_: the table of connectivity options wasn't legible enough with its many columns. A set of dedicated icons would make things both more compact and legible.

I gave myself a few hours over the weekend to make a working prototype, trying to respect as much as possible _monome_'s design language.

[![p3r7/norns-gallery - GitHub](https://gh-card.dev/repos/p3r7/norns-gallery.svg){:style="max-width: var(--img-width-gh-card)"}](https://github.com/p3r7/norns-gallery)

![norns_gallery](/assets/img/norns_gallery.png)

![norns_connectivity_icons](/assets/img/afd_norns_connectivity_icons.png)


I presented it to Dan, who forwarded it to Tyler.

Both of them got hooked by the idea and got back to convince me of integrating it with the wiki.


## _norns.community_

At the time I got recontacted, Tyler and Dan had since made a huge effort on the data architecture side of things.

They asked me for advices on this aspect, but I didn't see anything worth changing, so kudos to them.

Regarding the integration of the _norns.gallery_, on the other hand, things weren't as smooth.

Firstly, one problematic aspect was how to share state (list of _scripts_ and meta-data) between the 2 instances.

We quickly figured out that the wiki had an API that would solve the issue.

Secondly, we failed to clearly scope how the gallery would fit with the features provided with the wiki.

Indeed, the gallery was initially conceived as a search tool and there was an overlap of functionality with the wiki's native [tagging & searching system](https://norns.community/t).

We initially thought that the latter was too limiting for the amount of search criterias we originally wanted[^6] and saw the gallery as a way to implement proper _faceted search_.

![norns_gallery_faceted_search](/assets/img/norns_gallery_faceted_search.png){:style="max-width: var(--img-width-small)"}

It came with an even richer iconography to allow marking a _script_ connectivity options as required:

![norns_gallery_required](/assets/img/norns_gallery_required.png){:style="max-width: var(--img-width-xsmall)"}

This early prototype still lives as an [interactive demo](https://p3r7.github.io/norns-gallery-render-static/).

However, we quickly saw several drawbacks:

 - the wiki's native tag system would be a pretty limiting meta-data structure[^7] for storing many different aspects (_dimensions_)
 - imposing too many constraints on the proper tagging of _script_ may have deterred contributors
 - having 2 search interfaces would be counter-intuitive to end users

So we agreed to simplify and store only 2 aspects of _scripts_ with tags: their category and their connectivity options.

Tyler then oversaw the integration of the gallery's DOM as an embedded iframe.

In this process, he made it more responsive[^8], more compact and did some CSS touchups to make the whole thing appear seamless.

Furthermore, he pushed the idea of tighter coupling, making filtered versions for pages dedicated to authors and specific categories.


## Going beta then live

Dan and Tyler invited several batches of _script_ authors to register and add their creations to the platform.

It was rather satisfying to see their excitement as they proceeded and watch the gallery get automagically populated.

When we [launched](https://llllllll.co/t/norns-community/43793), the reception was very positive.

![norns_community](/assets/img/norns_community.png)


## Going forward

This first release was a success.

At the time of writing, from half to 2/3rd of all existing _scripts_ have been documented. There is still some hidden gems that would benefit from being cataloged.

There is the eventuality of a tighter integration with _norns_ package manager (_maiden_) that may tie a tighter link between _norns_ as a platform and the community aspect of its _script_ developments.

I would also be happy to see the wiki host more general information pages. It could notably steal from _awesome-monome-norns_ a list of engines and reusable Lua libs.

Ultimately I would like to mark _awesome-monome-norns_ as deprecated, superseded by both the official doc and _norns.community_.


## Acknowledgments

It was a pleasure interacting with Dan and Tyler on this project.

Tyler was especially impressive at how quick he was to grasp and patch the gallery's source code, even though it was written in a foreign language to him (ClojureScript).

Dan struck me on one occasion with his intuition, leading us to a great optimization with a few innocent questions even though we were touching on a subject he had of us all the less know-how.

You can read Tyler's _post-mortem_ of the project [on his blog](https://nor.the-rn.info/2021/04/09/building-norns-community/).

You can check out their _scripts_ on their _norns.community_ pages:

 - [Dan's](https://norns.community/en/authors/dan_derks)
 - [Tyler's](https://norns.community/en/authors/northern-information)


## Conclusion

If you have notes you keep to yourself, it might be worth it taking the extra step of sharing them with the world.

In addition to helping others, as a positive side-effect you might end up meeting other passionate people with whom you might sharpen your skills and grow as a person.


## Notes


[^1]: For more details read: [What / Why is norns?](https://github.com/p3r7/awesome-monome-norns#what--why-is-norns)

[^2]: In the meantime, it got quite an overhaul, notably thanks to Dan.

[^3]: I extensively use [org-mode](https://orgmode.org/) to offload as much things as possible out of my brain.

[^4]: GitHub pull requests were not necessary the most user-friendly way to approach collaborative editing.

[^5]: Notably the importance of having the same information delivered in different forms to target contexts and audiences. See also: [The Documentation System](https://documentation.divio.com/)

[^6]: Being able to search by whether hardware controllers are required for _script_ operation and also by support of [specific grid models](https://monome.org/docs/grid/).

[^7]: As opposed to key/value(s) pairs or even a relational data model.

[^8]: Notably by replacing [tailwind](https://tailwindcss.com/) with [Bootstrap](https://getbootstrap.com/) for which he had a working secret sauce.
