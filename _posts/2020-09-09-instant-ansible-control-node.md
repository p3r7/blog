---
layout: post
title: Instant Ansible control node
description: Spin up an Ansible control node in seconds with Vagrant
summary: With Vagrant
tags: [ansible,vagrant]
---

![instant_noodles](/assets/gif/instant_noodles.gif)


## Introduction

So, you wanna learn Ansible.

For that you'd need a [control node](https://docs.ansible.com/ansible/latest/network/getting_started/basic_concepts.html#control-node), i.e. a system with the `ansible` binary installed on.


## Local vs Containers vs VMs

You don't want to install Ansible locally. Even if you think you do, don't[^1].

You could use Docker and its [official image](https://hub.docker.com/r/ansible/ansible). That's what the cool kids use these days.

But for this use-case, a Docker container might not be the most clever choice as quite a few Ansible _modules_ (i.e. _functions_ in Ansible lingo) can rely on python _modules_ (i.e. libraries) and external tools not installed by default on this container.

To have more liberty of installing whatever dependency we want alongside Ansible, a VM might be a wiser choice.[^2]


## The VM pain point

VMs are generally slow to create, as you basically install the OS yourself.

Luckily, once a type of instance as been created once, it can be saved as a template for quicker spawning of new instances. This is pretty analogous to container images.

[Vagrant](https://www.vagrantup.com/) is a wrapper tool around [popular virtualization technologies](https://www.vagrantup.com/docs/providers)[^3] that provide a unified syntax for declaring and spawning VMs from templates.

Those templates are called _vagrant boxes_ and a [hefty amount of community-driven boxes are readily available](https://app.vagrantup.com/boxes/search). You can think of it as Docker Hub for whatever virtualization technology you're using.


## Setting up an Ansible VM

As of writing, there is [no decent Ansible control node Vagrant box available](https://app.vagrantup.com/boxes/search?utf8=%E2%9C%93&sort=downloads&provider=&q=ansible).

But that's not an issue as we could just simply start with a generic box such as those provided officially by [Debian](https://app.vagrantup.com/debian), [Ubuntu](https://app.vagrantup.com/ubuntu) or the [Roboxes](https://roboxes.org/) project.

Vagrant support [running scripts to provision a VM](https://www.vagrantup.com/docs/provisioning) as it gets created. Most people use [shell scripts](https://www.vagrantup.com/docs/provisioning/shell) but more _provisioners_ (i.e. methods) are supported.

Namely, [Ansible](https://www.vagrantup.com/docs/provisioning/ansible) can be used if installed on the host (i.e. alongside Vagrant).

More relevant to our use case, a little-known feature is Vagrant can install Ansible for us on the guest (i.e. the VM) and use it for the VM to provision itself!

This is provided by the [Ansible Local Provisioner](https://www.vagrantup.com/docs/provisioning/ansible_local). We can then use this Ansible instance installed on the guest to tweak it's own configuration just after it gets installed.

That's basically an Ansible control node provisioning itself 🤯.


## The end result

The end result is available on GitHub: [vagrant-ansible-control-node](https://github.com/Eigenbahn/vagrant-ansible-control-node).

It uses VirtualBox as a _provider_ but could be easily adapted to whatever floats your boat.

You'd just have to:

    $ git clone https://github.com/Eigenbahn/vagrant-ansible-control-node
    $ cd vagrant-ansible-control-node
    $ vagrant up
    # [...]
    $ vagrant ssh

And bam! You're in.

Depending on your download speed, you'd end up with your control node in seconds to minutes.

Also, if you're an Emacs user, you might want to check out package [magrant](https://github.com/p3r7/magrant) for a tighter integration with Vagrant cli commands.


## Notes

[^1]: Use whatever strategy (virtualenv/container/VM) to keep your base system as pure as possible. Trust me.

[^2]: Likewise it's ok to install yarn/composer/leiningen/whatever to bundle stuff on your control node in a first time instead of struggling to setup a bundling server or even propper CI/CD.

[^3]: Even Docker is supported as a _provider_ (i.e. virtualization technology), even though you'll struggle to find anybody using vagrant for this use-case.
