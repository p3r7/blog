---
layout: post
title: Instant Ansible control node
description: Spawn an Ansible control node in seconds with Vagrant
summary: With Vagrant
tags: [ansible]
---

![instant_noodles](/assets/gif/instant_noodles.gif)


## Introduction

So, you wanna learn Ansible.

For that you'd need a [control node](https://docs.ansible.com/ansible/latest/network/getting_started/basic_concepts.html#control-node), i.e. a system with the `ansible` binary installed on.


## Local vs Containers vs VMs

You don't want to install Ansible locally. Even if you think you do, don't.

You could use Docker and it's [https://hub.docker.com/r/ansible/ansible](official image). That's what the cool kids use these days.

But for this use-case, a Docker container might not be the most clever choice as quite a few Ansible _modules_ (i.e. _functions_ in Ansible lingo) can rely on python _modules_ (i.e. libraries) and external tools not installed by default on this container.

To have more liberty of installing whatever dependency we want alongside Ansible, a VM might be a wiser choice.


## The VM pain point

VMs are slow to create, as you basically install the OS yourself.

Luckily, once a type of instance as been created once, it can be saved as a template for quicker spawning new instances. This is pretty analogous to container images.

[Vagrant](https://www.vagrantup.com/) is a wrapper tool around [popular virtualization technologies](https://www.vagrantup.com/docs/providers)[^1] that provide a unified syntax for declaring and spawning VMs from templates.

Those templates are called _vagrant boxes_ and a [community-driven list of boxes are readily available](https://app.vagrantup.com/boxes/search). You can think of it as Docker Hub for whatever virtualization technology you're using.


## Setting up an Ansible VM

As of writting, there is [no recent Ansible control node Vagrant box available](https://app.vagrantup.com/boxes/search?utf8=%E2%9C%93&sort=downloads&provider=&q=ansible).

But that's not an issue as we could just simply start with a generic box such as those provided officially by [Debian](https://app.vagrantup.com/debian), [Ubuntu](https://app.vagrantup.com/ubuntu) or the [Roboxes](https://roboxes.org/) project.

Vagrant support [running scripts to provision a VM](https://www.vagrantup.com/docs/provisioning) as it gets created. Most people use [shell scripts](https://www.vagrantup.com/docs/provisioning/shell) but more _provisioners_ (i.e. methods) are supported.

Namely, [Ansible](https://www.vagrantup.com/docs/provisioning/ansible) can be used if installed on the host.

More relevant to our use case, a little-known feature is Vagrant can install Ansible for us on the guest (i.e. the VM) and use it for the VM to provision itself!

This is provided by the [Ansible Local Provisioner](https://www.vagrantup.com/docs/provisioning/ansible_local). We can then use this ansible instance installed on the guest to tweak it's own configuration just after it gets installed.

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

Depending on your download speed, you'd end up with your control node in seconds to minutes.


## Notes

[^1]: Even Docker is supported as a _provider_ (i.e. virtualization technology), even though you'll strugle to find anybody using vagrant for this use-case.
