---
layout: post
title: Proper packages on a Synology NAS
description: Install Entware on your Synology NAS
summary: Entware and opkg
tags: [ansible,synology]
---


This article is part of a multi-post series about making the most out of an entry-level Synology NAS:
 - [Synology NAS and Ansible](/2020/04/15/ansible-synology)
 - [Proper packages on a Synology NAS](2020/04/29/synology-entware)


## A brief recap

In my [last post](/2020/04/15/ansible-synology), I complained about how limited and out of date Synology DSM packages are.

In this post we will address this issue.


## Entware

[Entware](https://github.com/Entware/Entware) is a proper linux repository of package with flavors for a wide variety of devices.

It package manager is [opkg](https://openwrt.org/docs/guide-user/additional-software/opkg) and as such can be seen as an offshoot of the OpenWRT project


## Installation

The [Entware wiki](https://github.com/Entware/Entware/wiki) gives pretty [straightforward instruction](https://github.com/Entware/Entware/wiki/Install-on-Synology-NAS) for installing manually.

But as we [already have Ansible installed](/2020/04/15/ansible-synology), we can automate this process as much as possible.

{% raw %}
```yaml
- hosts: <my-nas>
  remote_user: ansible
  gather_facts: False

  vars:
    entware_root: /volume1/@Entware
    entware_version: 3.2
    entware_force_install: False
    my_cpu_arch: armv7sf

  tasks:

  - name: entware | test if already installed
    stat:
      path: '{{ entware_root }}/opt/etc/entware_release'
    register: entware_release

  - name: entware | delete current install
    when: entware_force_install
    file:
      path: '{{ entware_root }}/opt'
      state: absent

  - name: entware | create root folder
    file:
      path: '{{ entware_root }}/opt'
      state: directory

  - name: entware | mount to /opt
    mount:
      src: '{{ entware_root }}/opt'
      path: /opt
      opts: bind
      state: mounted
      fstype: none

  - name: entware | run install script
    when: entware_force_install or not entware_release.stat.exists
    command: wget -O - http://bin.entware.net/{{ my_cpu_arch }}-k{{ entware_version }}/installer/generic.sh | /bin/sh
    args:
      creates: '{{ entware_root }}/opt/etc/entware_release'

   - name: entware | add to path
     blockinfile:
       path: /root/.profile
       marker: "# {mark} ANSIBLE MANAGED BLOCK - ENTWARE"
       block: |
         if [ -d "/opt/sbin" ] ; then
             PATH="/opt/sbin:$PATH"
         fi
         if [ -d "/opt/bin" ] ; then
             PATH="/opt/bin:$PATH"
         fi
```
{% endraw %}

Change the values of vars according to your needs.

The last step is to create an _Autostart Task_ on the DSM web interface. For this, just follow the documentation linked above.


## Profit

You can now enjoy a wide variety of recently updated packages:

```
$ ssh syno-admin-user@nas
bash-4.3$ sudo su
Password: ######
ash-4.3# opkg list | grep youtube-dl
youtube-dl - 2020.3.8-1 - youtube-dl is a small command-line program to download videos from YouTube.com and other video sites. It requires the Python3 interpreter.
```
