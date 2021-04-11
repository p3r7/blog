---
layout: post
title: Akai MPC 2000 long file names
description: Reading files on a MPC 2000
summary: On FAT12 without vFAT
tags: [retrocomputing,music]
---


## Introduction

![mpc2k](/assets/img/mpc2k.jpg){:style="max-width: 450px"}

This is an Akai MPC 2000 XL, a 20 years old sampler / midi-based workstation.

Despite its age, its workflow allows doing things at a comparative speed to modern solutions.

For saving samples and projects, it offers several storage options thanks to a variety of ports but the most standard one is a floppy drive accessible on the front-facing side.

For convenience, I opted for a cheap replacement USB floppy emulator drive, made by Gotek.

It allows formatting a USB drive into up to 999 FAT12 partitions, each corresponding to a virtual floppy disk.

Please note that those virtual floppy are seen as regular floppy disks by the MPC, so saving and loading is still slow and limited to 1400 kB / floppy.


## Making backup images of floppies

Under Linux, one can easilly make backups of floppies:

```bash
$ # make a single vFloppy image
$ MY_FLOPPY_ID=4
$ sudo dd if=/dev/sdb of=~/Documents/backup_gotek_$MY_FLOPPY_ID.img skip=$((4*1536*1024)) bs=512c count=$((1440*1024)) iflag=skip_bytes,count_bytes conv=noerror
$ sudo chown me. ~/Documents/backup_gotek_$MY_FLOPPY_ID.img
$
$ # make an image of a whole USB drive
$ sudo dd if=/dev/sdb of=~/Documents/backup_gotek_all.img
$ sudo chown me. ~/Documents/backup_gotek_all.img
$ # extract an image of a single vFloppy from it
$ dd if=~/Documents/backup_gotek_all.img of=~/Documents/backup_gotek_$MY_FLOPPY_ID.img skip=$((4*1536*1024)) bs=512c count=$((1440*1024)) iflag=skip_bytes,count_bytes conv=noerror
$
$ # in any case, we can verify we have a valid image using mdir from mtools
$ mdir -i ~/Documents/backup_gotek_$MY_FLOPPY_ID.img ::
```

> NB: I'm not 100% sure about the value for `count`. Floppies have a size of 1440 kB but we need to jump 1536 to go from one to another. I don't know if this higher value corresponds to rounding due to fixed sized blocks / segments / whatever or if I'm actually truncating data.


## Mounting floppies

Depending on your OS, there are several solutions for mounting virtual floppies from a Gotek-formatted USB drive on your computer.

For Windows, the best solution is [Ipcas GmbH's Floppy Emulator](https://floppy-emulator.software.informer.com/1.3/) that works fairly well but needs to be executed as an admin on recent Windows 10 versions.

There is also an [official Gotek software](http://www.gotekemulator.com/Download.asp) which is said to mostly not work.

Under Linux, we have a wide range of possibilities.

We can directly `mount` a virtual floppy to see its content:

```bash
$ MY_FLOPPY_ID=4
$ sudo mkdir /tmp/floppy-mount
$ sudo mount -o loop,offset=$[$MY_FLOPPY_ID*1536]k,sizelimit=1440k,users,rw,umask=000 /dev/sdb /tmp/floppy-mount
$ cd /tmp/floppy-mount
$ ls -l
total 651
-rwxrwxrwx 1 root root 25494 Jan  1  1980 ALL_SEQ_.all
-rwxrwxrwx 1 root root 25494 Jan  1  1980 ALL_SEQ_.all
-rwxrwxrwx 1 root root 25078 Jan  1  1980 CAEGOMKW.all
[...]
$
$ # when done, unmount
$ cd -
$ sudo umount /tmp/floppy-mount
```

The `mtools` suite is dedicated to manipulating MSDOS filesystems:

```bash
$ MY_FLOPPY_ID=4
$
$ # list files on vFloppy
$ sudo mdir -i /dev/sdb@@$(($MY_FLOPPY_ID*1536*1024)) ::
 Volume in drive : has no label
 Volume Serial Number is 4A86-66E6
Directory for ::/

CLAP11   SND     23466 2016-11-08  21:28
CLAP12   SND     22878 2016-11-08  21:28
CLAP14   SND     19442 2016-11-08  21:28
HAT11    SND     26576 2016-11-08  21:28
HAT12    SND     17734 2016-11-08  21:28
INF-DEMO PGM      4608 2012-07-06  16:36
[...]
       25 files             660 758 bytes
                            792 064 bytes free

```

Then, one can use command `mcopy` to extract the floppy content.

Please note that all the previous commands can also work on `.img` files instead of mountpoints (like our `~/Documents/backup_gotek_$MY_FLOPPY_ID.img` from the previous section examples).

There is also [@dennisMe2's usbfd](https://github.com/dennisMe2/usbfd) that aims to ease backuping those vFloppies:

```bash
$ # backup a single floppy
$ MY_FLOPPY_ID=4
$ sudo ./usbfd -d /dev/sdb -m $MY_FLOPPY_ID
$
$ # backup floppies #1 to #99
$ sudo ./usbfd -d /dev/sdb -m {1..99}
```


## Long file names

When sampling sounds on the MPC, you are asked to give them a name.

Those names are used as filenames when saving them on any storage medium.

The funny thing is that the MPC 2000 allows names or up to 16 characters (excluding extension), as opposed to the traditional 8 characters normally found on older FAT filesystems.

Even though the (virtual) floppies are formatted in FAT12, it has no issues saving and loading files with those long file names.

But when trying to access those files from a computer, they get all truncated to 8 characters.

Worse, if multiple files have the mistake of starting with the same 8 characters, and they cannot be extracted separately.

When using mount, most graphical file explorer will see them a single file.

`ls`, Emacs' [dired](https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired.html) and `mdir` are able to list them:

![mpc2k_gotek_emacs_mount](/assets/img/mpc2k_gotek_emacs_mount.png){:style="max-width: 450px"}

```bash
$ sudo mdir -i /dev/sdb@@$((5*1536*1024)) ::
 Volume in drive : has no label
 Volume Serial Number is 4A86-64FE
Directory for ::/

FLEETWOO SND     79018 1980-00-00   0:00
FLEETWOO SND     70242 1980-00-00   0:00
FLEETWOO SND     74262 1980-00-00   0:00
FLEETWOO SND     71806 1980-00-00   0:00
FLEETWOO SND     83126 1980-00-00   0:00
FLEETWOO SND     52694 1980-00-00   0:00
FLEETWOO SND     74566 1980-00-00   0:00
FLEETWOO SND     79154 1980-00-00   0:00
FLEETWOO SND     73646 1980-00-00   0:00
FLEETWOO SND     63742 1980-00-00   0:00
FLEETWOO SND     76258 1980-00-00   0:00
[...]
       18 files           1 265 996 bytes
                            186 368 bytes free

```

But neither of `cp`, `mcopy` or `usbfd` will allow backuping more than a single file.


## vFAT or not vFAT

After googling a bit, I discovered that something called vFAT allows having file names longer than 8 characters on any FAT partition.

This feature is called LFN (for Long File Names) and uses "dummy" interleaved file records for storing those longer names.

So I just thought that the MPC simply uses vFAT and that I was using the wrong arguments when using `mount` / `mtools`.

In fact, those commands are pretty smart at determining the filesystem type by themselves and setting it manually (using `-t vfat`) didn't help.

So apparently, Akai decided to do something custom instead of vFAT in 1999 even though it was already there since Windows 95.


## Reverse engineering: basic hexedit

The next logical step was to boot up an hex viewer/editor[^1] and to try to understand what's going on.

![mpc2k_gotek_emacs_hex](/assets/img/mpc2k_gotek_emacs_hex.jpg){:style="max-width: 450px"}

Haha! The full file names are legible[^2] in the file allocation table!

But how is that possible that our previous tools weren't able to parse them?

And how to programmatically extract files with those full names?


## Reverse engineering: Kaitai Struct to the rescue

That's then that I discovered [Kaitai Struct](https://kaitai.io/), set of libraries / descriptor format for parsing file(system) formats.

It comes bundled with [support for FAT partitions](https://formats.kaitai.io/vfat/index.html).

It even has [web editor](https://ide.kaitai.io/) that allows you to load local files!

![mpc2k_gotek_kaitai](/assets/img/mpc2k_gotek_kaitai.png){:style="max-width: 450px"}

Turns out the remaining file name after characters #8 is stored in a field called `reserved`.

I don't know if it's `reserved` as in "reserved for future standard evolutions" or "vendor-reserved" but anyhow that's how they did it.


## Making a custom tool

Using the Kaitai python lib, it was trivial to make a tool that reconstructs those custom long file names and extract the corresponding files following the offset in the file allocation table.

Introducing [mpc-2000-floppy-extractor](https://github.com/p3r7/mpc-2000-floppy-extractor)!

I won't go into implementation details but the code is hopefully self-explanatory.

It can both list files and extract them.

It works on `.img` files as well as mountpoints, either vFloppies or actual hardware floppy drives.

```bash
$ # list files:
$ # - real floppy
$ sudo python main.py --src=/dev/fd0
$ # - vFloppy USB mountpoint (single)
$ sudo python main.py --src=/dev/sdb --floppy 1
$ # - vFloppy USB mountpoint (multiple)
$ sudo python main.py --src=/dev/sdb --floppy 1,2,10-20
$ # - floppy img file
$ sudo python main.py --src=~/Documents/floppy_1.img
$
$ # extract floppies:
$ # - real floppy
$ sudo python main.py --src=/dev/fd0 --dest=/tmp/out_mpc_floppy/ -v
$ # - vFloppy USB mountpoint (single)
$ sudo python main.py --src=/dev/sdb --floppy 1 --dest=/tmp/out_mpc_floppy/ -v
$ # - vFloppy USB mountpoint (multiple)
$ sudo python main.py --src=/dev/sdb --floppy 1,2,10-20 --dest=/tmp/out_mpc_floppy/ -v
$ # - floppy img file
$ python main.py --src=~/Documents/floppy_1.img --dest=/tmp/out_mpc_floppy/ -v
```

More examples are available in the project README.


## Notes

[^1]: Screenshot is Emacs with `hexl-mode`.

[^2]: Even though slightly scrambled.
