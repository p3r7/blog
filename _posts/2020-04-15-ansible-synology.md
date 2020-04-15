---
layout: post
title: Synology NAS and Ansible
description: Use Ansible to provision your Synology NAS
summary: Be free to install more stuff on the little guy.
tags: [ansible,synology]
---


## My Synology NAS

I have an entry-level Synology NAS with an ARM v7 CPU.

This little box is a nice plug-n-play solution for basic NAS usage.

Everything is configurable via an easy to use web GUI and additional _packages_ (or should I say "Apps") can be installed via a dedicated section.

But the selection of those _packages_ is pretty limited.

Community _packages_ can be added but they are not well maintained (old software versions, installation script buggy or with unclear dependencies...).

Sadly, even though Docker is supported on this CPU architecture (thank you Raspberry Pi) the installed kernel version (3.2.40) is too old to support it.

So, if we want to make the most of this little device, we have to lift up the hood, realize that the OS (called DSM) is nothing more than a tweaked Linux.


## Ansible

I really like Ansible.

After years of using custom bash script (plus some forgettable bewilderments with [chef](https://www.chef.io/)) it really changed how I approach provisioning when I discovered it 3 years ago.

To me it's main strengths are that:

 - it's robust
 - it's well documented
 - it's fast to write stuff with
 - it's small in scope and thus relatively fast to master

Don't read me wrong: it's not a _panacea_. Being very declarative and procedural it's not suited for complex orchestration.

So it's natural that I want to use Ansible to install stuff on my little NAS.


## Enabling SSH access

First of all, we need to enable SSH access.

The [official documentation](https://www.synology.com/en-global/knowledgebase/DSM/tutorial/General_Setup/How_to_login_to_DSM_with_root_permission_via_SSH_Telnet) is pretty straightforward.


## Getting Ansible to communicate with the NAS

To give access to a server from my Ansible host, I generally create an `ansible` user of the target server with passwordless sudo access.

And what better way to do this than through an Ansible playbook?

The only trick here is to log in using an "administrator" account with password access.

One small annoyance is that the Linux OS on the NAS doesn't come with commands such as `useradd` or `groups`. As a result the [user](https://docs.ansible.com/ansible/latest/modules/user_module.html) and [group](https://docs.ansible.com/ansible/latest/modules/group_module.html) modules couldn't be used.

Instead, we need to rely on alternative commands `synouser` and `synogroup`.

```yaml
- hosts: <my-nas>
  remote_user: <my-nas-user>
  gather_facts: False

  vars:
    my_remote_user: <my-nas-user>
    my_remote_password: '<my-nas-user-pwd>'
    my_ansible_username: ansible
    my_ansible_password: '<my-secure-ansible-password>'
    my_ansible_public_key_path: /root/.ssh/ansible.pub

  tasks:

  # log in
  - name: try login in as user with my password
    command: sshpass -p "{{ my_remote_password }}" ssh -q -l {{ my_remote_user }} "{{ ansible_host }}" -o PreferredAuthentications=password -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -o ConnectTimeout=1 echo "Worked"
    register: ansible_check_connect_user_pwd
    connection: local
    ignore_errors: yes
    changed_when: False
  - name: if password worked, use it
    connection: local
    set_fact:
      ansible_ssh_pass: "{{ my_remote_password }}"
      ansible_sudo_pass: "{{ my_remote_password }}"
    when: ansible_check_connect_user_pwd is succeeded

  - name: gather facts
    become: true
    setup:

  # retrieve current list of administrators
  - name: get list of administrators from /etc/group file
    become: true
    shell: cat /etc/group | grep administrators | sed 's/.*://'
    register: administrators
  - name: split list of administrators
    set_fact:
      administrators_list: "{{ administrators.stdout.split(',') }}"

  # create user
  - name: read /etc/passwd file
    become: true
    shell: cat /etc/passwd
    register: etc_passwd
  - name: create user ansible
    when: etc_passwd.stdout.find(my_ansible_username) == -1
    become: true
    # NB: args are [username pwd "full name" expired{0|1} mail AppPrivilege]
    # only using AppPrivilege 0x01, i.e. FTP
    command: /usr/syno/sbin/synouser --add {{ my_ansible_username }} "{{ my_ansible_password }}" "" 0 "" 1
    args:
      creates: /volume1/homes/homes/{{ my_ansible_username }}
  - name: fix user ansible home permission
    become: true
    file:
      path: /volume1/homes/{{ my_ansible_username }}
      mode: u=rwx,g=rx,o=rx

  # add user to administrators
  - name: add user ansible to administrators group
    become: true
    command: /usr/syno/sbin/synogroup --member administrators {{ ' '.join(administrators_list) }} {{ my_ansible_username }}

  # give user passwordless sudo access
  - name: give user ansible sudo access
    become: true
    lineinfile:
      dest: /etc/sudoers.d/ansible
      line: "{{ my_ansible_username }} ALL=(ALL) NOPASSWD: ALL"
      regexp: "^{{ my_ansible_username }}"
      state: present
      create: yes
      mode: 0440
      # NB: no visudo on this OS...
      # validate: '/usr/sbin/visudo -cf %s'

  # deploy cert
  - name: deploy ansible master certificate
    become: true
    authorized_key:
      state: present
      user: "{{ my_ansible_username }}"
      key: "{{ lookup('file', my_ansible_public_key_path) }}"
      manage_dir: yes
```

Then reboot your NAS for some changes to take effect (adding the _ansible_ user to the _administrators_ group).

You should then be able to ansible-ping it:

    $ ansible nas -m ping -u ansible
    nas | SUCCESS => {
        "ansible_facts": {
            "discovered_interpreter_python": "/usr/bin/python"
        },
        "changed": false,
        "ping": "pong"
    }

Now we're free to use Ansible like we would with any other device.
