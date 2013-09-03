## Build Environment Setup

### Development Platform Choices

Crowbar development is typically done under Linux in a virtual machine
environment. Virtual machines are not required, but they simplify development
since Crowbar testing usually involves deploying onto virtual machines.


There are several options for development platforms and virtualization environments. 
The hypervisors commonly in use are VirtualBox and KVM, while Ubuntu, Fedora, and SUSE are all
in use for development hosts.

 
|                    | OpenSUSE / SLES     | Ubuntu              | Fedora              |
| ------------------ | ------------------  |------------------   |------------------   | 
| VirtualBox         | Vagrant Friendly    | Vagrant Friendly    | Vagrant Firendly    |
| KVM                | Automation Friendly | Automation Friendly | Automation Friendly |


If you prefer other hypervisors, check out the corresponding [VirtualBox]
(https://github.com/crowbar/crowbar/wiki/Running-Crowbar-in-VirtualBox-VMs) and
[VMWare]
(https://github.com/crowbar/crowbar/wiki/Running-Crowbar-in-VMWare-VMs) docs.

### Development Environmant

Setting up a full Crowbar development environment is complex due to its many
dependencies - we are simplifying and automating this process as much as
possible. This document provides detailed instructions on how to setup a
_minimal Crowbar development instance_: access to the web interface and the
ability to run the unit, RSpec, and BDD tests.

We assume you are setting up the Crowbar development environment in a qemu-kvm
virtual machine (VM). This is is not a hard requirement - just adapt the steps and
commands accordingly depending on which environment you are using.


If you are using Fedora 18, [these scripts]
(https://github.com/cwolferh/crowbar-virt-for-f18) may save you a bit of time
setting up a qemu-kvm/virsh environment for Crowbar.

### Setting up the qemu-kvm host

#### Installing KVM

First you need to install KVM. On SUSE based systems, run:

    sudo zypper in kvm

#### Enabling CPU virtualization acceleration

[Intel VT-x]
(http://en.wikipedia.org/wiki/X86_virtualization#Intel_virtualization_.28VT-x.29)
or [AMD-V]
(http://en.wikipedia.org/wiki/X86_virtualization#AMD_virtualization_.28AMD-V.29)
capable CPUs are required for hardware virtualization. This is usually disabled
by default in the BIOS, so you may need to enable it manually.

Run the [qemu-kvm/setup-kvm]
(https://github.com/crowbar/crowbar/blob/master/dev-setup/qemu-kvm/setup-kvm)
script to set it up. It checks for CPU support and loads the appropriate kernel
modules.

### Setting up the virtual machine

After the virtualization environment is configured, refer to the following distro specific docs
for directions on setting up the remainder of the development environment: [openSUSE / SLES](300_dev-vm-SUSE.md),
[Ubuntu](400_dev-vm-Ubuntu.md), and [Fedora](500_dev-vm-Fedora.md).

