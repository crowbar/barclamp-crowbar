# Crowbar Installation Quick Start Guide

This guide is intended to help get you up and running with Crowbar, to deploy either an OpenStack or Hadoop cluster, as quickly as possible. It contains the following instructions:

# Configuring the Network

The network should be set up as flat as possibles. Crowbar manages all networks, and is preconfigured with one networkto allow the initial configuration to come up quickly.  

The Crowbar network configuration can be customized to better map to site-specific networking needs and conventions. These changes include adding additional vLANs, changing vLAN mappings, and teaming NICs. Instructions to assist in understanding these changes are available at [http://github.com/crowbar/crowbar](http://github.com/crowbar/crowbar "Crowbar open source website").

The admin node manages all the cluster compute and storage nodes. It assigns the other nodes IP addresses, PXE boots them, configures them, and provides them the necessary software for their roles. To provide these services, the admin node runs the following (and other) services:

- Crowbar Server— Manages all nodes, supplying configuration of hardware and software.
- DHCP Server— Assigns and manages IPs for the compute and storage nodes.
- NTP Server(Network Time Protocol server) — Makes sure all nodes are keeping the same clock.
- TFTP Server— PXE boots compute and storage nodes with a Linux kernel. The TFTP server services any PXE boot request it receives with its default options.
- DNS Server— Manages the name resolution for the nodes and can be configured to provide external name forwarding.


# Configuring and Installing the Crowbar Admin Node

Installing the admin node involves installing the base operating system, optionally customizing the Crowbar configuration (primarily the networking configuration), and installing Crowbar itself.

Crowbar runs on the admin node and is used to configure and deploy the rest of the solution. Crowbar also provides ongoing operations management, managing the cluster compute and storage nodes. It assigns the other nodes IP addresses, PXE boots them, configures them, and provides them the necessary software for their roles.

**Note:** The admin node must be the *only* DHCP server visible to the compute and storage nodes.

## Machine Installation

Installation of the OS onto the admin node is performed by booting from the iso image or burn the iso image to a DVD and use the DVD to load the OS to a physical system.

1. Obtain a pre-built Crowbar ISO via [Downloads](https://sourceforge.net/projects/crowbar/)
2. Deploy the iso by booting from it using a VM or burn the image to a DVD and use it to bootstrap  the OS onto a physical system.
3. Once booted to a black screen, hit Alt-F2 to see the prompt and login as crowbar/crowbar

## Installation via Vagrant

Instructions to install via Vagrant are located [here](https://github.com/crowbar/crowbar-utils/tree/master/vagrant-crowbar-2)

## Post-installation

When this process has completed, the operating system and deployment software has been installed on the admin node. The Crowbar software has been copied to the admin node, but final installation has not been completed.


# Installing Crowbar

Installation of the OS onto the admin node does not complete the Crowbar installation.  To complete the Crowbar installation follow these steps:

1. Log onto the admin node. The default username is *crowbar*, password is *crowbar*.

2. This step and substeps below are temporary.  Performing these steps changes the execution path to be equivalent to that of the production execution.  This is a necessary workaround to circumvent the developments auto-boot-strapping shortcut that will be go away shortly after this initial cut of the documentation is complete.  

  a. sudo -i 
  b. cd /tftpboot/ubunto_dvd/extra
  c. Open the install-crowbar-native.sh with your favorite editor and comment out the following lines before proceeding.

    i. Lines 63-67 as shown below.  
      if [[ ! -f $DVD_PATH/sha1_passed ]]; then
        (cd $DVD_PATH && sha1sum -c sha1sums &>dev/null) || \
        die "SHA1sums do not match install is corrupt."
        >$DVD_PATH/sha1_passed
      fi

    ii. And lines starting at #227 (# Wait for puma to come to life) to the line # 282 or end of file. 
 
3. Run the install-crowbar shell script as root using the following procedures:
  a. sudo -i
  b. cd /opt/dell/bin
  c. ./install-crowbar <Domain Name> --no-screen
  d. Upon seeing the reboot message;  go forth and reboot the system 

4. Once the system comes back up,  use a web browser to connect to Crowbars main page at URL: HTTP://192.168.124.10:3000 You should be seeing a Crowbar Nodes screen with a "Configure System" button. 

5. Click on the "Configure System" button, which should take to the Initial System Configuration page.  
  a. Click on the "Add network-admin Role" button to add it to the "Roles for Review" list.   
  b. Click on the "Add <Domain Name> Node" button to add it to the  "Crowbar Admin Node" list. If for some reason you leave this page, you can always get back to here by selecting "Bootstrap" from the Utilities Menu drop-down list.   Also, be patient as the responsiveness of the actions may appear to be delayed after clicking the buttons.

  c.Upon hitting the "Add <domain Name> Node"  annealer process will run.  One can watch the annealer processes by going to the Nodes option in the Nodes menu list, clicking on the <Domain Name> link.  Once the <Domain Name> page is shown, click on the "All Node Roles" button and one should see the states change as the annealr process steps through the nodes. 


## Crowbar and Operations Service Access

- SSH Service - *SSH crowbar@192.168.124.10*, credentials are *crowbar*
- Crowbar UI - *http://192.168.124.10:3000*, credentials are *crowbar/crowbar*
- Chef UI - *http://192.168.124.10:4040*, credentials are *admin/password*


# Discovering Network Nodes

To initiate discovery of network nodes:

* Power on the nodes. They will PXE boot from the Crowbar admin node.
* Verify nodes statuses in the Crowbar UI by navigating to *Nodes > Dashboard*, and then ensuring that all nodes' icons are blinking yellow. This indicates that the nodes are ready to be allocated, and utilized in an OpenStack or a Hadoop cluster.

**Note:** This can take some time; please be patient.
