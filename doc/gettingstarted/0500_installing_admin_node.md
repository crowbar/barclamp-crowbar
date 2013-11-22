#Configuring and Installing the Crowbar Admin Node#
Installing the admin node involves installing the base operating system, optionally customizing the Crowbar configuration (primarily the networking configuration), and installing Crowbar itself.

Crowbar runs on the admin node, and is used to configure and deploy the rest of the solution. Crowbar also provides ongoing operations management, managing the cluster compute and storage nodes. It assigns the other nodes IP addresses, PXE boots them, configures them, and provides them the necessary software for their roles.

>**Note:** The admin node must be the *only* DHCP server visible to the compute and storage nodes.

Installation of the OS onto the admin node is performed by booting from the iso image or burn the iso image to a DVD and use the DVD to load the OS to a physical system.

1. Obtain a pre-built Crowbar ISO via [Downloads](https://sourceforge.net/projects/crowbar/).
2. Deploy the ISO by either:
  * Booting from it using a VM, or
  * Burning the image to a DVD, and then using it to bootstrap the OS onto a physical system
3. Once booted to a black screen, press [**Alt-F2**] to see the prompt.
4. Login as *crowbar*/*crowbar*.

When this process has completed, the operating system and deployment software has been installed on the admin node. The Crowbar software has been copied to the admin node, but final installation has not been completed.

