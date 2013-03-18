#Configuring and Installing the Crowbar Admin Node#
The admin node must be configured first. Installing the admin node involves installing the base operating system, optionally customizing the Crowbar configuration (primarily the networking configuration), and installing Crowbar itself.

Once configured, Crowbar running on the admin node is used to configure and deploy the rest of the solution, and to provide ongoing operations management. The admin node manages all the cluster compute and storage nodes. It assigns the other nodes IP addresses, PXE boots them, configures them, and provides them the necessary software for their roles.

**Note:** The admin node must be the *only* DHCP server visible to the compute and storage nodes.

The initial admin node installation is performed by PXE booting the admin node from a bootstrap node, typically a laptop. To install the Crowbar admin node:

1. Obtain a pre-built Crowbar ISO from [http://crowbar.zehicle.com/](http://crowbar.zehicle.com/ "Crowbar ISO repository").

2. Power on the admin node, and ensure that:

	a. It is set up to boot from the hard disk for subsequent boots.

	b. This first boot (and only this first boot) is a network boot.
3. Power off the admin node.
4. Make sure you have VMware Player installed on the laptop.
5. Make sure you have the Crowbar ISO image loaded on the laptop.
6. Turn off or disable wireless networking on the laptop.
7. Open the VMware machine configuration distributed with Crowbar (this will be a *.vmx* file).
8. Edit the machine settings within Player to ensure that the network adapter is configured to use Bridged Networking, connected to the physical network adapter.
9. Configure VMware Player to mount the Crowbar ISO image as a DVD in the VM.
10. Connect the network crossover cable between *eth0* of the admin node and the network port of the laptop.
11. Power on the VM. It should boot, and present a login prompt in under a minute.
12. Power on the admin node. It should PXE boot, obtaining its image from the VM. The admin node will automatically install its operating system and deployment software.
13. Once the installation is complete, power down the installer VM, and disconnect the laptop.
14. Reconnect *eth0* of the admin node to the appropriate switch port.

	When this process has completed, the operating system and deployment software has been installed on the admin node. The Crowbar software has been copied to the admin node, but final installation has not been completed.
15. Verify the Crowbar admin node installation by logging into Crowbar UI, and ensuring that the admin node's icon is green.

**Note:** This can take some time; please be patient.