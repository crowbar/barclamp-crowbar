#Networking#
##Network Setup##
Due to the nature of the different software used, the network is set up as flat as possible using a dedicated BMC port and bonded LOMs. Crowbar manages all networks, and comes out of the box preconfigured to allow the initial configuration to come up quickly by predefining the storage, admin, public, and BMC networks.

The Crowbar network configuration can be customized to better map to site-specific networking needs and conventions. These changes include adding additional VLANs, changing VLAN mappings, and teaming NICs. Please refer to the Network Barclamp section for specific details.

>![notes.png](graphics/notes.png "notes.png") Networks for the environment are configured when the Crowbar installation is performed. They cannot be changed without re-installing Crowbar. For specific information on how to configure the network JSON file, please see the Crowbar Deployment Guide.
 
###Default Networks###
The default networks are presented in the following table. These defaults can be modified prior to installing Crowbar for your specific environment.

######Default Networks######

----------

| Usage	| Description | Default reserved VLAN tag | Tagged |
| :---- | :----------- | :------------------------- | :------ |
| Admin/Internal VLAN | Used for administrative functions such as Crowbar node installation, TFTP booting, DHCP assignments, KVM, system logs, backups, and other monitoring. There is only one VLAN set up for this function and it is spanned across the entire network. | 100 | Not tagged |
| BMC VLAN	| Used for connecting to the BMC of each node. 	| 100	|Not tagged |
|Storage VLAN | Used by the Swift storage system for replication of data between machines, monitoring of data integrity, and other storage-specific functions.	| 200	| 802.1q Tagged |
| Edge/External VLANs |	Used for connections to devices external to the Cloud infrastructure; these include externally visible services, such as load balancers and web servers. Use one or many of these networks, dependent on the need to segregate traffic among groups of servers.	| 300	| 802.1q Tagged |

----------


>![notes.png](graphics/notes.png "notes.png") The admin and BMC networks are expected to be in the same L2 network.

Each network defined in the system has the following parameters:
######Network Parameters######

----------

| Name | Default | Description |
| :--- | :------ | :------------ |
| vlan |	Integer |	The VLAN to use on the switch and interfaces for this network |
| use_vlan |	true |	A value of true indicates that the VLAN should be applied to the interface. A value of false assumes that the node will receive untagged traffic for this network. |
|add_bridge |	false |	Indicates if the network should have a bridge built on top of it. If a bridge is created, it will be named *br0*. |
| subnet |	IP Address |	The subnet for this network |
| netmask | Netmask | The netmask for this network |
| router | IP Address | The default router for this network 
| broadcast | IP Address | The default broadcast address for this network |
| ranges | map | This contains a map of strings to start and stop values for the network. This allows for sub-ranges with the network for specific uses (e.g., dhcp, admin, bmc, hosts}. |

----------


####IP Addressing####

By default, IP addresses are assigned in the following fashion, using large subnets to support many machines on the production network. The table below shows the networks that are installed when the default network configuration is used. In each network, the first 10 IP addresses are reserved for switches, routers, and firewalls.
######Default Network Addresses######

----------

| LAN | VLAN | Network | Subnet	| Gateway | 802.1q | Bridged|
| :-- | :--- | :----- | :------| :-------| :----- | :------|
| Storage | 200 | 192.168.125.0 | 255.255.255.0	| none | Yes | No |
| Public | 300 | 192.168.122.0	| 255.255.255.0	| 192.168.122.1	| Yes |No |
| Admin | 100 | 192.168.124.0 | 255.255.255.0 | 192.168.124.1 | No | No |
| BMC |100 | 192.168.124.0 | 255.255.255.0 | 192.168.124.1	| No | No |
| BMC_Vlan | 100 | 192.168.124.0 | 255.255.255.0 | 192.168.124.1 |No | No |

----------


>![notes.png](graphics/notes.png "notes.png") Each network's ".1" address is reserved for the network gateway.