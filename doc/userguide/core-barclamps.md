#Core Crowbar Barclamps#
There are a set of core barclamps that are a part of every Crowbar installation. They are listed in the table below.
######Barclamps######

----------

| Barclamp | Function/Comments |
| :------ | :---------------- |
|Crowbar | The roles and recipes to set up the barclamp framework. References other barclamps. Modify the default proposal to change the Usernames and passwords for access to the Crowbar UI. |
| Deployer | Initial classification system for the Crowbar environment (aka the state machine). |
| Provisioner | The roles and recipes to set up the provisioning server and a base environment for all nodes. |
| Network |	Instantiates network interfaces on the Crowbar managed systems. Also manages the address pool. |
| RAID | Sets up LSI RAID controllers in a variety of configurations. If missing, the RAID controllers can be set up manually. |
| BIOS | Configures BIOS options for Dell™ PowerEdge™ servers. If missing, the BIOS options can be configured manually. |
| IPMI |	Allows management of the IP Management Interface (IPMI) on servers when the BMC network is enabled. |
| NTP | Common NTP service for the cluster (required for secure access). An NTP server can be specified. |
| DNS | Manages the DNS subsystem for the cluster. |
| Logging | Centralized logging system based on syslog. |
| Nagios | System monitoring service for the cluster that can be used by other barclamps. |
| Ganglia | Performance monitoring service for the cluster that can be used by other barclamps. |
| Test | Provides a shell against which you can write tests. |

----------

Details about these barclamps are provided below.
##Crowbar Barclamp##
The Crowbar barclamp provides the roles and recipes to set up the barclamp framework. 

The Crowbar barclamp initializes the system, creates initial proposals of other barclamps defined in its configuration, and creates the users to access the Crowbar API and UI. By default, the system creates Network, Ganglia, Nagios®, NTP, DNS, Provisioner, Deployer, IPMI, RAID, and BIOS proposals based upon the default configuration of their barclamps. The initialization function of the Crowbar barclamp works exactly like other barclamps. A proposal is created and can be committed during installation. 

All barclamps’ transition functions can be called directly, but the Crowbar barclamp calls these in an order specified in its configuration, which is determined by their priority. The default unspecified priority is 100. The special cases are the Provisioner, which is last, and the Deployer and Network, which are first and second, respectively. 
######Crowbar Barclamp Parameters######

----------

| Name | Default | Description |
| :-- | :------ | :---------- |
| instances | The starting barclamps using their default configurations. |A map of barclamp names that reference a list of JSON files (default is special to mean to take the defaults) that represent barclamp instances to create. |
| users | A map of users — containing Crowbar. | This map defines the users allowed to access Crowbar’s UI and REST API. |

----------

The users map contains a map. The key is the user name and the rest of the required fields are as follows:  
######User Name Key######

----------

| Name | Description |
| :-- | :---------- |
| password | Clear text password of the user. |
| description |	A description of the user. |

----------

##Deployer Barclamp##
The Deployer provides an initial classification system for the Crowbar environment. As nodes are discovered, the Deployer makes sure that discovery tools are run on the node by making sure that the Deployer-client role is assigned to the node. The results of that discovery are classified, and the node’s attributes are updated to reflect its potential usage. The Deployer also builds a map of valid and usable disks.

The Deployer gives the primary name to the node at the discovered state. The names default to the letter “d” and the MAC address (with dashes instead of colons). The Deployer also allocates the admin and BMC addresses from the network barclamp.

In addition, the Deployer defines and provides the node’s configuration for RAID and BIOS. These values are assigned part of the hardware-installing state transition. The Deployer uses a list of role name patterns that define what the RAID and BIOS configurations should be. These are applied as values in the node attributes under *crowbar > hardware*.  bios_set can be either *Virtualization* or *Storage*. RAID set can be either *JBODOnly* or *SingleRaid10*.
 
The Deployer is also responsible for manipulating the run list during the hardware-installing and update (or hardware-updating) states. The run list should only include BIOS, RAID, and IPMI operations. 

The Deployer also controls the allocate flag on the node. The allocate flag is used to pause the node after discovery. The node waits for it to be allocated to continue. The Deployer has a configuration option to indicate if the allocate flag should be set to false (and cause a pause) or just allocate all nodes. 
######Deployer Barclamp Parameters######

----------

| Name | Default | Description |
| :-- | :------ | :---------- |
| bios_map | A list of default settings for bios and raid for swift and nova. | The map defines a list of patterns that would apply a configuration setting for BIOS and RAID. |
| use_allocate | true | A Boolean value true indicates that a pause should be injected after the discovered state to allow the admin to accept and allocate the node. |

----------

######BIOS Map Entry Keys######

----------

| Name | Description |
| :-- | :---------- |
| pattern | Regular expression applied to the role names on the node. |
| bios_set | The BIOS set of parameters to apply. Values are: *Virtualization* or *Storage*. |
| raid_set | The RAID set of parameters to apply. Values are: *JBODOnly* or *SingeRaid10*. |

----------

##Provisioner Barclamp##
The Provisioner provides the roles and recipes to set up the provisioning server and a base environment for all provisioned nodes. The Provisioner also provides the transition entry point for nodes that need to have DHCP transitions done. The Provisioner assumes that IP addressing is handled outside of this barclamp.
######Provisioner Barclamp Parameters######

----------

| Name | Default | Description |
| :-- | :------ | :---------- | 
| default_user | crowbar | User to create for external login. |
| default_password | unset | Clear text password to use for external login. |
| default_password_hash | Hash of crowbar | MD5 hash of password to use for external login. ** |
| web_port | 8091 | The default Web port that the repository web server uses. |
| use_local_security | true | This defaults the security updates path in the install to use the admin node instead of the Internet. |
| dhcp | map | This is a map that contains the DHCP parameters (lease-time and state machine). |
| lease-time | 60 |	The number of seconds a DHCP lease is valid for the system. |
| state_machine | map | This is the state machine that the DHCP server uses in this instance of the barclamp. |

----------
** The following command will generate the hash:

	printf 'password' | mkpasswd -s -m md5

>![notes.png](graphics/notes.png "notes.png") While neither is required, one of default_password or default_password_hash is required.
##Network Barclamp##
The Network barclamp provides two functions for the system. The first is a common role to instantiate network interfaces on the Crowbar managed systems. The other function is address pool management. 

The network interfaces are controlled by the network role that is applied by the barclamp as a node transition to “installed”. Based upon assigned addresses, the network recipe creates the appropriate single, dual, or team mode interface sets. 

The network assignment function is handled by the creation of an API extension of the base barclamp. The barclamp adds the *allocate_ip* REST API call. This function allocates an IP address from a requested network and updates the node's attributes and the network’s data bag. The available networks (and their parameters) are defined in the configuration for the barclamp. 

Modification of the following parameters should only be done when installing Crowbar, prior to running the *./install systemname.yourdomain.com* command. See the Crowbar OpenStack Deployment Guide for more information.
######Network Configuration Options######

----------

| Name | Default | Description |
| :-- | :------ | :---------- |
| mode | single | A string value of single, dual, or team. This specifies the default network interface construction model. |
| teaming |	map | A map of values specific to teaming. |
| networks | map | A map of networks that this barclamp should manage. |

----------

######Teaming Sub-Parameters######

----------

| Name | Default | Description |
| :-- | :------ | :---------- |
| mode | 6 | The default teaming algorithm to use for the bonding driver in Linux. |

----------

######Default Networks######

----------

| Name | Usage | Notes |
| :-- | :-----| :--- |
| admin | Private network for node-to-node communication | A router, if wanted, is external to the system. This network must be owned by the Crowbar system to run DHCP. |
| bmc | Private network for BMC communication | This can be the same as the admin network by using the ranges to limit what IP goes where. A router, if wanted, is external to the system. |
|bmc_vlan |	Private network for admin nodes on the BMC network | This must be the same as the BMC network and have the same VLAN. This is used to generate a VLAN-tagged interface on the admin nodes that can access the BMC LAN. |
| storage |	Private network for storage traffic | A router, if wanted, is external to the system. |
| public | Public network for Crowbar and other components | A router, if wanted, is external to the system. |

----------

######Network Parameters######

----------

| Name | Default | Description |
| :-- | :------ | :---------- |
| vlan | Integer | The VLAN to use on the switch and interfaces for this network. |
| use_vlan | true |	A value of true indicates that the VLAN should apply to the interface. A value of false assumes that the node receives untagged traffic for this network. |
| add_bridge | false | Indicates if the network should have a bridge built on top of it. The bridge will be *br*. This is mostly for Nova compute. |
| subnet | IP Address |	The subnet for this network. |
| netmask |	Netmask | The netmask for this network. |
| router | IP Address | The default router for this network. |
| broadcast	| IP Address | The default broadcast address for this network. |
| ranges | map | This contains a map of strings to start and stop values for the network. This allows allocating sub-ranges with the network for specific uses. For example: DHCP, admin, BMC, hosts. |

----------

######Range Map String Key######

----------

| Name | Type | Description |
| :-- | :--- | :---------- |
|start | IP Address | First address in the range, inclusive. |
| end | IP Address | Last address in the range, inclusive. |

----------

>![caution.png](graphics/caution.png "caution") Settings in the Network barclamp should not be changed after the installation of the Admin Node.
##RAID Barclamp##
RAID is an acronym for “Redundant Array of Independent Disks.” This means that a RAID controller makes (or can make) multiple disks look like one big/smart/safe disk. 

Different RAID controllers have different capabilities. In general, a RAID controller can support one or more RAID volumes of various types. Any disks not included in a RAID volume are directly exposed to the operating system. Disks that are directly exposed like this are known as Just a Bunch Of Disks (JBOD).

Crowbar supports specific RAID controllers. See the Dell OpenStack Reference Architecture Guide for more information.

The Crowbar code makes sure that the configuration on the RAID controller matches that specified within the Crowbar configuration. 

The parts that determine the configuration for a node are:

- A set of Chef data bags, which contain the RAID configuration (in data bags/crowbar-data). The defaults are *SingleRaid10* and *JBOD Only*.
- An attribute on the Chef node of the machine that identifies which data bag (described above) should be applied to this node (the attribute is *node[:crowbar][:hardware][:raid_set]*, and it should include the name of a data bag).
- Crowbar (the Deployer barclamp) sets the above property when a node is allocated to a proposal.

When invoked, the recipe uses a Chef Lightweight Resources and Providers (LWRP) to inspect the current configuration on the system and compare it to the desired state. If the two diverge, the code will: 

- Delete any RAID sets that are not required any more 
- Allocate available disks among the desired RAID sets, according to the order attribute. 
- Issue commands to apply the configuration. 
##BIOS Barclamp##
The BIOS barclamp provides the following specific control features for certain Dell servers: 

- Uploading a known BIOS firmware into flash. Setting parameters to a defined set, based on the machine’s role.
- IPMI barclamp.
- LAN parameters (IP address, netmask, gateway) and User credentials.
- The IPMI barclamp has a couple of list parameters.
##IPMI Barclamp##
The IPMI barclamp configures IPMI access on platforms that support it. It selects an IP address and assigns it to the BMC. Prior to doing this, it checks the IP address that is currently assigned to the BMC. If the address falls within the range of addresses that are configured for the BMC network in the network JSON, then it will use that IP address instead of assigning a new one.

The IPMI barclamp allows the user to control the following parameters: 
######IPMI Barclamp Parameters######

----------

| Name | Default | Description |
| :-- | :------ | :---------- |
| bmc_enable | true	| Controls if the barclamp attempts to work on the BMC. |
| bmc_password | cr0wBar! | The password that will be configured on the BMC. |
| bmc_user | root | The username that will be configured on the BMC. |
| debug	| true | Turns on more verbose output. |

----------

##NTP Barclamp##
The NTP barclamp provides a common NTP service for the cluster. You can specify an NTP server or servers and all other nodes are clients of them. By default, the time on all nodes are synced to the hardware clock of the Crowbar admin node. The time zone of all Crowbar managed nodes in the cluster is set to UTC.
######NTP Barclamp Parameters######

----------

| Name | Default | Description |
| :-- | :------ | :---------- |
| external_servers | empty list | A list of IP addresses or hostnames that should be used as external NTP servers. Hostname can be used if the DNS barclamp is configured to have access to an external resolver. |
| admin_ip_eval  | Chef::Recipe::Barclamp::Inventory.get_network_by_type(node, \"admin\").address  | The Ruby eval expression that returns the admin IP address of a node. |

----------

>![notes.png](graphics/notes.png "note") If you are setting up an external server it can take up to 5 minutes for the nodes to sync with the server. Systems should not be rebooted during this process. If they are rebooted, then they will pause during bootup for time synchronization.
##Logging Barclamp##
The Logging  barclamp provides a centralized logging system based on syslog. The barclamp enables a centralized log server that can then forward information to external syslog servers. The Crowbar installation process sends logs to the admin node by default, but the configuration from the logging barclamp can override this initial configuration.
######Logging Barclamp Configuration Parameters######

----------

| Name | Default | Description |
| :-- | :------ | :---------- |
| external_servers | Empty list	| A list of IP addresses for the logging-server to which to forward logs.|

----------

##Nagios® Barclamp##
The Nagios® barclamp provides a common Nagios® service for the cluster. A Nagios® server or servers can be specified and all other nodes are clients of them. The barclamp attempts to direct all traffic over the admin network.
######Nagios® Barclamp Parameters######

----------

| Name | Default | Description |
| :-- | :------ | :---------- |
| admin_interface_eval | Chef::Recipe::Barclamp::Inventory.get_network_by_type(node, \"admin\").interface | The Ruby eval expression that returns the admin interface of the node. |
| admin_ip_eval | Chef::Recipe::Barclamp::Inventory.get_network_by_type(node, \"admin\").address | The Ruby eval expression that returns the admin IP address of the node. |

----------

##Ganglia Barclamp##
The Ganglia barclamp provides a common Ganglia service for the cluster. Ganglia server or servers can be specified and all other nodes are clients of them.
######Ganglia Barclamp Parameters######

----------

| Name | Default | Description |
| :-- | :------ | :---------- |
| interface_eval | Chef::Recipe::Barclamp::Inventory.get_network_by_type(node, \"admin\").interface | The Ruby evaluation string that gets the interface of the admin interface. |

----------

##Test Barclamp##
The Test barclamp provides a shell for writing tests against. It allows for failures to be injected and other barclamps can be validated against it.
######Test Barclamp Parameters######

----------

| Name |Description |
| :-- | :---------- |
| barclamps | A list of supported barclamps that are used as the return value for the barclamp list API call. |
| instances | A map of barclamp names that reference a list of JSON files (default is special to mean to take the defaults) that represent starting barclamp instances to create. |

----------

>![caution.png](graphics/caution.png "caution") This barclamp is currently used only to perform a quick smoke test of Crowbar following Crowbar installation, and as a result should never be deployed.
