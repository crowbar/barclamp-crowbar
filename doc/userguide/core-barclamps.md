#Core Crowbar Barclamps#
There are a set of core barclamps that are a part of every Crowbar installation. They are listed in the table below.
######Barclamps######

<table border="0">
<tr>
<th>Barclamp</th>
<th>Description</th>
</tr>
<tr>
<td>Crowbar</td>
<td>The roles and recipes to set up the barclamp framework. References other barclamps. Modify the default proposal to change the Usernames and passwords for access to the Crowbar UI.</td>
</tr>
<tr>
<td>Deployer</td>
<td>Initial classification system for the Crowbar environment (aka the state machine).</td>
</tr>
<tr>
<td>Provisioner</td>
<td>The roles and recipes to set up the provisioning server and a base environment for all nodes.</td>
</tr>
<tr>
<td>Network</td>
<td>Instantiates network interfaces on the Crowbar managed systems. Also manages the address pool.</td>
</tr>
<tr>
<td>Dell RAID</td>
<td>Sets up LSI RAID controllers in a variety of configurations. If missing, the RAID controllers can be set up manually.</td>
</tr>
<tr>
<td>BIOS</td>
<td>Configures BIOS options for Dell™ PowerEdge™ servers. If missing, the BIOS options can be configured manually.</td>
</tr>
<tr>
<td>IPMI</td>
<td>Allows management of the IP Management Interface (IPMI) on servers when the BMC network is enabled.</td>
</tr>
<tr>
<td>NTP</td>
<td>Common NTP service for the cluster (required for secure access). An NTP server can be specified.</td>
</tr>
<tr>
<td>DNS</td>
<td>Manages the DNS subsystem for the cluster.</td>
</tr>
<tr>
<td>Logging</td>
<td>Centralized logging system based on syslog.</td>
</tr>
<tr>
<td>Nagios</td>
<td>System monitoring service for the cluster that can be used by other barclamps.</td>
</tr>
<tr>
<td>Ganglia</td>
<td>Performance monitoring service for the cluster that can be used by other barclamps.</td>
</tr>
<tr>
<td>Test</td>
<td>Provides a shell against which you can write tests.</td>
</tr>
</table>


Details about these barclamps are provided below.
##Crowbar Barclamp##
The Crowbar barclamp provides the roles and recipes to set up the barclamp framework. 

The Crowbar barclamp initializes the system, creates initial proposals of other barclamps defined in its configuration, and creates the users to access the Crowbar API and UI. By default, the system creates Network, Ganglia, Nagios®, NTP, DNS, Provisioner, Deployer, IPMI, RAID, and BIOS proposals based upon the default configuration of their barclamps. The initialization function of the Crowbar barclamp works exactly like other barclamps. A proposal is created and can be committed during installation. 

All barclamps’ transition functions can be called directly, but the Crowbar barclamp calls these in an order specified in its configuration, which is determined by their priority. The default unspecified priority is 100. The special cases are the Provisioner, which is last, and the Deployer and Network, which are first and second, respectively. 
######Crowbar Barclamp Parameters######


<table border="0">
<tr>
<th>Name</th>
<th>Default</th>
<th>Description</th>
</tr>
<tr>
<td>instances</td>
<td>The starting barclamps using their default configurations.</td>
<td>A map of barclamp names that reference a list of JSON files (default is special to mean to take the defaults) that represent barclamp instances to create.</td>
</tr>
<tr>
<td>users</td>
<td>A map of users — containing Crowbar.</td>
<td>This map defines the users allowed to access Crowbar’s UI and REST API.</td>
</tr>
</table>


The users map contains a map. The key is the user name and the rest of the required fields are as follows:  
######User Name Key######


<table border="0">
<tr>
<th>Name</th>
<th>Description</th>
</tr>
<tr>
<td>password</td>
<td>The user's clear text password.</td>
</tr>
<tr>
<td>users</td>
<td>A description of the user.</td>
</tr>
</table>


##Deployer Barclamp##
The Deployer provides an initial classification system for the Crowbar environment. As nodes are discovered, the Deployer makes sure that discovery tools are run on the node by making sure that the Deployer-client role is assigned to the node. The results of that discovery are classified, and the node’s attributes are updated to reflect its potential usage. The Deployer also builds a map of valid and usable disks.

The Deployer gives the primary name to the node at the discovered state. The names default to the letter “d” and the MAC address (with dashes instead of colons). The Deployer also allocates the admin and BMC addresses from the network barclamp.

In addition, the Deployer defines and provides the node’s configuration for RAID and BIOS. These values are assigned part of the hardware-installing state transition. The Deployer uses a list of role name patterns that define what the RAID and BIOS configurations should be. These are applied as values in the node attributes under *crowbar > hardware*.  bios_set can be either *Virtualization* or *Storage*. RAID set can be either *JBODOnly* or *SingleRaid10*.
 
The Deployer is also responsible for manipulating the run list during the hardware-installing and update (or hardware-updating) states. The run list should only include BIOS, RAID, and IPMI operations. 

The Deployer also controls the allocate flag on the node. The allocate flag is used to pause the node after discovery. The node waits for it to be allocated to continue. The Deployer has a configuration option to indicate if the allocate flag should be set to false (and cause a pause) or just allocate all nodes. 
######Deployer Barclamp Parameters######


<table border="0">
<tr>
<th>Name</th>
<th>Default</th>
<th>Description</th>
</tr>
<tr>
<td>bios_map</td>
<td>A list of default settings for bios and raid for swift and nova.</td>
<td>The map defines a list of patterns that would apply a configuration setting for BIOS and RAID.</td>
</tr>
<tr>
<td>use_allocate</td>
<td>true</td>
<td>A Boolean value true indicates that a pause should be injected after the discovered state to allow the admin to accept and allocate the node.</td>
</tr>
</table>


######BIOS Map Entry Keys######


<table border="0">
<tr>
<th>Name</th>
<th>Description</th>
</tr>
<tr>
<td>pattern</td>
<td>Regular expression applied to the role names on the node.</td>
</tr>
<tr>
<td>bios_set</td>
<td>The BIOS set of parameters to apply. Values are: <i>Virtualization</i> or <i>Storage</i>.</td>
</tr>
<tr>
<td>raid_set</td>
<td> RAID set of parameters to apply. Values are: <i>JBODOnly</i> or <i>SingeRaid10</i>.</td>
</tr>
</table>


##Provisioner Barclamp##
The Provisioner provides the roles and recipes to set up the provisioning server and a base environment for all provisioned nodes. The Provisioner also provides the transition entry point for nodes that need to have DHCP transitions done. The Provisioner assumes that IP addressing is handled outside of this barclamp.
######Provisioner Barclamp Parameters######


<table border="0">
<tr>
<th>Name</th>
<th>Default</th>
<th>Description</th>
</tr>
<tr>
<td>default_user</td>
<td>crowbar</td>
<td>User to create for external login.</td>
</tr>
<tr>
<td>default_password</td>
<td>unset</td>
<td>Clear text password to use for external login.</td>
</tr>
<tr>
<td>default_password_hash</td>
<td>Hash of crowbar</td>
<td>MD5 hash of password to use for external login. **</td>
</tr>
<tr>
<td>web_port</td>
<td>8091</td>
<td>The default Web port that the repository web server uses.</td>
</tr>
<tr>
<td>use_local_security</td>
<td>true</td>
<td>This defaults the security updates path in the install to use the admin node instead of the Internet.</td>
</tr>
<tr>
<td>dhcp</td>
<td>map</td>
<td>This is a map that contains the DHCP parameters (lease-time and state_machine).</td>
</tr>
<tr>
<td>lease-time</td>
<td>60</td>
<td>The number of seconds a DHCP lease is valid for the system.</td>
</tr>
<tr>
<td>state_machine</td>
<td>map</td>
<td>This is the state machine that the DHCP server uses in this instance of the barclamp.</td>
</tr>
</table>

** The following command will generate the hash:

	printf 'password' | mkpasswd -s -m md5

>While neither is required, one of either default_password or default_password_hash is required.

##Network Barclamp##
The Network barclamp provides two functions for the system. The first is a common role to instantiate network interfaces on the Crowbar managed systems. The other function is address pool management. 

The network interfaces are controlled by the network role that is applied by the barclamp as a node transition to “installed”. Based upon assigned addresses, the network recipe creates the appropriate single, dual, or team mode interface sets. 

The network assignment function is handled by the creation of an API extension of the base barclamp. The barclamp adds the *allocate_ip* REST API call. This function allocates an IP address from a requested network and updates the node's attributes and the network’s data bag. The available networks (and their parameters) are defined in the configuration for the barclamp. 

Modification of the following parameters should only be done when installing Crowbar, prior to running the *./install systemname.yourdomain.com* command. See the Crowbar OpenStack Deployment Guide for more information.
######Network Configuration Options######


<table border="0">
<tr>
<th>Name</th>
<th>Default</th>
<th>Description</th>
</tr>
<tr>
<td>mode</td>
<td>single</td>
<td>A string value of single, dual, or team. This specifies the default network interface construction model.</td>
</tr>
<tr>
<td>teaming</td>
<td>map</td>
<td>A map of values specific to teaming.</td>
</tr>
<tr>
<td>networks</td>
<td>map</td>
<td>A map of networks that this barclamp should manage.</td>
</tr>
</table>


######Teaming Sub-Parameters######


<table border="0">
<tr>
<th>Name</th>
<th>Default</th>
<th>Description</th>
</tr>
<tr>
<td>mode</td>
<td>6</td>
<td>The default teaming algorithm to use for the bonding driver in Linu.</td>
</tr>
</table>


######Default Networks######


<table border="0">
<tr>
<th>Name</th>
<th>Usage</th>
<th>Notes</th>
</tr>
<tr>
<td>admin</td>
<td>Private network for node-to-node communication</td>
<td>A router, if wanted, is external to the system. This network must be owned by the Crowbar system to run DHCP.</td>
</tr>
<tr>
<td>bmc</td>
<td>Private network for BMC communication</td>
<td>This can be the same as the admin network by using the ranges to limit what IP goes where. A router, if wanted, is external to the system.</td>
</tr>
<tr>
<td>bmc_vlan</td>
<td>Private network for admin nodes on the BMC network</td>
<td>This must be the same as the BMC network and have the same VLAN. This is used to generate a VLAN-tagged interface on the admin nodes that can access the BMC LAN.</td>
</tr>
<tr>
<td>storage</td>
<td>Private network for storage traffic</td>
<td>A router, if wanted, is external to the system.</td>
</tr>
<tr>
<td>public</td>
<td>Public network for Crowbar and other components</td>
<td>A router, if wanted, is external to the system.</td>
</tr>
</table>


######Network Parameters######


<table border="0">
<tr>
<th>Name</th>
<th>Default</th>
<th>Description</th>
</tr>
<tr>
<td>vlan</td>
<td>Integer</td>
<td>The VLAN to use on the switch and interfaces for this network.</td>
</tr>
<tr>
<td>use_vlan</td>
<td>true</td>
<td>A value of true indicates that the VLAN should apply to the interface. A value of false assumes that the node receives untagged traffic for this network.</td>
</tr>
<tr>
<td>add_bridge</td>
<td>false</td>
<td>Indicates if the network should have a bridge built on top of it. The bridge will be <i>br</i>. This is mostly for Nova compute.</td>
</tr>
<tr>
<td>subnet</td>
<td>IP Address</td>
<td>The subnet for this network.</td>
</tr>
<tr>
<td>netmask</td>
<td>Netmask</td>
<td>The netmask for this network.</td>
</tr>
<tr>
<td>router</td>
<td>IP Address</td>
<td>The default router for this network.</td>
</tr>
<tr>
<td>broadcast</td>
<td>IP Address</td>
<td>The default broadcast address for this network.</td>
</tr>
<tr>
<td>ranges</td>
<td>map</td>
<td>This contains a map of strings to start and stop values for the network. This allows allocating sub-ranges with the network for specific uses. For example: DHCP, admin, BMC, hosts.</td>
</tr>
</table>


######Range Map String Key######


<table border="0">
<tr>
<th>Name</th>
<th>Type</th>
<th>Description</th>
</tr>
<tr>
<td>start</td>
<td>IP Address</td>
<td>The first address in the range, inclusive.</td>
</tr>
<tr>
<td>end</td>
<td>IP Address</td>
<td>The last address in the range, inclusive.</td>
</tr>
</table>


>Settings in the Network barclamp should not be changed after the installation of the Admin Node.

##Dell RAID Barclamp##
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


<table border="0">
<tr>
<th>Name</th>
<th>Default</th>
<th>Description</th>
</tr>
<tr>
<td>bmc_enable</td>
<td>true</td>
<td>Controls if the barclamp attempts to work on the BMC.</td>
</tr>
<tr>
<td>bmc_password</td>
<td>cr0wBar!</td>
<td>The password that will be configured on the BMC.</td>
</tr>
<tr>
<td>bmc_user</td>
<td>root</td>
<td>The username that will be configured on the BMC.</td>
</tr>
<tr>
<td>debug</td>
<td>true</td>
<td>Turns on more verbose output.</td>
</tr>
</table>


##NTP Barclamp##
The NTP barclamp provides a common NTP service for the cluster. You can specify an NTP server or servers and all other nodes are clients of them. By default, the time on all nodes are synced to the hardware clock of the Crowbar admin node. The time zone of all Crowbar managed nodes in the cluster is set to UTC.
######NTP Barclamp Parameters######


<table border="0">
<tr>
<th>Name</th>
<th>Default</th>
<th>Description</th>
</tr>
<tr>
<td>external_servers</td>
<td>Empty list</td>
<td>A list of IP addresses or hostnames that should be used as external NTP servers. Hostname can be used if the DNS barclamp is configured to have access to an external resolver.</td>
</tr>
<tr>
<td>admin_ip_eval</td>
<td>Chef::Recipe::Barclamp::Inventory.get_network_by_type(node, \"admin\").address</td>
<td>The Ruby eval expression that returns the admin IP address of a node.</td>
</tr>
</table>


>If you are setting up an external server it can take up to 5 minutes for the nodes to sync with the server. Systems should not be rebooted during this process. If they are rebooted, then they will pause during bootup for time synchronization.

##Logging Barclamp##
The Logging  barclamp provides a centralized logging system based on syslog. The barclamp enables a centralized log server that can then forward information to external syslog servers. The Crowbar installation process sends logs to the admin node by default, but the configuration from the logging barclamp can override this initial configuration.
######Logging Barclamp Configuration Parameters######

<table border="0">
<tr>
<th>Name</th>
<th>Default</th>
<th>Description</th>
</tr>
<tr>
<td>external_servers</td>
<td>Empty list</td>
<td>A list of IP addresses for the logging-server to which to forward logs.</td>
</tr>
</table>

##Nagios® Barclamp##
The Nagios® barclamp provides a common Nagios® service for the cluster. A Nagios® server or servers can be specified and all other nodes are clients of them. The barclamp attempts to direct all traffic over the admin network.
######Nagios® Barclamp Parameters######

<table border="0">
<tr>
<th>Name</th>
<th>Default</th>
<th>Description</th>
</tr>
<tr>
<td>admin_interface_eval</td>
<td>Chef::Recipe::Barclamp::Inventory.get_network_by_type(node, \"admin\").interface</td>
<td>The Ruby eval expression that returns the admin interface of the node.</td>
</tr>
<tr>
<td>admin_ip_eval</td>
<td>Chef::Recipe::Barclamp::Inventory.get_network_by_type(node, \"admin\").interface</td>
<td>The Ruby eval expression that returns the admin IP address of the node.</td>
</tr>
</table>

##Ganglia Barclamp##
The Ganglia barclamp provides a common Ganglia service for the cluster. Ganglia server or servers can be specified and all other nodes are clients of them.
######Ganglia Barclamp Parameters######

<table border="0">
<tr>
<th>Name</th>
<th>Default</th>
<th>Description</th>
</tr>
<tr>
<td>interface_eval</td>
<td>Chef::Recipe::Barclamp::Inventory.get_network_by_type(node, \"admin\").interface</td>
<td>The Ruby evaluation string that gets the interface of the admin interface.</td>
</tr>
</table>

##Test Barclamp##
The Test barclamp provides a shell for writing tests against. It allows for failures to be injected and other barclamps can be validated against it.
######Test Barclamp Parameters######

<table border="0">
<tr>
<th>Name</th>
<th>Description</th>
</tr>
<tr>
<td>barclamps</td>
<td>A list of supported barclamps that are used as the return value for the barclamp list API call.</td>
</tr>
<tr>
<td>instances</td>
<td>A map of barclamp names that reference a list of JSON files (default is special to mean to take the defaults) that represent starting barclamp instances to create.</td>
</tr>
</table>

>This barclamp is currently used only to perform a quick smoke test of Crowbar following Crowbar installation, and as a result should never be deployed.
