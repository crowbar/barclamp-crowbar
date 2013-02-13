#Configuring the Network#
The network should be set up as flat as possible, using a dedicated BMC port and bonded LOMs. Crowbar manages all networks, and is preconfigured to allow the initial configuration to come up quickly by predefining the storage, admin, public, and BMC networks.

The Crowbar network configuration can be customized to better map to site-specific networking needs and conventions. These changes include adding additional vLANs, changing vLAN mappings, and teaming NICs. Instructions for making these changes are available at [http://github.com/crowbar/crowbar](http://github.com/crowbar/crowbar "Crowbar open source website").

The Crowbar-deployed OpenStack solution needs at least four (4) vLANs to support the cluster. It is highly recommended that a firewall and other security devices be placed in line.

All servers in an OpenStack Cluster are tied together using TCP/IP networks. These networks form a data interconnect; across which individual servers pass data back and forth, return query results, and load/unload data. These networks are also used for management.

The admin node manages all the cluster compute and storage nodes. It assigns the other nodes IP addresses, PXE boots them, configures them, and provides them the necessary software for their roles. To provide these services, the admin node runs the following (and other) services:

- **Crowbar Server** — Manages all nodes, supplying configuration of hardware and software.
- **DHCP Server** — Assigns and manages IPs for the compute and storage nodes.
- **NTP Server** (Network Time Protocol server) — Makes sure all nodes are keeping the same clock.
- **TFTP Server** — PXE boots compute and storage nodes with a Linux kernel. The TFTP server services any PXE boot request it receives with its default options.
- **DNS Server** — Manages the name resolution for the nodes and can be configured to provide external name forwarding.