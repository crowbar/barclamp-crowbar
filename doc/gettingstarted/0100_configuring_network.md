#Configuring the Network#
The network should be set up as flat as possibles. Crowbar manages all networks, and is preconfigured with one network to allow the initial configuration to come up quickly.  

The Crowbar network configuration can be customized to better map to site-specific networking needs and conventions. These changes include adding additional vLANs, changing vLAN mappings, and teaming NICs. Instructions to assist in understanding these changes are available at [http://github.com/crowbar/crowbar](http://github.com/crowbar/crowbar "Crowbar open source website").

The admin node manages all the cluster compute and storage nodes. It assigns the other nodes IP addresses, PXE boots them, configures them, and provides them the necessary software for their roles. To provide these services, the admin node runs the following (and other) services:

- **Crowbar Server** — Manages all nodes, supplying configuration of hardware and software.
- **DHCP Server** — Assigns and manages IPs for the compute and storage nodes.
- **NTP Server** (Network Time Protocol server) — Makes sure all nodes are keeping the same clock.
- **TFTP Server** — PXE boots compute and storage nodes with a Linux kernel. The TFTP server services any PXE boot request it receives with its default options.
- **DNS Server** — Manages the name resolution for the nodes and can be configured to provide external name forwarding.
