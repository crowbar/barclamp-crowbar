## Node Model

### Boot Environment ("bootenv")

The node's Boot Environment determines which operating system will be installed on the node after configuration and inventory by Sledgehammer.

There are several built in bootenv values 

* local - used by the bootstrapping process when installing the admin node to prevent reimaging the admin server(s)
* noop - used by operators who want to tell crowbar to skip installing the OS.  Generally used because an operating system is already installed on the system (e.g.: a VM)
* [system defined] - used by the Provisioner to select an operating system image from the local catalog

### Hint

Hints are settable on a Node to make user preferences known to Roles during operation.  For example, a user may wish a node to have a specific IP address.  This advice is communicated to the network role(s) by adding a key-value pair to the node's hint json data set.

The pattern for hints is a hash where hints are collected by the roles that's expected to consume them.  For example a hint for the Admin Network (role network-admin) would be stored under the =network-admin= key and then specific key value pairs should be included for that specific role's logic.
#### Common Hints

<table>
  <tr><td>Field</td><td>Schema</td></td>Comments</td></td>
  <tr> 
    <td>Admin IP address</td>
    <td>"network_admin": {"ip_v4address": [a.d.d.r] }</td>
    <td>Must comply with network subnet requirements</td>
  </td>
  <tr> 
    <td>Admin MAC address</td>
    <td>"provisioner-dhcp-database": {"mac": [M:A:C:A:D:D:R:S] }</td>
    <td>Requires also setting an IP address</td>
  </td>
</table>


#### Hint Shortcut

Some hints are so common that there are parameter short-cuts during node creation.  This makes it easier to set these special values.

* =ip= maps to =network_admin \ ip_v4address=
* =mac= sets up DHCP database entry so node name and ip is user controlled during initial discovery

### Aliveness and availability:

Nodes in the Crowbar framework have two related flags that control
whether the annealer can operate on them.

Aliveness is under the control of the Crowbar framework and
encapsulates the framework's idea of whether any given node is
manageable or not.  If a node is pingable and can be SSH'ed into as
root without a password using the credentials of the root user on
the admin node, then the node is alive, otherwise it is dead.
Aliveness is tested everytime a jig tries to do something on a node
-- if a node cannot be pinged and SSH'ed into from at least one of
its addresses on the admin network, it will be marked as
dead.  When a node is marked as dead, all of the noderoles on that
node will be set to either blocked or todo (depending on the state of
their parent noderoles), and those changes will ripple down the
noderole dependency graph to any child noderoles.

Nodes will also mark themselves as alive and dead in the course of
their startup and shutdown routines.

Availability is under the control of the Crowbar cluster
administrators, and should be used by them to tell Crowbar that it
should stop managing noderoles on the node.  When a node is not
available, the annealer will not try to perform any jig runs on a
node, but it will leave the state of the noderoles alone.

A node must be both alive and available for the annealer to perform
operations on it.
