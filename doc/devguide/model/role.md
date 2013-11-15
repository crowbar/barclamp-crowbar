## Role Model

### Flags

Roles have several flags that detemrine how Crowbar manages relationships when creating the node-role graph.

#### Implicit

Any role that depends on this role must have the parent bound to the same node.
So, if a parent role requires a role then the child must be attached to that node.

Generally, these roles can be automatically added to any node that

#### Bootstrap

When we have a set of roles that we want to be bound to the admin node, this happens for these roles.

You only need to do this to a few roles (ONE, crowbar-admin-node) because the role dependency logic flags will take care of the rest of the binding.

Indicates that this role will be automatically bound to the first admin node.

#### Discovery

"Crowbar-managed-node" is the only one that uses this.  All the other things that need to be done when a node is discovered are triggered as dependencies on this role.

Indicates that this role will be automatically bound to all newly discovered nodes.

#### Server

By default when a node-role is run, we push/pull data from it.  This flag tells the child roles that data from this role should be shared with them.

Indicates that userdata, system data, and wall data for this node will be visible to child nodes.  As the name of the flag indicates,
the only roles that will generally require that are ones that implement servers that other roles need to talk to.

Indicates that the attributes used by this node should be made available to its children in the noderole graph.

#### Cluster

Needed to create linked set of services like the ceph-mons.  When we add a new monitor then we want all the children of the monitor to be held until all the other cluster node-roles are updated together.  Basically, all of the children are bound to all of the cluster siblings automatically.

Indicates that all noderoles for this role in a given deployment should be
bound as parents instead of just one.  This ensures that all instances of
a clsutered service are up instead of just the first one.

Indicates that this role implements a clustered service.

When the noderole graph is built, any child noderoles of this service will be bound to all of the noderoled for this role in the deployment.


### Hooks / In-line Calls 

To integrate Roles, the following hooks can be overriden.

#### On Deployment Create (optional)

Called when this role is added to a deployment (the [[deployment_role]] is created).  

#### On Deployment Delete (optional)

Called when this role is removed from a deployment (the [[deployment_role]] is deleted).  

> At present, you cannot remove roles from a deployment.

