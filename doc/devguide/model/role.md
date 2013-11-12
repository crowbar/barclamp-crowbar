## Role Model

### Flags

Roles have several flags that detemrine how Crowbar manages relationships when creating the node-role graph.

#### Library

Indicates that this role can be automatically added to any node that
requires it as a dependency.  Otherwise, roles must be bound to nodes
in dependency order.

#### Implicit

Indicates that this role will be automatically bound to the first admin node.

#### Bootstrap

Indicates that this role will be automatically bound to all newly discovered nodes.

#### discovery

Indicates that userdata, system data, and wall data for this node will be visible to child nodes.  As the name of the flag indicates,
the only roles that will generally require that are ones that implement servers that other roles need to talk to.

#### Server

Indicates that the attributes used by this node should be made available to its children in the noderole graph.

Indicates that all noderoles for this role in a given deployment should be
bound as parents instead of just one.  This ensures that all instances of
a clsutered service are up instead of just the first one.

#### Cluster

Indicates that this role implements a clustered service.

When the noderole graph is built, any child noderoles of this service will be bound to all of the noderoled for this role in the deployment.  The cluster flag and the implicit flag are mutually exclusive.


### Hooks 

To integrate Roles, the following hooks can be overriden.

#### On Deployment Create (optional)

Called when this role is added to a deployment (the [[deployment_role]] is created).  

#### On Deployment Delete (optional)

Called when this role is removed from a deployment (the [[deployment_role]] is deleted).  

> At present, you cannot remove roles from a deployment.

