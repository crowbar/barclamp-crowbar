## Role Model

### Flags

Roles have several flags that detemrine how Crowbar manages relationships when creating the node-role graph.

#### Implicit

#### Bootstrap

#### Library

#### Discovery

#### Server

Indicates that the attributes used by this node should be made available to its children in the noderole graph.

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

