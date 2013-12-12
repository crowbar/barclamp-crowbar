## Role Model

### Flags

Roles have several flags that detemrine how Crowbar manages relationships when creating the node-role graph.

#### implicit

Any role that depends on a parent role with the implicit flag will
automatically bind a noderole for the parent role to the same node.

#### bootstrap

Indicates that this role will be automatically bound to the first admin node.

You only need to do this to a few roles (by default, crowbar-admin-node)
because the role dependency logic flags will take care of the rest of
the bindings.  


#### discovery

Indicates that this role will be automatically bound to all newly
discovered nodes.

"crowbar-managed-node" is the only one that uses this by default, and
we let the binding logic pull in the rest of the roles that it
requires.  Other barclamps that have roles that need ot be
automatically bound should add this flag.

#### server

Indicates that the attributes used by this node should be made
available to its children in the noderole graph.

By default when a node-role is run, we push/pull data from it.  This
flag tells the the annealer that child noderoles should be able to see
data that roles with this flag push back into Crowbar.  Once we flesh
out attribute support, this flag will go away.

#### cluster

Indicates that all noderoles for this role in a given deployment should be
bound as parents instead of just one.  This ensures that all instances of
a clustered service are up instead of just the first one.

It is needed to create linked set of services like the ceph-mons.
When we add a new monitor then we want all the children of the monitor
to be held until all the other cluster noderoles are updated together.

#### destructive

Indicates that this role is not idempotent, and that after it
transitions to active for the first time it should never be run
again. The only user of this flag is the provisioner-os-install role.

### Hooks / In-line Calls

Roles have several different hooks that are called as part of the
deployment and/or node role lifecycle.  These hooks allow you to
customize how a role behaves in the crowbar framework.  Hooks should
be kept short and fast so that they do not block the API or the UI --
if you want to do something that takes a long time, you should create
a new role.

#### Deployment hooks

There are two hooks for letting roles interact with deployment roles:

* `on_deployment_create` 
* `on_deployment_delete`.  

They are called passing the relavent deployment as a parameter just after a
deployment_role is created or just before it is destroyed.

This function is import to set defaults, cleanup, validate and perform other setups when a new role is added or removed from a deployment.  This can be very helpful to ensure that sane defaults are set and items are cleaned up.

#### Node hooks

There are two hooks for letting roles take actions when nodes are created or deleted:

* `on_node_create` 
* `on_node_delete`

They are _for all roles_ in the system when a new node is added.  The role does not have to be included in a deployment or used in anyway for this hook to be called.  If a node exists and implements this hook then it will get called when a node is created or destroyed.  

> It is expected that the code will scope correctly!

#### Noderole hooks

There are five hooks that get called as part of the state transitions
for node roles:

* `on_proposed`
* `on_todo`
* `on_transition`
* `on_active`
* `on_error`

Each of these hooks is called with the noderole as a parameter just
after the noderole transitions to the state for the hook in question.
