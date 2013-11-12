## Run / Delayed Job

The Crowbar framework runs all jig actions in the background using
delayed_jobs + a thin queuing layer that ensures that only one task is
running on a node at any given time.  For now, we limit ourselves to
having up to 10 tasks running in the background at any given time,
which should be enough for the immediate future until we come up with
proper tuning guidelines or auto-tuning code for significantly larger
clusters.

Postgresql 9.3:

Migrating to delayed_jobs for all our background processing made it
immediatly obvious that sqlite is not at all suited to handling real
concurrency once we started doing multiple jig runs on different nodes
at a time. Postgresql is more than capable of handling our forseeable
concurrency and HA use cases, and gives us lots of scope for future
optimizations and scalability.

DHCP and DNS:

The roles for DHCP and DNS have been refactored to have seperate
database roles, which are resposible for keeping their respective
server roles up to date.  Theys use the on_node_* roles mentioned in
"Roles, nodes, noderoles, lifeycles, and events, oh my!" along with a
new on_node_change event hook create and destroy DNS and DHCP database
entries, and (in the case of DHCP) to control what enviroment a node
will PXE/UEFI boot into.  This gives us back the abiliy to boot into
something besides Sledgehammer.

Deployment tree:

Until now, the only deployment that Crowbar 2.0 knew about was the
system deployment.  The system deployment, however, cannot be placed
into proposed and therefore cannot be used for anything other than
initial bootstrap and discovery.  To do anything besides
bootstrap the admin node and discover other nodes, we need to create
another deployment to host the additional noderoles needed to allow
other workloads to exist on the cluster.  Right now, you can only
create deployments as shildren of the system deployment, limiting the
deployment tree to being 2 layers deep.

Provisioner Installing Ubuntu 12.04:

Now, we get to the first of tqo big things that were added in the last
week -- the provisioner being able to install Ubuntu 12.04 and bring
the resulting node under management by the rest of the CB 2.0
framework.  This bulds on top of the deployment tree and DHCP/DNS
database role work.  To install Ubuntu 12.04 on a node from the web UI:

1: Create a new deployment, and add the provisioner-os-install role to
that deployment.  In the future you will be able to edit the
deployment role information to change what the default OS for a
deployment should be.
2: Drag one of the non-admin nodes onto the provisioner-os-install
role.  This will create a proposed noderole binding the
provisioner-os-install role to that node, and in the future you would
be able to change what OS would be installed on that node by editing
that noderole before committing the deployment.
3: Commit the snapshot.  This will cause several things to happen:
  * The freshly-bound noderoles will transition to TODO, which will
    trigger an annealer pass on the noderoles.
  * The annealer will grab all the provisioner-os-install roles that
    are in TODO, set them in TRANSITION, and hand them off to
    delayed_jobs via the queuing system.
  * The delayed_jobs handlers will use the script jig to schedule a
    reboot of the nodes for 60 seconds in the future and then return,
    which will transition the noderole to ACTIVE.
  * In the crowbar framework, the provisioner-os-install role has an
    on_active hook which will change the boot environment of the node
    passed to it via the noderole to the appropriate os install state
    for the OS we want to install, and mark the node as not alive so
    that the annealer will ignore the node while it is being
    installed.
  * The provisioner-dhcp-database role has an on_node_change handler
    that watches for changes in the boot environment of a node.  It
    will see the bootenv change, update the provisioner-dhcp-database
    noderoles with the new bootenv for the node, and then enqueue a
    run of all of the provisioner-dhcp-database roles.
  * delayed_jobs will see the enqueued runs, and run them in the order
    they were submitted.  All the runs sholuld happen before the 60
    seconds has elapsed.
  * When the nodes finally reboot, the DHCP databases should have been
    updated and the nodes will boot into the Uubntu OS installer,
    install, and then set their bootenv to local, which will tell the
    provisioner (via the provisioner-dhcp-database on_node_change
    hook) to not PXE boot the node anymore.
  * When the nodes reboot off their freshly-installed hard drive, they
    will mark themselves as alive, and the annealer will rerun all of
    the usual discovery roles.
The semi-astute observer will have noticed some obvious bugs and race
conditions in the above sequence of steps.  These have been left in
place in the interest of expediency and as learning oppourtunities for
others who need to get familiar with the Crowbar codebase.
