### Snapshot API

Snapshots are really a time captured Deployment and effectly refect the a Deployment state.

Snapshots have 3 primary states:

PROPOSED: All snapshots on non-system deployments start in the PROPOSED state. When in this state, the annealer will ignore any noderoles in the snapshot, no hooks will be called for any state transitions, and user data for the node roles can be edited.
COMMITTED: When a snapshot is in committed, userdata in node roles cannot be edited, the annealer will transition any nodes in TODO, and hooks will be called for any state transitions.
ARCHIVED: Snapshots in archived state will be ignored by the annealer and by just about everything. Eventually you will be able to look through them to satisfy your curiosity about the past states of the cluster.
There are two other states that are substates of the COMMITTED state
that are primarily for consumption by the UI:

ACTIVE, which is when all of the noderoles in a COMMITTED snapshot are in the ACTIVE state, and
ERROR, when any noderole in a COMMITTED snapshot is in the ERROR state.
State transitions for snapshots:

(previous snapshot) -> PROPOSED: The previous snapshot is set to ARCHIVED. The new snapshot gets a copy of all the noderoles in the previous snapshot, which will have the same data and the same states as the old ones.
PROPOSED -> COMMITTED: All PROPOSED noderoles in the snapshot are transitioned to TODO or BLOCKED, depending on whether or not they have any non-active parents. The only way to go from proposed to committed is via the commit method for the snapshot.
COMMITTED -> PROPOSED: The only way to go from COMMITTED to PROPOSED is via the snapshot recall method.
(COMMITTED || PROPOSED) -> ARCHIVED: A snapshot gets transitioned to ARCHIVED when it is superceded by a new snapshot. Archived snapshots are ignored.
The system deployment is super-special in that its snapshots can not
be transitioned to PROPOSED.

Instances can only be created by

1. Cloning the template instance using barclamp.create_proposal (see Barclamp/Config)
2. Editing an existing configuration which clones from the active instance

#### API Actions

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Comments </th></tr>
<tr><td> GET  </td>
  <td> api/v2/snapshots </td>
  <td> List </td></tr>
<tr><td> GET  </td>
  <td> api/v2/snapshots/:id </td>
  <td> Specific Item </td></tr>
<tr><td> PUT  </td>
  <td> api/v2/snapshots/:id </td>
  <td> Update Item </td></tr>
<tr><td> POST  </td>
  <td> api/v2/snapshots </td>
  <td> Create Item </td></tr>
<tr><td> DELETE  </td>
  <td> - </td>
  <td> NOT SUPPORTED </td></tr>
<tr><td> PUT  </td>
  <td> api/v2/snapshots/:id/propose </td>
  <td> Creates a new snapshot as the Deployment Head </td></tr>
<tr><td> PUT  </td>
  <td> api/v2/snapshots/:id/commit </td>
  <td> Commits snapshot for action by the Annealer </td></tr>
<tr><td> PUT  </td>
  <td> api/v2/snapshots/:id/recall </td>
  <td> Interrupts the Annealer </td></tr>
<tr><td> GET  </td>
  <td> api/v2/snapshots/:id/node_roles </td>
  <td> Shows the node-roles for a snapshot </td></tr>
<tr><td> GET  </td>
  <td> api/v2/snapshots/:id/graph </td>
  <td> Shows the node-role relationships for a snapshot.  Used for the UI Graph</td></tr>

</table>