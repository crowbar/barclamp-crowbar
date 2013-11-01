### Node Role APIs

Node Roles are the core of Crowbar deployment and orchestration engine

There are four types of data that Crowbar tracks, three of them are maintain on node role.
1. user data (node_role.data) is set by users during the proposed state (also known as "out bound data")
2. system data (node_role.sysdata) is set by crowbar before annealing (also known as "out bound data")
3. wall data (node_role.wall) is set by the jig after transistion (also known as "in bound data")
4. discovery data (node.wall) is stored on the node instead of node role because it reflects node information aggregated from all the jigs.  This information is available using the node.attrib_[name] and Attrib model.  Please see the node API docs for more about this type of data

NodeRole does not have a natural key, so you must reference them them by ID or under the relenvant Nodes, Roles, or Snapshots.

#### API Actions

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Comments </th></tr>
<tr><td> GET  </td>
  <td> api/v2/node_roles </td>
  <td> List </td></tr>
<tr><td> GET  </td>
  <td> api/v2/node_roles/:id </td>
  <td> Specific Item </td></tr>
<tr><td> PUT  </td>
  <td> api/v2/node_roles/:id </td>
  <td> Update Item </td></tr>
<tr><td> POST  </td>
  <td> api/v2/node_roles </td>
  <td> Create Item </td></tr>
<tr><td> DELETE  </td>
  <td> - </td>
  <td> NOT SUPPORTED </td></tr>

</table>

You must create a NodeRole in order to attach a Node to a Deployment!

Helpers have been added to NodeRole create so that you do not need to provide IDs when creating a new NodeRole.  You can pass:

* Snapshot Name instead of Snapshot ID
* Deployment Name instead of Snapshot ID (uses the Head snapshot)
* Node Name instead of Node ID
* Role Name instead of Role ID
