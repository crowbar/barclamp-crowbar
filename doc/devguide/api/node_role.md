### Node Role APIs

Node Roles are the core of Crowbar deployment and orchestration engine

There are four types of data that Crowbar tracks, three of them are maintain on node role.
1. user data (node_role.data) is set by users during the proposed state (also known as "out bound data")
2. system data (node_role.sysdata) is set by crowbar before annealing (also known as "out bound data")
3. wall data (node_role.wall) is set by the jig after transistion (also known as "in bound data")
4. discovery data (node.wall) is stored on the node instead of node role because it reflects node information aggregated from all the jigs.  This information is available using the node.attrib_[name] and Attrib model.  Please see the node API docs for more about this type of data


#### No Natural Key

NodeRole does not have a natural key, so you must reference them them by ID or under the relenvant Nodes, Roles, or Snapshots.

#### Node Attribute Set

> Note: You _must_ create the Attrib with the correct maps to use this method!

This is a friendly way to update the node.discovery json data without having to know the schema.

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> PUT  </td><td> /api/v2/nodes/[node]/attribs/[attrib] </td><td> Takes json {'data':[value] } </td><td> Node Json </td></tr>
</table>

#### NodeRole Create

You must create a NodeRole in order to attach a Node to a Deployment!

Helpers have been added to NodeRole create so that you do not need to provide IDs when creating a new NodeRole.  You can pass:

* Snapshot Name instead of Snapshot ID
* Deployment Name instead of Snapshot ID (uses the Head snapshot)
* Node Name instead of Node ID
* Role Name instead of Role ID

#### NodeRole Delete

This option is NOT supported at this time, but will likely be added before release.