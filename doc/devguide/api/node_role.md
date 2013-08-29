### Node Role APIs

Node Roles are the core of Crowbar deployment and orchestration engine

There are four types of data that Crowbar tracks, three of them are maintain on node role.
1. user data (node_role.data) is set by users during the proposed state (also known as "out bound data")
2. system data (node_role.sysdata) is set by crowbar before annealing (also known as "out bound data")
3. wall data (node_role.wall) is set by the jig after transistion (also known as "in bound data")
4. discovery data (node.wall) is stored on the node instead of node role because it reflects node information aggregated from all the jigs.  This information is available using the node.attrib_[name] and Attrib model.  Please see the node API docs for more about this type of data


#### Node Attribute Set

> Note: You _must_ create the Attrib with the correct maps to use this method!

This is a friendly way to update the node.discovery json data without having to know the schema.

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> PUT  </td><td> /api/v2/nodes/[node]/attribs/[attrib] </td><td> Takes json {'data':[value] } </td><td> Node Json </td></tr>
</table>

