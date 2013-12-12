### Node APIs

Node APIs are used to manage nodes (servers) within the Crowbar system

When Nodes are created, updated or deleted, roles and jigs are notified so they can tale appropriate actions.

#### API Actions

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Comments </th></tr>
<tr><td> GET  </td>
  <td> api/v2/nodes </td>
  <td> List </td></tr>
<tr><td> GET  </td>
  <td> api/v2/nodes/:id </td>
  <td> Specific Item </td></tr>
<tr><td> PUT  </td>
  <td> api/v2/nodes/:id </td>
  <td> Update Item, notifies all jigs and roles </td></tr>
<tr><td> POST </td>
  <td> api/v2/nodes </td>
  <td> Create Item, notifies all jigs and roles </td></tr>
<tr><td> DELETE </td>
  <td> api/v2/nodes/:id </td>
  <td> Delete Item + notifies all jigs and roles </td></tr>
<tr><td> GET  </td>
  <td> api/v2/nodes/:id/node_roles </td>
  <td> Shows all the roles that the node is using (including their status) </td></tr>

</table>

Details:

* name - must be FQDN



