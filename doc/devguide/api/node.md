### Node APIs

Node APIs are used to manage nodes (servers) within the Crowbar system


#### Node CRUD

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> GET  </td><td> /api/v2/nodes </td><td> no options </td><td> </td></tr>
<tr><td> GET  </td><td> /api/v2/nodes/[id]</td><td> id is the node ID or name. </td><td>   </td></tr>
<tr><td> POST  </td><td> /api/v2/nodes  </td><td> json definition (see Node Show) </td><td> must be a legal object and FQDN name </td></tr>
<tr><td> PUT  </td><td> /api/v2/nodes/[id] </td><td> json definition (see Node Show) </td><td> must be a legal object and FQDN name </td></tr>
<tr><td> DELETE  </td><td> /api/v2/nodes/[id] </td><td> Database ID or name </td><td> must be an existing object ID </td></tr>
</table>

Details:

* id - Node name or database ID (must be FQDN)

#### Node Attribute Set

> Note: You _must_ create the Attrib with the correct maps to use this method!

This is a friendly way to update the node.discovery json data without having to know the schema.

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> PUT  </td><td> /api/v2/nodes/[node]/attribs/[attrib] </td><td> Takes json {'data':[value] } </td><td> Node Json </td></tr>
</table>

### Node Attributes

Node Attributes API is used to retrieve data about attributes that have been associated with a Node.

Typically, attribute data is populated by the CMDB system(s) based on the associations established using this API.




