### Group APIs

Group APIs are used to manage groups.  Groups are used to organized things

#### Group CRUD

Create, Read, Update, Delete actions for Groups

##### Read

By default, returns HTML for node information

**Input:**


<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> GET  </td><td> /group/2.0/[id] </td><td> id is the group ID or name. </td><td> json extension required to return json  </td></tr>
</table>


**Output:**

    {
      "id":4,
      "name":"greg.example.com",
      "description":null,
      "order":10000,
      "category":"ui",
      ...
      "created_at":"2012-08-13T17:20:21Z",
      "updated_at":"2012-08-13T17:20:21Z"
    }

Details:

* Format - html (can request json)
* id - Node id
* name - Node name
* category - one of the allowed categories in lowercase: ui, rack 
* all Node properties serialized

##### Group CRUD: Create

Creates a new group

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> POST  </td><td> /node/2.0/ </td><td> json definition (see Node Show) </td><td> must be a legal object </td></tr>
</table>

**Input:**

    {
      "name":"fqdn.example.com",
      "description":"description",
      "category":"ui"
      "order":10000,
    }

Details:

* name - group name (must be letters - numbers and start with a letter)
* description - optional (default null)
* category - (default = ui) determines the collection of groups.  Allowed categories are
  * ui
  * rack
* order - optional (default 10000) 

##### Group CRUD: Delete 

Deletes a group

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> DELETE  </td><td> /group/2.0/[id] </td><td> Database ID or name </td><td> must be an existing object ID </td></tr>
</table>

No body.

**Ouptut**

None.

Details:

* id - Group name or database ID

#### Node Actions on Groups

These actions are for adding, removing, or moving nodes in groups

##### Add Node

Adds a node to an existing group

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> POST  </td><td> /group/2.0/[group-id]/node/[node-id] </td><td> json definition (see Node Show) </td><td> must be a legal objects </td></tr>
</table>

**Input:**

All data is contained in the URL

Details:

* group-id: id of the group
* node-id: id if the node (can be name) 

##### Move Node

Moves a node from an existing group to an another group _in the same category_.  This is effectively a combined delete/add action.

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> PUT  </td><td> /group/2.0/[group-id]/node/[node-id] </td><td> json definition (see Node Show) </td><td> must be a legal objects </td></tr>
</table>

**Input:**

All data is contained in the URL

Details:

* group-id: id of the group
* node-id: id if the node (can be name) 


##### Delete Node

Removes a node from an existing group 

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> DELETE </td><td> /group/2.0/[group-id]/node/[node-id] </td><td> json definition (see Node Show) </td><td> must be a legal objects </td></tr>
</table>

**Input:**

All data is contained in the URL

Details:

* group-id: id of the group
* node-id: id if the node (can be name) 

