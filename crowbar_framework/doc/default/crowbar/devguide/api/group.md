### Group APIs

Group APIs are used to manage groups.  Groups are used to organized things

#### Group CRUD: Read

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

#### Gruop CRUD: Create

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

#### Group CRUD: Delete 

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
