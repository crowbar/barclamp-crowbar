### Node APIs

Node APIs are used to manage nodes (servers) within the Crowbar system

#### Node Show

By default, returns HTML for node information

**Input:**



<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> GET  </td><td> /node/2.0/[id]?format=json </td><td> id is the node ID or name. </td><td> json extension required to return json  </td></tr>
</table>


**Output:**

    {
      "id":4,
      "fingerprint":-1224971211,
      "state":null,
      "name":"greg.example.com",
      "description":null,
      "order":10000,
      ...
      "created_at":"2012-08-13T17:20:21Z",
      "updated_at":"2012-08-13T17:20:21Z"
    }

Details:

* Format - html (can request json)
* id - Node id
* name - Node name
* all Node properties serialized

#### Node Create (API only)

Creates a new node

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> POST  </td><td> /node/2.0/ </td><td> json definition (see Node Show) </td><td> must be a legal object </td></tr>
</table>

**Input:**

    {
      "name":"fqdn.example.com",
      "description":"description",
      "order":10000,
    }

Details:

* name - Node name (must be FQDN)
* description - optional (default null)
* order - optional (default 10000) 

#### Node Delete (API only)

Deletes a node

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> DELETE  </td><td> /node/2.0/[id] </td><td> Database ID or name </td><td> must be an existing object ID </td></tr>
</table>

No body.

**Ouptut**

None.

Details:

* id - Node name or database ID (must be FQDN)
