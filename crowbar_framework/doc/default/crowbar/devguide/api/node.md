
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
<tr><td> GET  </td><td> /node/2.0/ </td><td> json definition (see Node Show) </td><td> must be a legal object </td></tr>
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
      
#### Node Status

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> GET  </td><td> /node/status/2.0 </td><td> none </td><td> All nodes </td><td> Used by Dashboard </td><td> 
<tr><td> GET  </td><td> /node/status/2.0/[id] </td><td> id is the node ID or name. </td><td> - </td><tr>
<table>

**Output:**

    {
      "state":{"1":null},
      "sum":-1881043387,
      "i18n":{"ready":"Ready"},
      "groups":{
        "0":{"failed":0,"ready":0,"building":0,"pending":0,"unready":1,"name":"all","unknown":0}
      },
      "count":1,
      "status":{"1":"unready"}
    }

Details:

* Format - json
* i18n - the localized versions of the status strings for display.
* state - ?
* groups - ?
* status - ?
* count - ?
* sum - Hashed value of the nodes included to identify state changes for refresh
