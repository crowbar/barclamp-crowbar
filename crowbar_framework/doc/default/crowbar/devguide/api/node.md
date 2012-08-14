### Node APIs

Node APIs are used to manage nodes (servers) within the Crowbar system

#### Node

By default, returns HTML for node information

**Input:**

| URL | Options | Returns | Comments |
|:---:|:-------:|:-------:|:--------:|
| /node/2.0/[id]?format=json | id is the node ID or name. | json extension required to return json  |

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
      
#### Node Status

**Input:**

| URL | Options | Returns | Comments |
|:---:|:-------:|:-------:|:--------:|
| /node/status/2.0 | none | All nodes | Used by Dashboard | 
| /node/status/2.0/[id] | id is the node ID or name. |  |

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

