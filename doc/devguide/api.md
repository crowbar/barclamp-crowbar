## Crowbar API

This document is the reference guide for the Crowbar v2 API

### Using the API

The Crowbar API is RESTful and accepts/returns JSON.  XML output is not supported.

The Crowbar API is versioned.  API urls include the Crowbar version of the API (e.g.: 1.0 or v2).  Please use the most highest version available!

> Legacy Note: routes with 1.0 are deprecated!

### API Index

_This is a reference index - API is documented in subpages_

    * /api/v2
      * /anneal (check the annealer status)
      * /make_admin (used by smoke test automation to script install)
      * /nodes
        * /[:id]/node_roles 
        * /[:id]/attribs 
      * /jigs
      * /barclamps
      * /deployments
        * /head (first snapshot)
        * /next (second snapshot)
        * /[:id]/roles
      * /deployment_roles
      * /snapshots 
        * /[:id]/node_roles 
        * /graph (GET only)
        * /propose (PUT only)
        * /commit (PUT only)
        * /recall (PUT only)
      * /jigs
      * /roles 
        * /template/:key/:value (shortcut to set single values of the template)
      * /attribs
      * /groups
        * /[:id]/nodes
    * /:barclamp/v2
      * see docs per barclamp

### Crowbar 2 API Pattern

The Crowbar 2 API follows the following behavior pattern.

#### Expectations:

* Core objects can be referenced equally by name or ID.  This means that objects with natural key names are NOT allowed to start with a number (similar to FQDN restrictions)
* JSON is the API serialization model

  > Warning: Do NOT use API calls without the version # included!  Calls without version numbers are tightly coupled to the UI screens and do not have any contract at all.  They are expected to be used internally by the UI and not maintained for external users!

#### Digest Authentication
API callers may bypass the login screen and use digest authentication for all requests.  Calls directly to API pages will be challenged for digest authentication.  Users who have logged in using the normal login process will be able to use their UI session to make API calls.

  > Note: Because of the hashing method for Digest, user accounts need to be specifically configured for API only access.  A user account with API access will still be able to log in normally.

#### Common API URL Patterns:

Crowbar uses a versioned URL pattern.  Version in the URL allows the barclamp to offer an API contract independent of Crowbar.  By convention, resources names are pluralized in the API.  For example, the API will use =nodes= instead of =node= in paths.

* UI URLs: _these are less documented, unsupported for external use, and do not include a version number_.  Do not use these for API calls!

* Base Form: `[barclamp | api]/[bc_version]/[resources]/[id]`
  * version - version of Crowbar framework being used (v2 for this guide)
  * barclamp - barclamp that owns the requested activity.  Framework uses 'api'
  * bc_version - the version of the barclamp being used. 
  * key_word - groups the API into different categories
     * reserved words such as status and crowbar
     * resource types like node, group, network, etc
  * id - (optional) name or DB id of the barclamp configuration
  * Result codes
     * 200 = OK, success
     * 500 = Error in processing.  Error given as HTML
     * 404 = item not found in database (may return 500 in some cases)

* List: 
  * HTTP get
  * Returns a json array of objects

* CRUD Operations: 
  * id - name or database ID of the item.  Items that do not have natural keys are not expected to honor use of name instead of database ID.  When possible, either will be allowed.
  * RESTful Verbs for CRUD:
     * POST / Create - ID is ignored if provided
     * GET / Read - Objects will be shallow
     * PUT / Update - returns the updated object serialized
     * DELETE/ Delete - no return data except 200
  * Special Cases
     * PUT - used to start an action on existing data (commit a snapshot)
     * POST - used to create new state (propose a snapshot)
     * DELETE - Unlink/Deactivate/Dequeue

In general, Crowbar REST pattern uses the 4 HTTP verbs as follows:

   * GET to retrieve information 
   * PUT to transform or change existing data  
   * POST to create new data or relationships
   * DELETE to remove data or relationships

### Expected Fields

By convention, most Crowbar models have the same fields.

* id - database assigned role, number
* name - resource name, often a natural key with enforced uniqueness
* description - user definable content
* order - override alpha sort order
* created_at - when object was created
* updated_at - when object was last updated
* object_id - cross reference id to an object.  In most cases, you can use the name of the object instead of the API

> Some of the information stored in objects is maintained as json and will appear as nested data.

### API Headers & Response Patterns

The Crowbar REST API uses HTTP "content-type" metadata header tags to help clients quickly identify the information being returned by the API.

The API adds ="application/vnd.crowbar.[type].[form]+json; version=2.0"= to the content-type tag.

* [type] is the object type being returned.  E.g.: node, deployment, jig, etc
* [form] describes how the objects are formed
   * obj = single obj
   * list = list of objects
   * empty = nothing
   * error = error.

REST results should be returned with the appropriate standard HTTP rGETesponse code, such as:

* 200 = ok
* 404 = object not found
* 500 = application error
* [complete list](http://en.wikipedia.org/wiki/List_of_HTTP_status_codes) 

### Example Documentation

The following table should be populated for all API calls:

#### API Actions

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Comments </th></tr>
<tr><td> GET  </td>
  <td> api/v2/resources </td>
  <td> List </td></tr>
<tr><td> GET  </td>
  <td> api/v2/resources/:id </td>
  <td> Specific Item </td></tr>
<tr><td> PUT  </td>
  <td> api/v2/resources/:id </td>
  <td> Update Item </td></tr>
<tr><td> POST  </td>
  <td> api/v2/resources </td>
  <td> Create Item </td></tr>
<tr><td> DELETE  </td>
  <td> api/v2/resources/:id </td>
  <td> Delete Item </td></tr>
<tr><td> VARIOUS  </td>
  <td> api/v2/resources/:id/extra </td>
  <td> Special Ops </td></tr>

</table>

### JSON Output Example:

    {
      "id":41,
      "name":"sim.cr0wbar.com",
      "description":"example",
      "order":100,
      "admin":true,
      "alias":"sim",
      "alive":true,
      "allocated":false,
      "available":true,
      "bootenv":"sledgehammer",
      "deployment_id":1,
      "discovery":{
         {"foo":"this is json"}
      },
      "created_at":"2013-11-01T03:23:27Z",
      "updated_at":"2013-11-01T03:23:27Z"
    }

### Node Create Example:

Node JSON Data: /tmp/node_sample.json
```
{
  "admin": false,
  "alias": "newtest",
  "alive": true,
  "allocated": false,
  "available": true,
  "bootenv": "local",
  "description": "Testing Only - should be automatically removed",
  "hint": "{}",
  "name": "newtest.cr0wbar.com",
  "order": 100
}

```

Curl command to create the node:
```
judd@cb2dev2ubuntu:~$ curl --digest -u 'developer:Cr0wbar!' --data @/temp/node_samples.json -H "Content-Type:application/json" --url http://127.0.0.1:3000/api/v2/nodes

{"admin":false,"alias":"simaa","alive":true,"allocated":false,"available":true,"bootenv":"local","created_at":"2013-12-21T05:49:00Z","deployment_id":1,"description":"devBDD Testing Only - should be automatically removed","discovery":{},"hint":"{}","id":41,"name":"simaa.cr0wbar.com","order":100,"target_role_id":null,"updated_at":"2013-12-21T05:49:00Z"}judd@cb2dev2ubuntu:~$ 
```

