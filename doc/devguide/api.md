## Crowbar API

This document is the reference guide for the Crowbar v2 API

### Using the API

The Crowbar API is RESTful and accepts/returns JSON.  XML output is not supported.

The Crowbar API is versioned.  API urls include the Crowbar version of the API (e.g.: 1.0 or v2).  Please use the most highest version available!

> Legacy Note: routes with 1.0 should considered deprecated!

### Crowbar 2 API Pattern

The Crowbar 2 API attempts to follow the following behavior pattern.

#### Expectations:

* Core objects can be referenced equally by name or ID.  This means that objects with natural key names are NOT allowed to start with a number (similar to FQDN restrictions)
* JSON is the API serialization model

  > Warning: Do NOT use API calls without the version # included!  Calls without version numbers are tightly coupled to the UI screens and do not have any contract at all.  They are expected to be used internally by the UI and not maintained for external users!

#### Digest Authentication
API callers may bypass the login screen and use digest authentication for all requests.  Calls directly to API pages will be challenged for digest authentication.  Users who have logged in using the normal login process will be able to use their UI session to make API calls.

  > Note: Because of the hashing method for Digest, user accounts need to be specifically configured for API only access.  A user account with API access will still be able to log in normally.

#### Common API URL Patterns:

* UI URLs: _these are less documented, unsupported for external use, and do not include a version number_.  Do not use these for API calls!

* Base Form: `/[api | [barclamp name]/[bc_version]/[resources]/[id]`
  * version - version of Crowbar framework being used (v2 for this guide)
  * barclamp - barclamp that owns the requested activity
  * bc_version - the version of the barclamp being used.  This allows the barclamp to offer an API contract independent of Crowbar.  For example, it would be possible to have an update to the Network barclamp that adds to the API (v2 -> v3) and callers would need to be able to identify/require a version.  It is anticipated that Barclamps will honor previous versions where possible.
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

* CRUD Operation: 
  * id - name or database ID of the item.  Items that do not have natural keys are not expected to honor use of name instead of database ID.  When possible, either will be allowed.
  * RESTful Verbs for CRUD:
     * Create - HTTP Post (ID is ignored)
     * Read - HTTP Get
       * Objects will be shallow (they will not populate child references beyond the ID(s)).
     * Unlink/Deactivate/Dequeue - HTTP Delete 
     * Update - HTTP Put (returns the updated object serialized)
     * Delete - HTTP Delete (no return except 200)

* Action: 
  * HTTP PUT 

### API Index

_This is a temporary summary - API should be split down into subpages!_

* /api/v2
  * /nodes (list)
    * /:node (CRUD)
      * /attribs
      * /groups
      * /transition (put)
      * /allocate (put)
  * /barclamps
    * /:barclamp (R)
      * /deployments
      * /template (redirects to snapshot)
  * /deployments
    * /:deployment (CRUD)
      * /commit (put)
      * /recall (put)
  * /snapshots 
    * /:snapshot (CRUD)
  * /jigs
    * /:jig (CRUD)
  * /role_types 
    * /:role_type (CRUD)
  * /roles (CRUD)
    * /:role (CRUD)
      * /attribs
      * /nodes
  * /attribs
    * /:attrib
  * /groups
    * /:group
      * /nodes
  
* /:barclamp
  * /v2 API defined by barclamp
  * other paths are UI paths
    
* /dashboard
  * /status/:node
* /barclamp
  * /status/:deployment


#### Documentation

The following table should be populated for all API calls:

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> GET  </td>
  <td> api/v2/barclamp/crowbar/deployment/default </td>
  <td> id is the node ID or name. </td>
  <td> Json: please include an example below the table! </td>
  <td> Jokes, etc </td></tr>
</table>