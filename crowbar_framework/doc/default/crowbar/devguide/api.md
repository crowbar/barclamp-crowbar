## Crowbar API

This document is the reference guide for the Crowbar 2.0 API

### Using the API

The Crowbar API is RESTful and accepts/returns JSON.  While some arguments may return XML as an option, it is NOT a supported output.

The Crowbar API is versioned.  API urls include the Crowbar version of the API (e.g.: 1.0 or 2.0).  Please use the most highest version available!

> Legacy Note: some routes maintain a 1.0 API; however, these should all be considered deprecated!

### Crowbar 2 API Pattern

The Crowbar 2 API attempts to follow the following behavior pattern.

#### Expectations:

* Core objects can be referenced equally by name or ID.  This means that objects with natural key names are NOT allowed to start with a number (similar to FQDN restrictions)
* JSON is the API serialization model
* Single objects will return the simple serialization (object.json) output
* Cross references will use database IDs (normalized xref) for returns, not natural keys or names
* Some calls are shared by the UI and may require `?format=json`.  It is recommended that all API calls include this parameter for safety.

  > Warning: Do NOT use API calls without the version # included!  Calls without version numbers are tightly coupled to the UI screens and do not have any contract at all.  They are expected to be used internally by the UI and not maintained for external users!

#### Digest Authentication
API callers may bypass the login screen and use digest authentication for all requests by visiting the `/digest` page before making requests to establish a digest session.  API calls will respect either digest or session based logins.

#### Common API URL Patterns:

* UI URLs: _these are undocumented, unsupported for external use, and do not include a version number_
  * Do not use these for API calls!

* Basic Contract: `[key_word]/2.0/` ...
  * key_word - groups the API into different categories
     * reserved words such as status and crowbar
     * resource types like node, group, etc
  * "2.0" indicates the API version (will be incremented if API changes)
  * Result codes
     * 200 = OK, success
     * 500 = Error in processing.  Error given as HTML
     * 404 = item not found in database (may return 500 in some cases)

* List/Status: `status/2.0/[class]/[id]?filter=X`
  * HTTP get
  * class - not all objects are provided with list status support!  This is NOT a CRUD operation.
  * returns performance/data specialized list format that varies per object, _not serialized_ objects.  
  * id - (optional) may be used for opimization to return a restricted result set.  The list returned must be the same format as the list if all items are returned
  * ?filter=X - (optional) may be used to filter the list results  

* CRUD Operation: `[object_type]/2.0/[id]`
  * id - name or database ID of the item.  Items that do not have natural keys are not expected to honor use of name instead of database ID.  When possible, either will be allowed.
  * RESTful Verbs for CRUUD:
     * Create - HTTP Post (ID is ignored)
     * Read - HTTP Get
       * Objects will be shallow (they will not populate child references beyond the ID(s)).
     * Unlink/Deactivate/Dequeue - HTTP Delete 
     * Update - HTTP Put (returns the updated object serialized)
     * Delete - HTTP Delete (no return except 200)

  > Unlink/Deactivate/Dequeue perform "unlinking" operations where the API removed a reference but does not delete the target object 
  
* Action: `[object_type]/2.0/[id]/[action]/[predicate_id]`
  * Must be HTTP PUT / POST / DELETE
      * Delete without an action implies object is removed
      * Delete with action implies unlinking operation
  * action - defines the action to be taken on the ID
  * predicate_id - (optional) object that receives the action.  As in "ID-Foo joins ID-Bar" where joins is the action and ID-Bar is the predicate.

* Barclamps: `crowbar/2.0/[barclamp]/[bc_version]/[id]`
   * barclamp - the name or ID of the barclamp being referenced
   * bc_version - the version of the barclamp being used.  This allows the barclamp to offer an API contract independent of Crowbar.  For example, it would be possible to have an update to the Network barclamp that adds to the API (2.0 -> 2.1) and callers would need to be able to identify/require a version.  It is anticipated that Barclamps will honor previous versions where possible.
   * id - name or DB id of the barclamp configuration

* Configurations: `(barclamp url) + /instance/[active | iid]`
   * iid 
      * name or DB id of the instance requested for the barclamp configuration
      * convenience value returns the active instance for the barclamp

#### Documentation

The following table should be populated for all API calls:

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> GET  </td><td> /node/2.0/[id] </td><td> id is the node ID or name. </td><td> Json: please include an example below the table! </td><td> Jokes, etc </td></tr>
</table>

An example JSON file should be provided
