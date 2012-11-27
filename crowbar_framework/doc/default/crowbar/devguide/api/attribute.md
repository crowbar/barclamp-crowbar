### Attribute APIs

Attribute APIs are used to manage attributes tracked by the CMDB system

#### Attribute CRUD

List, Create, Read, Delete actions for Attribute

> There is no update at this time!

##### List

Returns list of Attribute id:names in the system

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> GET  </td>
  <td> /2.0/crowbar/2.0/attribute </td>
  <td> - </td>
  <td> - </td></tr>
</table>


**Output:**

    {
      1:"ram",
      2:"cpu",
      4:"nics"
    }

Details:

* id - Attribute id
* name - Attribute name

##### Read

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> GET  </td>
  <td> /2.0/crowbar/2.0/attribute/[id] </td>
  <td> id is the Attribute ID or name. </td>
  <td> -  </td></tr>
</table>


**Output:**

    {
      "id":4,
      "name":"ram",
      "description":null,
      "order":10000,
      "created_at":"2012-08-13T17:20:21Z",
      "updated_at":"2012-08-13T17:20:21Z"
    }

Details:

* Format - json
* id - Attribute id
* name - Attribute name

##### Attribute CRUD: Create

Creates a new Attribute

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> POST  </td>
  <td> /2.0/crowbar/2.0/attribute/ </td>
  <td> json definition (see Attribute Show) </td>
  <td> must be a legal object </td></tr>
</table>

**Input:**

    { 
      "name":"chef",
      "description":"description",
      "order":10000,
    }

Details:

* name (required) - Attribute name (must be letters - numbers and start with a letter)
* description - optional (default null)
* order - optional (default 10000) 

##### Attribute CRUD: Delete 

Deletes an Attribute

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> DELETE  </td>
  <td> /2.0/crowbar/2.0/attribute/[id] </td>
  <td> Database ID or name </td>
  <td> must be an existing object ID </td></tr>
</table>

No body.

**Ouptut**

None.

Details:

* id - Attribute name or database ID



