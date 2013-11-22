### Barclamp APIs

Barclamps are the core modulization for Crowbar.  For that reason, the API for barclamps is more limited because changes to barclamps can cause breaking changes to the framework.

There are very limited actions to be taken on a barclamp.

#### API Actions

<table border=0>
<tr><th> Verb </th><th> URL </th><th> Comments </th></tr>
<tr><td> GET  </td>
  <td> api/v2/barclamps </td>
  <td> List </td></tr>
<tr><td> GET  </td>
  <td> api/v2/barclamps/:id </td>
  <td> Specific Item </td></tr>
<tr><td> PUT  </td>
  <td> api/v2/barclamps/:id </td>
  <td> Update Item </td></tr>
<tr><td> POST  </td>
  <td> api/v2/barclamps </td>
  <td> Create Item </td></tr>
<tr><td> DELETE  </td>
  <td> api/v2/barclamps/:id </td>
  <td> Delete Item </td></tr>

</table>

#### Barclamp APIs

Barclamps may expose their own set of APIs as */barclamp/v2/[resoources]* depending on their need for functionality.  Barclamps are expected to extend the documentation in the */doc/devguide/api* paths if they expose REST APIs.

>Barclamps are not required to have additional APIs.
