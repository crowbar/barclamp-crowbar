### Group APIs

Group APIs are used to manage groups.  Groups are used to organized things

#### API Actions

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Comments </th></tr>
<tr><td> GET  </td>
  <td> api/v2/groups </td>
  <td> List </td></tr>
<tr><td> GET  </td>
  <td> api/v2/groups/:id </td>
  <td> Specific Item </td></tr>
<tr><td> PUT  </td>
  <td> api/v2/groups/:id </td>
  <td> Update Item </td></tr>
<tr><td> POST  </td>
  <td> api/v2/groups </td>
  <td> Create Item </td></tr>
<tr><td> DELETE  </td>
  <td> api/v2/groups/:id </td>
  <td> Delete Item </td></tr>

</table>

Category (default = ui) determines the collection of groups.  Allowed categories are

* ui
* rack


