### Deployment  APIs

Deployments are the scope boundry for Crowbar activities on nodes-roles.  They are a central component of the Crowbar data model.

> The =system= deployment is a special purpose built-in deployment that cannot be edited by Crowbar users.  It handles all node discovery operations.

#### API Actions

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Comments </th></tr>
<tr><td> GET  </td>
  <td> api/v2/deployments </td>
  <td> List </td></tr>
<tr><td> GET  </td>
  <td> api/v2/deployments/:id </td>
  <td> Specific Item </td></tr>
<tr><td> PUT  </td>
  <td> api/v2/deployments/:id </td>
  <td> Update Item </td></tr>
<tr><td> POST  </td>
  <td> api/v2/deployments </td>
  <td> Create Item </td></tr>
<tr><td> DELETE  </td>
  <td> api/v2/deployments/:id </td>
  <td> Delete Item </td></tr>
<tr><td> GET  </td>
  <td> api/v2/deployments/head </td>
  <td> returns the current active deployment snapshot </td></tr>
<tr><td> GET  </td>
  <td> api/v2/deployments/next </td>
  <td> returns the most recent inactive deployment snapshot </td></tr>
<tr><td> GET  </td>
  <td> api/v2/deployments/:id/roles </td>
  <td> returns deployment_roles bound to the deployment head </td></tr>

</table>
