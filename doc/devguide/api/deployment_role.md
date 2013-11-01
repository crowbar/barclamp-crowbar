### Deployment-Role API

DeploymentRoles provide the default values for node-roles in a snapshot.  They are populated from the role's template during import.

Unlike node-roles, they do not store any inbound or system data.

#### API Actions

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Comments </th></tr>
<tr><td> GET  </td>
  <td> api/v2/deployment_roles </td>
  <td> List </td></tr>
<tr><td> GET  </td>
  <td> api/v2/deployment_roles/:id </td>
  <td> Specific Item </td></tr>
<tr><td> PUT  </td>
  <td> api/v2/deployment_roles/:id </td>
  <td> Update Item </td></tr>
<tr><td> POST  </td>
  <td> api/v2/deployment_roles </td>
  <td> Create Item </td></tr>
<tr><td> DELETE  </td>
  <td> - </td>
  <td> NOT SUPPORTED </td></tr>

</table>

The API includes shortcuts for 

   * deployment -> provide the name, resolved into snapshot_id of the deployment.head
   * snapshot -> provide the name, resolved into snapshot_id
   * role -> provide the name, resolved into role_id
