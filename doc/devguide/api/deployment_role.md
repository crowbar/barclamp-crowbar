### DeploymentRole API

DeploymentRoles provide the default values for NodeRoles in a snapshot.  They are populated from the role's template during import.

Unlike NodeRoles, they do not store any inbound or system data.

#### CRUD

<table border=1>
  <tr><th> Verb </th><th> URL                      </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
  <tr><td> GET  </td><td> /api/v2/deployment_role/  </td><td> none   </td><td> Lists All </td><td> - </td></tr> 
  <tr><td> GET  </td><td> /api/v2/deployment_role/[:id]  </td><td> none   </td><td> Return Object </td><td> - </td></tr> 
  <tr><td> PUT  </td><td> /api/v2/deployment_role/[:id]  </td><td> none   </td><td>  Update Object </td><td> - </td></tr> 
  <tr><td> POST  </td><td> /api/v2/deployment_role/  </td><td> none   </td><td>  </td><td> Creates New </td></tr> 
  <tr><td> DELETE  </td><td> /api/v2/deployment_role/[:id]  </td><td> none</td><td>  NOT SUPPORTED </td><td> - </td></tr> 
</table>

The API includes shortcuts for role_id and snapshot_id to make it easier to use the API.
* deployment -> provide the name, resolved into snapshot_id of the deployment.head
* role -> provide the name, resolved into role_id
