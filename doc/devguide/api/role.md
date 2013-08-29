### Barclamp/Role APIs

#### Update key in template

You can update a single key/value in the template using the following API

<table border=1>
  <tr><th> Verb </th><th> URL                       </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
  <tr><td> PUT  </td><td> /api/v2/roles/[role]/template/[key]/[value]</td><td> none   </td><td> Role Object </td><td> - </td></tr> 
</table>


#### General RoleActions

<table border=1>
  <tr><th> Verb </th><th> URL                       </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
  <tr><td> GET  </td><td> /[:barclamp]/v2/roles     </td><td> none   </td><td> Roles Assigned </td><td> - </td></tr> 
  <tr><td> GET  </td><td> /[:barclamp]/v2/roles/[:role]/attribs  </td><td> none   </td><td> Attribs Assigned </td><td> - </td></tr> 
</table>

You cannot add/delete roles to the Barclamp instance.  These are determined by the Barclamp during installation time.

#### 

<table border=1>
  <tr><th> Verb </th><th> URL                       </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
  <tr><td> GET  </td><td> /[:barclamp]/v2/roles/[:role]/nodes    </td><td> none   </td><td> Nodes Assigned </td><td> - </td></tr> 
  <tr><td> PUT  </td><td> /[:barclamp]/v2/roles/[:role]/nodes/[:node]   </td><td> none   </td><td> Add Node to Role </td><td> Proposed Instances Only </td></tr> 
  <tr><td> DELETE </td><td> /[:barclamp]/v2/roles/[:role]/nodes/[:node]  </td><td> none   </td><td> Remove Node from Role</td><td> Proposed Instances Only </td></tr> 
</table>

