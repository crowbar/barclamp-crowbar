### Barclamp/Role APIs

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

