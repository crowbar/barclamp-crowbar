### Instance APIs

#### Barclamp Instances

**Input:**

<table border=1>
  <tr><th> Verb </th><th> URL                      </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
  <tr><td> GET  </td><td> /[:barclamp]/v2/instances  </td><td> none   </td><td> Instance List </td><td> - </td></tr> 
  <tr><td> GET  </td><td> /[:barclamp]/v2/instances/[:instance]  </td><td> none   </td><td> Instance Info </td><td> - </td></tr> 
</table>

#### Barclamp Config Roles

**Input:**

<table border=1>
  <tr><th> Verb </th><th> URL                      </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
  <tr><td> GET  </td><td> /[:barclamp]/v2/instances/[:instance]/roles  </td><td> none   </td><td> Roles Assigned </td><td> - </td></tr> 
  <tr><td> GET  </td><td> /[:barclamp]/v2/instances/[:instance]/roles/[:role]/attribs  </td><td> none   </td><td> Attribs Assigned </td><td> - </td></tr> 
  <tr><td> GET  </td><td> /[:barclamp]/v2/instances/[:instance]/roles/[:role]/nodes  </td><td> none   </td><td> Nodes Assigned </td><td> - </td></tr> 
  <tr><td> PUT  </td><td> /[:barclamp]/v2/instances/[:instance]/roles/[:role]/nodes/[:node]  </td><td> none   </td><td> Add Node to Role </td><td> Proposed Instances Only </td></tr> 
  <tr><td> DELETE </td><td> /[:barclamp]/v2/instances/[:instance]/roles/[:role]/nodes/[:node]  </td><td> none   </td><td> Remove Node from Role</td><td> Proposed Instances Only </td></tr> 
</table>

