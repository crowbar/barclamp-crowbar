### Barclamp/Instance APIs

Instances can only be created by

1. Cloning the template instance using barclamp.create_proposal (see Barclamp/Config)
2. Editing an existing configuration which clones from the active instance

<table border=1>
  <tr><th> Verb </th><th> URL                      </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
  <tr><td> GET  </td><td> /[:barclamp]/v2/instances  </td><td> none   </td><td> Instance List </td><td> - </td></tr> 
  <tr><td> GET  </td><td> /[:barclamp]/v2/instances/[:instance]  </td><td> none   </td><td> Instance Info </td><td> - </td></tr> 
</table>
