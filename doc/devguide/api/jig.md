### Jig (aka CMDB interface) APIs

Jigs are the interface between Crowbar and doing work in the infrastructure.

#### System Jigs

Crowbar has three built-in jigs

* Script - uses SSH to perform operations on nodes.  This is used for bootstrapping actions that install the agents for other Jigs.  Not activated in development mode.
* Noop (no operation) - takes internal actions in Crowbar only.  Used when database updates or coordination points are needed that have no external action.
* Test - used by the test infrastructure to validate Crowbar logic when no phyiscal infrastructure is available.  Not activited in production mode.

#### API Actions

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Comments </th></tr>
<tr><td> GET  </td>
  <td> api/v2/jigs </td>
  <td> List </td></tr>
<tr><td> GET  </td>
  <td> api/v2/jigs/:id </td>
  <td> Specific Item </td></tr>
<tr><td> PUT  </td>
  <td> api/v2/jigs/:id </td>
  <td> Update Item </td></tr>
<tr><td> POST  </td>
  <td> api/v2/jigs </td>
  <td> Create Item </td></tr>
<tr><td> DELETE  </td>
  <td> api/v2/jigs/:id </td>
  <td> Delete Item </td></tr>
<tr><td> VARIOUS  </td>
  <td> api/v2/jigs/:id/extra </td>
  <td> Special Ops </td></tr>

</table>