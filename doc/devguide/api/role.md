### Role APIs

Roles are a core data type in Crowbar.  They are used to define services that Crowbar deploys in the environment.  

> Role names are globally unique. This restriction may be related in the future

#### Role Types

Crowbar allows Barclamp creators to override the default Role behavior.  This is a very import extension point for Crowbar because custom Role beaviors are essential to many orchestration situations.

If no override is provided, Crowbar will use the Crowbar::Role class.

A role specific override can be created using the name of the barclamp-role to create the class in the Barclamp model name space.  For example, a role called test-admin should be created as BarclampTest::Admin (or app/models/barclamp_test/admin.rb).  When the role is imported, Crowbar will automatically use this type if it has been defined.

A barclamp specific override can be created using the name of the barclamp and the class role.  If present, this class will be used if no specific role class has been provided.  This is very useful for barclamps that create roles dynamically like the network barclamp.  For example, Crowbar will use the BarclampNetwork::Role (or app/models/barclamp_network/role.rb) class when new Network roles are added.  This allows Barclamp creators to add custom event handling without knowing the name of the roles in advance.

> This is also related to how Role Events are handled


#### API Actions

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Comments </th></tr>
<tr><td> GET  </td>
  <td> api/v2/roles </td>
  <td> List </td></tr>
<tr><td> GET  </td>
  <td> api/v2/roles/:id </td>
  <td> Specific Item </td></tr>
<tr><td> PUT </td>
  <td> - </td>
  <td> NOT SUPPORTED / managed during import only </td></tr>
<tr><td> POST  </td>
  <td> - </td>
  <td> NOT SUPPORTED / roles are only created during import </td></tr>
<tr><td> DELETE  </td>
  <td> - </td>
  <td> NOT SUPPORTED </td></tr>
<tr><td> PUT  </td>
  <td> /api/v2/roles/[role]/template/[key]/[value] </td>
  <td> update a single key/value in the template </td></tr>

</table>

#### Role Events (triggered on NodeRole state changes)

The Role model has a series of events (Self.on_[STATE]) that are called when any NodeRole changes state.  This is a designed override point where Barclamp Roles can add functionality into the Crowbar Annealer engine.  This functionality is added when a Barclamp defines it's own Role definitions (Barclamp[Name]::[Role]).  If there is no override, then the default behavior is used.

There is a matching event for each NodeRole state.  The event is called when the NodeRole enters that state.  The purpose of this function is to enable Barclamp creators to leverage information available to Crowbar within the Annealer operation including before user editing (Proposed) or system error (Error) states.