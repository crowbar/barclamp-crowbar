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

#### Role Events (triggered on NodeRole state changes)

The Role model has a series of events (Self.on_[STATE]) that are called when any NodeRole changes state.  This is a designed override point where Barclamp Roles can add functionality into the Crowbar Annealer engine.  This functionality is added when a Barclamp defines it's own Role definitions (Barclamp[Name]::[Role]).  If there is no override, then the default behavior is used.

There is a matching event for each NodeRole state.  The event is called when the NodeRole enters that state.  The purpose of this function is to enable Barclamp creators to leverage information available to Crowbar within the Annealer operation including before user editing (Proposed) or system error (Error) states.