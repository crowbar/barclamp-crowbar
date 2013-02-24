### Barclamp/Config APIs

#### Barclamp Config

<table border=1>
  <tr><th> Verb </th><th> URL                      </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
  <tr><td> GET  </td><td> /[:barclamp]/v2/configs  </td><td> none   </td><td> Config List </td><td> - </td></tr> 
  <tr><td> POST </td><td> /[:barclamp]/v2/configs  </td><td> none   </td><td> New Config </td><td> - </td></tr> 
  <tr><td> GET  </td><td> /[:barclamp]/v2/configs/[:config]  </td><td> none   </td><td> Existing Config Detail </td><td> - </td></tr> 
  <tr><td> PUT  </td><td> /[:barclamp]/v2/configs/[:config]  </td><td> none   </td><td> Update Config Detail </td><td> - </td></tr> 
  <tr><td> GET  </td><td> /[:barclamp]/v2/configs/[:config]  </td><td> none   </td><td> Existing Config Detail </td><td> - </td></tr> 
</table>

Posting a new configuration will automatically clone the template instance as the proposed configuration instance for the new configuration.

> Not all barclamps allow multiple configurations!

#### Barclamp Config Actions

<table border=1>
  <tr><th> Verb </th><th> URL                      </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
  <tr><td> PUT  </td><td> /[:barclamp]/v2/configs/[:config]/commit  </td><td> none   </td><td> Commit Proposed </td><td> - </td></tr> 
  <tr><td> PUT  </td><td> /[:barclamp]/v2/configs/[:config]/dequeue </td><td> none   </td><td> Dequeue Proposed Config </td><td> - </td></tr> 
  <tr><td> PUT  </td><td> /[:barclamp]/v2/configs/[:config]/propose </td><td> none   </td><td> Create an new Proposal based on Active</td><td> - </td></tr> 
  <tr><td> PUT  </td><td> /[:barclamp]/v2/configs/[:config]/transition </td><td> none   </td><td> Send Transistion Data into the system</td><td> - </td></tr> 
</table>


