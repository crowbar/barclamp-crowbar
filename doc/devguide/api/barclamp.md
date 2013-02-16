### Barclamp APIs

Barclamps are the core modulization for Crowbar.  For that reason, the API for barclamps is more limited because changes to barclamps can cause breaking changes to the framework.

#### Barclamp List

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
  <tr><td> GET </td><td> /crowbar/v2/barclamps  </td><td> none  </td><td> All barclamps </td><td> - </td></tr> 
</table>

**Output:**

#### Barclamp Catalog

Provides the list of supported resources for the API

<table border=1>
  <tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
  <tr><td> GET  </td><td> /[:barclamp]/v2/catalog </td><td> none  </td><td> Actions List </td><td> - </td></tr> 
</table>

#### Barclamp Template

**Input:**

<table border=1>
  <tr><th> Verb </th><th> URL                      </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
  <tr><td> GET  </td><td> /[:barclamp]/v2/template   </td><td> none   </td><td> Redirects to Template Instance </td><td> - </td></tr> 
</table>

This will redirect to the Instance used for the template.

#### Barclamp Config

**Input:**

<table border=1>
  <tr><th> Verb </th><th> URL                      </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
  <tr><td> GET  </td><td> /[:barclamp]/v2/configs  </td><td> none   </td><td> Config List </td><td> - </td></tr> 
  <tr><td> POST </td><td> /[:barclamp]/v2/configs  </td><td> none   </td><td> New Config </td><td> - </td></tr> 
  <tr><td> GET  </td><td> /[:barclamp]/v2/configs/[:config]  </td><td> none   </td><td> Existing Config Detail </td><td> - </td></tr> 
  <tr><td> PUT  </td><td> /[:barclamp]/v2/configs/[:config]  </td><td> none   </td><td> Update Config Detail </td><td> - </td></tr> 
  <tr><td> GET  </td><td> /[:barclamp]/v2/configs/[:config]  </td><td> none   </td><td> Existing Config Detail </td><td> - </td></tr> 
</table>

#### Barclamp Config Actions

**Input:**

<table border=1>
  <tr><th> Verb </th><th> URL                      </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
  <tr><td> PUT  </td><td> /[:barclamp]/v2/configs/[:config]/commit  </td><td> none   </td><td> Commit Proposed </td><td> - </td></tr> 
  <tr><td> PUT  </td><td> /[:barclamp]/v2/configs/[:config]/dequeue </td><td> none   </td><td> Dequeue Proposed Config </td><td> - </td></tr> 
  <tr><td> PUT  </td><td> /[:barclamp]/v2/configs/[:config]/propose </td><td> none   </td><td> Create an new Proposal based on Active</td><td> - </td></tr> 
  <tr><td> PUT  </td><td> /[:barclamp]/v2/configs/[:config]/transition </td><td> none   </td><td> Send Transistion Data into the system</td><td> - </td></tr> 
</table>

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


#### Not Supported

Barclamp API does not support create, update and delete operations.

#### API v1 temporary dumping ground

 scope 'crowbar' do
    version = "1.0"

    get    ":controller/#{version}/help", :action => 'help', :as => :help_barclamp
    get    ":controller/#{version}/proposals/nodes", :action=>'nodes', :as => :barclamp_nodes
    put    ":controller/#{version}/proposals", :action => 'proposal_create', :as => :create_proposal_barclamp
    get    ":controller/#{version}/proposals", :action => 'proposals', :as => :proposals_barclamp
    post   ":controller/#{version}/proposals/commit/:id", :action => 'proposal_commit', :as => :commit_proposal_barclamp
    delete ":controller/#{version}/proposals/dequeue/:id", :action => 'proposal_dequeue', :as => :dequeue_barclamp
    delete ":controller/#{version}/proposals/:id", :action => 'proposal_delete', :as => :delete_proposal_barclamp
    post   ":controller/#{version}/proposals/:id", :action => 'proposal_update', :as => :update_proposal_barclamp
    get    ":controller/#{version}/proposals/:id", :action => 'proposal_show', :as => :proposal_barclamp
    get    ":controller/#{version}/elements", :action => 'elements'
    get    ":controller/#{version}/elements/:id", :action => 'element_info'
    match  ":controller/#{version}/transition/:id", :action => 'transition', :via => [:get, :post]
    get    ":controller/#{version}", :action => 'index', :as => :index_barclamp
    delete ":controller/#{version}/:id", :action => 'delete', :as => :delete_barclamp
    get    ":controller/#{version}/:id", :action => 'show', :as => :show_barclamp
    get    ":controller", :action => 'versions', :as => :versions_barclamp
    post   ":controller/#{version}/:action/:id", :as => :action_barclamp
    get    '/', :controller => 'barclamp', :action => 'barclamp_index', :as => :barclamp_index_barclamp
    get    "modules/#{version}", :controller => 'barclamp', :action => 'modules', :as => :barclamp_modules
            
    # Generic fall through routes
    get    ":barclamp/#{version}/help", :action => 'help', :controller => 'barclamp'
    get    ":barclamp/#{version}/proposals/nodes", :controller => "barclamp", :action=>'nodes'
    put    ":barclamp/#{version}/proposals", :action => 'proposal_create', :controller => 'barclamp'
    get    ":barclamp/#{version}/proposals", :action => 'proposals', :controller => 'barclamp'
    post   ":barclamp/#{version}/proposals/commit/:id", :action => 'proposal_commit', :controller => 'barclamp'
    get    ":barclamp/#{version}/proposals/status(/:id)(.:format)", :controller => 'barclamp', :action => 'proposal_status', :controller => 'barclamp'
    delete ":barclamp/#{version}/proposals/:id", :action => 'proposal_delete', :controller => 'barclamp'
    post   ":barclamp/#{version}/proposals/:id", :action => 'proposal_update', :controller => 'barclamp'
    get    ":barclamp/#{version}/proposals/:id", :action => 'proposal_show', :controller => 'barclamp'
    get    ":barclamp/#{version}/elements", :action => 'elements', :controller => 'barclamp'
    get    ":barclamp/#{version}/elements/:id", :action => 'element_info', :controller => 'barclamp'
    match  ":barclamp/#{version}/transition/:id", :action => 'transition', :via => [:get, :post], :controller => 'barclamp'
    get    ":barclamp/#{version}", :action => 'index', :controller => 'barclamp'
    get    ":barclamp/#{version}/status", :action => 'status', :controller => 'barclamp'
    delete ":barclamp/#{version}/:id", :action => 'delete', :controller => 'barclamp'
    get    ":barclamp/#{version}/:id", :action => 'show', :controller => 'barclamp'
    get    ":barclamp", :action => 'versions', :controller => 'barclamp'
    post   ":barclamp/#{version}/:action/:id", :controller => 'barclamp'

    match "/", :controller => 'barclamp', :action => 'barclamp_index', :via => :get, :as => :barclamp_index_barclamp
        
  end
