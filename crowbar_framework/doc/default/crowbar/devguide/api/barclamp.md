### Barclamp APIs

#### SAMPLE - REFERENCE ONLY

**Input:**

<table border=1>
<tr><th> Verb </th><th> URL </th><th> Options </th><th> Returns </th><th> Comments </th></tr>
<tr><td> GET </td><td> /barclamp/status/2.0  </td><td> none  </td><td> All proposals  </td><td> Used by Barclamp List  </td></tr> 
<tr><td> GET   </td><td> /barclamp/status/2.0/[id]  </td><td> id is the proposal ID.  </td><td> Used by Proposal Views  </td></tr>
</table>

**Output:**


    {
      "i18n":{"unknown":"Unknown, requesting status...","ready":"Active"},
      "proposals":{"5":"ready","11":"ready" },
      "count":14,
      "error":""
    }

Details:

* Format - json
* i18n - ?

#### PATTERN

  scope '2.0' do
    version = "2.0"

    get    ":controller/#{version}/help", :action => 'help', :as => :help_barclamp    # returns barclamp's actions
    get    ":controller/#{version}/elements", :action => 'elements'                   # Returns barclamp's roles
    post   ":controller/#{version}/:id/instance", :action => 'proposal_create', :as => :create_proposal_barclamp  # Create an instance of a configuration
    get    ":controller/#{version}/:id/instance", :action => 'proposals', :as => :proposals_barclamp              # Show an instance of a configuration
    put    ":controller/#{version}/:id/instance/:iid/commit", :action => 'proposal_commit', :as => :commit_proposal_barclamp # apply/commit an instance of the c
onfiguration
    delete ":controller/#{version}/:id/instance/:iid/dequeue", :action => 'proposal_dequeue', :as => :dequeue_barclamp # remove the instance from the queue
    delete ":controller/#{version}/:id/instance/:iid/deactivate", :action => 'proposal_dequeue', :as => :dequeue_barclamp # uncommit an instance of the configur ation
    get    ":controller/#{version}/:id/instance/active/status", :action => 'proposal_show', :as => :proposal_barclamp # show an instance of the configuration
    get    ":controller/#{version}/:id/instance/:iid/status", :action => 'proposal_show', :as => :proposal_barclamp # show an instance of the configuration
    post   ":controller/#{version}/:id/instance/:iid/:action", :as => :action_barclamp # general action rule for an instance of the configuration
    delete ":controller/#{version}/:id/instance/:iid", :action => 'proposal_delete', :as => :delete_proposal_barclamp # delete the instance of the configuration
    put    ":controller/#{version}/:id/instance/:iid", :action => 'proposal_update', :as => :update_proposal_barclamp # update the instance of the configuration
    get    ":controller/#{version}/:id/instance/:iid", :action => 'proposal_show', :as => :proposal_barclamp # show an instance of the configuration
    get    ":controller/#{version}/:id/elements", :action => 'element_info'  # return recommendations of tthe roles of the configuration
    match  ":controller/#{version}/:id/transition", :action => 'transition', :via => [:get, :post]  # Transition function for this configuration
    post   ":controller/#{version}/:id/:action", :as => :action_barclamp # general action rule for the configuration
    get    ":controller/#{version}", :action => 'index', :as => :index_barclamp
    delete ":controller/#{version}/:id", :action => 'delete', :as => :delete_barclamp
    get    ":controller/#{version}/:id", :action => 'show', :as => :show_barclamp
    get    ":controller", :action => 'versions', :as => :versions_barclamp

    # Barclamp functions
    get    '/', :controller => 'barclamp', :action => 'barclamp_index', :as => :barclamp_index_barclamp
    get    "modules/#{version}", :controller => 'barclamp', :action => 'modules', :as => :barclamp_modules

    match "/", :controller => 'barclamp', :action => 'barclamp_index', :via => :get, :as => :barclamp_index_barclamp

  end
  end
