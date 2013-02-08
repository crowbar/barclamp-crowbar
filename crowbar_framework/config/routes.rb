# Copyright 2012, Dell 
# 
# Licensed under the Apache License, Version 2.0 (the "License"); 
# you may not use this file except in compliance with the License. 
# You may obtain a copy of the License at 
# 
#  http://www.apache.org/licenses/LICENSE-2.0 
# 
# Unless required by applicable law or agreed to in writing, software 
# distributed under the License is distributed on an "AS IS" BASIS, 
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
# See the License for the specific language governing permissions and 
# limitations under the License. 
# 
Crowbar::Application.routes.draw do

  namespace :scaffolds do
    resources :attribs do as_routes end
    resources :barclamps do as_routes end
    resources :groups do as_routes end
    resources :roles do as_routes end
    resources :nodes do as_routes end
    resources :attrib_instances do as_routes end
    resources :proposals do as_routes end
    resources :proposal_configs do as_routes end
    resources :docs do as_routes end
    resources :navs do as_routes end
    resources :groups do as_routes end  
    resources :interfaces do as_routes end
    resources :networks do as_routes end
    resources :ip_addresses do as_routes end
    resources :jig_attributes do as_routes end
    resources :os do as_routes end
    resources :os_packages do as_routes end
    resources :proposal_queues do as_routes end
    resources :proposal_queue_items do as_routes end
    resources :role_element_orders do as_routes end
  end

# DO NOT DELETE OR ALTER THIS LINE - it is for engine mounts

  resources :nodes, :only => [:index, :new] do
    get 'status', :on => :collection
  end


  # documentation / help
  scope 'docs' do
    get '/', :controller=>'docs', :action=>'index', :as => "docs"
    get 'topic/:id', :controller=>'docs', :action=>'topic', :as => "docs_topic", :constraints => { :id => /.*/ }
    get ':controller/:id', :action=>'docs', :as => "docs_barclamp"
  end

  scope 'network' do
    version = "2.0"
    get '/', :controller => 'network', :action=>'switch', :as => :network
    get 'switch(/:id)', :controller => 'network', :action=>'switch', :constraints => { :id => /.*/ }, :as => :switch
    get 'vlan(/:id)', :controller => 'network', :action=>'vlan', :constraints => { :id => /.*/ }, :as => :vlan
    get ":controller/#{version}", :action=>'network', :as => :network_barclamp
  end

  scope 'utils' do
    version = "2.0"
    get '/', :controller=>'support', :action=>'index', :as => :utils
    get 'i18n/:id', :controller=>'support', :action=>'i18n', :constraints => { :id => /.*/ }, :as => :utils_i18n
    get 'marker/:id', :controller=>'support', :action=>'marker', :constraints => { :id => /.*/ }, :as => :utils_marker
    get 'files/:id', :controller=>'support', :action=>'index', :constraints => { :id => /.*/ }, :as => :utils_files
    get 'import(/:id)', :controller=>'support', :action=>'import', :constraints => { :id => /.*/ }, :as => :utils_import
    get 'upload/:id', :controller=>'support', :action=>'upload', :constraints => { :id => /.*/ }, :as => :utils_upload
    get 'restart/:id', :controller=>'support', :action=>'restart', :constraints => { :id => /.*/ }, :as => :restart
    get ":controller/#{version}/export", :action=>'export', :as => :utils_export
    get ":controller/#{version}", :action=>'utils', :as => :utils_barclamp
  end

  scope 'support' do
    get 'logs', :controller => 'support', :action => 'logs'
    get 'get_cli', :controller => 'support', :action => 'get_cli'
  end

  # Barclamp UI routes (overlays that can be used generically by barclamps to create custom views)
  # The pattern is /barclamp/[your barclamp]/[method]
  scope 'barclamp' do
    get "graph", :controller=>'barclamp', :action=>"graph", :as=>"barclamp_graph"
    constraints(:id => /([a-zA-Z0-9\-\.\_]*)/ ) do
      get ":controller/network(/:id)", :action=>"network", :as=>"barclamp_network"
      get ":controller/node(/:id)", :action=>"node", :as=>"barclamp_node"
      get ":controller/util(/:id)", :action=>"util", :as=>"barclamp_util"
    end
  end

  # UI only routes
  scope :defaults => {:format=> 'html'} do
    get "dashboard", :controller => 'nodes', :action => 'index', :as => 'dashboard'
    constraints(:id=> /([a-zA-Z0-9\-\.\_]*)/) do
      get "dashboard/:id" => 'nodes#index', :as => 'dashboard_detail'
      scope  'node' do
        get  'list' => "nodes#list", :as => :nodes_list
        get  'families' => "nodes#families", :as => :nodes_families
        get  ':id/edit' => "nodes#edit", :as => :edit_node
        post ':id/edit' => "nodes#update", :as => :update_node
        put  ':id/update' => 'nodes#update', :as => :update_node
        get  ':id' => 'nodes#show', :as => 'node'
        # barclamp UI extension
        match ':controller(/:id)', :action=>'nodes', :as=> :nodes_barclamp
      end
      scope 'nodes' do
        post 'list' => "nodes#list", :as => :nodes_list
        get  'list' => "nodes#list", :as => :nodes_list
      end
    end
  end
  
  

  # Digest Auth 
  get 'digest' => 'digest#index' 
  
     
  put 'reset_password(/:id)', :controller => 'users', :action=>"reset_password", :as=>:reset_password
  get 'edit_password/:id', :controller => 'users', :action=>'edit_password', :constraints => { :id => /.*/ }, :as => :edit_password
  delete 'unlock_user/:id', :controller => 'users', :action=>'unlock_user', :constraints => { :id => /.*/ }, :as => :unlock_user
  post 'lock_user/:id', :controller => 'users', :action=>'lock_user', :constraints => { :id => /.*/ }, :as => :lock_user
  match "manage_users", :controller => 'users', :action => 'index'
  match "delete_users", :controller => 'users', :action => 'delete_users', :as=> :delete_users
                               
  devise_for :users, :path_prefix => 'my'
  
  get    "/users/new(.:format)", :controller => 'users', :action=>'index', :as=> :new_user
  resources :users, :except => :new 
     
  devise_scope :user do
  
  # API routes (must be json and must prefix 2.0)
  scope :defaults => {:format=> 'json'} do
    # 2.0 API Pattern
    scope '2.0' do
      constraints(:id => /([a-zA-Z0-9\-\.\_]*)/, :version => /[0-9].[0-9]/ ) do

        # status operations
        scope 'status' do
          get "node(/:id)" => 'nodes#status', :as=>'node_status'
        end

        # actions
        get "node/:id/hit/:req" => "nodes#hit", :as => :hit_node # MOVE TO GENERIC - IPMI BARCLAMP??
                
        scope 'crowbar' do    # MOVE TO GENERIC!
          scope '2.0' do      # MOVE TO GENERIC!
            # TODO: TEMPORARY UNTIL WE FIX THE ROUTE MODEL
            match "barclamp(/:id)", :controller=>'crowbar', :action=>'barclamp_temp', :version=>'2.0'
            # group + node CRUD operations
            match  "group/:id/node/(:node)" => 'groups#node_action',  :constraints => { :node => /([a-zA-Z0-9\-\.\_]*)/ }
            get    "network/networks", :controller => 'network', :action=>'networks'     # MOVE TO GENERIC!
            get    "network/networks/:id", :controller => 'network', :action=>'network_show'     # MOVE TO GENERIC!
            post   "network/networks", :controller => 'network', :action=>'network_create'     # MOVE TO GENERIC!
            put    "network/networks/:id", :controller => 'network', :action=>'network_update'     # MOVE TO GENERIC!
            delete "network/networks/:id", :controller => 'network', :action=>'network_delete'     # MOVE TO GENERIC!
            post   "network/networks/:id/allocate_ip", :controller => 'network', :action=>'network_allocate_ip'
            delete "network/networks/:id/deallocate_ip/:network_id/:node_id", :controller => 'network', :action=>'network_deallocate_ip'
            post   "network/networks/:id/enable_interface", :controller => 'network', :action=>'network_enable_interface'

            # basic list operations 
            get "node", :controller=>'nodes', :action=>'index'     # MOVE TO GENERIC!
            get "group", :controller=>'groups', :action=>'index'     # MOVE TO GENERIC!
            #get ":action", :controller=>'crowbar'
            # basic CRUD operations
            # (replace w/ generic)
            match "/node/:id/:target(/:target_id)" , :controller=>'crowbar', :action=>'node', :version=>'2.0'
            resources :node, :controller=>'nodes'     # MOVE TO GENERIC!
            resources :group, :controller=>'groups'     # MOVE TO GENERIC!
            
           
            # these all need to be updated.
            scope 'users' do
              get :controller => "users", :action => "users"
              get ":id", :controller => "users", :action => "user_show"
              post :controller => "users", :action => "user_create"
              put ":id", :controller => "users", :action => "user_update"
              delete ":id", :controller => "users", :action => "user_delete"
              post ":id/admin", :controller => "users", :action => "user_make_admin"
              delete ":id/admin", :controller => "users", :action => "user_remove_admin"
              post ":id/lock", :controller => "users", :action => "user_lock"
              delete ":id/lock", :controller => "users", :action => "user_unlock"
              put ":id/reset_password", :controller => "users", :action => "user_reset_password"
            end
          end
        end
        
        # generic barclamp matcher
        match ":controller/:version/:action/:id/:target/:target_id", :as => :barclamp_action_target
        match ":controller/:version/:action(/:id)", :as => :barclamp_action
        match ":controller(/:version)", :action=> 'catalog'
                
      end
    end
  end
 

    
  end 

 
  scope 'proposal' do
    version = "2.0"
    get    "status/#{version}(/:id)(.:format)", :controller=>'proposals', :action => 'status', :constraints => { :id => /.*/ }, :as=>:proposal_status
  end
  
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

  root :to => "nodes#index"  
end
