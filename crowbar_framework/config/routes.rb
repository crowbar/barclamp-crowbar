# Copyright 2013, Dell 
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
    resources :docs do as_routes end
    resources :navs do as_routes end
    resources :jig_attributes do as_routes end
    resources :os do as_routes end
    resources :os_packages do as_routes end
    resources :proposal_queues do as_routes end
    resources :proposal_queue_items do as_routes end
    resources :role_element_orders do as_routes end
    resources :node_attribute_filters do as_routes end
    # Network scaffolds (these should move to the network barclamp)
    resources :allocated_ip_addresses do as_routes end
    resources :bmc_interfaces do as_routes end
    resources :bonds do as_routes end
    resources :bus_maps do as_routes end
    resources :buses do as_routes end
    resources :conduit_actions do as_routes end
    resources :conduit_filters do as_routes end
    resources :conduit_rules do as_routes end
    resources :conduits do as_routes end
    resources :create_bonds do as_routes end
    resources :create_vlans do as_routes end
    resources :interface_maps do as_routes end
    resources :interface_selectors do as_routes end
    resources :interfaces do as_routes end
    resources :ip_addresses do as_routes end
    resources :ip_ranges do as_routes end
    resources :network_mode_filters do as_routes end
    resources :networks do as_routes end
    resources :node_attribute_filters do as_routes end
    resources :physical_interfaces do as_routes end
    resources :routers do as_routes end
    resources :select_by_indices do as_routes end
    resources :select_by_speeds do as_routes end
    resources :vlan_interfaces do as_routes end
    resources :vlans do as_routes end
  end

# DO NOT DELETE OR ALTER THIS LINE - it is for engine mounts

  resources :nodes, :only => [:index, :new] do
    get 'status', :on => :collection
  end
  
  # UI scope documentation / help
  scope 'docs' do
    get '/', :controller=>'docs', :action=>'index', :as => "docs"
    get 'topic/:id', :controller=>'docs', :action=>'topic', :as => "docs_topic", :constraints => { :id => /.*/ }
  end

  # CB1 - should move to network barclamp!
  #scope 'network' do
    #version = "2.0"
    #resources :networks, :conduits
    #get '/', :controller => 'networks', :action=>'switch', :as => :network
    #get 'switch(/:id)', :controller => 'networks', :action=>'switch', :constraints => { :id => /.*/ }, :as => :switch
    #get 'vlan(/:id)', :controller => 'networks', :action=>'vlan', :constraints => { :id => /.*/ }, :as => :vlan
  #end

  # UI scope
  scope 'utils' do
    constraints(:id => /.*/ ) do
      get '/'             => 'support#index', :as => :utils
      get 'i18n/:id'      => 'support#i18n', :as => :utils_i18n
      get 'marker/:id'    => 'support#marker', :as => :utils_marker
      get 'files/:id'     => 'support#index', :as => :utils_files
      get 'import(/:id)'  => 'support#import', :as => :utils_import
      get 'upload/:id'    => 'support#upload', :as => :utils_upload
      get 'restart/:id'   => 'support#restart', :as => :restart
      get 'digest'        => "support#digest"
    end
  end

  # UI scope - legacy methods
  scope 'support' do
    get 'logs', :controller => 'support', :action => 'logs'
    get 'get_cli', :controller => 'support', :action => 'get_cli'
  end

  # Barclamp UI routes (overlays that can be used generically by barclamps to create custom views)
  # The pattern is /barclamp/[your barclamp]/[method]
  scope 'barclamp' do
    get "graph", :controller=>'barclamp', :action=>"graph", :as=>"barclamp_graph"
    # legacy...likey to be refactored
    get "modules", :controller=>'barclamp', :action=>"modules", :as=>"barclamp_modules"
    get "/", :controller=>'barclamp', :action=>"modules", :as=>"index_barclamp"
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
      end
      scope 'nodes' do
        post 'list' => "nodes#list", :as => :nodes_list
        get  'list' => "nodes#list", :as => :nodes_list
      end
    end
  end
     
  # REVIEW NEEDED!  should this be under the devise_scope??
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
    
    # API routes (must be json and must prefix 2.0)()
    scope :defaults => {:format=> 'json'} do
      # v2 API Pattern
      scope ':barclamp' do
        scope ':version' do
          constraints(:id => /([a-zA-Z0-9\-\.\_]*)/, :version => /v[1-9]/ ) do
            
            resources :barclamps
            match "template"                => "barclamps#template"
            
            resources :configs,  :controller=>"barclamp_configs"
            match "configs/:id/commit"      => "barclamp_configs#commit",     :as=>'configs_commit'
            match "configs/:id/dequeue"     => "barclamp_configs#dequeue",    :as=>'configs_dequeue'
            match "configs/:id/propose"     => "barclamp_configs#propose",    :as=>'configs_propose'
            match "configs/:id/transistion" => "barclamp_configs#transition", :as=>'configs_transistion'
            
            resources :instances,:controller=>"barclamp_instances"
  
            resources :roles,    :controller=>"barclamp_roles"
            match "roles/:id/attribs"       => "barclamp_roles#attribs",  :as=>'roles_attribs'
            match "roles/:id/nodes"         => "barclamp_roles#nodes", :as=>'roles_nodes'
            
            resources :jigs
            resources :attribs
            
            resources :nodes
            match "nodes/:id/attribs(/:attrib)" => "nodes#attribs", :as=>'nodes_attribs'
          end
        end
      end
      # depricated 2.0 API Pattern
      scope '2.0' do
        constraints(:id => /([a-zA-Z0-9\-\.\_]*)/, :version => /[0-9].[0-9]/ ) do
  
          # status operations
          scope 'status' do
            get "node(/:id)" => 'nodes#status', :as=>'node_status'
          end
  
          # actions
          get "node/:id/hit/:req" => "nodes\#hit", :as => :hit_node # MOVE TO GENERIC - IPMI BARCLAMP??
                  
          scope 'crowbar' do    # MOVE TO GENERIC!
            scope '2.0' do      # MOVE TO GENERIC!
              # group + node CRUD operations
              match  "group/:id/node/(:node)" => 'groups#node_action',  :constraints => { :node => /([a-zA-Z0-9\-\.\_]*)/ }
    

              get "group", :controller=>'groups', :action=>'index'     # MOVE TO GENERIC!
              #get ":action", :controller=>'crowbar'
              # basic CRUD operations
              # (replace w/ generic)
              match "/node/:id/:target(/:target_id)" , :controller=>'crowbar', :action=>'node', :version=>'2.0'
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
  
  root :to => "nodes#index"  
end
