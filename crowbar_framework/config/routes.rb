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
  
# DO NOT DELETE OR ALTER THIS LINE - it is for engine mounts

  # ??? what is this ??
  resources :nodes, :only => [:index, :new] do
    get 'status', :on => :collection
  end
  
  # UI scope documentation / help
  scope 'docs' do
    get '/', :controller=>'docs', :action=>'index', :as => "docs"
    get 'topic/:id', :controller=>'docs', :action=>'topic', :as => "docs_topic", :constraints => { :id => /.*/ }
  end

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
    namespace :scaffolds do
      resources :attrib_types do as_routes end
      resources :attribs do as_routes end
      resources :barclamps do as_routes end
      resources :deployments do as_routes end
      resources :docs do as_routes end
      resources :groups do as_routes end
      resources :jig_events do as_routes end
      resources :jig_maps do as_routes end
      resources :jig_runs do as_routes end
      resources :jigs do as_routes end
      resources :navs do as_routes end
      resources :nodes do as_routes end
      resources :os do as_routes end
      resources :os_packages do as_routes end
      resources :role_element_orders do as_routes end
      resources :role_types do as_routes end
      resources :roles do as_routes end
      resources :snapshots do as_routes end
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
   
      scope 'framework' do            
        scope 'status' do
          get "nodes(/:id)" => "nodes#status",  :as=>'nodes_status'
          get "deployments(/:id)" => "deployments#status",  :as=>'deployments_status'
        end
      end
      
      # v2 API Pattern
      scope ':barclamp' do
        scope ':version' do
          constraints(:id => /([a-zA-Z0-9\-\.\_]*)/, :version => /v[1-9]/ ) do
            
            resources :barclamps
            match "template"                => "barclamps#template"
            
            resources :deployments do
              member do  
                put 'commit'
                put 'dequeue'
                put 'propose'
                put 'transistion'
              end
            end
            
            resources :snapshots
  
            resources :roles do
              resources :attribs
              resources :nodes
            end
            resources :role_types
            
            resources :jigs
            resources :attribs
            resources :attrib_types
            
            resources :nodes do
              resources :attribs
              resources :groups
            end
                        
            resources :groups do
              member do
                get 'nodes'
              end
            end
            resources :users

          end
        end
      end
      
      # depricated 2.0 API Pattern
      scope '2.0' do
        constraints(:id => /([a-zA-Z0-9\-\.\_]*)/, :version => /[0-9].[0-9]/ ) do
  
          # MOVE TO IMPI actions
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
                  
        end
      end
    end
    
  end 
  
  root :to => "nodes#index"  
end
