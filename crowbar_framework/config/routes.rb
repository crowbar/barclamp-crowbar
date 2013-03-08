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
  
  # Install route from each barclamp
  Dir.glob(File.join(File.dirname(__FILE__), 'routes.d', '*.routes')) do |routes_file|
      eval(IO.read(routes_file), binding)
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
    get "/:barclamp/deployment/:id" => "deployment#show", :as=>"deployment"
    get "graph", :controller=>'barclamp', :action=>"graph", :as=>"barclamp_graph"
    get "(/:id)", :controller=>'barclamp', :action=>"index", :as=>"barclamp"
  end

  # UI only routes
  scope 'dashboard' do
    get '/' => 'dashboard#index',           :as => :dashboard
    get 'node/:id' => 'nodes#show',         :as => :dashboard_detail
    get 'families' => 'dashboard#families', :as => :dashboard_families
    get 'list' => 'dashboard#list',         :as => :dashboard_list
    
    constraints(:id=> /([a-zA-Z0-9\-\.\_]*)/) do
      get "dashboard/:id" => 'nodes#index', :as => 'dashboard_detail'
      scope  'node' do
        get  'list' => "nodes#list"
        get  'families' => "nodes#families"
        get  ':id/edit' => "nodes#edit", :as => :edit_node
        post ':id/edit' => "nodes#update", :as => :update_node
        put  ':id/update' => 'nodes#update', :as => :update_node
        get  ':id' => 'nodes#show', :as => 'node'
      end
      scope 'nodes' do
        match 'list' => "nodes#list", :as => :nodes_list
      end
    end
  end
  
  # REVIEW NEEDED!  should this be under the devise_scope??
  put 'reset_password(/:id)', :controller => 'users', :action=>"reset_password", :as=>:reset_password
  get 'edit_password/:id', :controller => 'users', :action=>'edit_password', :constraints => { :id => /.*/ }, :as => :edit_password
  delete 'unlock/:id', :controller => 'users', :action=>'unlock', :constraints => { :id => /.*/ }, :as => :unlock
  post 'lock/:id', :controller => 'users', :action=>'lock', :constraints => { :id => /.*/ }, :as => :lock
  match "manage_users", :controller => 'users', :action => 'index'
  match "delete_users", :controller => 'users', :action => 'delete_users', :as=> :delete_users
                               
  devise_for :users, :path_prefix => 'my'
  
  get    "/users/new(.:format)", :controller => 'users', :action=>'index', :as=> :new_user
  resources :users, :except => :new 
    
    # API routes (must be json and must prefix v2)()
    scope :defaults => {:format=> 'json'} do
      
      constraints(:id => /([a-zA-Z0-9\-\.\_]*)/, :version => /v[1-9]/ ) do

        # framework resources pattern (not barclamps specific)
          scope ':version' do
            scope 'status' do
              get "nodes(/:id)" => "nodes#status",  :as=>:nodes_status
              get "deployments(/:id)" => "deployments#status", :as=>:deployments_status
            end
            
            resources :nodes do 
              resources :attribs
              resources :groups
              match 'transistion'   # these should be limited to put, but being more lax for now
              match 'allocate'   # these should be limited to put, but being more lax for now
            end
            resources :barclamps do
              resources :deployments
            end
            resources :deployments
            resources :snapshots
            resources :jigs 
            #resources :users 
            resources :attrib_types
            resources :attribs
            resources :role_types
            resources :roles
            resources :groups do
              member do
                get 'nodes'
              end
            end
            
            resources :users
            scope 'users' do
                post ":id/admin", :controller => "users", :action => "make_admin"
                delete ":id/admin", :controller => "users", :action => "remove_admin"
                post ":id/lock", :controller => "users", :action => "lock"
                delete ":id/lock", :controller => "users", :action => "unlock"
                put ":id/reset_password", :controller => "users", :action => "reset_password"
            end
          end # version
        # end # api
        
        # Barclamp resource v2 API Pattern
        scope ':barclamp' do
          scope ':version' do
            
            match "template"                => "barclamps#template"
            
            resources :deployments do
              member do  
                put 'commit'
                put 'recall'
              end
            end
            
            resources :snapshots
  
            resources :roles do
              resources :attribs
              resources :nodes
            end
            
            resources :attribs
            
           
            
          end # version scope
        end # barclamp scope
      end # id constraints
    end

  root :to => "dashboard#index"  
  
end
