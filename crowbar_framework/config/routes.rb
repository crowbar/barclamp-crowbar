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

  # UI 
  resources :jigs
  resources :barclamps
  resources :deployments
  resources :docs
  resources :nodes
  resources :roles
  resources :deployment_roles
  resources :node_roles
  resources :snapshots
  resources :cycles
  resources :groups

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
      resources :attribs do as_routes end
      resources :barclamps do as_routes end
      resources :docs do as_routes end
      resources :groups do as_routes end
      resources :jigs do as_routes end
      resources :navs do as_routes end
      resources :nodes do as_routes end
      resources :roles do as_routes end
      resources :cycles do as_routes end
      resources :role_requires do as_routes end
      resources :deployments do as_routes end
      resources :snapshots do as_routes end
      resources :node_roles do as_routes end
      resources :deployment_roles do as_routes end
    end
  end

  # UI scope - legacy methods
  scope 'support' do
    get 'logs', :controller => 'support', :action => 'logs'
    get 'get_cli', :controller => 'support', :action => 'get_cli'
  end

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
  scope :defaults => {:format => 'json'} do

    constraints(:id => /([a-zA-Z0-9\-\.\_]*)/, :version => /v[1-9]/) do

      # framework resources pattern (not barclamps specific)
      scope 'api' do
        scope 'status' do
          get "nodes(/:id)" => "nodes#status", :as => :nodes_status
          get "deployments(/:id)" => "deployments#status", :as => :deployments_status
        end
        scope ':version' do
          resources :nodes
          resources :barclamps
          resources :deployments
          resources :snapshots
          resources :jigs
          resources :attribs
          resources :roles
          resources :cycles
          resources :node_roles
          resources :deployment_roles
          resources :snapshots
          resources :groups do
            member do
              get 'nodes'
            end
          end
          resources :users do
            post "admin", :controller => "users", :action => "make_admin"
            delete "admin", :controller => "users", :action => "remove_admin"
            post "lock", :controller => "users", :action => "lock"
            delete "lock", :controller => "users", :action => "unlock"
            put "reset_password", :controller => "users", :action => "reset_password"
          end
        end # version
      end # api
    end # id constraints
  end # json

  root :to => "nodes#index"
end
