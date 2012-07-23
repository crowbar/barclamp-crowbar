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

  resources :nodes, :only => [:index, :new] do
    get 'status', :on => :collection
  end

  # documentation / help
  scope 'docs' do
    get '/', :controller=>'docs', :action=>'index', :conditions => { :method => :get }, :as => "docs"
    get 'topic/:id', :controller=>'docs', :action=>'topic', :conditions => { :method => :get }, :as => "docs_topic"
    get ':controller/:id', :action=>'docs', :conditions => { :method => :get }, :as => "docs_barclamp"
  end

  get "dashboard", :controller => 'nodes', :action => 'index', :as => 'dashboard'
  get "dashboard/:name", :controller => 'nodes', :action => 'index', :constraints => { :name => /.*/ }, :as => 'dashboard_detail'

  scope 'network' do
    get ':controller/1.0', :action=>'network', :as => :network_barclamp
    get '/', :controller => 'network', :action=>'switch', :as => :network
    get 'switch(/:id)', :controller => 'network', :action=>'switch', :constraints => { :id => /.*/ }, :as => :switch
    get 'vlan(/:id)', :controller => 'network', :action=>'vlan', :constraints => { :id => /.*/ }, :as => :vlan
  end

  scope 'utils' do
    get  '/', :controller=>'support', :action=>'index', :as => :utils
    get  'files/:id', :controller=>'support', :action=>'index', :constraints => { :id => /.*/ }, :as => :utils_files
    get 'chef', :controller=>'support', :action=>'export_chef', :as => :export_chef
    get ':controller/1.0/export', :action=>'export', :as => :utils_export
    get ':controller/1.0', :action=>'utils', :as => :utils_barclamp
    get 'import(/:id)', :controller=>'support', :action=>'import', :constraints => { :id => /.*/ }, :as => :utils_import
    get 'upload/:id', :controller=>'support', :action=>'upload', :constraints => { :id => /.*/ }, :as => :utils_upload
    get 'restart/:id', :controller=>'support', :action=>'restart', :as => :restart
  end
  
  scope 'nodes' do
    constraints(:name => /.*/ ) do
      match ':name/hit/:req' => "nodes#hit", :as => :hit_node
      match ':name/edit' => "nodes#edit", :as => :edit_node
      match ':name' => "nodes#show", :as => :node
      match ':name/update' => 'nodes#update', :as => :update_node
    end
    get 'status(.:format)' => "nodes#status", :as => :nodes_status
    get 'list', :controller => 'nodes', :action => 'list', :as => :nodes_list
    get 'families', :controller=>'nodes', :action=>'families', :as => :nodes_families
    post 'groups/1.0/:id/:group', :controller => 'nodes', :action=>'group_change', :constraints => { :id => /.*/ }, :as => :group_change
    get ':controller/1.0', :action => 'nodes', :as => :nodes_barclamp
  end
  
  scope 'crowbar' do
    version = "1.0"

    get "show/#{version}/:id", :controller => 'barclamp', :action => 'barclamp_show', :as => :barclamp_show_barclamp
    match "roles/#{version}", :controller => 'barclamp', :action => 'barclamp_roles', :via => :get, :as => :barclamp_roles_barclamp
    match "proposals/#{version}", :controller => 'barclamp', :action => 'barclamp_proposals', :via => :get, :as => :barclamp_proposals_barclamp
    match "proposals/#{version}", :controller => 'barclamp', :action => 'proposal_status', :via => :get, :as => :status_proposals_barclamp


    get ":controller/#{version}/help", :action => 'help', :as => :help_barclamp
    get ":controller/#{version}/proposals/nodes", :action=>'nodes', :as => :barclamp_nodes
    put ":controller/#{version}/proposals", :action => 'proposal_create', :as => :create_proposal_barclamp
    get ":controller/#{version}/proposals", :action => 'proposals', :as => :proposals_barclamp
    post ":controller/#{version}/proposals/commit/:id", :action => 'proposal_commit', :as => :commit_proposal_barclamp
    get "#{version}/proposals/status.:format", :controller=>'barclamp', :action => 'proposal_status', :as => :status_proposals
    delete ":controller/#{version}/proposals/:id", :action => 'proposal_delete', :as => :delete_proposal_barclamp
    delete ":controller/#{version}/proposals/dequeue/:id", :action => 'proposal_dequeue', :as => :dequeue_barclamp
    post ":controller/#{version}/proposals/:id", :action => 'proposal_update', :as => :update_proposal_barclamp
    get ":controller/#{version}/proposals/:id", :action => 'proposal_show', :as => :proposal_barclamp
    get ":controller/#{version}/elements", :action => 'elements'
    get ":controller/#{version}/elements/:id", :action => 'element_info'
    match ":controller/#{version}/transition/:id", :action => 'transition', :via => [:get, :post]
    get ":controller/#{version}", :action => 'index', :as => :index_barclamp
    delete ":controller/#{version}/:id", :action => 'delete', :as => :delete_barclamp
    get ":controller/#{version}/:id", :action => 'show', :as => :show_barclamp
    get ":controller", :action => 'versions', :as => :versions_barclamp
    post ":controller/#{version}/:action/:id", :as => :action_barclamp
    get  '/', :controller => 'barclamp', :action => 'barclamp_index', :as => :barclamp_index_barclamp
    get "modules/#{version}", :controller => 'barclamp', :action => 'modules', :as => :barclamp_modules

            
    # Generic fall through routes
    get ":barclamp/#{version}/help", :action => 'help', :controller => 'barclamp'
    get ':barclamp/1.0/proposals/nodes', :controller => "barclamp", :action=>'nodes'
    put ":barclamp/#{version}/proposals", :action => 'proposal_create', :controller => 'barclamp'
    get ":barclamp/#{version}/proposals", :action => 'proposals', :controller => 'barclamp'
    post ":barclamp/#{version}/proposals/commit/:id", :action => 'proposal_commit', :controller => 'barclamp'
    get ":barclamp/#{version}/proposals/status.:format", :controller => 'barclamp', :action => 'proposal_status', :controller => 'barclamp'
    delete ":barclamp/#{version}/proposals/:id", :action => 'proposal_delete', :controller => 'barclamp'
    post ":barclamp/#{version}/proposals/:id", :action => 'proposal_update', :controller => 'barclamp'
    get ":barclamp/#{version}/proposals/:id", :action => 'proposal_show', :controller => 'barclamp'
    get ":barclamp/#{version}/elements", :action => 'elements', :controller => 'barclamp'
    get ":barclamp/#{version}/elements/:id", :action => 'element_info', :controller => 'barclamp'
    match ":barclamp/#{version}/transition/:id", :action => 'transition', :via => [:get, :post], :controller => 'barclamp'
    get ":barclamp/#{version}", :action => 'index', :controller => 'barclamp'
    get ":barclamp/#{version}/status", :action => 'status', :controller => 'barclamp'
    delete ":barclamp/#{version}/:id", :action => 'delete', :controller => 'barclamp'
    get ":barclamp/#{version}/:id", :action => 'show', :controller => 'barclamp'
    get ":barclamp", :action => 'versions', :controller => 'barclamp'
    post ":barclamp/#{version}/:action/:id", :controller => 'barclamp'

    match "/", :controller => 'barclamp', :action => 'barclamp_index', :via => :get, :as => :barclamp_index_barclamp
  end

  root :to => "nodes#index"  
end