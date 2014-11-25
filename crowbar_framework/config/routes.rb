#
# Copyright 2011-2013, Dell
# Copyright 2013-2014, SUSE LINUX Products GmbH
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

Rails.application.routes.draw do
  # Install route from each barclamp
  Rails.root.join("config", "routes.d").children.each do |routes|
    eval(routes.read, binding) if routes.extname == ".routes"
  end if Rails.root.join("config", "routes.d").directory?

  # Root route have to be on top of all
  root to: "nodes#index"

  get 'docs', :controller => 'docs', :action => 'index'
  get 'docs/*id', :controller => 'docs', :action => 'show', :as => 'topic_docs'
  get 'docs/:controller/:id', :action=>'docs', :as => 'docs_barclamp'

  # nodes
  resources :nodes, :only => [:index]

  get 'nodes/:name/attribute/*path', :controller => 'nodes', :action => 'attribute',
              :constraints => { :name => /.*/, :path => /.*/ }
  get 'nodes/status(.:format)', :controller => 'nodes', :action => 'status', :as => 'nodes_status'
  get 'nodes/list(.:format)', :controller => 'nodes', :action => 'list', :as => 'nodes_list'
  get 'nodes/unallocated(.:format)', :controller => 'nodes', :action => 'unallocated', :as => 'unallocated_list'
  post 'nodes/bulk(.:format)', :controller => 'nodes', :action => 'bulk', :as => 'bulk_nodes'
  get 'nodes/families', :controller=>'nodes', :action=>'families', :as => 'nodes_families'
  get 'nodes/:id/hit/:req', :controller=>'nodes', :action=>'hit', :constraints => { :id => /.*/ }, :as => 'hit_node'
  get 'nodes/:name/edit', :controller=>'nodes', :action =>'edit', :constraints => { :name => /.*/ }, :as => 'edit_node'
  get 'dashboard', :controller => 'nodes', :action => 'index', :as => 'dashboard'
  get 'dashboard/:name', :controller => 'nodes', :action => 'index', :constraints => { :name => /.*/ }, :as => 'dashboard_detail'
  post 'nodes/groups/1.0/:id/:group', :controller => 'nodes', :action=>'group_change', :constraints => { :id => /.*/ }, :as => 'group_change'
  # this route allows any barclamp to extend the nodes view
  get 'nodes/:controller/1.0', :action => 'nodes', :as => 'nodes_barclamp'
  get 'nodes/:name/update', :controller => 'nodes', :action=>'update', :constraints => { :name => /.*/ }, :as => 'update_node'
  get 'nodes/:name', :controller => 'nodes', :action => 'show', :constraints => { :name => /.*/ }, :as => 'node'

  # this route allows any barclamp to extend the network view
  get 'network/:controller/1.0', :action=>'network', :as => 'network_barclamp'
  # these paths require the network barclamp
  get 'network', :controller => 'network', :action=>'switch', :as => 'network'
  get 'network/switch/:id', :controller => 'network', :action=>'switch', :constraints => { :id => /.*/ }, :defaults => { :id => "default" }, :as => 'switch'
  get 'network/vlan/:id', :controller => 'network', :action=>'vlan', :constraints => { :id => /.*/ }, :defaults => { :id => "default" }, :as => 'vlan'

  # clusters
  get 'clusters',     :controller => 'dashboard', :action => 'clusters', :as => 'clusters'
  get 'active_roles', :controller => 'dashboard', :action => 'active_roles', :as => 'active_roles'

  # deployment queue
  get 'deployment_queue', :controller => 'deploy_queue', :action => 'index', :as => 'deployment_queue'

  #support paths
  get 'utils(.:format)', :controller=>'support', :action=>'index', :as => 'utils'
  get 'utils/files/:id', :controller=>'support', :action=>'destroy', :constraints => { :id => /.*/ }, :as => 'utils_files'
  get 'utils/chef', :controller=>'support', :action=>'export_chef', :as => 'export_chef'
  get 'utils/supportconfig', :controller=>'support', :action=>'export_supportconfig', :as => 'export_supportconfig'
  get 'utils/:controller/1.0/export', :action=>'export', :as => 'utils_export'
  get 'utils/:controller/1.0', :action=>'utils', :as => 'utils_barclamp'
  get 'utils/import/:id', :controller=>'support', :action=>'import', :constraints => { :id => /.*/ }, :as => 'utils_import'
  get 'utils/upload/:id', :controller=>'support', :action=>'upload', :constraints => { :id => /.*/ }, :as => 'utils_upload'
  get 'utils/restart/:id(.:format)', :controller=>'support', :action=>'restart', :as => 'restart'

  # barclamps
  get 'crowbar/:controller/1.0/help', :action => 'help', :as => 'help_barclamp'
  get 'crowbar/:controller/1.0/proposals/nodes', :action =>'nodes', :as => 'barclamp_nodes'
  put 'crowbar/:controller/1.0/proposals', :action => 'proposal_create', :as => 'create_proposal_barclamp'
  get 'crowbar/:controller/1.0/proposals', :action => 'proposals', :as => 'proposals_barclamp'
  post 'crowbar/:controller/1.0/proposals/commit/:id', :action => 'proposal_commit', :as => 'commit_proposal_barclamp'
  get 'crowbar/:controller/1.0/proposals/status/(:id/)(.:format)', :action => 'proposal_status', :as => 'status_proposals_barclamp'
  delete 'crowbar/:controller/1.0/proposals/:id', :action => 'proposal_delete', :as => 'delete_proposal_barclamp'
  delete 'crowbar/:controller/1.0/proposals/dequeue/:id', :action => 'proposal_dequeue', :as => 'dequeue_barclamp'
  post 'crowbar/:controller/1.0/proposals/:id', :action => 'proposal_update', :as => 'update_proposal_barclamp'
  get 'crowbar/:controller/1.0/proposals/:id', :action => 'proposal_show', :as => 'proposal_barclamp'

  get 'crowbar/:controller/1.0/elements', :action => 'elements'
  get 'crowbar/:controller/1.0/elements/:id', :action => 'element_info'
  post 'crowbar/:controller/1.0/transition/:id', :action => 'transition'
  get  'crowbar/:controller/1.0/transition/:id', :action => 'transition'

  get 'crowbar/:controller/1.0', :action => 'index', :as => 'index_barclamp'
  delete 'crowbar/:controller/1.0/:id', :action => 'delete', :constraints => { :id => /.*/ }, :as => 'delete_barclamp'
  get 'crowbar/:controller/1.0/:id', :action => 'show', :constraints => { :id => /.*/ }, :as => 'show_barclamp'
  get 'crowbar/:controller', :action => 'versions', :as => 'versions_barclamp'
  post 'crowbar/:controller/1.0/:action/:id', :constraints => { :id => /.*/ }, :as => 'action_barclamp'
  get 'crowbar', :controller => 'barclamp', :action => 'barclamp_index', :as => 'barclamp_index_barclamp'
  get 'crowbar/modules/1.0', :controller => 'barclamp', :action => 'modules', :as => 'barclamp_modules'

  get 'crowbar/:barclamp/1.0/help', :action => 'help', :controller => 'barclamp'
  get 'crowbar/:barclamp/1.0/proposals/nodes', :controller => "barclamp", :action=>'nodes'
  put 'crowbar/:barclamp/1.0/proposals', :action => 'proposal_create', :controller => 'barclamp'
  get 'crowbar/:barclamp/1.0/proposals', :action => 'proposals', :controller => 'barclamp'
  post 'crowbar/:barclamp/1.0/proposals/commit/:id', :action => 'proposal_commit', :controller => 'barclamp'
  get 'crowbar/:barclamp/1.0/proposals/status(.:format)', :action => 'proposal_status', :controller => 'barclamp'
  delete 'crowbar/:barclamp/1.0/proposals/:id', :action => 'proposal_delete', :controller => 'barclamp'
  post 'crowbar/:barclamp/1.0/proposals/:id', :action => 'proposal_update', :controller => 'barclamp'
  get 'crowbar/:barclamp/1.0/proposals/:id', :action => 'proposal_show', :controller => 'barclamp'
  get 'crowbar/:barclamp/1.0/elements', :action => 'elements', :controller => 'barclamp'
  get 'crowbar/:barclamp/1.0/elements/:id', :action => 'element_info', :controller => 'barclamp'
  post 'crowbar/:barclamp/1.0/transition/:id', :action => 'transition', :controller => 'barclamp'
  get 'crowbar/:barclamp/1.0/transition/:id', :action => 'transition', :controller => 'barclamp'
  get 'crowbar/:barclamp/1.0', :action => 'index', :controller => 'barclamp'
  get 'crowbar/:barclamp/1.0/status', :action => 'status', :controller => 'barclamp'
  delete 'crowbar/:barclamp/1.0/:id', :action => 'delete', :controller => 'barclamp'
  get 'crowbar/:barclamp/1.0/:id', :action => 'show', :controller => 'barclamp'
  get 'crowbar/:barclamp', :action => 'versions', :controller => 'barclamp'
  post 'crowbar/:barclamp/1.0/:action/:id', :controller => 'barclamp'

  match '/:controller/:action/*(:.format)', :via => [:get, :post, :put, :patch, :delete]
end
