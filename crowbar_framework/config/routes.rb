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

ActionController::Routing::Routes.draw do |map|
  # Install route from each barclamp
  Dir.glob(File.join(File.dirname(__FILE__), 'routes.d', '*.routes')) do |routes_file|
      eval(IO.read(routes_file), binding)
  end

  map.root :controller => "nodes", :action=>'index'

  map.docs 'docs', :controller => 'docs', :action => 'index', :conditions => { :method => :get }
  map.topic_docs 'docs/*id', :controller => 'docs', :action => 'show', :conditions => { :method => :get }

  map.docs_barclamp 'docs/:controller/:id', :action=>'docs', :conditions => { :method => :get }

  # nodes
  map.resources :nodes, :only => [:index]
  map.connect 'nodes/:name/attribute/*path', :controller => 'nodes', :action => 'attribute',
              :requirements => { :name => /.*/, :path => /.*/ }, :conditions => { :method => :get }
  map.nodes_status 'nodes/status.:format', :controller => 'nodes', :action => 'status', :conditions => { :method => :get }
  map.nodes_list 'nodes/list.:format', :controller => 'nodes', :action => 'list', :conditions => { :method => :get }
  map.unallocated_list 'nodes/unallocated.:format', :controller => 'nodes', :action => 'unallocated', :conditions => { :method => :get }
  map.bulk_nodes 'nodes/bulk.:format', :controller => 'nodes', :action => 'bulk', :conditions => { :method => :post }
  map.nodes_families 'nodes/families', :controller=>'nodes', :action=>'families'
  map.hit_node 'nodes/:id/hit/:req', :controller=>'nodes', :action=>'hit', :requirements => { :id => /.*/ }
  map.edit_node 'nodes/:name/edit', :controller=>'nodes', :action =>'edit', :requirements => { :name => /.*/ }
  map.dashboard 'dashboard', :controller => 'nodes', :action => 'index'
  map.dashboard_detail 'dashboard/:name', :controller => 'nodes', :action => 'index', :requirements => { :name => /.*/ }
  map.group_change 'nodes/groups/1.0/:id/:group', :controller => 'nodes', :action=>'group_change', :conditions => { :method => :post }, :requirements => { :id => /.*/ }
  # this route allows any barclamp to extend the nodes view
  map.nodes_barclamp 'nodes/:controller/1.0', :action => 'nodes'  
  map.update_node 'nodes/:name/update', :controller => 'nodes', :action=>'update', :requirements => { :name => /.*/ }
  map.node 'nodes/:name', :controller => 'nodes', :action => 'show', :requirements => { :name => /.*/ }
  
  # this route allows any barclamp to extend the network view
  map.network_barclamp 'network/:controller/1.0', :action=>'network'
  # these paths require the network barclamp 
  map.network 'network', :controller => 'network', :action=>'switch'
  map.switch 'network/switch/:id', :controller => 'network', :action=>'switch', :requirements => { :id => /.*/ }
  map.vlan 'network/vlan/:id', :controller => 'network', :action=>'vlan', :requirements => { :id => /.*/ }

  # clusters
  map.clusters  'clusters',     :controller => 'dashboard', :action => 'clusters'
  map.active_roles 'active_roles', :controller => 'dashboard', :action => 'active_roles'
  
  #support paths


  map.utils 'utils.:format', :controller=>'support', :action=>'index'
  map.utils_files 'utils/files/:id', :controller=>'support', :action=>'destroy', :requirements => { :id => /.*/ }
  map.export_chef 'utils/chef', :controller=>'support', :action=>'export_chef'
  map.export_supportconfig 'utils/supportconfig', :controller=>'support', :action=>'export_supportconfig'
  map.utils_export 'utils/:controller/1.0/export', :action=>'export'
  map.utils_barclamp 'utils/:controller/1.0', :action=>'utils'
  map.utils_import 'utils/import/:id', :controller=>'support', :action=>'import', :requirements => { :id => /.*/ }
  map.utils_upload 'utils/upload/:id', :controller=>'support', :action=>'upload', :requirements => { :id => /.*/ }
  map.restart 'utils/restart/:id.:format', :controller=>'support', :action=>'restart'
  
  # barclamps
  map.help_barclamp             'crowbar/:controller/1.0/help', :action => 'help', :conditions => { :method => :get }
  map.barclamp_nodes            'crowbar/:controller/1.0/proposals/nodes', :action=>'nodes'
  map.create_proposal_barclamp  'crowbar/:controller/1.0/proposals', :action => 'proposal_create', :conditions => { :method => :put }
  map.proposals_barclamp        'crowbar/:controller/1.0/proposals', :action => 'proposals', :conditions => { :method => :get }
  map.commit_proposal_barclamp  'crowbar/:controller/1.0/proposals/commit/:id', :action => 'proposal_commit', :conditions => { :method => :post }
  map.status_proposals_barclamp 'crowbar/:controller/1.0/proposals/status/:id/.:format', :action => 'proposal_status', :conditions => { :method => :get }
  map.delete_proposal_barclamp  'crowbar/:controller/1.0/proposals/:id', :action => 'proposal_delete', :conditions => { :method => :delete }
  map.dequeue_barclamp  'crowbar/:controller/1.0/proposals/dequeue/:id', :action => 'proposal_dequeue', :conditions => { :method => :delete }
  map.update_proposal_barclamp  'crowbar/:controller/1.0/proposals/:id', :action => 'proposal_update', :conditions => { :method => :post }
  map.proposal_barclamp         'crowbar/:controller/1.0/proposals/:id', :action => 'proposal_show', :conditions => { :method => :get }
  map.connect 'crowbar/:controller/1.0/elements', :action => 'elements', :conditions => { :method => :get }
  map.connect 'crowbar/:controller/1.0/elements/:id', :action => 'element_info', :conditions => { :method => :get }
  map.connect 'crowbar/:controller/1.0/transition/:id', :action => 'transition', :conditions => { :method => :post }
  map.connect 'crowbar/:controller/1.0/transition/:id', :action => 'transition', :conditions => { :method => :get }
  map.index_barclamp            'crowbar/:controller/1.0', :action => 'index', :conditions => { :method => :get }
  map.delete_barclamp           'crowbar/:controller/1.0/:id', :action => 'delete', :conditions => { :method => :delete }, :requirements => { :id => /.*/ }
  map.show_barclamp             'crowbar/:controller/1.0/:id', :action => 'show', :conditions => { :method => :get }, :requirements => { :id => /.*/ }
  map.versions_barclamp         'crowbar/:controller', :action => 'versions', :conditions => { :method => :get }
  map.action_barclamp           'crowbar/:controller/1.0/:action/:id', :conditions => { :method => :post }, :requirements => { :id => /.*/ }
  map.barclamp_index_barclamp   'crowbar', :controller => 'barclamp', :action => 'barclamp_index', :conditions => { :method => :get }
  map.barclamp_modules 'crowbar/modules/1.0', :controller => 'barclamp', :action => 'modules', :conditions => { :method => :get }

  map.connect 'crowbar/:barclamp/1.0/help', :action => 'help', :conditions => { :method => :get }, :controller => 'barclamp'
  map.connect 'crowbar/:barclamp/1.0/proposals/nodes', :controller => "barclamp", :action=>'nodes'
  map.connect 'crowbar/:barclamp/1.0/proposals', :action => 'proposal_create', :conditions => { :method => :put }, :controller => 'barclamp'
  map.connect 'crowbar/:barclamp/1.0/proposals', :action => 'proposals', :conditions => { :method => :get }, :controller => 'barclamp'
  map.connect 'crowbar/:barclamp/1.0/proposals/commit/:id', :action => 'proposal_commit', :conditions => { :method => :post }, :controller => 'barclamp'
  map.connect 'crowbar/:barclamp/1.0/proposals/status.:format', :action => 'proposal_status', :conditions => { :method => :get }, :controller => 'barclamp'
  map.connect 'crowbar/:barclamp/1.0/proposals/:id', :action => 'proposal_delete', :conditions => { :method => :delete }, :controller => 'barclamp'
  map.connect 'crowbar/:barclamp/1.0/proposals/:id', :action => 'proposal_update', :conditions => { :method => :post }, :controller => 'barclamp'
  map.connect 'crowbar/:barclamp/1.0/proposals/:id', :action => 'proposal_show', :conditions => { :method => :get }, :controller => 'barclamp'
  map.connect 'crowbar/:barclamp/1.0/elements', :action => 'elements', :conditions => { :method => :get }, :controller => 'barclamp'
  map.connect 'crowbar/:barclamp/1.0/elements/:id', :action => 'element_info', :conditions => { :method => :get }, :controller => 'barclamp'
  map.connect 'crowbar/:barclamp/1.0/transition/:id', :action => 'transition', :conditions => { :method => :post }, :controller => 'barclamp'
  map.connect 'crowbar/:barclamp/1.0/transition/:id', :action => 'transition', :conditions => { :method => :get }, :controller => 'barclamp'
  map.connect 'crowbar/:barclamp/1.0', :action => 'index', :conditions => { :method => :get }, :controller => 'barclamp'
  map.connect 'crowbar/:barclamp/1.0/status', :action => 'status', :conditions => { :method => :get }, :controller => 'barclamp'
  map.connect 'crowbar/:barclamp/1.0/:id', :action => 'delete', :conditions => { :method => :delete }, :controller => 'barclamp'
  map.connect 'crowbar/:barclamp/1.0/:id', :action => 'show', :conditions => { :method => :get }, :controller => 'barclamp'
  map.connect 'crowbar/:barclamp', :action => 'versions', :conditions => { :method => :get }, :controller => 'barclamp'
  map.connect 'crowbar/:barclamp/1.0/:action/:id', :conditions => { :method => :post }, :controller => 'barclamp'

  map.connect ':controller/:action/:id'
  map.connect ':controller/:action/:id.:format'
end
