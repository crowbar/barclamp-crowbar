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
class CreateNavs < ActiveRecord::Migration
  def self.up

    create_table(:navs, :primary_key=>'name', :id=>false) do |t|
      t.string :item
      t.string :parent_item, :default=>'root'
      t.string :name
      t.string :description
      t.string :path
      t.integer :order, :length=>5, :default => 9999
      t.boolean :development, :default=>false
      t.timestamps
    end
    
    Nav.find_or_create_by_item :item=>'root', :name=>'nav.root', :description=>'nav.root_description', :path=>"root_path", :order=>0, :development=>true
  
    # dashboard
    Nav.find_or_create_by_item :item=>'nodes', :parent_item=>'root', :name=>'nav.nodes', :description=>'nav.nodes_description', :path=>"dashboard_path", :order=>1000
      Nav.find_or_create_by_item :item=>'dashboard', :parent_item=>'nodes', :name=>'nav.dashboard', :description=>'nav.dashboard_description', :path=>"dashboard_path", :order=>100
      Nav.find_or_create_by_item :item=>'bulkedit', :parent_item=>'nodes', :name=>'nav.list', :description=>'nav.list_description', :path=>"nodes_list_path(:allocated=>'yes')", :order=>200
      Nav.find_or_create_by_item :item=>'families', :parent_item=>'nodes', :name=>'nav.families', :description=>'nav.families_description', :path=>"nodes_families_path", :order=>300, :development=>true
  
    # network
    Nav.find_or_create_by_item :item=>'network', :parent_item=>'root', :name=>'nav.network', :description=>'nav.network_description', :path=>"network_path", :order=>2000, :development=>true

    # barclamps
    Nav.find_or_create_by_item :item=>'barclamps', :parent_item=>'root', :name=>'nav.barclamps', :description=>'nav.barclamps_description', :path=>"barclamp_modules_path", :order=>3000, :development=>true
      Nav.find_or_create_by_item :item=>'all_bc', :parent_item=>'barclamps', :name=>'nav.all_bc', :description=>'nav.all_bc_description', :path=>"barclamp_modules_path", :order=>100
      Nav.find_or_create_by_item :item=>'crowbar', :parent_item=>'barclamps', :name=>'nav.crowbar_bc', :description=>'nav.crowbar_bc_description', :path=>"index_barclamp_path(:controller=>'crowbar')", :order=>200
      Nav.find_or_create_by_item :item=>'barclamp_graph', :parent_item=>'barclamps', :name=>'nav.barclamp_graph', :description=>'nav.barclamp_graph_description', :path=>"barclamp_graph_path", :order=>800, :development=>true

    # utils
    Nav.find_or_create_by_item :item=>'utils', :parent_item=>'root', :name=>'nav.utils', :description=>'nav.utils_description', :path=>"utils_path", :order=>6000
      Nav.find_or_create_by_item :item=>'util_import', :parent_item=>'utils', :name=>'nav.util_import', :description=>'nav.util_import_description', :path=>"utils_import_path", :order=>100, :development=>true
      Nav.find_or_create_by_item :item=>'util_index', :parent_item=>'utils', :name=>'nav.util_logs', :description=>'nav.util_logs_description', :path=>"utils_path", :order=>200

    # help
    Nav.find_or_create_by_item :item=>'help', :parent_item=>'root', :name=>'nav.help', :description=>'nav.help_description', :path=>"docs_path", :order=>9999
      Nav.find_or_create_by_item :item=>'crowbar_wiki', :parent_item=>'help', :name=>'nav.wiki', :description=>'nav.wiki_description', :path=>"https://crowbar.github.com/", :order=>200

    # users
    Nav.find_or_create_by_item :item=>'users', :parent_item=>'root', :name=>'nav.users', :description=>'nav.users_description', :path=>"manage_users_path", :order=>6000
      Nav.find_or_create_by_item :item=>'manage_users', :parent_item=>'users', :name=>'nav.manage_users', :description=>'nav.manage_users_description', :path=>"manage_users_path", :order=>100

  end

  def self.down
    drop_table :navs
  end
end
