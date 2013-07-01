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
  
    # nodes
    Nav.find_or_create_by_item :item=>'nodes', :parent_item=>'root', :name=>'nav.nodes', :description=>'nav.nodes_description', :path=>"nodes_path", :order=>1000
      Nav.find_or_create_by_item :item=>'groups', :parent_item=>'nodes', :name=>'nav.groups', :description=>'nav.groups_description', :path=>"nodes_path", :order=>2000

    # deployments
    Nav.find_or_create_by_item :item=>'deploy', :parent_item=>'root', :name=>'nav.deployments', :description=>'nav.deployments_description', :path=>"deployments_path", :order=>2000
      Nav.find_or_create_by_item :item=>'deploy', :parent_item=>'deploy', :name=>'nav.deployments', :description=>'nav.deployments_description', :path=>"deployments_path", :order=>1000
      Nav.find_or_create_by_item :item=>'roles', :parent_item=>'deploy', :name=>'nav.roles', :description=>'nav.roles_description', :path=>"roles_path", :order=>2000

    # utils
    Nav.find_or_create_by_item :item=>'utils', :parent_item=>'root', :name=>'nav.utils', :description=>'nav.utils_description', :path=>"utils_path", :order=>6000
      Nav.find_or_create_by_item :item=>'util_index', :parent_item=>'utils', :name=>'nav.util_logs', :description=>'nav.util_logs_description', :path=>"utils_path", :order=>200
      Nav.find_or_create_by_item :item=>'jigs', :parent_item=>'utils', :name=>'nav.jigs', :description=>'nav.jigs_description', :path=>"jigs_path", :order=>300
      Nav.find_or_create_by_item :item=>'barclamps', :parent_item=>'utils', :name=>'nav.barclamps', :description=>'nav.barclamps_description', :path=>"barclamps_path", :order=>4000

    # users
    Nav.find_or_create_by_item :item=>'users', :parent_item=>'root', :name=>'nav.users', :description=>'nav.users_description', :path=>"manage_users_path", :order=>6000
      Nav.find_or_create_by_item :item=>'manage_users', :parent_item=>'users', :name=>'nav.manage_users', :description=>'nav.manage_users_description', :path=>"manage_users_path", :order=>100

    # help
    Nav.find_or_create_by_item :item=>'help', :parent_item=>'root', :name=>'nav.help', :description=>'nav.help_description', :path=>"docs_path", :order=>9999
      Nav.find_or_create_by_item :item=>'crowbar_wiki', :parent_item=>'help', :name=>'nav.wiki', :description=>'nav.wiki_description', :path=>"http://crowbar.github.com/", :order=>200

  end

  def self.down
    drop_table :navs
  end
end
