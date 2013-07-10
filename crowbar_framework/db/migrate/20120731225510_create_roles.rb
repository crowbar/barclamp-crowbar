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
class CreateRoles < ActiveRecord::Migration
  def change  
    create_table :roles do |t|
      t.string      :name,              :null=>false
      t.string      :description,       :null=>true
      t.string      :role_template,     :null=>true
      t.string      :node_template,     :null=>true
      t.boolean     :library,           :null=>false, :default=>false  # brings in library code thats used to access another role (sql client)
      t.boolean     :implicit,          :null=>false, :default=>false  # role is applied automatically to nodes after allocation
      t.boolean     :bootstrap,         :null=>false, :default=>false  # used for the admin node(s) during bring up
      t.boolean     :discovery,         :null=>false, :default=>false  # related to the node being discovered in the system (by deployer)
      t.integer     :min_nodes,         :null=>false, :default=>1      # how many nodes must this role be applied to
      t.belongs_to  :jig,               :null=>false
      t.belongs_to  :barclamp,          :null=>false
      t.timestamps
    end
    #natural key
    add_index(:roles, [:barclamp_id, :name], :unique => true)
  end
end
