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
      t.string      :type,              :null=>true
      t.text        :template,          :null=>false, :default=>"{}"
      t.string      :jig_name,          :null=>false
      # brings in library code thats used to access another role (sql client)
      t.boolean     :library,           :null=>false, :default=>false
      # role is applied automatically to nodes after allocation
      t.boolean     :implicit,          :null=>false, :default=>false
      # used for the admin node(s) during bring up
      t.boolean     :bootstrap,         :null=>false, :default=>false
      # related to the node being discovered in the system (by deployer)
      t.boolean     :discovery,         :null=>false, :default=>false
      # Indicates that the attributes used by this node should be made available to
      # its children in the noderole graph.
      t.boolean     :server,            :null=>false, :default=>false
      # Indicates that this role implements a clustered service.
      # When the noderole graph is built, any child noderoles of this service
      # will be bound to all of the noderoled for this role in the deployment.
      # The cluster flag and the implicit flag are mutually exclusive.
      t.boolean     :cluster,           :null=>false, :default=>false
      # Indicates that the role is not idempotent -- once a noderole binding
      # it has transitioned to active, it will not be run again.
      t.boolean     :destructive,       :null=>false, :default=>false
      t.belongs_to  :barclamp,          :null=>false
      t.integer     :cohort
      t.timestamps
    end
    #natural key
    add_index(:roles, [:barclamp_id, :name], :unique => true)
  end
end
