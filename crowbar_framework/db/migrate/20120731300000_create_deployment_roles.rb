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
class CreateDeploymentRoles < ActiveRecord::Migration
  def change  
    create_table :deployment_roles do |t|
      t.belongs_to  :snapshot,          :null=>false
      t.belongs_to  :role,              :null=>false
      t.string      :data,              :null=>true
      t.string      :wall,              :null=>true
      t.timestamps
    end
    #natural key 
    add_index(:deployment_roles, [:snapshot_id, :role_id], :unique => true)   
  end
end
