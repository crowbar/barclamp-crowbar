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
class CreateRoleInstances < ActiveRecord::Migration
  def change  
    create_table :role_instances do |t|
      t.belongs_to  :role
      t.belongs_to  :barclamp_instance
      t.string      :description,       :null=>true
      t.integer     :order,             :default => 9999, :null => false
      t.integer     :run_order,         :default => 9999, :null => false
      t.string      :states,            :default=>"all",  :null=>true
      t.timestamps
    end
    #natural key
    add_index(:role_instances, [:barclamp_instance_id, :role_id], :unique => true)   
  end
end
