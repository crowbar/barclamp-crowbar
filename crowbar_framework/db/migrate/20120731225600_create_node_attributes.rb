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
class CreateNodeAttributes < ActiveRecord::Migration
  
  # This should become an in memory table once the history table is added
  # then it should be recreated on startup from the history
  
  def change
    create_table :node_attributes do |t|
      t.belongs_to  :node,           :null=>false
      t.belongs_to  :attribute,      :null=>false
      t.belongs_to  :cmdb_run,       :null=>true
      t.string      :value_actual,   :default=>"\004\b0"
      t.string      :value_proposed, :default=>"\004\b0"
      t.timestamps      
    end
    add_index(:node_attributes,    [:node_id, :attribute_id],     :unique => false)   
    add_index(:node_cmdb_run,      [:node_id, :cmdb_run_id],      :unique => false)   
    add_index(:attribute_cmdb_run, [:attribute_id, :cmdb_run_id], :unique => false)   
  end
end
