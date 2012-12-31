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
class CreateNodeAttribs < ActiveRecord::Migration
  
  # This should become an in memory table once the history table is added
  # then it should be recreated on startup from the history
  
  def change
    create_table(:node_attribs, :primary_key=>:generated_id, :id=>false) do |t|
      t.integer     :generated_id,   :null=>true # manually generated!  not the auto ID propoerty
      t.string      :name,           :null=>true
      t.belongs_to  :node,           :null=>false
      t.belongs_to  :attrib,         :null=>false
      t.belongs_to  :cmdb_run,       :null=>true
      t.string      :value_actual,   :default=>"\004\b0"
      t.string      :value_proposed, :default=>"\004\b0"
      t.timestamps      
    end
    # this is a critical table, it needs a lot of indexes!
    add_index(:node_attribs,    [:name],                    :unique => true)   
    add_index(:node_attribs,    [:node_id, :attrib_id],     :unique => true)   
  end
end
