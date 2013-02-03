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
class CreateAttribInstances < ActiveRecord::Migration
    
  def change
    create_table :attrib_instances do |t|
      t.string      :type,          :null => false, :default => Crowbar::AttribInstanceDefault.to_s
      t.belongs_to  :attrib,        :null=>false
      t.belongs_to  :node,          :null=>true
      t.belongs_to  :jig_run,       :null=>true
      t.string      :value_actual,  :default=>"empty"
      t.string      :value_request, :default=>"empty"
      t.integer     :id_actual,     :default=>-1
      t.integer     :id_request,    :default=>-1
      t.timestamps      
    end

    add_index(:attrib_instances,    [:attrib_id, :node_id],     :unique => true)   
  end
end
