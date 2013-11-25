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
class CreateNodes < ActiveRecord::Migration
  def change
    create_table :nodes do |t|
      t.string      :name,          :limit => 255, :null => false
      t.string      :alias,         :limit => 100, :null => false
      t.string      :description,   :null=>true
      t.integer     :order,         :default=>10000
      t.boolean     :admin,         :default=>false
      t.integer     :target_role_id,:null=>true
      t.belongs_to  :deployment     # should be system by default
      t.text        :discovery,     :null=>false, :default=>'{}'
      t.text        :hint,          :null=>false, :default=>'{}'
      t.boolean     :allocated,     :default=>false
      t.boolean     :alive,         :null => false, :default => false
      t.boolean     :available,     :null => false, :default => true
      t.string      :bootenv,       :null => false, :default => "sledgehammer"
      t.timestamps
    end
    #natural key
    add_index(:nodes, :name, :unique => true)
  end
end
