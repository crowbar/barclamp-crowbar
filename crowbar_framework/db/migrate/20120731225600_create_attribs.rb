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
class CreateAttribs < ActiveRecord::Migration

  def change
    create_table :attribs do |t|
      t.integer     :barclamp_id,   :null => true
      t.integer     :role_id,       :null => true
      t.string      :type,          :null => true
      t.string      :name,          :null => false
      t.string      :description,   :null => true
      t.integer     :order,         :default=>10000
      t.string      :map,           :null => true
      t.timestamps
    end

    # natural key
    add_index :attribs,    [:name], :unique => true
  end
end
