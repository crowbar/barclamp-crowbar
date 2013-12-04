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
class CreateRoleRequires < ActiveRecord::Migration
  def change
    create_table :role_requires do |t|
      t.belongs_to  :role,              :null=>false
      t.string      :requires,          :null=>false
      t.timestamps
    end
    add_index(:role_requires, [:role_id, :requires], :unique => true)
    add_index(:role_requires, [:requires], :unique => false)

    create_table :role_require_attribs do |t|
      t.belongs_to :role
      t.string     :attrib_name
      t.timestamps
    end
    add_index(:role_require_attribs, [:role_id, :attrib_name], :unique => true)
  end
end
