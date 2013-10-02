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

class CreateRuns < ActiveRecord::Migration
  def change
    create_table :runs do |t|
      t.belongs_to  :node_role,         :null=>false
      t.belongs_to  :node,              :null=>false
      t.boolean     :running,           :null=>false, :default => false
    end

    add_index(:runs, :node_role_id)
    add_index(:runs, :node_id)
    add_index(:runs, :running)
  end
end
