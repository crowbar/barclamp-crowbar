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
class CreateCmdbs < ActiveRecord::Migration
  def up
    create_table :cmdbs do |t|
      t.string :name
      t.string :description, :null=>true
      t.string :type, :null=>true
      t.integer :order, :default=>10000
      t.string :backend , :null => false, :default => 'chef'
      t.timestamps
    end
    #natural key
    add_index(:cmdbs, :name, :unique => true)   

    if Rails.env == 'development'
      c = Cmdb.find_or_create_by_name!(:name=>'TestChef', :type=>'chef')
      c.save!
    end
  end

  def down
    drop_table :cmdbs
  end
end
