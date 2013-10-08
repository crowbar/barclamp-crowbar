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
class CreateDocs < ActiveRecord::Migration
  def self.up

    create_table :docs do |t|
      t.text        :name
      t.belongs_to  :barclamp, :null=>true
      t.text        :description, :null=>true
      t.belongs_to  :parent, :null=>true
      t.string      :order, :length=>5, :default => '009999'
      t.timestamps
    end
    
    add_index(:docs, :name, :unique => true)   
  end

  def self.down
    drop_table :docs
  end
end
