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

    create_table(:docs, :primary_key=>'name', :id=>false) do |t|
      t.string :name
      t.string :parent_name, :default=>'root'
      t.string :description
      t.string :url, :null=>true
      t.string :author, :null=>true
      t.string :license, :null=>true
      t.string :copyright, :null=>true
      t.string :date, :null=>true, :length=>20
      t.string :order, :length=>5, :default => '00999'
      t.timestamps
    end
    
  end

  def self.down
    drop_table :docs
  end
end