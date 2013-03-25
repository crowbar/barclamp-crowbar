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
class CreateJigs < ActiveRecord::Migration
  def self.up
    create_table :jigs do |t|
      t.string :name
      t.string :description,  :null=>true
      t.string :type,         :null=>false
      t.boolean :active,      :default => false 
      t.string :server,       :null=>true
      t.string :client_name,  :null=>true
      t.string :key,          :null=>true
      t.integer :order,       :default=>10000
      t.timestamps
    end
    #natural key
    add_index(:jigs, :name, :unique => true)   
    # create test jig
    Jig.find_or_create_by_name(:name =>'test', :order=>9999, :active=>true, :type=>'BarclampCrowbar::Jig') unless Rails.env.production? 
  end

  def self.down
    drop_table :jigs
  end
end
