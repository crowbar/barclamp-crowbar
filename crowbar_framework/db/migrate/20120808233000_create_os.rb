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
class CreateOs < ActiveRecord::Migration
  def change
    create_table :os do |t|
      t.string      :name,  :unique=>true
      t.string      :description, :null=>true
      t.integer     :order, :default=>10000
      t.timestamps
    end
    #natural key
    add_index(:os, :name, :unique => true)   
    #pre-populate
    Os.create(:name=>"centos-6.2")
    Os.create(:name=>"ubuntu-12.04")
    Os.create(:name=>"redhat-6.2")
  end
  
end
