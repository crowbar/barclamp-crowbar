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
class CreateBarclamps < ActiveRecord::Migration
  def change
    create_table :barclamps do |t|
      t.string     :name
      t.string     :description,               :null=>true
      t.string     :type,                      :null=>false
      t.string     :display
      t.integer    :version
      t.string     :online_help,               :null=>true
      t.string     :source_path,               :null=>true
      t.integer    :proposal_schema_version,   :default=>2
      t.integer    :layout,                    :default=>2
      t.integer    :order,                     :default=>9999
      t.integer    :run_order,                 :default=>1000
      t.integer    :jig_order,                 :default=>1000
      t.string     :commit,                    :null=>true, :default=>'unknown'
      t.date       :build_on,                  :null=>true, :default=>'unknown'
      t.boolean    :user_managed,              :default=>true
      t.boolean    :allow_multiple_proposals,  :default=>false
      t.string     :mode,                      :default=>"full"
      t.boolean    :transitions,               :default=>false
      t.string     :transition_list,           :default=>"all"
      t.references :template                   # points to a barclamp_instance
      t.timestamps
    end
    #natural key
    add_index(:barclamps, :name, :unique => true)   
  end
end
