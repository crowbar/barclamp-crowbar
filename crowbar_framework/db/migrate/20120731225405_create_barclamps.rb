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
      t.integer    :proposal_schema_version,   :default=>'2'
      t.string     :api_version,               :default=>"v2"
      t.string     :api_version_accepts,       :default=>"v2"
      t.string     :license,                   :default=>"apache2"
      t.string     :copyright,                 :default=>"Dell, Inc 2013"
      t.text     :online_help,               :null=>true
      t.string     :source_path,               :null=>true
      t.integer    :layout,                    :default=>2
      t.string     :requirements,              :null=>true
      t.string     :members,                   :null=>true
      t.string     :commit,                    :null=>true, :default=>'unknown'
      t.datetime   :build_on,                  :null=>true, :default=>Time.now
      t.boolean    :user_managed,              :default=>true
      t.boolean    :allow_multiple_deployments,:default=>false
      t.string     :mode,                      :default=>"full"
      t.timestamps
    end
    #natural key
    add_index(:barclamps, :name, :unique => true)   
  end
end
