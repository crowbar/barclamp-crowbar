#
# Copyright 2011-2013, Dell
# Copyright 2013-2014, SUSE LINUX Products GmbH
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

class CreateSessions < ActiveRecord::Migration
  def self.up
    create_table :sessions, :force => true do |t|
      t.string :session_id, :null => false
      t.text :data, :null => true

      t.timestamps
    end

    add_index :sessions, [:session_id], :name => "sessions_on_session_id"
    add_index :sessions, [:updated_at], :name => "sessions_on_updated_at"
  end

  def self.down
    remove_index :sessions, :name => "sessions_on_updated_at"
    remove_index :sessions, :name => "sessions_on_session_id"

    drop_table :sessions
  end
end
