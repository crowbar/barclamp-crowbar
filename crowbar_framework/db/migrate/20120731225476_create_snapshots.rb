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
class CreateSnapshots < ActiveRecord::Migration
  def change
    create_table :snapshots do |t|
      t.string      :name,                        :null=>false, :default=>I18n.t('not_set')
      t.string      :description,                 :null=>true
      t.integer     :order,                       :null=>false, :default=>10000
      t.belongs_to  :deployment,                  :null=>true
      t.belongs_to  :barclamp,                    :null=>false                    
      t.integer     :status,                      :default => Snapshot::STATUS_CREATED
      t.string      :failed_reason,               :null=>true
      t.string      :element_order,               :null=>true
      t.timestamps      
    end
    #natural key -> none
  end
    
end
