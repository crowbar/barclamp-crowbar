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
class CreateCmdbMaps < ActiveRecord::Migration
  def up
    create_table :cmdb_maps do |t|
      t.string :name
      t.string :description
      t.string :order
      t.string :revision
      t.string :direction
      t.text :map

      t.references :barclamp

      t.timestamps
    end

    # add sample data for development
    if Rails.env == 'development' 
      CmdbMap.find_or_create_by_name!( 
        :name=>'testmap', 
        :barclamp=>'test',
        :map=>%/{ "name" : { "chef": "[:name]" }, "mac": { "chef": "[:macaddress]" }, "memory": { "chef": "[:memory][:total]" }, "cpu_type": { "chef": "[:cpu]['0'][:model_name]" }, "cpu_count": { "chef": "[:cpu][:total]"}, "hardware": { "chef": "[:dmi][:system][:product_name]" }, "uptime": { "chef": "[:uptime]" } }/
      )
    end
  end


  def down
    drop_table :cmdb_maps
  end
end
