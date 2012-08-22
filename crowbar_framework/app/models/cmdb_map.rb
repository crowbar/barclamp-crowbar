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
class CmdbMap < ActiveRecord::Base
  attr_accessible :name, :description, :order, :map, :revision
  
  belongs_to :barclamp

  has_many :cmdb_runs
  has_many :cmdb_attributes

  # get code from map name
  def init_map
    @my_map = ActiveSupport::JSON.decode(self.map)
    puts "getting map #{@my_map.keys}"
  end

  def all_maps
    @my_map.keys
  end

  def map_get(attribute)
    self.init_map if @my_map.empty?
    puts "MAP looking up #{attribute}"
    cmdb_type = 'chef'
    #puts @my_map[attribute]
    #puts @my_map[attribute][cmdb_type]
    return @my_map[attribute][cmdb_type]
  end
  
  def method_missing(m,*args,&block)
    method = m.to_s
    if method.starts_with? "map_"
      return map_get method[4..100]
    else
      puts "Node #{name} #{method.inspect} #{args.inspect} #{block.inspect}"
      Rails.logger.fatal("Cannot delegate method #{m} to #{self.class}")
      throw "ERROR #{method} not defined for node #{name}"
    end
  end
end



