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

class CmdbEvent < ActiveRecord::Base
  attr_accessible :attributes, :direction, :name, :result, :status, :cmdb_run, :type

  # RESTORE THESE (after building tests)
  #belongs_to :cmdb_run
  #belongs_to :node, :through => :cmdb_run
  #belongs_to :cmdb, :through => :cmdb_run

  # map node from Chef into an array of CmdbAttributes
  def attrs_from_cmdb(cmdb, node)
    #c = Cmdb.new('chef')
    puts "node => #{cmdb.class}"
    n = cmdb.node(node) # got the cmdb node
    puts "node => #{n.class}"
    m = map('1') # lame

    # map the attributes to the cmdb_attribute object
    m.mapping.each_pair do | cmdb_attr, lookup_value |
      # get the values out of the node object
      puts "cmdb_attr => #{cmdb_attr}, lookup_value => #{lookup_value}"
      node_attr_value = eval("n#{lookup_value}")
      puts "eval result => #{node_attr_value}" 
      # and shove them into the database as a CmdbAttribute
      a = CmdbAttribute.new(:name => cmdb_attr, :value => node_attr_value)
      a.save!
    end     
  end
  
  # make sure I can get the map I need to put attrs in the DB
  def map(map_id)
    begin
      CmdbMap.find(map_id)
    rescue Exception => e
      Rails.logger.warn("Could not find CmdbMap.id #{map_id}: #{e.inspect}")
      return nil
    end
  end


  def attrs_to_cmdb(cmdb, node)
    # get the node_attributes for this node
    #
  end

  def run_cmdb_on_node(cmdb, node)
    
  end


end
