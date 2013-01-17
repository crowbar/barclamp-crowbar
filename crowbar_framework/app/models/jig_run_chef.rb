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

class JigRunChef < JigRun

  def init
    super.init
  end

  def run_jig_on_node(jig, node)
    super.run_jig_on_node(jig, node)
  end
  
  def attrs_from_jig(jig, node)
    #c = Jig.new('chef')
    puts "node => #{jig.class}"
    n = jig.node(node) # got the jig node
    puts "node => #{n.class}"
    m = map('1') # lame

    # map the attributes to the jig_attribute object
    m.mapping.each_pair do | jig_attr, lookup_value |
      # get the values out of the node object
      puts "jig_attr => #{jig_attr}, lookup_value => #{lookup_value}"
      node_attr_value = eval("n#{lookup_value}")
      puts "eval result => #{node_attr_value}" 
      # and shove them into the database as a JigAttribute
      a = JigAttribute.new(:name => jig_attr, :value => node_attr_value)
      a.save!
    end     
  end
    
  def attrs_to_jig(jig, node)
    super.attrs_to_jig(jig, node)
  end

  # make sure I can get the map I need to put attrs in the DB
  def map(map_id)
    super.map map_id
  end
end

