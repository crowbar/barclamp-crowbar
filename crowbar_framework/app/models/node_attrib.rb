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

class NodeAttrib < ActiveRecord::Base

  NODE_ID_SPACE = 10000000
  MARSHAL_NIL   = "\004\b0"
  
  before_create :create_identity

  attr_accessible :node_id, :attrib_id, :value_actual, :value_proposed
  attr_readonly   :name

  belongs_to  :attrib
  belongs_to  :node
  #belongs_to  :run, :class_name => "CmdbRun", :foreign_key => "cmdb_run_id"

  self.primary_key = 'generated_id'

  def self.find(id)
    NodeAttrib.find_by_generated_id id
  end

  def self.find_or_create_by_node_and_attrib(node, attrib)
    throw "Node provided cannot be nil" unless node
    throw "Attrib provided cannot be nil" unless attrib
    nid = node.id
    aid = attrib.id
    na = NodeAttrib.find NodeAttrib.id_generate(nid, aid)
    na = NodeAttrib.create(:node_id=>nid, :attrib_id=>aid) unless na
    na
  end

  def self.id_generate node, attribute
    node*NODE_ID_SPACE+attribute
  end

  # Returns state of value (:ready or :pending)
  def state
    if value_proposed.eql? MARSHAL_NIL
      return :ready
    elsif value_actual.eql? MARSHAL_NIL
      return :pending
    elsif value_actual == value_proposed
      return :ready
    else
      return :pending
    end
  end   
  
  def id
    return self.generated_id
  end
    
  # for now, none of the proposed values are visible
  def value
    return self.actual
  end
  
  def description
    return self.value
  end
  
  def actual=(value)
    self.value_actual = Marshal::dump(value)
  end
  
  def actual
    Marshal::load(self.value_actual)
  end
  
  def proposed=(value)
    self.value_proposed = Marshal::dump(value)
  end
  
  def proposed
    Marshal::load(self.value_proposed)
  end
  
  private
  
  # make sure some safe values are set for the node
  def create_identity
    throw "NodeAttrib cannot create without a Node ID" unless self.node_id
    n = Node.find self.node_id
    throw "NodeAttrib cannot create without a valid Node (ID was #{self.node_id})" unless n
    throw "NodeAttrib cannot create without an Attrib ID" unless self.attrib_id
    a = Attrib.find self.attrib_id
    throw "NodeAttrib cannot create without a valid Attrib (ID was #{self.attrib_id})" unless a
    self.generated_id = NodeAttrib.id_generate n.id, a.id
    self.name = "#{a.name}@#{n.name}"
  end
  
end
