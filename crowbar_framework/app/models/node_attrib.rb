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
  NODE_NAME_DELIM = '@'
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

  def self.delete_by_node_and_attrib(node, attrib)
    na = NodeAttrib.find NodeAttrib.id_generate(node.id, attrib.id)
    if na.nil?
      id = -1
    else
      id = na.id
      id = (na.delete ? id : -1 )
    end
    id
  end

  def self.find_by_node_and_attrib(node, attrib)
    throw "Node provided cannot be nil" unless node
    throw "Attrib provided cannot be nil" unless attrib
    NodeAttrib.find NodeAttrib.id_generate(node.id, attrib.id)
  end
    
  def self.find_or_create_by_node_and_attrib(node, attrib)
    na = find_by_node_and_attrib node, attrib
    na = NodeAttrib.create(:node_id=>node.id, :attrib_id=>attrib.id) if na.nil?
    na
  end


  def self.name_generate node, attribute
    "#{attribute.name}#{NODE_NAME_DELIM}#{node.name}"
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
  
  def as_json options={}
   {
     :id=> id,
     :node_id=> node_id,
     :attrib_id=> attrib_id,
     :name=> name,
     :value=> value,
     :state => state,
     :order => attrib.order,              # allows object to confirm to Crowbar pattern
     :description=> attrib.description,   # allows object to confirm to Crowbar pattern
     :created_at=> created_at,
     :updated_at=> updated_at
   }
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
    self.name = NodeAttrib.name_generate n, a
  end
  
end
