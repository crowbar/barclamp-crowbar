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

class AttribInstance < ActiveRecord::Base

  before_create :set_type

  attr_accessible :node_id, :attrib_id, :role_instance_id, :type     # core relationshipships
  attr_accessible :value_actual, :value_request   # data storage
  attr_accessible :jig_run_id                     # jig relationship

  belongs_to      :attrib
  belongs_to      :node

  belongs_to      :role_instance
  has_one         :role,              :through=>:role_instance
  has_one         :barclamp_instance, :through=>:role_instance
  alias_attribute :instance,          :barclamp_instance
  has_one         :barclamp,          :through=>:barclamp_instance
  
  belongs_to      :jig_run
  alias_attribute :run,               :jig_run

  DEFAULT_CLASS = Crowbar::AttribInstanceDefault rescue AttribInstance
  
  MARSHAL_NIL   = "\004\b0"
  MARSHAL_EMPTY = "empty"
  
  def self.find_or_create_by_attrib_and_node(attrib, node=nil, defaultclass=DEFAULT_CLASS)
    node_id = (node.nil? ? 0 : node.id)
    attrib_id = (attrib.nil? ? nil : attrib.id)

    ai = AttribInstance.find_by_attrib_id_and_node_id(attrib_id, node_id)
    ai = defaultclass.create!(:node_id=>node_id, :attrib_id=>attrib_id) if ai.nil?
    ai
  end

  # list the jig maps that apply to this attribute
  def maps
    JigMap.find_all_by_attrib_id_and_barclamp_id attrib.id, barclamp.id
  end
  
  def name
    attrib.name
  end

  # for now, none of the proposed values are visible
  def value
    return self.actual
  end
    
  # Returns state of value of :empty, :set (by API) or :managed (by Jig)
  def state
    throw "must be provided by subclass"
  end

  def request=(value)
    throw "must be provided by subclass"
  end
  
  def request
    throw "must be provided by subclass"
  end
  
  # used by the API when values are set outside of Jig runs
  def actual=(value)
    throw "must be provided by subclass"
  end
  
  def actual
    throw "must be provided by subclass"
  end
  
  def as_json options={}
   {
     :id=> id,
     :node_id=> node_id,
     :attrib_id=> attrib_id,
     :name=> attrib.name + "@" + node.name,
     :value=> value,
     :state => state,
     :order => attrib.order,              # allows object to confirm to Crowbar pattern
     :description=> attrib.description,   # allows object to confirm to Crowbar pattern
     :created_at=> created_at,
     :updated_at=> updated_at
   }
  end
  
  def self.calc_state v_actual, v_request, run
    if v_actual.eql? MARSHAL_EMPTY and v_request.eql? MARSHAL_EMPTY
      return :empty
    elsif !v_actual.eql? v_request and !v_request.eql? MARSHAL_EMPTY
      return :unready
    elsif run == 0
      return :ready
    else
      return :ready
    end
  end
  
  def self.serial_in value
    Marshal::dump(value)
  end

  def self.serial_out value
    if value.eql? MARSHAL_EMPTY
      nil
    else
      Marshal::load(value)
    end
  end
  
  private
  
  # make sure some safe values are set for the node
  def set_type
    self.type = Crowbar::AttribInstanceDefault.to_s if self.type.nil?
  end
  
end
