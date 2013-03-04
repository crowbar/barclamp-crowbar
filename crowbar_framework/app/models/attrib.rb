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

class Attrib < ActiveRecord::Base

  before_create :set_type_and_role

  attr_accessible :node_id, :attrib_type_id, :role_id, :type     # core relationship
  attr_accessible :value_actual, :value_request   # data storage
  attr_accessible :jig_run_id                     # jig relationship

  belongs_to      :attrib_type
  belongs_to      :node
  belongs_to      :role

  has_one         :role_type,         :through=>:role
  has_one         :snapshot,          :through=>:role
  has_one         :deployment,        :through=>:snapshot
  has_one         :barclamp,          :through=>:snapshot
  
  belongs_to      :jig_run
  alias_attribute :run,               :jig_run

  DEFAULT_CLASS = BarclampCrowbar::AttribDefault rescue Attrib
  
  MARSHAL_NIL   = "null"
  MARSHAL_EMPTY = "empty"
  
  def self.find_or_create_by_attrib_type_and_node(attrib_type, node=nil, defaultclass=DEFAULT_CLASS)
    raise "attrib_type must be provided" unless attrib_type
    node_id = node.read_attribute(:id) if node
    attrib_type_id = attrib_type.read_attribute(:id)
    ai = Attrib.find_by_attrib_type_id_and_node_id attrib_type_id, node_id
    ai ||= defaultclass.create! :node_id=>node_id, :attrib_type_id=>attrib_type_id 
    ai
  end

  # list the jig maps that apply to this attribute
  def maps
    JigMap.find_all_by_attrib_type_id_and_barclamp_id attrib.id, barclamp.id
  end
  
  # convenience method to get name from type
  def name
    attrib_type.name rescue I18n.t('unknown')
  end
  
  # convenience method to get description from type
  def description
    attrib_type.description rescue I18n.t('not_set')
  end

  # convenience method to get order from type
  def order
    attrib_type.order rescue -1
  end

  # for now, none of the proposed values are visible
  def value
    return self.actual
  end
    
  # Returns state of value of :empty, :set (by API) or :managed (by Jig)
  def state
    raise "must be provided by subclass"
  end

  def request=(value)
    raise "must be provided by subclass"
  end
  
  def request
    raise "must be provided by subclass"
  end
  
  # used by the API when values are set outside of Jig runs
  def actual=(value)
    raise "must be provided by subclass"
  end
  
  def actual
    raise "must be provided by subclass"
  end
  
  def as_json options={}
   {
     :id=> id,
     :node_id=> node_id,
     :attrib_type_id=> attrib_type_id,
     :value=> value,
     :state => state,
     :created_at=> created_at,
     :updated_at=> updated_at,
     :name => name,
     :order => order,
     :description=> description
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
     ActiveSupport::JSON.encode(value)
  end

  def self.serial_out value
    if value.eql? MARSHAL_EMPTY
      nil
    else
      ActiveSupport::JSON.decode(value)
    end
  end
  
  private
  
  # make sure some safe values are set for the node
  def set_type_and_role
    # we need to have a type, cannot use the superclass!
    self.type = DEFAULT_CLASS.to_s if self.type.nil?
    # we need to have a role!
    # if the relationship does not exist then assume it's user defined
    if self.role_id.nil?
      role_type = RoleType.add :name=>"user_defined", :description=>I18n.t('model.role.user_defined_role_description'), :order=>999990
      crowbar = Barclamp.find_by_name 'crowbar'
      base_config = crowbar.template
      # use the proposed snapshot
      base_config = crowbar.deployments.first.proposed if crowbar.deployments.count>0
      # if no active snapshot, use the template
      user = base_config.add_role role_type
      self.role_id = user.id
    end
  end
  
end
