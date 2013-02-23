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
  has_one         :deployment,        :through=>:role
  has_one         :barclamp,          :through=>:deployment
  
  belongs_to      :jig_run
  alias_attribute :run,               :jig_run

  DEFAULT_CLASS = BarclampCrowbar::AttribDefault rescue Attrib
  
  MARSHAL_NIL   = "\004\b0"
  MARSHAL_EMPTY = "empty"
  
  def self.find_or_create_by_attrib_type_and_node(attrib_type, node=nil, defaultclass=DEFAULT_CLASS)
    node_id = (node.nil? ? 0 : node.id)
    attrib_type_id = (attrib_type.nil? ? nil : attrib_type.id)

    ai = Attrib.find_by_attrib_type_id_and_node_id(attrib_type_id, node_id)
    ai = defaultclass.create!(:node_id=>node_id, :attrib_type_id=>attrib_type_id) if ai.nil?
    ai
  end

  # list the jig maps that apply to this attribute
  def maps
    JigMap.find_all_by_attrib_type_id_and_barclamp_id attrib.id, barclamp.id
  end
  
  def name
    attrib_type.name
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
   j = {
     :id=> id,
     :node_id=> node_id,
     :attrib_id=> attrib_type_id,
     :value=> value,
     :state => state,
     :created_at=> created_at,
     :updated_at=> updated_at,
     :name => I18n.t('unknown'),
     :order => -1,
     :description=> I18n.t('not_set')
   }
   # protects against error
   if attrib_type
    j[:name] = "#{attrib_type.name}@#{node.name}" rescue I18n.t('unknown')
    j[:order] = attrib_type.order              # allows object to confirm to Crowbar pattern
    j[:description] = attrib_type.description  # allows object to confirm to Crowbar pattern
   end
   j
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
  def set_type_and_role
    # we need to have a type, cannot use the superclass!
    self.type = DEFAULT_CLASS.to_s if self.type.nil?
    # we need to have a role!
    # if the relationship does not exist then assume it's user defined
    if self.role_id.nil?
      role = RoleType.add :name=>"user_defined", :description=>I18n.t('model.role.user_defined_role_description'), :order=>999990
      crowbar = Barclamp.find_by_name 'crowbar'
      # use the actice snapshot
      base_config = crowbar.active.first || crowbar.template
      # if no active snapshot, use the template
      user = base_config.add_role role
      self.role_id = user.id
    end
  end
  
end
