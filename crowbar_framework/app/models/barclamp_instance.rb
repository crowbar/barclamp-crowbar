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

#######
# An 'instance' of a configuration of a barclamp.
# An instance aputres the point in time information for a barclamp 'configuration'.
class BarclampInstance < ActiveRecord::Base

  #
  # Status of the proposal
  #
  STATUS_NONE        = 1  # Not applied, just created
  STATUS_QUEUED      = 2  # Attempt at commit, but is queued
  STATUS_COMMITTING  = 3  # Attempt at commit is in progress
  STATUS_FAILED      = 4  # Attempted commit failed
  STATUS_APPLIED     = 5  # Attempted commit succeeded

  ROLE_ORDER         = "'role_instances'.'order', 'role_instances'.'run_order'"
  
  attr_accessible :name, :description, :order, :status, :failed_reason
  attr_accessible :barclamp_configuration_id, :barclamp_id
  
  belongs_to      :barclamp
  belongs_to      :barclamp_configuration,  :inverse_of => :barclamp_instances
  alias_attribute :configuration,           :barclamp_configuration

  has_many        :roles,             :through => :role_instances
  has_many        :role_instances,    :dependent => :destroy, :order => ROLE_ORDER
  has_many        :private_roles,     :class_name => "RoleInstance", :conditions=>'run_order<0', :order => ROLE_ORDER
  has_many        :public_roles,      :class_name => "RoleInstance", :conditions=>'run_order>=0', :order => ROLE_ORDER
  alias_attribute :instances,         :role_instances

  has_many        :attrib_instances,  :through => :role_instances
  alias_attribute :values,            :attrib_instances
  has_many        :attribs, :through => :attrib_instances
  
  def active?
    configuration.active_configuration_id == self.id
  end

  ##
  # Update hash functions convert to json
  # This tracks the attributes sections
  #
  def config_hash
    # TODO REMOVE / REPLACE
    {} unless config
    JSON::parse(config)
  end

  def config_hash=(chash)
    # TODO REMOVE
    config = chash.to_json
    save!
  end

  # Add a role to a Barclamp instance by creating the needed RoleInstance
  def add_role(role)
    role = Role.add role, self.name
    begin
      RoleInstance.find_by_role_id_and_barclamp_id :role_id => role.id, :barclamp_instance_id => self.id 
    rescue
      RoleInstance.find_or_create_by_role_id_and_barclamp_instance_id :role_id => role.id, :barclamp_instance_id => self.id
    end 
  end

  # Add attrib to barclamp
  # assume first public role (fall back to private role) if none given
  def add_attrib(attrib, role=nil)
    if role.nil?
      role = public_roles.first.role
      role = private_roles.first.role if role.nil?
    else
      role = Role.add role, self.name
    end
    desc = I18n.t 'added', :scope => 'model.role', :name=>self.name
    ri = RoleInstance.find_or_create_by_role_id_and_barclamp_instance_id :role_id=>role.id, 
                                                                         :barclamp_instance_id=>self.id,
                                                                         :description=>desc
    ri.add_attrib attrib, nil, self.name
  end

  ##
  # Clone this config_instance
  # optionally, change parent too
  def deep_clone(parent_configuration=nil)
    new_config = self.dup
    new_config.barclamp_configuration_id = parent_configuration.id if parent_configuration
    new_config.name += "_" + self.id.to_s
    new_config.status = STATUS_NONE
    new_config.failed_reason = nil
    new_config.save

    # clone the instances
    role_instances.each { |ri| ri.deep_clone(self.id) }

    new_config
  end

  #
  # This builds an old-time role hash for usage by the rest of the system for now
  # This will be chef code part of Jig abstraction
  # 
  def to_proposal_object_hash
    # OLD CB1 
    phash = {}

    bc_name = proposal.barclamp.name
    phash["id"] = "bc-#{bc_name}-#{proposal.name}"
    phash["description"] = proposal.description
    phash["attributes"] = JSON::parse(config)
    phash["deployment"] = {}
    phash["deployment"][bc_name] = {}
    phash["deployment"][bc_name]["config"] = {}
    phash["deployment"][bc_name]["config"]["environment"] = "#{bc_name}-config-#{proposal.name}"
    phash["deployment"][bc_name]["config"]["mode"] = proposal.barclamp.mode
    phash["deployment"][bc_name]["config"]["transitions"] = proposal.barclamp.transitions
    phash["deployment"][bc_name]["config"]["transition_list"] = proposal.barclamp.transition_list.split(",")
    phash["deployment"][bc_name]["crowbar-revision"] = 0
    phash["deployment"][bc_name]["element_order"] = []
    phash["deployment"][bc_name]["element_states"] = {}
    proposal.barclamp.roles.each do |role|
      phash["deployment"][bc_name]["element_states"][role.name] = role.states.split(",")
      role.role_element_orders.each do |roe|
        index = roe.order
        phash["deployment"][bc_name]["element_order"][index] = [] unless phash["deployment"][bc_name]["element_order"][index]
        phash["deployment"][bc_name]["element_order"][index] << role.name
      end
    end

    elements = {}
    node_roles.each do |nr|
      next unless nr.role
      next unless nr.node
      elements[nr.role.name] = [] unless elements[nr.role.name]
      elements[nr.role.name] << nr.node.name
    end
    phash["deployment"][bc_name]["elements"] = elements

    phash
  end

end
