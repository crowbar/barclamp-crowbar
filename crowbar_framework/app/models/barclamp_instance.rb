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
  
  attr_accessible :id, :name, :description, :order, :status, :failed_reason
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
    configuration.active_instance_id == self.id
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
  # Returns a RoleInstance (not a role)
  def add_role(role)
    role = Role.add role, self.name
    ri = RoleInstance.find_by_role_id_and_barclamp_instance_id role.id, self.id 
    ri ||= RoleInstance.find_or_create_by_role_id_and_barclamp_instance_id :role_id => role.id, :barclamp_instance_id => self.id
    ri
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
  # optionally, change parent too (you do NOT have parents for templates)
  def deep_clone(parent_configuration=nil, name=nil, with_nodes=true)
    new_config = BarclampInstance.create(
          :barclamp_id => barclamp_id = self.barclamp_id,
          :barclamp_configuration_id => (parent_configuration.nil? ? nil : parent_configuration.id),
          :name => name || (self.name + "_" + self.id.to_s),
          :description => self.description, :order => self.order)

    # clone the instances
    role_instances.each { |ri| ri.deep_clone(new_config, with_nodes) }

    new_config
  end

  #
  # This builds an old-time role hash for usage by the rest of the system for now
  # This will be chef code part of Jig abstraction
  # 
  def to_proposal_object_hash
    throw "depricated"
    # depricated!
  end

end
