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

  attr_accessible :name, :description, :order, :status, :failed_reason
  attr_accessible :barclamp_configuration_id, :role_instance_id, :barclamp_id
  
  belongs_to      :barclamp
  belongs_to      :barclamp_configuration,  :inverse_of => :barclamp_instances
  alias_attribute :configuration,           :barclamp_configuration

  has_many        :roles,             :through => :role_instances
  has_many        :role_instances,    :dependent => :destroy 
  alias_attribute :instances,         :role_instances
  
  def active?
    configuration.active_configuration_id == self.id
  end

  ##
  # Update hash functions convert to json
  # This tracks the attributes sections
  #
  def config_hash
    {} unless config
    JSON::parse(config)
  end

  def config_hash=(chash)
    config = chash.to_json
    save!
  end

  # Add a role to a Barclamp instance by creating the needed RoleInstance
  def add_role(role)
    role = Role.find_or_create_by_name(:name => role) unless role.is_a? Role
    begin
      RoleInstance.find_by_role_id_and_barclamp_id :role_id => role.id, :barclamp_instance_id => self.id 
    rescue
      RoleInstance.find_or_create_by_role_id_and_barclamp_instance_id :role_id => role.id, :barclamp_instance_id => self.id
    end 
  end

  ##
  # Set node_roles from proposal json elements
  #
  # This is used to convert the elements section of a json config
  # into node role objects tying the node/role/config_instance together.
  #
  def update_node_roles(elements)
    nodes.delete_all
    elements.each do |role_name, node_list|
      role = Role.find_by_name(role_name)
      node_list.each do |node_name|
        node = Node.find_by_name(node_name)
        nr = NodeRole.create
        nr.node = node
        nr.role = role
        node_roles << nr
      end
    end
    reload
  end

  #
  # Helper function to tie a node and role to this config_instance
  #
  def add_node_to_role(node, role)
    nr = NodeRole.find_by_node_id_and_role_id_and_barclamp_instance_id(node.id, role.id, self.id)
    unless nr
      nr = NodeRole.create
      nr.node = node
      nr.role = role
      node_roles << nr
    end
    true
  end

  #
  # Helper function to remove a node/role pair from thie config_instance
  #
  def remove_node_from_role(node, role)
    nr = NodeRole.find_by_node_id_and_role_id_and_barclamp_instance_id(node.id, role.id, self.id)
    if nr
      nr.destroy 
      reload
    end
    true
  end

  #
  # Helper function to remove all nodes from this config_instance
  # 
  def remove_all_nodes
    nodes.delete_all
  end

  #
  # Helper function to build a list of nodes in a specific role (specified by name).
  #
  def get_nodes_by_role(role_name)
    role = Role.find_by_name_and_barclamp_id(role_name, proposal.barclamp.id)
    nrs = NodeRole.find_all_by_role_id_and_barclamp_instance_id(role.id, self.id)
    answer = []
    nrs.each do |nr|
      answer << nr.node
    end
    answer
  end

  #
  # Helper function to get a hash where the keys are role names and the values
  # are lists of nodes for that role.
  #
  def get_nodes_by_roles
    answer = {}
    node_roles.each do |nr|
      answer[nr.role.name] = [] unless answer[nr.role.name]
      answer[nr.role.name] << nr.node
    end
    answer
  end

  #
  # Helper function to look-up a node's specific config for this proposal
  #
  # Returns the hash from the node role's json blob
  #
  def get_node_config_hash(node)
    nr = NodeRole.find_by_node_id_and_barclamp_instance_id_and_role_id(node.id, self.id, nil)
    return {} unless nr
    return {} unless nr.config
    nr.config_hash
  end

  #
  # Helper function to set the hash into the node's specific config holder.
  #
  # Stores the hash into the node role's json blob
  #
  def set_node_config_hash(node, hash)
    nr = NodeRole.find_by_node_id_and_barclamp_instance_id_and_role_id(node.id, self.id, nil)
    unless nr
      nr = NodeRole.create
      nr.barclamp_instance = self
      nr.role = nil
      nr.node = node
      nr.save
    end

    nr.config_hash = hash
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
