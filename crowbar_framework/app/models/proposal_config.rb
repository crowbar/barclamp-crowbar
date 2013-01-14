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
#  configuration details for a given proposals.
#  proposal 
#
class ProposalConfig < ActiveRecord::Base

  #
  # Status of the proposal
  #
  STATUS_NONE        = 1  # Not applied, just created
  STATUS_QUEUED      = 2  # Attempt at commit, but is queued
  STATUS_COMMITTING  = 3  # Attempt at commit is in progress
  STATUS_FAILED      = 4  # Attempted commit failed
  STATUS_APPLIED     = 5  # Attempted commit succeeded

  belongs_to      :proposal, :inverse_of => :proposal_config
  has_many        :node_roles
  has_many        :nodes, :through => :node_roles
  has_many        :roles, :through => :node_roles

  has_one         :barclamp, :through =>:proposal

  def failed?
    status == STATUS_FAILED
  end

  def applied?
    status == STATUS_APPLIED
  end

  def queued?
    status == STATUS_QUEUED
  end

  def committing?
    status == STATUS_COMMITTING
  end

  def operations
    proposal.barclamp.operations()
  end

  ##
  # Update hash functions convert to json
  # This tracks the attributes sections
  #
  def config_hash
    return {} unless self.config
    return {} if self.config == ""
    JSON::parse(self.config)
  end

  def config_hash=(chash)
    self.config = chash.to_json
    save!
  end

  ##
  # Set node_roles from proposal json elements
  #
  # This is used to convert the elements section of a json config
  # into node role objects tying the node/role/proposal_config together.
  #
  def update_node_roles(elements)
    nodes.delete_all
    save_it = false
    elements.each do |role_name, node_list|
      role = Role.find_by_name(role_name)
      next unless role
      node_list.each do |node_name|
        node = Node.find_by_name(node_name)
        next unless node
        nr = NodeRole.create
        nr.node = node
        nr.role = role
        node_roles << nr
        save_it = true
      end
    end
    save! if save_it
    reload
  end

  #
  # Helper function to tie a node and role to this propsal_config.
  #
  def add_node_to_role(node, role)
    return false if node.nil? or role.nil?
    nr = NodeRole.find_by_node_id_and_role_id_and_proposal_config_id(node.id, role.id, self.id)
    unless nr
      nr = NodeRole.create
      nr.node = node
      nr.role = role
      node_roles << nr
    end
    true
  end

  #
  # Helper function to remove a node/role pair from thie proposal_config.
  #
  def remove_node_from_role(node, role)
    return false if node.nil? or role.nil?
    nr = NodeRole.find_by_node_id_and_role_id_and_proposal_config_id(node.id, role.id, self.id)
    if nr
      nr.destroy 
      reload
      return true
    end
    false
  end

  #
  # Helper function to remove all nodes from this proposal_config
  # 
  def remove_all_nodes
    nodes.delete_all
  end

  #
  # Helper function to build a list of nodes in a specific role (specified by name).
  #
  def get_nodes_by_role(role_name)
    role = Role.find_by_name_and_barclamp_id(role_name, proposal.barclamp.id)
    return [] unless role
    nrs = NodeRole.find_all_by_role_id_and_proposal_config_id(role.id, self.id)
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
      next unless nr.role
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
    return nil unless node
    nr = NodeRole.find_by_node_id_and_proposal_config_id_and_role_id(node.id, self.id, nil)
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
    nr = NodeRole.find_by_node_id_and_proposal_config_id_and_role_id(node.id, self.id, nil)
    unless nr
      nr = NodeRole.create
      nr.proposal_config = self
      nr.role = nil
      nr.node = node
      nr.save
      reload
    end

    nr.config_hash = hash
    true
  end

  ##
  # Clone this proposal_config
  #
  def deep_clone
    new_config = self.dup
    new_config.save

    node_roles.each do |nr|
      a = NodeRole.new
      a.node = nr.node
      a.role = nr.role
      a.proposal_config = new_config
      a.save
    end
    new_config.reload if node_roles.size > 0

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
    phash["attributes"] = config_hash
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

  def to_s
    "PC: #{proposal.name} #{proposal.active?}"
  end

end
