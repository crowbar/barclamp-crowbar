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
class ProposalConfig < ActiveRecord::Base
  attr_accessible :config, :reversion, :applied, :failed, :failed_reason
  belongs_to      :proposal, :inverse_of => :proposal_config
  has_many        :node_role
  has_many        :node, :through => :node_role

  def failed?
    failed
  end

  def applied?
    applied
  end

  def config_hash
    JSON::parse(config)
  end

  def config_hash=(chash)
    config = chash.to_json
    save!
  end

  def deep_clone
    new_config = self.dup
    new_config.save

#    node_role.each do |nr|
#      new_nr = nr.deep_clone
#      new_nr.save
#      new_config.node_role << new_nr
#    end

    new_config
  end

  #
  # This builds an old-time role hash for usage by the rest of the system for now
  # This will be chef code part of CMDB abstraction
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

    # GREG: Build deployment from node_roles and roles
    phash["deployment"][bc_name]["elements"] = {}

    phash
  end

end
