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
  attr_accessible :config
  attr_accessible :reversion
  attr_accessible :applied
  belongs_to      :proposal, :inverse_of => :proposal_config
  has_many        :node_role
  has_many        :node, :through => :node_role


  #
  # This builds an old-time role hash for usage by the rest of the system for now
  # This will be chef code part of CMDB abstraction
  # 
  def to_role_object_hash
    proposal = {}

    proposal["id"] = "bc-#{proposal.barclamp.name}-#{proposal.name}"
    proposal["description"] = proposal.description
    proposal["attributes"] = config
    # GREG: Build deployment from node_roles and roles
    proposal["deployment"] = {}
    proposal
  end

end
