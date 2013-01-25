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

#############
# Node_role is an association class between a role in a proposal configuration and the
# node that is assigned that role. This supports a many2many association between
# roles and nodes, with some extra info.
# 
# The config attribute is a json blob of configuration specific to the role or node.
#
# This is currently used as a special case relation.  When role is nil, the node/role
# represents node specific configuration to the proposal_config.
#

class NodeRole < ActiveRecord::Base
  
  attr_accessible :config  # CB1 TODO remove - replaced by NodeAttributes
  attr_accessible :node_id, :role_id, :barclamp_instance_id
  belongs_to      :node
  belongs_to      :role
  # CB1
  belongs_to      :proposal_config
  # CB2
  belongs_to      :barclamp_instance, :class_name => "BarclampInstance", :foreign_key => "barclamp_instance_id"

  #
  # Helper functions to let the rest of the system operate on hash objects that 
  # are stored in the database json blobs.
  #
  def config_hash
    return {} unless self.config
    JSON::parse(self.config)
  end

  def config_hash=(newconfig)
    self.config = newconfig ? newconfig.to_json : nil
    save!
    self.config
  end
end
