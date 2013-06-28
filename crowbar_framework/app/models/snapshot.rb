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

class Snapshot < ActiveRecord::Base

  before_create   :set_name

  ERROR   = -1
  READY   = 0
  BLOCKED = 1
  ACTIVE  = 2
  UNKNOWN = nil

  attr_accessible :id, :name, :description, :order, 
  attr_accessible :deployement_id
  
  belongs_to      :deployment

  has_many        :deployments_roles, :dependent => :destroy
  alias_attribute :my_roles,          :deployments_roles
  has_many        :roles,             :through => :eployments_roles

  has_many        :nodes_roles,       :dependent => :destroy
  alias_attribute :my_nodes,          :nodes_roles
  has_many        :nodes,             :through => :nodes_roles
 
  def active?
    deployment.active_snapshot_id == self.id
  end

  def committed? 
    deployment.committed_snapshot_id == self.id
  end
  
  def proposed?
    deployment.proposed_snapshot_id == self.id
  end
 
  # review all the nodes for the nameshot and figure out an aggregated state
  def state
    active = 0
    my_nodes.each do |n|
      # any node that's error or unknown will cause the whole state to be in the other state
      case n.state
        when < 0 
          return ERROR
        when > 0 
          active+=1
        when nil
          return UNKNOWN
        end
    end
    # if all the nodes fall through the tests above then the snapshot is ready or active
    return (active == 0 ? READY : ACTIVE)
  end

  # returns a has with all the node status information (unready nodes only)
  def status
    s = {}
    # TODO ZEHICLE pull together all the nodes and aggregate status
    my_nodes.each do |n|
      # any node that's error or unknown will cause the whole state to be in the other state
      case n.state
        when < 0 
          s[n.id] = n.status
        when > 0 
          s[n.id] = n.status
        end
    end
    return s
  end
  
  ##
  # Clone this snapshot
  # optionally, change parent too
  def deep_clone(parent_deployment=nil, name=nil, with_nodes=true)

    new_snap = self.dup

    Snapshot.transaction begin    
      new_snap.deployment_id = parent_deployment.id if parent_deployment
      new_snap.name = name || "#{self.name}_#{self.id}"
      new_snap.save      
      # TODO ZEHICLE clone the roles
      # TODO ZEHICLE clone the nodes
    end

    new_snap
  end

  private

  def set_name
    name = Time.now.strftime("%Y-%M-%D %H:%M:%S")
    description = "name automatically set"
  end
end
