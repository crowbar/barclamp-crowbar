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

  before_create    :sane_defaults

  ERROR       = -1
  ACTIVE      = 0
  TODO        = 1
  BLOCKED     = 2
  TRANSITION  = 3
  PROPOSED    = nil

  attr_accessible :id, :name, :description, :order, :deployment_id
  
  belongs_to      :deployment

  has_many        :deployment_roles,  :dependent => :destroy
  has_many        :roles,             :through => :deployment_roles

  has_many        :node_roles,        :dependent => :destroy
  has_many        :nodes,             :through => :node_roles
 
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
      if n.state < 0 
        return ERROR
      elsif n.state > 0 
        active+=1
      elsif n.state == nil
        return UNKNOWN
      end
    end
    # if all the nodes fall through the tests above then the snapshot is ready or active
    return (active == 0 ? ACTIVE : TRANSITION)
  end

  # returns a has with all the node status information (unready nodes only)
  def status
    s = {}
    # any node that's error or unknown will cause the whole state to be in the other state  
    my_nodes.each { |n| s[n.id] = n.status unless n.state == 0 }
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

  def sane_defaults
    # make sure we have sane defaults
    self.name = "#{deployment.name} #{Time.now.strftime("%Y-%m-%d %H:%M:%S")}"
    self.description = deployment.description
    # Create the implicit deployment roles 
    Role.find(:all, :conditions=>['implicit = ?', true]) do |r|
      deployment_roles.build(:snapshot_id=>self.id, :role_id=>r.id, :data=>r.role_template)
    end
  end
end