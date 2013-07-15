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
    state_map = Hash.new
    node_roles.each do |nr|
      state_map[nr.state] = true
    end
    case
    when state_map[NodeRole::ERROR] then NodeRole::ERROR
    when state_map[NodeRole::BLOCKED] ||
        state_map[NodeRole::TODO] ||
        state_map[NodeRole::TRANSITION]
      NodeRole::TODO
    when state_map[NodeRole::PROPOSED] then NodeRole::PROPOSED
    else NodeRole::ACTIVE
    end
  end

  # returns a has with all the node status information (unready nodes only)
  def status
    s = {}
    # any node that's error or unknown will cause the whole state to be in the other state  
    my_nodes.each { |n| s[n.id] = n.status unless n.state == 0 }
    return s
  end
  
  ##
  # Clone this snapshot.  It will also clone any node roles specific to this snapshot,
  # and take care of making sure that the node role dependency graph stays sane.
  def deep_clone
    Snapshot.transaction do
      newsnap = self.dup
      node_role_map = Hash.new
      self.node_roles.each{|nr| node_role_map[nr.id] = [nr,nr.dup]}
      node_role_map.each do |id,nr_array|
        old_nr = nr_array[0]
        new_nr = nr_array[1]
        old_nr.children.each do |c_nr|
          new_nr.children << (node_role_map[c_nr.id][1] || nil rescue nil) || c_nr
        end
        old_nr.parents.each do |p_nr|
          new_nr.parents << (node_role_map[p_nr.id][1] || nil rescue nil) || p_nr
        end
        new_nr.snapshot = newsnap
        new_nr.save!
        newsnap.node_roles << new_nr
      end
      self.deployment_roles.each do |dr|
        new_dr = dr.dup
        new_dr.snapshot = newsnap
        new_dr.save!
        newsnap.deployment_roles << new_dr
      end
      newsnap.save!
      newsnap
    end
  end
end
