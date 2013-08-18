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

  attr_accessible :id, :name, :description, :order, :deployment_id, :snapshot_id
  
  belongs_to      :deployment

  has_many        :deployment_roles,  :dependent => :destroy
  has_many        :roles,             :through => :deployment_roles

  has_many        :node_roles,        :dependent => :destroy
  has_many        :nodes,             :through => :node_roles
 
  has_one         :snapshot
  alias_attribute :next,              :snapshot


  def active?
    state == NodeRole::ACTIVE
  end

  def committed? 
    state == NodeRole::TODO
  end
  
  def proposed?
    state == NodeRole::PROPOSED
  end

  def tail?
    snapshot_id.nil?
  end

  class MissingJig < Exception
    def initalize(nr)
      @errstr = "NodeRole #{nr.name}: Missing jig #{nr.jig_name}"
    end
    def to_s
      @errstr
    end
    def to_str
      to_s
    end
  end
   
  # review all the nodes for the nameshot and figure out an aggregated state
  # options for snapshots are ACTIVE, PROPOSED and TODO
  def state
    state_map = Hash.new
    node_roles.each { |nr| state_map[nr.state] ||= true }
    case
    when state_map[NodeRole::ERROR] then NodeRole::ERROR
    when state_map[NodeRole::BLOCKED],
         state_map[NodeRole::TODO],
         state_map[NodeRole::TRANSITION]
      if state_map[NodeRole::TRANSITION]
        NodeRole::TRANSITION
      elsif !state_map[NodeRole::TODO]
        NodeRole::BLOCKED
      else
        NodeRole::TODO
      end
    when state_map[NodeRole::PROPOSED] then NodeRole::PROPOSED
    else NodeRole::ACTIVE
    end
  end

  # returns a hash with all the node status information (unready nodes only)
  def status
    s = {}
    # any node that's error or unknown will cause the whole state to be in the other state  
    my_nodes.each { |n| s[n.id] = n.status unless n.state == NodeRole::ACTIVE }
    return s
  end
  
    # commit the current proposal (cannot be done if there is a committed proposal)
  def commit
    raise I18n.t('deployment.commit.raise') unless proposed?
    NodeRole.transaction do
      node_roles.each { |nr| nr.commit! }
    end
    self
  end
  
  # create a new proposal from the this one
  def propose(name=nil)
    raise I18n.t('deployment.propose.raise') unless [NodeRole::ACTIVE, NodeRole::ERROR].include? state
    proposal = nil
    Deployment.transaction do
      # create the new proposal
      proposal = deep_clone(name)
      # move the pointer 
      deployment.snapshot_id = proposal.id
      deployment.save!
    end
    proposal
  end

  # attempt to stop a proposal that's in transistion by changing it's node_roles back to proposed
  def recall
    # first, we're going to create a new snapshot
    newsnap = self.deep_clone
    Snapshot.transaction do
      # then we block all the TODO items to prevent the annealer from moving forward
      self.node_roles.each do |nr|
        if nr.state == NodeRole::TODO
          nr.state = NodeRole::BLOCKED
          nr.status = I18n.t('recall_status', :scope=>'snapshot') 
          # we may need to remove all the node-role HABTM entries, but that seeems extreme right now
        end
      end
      self.name = I18n.t('recall', :name=>self.name, :scope=>'snapshot')
      self.save
      self.deployment.snapshot_id = newsnap.id
      self.deployment.save
    end
    newsnap
  end

  ##
  # Clone this snapshot.  It will also clone any node roles specific to this snapshot,
  # and take care of making sure that the node role dependency graph stays sane.
  def deep_clone(name=nil)
    Snapshot.transaction do
      newsnap = self.dup
      # build the linked list
      newsnap.snapshot_id = self.id
      newsnap.order += 1
      newsnap.name = name unless name.nil?
      newsnap.save!
      # collect the deployment roles
      self.deployment_roles.each do |dr|
        new_dr = dr.dup
        new_dr.snapshot = newsnap
        new_dr.save!
        newsnap.deployment_roles << new_dr
      end
      # collect the node roles
      node_role_map = Hash.new
      self.node_roles.each do |nr| 
        # we must create (not duplicate) the NR because of the state machine controls on setting state
        new_nr = NodeRole.create({:node_id=>nr.node_id, :role_id=>nr.role_id, :snapshot_id=>newsnap.id, :state=>NodeRole::PROPOSED, 
                      :data=>nr.read_attribute("data"), :wall=>nr.read_attribute("wall")}, 
                      :without_protection => true)
        # store the new nr because we need it for relationships (it's automatically linked to the snapshot when created)
        node_role_map[nr.id] = [nr,new_nr]
      end
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
      end
      newsnap.save!
      newsnap
    end
  end



end
