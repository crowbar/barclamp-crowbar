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

  ARCHIVED = -1
  PROPOSED = 0
  COMMITTED = 1
  ACTIVE = 2
  ERROR = 3
  STATES = {
    ARCHIVED => "archived",
    PROPOSED => "proposed",
    COMMITTED => "committed",
    ACTIVE => "active",
    ERROR => "error"
  }
  
  attr_accessible :id, :name, :description, :order, :deployment_id, :snapshot_id
  
  belongs_to      :deployment

  has_many        :deployment_roles,  :dependent => :destroy
  has_many        :roles,             :through => :deployment_roles

  has_many        :node_roles,        :dependent => :destroy
  has_many        :nodes,             :through => :node_roles
 
  has_one         :snapshot
  alias_attribute :next,              :snapshot

  def self.state_name(s)
    raise("#{state || 'nil'} is not a valid Snapshot state!") unless s and STATES.include? s
    I18n.t(STATES[s], :scope=>'node_role.state')
  end

  def state_name(s)
    self.class.state_name(s)
  end

  def active?
    committed? &&
      node_roles.committed.not_in_state(NodeRole::ACTIVE).count == 0
  end

  def committed?
    read_attribute('state') == COMMITTED
  end
  
  def proposed?
    read_attribute('state') == PROPOSED
  end

  def archived?
    read_attribute('state') == ARCHIVED
  end

  def annealable?
    committed? && !active? && !error?
  end

  def proposable?
    active? && !deployment.system?
  end

  def error?
    committed? &&
      node_roles.committed.in_state(NodeRole::ERROR).count > 0
  end

  def archive
    write_attribute("state",ARCHIVED)
    save!
  end

  def state
    s = read_attribute("state")
    
    return s if s == ARCHIVED || s == PROPOSED
    return ACTIVE if active?
    return ERROR if error?
    COMMITTED
  end

  def tail?
    snapshot_id.nil?
  end

  def parent
    Snapshot.where(:snapshot_id => id).first
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

  # returns a hash with all the snapshot error status information 
  def status
    node_roles.each { |nr| s[nr.id] = nr.status if nr.error?  }
  end
  
    # commit the current proposal (cannot be done if there is a committed proposal)
  def commit
    raise I18n.t('deployment.commit.raise', :default=>'blocked: already 1 committed') unless proposed?
    NodeRole.transaction do
      node_roles.in_state(NodeRole::PROPOSED).each { |nr| nr.commit! }
    end
    parent.archive unless parent.nil?
    write_attribute("state",COMMITTED)
    save!
    Run.run!
    self
  end
  
  # create a new proposal from the this one
  def propose(name=nil)
    raise "Snapshot #{name} not ACTIVE, cannot create a new snapshot from it!" unless active?
    raise "Cannot propose a new snapshot for the system deployment!" if deployment.system?
    proposal = nil
    Deployment.transaction do
      # create the new proposal
      proposal = deep_clone(name)
      # move the pointer 
      deployment.snapshot_id = proposal.id
      archive
      deployment.save!
    end
    proposal
  end

  def recallable?
    !deployment.system?
  end
  
  # attempt to stop a proposal that's in transistion.
  # Do this by changing its state from COMMITTED to PROPOSED.
  def recall
    raise "Cannot recall a system deployment" unless recallable?
    write_attribute("state",PROPOSED)
    save!
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
      newsnap.send(:write_attribute,"state",PROPOSED)
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
        Rails.logger.info("Snapshot: duplicating NodeRole #{nr.id}")
        new_nr = nr.dup
        new_nr.snapshot = newsnap
        # store the new nr because we need it for relationships (it's automatically linked to the snapshot when created)
        node_role_map[nr.id] = [nr,new_nr]
      end
      node_role_map.each do |id,nr_array|
        old_nr = nr_array[0]
        new_nr = nr_array[1]
        old_nr.children.each do |c_nr|
          new_nr.children << (node_role_map[c_nr.id][1] || c_nr rescue c_nr)
        end
        old_nr.parents.each do |p_nr|
          new_nr.parents << (node_role_map[p_nr.id][1] || p_nr rescue p_nr)
        end
        new_nr.save!
      end
      newsnap.save!
      newsnap
    end
  end
end
