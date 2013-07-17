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

############
# A 'deployment' of a barclamp represents a deployed (or deployable) cluster of roles 
#
# 'snapshots' is the history relation. It contains all historical deployments.
# active_snapshot is the currently active proposal snapshot (or queued or committing)
# proposed_snapshot is the most recently editted/created snapshot. (It might not be applied).
#

class Deployment < ActiveRecord::Base
  

  attr_accessible :name, :description, :barclamp_id, :active_snapshot_id
  attr_accessible :proposed_snapshot_id, :committed_snapshot_id

  validates_uniqueness_of   :name, :case_sensitive => false, :message => I18n.t("db.notunique", :default=>"Name item must be unique")
  validates_format_of       :name, :with=>/^[a-zA-Z][_a-zA-Z0-9]*$/, :message => I18n.t("db.lettersnumbers", :default=>"Name limited to [_a-zA-Z0-9]")
  
  has_many          :snapshots,           :inverse_of => :deployment, :dependent => :destroy
  
  has_one           :active_snapshot,     :class_name => "Snapshot", :primary_key => "active_snapshot_id", :foreign_key => 'id'
  alias_attribute   :active,              :active_snapshot

  has_one           :committed_snapshot,  :class_name => "Snapshot", :primary_key => "committed_snapshot_id", :foreign_key => 'id'
  alias_attribute   :committed,           :committed_snapshot
  
  has_one           :proposed_snapshot,   :class_name => "Snapshot", :primary_key => "proposed_snapshot_id", :foreign_key => 'id'

  belongs_to        :parent, :class_name => "Deployment", :primary_key => "parent_id"

  # active includes nothing being committed
  def active?
    !committed? && !active_snapshot_id.nil?
  end

  def committed?
    !committed_snapshot_id.nil?
  end

  def proposed?
    !proposed_snapshot_id.nil?
  end

  def proposed
    proposal
  end

  # return the relevant proposal for the deployment, if missing then create it
  def proposal
    raise "proposal should never be called for #{self.name}" if self.system?
    if committed?
      raise "cannot create proposal when Deployment is committed"
    elsif proposed_snapshot_id.nil?
      ps = Deployment.transaction do
        if active?                               # clone from active
          active_snapshot.deep_clone
        else                                        # create new
          # Create the snapshot 
          snapshots.build(:deployment_id=>self.id, :name => self.name, :description => self.description)
        end
      end
      ps.save!
      self.proposed_snapshot_id = ps.id
      self.save!
    end
    proposed_snapshot(true)     # use true to ensure we get the latest
  end

  # Helper to atomically recommit a currently active or committed snapshot.
  def recommit(&block)
    raise "Can only be called on a system deployment" unless system?
    raise "Recommit must be passed a block that will take a snapshot!" unless block_given?
    Deployment.transaction do
      new_c = committed_snapshot || active_snapshot.deep_clone
      block.call(new_c)
      new_c.save!
      self.committed_snapshot_id = new_c.id
      new_c.node_roles.each do |nr|
        nr.commit!
      end
      self.save!
      return committed_snapshot(true)
    end
  end

  # commit the current proposal (cannot be done if there is a committed proposal)
  def commit
    Deployment.transaction do
      raise I18n.t('deployment.commit.raise') if committed? 
      new_c = proposal     # promote this one
      committed_snapshot_id = new_c.id
      proposed_snapshot_id = nil
      save
      new_c.node_roles.each do |nr|
        nr.commit!
      end
    end
    committed_snapshot(true)
  end

  def activate_committed
    Deployment.transaction do
      self.active_snapshot_id = self.committed_snapshot_id
      self.committed_snapshot_id = nil
      self.save
    end
    active_snapshot(true)
  end
    
  # This is from the snapshot
  # TODO: good enough for now, but likely to change
  def state
    case
    when self.active? then active_snapshot.status
    when self.committed? then committed_snapshot.status
    when !self.proposed_snapshot_id.nil? then proposed_snapshot.status
    else 999
    end
  end
  
  #
  # UI Helper function to return a single string for status
  # XXX: This should really move to an helper module
  #
  def status
    case self.state
    when 0 
      'missing'
    when 1 
      'created'
    when 2 
      'pending'
    when 3 
      'unready'
    when 4 
      'failed'
    when 5 
      'ready'
    when 666 
      'deleted'
    when 999
      'inactive'
    else 
      'hold'
    end
  end

  def system?
    read_attribute("system")
  end
  
  # Lookup the deployment_roles available for the deployment, use the Proposal then Active 
  def deployment_roles
    return proposed_snapshot.deployment_roles if proposed? 
    return active_snapshot.deployment_roles if active?
    []
  end

  # Lookup the roles available for the deployment, use the Proposal then Active 
  def roles
    return proposed_snapshot.roles if proposed? 
    return active_snapshot.roles if active?
    []
  end

  # Add a role to a snapshot by creating the needed DeploymentRole
  # Returns a Role
  def add_role(role_name)
    p = proposal
    r = Role.find_by_name role_name
    DeploymentRole.create :role_id=>r.id, :snapshot_id=>p.id
  end

end
