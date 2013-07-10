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
    if committed?
      raise "cannot create proposal when Deployment is committed"
    elsif proposed_snapshot_id.nil?
      ps = Deployment.transaction do
        if active?                               # clone from active
          active_snapshot.deep_clone
        else                                        # create new
          # Create the snapshot 
          snapshots.build(:deployment_id=>self.id)
        end
      end
      ps.save!
      self.proposed_snapshot_id = ps.id
      self.save!
    end
    proposed_snapshot(true)     # use true to ensure we get the latest
  end

  # commit the current proposal (cannot be done if there is a committed proposal)
  def commit
    raise I18n.t('deployment.commit.raise') if committed? 
    Deployment.transaction do
      new_c = self.proposal     # promote this one
      self.committed_snapshot_id = new_c.id
      self.proposed_snapshot_id = nil
      self.save
    end
    self.committed
  end

  # recall the committing proposal (simply deleted the committed proposal)
  def recall
    Deployment.transaction do
      old_c = self.committed     
      self.committed_snapshot_id = nil
      old_c.delete
      self.save
    end
  end

  # moves the commited proposal to the applied spot (deleted the old apply)
  # this should only be called by a Jig!
  def activate_committed
    Deployment.transaction do
      old_a = self.active
      self.active_snapshot_id = self.committed_snapshot_id
      self.committed_snapshot_id = nil
      self.save
      old_a.delete unless old_a.nil?
    end
    self.active
  end


  # commits a clone of the APPLIED snapshot without nodes
  # this can be used to effectively halt a deployment as a way of deleting it
  def commit_deallocate_active
    # commit_deallocated_active
    raise "cannot deallocate active unless there is no other committed work" if committed?
    Deployment.transaction do
      old_a = self.active
      new_c = old_a.deep_clone self, "Deallocating #{old_a.name}", false
      self.committed_snapshot_id = new_c.id
      self.save
    end
    self.committed
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
