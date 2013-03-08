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
# A 'deployment' of a barclamp represents a deployed (or deployable) cluster of the 
# barclamp's technology (e.g. mySql, NTP, openstack Nova, etc)
# It has a ""history"" of deployment snapshots that were created and applied.
#
# 'snapshots' is the history relation. It contains all historical deployments.
# active_snapshot is the currently active proposal snapshot (or queued or committing)
# proposed_snapshot is the most recently editted/created snapshot. (It might not be applied).
#

class Deployment < ActiveRecord::Base
  
  attr_accessible :name, :description, :order
  attr_accessible :barclamp_id, :active_snapshot_id, :proposed_snapshot_id, :committed_snapshot_id

  validates_uniqueness_of :name, :scope => :barclamp_id, :case_sensitive => false, :message => I18n.t("db.notunique", :default=>"Name item must be unique")
  validates_format_of :name, :with=>/^[a-zA-Z][_a-zA-Z0-9]*$/, :message => I18n.t("db.lettersnumbers", :default=>"Name limited to [_a-zA-Z0-9]")
  validates_exclusion_of :name, :in => %w(template), :message => I18n.t("db.config_excludes", :default=>"Illegal config name")
  

  belongs_to      :barclamp
  has_many        :snapshots,           :inverse_of => :deployment, :dependent => :destroy
  
  belongs_to :active_snapshot,          :class_name => "Snapshot", :foreign_key => "active_snapshot_id"
  alias_attribute :active,              :active_snapshot

  belongs_to :committed_snapshot,       :class_name => "Snapshot", :foreign_key => "committed_snapshot_id"
  alias_attribute :committed,           :committed_snapshot
  alias_attribute :queued,              :committed_snapshot

  belongs_to :proposed_snapshot,        :class_name => "Snapshot", :foreign_key => "proposed_snapshot_id"
  alias_attribute :proposed,            :proposed_snapshot
  alias_attribute :proposal,            :proposed_snapshot

  # active includes nothing being committed
  def active?
    !committed and !active_snapshot_id.nil?
  end

  def committed?
    !committed_snapshot_id.nil?
  end
  
  # commit the current proposal (cannot be done if there is a committed proposal)
  def commit
    raise "cannot commit a proposal unless there is no other currently in process" if committed? 
    raise "proposed deployment is not valid" unless self.barclamp.is_valid? self
    Deployment.transaction do
      new_c = self.proposal     # promote this one
      new_p = new_c.deep_clone self, nil, true   # create a clone for the new proposal
      self.committed_snapshot_id = new_c.id
      self.proposed_snapshot_id = new_p.id
      self.save
    end
    self.committed
  end

  # this is pending functionality because multiple pre-reqs will be required
  # it is stubbed here because it was a CB1 method for the service object
  def delete
    raise "not implemented - this really should require a deallocate"
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
  
  #
  # UI Helper function to return a single string for status
  # XXX: This should really move to an helper module
  #
  def status
    state = (active_snapshot.nil? ? 999 : active_snapshot.status)
    case state
    when 0 
      'missing'
    when 1 
      'none'
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

end
