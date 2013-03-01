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

  def active?
    active_snapshot_id != nil
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
