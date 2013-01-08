# Copyright 2012, Dell
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
# A proposal is a configuration for a particular barclamp.
# It has a ""history"" of configurations that were created and applied.
#
# Proposal_configs is the history relation. It contains all historical configurations.
# active_config is the currently active proposal config (or queued or committing)
# current_config is the most recently editted/created proposal_config. (It might not be applied).
#
# Proposal usage:
#   When a barclamp is imported, a template proposal is created with a base configuration.
#   This is currently provided from the crowbar.yml and <barclamp>.json files.
#
#   When an instance of the barclamp is created, the template object is cloned to become the 
#   instance.  The deep_clone routine is used to build a complete deep copy of the template for
#   future editting.
#
#   As the proposal is editted, the deep copies of the proposal_configs are generated to 
#   represent changes overtime.
# 

class Proposal < ActiveRecord::Base

  VALIDATION_EXPR = /^[a-zA-Z][_a-zA-Z0-9]*$/
  
  attr_accessible :name, :last_applied_rev, :description

  validates_format_of :name, :with=>VALIDATION_EXPR, :message => I18n.t("db.lettersnumbers", :default=>"Name limited to [_a-zA-Z0-9]")
  validates_uniqueness_of :name, :scope => :barclamp_id, :case_sensitive => false, :message => I18n.t("db.notunique", :default=>"Name item must be unique")

  belongs_to :barclamp
  has_many  :proposal_configs, :inverse_of => :proposal

  belongs_to :active_config, :class_name => "ProposalConfig", :foreign_key => "active_config_id"
  belongs_to :current_config, :class_name => "ProposalConfig", :foreign_key => "current_config_id"

  def active?
    active_config != nil
  end

  #
  # UI Helper function to return a single string for status
  # XXX: This should really move to an helper module
  #
  def status
    if active?
      return 'unready' if active_config.committing?
      return 'pending' if active_config.queued?
      return 'ready' if active_config.applied?
      return 'failed' if active_config.failed?
    end
    'hold'
  end

  #
  # Deep clone routine.  In Rails3.1+, clone was changed to dup.
  #
  # Dup copies the object and recreates a new id.
  # Recursively copy the proposal_configs under the object.
  #
  # This would be used for two cases: 
  #   1. cloning the template for a new proposal during create.
  #   2. creating a copy of an existing proposal to seed a new barclamp instance.
  #
  def deep_clone(name)
    new_prop = self.dup
    new_prop.name = name
    new_prop.save!

    proposal_configs.each do |x| 
      new_x = x.deep_clone
      new_x.save!
      new_prop.proposal_configs << new_x
      new_prop.active_config = new_x if x == active_config
      new_prop.current_config = new_x if x == current_config
    end
    new_prop.save!

    new_prop
  end

  #####
  # provide a canonical method of representing a reference to a proposal configuration
  def as_reference
    { :barclamp => self.barclamp.name, :insatnce => self.name }
  end

  ####
  #  find a prposal configuration from its representation as a reference
  #
  def self.from_reference(ref)
    ProposalConfig.find_by_name_and_barclamp_id(ref[:barclamp], ref[:insatnce])
  end


end
