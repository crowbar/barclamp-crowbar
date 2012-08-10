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

class Proposal < ActiveRecord::Base
  attr_accessible :name, :status, :last_applied_rev, :description

  validates_format_of :name, :with=>/[a-zA-Z][_a-zA-Z0-9]/, :message => I18n.t("db.lettersnumbers", :default=>"Name limited to [_a-zA-Z0-9]")

  belongs_to :barclamp
  has_many  :proposal_configs, :inverse_of => :proposal

  belongs_to :active_config, :class_name => "ProposalConfig", :foreign_key => "active_config_id"
  belongs_to :current_config, :class_name => "ProposalConfig", :foreign_key => "current_config_id"

  def active?
    active_config != nil
  end

  def deep_clone
    new_prop = self.dup
    new_prop.save!

    proposal_configs.each do |x| 
      new_x = x.deep_clone
      new_x.save!
      new_prop.proposal_configs << new_x
      new_prop.active_config = new_x if x == active_config
      new_prop.current_config = new_x if x == current_config
    end

    new_prop
  end

end
