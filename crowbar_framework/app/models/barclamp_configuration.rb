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
# A 'configuration' of a barclamp represents a deployed (or deployable) cluster of the 
# barclamp's technology (e.g. mySql, NTP, openstack Nova, etc)
# It has a ""history"" of configuration instances that were created and applied.
#
# 'instances' is the history relation. It contains all historical configurations.
# active_config is the currently active proposal config (or queued or committing)
# current_config is the most recently editted/created instance. (It might not be applied).
#
# Configuraiton usage:
#   When a barclamp is imported, a template configuration is created with a default values.
#   The barclamp provides these defaults in the crowbar.yml and <barclamp>.json files.
#
#   When a 'configuration' of the barclamp is created, the template object is cloned.
#   The deep_clone routine is used to build a complete deep copy of the template for
#   future editting.
#
#   As the configuration is editted, 'instances' are saved to capture changes overtime.
# 

class BarclampConfiguration < ActiveRecord::Base
  
  attr_accessible :name, :last_applied_rev, :description

  validates_format_of :name, :with=>/^[a-zA-Z][_a-zA-Z0-9]*$/, :message => I18n.t("db.lettersnumbers", :default=>"Name limited to [_a-zA-Z0-9]")

  belongs_to :barclamp
  has_many  :barclamp_instances, :class_name => "BarclampInstance", :inverse_of => :conifguration

  belongs_to :active_config, :class_name => "BarclampInstance", :foreign_key => "active_config_id"
  belongs_to :current_config, :class_name => "BarclampInstance", :foreign_key => "current_config_id"

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
  # Recursively copy the 'instances' under the object.
  #
  # This would be used for two cases: 
  #   1. cloning the template for a new configuration during create.
  #   2. creating a copy of an existing configuration to seed a new barclamp.
  #
  def deep_clone
    new_prop = self.dup
    new_prop.save!

    instances.each do |x| 
      new_x = x.deep_clone
      new_x.save!
      new_prop.instances << new_x
      new_prop.active_config = new_x if x == active_config
      new_prop.current_config = new_x if x == current_config
    end

    new_prop
  end

end
