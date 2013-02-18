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
# A 'configuration' of a barclamp represents a deployed (or deployable) cluster of the 
# barclamp's technology (e.g. mySql, NTP, openstack Nova, etc)
# It has a ""history"" of configuration instances that were created and applied.
#
# 'instances' is the history relation. It contains all historical configurations.
# active_instance is the currently active proposal instance (or queued or committing)
# proposed_instance is the most recently editted/created instance. (It might not be applied).
#

class BarclampConfiguration < ActiveRecord::Base
  
  attr_accessible :name, :description, :order
  attr_accessible :barclamp_id, :active_instance_id, :proposed_instance_id

  validates_uniqueness_of :name, :scope => :barclamp_id, :case_sensitive => false, :message => I18n.t("db.notunique", :default=>"Name item must be unique")
  validates_format_of :name, :with=>/^[a-zA-Z][_a-zA-Z0-9]*$/, :message => I18n.t("db.lettersnumbers", :default=>"Name limited to [_a-zA-Z0-9]")
  validates_exclusion_of :name, :in => %w(template), :message => I18n.t("db.config_excludes", :default=>"Illegal config name")
  

  belongs_to      :barclamp
  has_many        :barclamp_instances,  :class_name => "BarclampInstance", :inverse_of => :barclamp_configuration, :dependent => :destroy
  alias_attribute :instances,           :barclamp_instances
  
  belongs_to :active_instance,          :class_name => "BarclampInstance", :foreign_key => "active_instance_id"
  alias_attribute :active,              :active_instance
  belongs_to :proposed_instance,        :class_name => "BarclampInstance", :foreign_key => "proposed_instance_id"
  alias_attribute :proposed,            :proposed_instance

  def active?
    active_instance_id != nil
  end

  #
  # UI Helper function to return a single string for status
  # XXX: This should really move to an helper module
  #
  def status
    state = (active_instance.nil? ? 999 : active_instance.status)
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
