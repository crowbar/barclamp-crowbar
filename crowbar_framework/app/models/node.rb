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

class Node < ActiveRecord::Base
  
  before_save :update_fingerprint
  after_commit :default_group
  
  attr_accessible :name, :description, :order, :state, :fingerprint
  
  validates_uniqueness_of :name, :message => I18n.t("db.notunique", :default=>"Name item must be unique")
  validates_format_of :name, :with=>/^(([a-zA-Z]|[a-zA-Z][a-zA-Z0-9\-]*[a-zA-Z0-9])\.)*([A-Za-z]|[A-Za-z][A-Za-z0-9\-]*[A-Za-z0-9])$/, :message => I18n.t("db.fqdn", :default=>"Name must be a fully qualified domain name.")
  
  has_and_belongs_to_many :groups, :join_table => "node_groups", :foreign_key => "node_id", :order=>"[order], [name] ASC"
  
  belongs_to :os, :class_name => "Os" #, :foreign_key => "os_id"
  
  # Rob's list of CMDB attributes needed by the UI
    #alias
    #name
    #ip (list)
    #public_ip
    #mac
    #ipmi_enabled?
    #physical_drives (list)
    #memory (total)
    #cpu (type & count)
    #hardware (dmi product name)
    #raid_set
    #nics (list)
    #uptime
    #asset_tag
    #number_of_drives
    #physical_drives (list)
    #switch name, mac, port, unit
    #bios_set -> ["crowbar"]["hardware"]["bios_set"] 
    #get_bmc_user -> ["ipmi"]["bmc_user"] 
    #get_bmc_password-> ["ipmi"]["bmc_password"] 
    #bmc_address
  
  
  # Friendly name for the UI
  def alias
    (cmdb_get("alias") || name).split(".")[0]
  end

  def ready?
    state.eql? 'ready'
  end
  
  def admin?
    # TODO place holder
    false
  end

  def bmc_set?
    # TODO place holder
    true
  end
  
  def links
    # TODO place holder for barclamp defined links
    []
  end
  
  def allocated?
    # TODO place holder
    false
  end

  # Makes the open ended state information into a subset of items for the UI
  def status
    # if you add new states then you MUST expand the PIE chart on the nodes index page
    subState = !state.nil? ? state.split[0].downcase : ""
    case subState
    when "ready"
      "ready"     #green
    when "discovered", "wait", "waiting", "user", "hold", "pending", "input"
      "pending"   #flashing yellow
    when "discovering", "reset", "delete", "reinstall", "shutdown", "reboot", "poweron", "noupdate"
      "unknown"   #grey
    when "problem", "issue", "error", "failed", "fail", "warn", "warning", "fubar", "alert", "recovering"
      "failed"    #flashing red
    when "hardware-installing", "hardware-install", "hardware-installed", "hardware-updated", "hardware-updating"
      "building"  #yellow
    else
      "unready"   #spinner
    end
  end  

  def cmdb_get(attribute)
    puts "CMDB looking up #{attribute}"
    return nil
  end
  
  def method_missing(m,*args,&block)
    method = m.to_s
    if method.starts_with? "cmdb_"
      return cmdb_get method[5..100]
    else
      puts "Node #{name} #{method.inspect} #{args.inspect} #{block.inspect}"
      Rails.logger.fatal("Cannot delegate method #{m} to #{self.class}")
      throw "ERROR #{method} not defined for node #{name}"
    end
  end
  
  def <=>(other)
    # use Array#<=> to compare the attributes
    [self.order, self.name] <=> [other.order, other.name]
  end
  
  private
  
  def update_fingerprint
    self.fingerprint = self.name.hash
    self.state ||= 'unknown' 
  end  
  
  def default_group
    if groups.size == 0
      g = Group.find_or_create_by_name :name=>'not_set', :description=>I18n.t('not_set')
      groups << g
    end
  end

end
