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
  
  attr_accessible :name, :description, :order, :state, :admin, :allocated
  
  validates_uniqueness_of :name, :message => I18n.t("db.notunique", :default=>"Name item must be unique")
  validates_format_of :name, :with=>/^(([a-zA-Z]|[a-zA-Z][a-zA-Z0-9\-]*[a-zA-Z0-9])\.)*([A-Za-z]|[A-Za-z][A-Za-z0-9\-]*[A-Za-z0-9])$/, :message => I18n.t("db.fqdn", :default=>"Name must be a fully qualified domain name.")
  
  has_and_belongs_to_many :groups, :join_table => "node_groups", :foreign_key => "node_id", :order=>"[order], [name] ASC"
  
  belongs_to :os, :class_name => "Os" #, :foreign_key => "os_id"
  
  def is_admin?
    admin
  end

  def node_object
    NodeObject.find_node_by_name name
  end

  # This is an hack for now.
  def address(net = "admin")
    node_object.address(net)
  end

  def set_state(new_state)
    state = new_state
    cno = NodeObject.find_node_by_name name
    cno.crowbar["state"] = state
    cno.save
  end

  # GREG: Make this better one day.  Perf is not good.  Direct select would be better
  # A custom query should be able to build the list straight up.
  def update_run_list
    nrs = NodeRole.find_all_by_node_id(self.id)
    # Get the active ones
    nrs = nrs.select { |x| x.proposal_config_id == x.proposal_config.proposal.active_config_id }

    # For each of the roles
    cno = node_object
    cno.clear_run_list_map
    nrs.each do |nr|
      next unless nr.role
      cno.add_to_run_list(nr.role.name, nr.role.barclamp.cmdb_order, nr.role.states.split(","))
      config_name = "#{nr.role.barclamp.name}-config-#{nr.proposal_config.proposal.name}"
      # GREG: All? or something aggregated?
      cno.add_to_run_list(config_name, nr.role.barclamp.cmdb_order, ["all"])
    end
    # GREG: Still need to think about node specific data.
    cno.save
  end

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

end
