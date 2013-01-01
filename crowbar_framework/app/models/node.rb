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
  before_validation :default_population
  before_destroy    :cmdb_delete
  
  attr_accessible :name, :description, :alias, :order, :state, :admin, :allocated
  attr_readonly   :fingerprint
  
  # 
  # Validate the name should unique (no matter the case)
  # and that it starts with a valid FQDN
  #
  validates_uniqueness_of :name, :case_sensitive => false, :message => I18n.t("db.notunique", :default=>"Name item must be unique")
  validates_format_of :name, :with=>/^(([a-zA-Z]|[a-zA-Z][a-zA-Z0-9\-]*[a-zA-Z0-9]))*\.([A-Za-z]|[A-Za-z][A-Za-z0-9\-]*[A-Za-z0-9])*\.([A-Za-z]|[A-Za-z][A-Za-z0-9\-]*[A-Za-z0-9])$/, :message => I18n.t("db.fqdn", :default=>"Name must be a fully qualified domain name.")
  validates_length_of :name, :maximum => 255

  # TODO: 'alias' will move to DNS BARCLAMP someday, but will prob hang around here a while
  # validates_uniqueness_of :alias, :case_sensitive => false, :message => I18n.t("db.notunique", :default=>"Name item must be unique")
  validates_format_of :alias, :with=>/^([A-Za-z]|[A-Za-z][A-Za-z0-9\-]*[A-Za-z0-9])$/, :message => I18n.t("db.fqdn", :default=>"Name must be a fully qualified domain name.")
  validates_length_of :alias, :maximum => 100

  has_and_belongs_to_many :groups, :join_table => "node_groups", :foreign_key => "node_id", :order=>"[order], [name] ASC"

  has_many :node_attribs, :dependent => :destroy
  has_many :attribs, :through => :node_attribs
  
  belongs_to :os, :class_name => "Os" #, :foreign_key => "os_id"

  # for to_api_hash
  API_ATTRIBUTES = ["id", "name", "description", "order", "state", "fingerprint",
                    "admin", "allocated", "os_id", "created_at", "updated_at"]

  #
  # Find a set of nodes by role name
  #
  def self.find_by_role_name(role_name)
    role = Role.find_by_name(role_name)
    return [] unless role

    nrs = NodeRole.find_all_by_role_id(role.id)
    # Get the active ones
    nrs = nrs.select { |x| x.proposal_config_id == x.proposal_config.proposal.active_config_id }
    nrs.map { |x| x.node }
  end

  #
  # Create function that integrates with CMDB functions.
  #
  def self.create_with_cmdb(name)
    chef_node = NodeObject.find_node_by_name(name)
    node = Node.create(:name => name)
    node.admin = true if chef_node and chef_node.admin?
    node.save!
    unless chef_node
      cno = NodeObject.create_new name
      cno.crowbar["crowbar"] = {} if cno.crowbar["crowbar"].nil?
      cno.crowbar["crowbar"]["network"] = {} if cno.crowbar["crowbar"]["network"].nil?
      cno.save
    end
    node
  end

  # if there key is a hash, recurse.  Otherwise, take the addin value.
  def hash_merge!(base, addin)
    addin.keys.each do |key|
      if addin[key].is_a? Hash and base[key].is_a? Hash
        base[key] = hash_merge!(base[key], addin[key])
        next
      end
      base[key] = addin[key]
    end
    base
  end

  #
  # Update the CMDB view of the node at this point.
  #
  def update_cmdb
    # TODO - this should move into the CMDB object!
    cno = NodeObject.find_node_by_name(name)
    if cno
      cno.crowbar["state"] = self.state
      cno.crowbar["crowbar"] = {} unless cno.crowbar["crowbar"]
      cno.crowbar["crowbar"]["allocated"] = self.allocated

      # Get the active ones and merge into config
      nrs = NodeRole.find_all_by_node_id(self.id)
      nrs = nrs.select { |x| x.proposal_config_id == x.proposal_config.proposal.active_config_id }
      nrs.each { |nr| hash_merge!(cno.crowbar, nr.config_hash) }

      cno.rebuild_run_list
      cno.save
    end
  end

  def reset_cmdb_access
    if ["discovered","hardware-installed","hardware-updated","reset", "delete",
        "hardware-installing","hardware-updating","reinstall",
        "update","installing","installed"].member?(state) and !is_admin?
      Rails.logger.info("Crowbar transition: should be deleting a client entry for #{name}")
      client = ClientObject.find_client_by_name name
      Rails.logger.info("Crowbar transition: found and trying to delete a client entry for #{name}") unless client.nil?
      client.destroy unless client.nil?

      # Make sure that the node can be accessed by knife ssh or ssh
      if ["reset","reinstall","update","delete"].member?(state)
        system("sudo rm /root/.ssh/known_hosts")
      end
    end
  end

  def cmdb_hash
    NodeObject.find_node_by_name name 
  end

  #
  # This is an hack for now.
  # XXX: Once networking is better defined, we should use those routines
  #
  def address(net = "admin")
    cmdb_hash.address(net)
  end

  def public_ip
    cmdb_hash.public_ip
  end

  #
  # XXX: Remove this as we better.
  #
  alias :super_save :save
  def save
    update_cmdb
    super_save
  end

  #
  # XXX: Remove this as we better.
  #
  alias :super_save! :save!
  def save!
    update_cmdb
    super_save!
  end
  
  #
  # Helper function to test admin without calling admin. Style-thing.
  #
  def is_admin?
    admin
  end

  #
  # Helper function for allocated
  #
  def allocated?
    self.allocated
  end

  def allocate
    self.allocated = true
    save
  end

  
  #
  # Helper function to set state.  Assumes that the node will be save outside if this routine.
  #
  # Use transition function to set state.
  #
  def set_state(new_state, old_state = nil)
    # use the real transition function for this
    cb = CrowbarService.new Rails.logger
    cb.transition "default", name, new_state, old_state
  end

  def ipmi_cmd(cmd)
    bmc          = cmdb_hash.address("bmc").addr rescue nil
    bmc_user     = cmdb_hash.get_bmc_user
    bmc_password = cmdb_hash.get_bmc_password
    system("ipmitool -I lanplus -H #{bmc} -U #{bmc_user} -P #{bmc_password} #{cmd}") unless bmc.nil?
  end

  def reboot
    set_state("reboot")
    ipmi_cmd("power cycle")
  end

  def shutdown
    set_state("shutdown")
    ipmi_cmd("power off")
  end

  def poweron
    set_state("poweron")
    ipmi_cmd("power on")
  end

  def identify
    ipmi_cmd("chassis identify")
  end

  def run_list(pending = true)
    nrs = NodeRole.find_all_by_node_id(self.id)
    # Get the active ones
    nrs = nrs.select { |x| x.proposal_config_id == x.proposal_config.proposal.active_config_id }
    nrs.map { |x| x.role }
  end

  # XXX: Make this better one day.  Perf is not good.  Direct select would be better
  # A custom query should be able to build the list straight up.
  #
  # update_run_list:
  #   Rebuilds the run_list for the CMDB system for this node based upon its active proposal
  #   membership and its state.
  #
  #   This includes updating the CMDB node role with node specific data.
  #
  def update_run_list
    nrs = NodeRole.find_all_by_node_id(self.id)
    # Get the active ones
    nrs = nrs.select { |x| x.proposal_config_id == x.proposal_config.proposal.active_config_id }

    # For each of the roles
    cno = cmdb_hash
    cno.clear_run_list_map
    nrs.each do |nr|
      if nr.role
        # This is node role that defines run_list entry
        cno.add_to_run_list(nr.role.name, nr.role.barclamp.cmdb_order, nr.role.states.split(","))
        config_name = "#{nr.role.barclamp.name}-config-#{nr.proposal_config.proposal.name}"
        cno.add_to_run_list(config_name, nr.role.barclamp.cmdb_order, ["all"])
      end
      # Has custom data.
      if nr.config
        hash = nr.config_hash
        cno.crowbar.merge(hash)
      end
    end
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
  
  
  # Friendly name for the UI

  def ready?
    state.eql? 'ready'
  end
  
  def virtual?
    cmdb_hash.virtual?
  end
  
  def bmc_set?
    # TODO place holder
    true
  end
  
  def links
    # TODO place holder for barclamp defined links
    []
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

  # retrieves the attribute from nodeattribute
  # NOTE: for safety, will create the association if it is missing
  def attrib_get(attrib)
    a = Attrib.find_or_create_by_name(:name=>attrib, :description=>I18n.t('model.attribs.node.default_create_description'))
    return NodeAttrib.find_or_create_by_node_and_attrib(self, a)
  end
  
  def cmdb_get(attrib)
    puts "DEPRICATED 12/26/12+90 cmdb_get #{attrib}"
    attrib_get attrib
  end
  
  def cmdb_set(attrib, value=nil)
    puts "DEPRICATED 12/26/12+90 cmdb_set #{attrib}=#{value}"
    attrib_set attrib, value
  end
  
  def attrib_set(attrib, value=nil)
    a = Attrib.find_or_create_by_name(:name=>attrib, :description=>I18n.t('model.attribs.node.default_create_description'))
    na = NodeAttrib.find_or_create_by_node_and_attrib(self, a)
    na.actual = value
    na.save
    na
  end
  
  def method_missing(m,*args,&block)
    method = m.to_s
    if method.starts_with? "attrib_"
      return attrib_get(method[7..100]).value
    elsif method.starts_with? "cmdb_"
      return attrib_get(method[5..100]).value
    else
      Rails.logger.fatal("Cannot delegate method #{m} to #{self.class}")
      throw "ERROR #{method} not defined for node #{name}"
    end
  end
  
  def <=>(other)
    # use Array#<=> to compare the attributes
    [self.order, self.name] <=> [other.order, other.name]
  end

  def to_s
    "Node: #{name}"
  end

  # customize node hash delivered through api (as json)
  # maybe a mixin, later...
  def self.to_api_hash(attribute_hash)
    attribute_hash.reject{|k,v| !API_ATTRIBUTES.include?(k) }
  end

  def ip
    #TODO reference cmdb_hash.ip somehow?
    "unknown"
  end

  def group
    groups[0]
  end

  def group= group
    db_group = group.is_a?(Group) ? group : Group.find_or_create_by_name({'name' => group, 'description' => group, 'category' => 'ui'})
    if db_group
      category = db_group.category
      groups.each { |g| g.nodes.delete(self) if g.category.eql?(category) }
      groups << db_group unless db_group.nodes.include? self
    end
  end

  private

  # make sure we do housekeeping before we remove the DB object
  def cmdb_delete
    Cmdb.all.each { |c| c.delete_node(self) }
  end

  
  # make sure some safe values are set for the node
  def default_population
    self.fingerprint = self.name.hash
    self.name = self.name.downcase
    self.alias = self.name.split(".")[0]
    self.state ||= 'unknown' 
    if self.groups.size == 0
      g = Group.find_or_create_by_name :name=>'not_set', :description=>I18n.t('not_set', :default=>'Not Set')
      self.groups << g rescue nil
    end
  end

end
