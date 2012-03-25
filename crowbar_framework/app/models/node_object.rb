# Copyright 2011, Dell
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
# Author: RobHirschfeld
#
class NodeObject < ChefObject
  extend CrowbarOffline

  def self.find(search)
    answer = []
    if CHEF_ONLINE
      nodes = if search.nil?
        ChefObject.query_chef.search "node"
      else
        ChefObject.query_chef.search "node", "#{chef_escape(search)}"
      end
      if nodes[2] != 0 and !nodes[0].nil?
        nodes[0].delete_if { |x| x.nil? }
        answer = nodes[0].map do |x|
          NodeObject.new x
        end
        answer.delete_if { |x| !x.has_chef_server_roles? }
      end
    else
      files = offline_search 'node-', (search.nil? ? '' : search[5..100])
      answer = files.map! { |f| NodeObject.new(recover_json(f)) }
    end
    return answer
  end

  def has_chef_server_roles?
      return !@role.nil?
  end

  def self.find_all_nodes
    self.find nil
  end

  def self.find_nodes_by_name(name)
    self.find "name:#{chef_escape(name)}"
  end

  def self.find_node_by_alias(name)
    nodes = if CHEF_ONLINE 
      self.find "alias:#{chef_escape(name)}"
    else
      self.find_all_nodes.keep_if { |n| n.alias==name }
    end
    if nodes.length == 1
      return nodes[0]
    elsif nodes.length == 0
      nil
    else
      raise "#{I18n.t('model.fail.multiple_node')}: #{nodes.join(',')}"
    end
  end
  
  def self.find_node_by_name(name)
    val = if CHEF_ONLINE
      name += ".#{ChefObject.cloud_domain}" unless name =~ /(.*)\.(.)/
      ChefObject.crowbar_node(name)
    else
      name += ".#{OFFLINE_DOMAIN || ChefObject.cloud_domain}" unless name =~ /(.*)\.(.)/
      self.recover_json(self.nfile('node',name))
    end
    return val.nil? ? nil : NodeObject.new(val)
  end
  def self.all
    self.find nil
  end

  def self.make_role_name(name)
    "crowbar-#{name.gsub(".", "_")}"
  end

  def self.create_new_role(new_name, machine)
    name = make_role_name new_name
    if CHEF_ONLINE
      role = RoleObject.new Chef::Role.new
    else
      self.create_object 'role', name, "NodeObject Create New Role"
      role = RoleObject.find_role_by_name(name)
    end
    role.name = name
    role.default_attributes["crowbar"] = {}
    role.default_attributes["crowbar"]["network"] = {} if role.default_attributes["crowbar"]["network"].nil?
    role.save

    # This run_list call is to add the crowbar tracking role to the node. (SAFE)
    machine.run_list.run_list_items << "role[#{role.name}]"
    machine.save

    role
  end

  def self.create_new(new_name)
    machine = Chef::Node.new
    machine.name "#{new_name}"
    machine["fqdn"] = "#{new_name}"
    role = RoleObject.find_role_by_name NodeObject.make_role_name(new_name)
    role = NodeObject.create_new_role(new_name, machine) if role.nil?
    NodeObject.new machine
  end

  # depricate!  This is not a good way to do this!
  def self.human_attribute_name(attrib)
    Rails.logger.info("please change call to human_attribute_name for #{attrib} this call is depricated!")
    I18n.t attrib, :scope => "model.attributes.node"
  end

  def initialize(node)
    @role = RoleObject.find_role_by_name NodeObject.make_role_name(node.name)
    if @role.nil?
      # An admin node can exist without a role - so create one
      if !node["crowbar"].nil? and node["crowbar"]["admin_node"]
        @role = NodeObject.create_new_role(node.name, node)
      else
        Rails.logger.fatal("Node exists without role!! #{node.name}")
        NodeObject.offline_remove('node', node.name) if !CHEF_ONLINE
      end
    end
    @node = node
  end

  def has_node?
    !@node.nil?
  end

  def shortname
    Rails.logger.warn("shortname is depricated!  Please change this call to use handle or alias")
    name.split('.')[0]
  end

  def name
    @node.nil? ? 'unknown' : @node.name
  end

  def handle
    begin name.split('.')[0] rescue name end
  end
  
  def alias(suggest=false) 
    if display_set? 'alias'
      display['alias']
    else
      fallback = name.split('.')[0]
      fallback = default_loader['alias'] || fallback if suggest and !display_set? 'alias'
      fallback
    end
  end

  def alias=(value)
    value = value.strip.sub(/\s/,'-')
    # valid DNS Name
    if !(value =~ /^(([a-zA-Z]|[a-zA-Z][a-zA-Z0-9\-]*[a-zA-Z0-9])\.)*([A-Za-z]|[A-Za-z][A-Za-z0-9\-]*[A-Za-z0-9])$/)
      Rails.logger.warn "Alias #{value} not saved because it did not conform to valid DNS hostnames"
      raise "#{I18n.t('model.node.invalid_dns')}: #{value}"
    elsif value.length+ChefObject.cloud_domain.length>62  #62+dot = 63
      Rails.logger.warn "Alias #{value}.#{ChefObject.cloud_domain} FQDN not saved because it exceeded the 63 character length limit"
      raise "#{I18n.t('model.node.too_long_dns')}: #{value}.#{ChefObject.cloud_domain}"
    else
      # don't allow duplicate alias
      node = NodeObject.find_node_by_alias value 
      if node and !node.handle.eql?(handle)
        Rails.logger.warn "Alias #{value} not saved because #{node.name} already has the same alias."
        raise I18n.t('model.node.duplicate_alias') + ": " + node.name
      else
        set_display "alias", value
        @role.description = chef_description
      end
    end
  end
  
  def description(suggest=false, use_name=false)
    d = if display_set? 'description'
      display['description']
    elsif suggest
      default_loader['description']
    else
      nil
    end
    (use_name ? "#{d || ""} [#{name}]" : d)
  end

  def description=(value)
    set_display "description", value
    @role.description = chef_description
  end
  
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

  def ready?
    state === 'ready'
  end

  def state
    return 'unknown' if (@node.nil? or @role.nil?)
    if self.crowbar['state'] === 'ready' and CHEF_ONLINE and @node['ohai_time']
      since_last = Time.now.to_i-@node['ohai_time'].to_i
      return 'noupdate' if since_last > 1200 # or 20 mins
    end
    return self.crowbar['state']
  end

  def ip
    net_info = get_network_by_type("admin")
    return net_info["address"] unless net_info.nil?
    @node["ipaddress"] || (I18n.t :unknown)
  end

  def public_ip
    net_info = get_network_by_type("public")
    return net_info["address"] unless net_info.nil?
    @node["ipaddress"] || (I18n.t :unknown)
  end

  def crowbar_ohai
    nil if @node.nil?
    @node.automatic_attrs["crowbar_ohai"]
  end

  def mac
    unless @node["crowbar"].nil? or self.crowbar_ohai["switch_config"].nil?
      intf = sort_ifs[0]
      self.crowbar_ohai["switch_config"][intf]["mac"] || (I18n.t :unknown)
    else
      (I18n.t :not_set)
    end
  end

  def allocated
    (@node.nil? or @role.nil?) ? false : self.crowbar["crowbar"]["allocated"]
  end

  def allocated=(value)
    return false if @role.nil?
    self.crowbar["crowbar"]["allocated"] = value
  end

  def allocated?
    (@node.nil? or @role.nil?) ? false : self.crowbar["crowbar"]["allocated"]
  end

  def ipmi_enabled?
    #placeholder until we have a better mechanism
    @node.nil? ? false : @node["crowbar"]["allocated"]
  end

  def rename(value, domain)
    return "unknown" if @node.nil?
    @node.name value
    @node[:fqdn] = value
    @node[:domain] = domain
    # This modifying of the run_list is to handle change the name of the crowbar tracking role (SAFE)
    @node.run_list.run_list_items.delete "role[#{@role.name}]"
    @role.name = "crowbar-#{value.gsub(".", "_")}"
    @node.run_list.run_list_items << "role[#{@role.name}]"
    @node.save
    save
  end

  def memory
    @node['memory']['total'] rescue nil
  end

  def cpu
    @node['cpu']['0']['model_name'].squeeze(" ").strip rescue nil
  end

  def uptime
    @node["uptime"]
  end

  def asset_tag
    @node["dmi"]["chassis"]["serial_number"] rescue nil
  end

  def number_of_drives
    self.crowbar['crowbar']['disks'].length rescue -1
  end

  def [](attrib)
    return nil if @node.nil?
    @node[attrib]
  end

  # Function to help modify the run_list.
  def crowbar_run_list(*args)
    return nil if @role.nil?
    args.length > 0 ? @role.run_list(args) : @role.run_list
  end

  def add_to_run_list(rolename, priority, states = nil)
    states = [ "all" ] unless states
    crowbar["run_list_map"] = {} if crowbar["run_list_map"].nil?
    crowbar["run_list_map"][rolename] = { "states" => states, "priority" => priority }

    # only rebuild the run_list if it effects the current state.
    self.rebuild_run_list if states.include?("all") or states.include?(self.crowbar['state'])
  end

  def delete_from_run_list(rolename)
    crowbar["run_list_map"] = {} if crowbar["run_list_map"].nil?
    crowbar["run_list_map"][rolename] = { "states" => [ "all" ], "priority" => -1001 } unless crowbar["run_list_map"].nil?
    crowbar_run_list.run_list_items.delete "role[#{rolename}]"
  end

  def rebuild_run_list
    crowbar["run_list_map"] = {} if crowbar["run_list_map"].nil?

    # Cull by state
    map = crowbar["run_list_map"].select { |k,v| v["states"].include?("all") or v["states"].include?(self.crowbar['state']) }
    # Sort map
    vals = map.sort { |a,b| a[1]["priority"] <=> b[1]["priority"] }

    # Rebuild list
    crowbar_run_list.run_list_items.clear
    vals.each do |item|
      next if item[1]["priority"] == -1001 # Skip deleted items
      crowbar_run_list.run_list_items << "role[#{item[0]}]"
    end
  end

  def run_list_to_roles
    crowbar["run_list_map"] = {} if crowbar["run_list_map"].nil?
    a = crowbar["run_list_map"].select { |k,v| v["priority"] != -1001 }
    a.collect! { |x| x[0] }
  end

  def crowbar
    @role.default_attributes
  end

  def crowbar=(value)
    return nil if @role.nil?
    @role.default_attributes = value
  end

  # This include a map walk
  def role?(role_name)
    return false if @node.nil?
    return false if @role.nil?
    crowbar["run_list_map"] = {} if crowbar["run_list_map"].nil?
    return true if crowbar["run_list_map"][role_name]
    @node.role?(role_name)
  end

  def roles
    @node['roles'].nil? ? nil : @node['roles'].sort
  end

  def recursive_merge!(b, h)
    b.merge!(h) {|key, _old, _new| if _old.class.kind_of? Hash.class then recursive_merge(_old, _new) else _new end  }
  end

  def save
    if @role.default_attributes["crowbar-revision"].nil?
      @role.default_attributes["crowbar-revision"] = 0
      Rails.logger.debug("Starting Node Revisions: #{@node.name} - unset")
    else
      Rails.logger.debug("Starting Node Revisions: #{@node.name} - #{@role.default_attributes["crowbar-revision"]}")
      @role.default_attributes["crowbar-revision"] = @role.default_attributes["crowbar-revision"] + 1
    end
    Rails.logger.debug("Saving node: #{@node.name} - #{@role.default_attributes["crowbar-revision"]}")

    recursive_merge!(@node.normal_attrs, @role.default_attributes)

    if CHEF_ONLINE
      @role.save
      @node.save
    else
      NodeObject.offline_cache(@node, NodeObject.nfile('node', @node.name))
      NodeObject.offline_cache(@role, RoleObject.nfile('role', @role.name))
    end
    Rails.logger.debug("Done saving node: #{@node.name} - #{@role.default_attributes["crowbar-revision"]}")
  end

  def destroy
    Rails.logger.debug("Destroying node: #{@node.name} - #{@role.default_attributes["crowbar-revision"]}")
    @role.destroy
    @node.destroy
    Rails.logger.debug("Done with removal of node: #{@node.name} - #{@role.default_attributes["crowbar-revision"]}")
  end

  def networks
    self.crowbar["crowbar"]["network"] rescue {}
  end

  def get_network_by_type(type)
    return nil if @role.nil?
    networks.each do |intf, data|
      return data if data["usage"] == type
    end
    nil
  end

  #
  # This is from the crowbar role assigned to the admin node at install time.
  # It is not a node.role parameter
  #
  def admin?
    return false if @node.nil?
    return false if @node["crowbar"].nil?
    return false if @node["crowbar"]["admin_node"].nil?
    @node["crowbar"]["admin_node"]
  end

  def interface_list
    return [] if @node.nil?
    answer = []
    @node["network"]["interfaces"].each do |k,v|
      next if k == "lo"     # no loopback, please
      next if k =~ /^sit/   # Ignore sit interfaces
      next if k =~ /^vlan/  # Ignore nova create interfaces
      next if k =~ /^br/    # Ignore bridges interfaces
      next if k =~ /\.\d+/  # no vlan interfaces, please
      answer << k
    end
    answer
  end

  def adapter_count
    interface_list.size
  end

  def bus_index(bus_order, path)
    return 999 if bus_order.nil?

    dpath = path.split(".")[0].split("/")

    index = 0
    bus_order.each do |b|
      subindex = 0
      bs = b.split(".")[0].split("/")

      match = true
      bs.each do |bp|
        break if subindex >= dpath.size
        match = false if bp != dpath[subindex]
        break unless match
        subindex = subindex + 1
      end

      return index if match
      index = index + 1
    end

    999
  end

  def get_bus_order
    bus_order = nil
    @node["network"]["interface_map"].each do |data|
      bus_order = data["bus_order"] if @node[:dmi][:system][:product_name] =~ /#{data["pattern"]}/
      break if bus_order
    end rescue nil
    bus_order
  end

  def bus_index(bus_order, path)
    return 999 if bus_order.nil?

    dpath = path.split(".")[0].split("/")

    index = 0
    bus_order.each do |b|
      subindex = 0
      bs = b.split(".")[0].split("/")

      match = true
      bs.each do |bp|
        break if subindex >= dpath.size
        match = false if bp != dpath[subindex]
        break unless match
        subindex = subindex + 1
      end

      return index if match
      index = index + 1
    end

    999
  end

  def sort_ifs
    bus_order = get_bus_order
    map = self.crowbar_ohai["detected"]["network"]
    answer = map.sort{|a,b|
      aindex = bus_index(bus_order, a[1])
      bindex = bus_index(bus_order, b[1])
      aindex == bindex ? a[0] <=> b[0] : aindex <=> bindex
    }
    answer.map! { |x| x[0] }
  end

  def get_conduits
    conduits = nil
    @node["network"]["conduit_map"].each do |data|
      parts = data["pattern"].split("/")
      the_one = true
      the_one = false unless @node["network"]["mode"] =~ /#{parts[0]}/
      the_one = false unless self.crowbar_ohai["detected"]["network"].size.to_s =~ /#{parts[1]}/

      found = false
      @node.roles.each do |role|
        found = true if role =~ /#{parts[2]}/
        break if found
      end
      the_one = false unless found

      conduits = data["conduit_list"] if the_one
      break if conduits
    end rescue nil
    conduits
  end

  def build_node_map
    bus_order = get_bus_order
    conduits = get_conduits

    return {} if conduits.nil?

    sorted_ifs = sort_ifs
    if_remap = {}
    count = 1
    sorted_ifs.each do |intf|
      if_remap["1g#{count}"] = intf
      count = count + 1
    end

    ans = {}
    conduits.each do |k,v|
      hash = {}
      v.each do |mk, mv|
        if mk == "if_list"
          hash["if_list"] = v["if_list"].map do |y|
            if_remap[y]
          end
        else
          hash[mk] = mv
        end
      end
      ans[k] = hash
    end

    ans
  end

  def unmanaged_interfaces
    intf_to_if_map = build_node_map

    orig_if_list = self.crowbar_ohai["detected"]["network"] rescue nil
    return {} if orig_if_list.nil?
    if_list = orig_if_list.map { |x| x[0] }

    intf_to_if_map.each do |k,v|
      v.each do |mk, mv|
        if mk == "if_list"
          v["if_list"].each do |x|
            if_list.delete(x) if if_list.include?(x)
          end
        end
      end
    end

    if_list
  end

  def lookup_interface_info(conduit, intf_to_if_map = nil)
    intf_to_if_map = build_node_map if intf_to_if_map.nil?

    return [nil, nil] if intf_to_if_map[conduit].nil?

    c_info = intf_to_if_map[conduit]
    interface_list = c_info["if_list"]
    team_mode = c_info["team_mode"] rescue nil

    return [interface_list[0], interface_list, nil] if interface_list.size == 1

    @node["crowbar"]["bond_list"] = {} if (@node["crowbar"].nil? or @node["crowbar"]["bond_list"].nil?)
    bond_list = @node["crowbar"]["bond_list"]
    the_bond = nil
    bond_list.each do |bond, map|
      the_bond = bond if map == interface_list
      break if the_bond
    end

    if the_bond.nil?
      the_bond = "bond#{bond_list.size}"
      bond_list[the_bond] = interface_list
    end

    [the_bond, interface_list, team_mode]
  end

  # Switch config is actually a node set property from customer ohai.  It is really on the node and not the role
  def switch_name
    unless @node.nil? or @node["crowbar"].nil? or self.crowbar_ohai.nil? or self.crowbar_ohai["switch_config"].nil?
      intf = sort_ifs[0]
      switch_name = self.crowbar_ohai["switch_config"][intf]["switch_name"]
      unless switch_name == -1
        switch_name.to_s.gsub(':', '-')
      else
        nil
      end
    else
      nil
    end
  end

  # for stacked switches, unit is set while name is the same
  def switch_unit
    unless @node["crowbar"].nil? or self.crowbar_ohai["switch_config"].nil?
      intf = sort_ifs[0]
      switch_unit = self.crowbar_ohai["switch_config"][intf]["switch_unit"]
      (switch_unit == -1 ? nil : switch_unit)
    else
      nil
    end
  end

  def switch_port
    unless @node["crowbar"].nil? or self.crowbar_ohai.nil? or self.crowbar_ohai["switch_config"].nil?
      intf = sort_ifs[0]
      switch_port = self.crowbar_ohai["switch_config"][intf]["switch_port"]
      (switch_port == -1 ? nil : switch_port)
    else
      nil
    end
  end

  # used to determine if display information has been set or if defaults should be used
  def display_set?(type)
    !display[type].nil? and !display[type].empty?
  end
  
  def switch
    if switch_name.nil?
      self.handle[0..8]
    elsif switch_unit.nil?
      switch_name
    else
      switch_name + ':' + switch_unit
    end
  end
  
  # logical grouping for node to align with other nodes
  def group(suggest=false)
    g = if display_set? 'group'
      display['group']
    elsif suggest
      default_loader['group']
    else
      nil
    end
    # if not set, use calculated value
    (g.nil? ? switch : g)
  end
  
  def group=(value)
    set_display "group", value
  end

  # order WITHIN the logical grouping
  def group_order
    if switch_port.nil?
      self.alias
    else
      switch_port.to_i
    end
  end

  def hardware
    @node["dmi"].nil? ? I18n.t('unknown') : @node["dmi"].system.product_name
  end

  def raid_set
    return NOT_SET if @role.nil?
    return NOT_SET if self.crowbar["crowbar"].nil?
    return NOT_SET if self.crowbar["crowbar"]["hardware"].nil?
    self.crowbar["crowbar"]["hardware"]["raid_set"] || NOT_SET
  end

  def raid_set=(value)
    return nil if @role.nil?
    return nil if self.crowbar["crowbar"].nil?
    self.crowbar["crowbar"]["hardware"] = {} if self.crowbar["crowbar"]["hardware"].nil?
    self.crowbar["crowbar"]["hardware"]["raid_set"] = value unless value===NOT_SET
  end

  def bios_set
    return NOT_SET if @role.nil?
    return NOT_SET if self.crowbar["crowbar"].nil?
    return NOT_SET if self.crowbar["crowbar"]["hardware"].nil?
    self.crowbar["crowbar"]["hardware"]["bios_set"] || NOT_SET
  end

  def bios_set=(value)
    return nil if @role.nil?
    return nil if self.crowbar["crowbar"].nil?
    self.crowbar["crowbar"]["hardware"] = {} if self.crowbar["crowbar"]["hardware"].nil?
    self.crowbar["crowbar"]["hardware"]["bios_set"] = value unless value===NOT_SET
  end

  def to_hash
    return {} if @node.nil?
    nhash = @node.to_hash
    rhash = @role.default_attributes.to_hash
    nhash.merge rhash
  end

  def get_bmc_user
    @node["ipmi"]["bmc_user"] rescue nil
  end

  def get_bmc_password
    @node["ipmi"]["bmc_password"] rescue nil
  end

  def set_state(state)
    if CHEF_ONLINE
      # use the real transition function for this
      cb = CrowbarService.new Rails.logger
      results = cb.transition "default", @node.name, state
    else
      puts "Node #{name} Chef State Changed to #{state}"
      self.crowbar['state'] = state
      save
    end

    if state == "reset" or state == "reinstall" or state == "update"
      if CHEF_ONLINE
        bmc          = get_network_by_type("bmc")
        bmc_user     = get_bmc_user
        bmc_password = get_bmc_password
        system("ipmitool -I lanplus -H #{bmc["address"]} -U #{bmc_user} -P #{bmc_password} power cycle") unless bmc.nil?
      else
        NodeObject.clear_cache @node
        puts "Node #{name} to #{state} caused cache object to be deleted."
      end
    end
    results
  end

  def reboot
    set_state("reboot")
    bmc          = get_network_by_type("bmc")
    bmc_user     = get_bmc_user
    bmc_password = get_bmc_password
    return puts "Node #{name} IMPI Reboot call to #{bmc["address"]}" unless CHEF_ONLINE
    system("ipmitool -I lanplus -H #{bmc["address"]} -U #{bmc_user} -P #{bmc_password} power cycle") unless bmc.nil?
  end

  def shutdown
    set_state("shutdown")
    bmc          = get_network_by_type("bmc")
    bmc_user     = get_bmc_user
    bmc_password = get_bmc_password
    return puts "Node #{name} IMPI Shutdown call to #{bmc["address"]}" unless CHEF_ONLINE
    system("ipmitool -I lanplus -H #{bmc["address"]} -U #{bmc_user} -P #{bmc_password} power off") unless bmc.nil?
  end

  def poweron
    set_state("poweron")
    bmc          = get_network_by_type("bmc")
    bmc_user     = get_bmc_user
    bmc_password = get_bmc_password
    return puts "Node #{name} IMPI Power On call to #{bmc["address"]}" unless CHEF_ONLINE
    system("ipmitool -I lanplus -H #{bmc["address"]} -U #{bmc_user} -P #{bmc_password} power on") unless bmc.nil?
  end

  def identify
    bmc          = get_network_by_type("bmc")
    bmc_user     = get_bmc_user
    bmc_password = get_bmc_password
    return puts "Node #{name} IMPI Identify call to #{bmc["address"]}" unless CHEF_ONLINE
    system("ipmitool -I lanplus -H #{bmc["address"]} -U #{bmc_user} -P #{bmc_password} chassis identify") unless bmc.nil?
  end

  def allocate
    return if @node.nil?
    return if @role.nil?
    self.allocated = true
    save
  end

  def bmc_set?
    return false if @node.nil? or @node["crowbar_wall"].nil? or @node["crowbar_wall"]["status"].nil?
    return false if @node["crowbar_wall"]["status"]["ipmi"].nil?
    return false if @node["crowbar_wall"]["status"]["ipmi"]["address_set"].nil?
    @node["crowbar_wall"]["status"]["ipmi"]["address_set"]
  end

  private 
  
  # this is used by the alias/description code split
  def chef_description
    "#{self.alias}: #{self.description}"
  end

  def display
    if crowbar["crowbar"].nil? or crowbar["crowbar"]["display"].nil?
      {}
    else
      crowbar["crowbar"]["display"]
    end
  end
  
  def set_display(attrib, value)
    crowbar["crowbar"] = { "display"=>{} } if crowbar["crowbar"].nil? 
    crowbar["crowbar"]["display"] = {} if crowbar["crowbar"]["display"].nil?
    crowbar["crowbar"]["display"][attrib] = (value || "").strip
  end
  
  def default_loader
    f = File.join 'db','node_description.yml'
    begin
      if File.exist? f
        default = {}
        nodes = YAML::load_file f
        unless nodes.nil?
          node = name.split('.')[0]
          # get values from default file
          nodes['default'].each { |key, value| default[key] = value } unless nodes['default'].nil?
          nodes[node].each { |key, value| default[key] = value } unless nodes[node].nil?
          # some date replacement
          default['description'] = default['description'].gsub(/DATE/,I18n::l(Time.now)) unless default['description'].nil?
          default['alias'] =default['alias'].gsub(/NODE/,node) unless default['alias'].nil?
        end
        return default
      end
    rescue => exception
      Rails.logger.warn("Optional db\\node_description.yml file not correctly formatted.  Error #{exception.message}")
    end
    {}
  end

end


