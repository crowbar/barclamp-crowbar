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

require 'chef/mixin/deep_merge'

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
      self.find "crowbar_display_alias:#{chef_escape(name)}"
    else
      nodes = self.find_all_nodes.keep_if { |n| n.alias==name }
    end
    if nodes.length == 1
      return nodes[0]
    elsif nodes.length == 0
      nil
    else
      raise "#{I18n.t('multiple_node_alias', :scope=>'model.node')}: #{nodes.join(',')}"
    end
  end

  def self.find_node_by_public_name(name)
    nodes = if CHEF_ONLINE
      self.find "crowbar_public_name:#{chef_escape(name)}"
    else
      nodes = self.find_all_nodes.keep_if { |n| n.public_name==name }
    end
    if nodes.length == 1
      return nodes[0]
    elsif nodes.length == 0
      nil
    else
      raise "#{I18n.t('multiple_node_public_name', :scope=>'model.node')}: #{nodes.join(',')}"
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
    # deep clone of @role.default_attributes, used when saving node
    @attrs_last_saved = Marshal.load(Marshal.dump(@role.default_attributes))
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
    return value if self.alias==value
    value = value.strip.sub(/\s/,'-')
    # valid DNS Name
    if !(value =~ /^(([a-zA-Z]|[a-zA-Z][a-zA-Z0-9\-]*[a-zA-Z0-9])\.)*([A-Za-z]|[A-Za-z][A-Za-z0-9\-]*[A-Za-z0-9])$/)
      Rails.logger.warn "Alias #{value} not saved because it did not conform to valid DNS hostnames"
      raise "#{I18n.t('model.node.invalid_dns_alias')}: #{value}"
    elsif value.length+ChefObject.cloud_domain.length>255  
      Rails.logger.warn "Alias #{value}.#{ChefObject.cloud_domain} FQDN not saved because it exceeded the 63 character length limit"
      raise "#{I18n.t('too_long_dns_alias', :scope=>'model.node')}: #{value}.#{ChefObject.cloud_domain}"
    else
      # don't allow duplicate alias
      node = NodeObject.find_node_by_alias value 
      if node and !node.handle.eql?(handle)
        Rails.logger.warn "Alias #{value} not saved because #{node.name} already has the same alias."
        raise I18n.t('duplicate_alias', :scope=>'model.node') + ": " + node.name
      else
        set_display "alias", value
        @role.description = chef_description
        # move this to event driven model one day
        system("sudo -i /opt/dell/bin/single_chef_client.sh") if CHEF_ONLINE
      end
    end
    return value
  end

  def public_name(suggest=false)
    if !crowbar["crowbar"]["public_name"].nil? && !crowbar["crowbar"]["public_name"].empty?
      crowbar["crowbar"]["public_name"]
    elsif suggest
      default_loader['public_name']
    else
      nil
    end
  end

  def public_name=(value)
    value = value.strip.sub(/\s/,'-')
    # valid DNS Name
    if not (value.nil? or value.empty?)
      if !(value =~ /^(([a-zA-Z]|[a-zA-Z][a-zA-Z0-9\-]*[a-zA-Z0-9])\.)*([A-Za-z]|[A-Za-z][A-Za-z0-9\-]*[A-Za-z0-9])$/)
        Rails.logger.warn "Public name #{value} not saved because it did not conform to valid DNS hostnames"
        raise "#{I18n.t('invalid_dns_public_name', :scope=>'model.node')}: #{value}"
      elsif value.length>255
        Rails.logger.warn "Public name #{value} not saved because it exceeded the 255 character length limit"
        raise "#{I18n.t('too_long_dns_public_name', :scope=>'model.node')}: #{value}"
      else
        # don't allow duplicate public names
        node = NodeObject.find_node_by_public_name value
        if node and !node.handle.eql?(handle)
          Rails.logger.warn "Public name #{value} not saved because #{node.name} already has the same public name."
          raise I18n.t('duplicate_public_name', :scope=>'model.node') + ": " + node.name
        end
      end
    end

    crowbar["crowbar"]["public_name"] = value
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
    when "ready", "completed"
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
    begin
      intf = sort_ifs[0]
      self.crowbar_ohai["switch_config"][intf]["mac"] || (I18n.t :unknown)
    rescue
      Rails.logger.warn("mac: #{@node.name}: Switch config not detected during discovery")
      (I18n.t :not_set)
    end
  end

  def allocated
    (@node.nil? or @role.nil?) ? false : self.crowbar["crowbar"]["allocated"]
  end

  def allocated=(value)
    return false if @role.nil?
    Rails.logger.info("Setting allocate state for #{@node.name} to #{value}")
    self.crowbar["crowbar"]["allocated"] = value
    @role.save
    value
  end

  def allocated?
    (@node.nil? or @role.nil?) ? false : self.crowbar["crowbar"]["allocated"]
  end

  def ipmi_enabled?
    #placeholder until we have a better mechanism
    @node.nil? ? false : @node["crowbar"]["allocated"]
  end

  # creates a hash with key attributes of the node from ohai for comparison
  def family
    f = {}
    f[:drives] = physical_drives
    f[:ram] = memory
    f[:cpu] = cpu
    f[:hw] = hardware
    f[:raid] = raid_set
    f[:nics] = nics
    f
  end

  def nics
    @node["crowbar_ohai"]["detected"]["network"].length rescue 0
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

  def drive_info
    volumes = []
    controllers = @node["crowbar_wall"]["raid"]["controllers"] rescue []
    controllers = [] unless controllers
    controllers.each do |c,k|
      k["volumes"].each do |v|
        volumes << "#{v["raid_level"]} #{v["size"].to_i/1024/1024/1024}GB"
      end
    end
    volumes
  end

  def asset_tag
    if virtual?
      "vm-#{mac.gsub(':',"-")}"
    else
      @node["dmi"]["chassis"]["serial_number"] rescue nil
    end
  end

  def virtual?
    virtual = [ "KVM", "VMware Virtual Platform", "VMWare Virtual Platform", "VirtualBox", "Bochs" ]
    virtual.include? hardware
  end

  def number_of_drives
    # This needs to be kept in sync with the fixed method in
    # barclamp_library.rb in in the deployer barclamp.
    @node[:block_device].find_all do |disk,data|
      disk =~ /^[hsv]d/ && data[:removable] == "0"
    end.length
  end

  def physical_drives
    number_of_drives
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
    val = { "states" => states, "priority" => priority }
    crowbar["run_list_map"][rolename] = val
    Rails.logger.debug("crowbar[run_list_map][#{rolename}] = #{val.inspect}")
    Rails.logger.debug("current state is #{self.crowbar['state']}")

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
    Rails.logger.debug("rebuilt run_list will be #{vals.inspect}")

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
    begin
      crowbar["run_list_map"] = {} if crowbar["run_list_map"].nil?
      return crowbar["run_list_map"][role_name]["priority"] != -1001 if crowbar["run_list_map"][role_name] 
      @node.role?(role_name)
    rescue
      return false
    end
  end

  def roles
    @node['roles'].nil? ? nil : @node['roles'].sort
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

    # helper function to remove from node elements that were removed from the
    # role attributes; this is something that
    # Chef::Mixin::DeepMerge::deep_merge doesn't do
    def _remove_elements_from_node(old, new, from_node)
      old.each_key do |k|
        if not new.has_key?(k)
          from_node.delete(k) unless from_node[k].nil?
        elsif old[k].is_a?(Hash) and new[k].is_a?(Hash) and from_node[k].is_a?(Hash)
          _remove_elements_from_node(old[k], new[k], from_node[k])
        end
      end
    end

    _remove_elements_from_node(@attrs_last_saved, @role.default_attributes, @node.normal_attrs)
    Chef::Mixin::DeepMerge::deep_merge!(@role.default_attributes, @node.normal_attrs, {})

    if CHEF_ONLINE
      @role.save
      @node.save
    else
      NodeObject.offline_cache(@node, NodeObject.nfile('node', @node.name))
      NodeObject.offline_cache(@role, RoleObject.nfile('role', @role.name))
    end

    # update deep clone of @role.default_attributes
    @attrs_last_saved = Marshal.load(Marshal.dump(@role.default_attributes))

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

  # IMPORTANT: This needs to be kept in sync with the bus_index method in
  # BarclampLibrary::Barclamp::Inventory in the "barclamp" cookbook of the
  # deployer barclamp.
  def bus_index(bus_order, path)
    return 999 if bus_order.nil? or path.nil?

    # For backwards compatibility with the old busid matching
    # which just stripped of everything after the first '.'
    # in the busid
    path_old = path.split(".")[0]

    index = 0
    bus_order.each do |b|
      # When there is no '.' in the busid from the bus_order assume
      # that we are using the old method of matching busids
      if b.include?('.')
        path_used = path
      else
        path_used = path_old
      end
      return index if b == path_used
      index = index + 1
    end

    999
  end

  # IMPORTANT: This needs to be kept in sync with the get_bus_order method in
  # BarclampLibrary::Barclamp::Inventory in the "barclamp" cookbook of the
  # deployer barclamp.
  def get_bus_order
    bus_order = nil
    @node["network"]["interface_map"].each do |data|
      if @node[:dmi][:system][:product_name] =~ /#{data["pattern"]}/
        if data.has_key?("serial_number")
          bus_order = data["bus_order"] if @node[:dmi][:system][:serial_number].strip == data["serial_number"].strip
        else
          bus_order = data["bus_order"]
        end
      end
      break if bus_order
    end rescue nil
    bus_order
  end

  def sort_ifs
    bus_order = get_bus_order
    map = self.crowbar_ohai["detected"]["network"]
    answer = map.sort{|a,b|
      aindex = bus_index(bus_order, a[1]["path"])
      bindex = bus_index(bus_order, b[1]["path"])
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
    map = self.crowbar_ohai["detected"]["network"]
    if_remap = {}
    count_map = {}
    sorted_ifs.each do |intf|
      speeds = map[intf]["speeds"]
      speeds = ['1g'] unless speeds   #legacy object support
      speeds.each do |speed|
        count = count_map[speed] || 1
        if_remap["#{speed}#{count}"] = intf
        count_map[speed] = count + 1
      end
    end

    ans = {}
    conduits.each do |k,v|
      hash = {}
      v.each do |mk, mv|
        if mk == "if_list"
          hash["if_list"] = v["if_list"].map do |if_ref|
            self.map_if_ref(if_remap, if_ref)
          end
        else
          hash[mk] = mv
        end
      end
      ans[k] = hash
    end

    ans
  end

  ##
  # given a map of available interfaces on the local machine,
  # resolve references form conduit list. The supported reference format is <sign><speed><#> where
  #  - sign is optional, and determines behavior if exact match is not found. + allows speed upgrade, - allows downgrade
  #    ? allows either. If no sign is specified, an exact match must be found.
  #  - speed designates the interface speed. 10m, 100m, 1g and 10g are supported
  def map_if_ref(if_map, ref)
    speeds= %w{10m 100m 1g 10g}
    m= /^([-+?]?)(\d{1,3}[mg])(\d+)$/.match(ref) # [1]=sign, [2]=speed, [3]=count
    if_cnt = m[3]
    desired = speeds.index(m[2])
    found = nil
    filter = lambda { |x|
      found = if_map["#{speeds[x]}#{if_cnt}"] unless found
    }
    case m[1]
      when '+': (desired..speeds.length).each(&filter)
      when '-': desired.downto(0,&filter)
      when '?':
        (desired..speeds.length).each(&filter)
        desired.downto(0,&filter) unless found
      else
        found = if_map[ref]
      end
    found
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
    switch_find_info('name')
  end

  # for stacked switches, unit is set while name is the same
  def switch_unit
    switch_find_info('unit')
  end

  def switch_port
    switch_find_info('port')
  end

  # DRY version of the switch name/unit/port code
  def switch_find_info(type)
    info = nil
    begin
      sort_ifs.each do |intf|
        info = self.crowbar_ohai["switch_config"][intf]["switch_"+type]
        next if info == -1  # try next interface in case this is not in use
        info = info.name = name.to_s.gsub(':', '-')
        break  # if we got this far then we are done
      end
    rescue
      Rails.logger.warn("Switch #{type} Error: #{@node.name}: Switch config not detected during discovery")
    end
    info
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
      "#{switch_name}:#{switch_unit}"
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
    (g.nil? ? "sw-#{switch}" : g)
  end
  
  def group=(value)
    set_display "group", value
  end

  # order WITHIN the logical grouping
  def group_order
    begin
      if switch_port.nil? or switch_port == -1
        self.alias
      else
        switch_name + "%05d" % switch_unit.to_i + "%05d" % switch_port.to_i
      end
    rescue
       self.alias 
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

  def bmc_address
    @node["crowbar_wall"]["ipmi"]["address"] rescue nil
  end

  def get_bmc_user
    @node["ipmi"]["bmc_user"] rescue nil
  end

  def get_bmc_password
    @node["ipmi"]["bmc_password"] rescue nil
  end

  def bmc_cmd(cmd)
    if bmc_address.nil? || get_bmc_user.nil? || get_bmc_password.nil? ||
        !system("ipmitool -I lanplus -H #{bmc_address} -U #{get_bmc_user} -P #{get_bmc_password} #{cmd}")
      case cmd
      when "power cycle"
        ssh_command="/sbin/reboot -f"
      when "power off"
        ssh_command="/sbin/poweroff -f"
      else
        Rails.logger.warn("ipmitool #{cmd} failed for #{@node.name}.")
        return nil
      end
      Rails.logger.warn("failed ipmitool #{cmd}, falling back to ssh for #{@node.name}")
      # Have to redirect stdin, stdout, stderr and background reboot
      # command on the client else ssh never disconnects when client dies
      # `timeout` and '-o ConnectTimeout=10' are there in case anything
      # else goes wrong...
      unless system("sudo -i -u root -- timeout -k 5s 15s ssh -o ConnectTimeout=10 root@#{@node.name} '#{ssh_command} </dev/null >/dev/null 2>&1 &'")
        Rails.logger.warn("ssh fallback for shutdown/reboot for #{@node.name} failed - node in unknown state")
        return nil
      end
    end
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
        bmc_cmd("power cycle")
      else
        NodeObject.clear_cache @node
        puts "Node #{name} to #{state} caused cache object to be deleted."
      end
    end
    results
  end

  def reboot
    set_state("reboot")
    return puts "Node #{name} IMPI Reboot call to #{bmc_address}" unless CHEF_ONLINE
    bmc_cmd("power cycle")
  end

  def shutdown
    set_state("shutdown")
    return puts "Node #{name} IMPI Shutdown call to #{bmc_address}" unless CHEF_ONLINE
    bmc_cmd("power off")
  end

  def poweron
    set_state("poweron")
    return puts "Node #{name} IMPI Power On call to #{bmc_address}" unless CHEF_ONLINE
    bmc_cmd("power on")
  end

  def identify
    return puts "Node #{name} IMPI Identify call to #{bmc_address}" unless CHEF_ONLINE
    bmc_cmd("chassis identify")
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

  def export
    NodeObject.dump @node, 'node', name
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
          nodes[asset_tag].each { |key, value| default[key] = value } unless nodes[asset_tag].nil?
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


