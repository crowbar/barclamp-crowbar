#
# Copyright 2011-2013, Dell
# Copyright 2013-2014, SUSE LINUX Products GmbH
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

require 'chef/mixin/deep_merge'
require 'timeout'

class NodeObject < ChefObject
  self.chef_type = "node"

  def self.find(search)
    answer = []
    nodes = if search.nil?
      ChefObject.query_chef.search "node"
    else
      ChefObject.query_chef.search "node", "#{chef_escape(search)}"
    end

    if nodes.is_a?(Array) and nodes[2] != 0 and !nodes[0].nil?
      nodes[0].delete_if { |x| x.nil? }
      answer = nodes[0].map do |x|
        begin
          NodeObject.new x
        rescue Crowbar::Error::NotFound
          nil
        end
      end
      answer.compact!
    end
    return answer
  end

  def self.find_all_nodes
    self.find nil
  end

  def self.find_nodes_by_name(name)
    self.find "name:#{chef_escape(name)}"
  end

  def self.find_node_by_alias(name)
    nodes = self.find_all_nodes.select { |n| n.alias.downcase == name.downcase }
    if nodes.length == 1
      return nodes[0]
    elsif nodes.length == 0
      nil
    else
      raise "#{I18n.t('multiple_node_alias', :scope => 'model.node')}: #{nodes.join(',')}"
    end
  end

  def self.default_platform
    @@default_platform ||= begin
      admin = NodeObject.find("role:crowbar").select { |n| n.admin? }.first
      if admin.nil?
        ""
      else
        "#{admin[:platform]}-#{admin[:platform_version]}"
      end
    end
  end

  def self.available_platforms
    @available_platforms ||= begin
      provisioner = NodeObject.find("roles:provisioner-server").first
      if provisioner.nil?
        []
      else
        availables_oses = provisioner["provisioner"]["available_oses"].keys
        # Sort the platforms:
        #  - first, the default platform
        #  - between first and the Hyper-V/Windows bits: others, sorted
        #    alphabetically
        #  - last Hyper-V, and just before that Windows
        platform_order = {"windows" => 90, "hyperv" => 100}
        availables_oses.uniq.sort {|x, y|
          platform_x, version_x = x.split('-')
          platform_y, version_y = y.split('-')
          platform_order_x = platform_order[platform_x] || 1
          platform_order_y = platform_order[platform_y] || 1

          if x == default_platform
            -1
          elsif y == default_platform
            1
          elsif platform_x == platform_y
            version_y <=> version_x
          elsif platform_order_x == platform_order_y
            x <=> y
          else
            platform_order_x <=> platform_order_y
          end
        }
      end
    end
  end

  def self.disabled_platforms
    @disabled_platforms ||= begin
      provisioner = NodeObject.find("roles:provisioner-server").first
      if provisioner.nil?
        []
      else
        provisioner["provisioner"]["available_oses"].keys.select { |p|
          provisioner["provisioner"]["available_oses"][p]["disabled"]
        }
      end
    end
  end

  def self.find_node_by_public_name(name)
    nodes = self.find "crowbar_public_name:#{chef_escape(name)}"
    if nodes.length == 1
      return nodes[0]
    elsif nodes.length == 0
      nil
    else
      raise "#{I18n.t('multiple_node_public_name', :scope => 'model.node')}: #{nodes.join(',')}"
    end
  end

  def self.find_node_by_name(name)
    name += ".#{ChefObject.cloud_domain}" unless name =~ /(.*)\.(.)/
    begin
      chef_node = Chef::Node.load(name)
      unless chef_node.nil?
        NodeObject.new(chef_node)
      else
        nil
      end
    rescue Errno::ECONNREFUSED => e
      raise Crowbar::Error::ChefOffline.new
    rescue Crowbar::Error::NotFound => e
      nil
    rescue StandardError => e
      Rails.logger.warn("Could not recover Chef Crowbar Node on load #{name}: #{e.inspect}")
      nil
    end
  end

  def self.find_node_by_name_or_alias(name)
    node = find_node_by_name(name)

    if node.nil?
      find_node_by_alias(name)
    else
      node
    end
  end

  def self.all
    self.find nil
  end

  def self.make_role_name(name)
    "crowbar-#{name.gsub(".", "_")}"
  end

  def self.create_new_role(new_name, machine)
    name = make_role_name new_name
    role = RoleObject.new Chef::Role.new
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

  def method_missing(method, *args, &block)
    if @node.respond_to? method
      @node.send(method, *args, &block)
    else
      super
    end
  end

  def initialize(node)
    @role = RoleObject.find_role_by_name NodeObject.make_role_name(node.name)
    if @role.nil?
      # An admin node can exist without a role - so create one
      if !node["crowbar"].nil? and node["crowbar"]["admin_node"]
        @role = NodeObject.create_new_role(node.name, node)
      else
        Rails.logger.fatal("Node exists without role!! #{node.name}")
        raise Crowbar::Error::NotFound.new
      end
    end
    # deep clone of @role.default_attributes, used when saving node
    @attrs_last_saved = deep_clone(@role.default_attributes)
    @node = node
  end

  def default_platform
    self.class.default_platform
  end

  def target_platform
    @node[:target_platform] || default_platform
  end

  def pretty_target_platform
    CrowbarService.pretty_target_platform(target_platform)
  end

  def target_platform=(value)
    @node.set[:target_platform] = value
  end

  def crowbar_wall
    @node["crowbar_wall"] || {}
  end

  def availability_zone
    crowbar_wall["openstack"]["availability_zone"] rescue nil
  end

  def availability_zone=(value)
    @node["crowbar_wall"] ||= {}
    @node["crowbar_wall"]["openstack"] ||= {}
    @node["crowbar_wall"]["openstack"]["availability_zone"] = value
  end

  def intended_role
    crowbar_wall["intended_role"] rescue "no_role"
  end

  def intended_role=(value)
    @node["crowbar_wall"] ||= {}
    @node["crowbar_wall"]["intended_role"] = value
  end

  def raid_type
    crowbar_wall["raid_type"] || "single"
  end

  def raid_type=(value)
    @node["crowbar_wall"] ||= {}
    @node["crowbar_wall"]["raid_type"] = value
  end

  def raid_disks
    crowbar_wall["raid_disks"] || []
  end

  def raid_disks=(value)
    @node["crowbar_wall"] ||= {}
    @node["crowbar_wall"]["raid_disks"] = value
  end

  def license_key
    @node[:license_key]
  end

  def license_key=(value)
    if CrowbarService.require_license_key?(self.target_platform)
      @node.set[:license_key] = value
    else
      @node.set[:license_key] = ""
    end
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

  def update_and_validate(key, value, unique_check = true)
    send("validate_#{key}".to_sym, value, unique_check)
    send("update_#{key}".to_sym, value)

    value
  end

  def alias(suggest=false)
    if display_set? 'alias'
      display['alias']
    else
      # FIXME: This code is duplicated in crowbar_machines' #aliases method.
      # If you change this, currently you need to update that too.
      fallback = name.split('.')[0]
      fallback = default_loader['alias'] || fallback if suggest and !display_set? 'alias'
      fallback
    end
  end

  def alias=(value)
    return value if self.alias == value

    update_and_validate(
      :alias,
      value.strip.sub(/\s/, '-'),
      true
    )
  end

  def force_alias=(value)
    update_and_validate(
      :alias,
      value.strip.sub(/\s/, '-'),
      false
    )
  end

  def validate_alias(value, unique_check = true)
    if ! value.empty? && value !~ /^(([a-zA-Z]|[a-zA-Z][a-zA-Z0-9\-]*[a-zA-Z0-9])\.)*([A-Za-z]|[A-Za-z][A-Za-z0-9\-]*[A-Za-z0-9])$/
      Rails.logger.warn "Alias #{value} not saved because it did not conform to valid DNS hostnames"
      raise "#{I18n.t('model.node.invalid_dns_alias')}: #{value}"
    end

    if value.length>63 || value.length+ChefObject.cloud_domain.length>254
      Rails.logger.warn "Alias #{value}.#{ChefObject.cloud_domain} FQDN not saved because it exceeded the 63 character length limit or it's length (#{value.length}) will cause the total DNS max of 255 to be exeeded."
      raise "#{I18n.t('too_long_dns_alias', :scope => 'model.node')}: #{value}.#{ChefObject.cloud_domain}"
    end

    if unique_check
      node = NodeObject.find_node_by_alias value

      if node and node.handle != handle
        Rails.logger.warn "Alias #{value} not saved because #{node.name} already has the same alias."
        raise I18n.t('duplicate_alias', :scope => 'model.node') + ": " + node.name
      end
    end

    true
  end

  def update_alias(value)
    set_display "alias", value
    @role.description = chef_description

    # move this to event driven model one day
    system("sudo", "-i", Rails.root.join("..", "bin", "single_chef_client.sh").expand_path.to_s)
  end

  def public_name(suggest=false)
    if !crowbar["crowbar"].nil? && !crowbar["crowbar"]["public_name"].nil? && !crowbar["crowbar"]["public_name"].empty?
      crowbar["crowbar"]["public_name"]
    elsif suggest
      default_loader['public_name']
    else
      nil
    end
  end

  def public_name=(value)
    return value if self.public_name == value

    update_and_validate(
      :public_name,
      value.strip.sub(/\s/, '-'),
      true
    )
  end

  def force_public_name=(value)
    update_and_validate(
      :public_name,
      value.strip.sub(/\s/, '-'),
      false
    )
  end

  def validate_public_name(value, unique_check = true)
    unless value.to_s.empty?
      if !(value =~ /^(([a-zA-Z]|[a-zA-Z][a-zA-Z0-9\-]*[a-zA-Z0-9])\.)*([A-Za-z]|[A-Za-z][A-Za-z0-9\-]*[A-Za-z0-9])$/)
        Rails.logger.warn "Public name #{value} not saved because it did not conform to valid DNS hostnames"
        raise "#{I18n.t('invalid_dns_public_name', :scope => 'model.node')}: #{value}"
      end

      if value.length > 255
        Rails.logger.warn "Public name #{value} not saved because it exceeded the 255 character length limit"
        raise "#{I18n.t('too_long_dns_public_name', :scope => 'model.node')}: #{value}"
      end

      if unique_check
        node = NodeObject.find_node_by_public_name value

        if node and !node.handle == handle
          Rails.logger.warn "Public name #{value} not saved because #{node.name} already has the same public name."
          raise I18n.t('duplicate_public_name', :scope => 'model.node') + ": " + node.name
        end
      end
    end

    true
  end

  def update_public_name(value)
    unless value.nil?
      crowbar["crowbar"]["public_name"] = value
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
    when "ready", "completed"
      "ready"     #green
    when "discovered", "wait", "waiting", "user", "hold", "pending", "input"
      "pending"   #flashing yellow
    when "discovering", "reset", "delete", "shutdown", "poweron", "noupdate"
      "unknown"   #grey
    when "problem", "issue", "error", "failed", "fail", "warn", "warning", "fubar", "alert"
      "failed"    #flashing red
    when "hardware-installing", "hardware-install", "hardware-installed", "hardware-updated", "hardware-updating"
      "building"  #yellow
    else # including: installing, installed, reinstall, reboot, recovering, readying, applying
      "unready"   #spinner
    end
  end

  def ready?
    state === 'ready'
  end

  def state
    return 'unknown' if (@node.nil? or @role.nil?)
    if self.crowbar['state'] === 'ready' and @node['ohai_time']
      since_last = Time.now.to_i-@node['ohai_time'].to_i
      return 'noupdate' if since_last > 1200 # or 20 mins
    end
    return self.crowbar['state'] || 'unknown'
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

  def allocate!
    return if @node.nil?
    return if @role.nil?
    return if self.allocated?
    Rails.logger.info("Allocating node #{@node.name}")
    @role.save do |r|
      r.default_attributes["crowbar"]["allocated"] = true
    end
  end

  def allocate
    allocate!
  end

  def allocated?
    return false if (@node.nil? or @role.nil?)
    return false if self.crowbar["crowbar"].nil?
    return !!@role.default_attributes["crowbar"]["allocated"]
  end

  def ipmi_enabled?
    #placeholder until we have a better mechanism
    @node.nil? ? false : @node["crowbar"]["allocated"]
  end

  # creates a hash with key attributes of the node from ohai for comparison
  def family
    f = {}
    f[:drives] = pretty_drives
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
      serial = @node[:dmi]["chassis"]["serial_number"] rescue nil
      asset = @node[:dmi]["chassis"]["asset_tag"] rescue nil
      if asset.blank?
        serial
      elsif serial.blank?
        asset
      else
        "#{asset} (#{serial})"
      end
    end
  end

  def virtual?
    virtual = [ "KVM", "VMware Virtual Platform", "VMWare Virtual Platform", "VirtualBox", "Bochs" ]
    virtual.include? hardware
  end

  def number_of_drives
    if physical_drives.empty?
      -1
    else
      physical_drives.length
    end
  end

  def pretty_drives
    if number_of_drives < 0
      I18n.t("unknown")
    else
      number_of_drives
    end
  end

  def unclaimed_physical_drives
    physical_drives.select do |disk, data|
      device = unique_device_for(disk)
      device && disk_owner(device).blank?
    end
  end

  def physical_drives
    # This needs to be kept in sync with the fixed method in
    # barclamp_library.rb in in the deployer barclamp.
    # On windows platform there is no block_device chef entry.

    if @node[:block_device]
      @node[:block_device].find_all do |disk, data|
        disk =~ /^([hsv]d|cciss|xvd)/ && data[:removable] == "0" && !(data[:vendor] == "cinder" && data[:model] =~ /^volume-/)
      end
    else
      []
    end
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
    # Ruby 1.8 vs. 1.9 compatibility. Select returns Hash in 1.9 instead of
    # an array, so map it back to [key, val] pairs.
    map = map.to_a if map.is_a?(Hash)
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
    if a.is_a?(Hash)
      a.keys
    else
      a.collect! { |x| x[0] }
    end
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

  def increment_crowbar_revision!
    if @role.default_attributes["crowbar-revision"].nil?
      @role.default_attributes["crowbar-revision"] = 0
    else
      @role.default_attributes["crowbar-revision"] += 1
    end
  end

  def crowbar_revision
    @role.default_attributes["crowbar-revision"]
  end

  def save
    increment_crowbar_revision!
    origin = caller[0][/`.*'/][1..-2]
    Rails.logger.debug("Saving node: #{@node.name} - #{crowbar_revision} (caller: #{origin})")

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

    @role.save
    @node.save

    # update deep clone of @role.default_attributes
    @attrs_last_saved = deep_clone(@role.default_attributes)

    Rails.logger.debug("Done saving node: #{@node.name} - #{crowbar_revision}")
  end

  def destroy
    Rails.logger.debug("Destroying node: #{@node.name} - #{crowbar_revision}")
    @role.destroy
    @node.destroy
    Rails.logger.debug("Done with removal of node: #{@node.name} - #{crowbar_revision}")
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
      when '+'
        (desired..speeds.length).each(&filter)
      when '-'
        desired.downto(0,&filter)
      when '?'
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

    bond_list = @node["crowbar"]["bond_list"] || {}
    the_bond = nil
    bond_list.each do |bond, map|
      the_bond = bond if map == interface_list
      break if the_bond
    end

    if the_bond.nil?
      # This should not happen as bond_list is always kept uptodate in
      # the network::default recipe
      Rails.logger.error("Unable to find the bond device for the teamed interfaces: #{interface_list.inspect}")
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
    res = nil
    begin
      sort_ifs.each do |intf|
        switch_config_intf = self.crowbar_ohai["switch_config"][intf]
        # try next interface in case this is one is missing data
        next if [switch_config_intf["switch_name"], switch_config_intf["switch_unit"], switch_config_intf["switch_port"]].include? -1
        info = switch_config_intf["switch_"+type]
        res = info.to_s.gsub(':', '-')
        break  # if we got this far then we are done
      end
    rescue
      Rails.logger.warn("Switch #{type} Error: #{@node.name}: Switch config not detected during discovery")
    end
    res
  end

  # used to determine if display information has been set or if defaults should be used
  def display_set?(type)
    !display[type].nil? and !display[type].empty?
  end

  def switch
    if switch_name.nil?
      "unknown"
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
        switch_name + "%05d" % switch_unit.to_i + "%05d" % switch_port.to_i + self.alias
      end
    rescue
       self.alias
    end
  end

  def hardware
    return I18n.t('unknown') if @node[:dmi].nil?
    return I18n.t('unknown') if @node[:dmi][:system].nil?
    return @node[:dmi][:system][:product_name]
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

  def ssh_cmd(cmd)
    if @node[:platform] == "windows"
      Rails.logger.warn("ssh command \"#{cmd}\" for #{@node.name} ignored - node is running Windows")
      return nil
    end

    # Have to redirect stdin, stdout, stderr and background reboot
    # command on the client else ssh never disconnects when client dies
    # `timeout` and '-o ConnectTimeout=10' are there in case anything
    # else goes wrong...
    unless system("sudo", "-i", "-u", "root", "--",
                  "timeout", "-k", "5s", "15s",
                  "ssh", "-o", "ConnectTimeout=10", "root@#{@node.name}",
                  "#{cmd} </dev/null >/dev/null 2>&1 &")
      Rails.logger.warn("ssh command \"#{cmd}\" for #{@node.name} failed - node in unknown state")
      return nil
    end
  end

  def net_rpc_cmd(cmd)
    case cmd
    when :power_cycle
      unless system("net", "rpc", "shutdown", "-f", "-r", "-I", @node.name ,"-U", "Administrator%#{@node[:provisioner][:windows][:admin_password]}")
        Rails.logger.warn("samba command \"#{cmd}\" for #{@node.name} failed - node in unknown state")
      end
    when :power_off
      unless system("net", "rpc", "shutdown", "-f", "-I", @node.name ,"-U", "Administrator%#{@node[:provisioner][:windows][:admin_password]}")
        Rails.logger.warn("samba command \"#{cmd}\" for #{@node.name} failed - node in unknown state")
      end
    when :reboot
      unless system("net", "rpc", "shutdown", "-r", "-I", @node.name ,"-U", "Administrator%#{@node[:provisioner][:windows][:admin_password]}")
        Rails.logger.warn("samba command \"#{cmd}\" for #{@node.name} failed - node in unknown state")
      end
    when :shutdown
      unless system("net", "rpc", "shutdown", "-I", @node.name ,"-U", "Administrator%#{@node[:provisioner][:windows][:admin_password]}")
        Rails.logger.warn("samba command \"#{cmd}\" for #{@node.name} failed - node in unknown state")
      end
    else
      Rails.logger.warn("samba command #{cmd} failed for #{@node.name}.")
    end
  end


  def bmc_cmd(cmd)
    if bmc_address.nil? || get_bmc_user.nil? || get_bmc_password.nil? ||
        !system("ipmitool", "-I", "lanplus", "-H", bmc_address, "-U", get_bmc_user, "-P", get_bmc_password, cmd)
      case cmd
      when "power cycle"
        ssh_command = "/sbin/reboot -f"
      when "power off"
        ssh_command = "/sbin/poweroff -f"
      else
        Rails.logger.warn("ipmitool #{cmd} failed for #{@node.name}.")
        return nil
      end
      Rails.logger.warn("failed ipmitool #{cmd}, falling back to ssh for #{@node.name}")
      ssh_cmd(ssh_command)
    end
  end

  def set_state(state)
    # use the real transition function for this
    cb = CrowbarService.new Rails.logger
    results = cb.transition "default", @node.name, state

    if %w(reset reinstall update).include? state
      # wait with reboot for the finish of configuration update by local chef-client
      # (so dhcp & PXE config is prepared when node is rebooted)
      begin
        Timeout.timeout(300) do
          # - The transition above will result in a chef-client run locally
          #   (through looper_chef_client.sh & blocking_chef_client.sh).
          # - If there was already a chef-client running, then our action will
          #   result in a queued chef-client run; this is marked by the
          #   chef-client.run file. Once the previous chef-client is done, the
          #   marker is removed, and we start a chef-client run.
          # - When a chef-client is running, the chef-client.lock is added. So
          #   we can look for the file and when it disappears, our chef-client
          #   run is over and the transition will have been effective.
          #
          # There are a few very unlikely races, though:
          #   - things are so fast that we execute this code before the markers
          #     are created.
          #   - if the queue marker is removed because previous chef-client run
          #     is over, but things are so fast that we check for the
          #     run marker before it exists.
          #   - if the queue marker is removed because previous chef-client run
          #     is over, but another action from the user leads to the queue
          #     creation again; all of this while we are in sleep(1).
          #   - if the run marker is removed because chef-client run is over,
          #     but another chef-client run is triggered; all of this while we
          #     are in sleep(1).
          # First race is totally unlikely.
          # Second race is worked around by the conditional sleep between our
          # two loops.
          # The last two races just lead to this method taking longer than
          # needed, and the timeout protects us from an infinite loop.

          had_queue = false
          while File.exist?("/var/run/crowbar/chef-client.run")
            had_queue = true
            Rails.logger.debug("chef-client still in the queue")
            sleep(1)
          end

          sleep(1) if had_queue

          while File.exist?("/var/run/crowbar/chef-client.lock")
            Rails.logger.debug("chef-client still running")
            sleep(1)
          end
        end
      rescue Timeout::Error
        Rails.logger.warn("chef client seems to be still running after 5 minutes of wait; going on with the reboot")
      end
      if @node[:platform] == "windows"
        net_rpc_cmd(:power_cycle)
      else
        ssh_cmd("/sbin/reboot")
      end
    end
    results
  end

  def update
    set_state("update")
  end

  def delete
    set_state("delete")
  end

  def reinstall
    set_state("reinstall")
  end

  def reset
    set_state("reset")
  end

  def reboot
    set_state("reboot")
    if @node[:platform] == "windows"
      net_rpc_cmd(:reboot)
    else
      ssh_cmd("/sbin/reboot")
    end
  end

  def shutdown
    set_state("shutdown")
    if @node[:platform] == "windows"
      net_rpc_cmd(:shutdown)
    else
      ssh_cmd("/sbin/poweroff")
    end
  end

  def poweron
    set_state("poweron")
    bmc_cmd("power on")
  end

  def powercycle
    set_state("reboot")
    if @node[:platform] == "windows"
      net_rpc_cmd(:power_cycle)
    else
      bmc_cmd("power cycle")
    end
  end

  def poweroff
    set_state("shutdown")
    if @node[:platform] == "windows"
      net_rpc_cmd(:power_off)
    else
      bmc_cmd("power off")
    end
  end

  def identify
    bmc_cmd("chassis identify")
  end

  def bmc_set?
    return false if @node.nil? or @node["crowbar_wall"].nil? or @node["crowbar_wall"]["status"].nil?
    return false if @node["crowbar_wall"]["status"]["ipmi"].nil?
    return false if @node["crowbar_wall"]["status"]["ipmi"]["address_set"].nil?
    @node["crowbar_wall"]["status"]["ipmi"]["address_set"]
  end

  def run_service(service_name, action)
    platform, version = @node["platform"], @node["platform_version"]
    case service_name
    when :chef
      service_cmd = ChefObject.service_command(platform, version, "chef-client", action)
    end
    if service_cmd.nil?
      Rails.logger.error("run_service: service command for #{platform} #{version} could not be determined")
    else
      ssh_cmd(service_cmd)
    end
  end

  def disk_owner(device)
    if device
      crowbar_wall[:claimed_disks][device][:owner] rescue ""
    else
      nil
    end
  end

  def disk_claim(device, owner)
    if device
      crowbar_wall[:claimed_disks] ||= {}

      unless disk_owner(device).to_s.empty?
        return disk_owner(device) == owner
      end

      Rails.logger.debug "Claiming #{device} for #{owner}"

      crowbar_wall[:claimed_disks][device] ||= {}
      crowbar_wall[:claimed_disks][device][:owner] = owner

      true
    else
      Rails.logger.debug "No device for disk claim given"
      false
    end
  end

  def disk_claim!(device, owner)
    disk_claim(device, owner) and save
  end

  def disk_release(device, owner)
    if device
      crowbar_wall[:claimed_disks] ||= {}

      unless disk_owner(device) == owner
        return false
      end

      Rails.logger.debug "Releasing #{device} from #{owner}"
      crowbar_wall[:claimed_disks][device][:owner] = nil

      true
    else
      Rails.logger.debug "No device for disk release given"
      false
    end
  end

  def disk_release!(device, owner)
    disk_release(device, owner) and save
  end

  def boot_device(device)
    if device
      Rails.logger.debug "Set boot device to #{device}"
      crowbar_wall["boot_device"] = device

      true
    else
      Rails.logger.debug "No device for boot given"
      false
    end
  end

  def boot_device!(device)
    boot_device(device) and save
  end

  # TODO: Remove duplicate code and use a gem/git submodule/whatever...
  # see barclamp-deployer/chef/cookbooks/barclamp/libraries/barclamp_library.rb
  def unique_name_already_claimed_by(device)
    claimed_name = (crowbar_wall[:claimed_disks] || []).find do |claimed_name, v|
      self.link_to_device?(device, claimed_name)
    end || []
    claimed_name.first
  end

  # TODO: Remove duplicate code and use a gem/git submodule/whatever...
  # see barclamp-deployer/chef/cookbooks/barclamp/libraries/barclamp_library.rb
  #
  # is the given linkname a link to the given device? In attributes,
  # that means that the given linkname can be found in the array
  # @node[:block_device][device][by-{id,path,uuid}]
  def link_to_device?(device, linkname)
    # device is i.e. "sda", "vda", ...
    # linkname is i.e. "/dev/disk/by-path/pci-0000:00:04.0-virtio-pci-virtio1"
    return true if File.join("", "dev", device) == linkname
    lookup_and_name = linkname.gsub(/^\/dev\/disk\//, '').split(File::SEPARATOR, 2)
    linked_devs = @node[:block_device][device][:disks][lookup_and_name[0]] rescue []
    linked_devs.include?(lookup_and_name[1]) rescue false
  end

  # IMPORTANT: keep these paths in sync with
  # BarclampLibrary::Barclamp::Inventory::Disk#unique_name
  # within the deployer barclamp to return always similar values.
  # TODO: Remove duplicate code and use a gem/git submodule/whatever...
  # see barclamp-deployer/chef/cookbooks/barclamp/libraries/barclamp_library.rb
  def unique_device_for(device)
    # check first if we have already a claimed disk which points to the same
    # device node. if so, use that as "unique device"
    already_claimed_name = self.unique_name_already_claimed_by(device)
    unless already_claimed_name.nil?
      Rails.logger.debug("Use #{already_claimed_name} as unique_name " \
                         "because already claimed by #{device}")
      return already_claimed_name
    end

    meta = @node["block_device"][device]

    if meta
      # For some disk (e.g. virtio without serial number on SLE12)
      # meta["disks"] is empty. In that case we can't get a "more unique"
      # name than "vdX"
      return "/dev/#{device}" unless meta["disks"]

      disk_lookups = ["by-path"]

      # If this looks like a virtio disk and the target platform is one
      # that might not have the "by-path" links (e.g. SLES 12). Avoid
      # using "by-path".
      if device =~ /^vd[a-z]+$/
        virtio_by_path_platforms = %w(
          ubuntu-12.04
          redhat-6.2
          redhat-6.4
          centos-6.2
          centos-6.4
          suse-11.3
        )
        unless virtio_by_path_platforms.include?(@node[:target_platform])
          disk_lookups = []
        end
      end

      # VirtualBox does not provide stable disk ids, so we cannot rely on them
      # in that case.
      unless hardware =~ /VirtualBox/i
        disk_lookups.unshift "by-id"
      end
      result = disk_lookups.map do |type|
        if meta["disks"][type] and not meta["disks"][type].empty?
          "#{type}/#{meta["disks"][type].first}"
        end
      end

      # virtio disk might have neither by-path nor by-id links, use the /dev/vdX
      # name in that case
      if result.empty?
        "/dev/#{device}"
      else
        "/dev/disk/#{result.compact.first}"
      end
    else
      nil
    end
  end

  private

  # Used for cloning role's default attributes.
  def deep_clone object, options = {}
    case object
    when Numeric,TrueClass,FalseClass,NilClass,Symbol #immutable
      object
    when ::String
      options[:full] ? object.clone : object
    when ::Hash
      object.reduce({}) do |acc,kv|
        acc[deep_clone(kv[0])] = deep_clone(kv[1])
        acc
      end
    when ::Array
       object.reduce([]) do |acc,v|
        acc << deep_clone(v)
      end
    else
      object.clone #deep copy
    end
  end

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
    crowbar["crowbar"] = { "display" => {} } if crowbar["crowbar"].nil?
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
          default['alias'] = default['alias'].gsub(/NODE/,node) unless default['alias'].nil?
        end
        return default
      end
    rescue => exception
      Rails.logger.warn("Optional db\\node_description.yml file not correctly formatted.  Error #{exception.message}")
    end
    {}
  end
end

