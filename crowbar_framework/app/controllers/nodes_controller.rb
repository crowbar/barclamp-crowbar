#
# Copyright 2011-2013, Dell
# Copyright 2013-2014, SUSE LINUX Products GmbH
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

class NodesController < ApplicationController
  def index
    @sum = 0
    @groups = {}
    session[:node] = params[:name]
    if params.has_key?(:role)
      result = NodeObject.all #this is not efficient, please update w/ a search!
      @nodes = result.find_all { |node| node.role? params[:role] }
      if params.has_key?(:names_only)
         names = @nodes.map { |node| node.handle }
         @nodes = {:role=>params[:role], :nodes=>names, :count=>names.count}
      end
    else
      @nodes = {}
      raw_nodes = NodeObject.all
      get_node_and_network(params[:selected]) if params[:selected]
      raw_nodes.each do |node|
        @sum = @sum + node.name.hash
        @nodes[node.handle] = { :alias=>node.alias, :description=>node.description, :status=>node.status, :state=>node.state }
        group = node.group
        @groups[group] = { :automatic=>!node.display_set?('group'), :status=>{"ready"=>0, "failed"=>0, "unknown"=>0, "unready"=>0, "pending"=>0}, :nodes=>{} } unless @groups.key? group
        @groups[group][:nodes][node.group_order] = node.handle
        @groups[group][:status][node.status] = (@groups[group][:status][node.status] || 0).to_i + 1
        if node.handle === params[:name]
          @node = node
          get_node_and_network(node.handle)
        end
      end
      flash[:notice] = "<b>#{t :warning, :scope => :error}:</b> #{t :no_nodes_found, :scope => :error}" if @nodes.empty? #.html_safe if @nodes.empty?
    end

    respond_to do |format|
      format.html
      format.xml { render :xml => @nodes }
      format.json { render :json => @nodes }
    end
  end

  def list
    @allocated = true

    @nodes = {}.tap do |nodes|
      NodeObject.all.each do |node|
        if node.target_platform.blank?
          node.target_platform = @template.default_platform
          node.save
        end

        nodes[node.handle] = node
      end
    end

    respond_to do |format|
      format.html { render "list" }
    end
  end

  def unallocated
    @allocated = false

    @nodes = {}.tap do |nodes|
      NodeObject.all.each do |node|
        if node.target_platform.blank?
          node.target_platform = @template.default_platform
          node.save
        end

        unless node.allocated?
          nodes[node.handle] = node
        end
      end
    end

    respond_to do |format|
      format.html { render "list" }
    end
  end

  def bulk
    @report = {
      :success => [],
      :failed => [],
      :duplicate => false
    }.tap do |report|
      node_values = params[:node] || {}

      node_aliases = node_values.values.map do |attributes|
        attributes["alias"]
      end

      node_values.each do |node_name, node_attributes|
        if node_aliases.grep(node_attributes["alias"]).size > 1
          report[:duplicate] = true
          report[:failed].push node_name
        end
      end

      unless report[:duplicate]
        node_values.each do |node_name, node_attributes|
          begin
            dirty = false
            node = NodeObject.find_node_by_name node_name

            if node_attributes["allocate"] and not node.allocated
              node.allocated = true
              dirty = true
            end

            unless node.alias == node_attributes["alias"]
              node.alias = node_attributes["alias"]
              dirty = true
            end

            unless node.public_name == node_attributes["public_name"]
              node.public_name = node_attributes["public_name"]
              dirty = true
            end

            unless node.target_platform == node_attributes["target_platform"]
              node.target_platform = node_attributes["target_platform"]
              dirty = true
            end

            unless node.license_key == node_attributes["license_key"]
              node.license_key = node_attributes["license_key"]
              dirty = true
            end

            unless node.intended_role == node_attributes["intended_role"]
              node.intended_role = node_attributes["intended_role"]
              dirty = true
            end

            if @template.crowbar_options[:show].include?(:bios) and not [node.bios_set, "not_set"].include? node_attributes["bios"]
              node.bios_set = node_attributes["bios"]
              dirty = true
            end

            if @template.crowbar_options[:show].include?(:raid) and not [node.raid_set, "not_set"].include? node_attributes["raid"]
              node.raid_set = node_attributes["raid"]
              dirty = true
            end

            unless node.group == node_attributes["group"]
              unless node_attributes["group"].blank? or node_attributes["group"] =~ /^[a-zA-Z][a-zA-Z0-9._:-]+$/
                raise I18n.t("nodes.list.group_error", :node => node.name)
              end

              node.group = node_attributes["group"]
              dirty = true
            end

            if dirty
              node.save
              report[:success].push node_name
            end
          rescue StandardError => e
            log_exception(e)
            report[:failed].push node_name
          end
        end
      end
    end

    if @report[:failed].length > 0
      node_list = @report[:failed].map do |node_name|
        node_name.split(".").first
      end

      flash[:alert] = I18n.t(
        @report[:duplicate] ? "nodes.list.duplicates" : "nodes.list.failed",
        :failed => node_list.to_sentence
      )
    elsif @report[:success].length > 0
      node_list = @report[:success].map do |node_name|
        node_name.split(".").first
      end

      flash[:notice] = I18n.t("nodes.list.updated", :success => node_list.to_sentence)
    else
      flash[:info] = I18n.t("nodes.list.nochange")
    end

    redirect_to params[:return] != "true" ? unallocated_list_path : nodes_list_path
  end

  def families
    @families = {}.tap do |families|
      NodeObject.all.each do |node|
        family = node.family.to_s

        unless families.has_key? family
          families[family] = {
            :names => [],
            :family => node.family
          }
        end

        families[family][:names].push({
          :alias => node.alias,
          :description => node.description,
          :handle => node.handle
        })
      end
    end
  end

  def group_change
    NodeObject.find_node_by_name(params[:id]).tap do |node|
      raise ActionController::RoutingError.new('Not Found') if node.nil?

      if params[:group].downcase.eql? 'automatic'
        node.group = ""
      else
        node.group = params[:group]
      end

      node.save

      Rails.logger.info "Node #{node.name} (#{node.alias}) changed its group to be #{node.group || "automatic"}."
      render :inline => "Added #{node.name} to #{node.group}.", :cache => false
    end
  end

  def status
    @result = {
      :nodes => {},
      :groups => {}
    }.tap do |result|
      begin
        NodeObject.all.each do |node|
          group_name = node.group || I18n.t("unknown")

          result[:groups][group_name] ||= begin
            {
              :tooltip => "",
              :status => {
                "ready" => 0,
                "failed" => 0,
                "pending" => 0,
                "unready" => 0,
                "building" => 0,
                "unknown" => 0
              }
            }
          end

          result[:groups][group_name].tap do |group|
            group[:status][node.status] = group[:status][node.status] + 1
            group[:tooltip] = @template.piechart_tooltip(@template.piechart_values(group))
          end

          result[:nodes][node.handle] = {
            :class => node.status,
            :status => I18n.t(node.state, :scope => :state, :default => node.state.titlecase)
          }
        end
      rescue => e
        log_exception(e)
        result[:error] = e.message
      end
    end

    respond_to do |format|
      format.json { render :json => @result }
    end
  end

  def hit
    action = params[:req]
    name = params[:name] || params[:id]
    machine = NodeObject.find_node_by_name name
    if machine.nil?
      render :text=>"Could not find node '#{name}'", :status => 404 and return
    else
      case action
      when 'reinstall', 'reset', 'update', 'delete'
        machine.set_state(action)
      when 'reboot', 'shutdown', 'poweron', 'identify', 'allocate'
        machine.send(action)
      else
        render :text=>"Invalid hit request '#{action}'", :status => 500 and return
      end
    end
    render :text=>"Attempting '#{action}' for node '#{machine.name}'", :status => 200
  end

  # GET /nodes/1
  # GET /nodes/1.xml
  def show
    get_node_and_network(params[:id] || params[:name])
    raise ActionController::RoutingError.new("Node #{params[:id] || params[:name]}: not found") if @node.nil?
    respond_to do |format|
      format.html # show.html.erb
      format.xml  { render :xml => @node }
      format.json { render :json => (params[:key].nil? ? @node : @node[params[:key]]) }
    end
  end

  def edit
    get_node_and_network(params[:id] || params[:name])
  end

  def update
    unless request.post?
      raise ActionController::UnknownHttpMethod.new("POST is required to update proposal #{params[:id]}")
    end

    get_node_and_network(params[:id] || params[:name])
    raise ActionController::RoutingError.new("Node #{params[:id] || params[:name]}: not found") if @node.nil?

    if params[:submit] == t('nodes.form.allocate')
      @node.allocated = true
      flash[:notice] = t('nodes.form.allocate_node_success') if save_node(true)
    elsif params[:submit] == t('nodes.form.save')
      flash[:notice] = t('nodes.form.save_node_success') if save_node(false)
    else
      Rails.logger.warn "Unknown action for node edit: #{params[:submit]}"
      flash[:notice] = "Unknown action: #{params[:submit]}"
    end
    redirect_to nodes_path(:selected => @node.name)
  end

  #this code allow us to get values of attributes by path of node
  def attribute
    @node = NodeObject.find_node_by_name(params[:name])
    raise ActionController::RoutingError.new("Node #{params[:name]}: not found") if @node.nil?
    @attribute = @node.to_hash
    params[:path].to_a.each do |element|
      @attribute = @attribute[element]
      raise ActionController::RoutingError.new("Node #{params[:name]}: unknown attribute #{params[:path].join('/')}") if @attribute.nil?
    end
    render :json => {:value => @attribute}
  end

  private

  def save_node(change_target_platform)
    if params[:group] and params[:group] != "" and !(params[:group] =~ /^[a-zA-Z][a-zA-Z0-9._:-]+$/)
      flash[:notice] = @node.name + ": " + t('nodes.list.group_error')
      return false
    end
    begin
      # if we don't have OpenStack, availability_zone will be empty; which is
      # okay, because we don't care about this in that case
      {
        :bios_set      => :bios,
        :raid_set      => :raid,
        :alias         => :alias,
        :public_name   => :public_name,
        :group         => :group,
        :description   => :description,
        :availability_zone => :availability_zone,
        :intended_role => :intended_role,
        :raid_type     => :raid_type,
        :raid_disks    => :raid_disks
      }.each do |attr, param|
        @node.send("#{attr}=", params[param]) if params.key?(param)
      end

      if change_target_platform
        @node.target_platform = params[:target_platform] || @template.default_platform
        @node.license_key = params[:license_key]
      end
      @node.save
      true
    rescue StandardError => e
      log_exception(e)
      flash[:alert] = I18n.t("nodes.form.failed",
                             :node => @node.name,
                             :message => e.message)
      false
    end
  end

  def get_node_and_network(node_name)
    network = {}
    @network = []
    @node = NodeObject.find_node_by_name(node_name) if @node.nil?
    if @node
      if @node.target_platform.blank?
        @node.target_platform = @template.default_platform
        @node.save
      end
      # If we're in discovery mode, then we have a temporary DHCP IP address.
      if not ['discovering', 'discovered', 'hardware-installing', 'hardware-installed'].include? @node.state
        intf_if_map = @node.build_node_map
        # build network information (this may need to move into the object)
        @node.networks.each do |intf, data|
          if data["usage"] == "bmc"
            ifname = "bmc"
            address = @node["crowbar_wall"]["ipmi"]["address"] rescue nil
          else
            ifname, ifs, team = @node.lookup_interface_info(data["conduit"])
            if ifname.nil? or ifs.nil?
              ifname = "Unknown"
            else
              ifname = "#{ifname}[#{ifs.join(",")}]" if ifs.length > 1
            end
            address = data["address"]
          end
          if address
            network[data["usage"]] = {} if network[data["usage"]].nil?
            network[data["usage"]][ifname] = address
          end
        end
        @network = network.sort
        @network << ['[not managed]', @node.unmanaged_interfaces] unless @node.unmanaged_interfaces.empty?
      elsif @node.state == 'discovering'
        @network = [ ['[dhcp]', 'discovering'] ]
      else
        @network = [ ['[dhcp]', @node[:ipaddress]] ]
      end
    end

    @network
  end
end
