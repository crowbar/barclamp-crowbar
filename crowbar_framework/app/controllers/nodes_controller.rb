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
class NodesController < ApplicationController

  require 'chef'

  # GET /nodes
  # GET /nodes.xml
  def index
    @sum = 0
    session[:node] = params[:name]
    if params.has_key?(:role)
      result = NodeObject.all #this is not efficient, please update w/ a search!
      @nodes = result.find_all { |node| node.role? params[:role] }
      if params.has_key?(:names_only)
         names = @nodes.map { |node| node.handle }
         @nodes = {:role=>params[:role], :nodes=>names, :count=>names.count}
      end
    else
      @groups = {}
      @nodes = {}
      raw_nodes = NodeObject.all
      get_node_and_network(params[:selected]) if params[:selected]
      raw_nodes.each do |node|
        @sum = @sum + node.name.hash
        @nodes[node.handle] = { :alias=>node.alias, :description=>node.description, :status=>node.status }
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
      format.html # index.html.haml
      format.xml  { render :xml => @nodes }
      format.json { render :json => @nodes }
    end
  end

  def list
    if request.post?
      nodes = {}
      params.each do |k, v|
        if k.starts_with? "node:"
          parts = k.split ':'
          node = parts[1]
          area = parts[2]
          nodes[node] = {} if nodes[node].nil?
          nodes[node][area] = (v.empty? ? nil : v)
        end
      end
      succeeded = []
      failed = []
      # before we start saving, make sure someone did not give us duplicate aliases
      # this SHOULD Be causght by the node.save but race conditoins are breaking the constency of the DB
      alias_dup = false
      nodes.each do |node_name, values|
        nodes.each do |nn, vv|
           alias_dup = true if nn!=node_name and vv['alias'] == values['alias']
           failed << node_name if alias_dup 
           break if alias_dup
        end
      end
      unless alias_dup
        nodes.each do |node_name, values|
          begin
            dirty = false
            node = NodeObject.find_node_by_name node_name
            if !node.allocated and values['allocate'] === 'checked'
              node.allocated = true
              dirty = true
            end
            if !(node.description == values['description'])
              node.description = values['description']
              dirty = true
            end
            if !(node.alias == values['alias'])
                node.alias = values['alias']
                dirty = true
            end
            if !(node.public_name == values['public_name'])
                node.public_name = values['public_name']
                dirty = true
            end
            if !(node.group == values['group'])
              if values['group'] and values['group'] != "" and !(values['group'] =~ /^[a-zA-Z][a-zA-Z0-9._:-]+$/)
                raise node.name + ": " + t('nodes.list.group_error')
              end
              node.group = values['group']
              dirty = true
            end
            if !values['bios'].nil? and values['bios'].length>0 and !(node.bios_set === values['bios']) and !(values['bios'] === 'not_set')
              node.bios_set = values['bios']
              dirty = true
            end
            if !values['raid'].nil? and values['raid'].length>0 and !(node.raid_set === values['raid']) and !(values['raid'] === 'not_set')
              node.raid_set = values['raid']
              dirty = true
            end
            if !(node.target_platform == values['target_platform'])
              node.target_platform = values['target_platform']
              unless CrowbarService.require_license_key?(node.target_platform)
                 values['license_key'] = ""
              end
              dirty = true
            end
            if !(node.license_key == values['license_key'])
              node.license_key = values['license_key']
              dirty = true
            end
            if dirty
              begin
                node.save
                succeeded << node_name
              rescue StandardError => e
                log_exception(e)
                failed << node_name
              end
            end
          rescue StandardError => e
            log_exception(e)
            failed << node_name
          end
        end
      end
      if failed.length>0
        flash[:notice] = failed.join(',') + ": " + I18n.t('failed', :scope=>'nodes.list')
      elsif succeeded.length>0
        flash[:notice] = succeeded.join(',') + ": " + I18n.t('updated', :scope=>'nodes.list')
      else
        flash[:notice] = I18n.t('nochange', :scope=>'nodes.list')
      end
    end

    @options = CrowbarService.read_options
    @columns = @options[:show].count + 8

    @nodes = {}

    default_os = find_default_os

    NodeObject.all.each do |node|
      if node.target_platform.blank?
         node.target_platform = default_os
         node.save
      end
      @nodes[node.handle] = node if params[:allocated].nil? or !node.allocated?
    end

    @target_platforms = options_target_platform(default_os)
  end

  def families
    nodes = NodeObject.all
    @families = {}
    nodes.each do |n|
      f = n.family.to_s  
      @families[f] = {:names=>[], :family=>n.family} unless @families.has_key? f
      @families[f][:names] << {:alias=>n.alias, :description=>n.description, :handle=>n.handle}
    end
  end
  
  def group_change
    node = NodeObject.find_node_by_name params[:id]
    if node.nil?
      raise "Node #{params[:id]} not found.  Cannot change group" 
    else
      group = params[:group]
      if params.key? 'automatic'
        node.group=""
      else
        node.group=group
      end
      node.save
      Rails.logger.info "node #{node.name} (#{node.alias}) changed its group to be #{node.group.empty? ? 'automatic' : group}."
      render :inline => "SUCCESS: added #{node.name} to #{group}.", :cache => false 
    end
  end
  
  def status
    nodes = {}
    groups = {}
    sum = 0
    begin
      result = NodeObject.all
      result.each do |node|
        nodes[node.handle] = {:status=>node.status, :raw=>node.state, :state=>(I18n.t node.state, :scope => :state, :default=>node.state.titlecase)}
        count = groups[node.group] || {"ready"=>0, "failed"=>0, "pending"=>0, "unready"=>0, "building"=>0, "unknown"=>0}
        count[node.status] = count[node.status] + 1
        groups[node.group || I18n.t('unknown') ] = count
        sum = sum + node.name.hash
      end
      render :inline => {:sum => sum, :nodes=>nodes, :groups=>groups, :count=>nodes.length}.to_json, :cache => false
    rescue StandardError => e
      count = (e.class.to_s == "Errno::ECONNREFUSED" ? -2 : -1)
      Rails.logger.fatal("Failed to iterate over node list due to '#{e.message}'\n#{e.backtrace.join("\n")}")
      render :inline => {:nodes=>nodes, :groups=>groups, :count=>count, :error=>e.message}, :cache => false
    end
  end

  def hit
    action = params[:req]
    name = params[:name] || params[:id]
    machine = NodeObject.find_node_by_name name
    if machine.nil?
      render :text=>"Could not find node '#{name}'", :status => 404
    else
      case action
      when 'reinstall', 'reset', 'update', 'delete'
        machine.set_state(action)
      when 'reboot'
        machine.reboot
      when 'shutdown'
        machine.shutdown
      when 'poweron'
        machine.poweron
      when 'identify'
        machine.identify
      when 'allocate'
        machine.allocate
      else
        render :text=>"Invalid hit request '#{action}'", :status => 500
      end
    end
    render :text=>"Attempting '#{action}' for node '#{machine.name}'", :status => 200
  end

  # GET /nodes/1
  # GET /nodes/1.xml
  def show
    get_node_and_network(params[:id] || params[:name])
    respond_to do |format|
      format.html # show.html.erb
      format.xml  { render :xml => @node }
      format.json { render :json => (params[:key].nil? ? @node : @node[params[:key]]) }
    end
  end

  def edit
    @options = CrowbarService.read_options
    @target_platforms = options_target_platform(find_default_os)
    get_node_and_network(params[:id] || params[:name])
  end

  def update
    if request.post?
      get_node_and_network(params[:id] || params[:name])
      if params[:submit] == t('nodes.form.allocate')
        @node.allocated = true
        flash[:notice] = t('nodes.form.allocate_node_success') if save_node(true)
      elsif params[:submit] == t('nodes.form.save')
        flash[:notice] = t('nodes.form.save_node_success') if save_node(false)
      else
        Rails.logger.warn "Unknown action for node edit: #{params[:submit]}"
        flash[:notice] = "Unknown action: #{params[:submit]}"
      end
    else
      Rails.logger.warn "PUT is required to update proposal #{params[:id]}"
      flash[:notice] = "PUT required"
    end
    redirect_to nodes_path(:selected => @node.name)
  end

  #this code allow us to get values of attributes by path of node
  def attribute
    @node = NodeObject.find_node_by_name(params[:name])
    raise ActionController::RoutingError.new("Node #{params[:name]}: not found") if @node.nil?
    @attribute = @node.to_hash
    params[:path].each do |element|
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
      @node.bios_set = params[:bios]
      @node.raid_set = params[:raid]
      @node.alias = params[:alias]
      @node.public_name = params[:public_name]
      @node.group = params[:group]
      @node.description = params[:description]
      if change_target_platform
        @node.target_platform = params[:target_platform]
        @node.license_key = params[:license_key]
        unless CrowbarService.require_license_key?(@node.target_platform)
          @node.license_key = ""
        end
      end
      @node.save
      true
    rescue StandardError => e
      log_exception(e)
      flash[:notice] = @node.name + ": " + t('nodes.list.failed') + ": " + e.message
      false
    end
  end

  def get_node_and_network(node_name)
    network = {}
    @network = []
    @node = NodeObject.find_node_by_name(node_name) if @node.nil?
    if @node
      if @node.target_platform.blank?
        @node.target_platform = find_default_os
        @node.save
      end
      intf_if_map = @node.build_node_map
      # build network information (this may need to move into the object)
      @node.networks.each do |intf, data|
        network[data["usage"]] = {} if network[data["usage"]].nil?
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
        network[data["usage"]][ifname] = address || 'n/a'
      end
      @network = network.sort
      @network << ['[not managed]', @node.unmanaged_interfaces] unless @node.unmanaged_interfaces.empty?
    end

    @network
  end

  def find_default_os
    NodeObject.all.each do |node|
      return "#{node[:platform]}-#{node[:platform_version]}" if node.admin?
    end
    return ""
  end

  def options_target_platform(default_os)
    return {
      CrowbarService.pretty_target_platform(default_os) => default_os,
      CrowbarService.pretty_target_platform("windows-6.2") => "windows-6.2",
      CrowbarService.pretty_target_platform("hyperv-6.2") => "hyperv-6.2"
    }
  end
end
