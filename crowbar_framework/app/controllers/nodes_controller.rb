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
#
class NodesController < ApplicationController

  # API GET /2.0/crowbar/2.0/nodes
  # UI GET /dashboard
  def index
    # EventQueue.publish(Events::WebEvent.new("nodes index page"))
    # k = Delayed::Job.enqueue(Jobs::TestJob.new)
    # puts "DEBUG: k = #{k.inspect}"

    @sum = Node.sum(:fingerprint)
    @groups = Group.find_all_by_category 'ui'
    @node = Node.find_key params[:id]
    session[:node] = params[:id]
    if params.has_key?(:role)
      result = Node.all 
      @nodes = result.find_all { |node| node.role? params[:role] }
      if params.has_key?(:names_only)
         names = @nodes.map { |node| node.name }
         @nodes = {:role=>params[:role], :nodes=>names, :count=>names.count}
      end
    else
      @nodes = {}
      Node.all.each { |n| @nodes[n.id]=n.name }
    end
    respond_to do |format|
      format.html # index.html.haml
      format.json { render :json => @nodes }
    end
  end

  # Bulk Edit
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
            # TODO: can one DE-allocate a node in bluk-edit?  If so, we need to add that here...
            node = Node.find_by_name node_name
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
            if !(node.group.name == values['group'])
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
            if dirty
              begin
                node.save!
                succeeded << node_name
              rescue Exception=>e
                failed << node_name
              end
            end
          rescue Exception=>e
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
    @nodes = {}
    Node.all.each do |node|
      @nodes[node.name] = node if params[:allocated].nil? or !node.allocated?
    end
  end

  def families
    @families = {}
    Node.all.each do |n|
      f = n.family.to_s  
      @families[f] = {:names=>[], :family=>n.family} unless @families.has_key? f
      @families[f][:names] << {:alias=>n.alias, :description=>n.description, :handle=>n.name}
    end
  end
  
  def group_change
    # TODO: not used?
    node = Node.find_by_name params[:id]
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
    
    groups = { 0=>{"name"=>'all', "ready"=>0, "failed"=>0, "pending"=>0, "unready"=>0, "building"=>0, "unknown"=>0} }
    status = {}
    state = {}
    i18n = {}
    sum = Node.sum(:fingerprint)
    begin
      result = Node.find_keys params[:id]
      unless result.nil?
        result.each do |node|
          state[node.id] = node.state
          status[node.id] = node.status
          i18n[node.state] = I18n.t node.state, :scope =>'state', :default=>node.state unless i18n.has_key? node.state
          node.groups.each do |group|
            groups[group.id] ||= {"name"=>group.name, "ready"=>0, "failed"=>0, "pending"=>0, "unready"=>0, "building"=>0, "unknown"=>0}
            groups[group.id][node.status] += 1 
          end
          groups[0][node.status] += 1
        end
      end
    end
    render :inline => {:sum => sum, :status=>status, :state=>state, :i18n=>i18n, :groups=>groups, :count=>state.length}.to_json, :cache => false
    
  end

  def hit
    action = params[:req]
    name = params[:name] || params[:id]
    machine = Node.find_by_name name
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

  # GET /2.0/node/1
  # GET /2.0/node/foo.example.com
  def show
    # for temporary backwards compatibility, we'll combine the chef object and db object
    @node = Node.find_key params[:id]
    respond_to do |format|
      format.html # show.html.erb
      format.json {
        render :json => @node
      }
    end
  end

  # RESTful DELETE of the node resource
  def destroy
    target = Node.find_key params[:id]
    if target.nil?
      render :text=>"Could not find node '#{params[:id]}'", :status => 404
    else
      if target.delete
        render :text => "Node #{params[:id]} deleted!"
      else
        render :text=>"Could not delete node '#{params[:id]}'", :status => 500
      end
    end
  end
  
  # RESTfule POST of the node resource
  def create
    if request.post?
      @node = Node.create! params
      render :json => @node
    end
  end
  
  def edit
    @node = Node.find_key params[:id]
    @groups = {}
    Group.all(:conditions=>["category=?",'ui']).each { |g| @groups[g.name] = g.id }
  end


  def allocate
    # allocate node
  end

  # RESTfule PUT of the node resource
  def update
    get_node_and_network(params[:id] || params[:name])
    if params[:submit] == t('nodes.edit.allocate')
      @node.allocated = true
      flash[:notice] = t('nodes.edit.allocate_node_success') if save_node
    elsif params[:submit] == t('nodes.edit.save')
      flash[:notice] = t('nodes.edit.save_node_success') if save_node
    else
      Rails.logger.warn "Unknown action for node edit: #{params[:submit]}"
      flash[:notice] = "Unknown action: #{params[:submit]}"
    end

    redirect_to nodes_path(:selected => @node.name)
  end

  private

  def save_node
    begin
      # TODO: add raid and bios save at some point
      # @node.bios_set = params[:bios]
      # @node.raid_set = params[:raid]
      @node.alias = params[:alias]
      @node.group = Group.find(params[:group])
      @node.description = params[:description]
      @node.save
      true
    rescue Exception=>e
      flash[:notice] = @node.name + ": " + t('nodes.list.failed') + ": " + e.message
      false
    end
  end

  def get_node_and_network(node_name)
    # TODO - figure out how to get the network stuff below...
    @network = {}
    @node = Node.find_by_name(node_name) if @node.nil?
    @node = Node.find_by_id(node_name) if @node.nil?
=begin
    if @node
      chef_node = @node.jig_hash
      intf_if_map = chef_node.build_node_map # HACK: XXX: This should be something else
      # build network information (this may need to move into the object)
      chef_node.networks.each do |intf, data|
        @network[data["usage"]] = {} if @network[data["usage"]].nil?
        if data["usage"] == "bmc"
          ifname = "bmc"
        else
          ifname, ifs, team = chef_node.lookup_interface_info(data["conduit"])
          if ifname.nil? or ifs.nil?
            ifname = "Unknown"
          else
            ifname = "#{ifname}[#{ifs.join(",")}]" if ifs.length > 1
          end
        end
        @network[data["usage"]][ifname] = data["address"] || 'n/a'
      end
      @network['[not managed]'] = chef_node.unmanaged_interfaces
    end
    @network.sort
=end
  end

end
