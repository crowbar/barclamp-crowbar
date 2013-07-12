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

  # API GET /crowbar/v2/nodes
  # UI GET /dashboard
  def index
    @list = if params.has_key? :group_id
      g = Group.find_key params[:group_id]
      g.nodes
    else
      Node.all
    end
    respond_to do |format|
      format.html { }
      format.json { render api_index :node, @list }
    end
  end
  
  def status
    
    groups = { 0=>{"name"=>'all', "ready"=>0, "failed"=>0, "pending"=>0, "unready"=>0, "building"=>0, "unknown"=>0} }
    status = {}
    state = {}
    i18n = {}
    sum = Node.name_hash
    begin
      result = Node.find_keys params[:id]
      unless result.nil?
        result.each do |node|
          #Rails.logger.debug "ZEHICLE #{node.name} nodes_controller"
          # CB2 temporary polling
          # Jig.refresh_node "temporary polling from nodes_controller.status", node
          
          # CB1 approach
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
  
  # CB1 move to IMPI
  def hit
    action = params[:req]
    name = params[:name] || params[:id]
    render api_not_supported action, name
  end
    
  def show
    respond_to do |format|
      format.html { @node = Node.find_key params[:id] } # show.html.erb
      format.json { render api_show :node, Node }
    end
  end

  # RESTful DELETE of the node resource
  def destroy
    n = Node.find_key(params[:id] || params[:name])
    Rails.logger.info("Will delete #{n.name}")
    Jig.delete_node(n)
    render api_delete Node
  end
  
  # RESTfule POST of the node resource
  def create
    n = Node.create! params
    Jig.create_node(n)
    render api_show :node, Node, n.id.to_s, nil, n
  end
  
  def update
    render api_update :node, Node
  end

  def attribs
    unless params[:version].eql?('v2') 
      render :text=>I18n.t('api.wrong_version', :version=>params[:version])
    else
      # working objects
      node = Node.find_key params[:id]
      # we need to treat attribs by type OR ID 
      # except that the ID is the attrib while the name is the type
      if params[:attrib]
        attrib = AttribType.add params[:attrib]
        ai = Attrib.find_by_node_id_and_attrib_id node.id, attrib.id
      elsif params[:attrib] =~ /^[0-9]+$/
        ai = Attrib.find params[:attrib]
        attrib = ai.attrib
      end
  
      # POST and PUT (do the same thing since PUT will create the missing info)
      if request.post? or request.put?
        # this is setup to add the param even if we could not find it earlier
        ai.actual = params["value"]
        render api_show :attrib, Attrib, nil, nil, ai
      # DELETE
      elsif request.delete? and attrib and node
        render api_delete Attrib, ai.id
      # fall through REST actions (all require ID)
      elsif request.get? and attrib
        render api_show :attrib, Attrib, nil, nil, ai
      elsif params[:attrib]
        render :text=>I18n.t('api.not_found', :type=>'attrib', :id=>params[:attrib]), :status => :not_found
      # list (no ID)
      elsif request.get?  
        render api_index :attrib, node.attribs, nodes_attribs_path
      # Catch
      else
        render :text=>I18n.t('api.unknown_request'), :status => 400
      end
    end
  end

  def transition
    unless request.put?
      render api_not_supported 'post|delete|get', 'node/:id/transition'
      return
    end
    key = params[:id] || params[:node_id]
    n = Node.find_key(key) || nil
    unless n
      render(:text=>I18n.t('api.not_found', :id=>key, :type=>type.to_s), :status => :not_found)
    end
    sa = n.state_attrib
    old_state = sa.state
    new_state = params[:value] || params[:state]
    # rest_of_transition(n,old_state,state)
    sa.state = new_state # may need to change
    sa.save
    # Just show the node for now.  This will need to evolve.
    render api_show :node, Node, n.id.to_s, nil, n
  end

  def allocate
    render api_not_supported 'put', 'node/allocate'
  end

  # RESTfule PUT of the node resource
  # CB1 - please review & update
  def update_remove
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

private

=begin 
Find a node by name or ID based on the passed in params
in: params from request
=end

  def find_node(params)
    if p= params[:name]
      return Node.find_by_name p
    end
  
    if id= params[:id]
      return Node.find_by_id id
    end
  end
end
