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
    # place holder
  end

  def make_admin
    n = Node.make_admin!
    redirect_to :action => 'index', :status => :found
  end

  # CB1 move to IMPI
  def hit
    action = params[:req]
    name = params[:name] || params[:id]
    render api_not_supported action, name
  end

  def show
    @node = Node.find_key params[:id]
    respond_to do |format|
      format.html {  } # show.html.erb
      format.json { render api_show :node, Node, nil, nil, @node }
    end
  end

  # RESTful DELETE of the node resource
  def destroy
    n = Node.find_key(params[:id] || params[:name])
    render api_delete Node
  end
  
  # RESTfule POST of the node resource
  def create
    n = Node.create! params
    render api_show :node, Node, n.id.to_s, nil, n
  end
  
  def update
    # If we wind up changing to being alive and available,
    # we will want to enqueue some noderoles to run.
    node = Node.find_key params[:id]
    # discovery requires a direct save
    if params.include? :discovery
      node.discovery = params[:discovery]
      node.save!
    end
    render api_update :node, Node, nil, @node
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

  def update_hook(o,old_attrs)
    # Call the on_node_change hook with any updated node attrs.
    # This will include aliveness and availability.
    unless old_attrs.empty?
      Rails.logger.info("Node: calling all role on_node_change hooks for #{o.name}")
      Role.all.each do |r|
        r.on_node_change(o,old_attrs)
      end
    end
    # Enqueue any roles we need if we changed aliveness and availablity
    # and wound up with a node that is both alive and available.
    if (old_attrs.include?("available") ||
        old_attrs.include?("alive")) &&
        o.alive && o.available
      Rails.logger.info("Node: #{o.name} is alive and available, enqueing noderoles to run.")
      o.node_roles.runnable.each do |nr|
        Run.enqueue(nr)
      end
    end
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

  def find_node(params)
    if p= params[:name]
      return Node.find_by_name p
    end
    if id= params[:id]
      return Node.find_by_id id
    end
  end
end
