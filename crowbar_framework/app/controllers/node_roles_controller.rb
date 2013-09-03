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

class NodeRolesController < ApplicationController

  def index
    if params.key? :node_id
      @node = Node.find_key params[:node_id]
      @list = @node.node_roles
    else
      @list = NodeRole.all
    end
    respond_to do |format|
      format.html { }
      format.json { render api_index :node_role, @list }
    end
  end

  def show
    respond_to do |format|
      format.html { @node_role = NodeRole.find_key params[:id] }
      format.json { render api_show :node_role, NodeRole }
    end
  end

  def create
    # helpers to allow create by names instead of IDs
    if params.key? :snapshot
      params[:snapshot_id] = Snapshot.find_key(params[:snapshot]).id
    elsif params.key? :deployment
      params[:snapshot_id] = Deployment.find_key(params[:deployment]).head.id
    end
    params[:node_id] = Node.find_key(params[:node]).id if params.key? :node
    params[:role_id] = Role.find_key(params[:role]).id if params.key? :role
    
    # the main body of the work
    snap = Snapshot.find_key params[:snapshot_id] 
    # it matters what state we are in when we add the node role (we store it because it can be expensive to compute)
    snapstate = snap.state 
    # we cant add to an active snap, so create proposal if there isn't
    if snapstate == NodeRole::ACTIVE 
      proposal = snap.propose
      params[:snapshot_id] = proposal.id
    end
    r = NodeRole.create! params

    # if we are committed then we need to add the node roles as TODO
    # this must be done AFTER the node role is created because of the NR state machine
    if snapstate == NodeRole::TODO
      r.state = NodeRole::TODO
      r.save
    end

    render api_show :node_role, NodeRole, nil, nil, r 
  end

  def update
    @node_role = NodeRole.find_key params[:id]
    # we can build the data from the input
    if params.key? :dataprefix
      params[:data] ||= {}
      params.each do |k,v|
        if k.start_with? params[:dataprefix]
          key = k.sub(params[:dataprefix],"")
          params[:data][key] = v
        end
      end
    end
    # if you've been passed data then save it
    unless params[:data].nil?
      @node_role.data = params[:data]
      @node_role.save!
      flash[:notice] = I18n.t 'saved', :scope=>'layouts.node_roles.show'
    end
    respond_to do |format|
      format.html { render 'show' }
      format.json { render api_show :node_role, NodeRole, nil, nil, @node_role }
    end
  end

  def destroy
    unless Rails.env.development?
      render  api_not_supported("delete", "node_role")
    else
      render api_delete NodeRole
    end
  end

  def anneal
    NodeRole.anneal!
  end
  
end

