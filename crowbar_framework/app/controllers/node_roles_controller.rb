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
      @list = @node.node_roles.current.sort{|a,b|[a.cohort,a.id] <=> [b.cohort,b.id]}
    else
      @list = NodeRole.current.order("cohort asc, id asc")
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
    raise "Cannot add noderole to snapshot in #{Snapshot.state_name(snap.state)}" unless snap.proposed?
    r = NodeRole.create! params

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

  def retry
    @node_role = NodeRole.find_key params[:node_role_id]
    @node_role.state = NodeRole::TODO
    @node_role.save!
    respond_to do |format|
      format.html { render :action => :show }
      format.json { render api_show :node_role, NodeRole, nil, nil, @node_role }
    end

  end

  def anneal
    respond_to do |format|
      format.html { }
      format.json {
        if NodeRole.anneal! || NodeRole.committed.in_state(NodeRole::TODO).count > 0
          render :json => { "message" => "scheduled" }, :status => 202
        elsif NodeRole.committed.in_state(NodeRole::TRANSITION).count > 0
          render :json => { "message" => "working" }, :status => 202
        elsif NodeRole.committed.in_state(NodeRole::ERROR).count > 0
          render :json => { "message" => "failed" }, :status => 409
        else
          render :json => { "message" => "finished" }, :state => 200
        end
      }
    end
  end

end

