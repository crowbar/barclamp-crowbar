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
    n.alive = true
    n.save!
    redirect_to :action => 'index', :status => :found
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
    params[:deployment_id] = Deployment.find_key(params[:deployment]).id if params.has_key? :deployment
    n = Node.create! params
    render api_show :node, Node, n.id.to_s, nil, n
  end
  
  def update
    params[:deployment_id] = Deployment.find_key(params[:deployment]).id if params.has_key? :deployment
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

end