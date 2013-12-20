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
              Group.find_key(params[:group_id]).nodes
            elsif params.has_key? :deployment_id
              Deployment.find_key(params[:deployment_id]).nodes
            elsif params.has_key? :snapshot_id
              Snapshot.find_key(params[:snapshot_id]).nodes
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

  def reboot
    node_action :reboot
  end

  def debug
    node_action :debug
  end

  def undebug
    node_action :undebug
  end

  # RESTfule POST of the node resource
  def create
    params[:deployment_id] = Deployment.find_key(params[:deployment]).id if params.has_key? :deployment
    # deal w/ hint shortcuts  (these are hardcoded but MUST match the imported Attrib list)
    hint = JSON.parse(params[:hint] || "{}")
    hint["network-admin"] = {"v4addr"=>params["ip"]} if params.has_key? :ip
    hint["provisioner-repos"] = {"admin_mac"=>params["mac"]} if params.has_key? :mac
    params[:hint] = JSON.generate(hint)

    n = Node.create! params
    render api_show :node, Node, n.id.to_s, nil, n
  end
  
  def update
    params[:deployment] ||= params[:node][:deployment] if params.has_key? :node
    params[:deployment_id] = Deployment.find_key(params[:deployment]).id if params.has_key? :deployment
    # If we wind up changing to being alive and available,
    # we will want to enqueue some noderoles to run.
    @node = Node.find_key params[:id]
    # discovery requires a direct save
    if params.has_key? :discovery
      @node.discovery = params[:discovery]
      @node.save!
    end
    render api_update :node, Node, nil, @node
  end

  def move
    deploy = Deployment.find_key params[:deployment_id]
    node = Node.find_key params[:node_id]
    node.deployment_id = deploy.id
    node.save!
    node.reload
    render api_show :node, Node, nil, nil, node
  end

  #test_ methods support test functions that are not considered stable APIs
  def test_load_data

    @node = Node.find_key params[:id]
    # get the file
    file = File.join "test", "data", (params[:source] || "node_discovery") + ".json"
    raw = File.read file
    # cleanup
    mac = 6.times.map{ |i| rand(256).to_s(16) }.join(":")
    raw = raw.gsub /00:00:00:00:00:00/, mac
    # update the node
    json = JSON.load raw
    @node.discovery  = json
    @node.save!
    render api_show :node, Node, nil, nil, @node

  end

  private

  def node_action(meth)
    n = Node.find_key(params[:id] || params[:name] || params[:node_id])
    n.send(meth)
    render api_show :node, Node, nil, nil, n
  end

end
