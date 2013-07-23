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
    unless Rails.env.development?
      render  api_not_supported("post", "node_role")
    else
      r = NodeRole.create! params
      render api_show :node_role, NodeRole, nil, nil, r 
    end
  end

  def update
    unless Rails.env.development?
      render  api_not_supported("delete", "node_role")
    else
      render api_update :node_role, NodeRole
    end
  end

  def destroy
    unless Rails.env.development?
      render  api_not_supported("delete", "node_role")
    else
      render api_delete NodeRole
    end
  end  
  
end

