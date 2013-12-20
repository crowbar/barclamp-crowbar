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
class AttribsController < ApplicationController

  def index
    @list = if params.has_key? :node_id
              Node.find_key(params[:node_id]).attribs
            elsif params.has_key? :role_id
              Role.find_key(params[:role_id]).attribs
            elsif params.has_key? :node_role_id
              NodeRole.find_key(params[:node_role_id]).attribs
            elsif params.has_key? :deployment_role_id
              DeploymentRole.find_key(params[:deployment_role_id]).attribs
            else
              Attrib.all
            end
    respond_to do |format|
      format.html { }
      format.json { render api_index :attrib, @list }
    end
  end

  def show
    @node = Node.find_key params[:node_id] if params.key? :node_id
    @attrib = Attrib.find_key params[:id]
    respond_to do |format|
      format.html {  }
      format.json { render api_show :attrib, Attrib, nil, nil, @attrib }
    end
  end

  def create
    if params.has_key? :node_id
      render api_not_supported 'post', 'nodes/:node_id/attribs/:id'
    else
      a = Attrib.create! params
      respond_to do |format|
        format.html { }
        format.json { render api_show :attrib, Attrib, nil, nil, a }
      end
    end
  end

  def update
    # based on the type of data being passed, figure out they attribute class (klass), key, role type (rt) and attribue type (atype)
    klass,key,rt,atype = case
                   when params.has_key?(:node_id) then [Node,:node_id,:node, :discovery]
                   when params.has_key?(:deployment_role_id) then [DeploymentRole,:deployment_role_id,:deployment_role, :user]
                   when params.has_key?(:node_role_id) then [NodeRole,:node_role_id,:node_role, :user]
                   when params.has_key?(:role_id) then [Role,:role_id,:role, :user]
                   else [nil,nil,nil]
                   end
    attrib = Attrib.find_key(params[:id])
    if (key == :node_id && !attrib.role_id.nil?) ||
        (key != :node_id && attrib.role_id.nil?) ||
        key.nil?
      render api_not_supported 'put', 'attribs/:id'
    end
    target = klass.find_key(params[key])
    attrib.set(target,params[:value], atype)
    target.save!
    render api_show rt, klass, nil, nil, target
  end

  def destroy
    respond_to do |format|
      format.html { }
      format.json { render api_delete Attrib }
    end
  end

end
