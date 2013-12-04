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
    @list = Attrib.all
    @node = Node.find_key params[:node_id] if params.has_key? :node_id
    respond_to do |format|
      format.html { }
      format.json { render api_index :attrib, @list }
    end
  end

  def show
    @node = Node.find_key params[:node_id] if params.has_key? :node_id
    @attrib = Attrib.find_key params[:id]
    respond_to do |format|
      format.html {  }
      format.json { render api_show :attrib, Attrib, nil, nil, @attrib }
    end
  end
  
  def create
    if params.has_key? :node_id
      render api_not_supported 'post', 'nodes/:id/attribs/:id'
    else
      a = Attrib.create! params
      respond_to do |format|
        format.html { }
        format.json { render api_show :attrib, Attrib, nil, nil, a }
      end
    end
  end
  
  def update
    if params.key? :node_id and params.key? :value
      node = Node.find_key params[:node_id]
      attrib = Attrib.find_key(params[:id])
      attrib.set(node,params[:value])
      node.reload
      render api_show :node, Node, nil, nil, node
    else
      respond_to do |format|
        format.html { render api_not_supported 'put', 'nodes/:id/attribs/:id' }
        format.json { render api_update :attrib, Attrib }
      end
    end
  end

  def destroy
    respond_to do |format|
      format.html { }
      format.json { render api_delete Attrib }
    end
  end

end
