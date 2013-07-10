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
    if params.has_key? :node_id
      n = Node.find_key params[:node_id]
      render api_index :attrib, n.attribs
    else
      render api_index :attrib, Attrib.all
    end
  end

  def show
    if params.has_key? :node_id
      redirect_to nodes_path(:id=>params[:node_id]), :method=>:get
    else
      render api_show :attrib, Attrib
    end
  end
  
  def create
    if params.has_key? :node_id
      render api_not_supported 'put', 'nodes/:id/attribs/:id'
    else
      a = Attrib.create! params
      render api_show :attrib, Attrib, nil, nil, a
    end
  end
  
  def update
    if params.has_key? :node_id
      # TODO this needs to be restricted to the node only
      key = params[:id]
      # this path, we know the ID
      if key =~ /^[0-9]+$/
        # TODO we should check the node ID
        a = Attrib.find params[:id]
        a.actual = params[:value]
        a.save
        redirect_to attribs_path(:id=>a.id)
      else
        # we have an Attrib TYPE and we'll do the right thing
        n = Node.find_key params[:node_id]
        a = n.set_attrib(params[:id], params[:value])
        redirect_to attribs_path(:id=>a.id)
      end
    else  
      render api_update :attrib, Attrib
    end
  end

  def destroy
    render api_delete Attrib
  end
      
end
