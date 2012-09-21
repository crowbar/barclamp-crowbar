# Copyright 2012, Dell
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
class GroupsController < ApplicationController

  # POST to add a node to the group
  def node_action
    @group = Group.find_key params[:id]
    throw "Could not find Group #{params[:id]}." unless @group

    unless request.get?
      @node = Node.find_key params[:node]
      throw "Could not find Node #{params[:node]}." unless @node
      if request.post?
        @group.nodes << @node
      elsif request.put?
        category = @group.category
        @nodes.groups.each { |g| @nodes.groups.delete(g) if g.category.eql?(category) }
        @nodes.groups << @group
      elsif request.delete?
        @group.nodes.delete(@node)
      end
    end
    render :json => {@group=>@group.nodes.each{ |n| n.id=>n.name }}
    
  end
    
  # PUT to move a node between groups
  def node_move
    if request.put?
      @group = Group.find_key params[:id]
      @node = Node.find_key params[:node]
      if @group and @node
        category = @group.category
        @nodes.groups.each { |g| @nodes.groups.delete(g) if g.category.eql?(category) }
        @nodes.groups << @group
        render :json => @group
      else
        throw "Could not find one or both of Group #{params[:id]} or Node #{params[:node]}."
      end
    else
      throw "HTTP PUT required to compelte this action"
    end
  end  
  
  # GET /group/2.0/1
  # GET /group/2.0/group
  def show
    @group = Group.find_key params[:id]
    respond_to do |format|
      format.html # show.html.erb
      format.json { render :json => @group }
    end
  end

  # RESTful delete of the node
  def destroy
    if request.delete?
      Group.delete Group.find_key(params[:id]).id
      render :text => "Group #{params[:id]} deleted!"
    else
      throw "HTTP DELETE required to compelte this action"
    end
  end
  
  # RESTful create
  def create
    if request.post?
      @group = Group.create! params
      render :json => @group
    else
      throw "HTTP POST required to compelte this action"
    end
  end
  
end
