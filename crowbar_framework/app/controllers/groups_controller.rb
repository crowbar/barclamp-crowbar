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
    unless @group
      render :json => {:error=>I18n.t('api.not_found', :type=>'Group', :id=>params[:id])}, :status => 404
      return
    else
      unless request.get?
        @node = Node.find_key params[:node]
        unless @node
          render :json => {:error=>I18n.t('api.not_found', :type=>'Node', :id=>params[:node])}, :status => 404
          return
        else
          if request.post?
            @group.nodes << @node unless @group.nodes.include? @node
          elsif request.put?
            category = @group.category
            @node.groups.each { |g| g.nodes.delete(@node) if g.category.eql?(category) }
            @node.groups << @group unless @group.nodes.include? @node
          elsif request.delete?
            @group.nodes.delete(@node) if @group.nodes.include? @node
          end
        end
      end
      result = {}
      @group.nodes.each { |n| result[n.id] = n.name }
      render :json => {:id=>@group.id, :name=>@group.name, :category=>@group.category, :nodes=>result}
    end
  end

  def index
    render api_index :group, Group.all
  end

  def show
    render api_show :group, Group
  end

  def create
    g = Group.create params
    render api_show :group, Group, nil, nil, g
  end
  
  def destroy
    render api_delete Group
  end
  
end
