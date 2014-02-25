# -*- encoding : utf-8 -*-
#
# Copyright 2011-2013, Dell
# Copyright 2013-2014, SUSE LINUX Products GmbH
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

class DashboardController < ApplicationController
  def index
    @sum = 0
    @groups = {}
    session[:node] = params[:name]
    if params.has_key?(:role)
      result = NodeObject.all #this is not efficient, please update w/ a search!
      @nodes = result.find_all { |node| node.role? params[:role] }
      if params.has_key?(:names_only)
         names = @nodes.map { |node| node.handle }
         @nodes = {:role=>params[:role], :nodes=>names, :count=>names.count}
      end
    else
      @nodes = {}
      raw_nodes = NodeObject.all
      get_node_and_network(params[:selected]) if params[:selected]
      raw_nodes.each do |node|
        @sum = @sum + node.name.hash
        @nodes[node.handle] = { :alias=>node.alias, :description=>node.description, :status=>node.status, :state=>node.state }
        group = node.group
        @groups[group] = { :automatic=>!node.display_set?('group'), :status=>{"ready"=>0, "failed"=>0, "unknown"=>0, "unready"=>0, "pending"=>0}, :nodes=>{} } unless @groups.key? group
        @groups[group][:nodes][node.group_order] = node.handle
        @groups[group][:status][node.status] = (@groups[group][:status][node.status] || 0).to_i + 1
        if node.handle === params[:name]
          @node = node
          get_node_and_network(node.handle)
        end
      end
      flash[:notice] = "<b>#{t :warning, :scope => :error}:</b> #{t :no_nodes_found, :scope => :error}" if @nodes.empty? #.html_safe if @nodes.empty?
    end

    respond_to do |format|
      format.html
      format.xml { render :xml => @nodes }
      format.json { render :json => @nodes }
    end
  end

  def show

  end
end
