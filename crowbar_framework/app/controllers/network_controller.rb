# Copyright 2011, Dell
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
# Author: RobHirschfeld
#
class NetworkController < ApplicationController

  require 'chef'
  
  def index
    @vports = {}
    @sum = 0
    @groups = {}
    @switches = {}
    @nodes = {}
    raw_nodes = NodeObject.all
    raw_nodes.each do |node|
      @sum = @sum + node.name.hash
      @nodes[node.handle] = { :alias=>node.alias, :description=>node.description(false, true), :status=>node.status }
      #build groups
      group = node.group
      @groups[group] = { :automatic=>!node.display_set?('group'), :status=>{"ready"=>0, "failed"=>0, "unknown"=>0, "unready"=>0, "pending"=>0}, :nodes=>{} } unless @groups.key? group
      @groups[group][:nodes][node.group_order] = node.handle
      @groups[group][:status][node.status] = (@groups[group][:status][node.status] || 0).to_i + 1
      #build switches
      node_nics(node).each do |key, value|
        @switches[key] = { :status=>{"ready"=>0, "failed"=>0, "unknown"=>0, "unready"=>0, "pending"=>0}, :nodes=>{}, :max_port=>24} unless @switches.key? key
        port = if value['switch_port'] == -1 or value['switch_port'] == "-1"
          @vports[key] = 1 + (@vports[key] || 0)
        else
          value['switch_port']
        end
        @switches[key][:max_port] = port if port>@switches[key][:max_port]
        @switches[key][:nodes][port] = node.handle
        @switches[key][:status][node.status] = (@switches[key][:status][node.status] || 0).to_i + 1
      end
    end
    #make sure port max is even
    flash[:notice] = "<b>#{I18n.t :warning, :scope => :error}:</b> #{I18n.t :no_nodes_found, :scope => :error}" if @nodes.empty? #.html_safe if @nodes.empty?
  end

  private 
  
  def node_nics(node)
    switches = {}
    begin
      if_list = node["crowbar_ohai"]["detected"]["network"] 
      if_list.each do |intf, details|
        raw = node["crowbar_ohai"]["switch_config"][intf]
        s_name = raw['switch_name'] || "-1"
        s_unit =  raw['switch_name'] || "-1"
        if s_name == "-1" or s_name = -1
          s_name = I18n.t('virtual') + ":" + intf.split('.')[0]
          s_unit = nil
        end
        s_name+= ":#{s_unit}" unless s_unit.nil?
        switches[s_name] = node["crowbar_ohai"]["switch_config"][intf]
      end
    rescue 
      Rails.logger.debug("could not build interface/switch list for #{node.name}")
    end
    switches
  end
  
end
