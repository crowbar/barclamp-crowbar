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
class DashboardController < ApplicationController

  def index  
    @sum = Node.name_hash
    @groups = Group.find_all_by_category 'ui'
    @node = Node.find_key params[:id]
    session[:node] = params[:id]
    if params.has_key?(:role)
      result = Node.all 
      @nodes = result.find_all { |node| node.role? params[:role] }
      if params.has_key?(:names_only)
         names = @nodes.map { |node| node.name }
         @nodes = {:role=>params[:role], :nodes=>names, :count=>names.count}
      end
    else
      @nodes = {}
      Node.all.each { |n| @nodes[n.id]=n.name }
    end
    respond_to do |format|
      format.html # index.html.haml
    end
  end

  def group_change
    # TODO: not used?
    node = Node.find_by_name params[:id]
    if node.nil?
      raise "Node #{params[:id]} not found.  Cannot change group" 
    else
      group = params[:group]
      if params.key? 'automatic'
        node.group=""
      else
        node.group=group
      end
      node.save
      Rails.logger.info "node #{node.name} (#{node.alias}) changed its group to be #{node.group.empty? ? 'automatic' : group}."
      render :inline => "SUCCESS: added #{node.name} to #{group}.", :cache => false 
    end
  end
  
  # Bulk Edit
  def list
    if request.post?
      nodes = {}
      params.each do |k, v|
        if k.starts_with? "node:"
          parts = k.split ':'
          node = parts[1]
          area = parts[2]
          nodes[node] = {} if nodes[node].nil?
          nodes[node][area] = (v.empty? ? nil : v)
        end
      end
      succeeded = []
      failed = []
      # before we start saving, make sure someone did not give us duplicate aliases
      # this SHOULD Be causght by the node.save but race conditoins are breaking the constency of the DB
      alias_dup = false
      nodes.each do |node_name, values|
        nodes.each do |nn, vv|
           alias_dup = true if nn!=node_name and vv['alias'] == values['alias']
           failed << node_name if alias_dup 
           break if alias_dup
        end
      end
      unless alias_dup
        nodes.each do |node_name, values|
          begin
            dirty = false
            # TODO: can one DE-allocate a node in bluk-edit?  If so, we need to add that here...
            node = Node.find_by_name node_name
            if !node.allocated and values['allocate'] === 'checked'
              node.allocated = true
              dirty = true
            end
            if !(node.description == values['description'])
              node.description = values['description']
              dirty = true
            end
            if !(node.alias == values['alias'])
              node.alias = values['alias']
              dirty = true
            end
            if !(node.group.name == values['group'])
              node.group = values['group']
              dirty = true
            end
            if !values['bios'].nil? and values['bios'].length>0 and !(node.bios_set === values['bios']) and !(values['bios'] === 'not_set')
              node.bios_set = values['bios']
              dirty = true
            end
            if !values['raid'].nil? and values['raid'].length>0 and !(node.raid_set === values['raid']) and !(values['raid'] === 'not_set')
              node.raid_set = values['raid']
              dirty = true
            end
            if dirty
              begin
                node.save!
                succeeded << node_name
              rescue Exception=>e
                failed << node_name
              end
            end
          rescue Exception=>e
            failed << node_name
          end
        end
      end
      if failed.length>0
        flash[:notice] = failed.join(',') + ": " + I18n.t('failed', :scope=>'nodes.list')
      elsif succeeded.length>0
        flash[:notice] = succeeded.join(',') + ": " + I18n.t('updated', :scope=>'nodes.list')
      else
        flash[:notice] = I18n.t('nochange', :scope=>'nodes.list')
      end
    end
    @nodes = {}
    Node.all.each do |node|
      @nodes[node.name] = node if params[:allocated].nil? or !node.allocated?
    end
  end

  def families
    @families = {}
    Node.all.each do |n|
      f = n.family.to_s  
      @families[f] = {:names=>[], :family=>n.family} unless @families.has_key? f
      @families[f][:names] << {:alias=>n.alias, :description=>n.description, :handle=>n.name}
    end
  end

end
