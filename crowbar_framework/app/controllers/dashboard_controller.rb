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

  def layercake

    taxmap = JSON::load File.open(File.join("config", "layercake.json"), 'r')
    @layers = { :count=>-1, :unclassified=>0, :os=>0, :hardware=>0, :test=>0, :network=>0, :crowbar_base=>0, :monitoring=>0, :performance=>0, :metering=>0, :database=>0, :nova=>0, :swift=>0, :glance=>0, :nova_api=>0, :swift_api=>0, :glance_api=>0, :identity_api=>0, :portal_api=>0, :api_ips=>{}, :api_names=>{} }
    result = Deployment.system_root.first.head.node_roles
    result.each do |node|
      node.run_list.each do |role|
        if taxmap.has_key? role.name
          taxmap[role.name].each do |layer|
            @layers[layer.to_sym] += 1
            if layer =~ /(.*)_api$/ 
              @layers[:api_ips][layer.to_sym] = node.public_ip
              @layers[:api_names][layer.to_sym] = node.alias
            end
          end
        else
          @layers[:unclassified] += 1
        end
      end unless node.run_list.nil?
    end
    respond_to do |format|
      format.html { }
    end
    #respond_with @layers
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
