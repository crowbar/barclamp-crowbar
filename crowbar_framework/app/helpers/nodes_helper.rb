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
module NodesHelper
  
  def roles_list(roles)
    return [] if roles.nil? or roles.empty?
    roles.delete_if { |role| role =~ /^.*-config-/ }
    (roles.map {|role| "<a href='#{nodes_path({:role=>role, :names_only=>true, :format=>'json'})}'>#{role}</a>"} * ', ').html_safe
  end

  def barclamps_list(roles)
    return [] if roles.nil? or roles.empty?
    roles.delete_if { |role| !(role =~ /^.*-config-/) }
    (roles.map {|role| "<a href='#{nodes_path({:role=>role, :names_only=>true, :format=>'json'})}'>#{role.gsub("-config-", " ").titlecase}</a>"} * ', ').html_safe
  end

  def ip_addresses(ip_list)
    html = ""
    ip_list.each_pair do |network, addresses|
      unless network=='~notconnected' && addresses.nil?
        if network == '[not managed]'
          html += "<li><b>#{network}:</b> #{addresses.join(',')}</li>"
        else
          html += "<li><b>#{network}:</b> #{addresses.keys.collect {|k| "#{k}: #{addresses[k]}"}.join(',')}</li>"
        end
      end
    end
    html
  end
  
    
  #this routine markes the current item w/ [] and also adds it if it is missing
  def option_default(options, item, scope = '')
    h = options.clone  #prevent changing the original list when we select an item
    if h.nil?
      h = { t(item, :scope => scope ) => item } 
    else 
      i = h.find{ |k, v| v == item }
      if i.nil?
        if item == ChefObject::NOT_SET or item.nil?
           h["[#{t(ChefObject::NOT_SET, :scope => scope)}]"] = item || ChefObject::NOT_SET
        else
          h["[#{item.humanize}]"] = item
        end
      else
        h.delete i[0]
        h["[#{i[0]}]"] = i[1] unless i[0].start_with?('[')
      end
    end
    return h
  end

end
