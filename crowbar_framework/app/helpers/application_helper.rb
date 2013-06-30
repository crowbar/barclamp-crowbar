# Copy21, Dell
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
# Methods added to this helper will be available to all templates in the application.
module ApplicationHelper
  
  def list_from_hashes(hashes, key, options={:del => ". "})
    a_list = ""
    return a_list if (hashes.nil? or key.nil?)
    a_list
  end

  def dl_item(term, definition, options={})
    unless definition.blank? && options[:show_if_blank] != true
      # need to add non-breaking space entity if we are showing blanks otherwise layout
      # is broken when rendered.
      if definition.blank?
        definition = "&nbsp;" 
        options[:escape_html] = false
      end
      html  = "<dt>#{options[:escape_html] != false ? (h term) : (term)}</dt>"
      dd = "<dd" + (options[:class].nil? ? "" : " class='"+options[:class]+"'") + (options[:title].nil? ? "" : " title='" + options[:title]+"'") + ">"
      html += "#{dd}#{options[:escape_html] != false ? (h definition) : (definition)}</dd>"
      raw html
    end
  end

  def cb_column_class(current_column, total)
    if (current_column % total) == 0
      "first"
    elsif (current_column % total) == (total-1)
      "last"
    end
  end
  
  def format_memory(kB)
    mem = (kB.to_f / 1024 / 1024)
    "#{sprintf("%#1.2f", mem)} GB"
  end
  
  def hash_to_ul(hash)
      result = "<ul>"
      hash.each do |key,value|
          result << "<li>"
          if key.is_a?(Hash)
              result << hash_to_ul(key)
          else
              result << "<em>#{key}</em>"
          end
          if value.is_a?(Hash)
              result << hash_to_ul(value)
          else
              result << ( value == nil ? "" : ": #{value}" )
          end
          result << "</li>"
      end
      result << "</ul>"
  end

  def render_attributes(raw, proposal)
    # POSSIBLE OBSOLETE IN 2.X
    if raw
      render :partial => 'barclamp/edit_attributes_raw'
    else
      begin
        render :partial => "barclamp/#{proposal.barclamp}/edit_attributes"
      rescue ActionView::MissingTemplate
        render :partial => 'barclamp/edit_attributes_raw'
      rescue Exception => e
        puts "Attribute Exception #{e.class}: #{e.message}"
        puts e.backtrace
        render :partial => 'barclamp/edit_attributes_raw'
      end
    end
  end

  def render_deployment(raw, proposal)
    # POSSIBLE OBSOLETE IN 2.X
    if raw
      render :partial => 'barclamp/edit_deployment_raw'
    else
      unless Rails.env == 'development'
        begin
          render :partial => "barclamp/#{proposal.barclamp}/edit_deployment"
        rescue ActionView::MissingTemplate
          render :partial => 'barclamp/edit_deployment_raw'
        rescue Exception => e
          puts "Deployment Exception #{e.message}"
          puts e.backtrace
          render :partial => 'barclamp/edit_deployment_raw'
        end
      else
        render :partial => "barclamp/#{proposal.barclamp}/edit_deployment"
      end
    end
  end

  def nodes_hash(group=nil)
    # POSSIBLE OBSOLETE IN 2.X
    nodes = {}
    NodeObject.all.each do |node|      
      nodes[node.name] = {:handle=>node.handle, :alias=>node.alias, :title=>node.description(false, true), :admin=>node.admin?, :group=>node.group} if node.group==group or group.nil? 
    end
    nodes
  end
  
 def build_scaffold_nav(subnav,item,level=0,sub_num=0)
    ul_class="nav_#{level}-#{sub_num}"
    subnav.dom_class = ul_class
    # puts "build_scaffold_nav  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! item.name: #{item.name}"
    # puts "build_scaffold_nav  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! subnav.dom_class: #{subnav.dom_class}"
    tmp_sub_num = 0
    level += 1
    item.children.each do |nav|
     # puts "children: #{nav.children}"
      #puts "nav.children.is_a? Nav #{nav.children.is_a? Nav}"
     # puts "children empty? #{nav.children.empty?}"
      has_children = (nav.children.is_a? Nav or !nav.children.empty?)
    #  puts "has_children #{has_children}"
      options = {:title=>t(nav.description, :default=>t(nav.name))}
      options[:class] = "has_children" unless !has_children
      subnav.item nav.item.to_sym, "[#{t(nav.name)}]", eval(nav.path), options do |nextnav|
        if (has_children)
        # puts "build_scaffold_nav  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! nav.name: #{nav.name}"
          tmp_sub_num += 1
          self.build_scaffold_nav(nextnav,nav,level,tmp_sub_num)
        end
      end
    end
  end
  
end
