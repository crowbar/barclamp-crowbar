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
# Author: RobHirschfeld 
# 

class DocsController < ApplicationController
      
  def index
    @root = Doc.find_by_name 'root'
    if @root.nil? or Rails.env == 'development'
      @root = gen_doc_index File.join('doc')
    end
  end  
  
  def topic
    begin 
      @topic = Doc.find_by_name params[:id]      
      file = page_path 'doc', @topic.name
      # navigation items
      @text = if File.exist? file
        %x[markdown #{file}]
      else
        I18n.t('.topic_missing', :scope=>'docs.topic') + ": " + file
      end
    rescue
      @text = I18n.t('.topic_missing', :scope=>'docs.topic')  + ": " + file
      flash[:notice] = @text
    end
  end
  
  private
  
  def page_path(path, name, language='default')
    File.join path, language, name.gsub("+", "/")+'.md'
  end
  
  def gen_doc_index(path)    
    root = Doc.find_or_create_by_name(:name=>'root', :parent_name=>nil, :description=>I18n.t('.root', :scope=>'docs'), :author=>'System', :license=>'Apache 2', :order=>'000000', :date=>'July 20, 2012')
    Dir.entries(path).each do |bc_index|
      # collect all the index files
      if bc_index =~ /(.*).yml$/
        bc = bc_index[/(.*).yml$/,1]
        topic = YAML.load_file File.join(path, bc_index)
        topic.each { |t, v| create_doc(path, 'root', t, v) }
      end
    end
    root
  end 

  def create_doc(path, parent, name, values)
    if name.to_s.include? "+"
      values ||= {} 
      values[:name] = name
      values[:parent_name] = parent
      title = name.gsub("+"," ").titleize
      file = page_path path, 'default', name
      values[:description] = if File.exist? file
        begin
          actual_title = File.open(file, 'r').readline
          actual_title[/(#*)(.*)/,2].strip       
        rescue 
          title
        end
      else 
        title
      end
      values["order"] = (values["order"].to_s || '999').rjust(6,'0')
      Doc.find_or_create_by_name(values)
      values.each do |k, v|
        create_doc path, name, k, v if k.to_s.include? "+"
      end
    end
  end
end
