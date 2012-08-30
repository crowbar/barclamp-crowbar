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

class DocsController < ApplicationController
      
  def index
    @id = params[:id].gsub("%2B",'+') rescue nil
    @root = Doc.find_by_name(@id || 'root')
    if @root.nil? or Rails.env == 'development' or params.has_key? :rebuild
      Doc.delete_all
      @root = gen_doc_index 'doc'
      @root = Doc.find_by_name(@id) if @id
    end
    respond_to do |format|
      format.html # index.html.haml
      format.xml  { render :xml => @root.children }
      format.json { render :json => @root.children }
    end
  end  
  
  def topic
    begin 
      @topic = Doc.find_by_name params[:id]
      @topic = Doc.find_by_name params[:id].gsub("/","+") unless @topic
      @file = page_path 'doc', @topic.name
      html = !params.has_key?(:source)
      # navigation items
      if File.exist? @file
        @text = (html ? %x[markdown #{@file}] : IO.read(@file))
        @text += topic_expand(@topic.name, html) if params.has_key? :expand
      else
        @text = I18n.t('.topic_missing', :scope=>'docs.topic') + ": " + @file
      end
    rescue
      @text = I18n.t('.topic_error', :scope=>'docs.topic')  + ": " + @file
      flash[:notice] = @text
    end
    if params.has_key? :expand
      if html
         render :layout => 'doc_export'
      else
         render :text=>@text, :content_type => :text 
      end
    end
  end
  
  private
  
  def topic_expand(name, html=true)
    text = "\n"
    topic = Doc.find_by_name name
    if topic.children.size > 0
      topic.children.each do |t|
        file = page_path 'doc', t.name
        if File.exist? file
          text += (html ? %x[markdown #{file}] : IO.read(file))
          text += topic_expand(t.name, html)
        end
      end
    end
    return text
  end
  
  def page_path(path, name, language='default')
    File.join path, language, name.gsub("+", "/")+'.md'
  end
  
  def gen_doc_index(path)    
    root_default = {:name=>'root', :parent_name=>nil, :description=>I18n.t('.root', :scope=>'docs'), :author=>I18n.t('unknown'), :license=>I18n.t('unknown'), :order=>'000000', :date=>I18n.t('unknown')}
    root = Doc.find_or_create_by_name root_default
    Dir.entries(path).each do |bc_index|
      # collect all the index files
      if bc_index =~ /(.*).yml$/
        bc = bc_index[/(.*).yml$/,1]
        begin 
          topic = YAML.load_file File.join(path, bc_index)
          default = topic.clone.delete_if{ |k, v| k.to_s.gsub("+","/").include? "/" }
          default = root_default.merge default 
          topic.each { |t, p| create_doc(path, bc, 'root', t, p, default) }
        rescue 
          flash[:notice] = I18n.t('docs.parseerror', :path=>bc_index)
        end
      end
    end
    root
  end 

  def create_doc(path, barclamp, parent, name, values, defaults)
    children = {}
    props = defaults.clone
    name = name.gsub("+","/")
    if name.to_s.include? "/"
      name_bc = name.split('/')[0]
      # guards badly formed yml
      if values.is_a?(String)
        children[values] = {}
      elsif !values.nil? 
        # split attributes from children and merge in defaults
        values.each do |k, v|
          if k.to_s.include? "/"
            children[k] = v
          else
            props[k] = v
          end
        end
      end
      if name_bc.eql? barclamp #topic is sourced from this barclamp
        file = page_path path, name
        title = if File.exist? file
          begin
            actual_title = File.open(file, 'r').readline
            actual_title[/(#*)(.*)/,2].strip       
          rescue 
            name.gsub("/"," ").titleize
          end
        else
          name.gsub("/"," ").titleize
        end
        t = Doc.find_or_initialize_by_name(name) 
        t.parent_name = parent
        t.order = (props["order"] || "9999").to_s.rjust(6,'0') rescue "!error"
        t.description = title
        t.author = props["author"]
        t.license = props["license"]
        t.copyright = props["copyright"]
        t.date = props["date"]
        t.save!
      else  #refernce only from a different barclamp
        Doc.find_or_create_by_name(:name=>name, :parent_name=>parent, :order=>'?noref', :description=>I18n.t('.missing_title', :scope=>'docs', :bc=>barclamp))
      end
    end
    # recurse children
    children.each { |k, v| create_doc(path, barclamp, name, k, v, props) } unless children.nil?
  end
end
