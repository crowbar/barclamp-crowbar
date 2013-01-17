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

class DocsController < ApplicationController
  
  # no login required for docs!
  skip_before_filter :crowbar_auth
      
  def index
    @id = params[:id].gsub("%2B",'+') rescue nil
    @root = Doc.find_by_name(@id || 'root')
    if @root.nil? or Rails.env == 'development' or params.has_key? :rebuild
      Doc.delete_all
      @root = Doc.gen_doc_index File.join '..', 'doc'
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
      @file = Doc.page_path 'doc', @topic.name
      html = !params.has_key?(:source)
      # navigation items
      if File.exist? @file
        @text = (html ? %x[markdown #{@file}] : IO.read(@file))
        @text += Doc.topic_expand(@topic.name, html) if params.has_key? :expand
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
  
end
