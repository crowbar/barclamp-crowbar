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
    if @root.nil? or Rails.env.development? or params.has_key? :rebuild
      # for dev, we want to be able to turn off rebuilds
      unless params[:rebuild].eql? "false"
        Doc.delete_all
        @root = Doc.gen_doc_index File.join '..', 'doc'
        @root = Doc.find_by_name(@id) if @id
      end
    end
    respond_to do |format|
      format.html # index.html.haml
      format.json { render :json => Doc.all }
    end
  end  
  
  def topic
    begin 
      @topic = Doc.find_by_name params[:id]
      @topic = Doc.find_by_name params[:id].gsub("/","+") unless @topic
      if @topic
        @file = Doc.page_path File.join('..','doc'), @topic.name
      else
        @file = File.join '..','doc', params[:id]
      end
      html = !params.has_key?(:source)
      image = false
      # navigation items
      if File.exist? @file
        if @file =~ /\.md$/
          raw = IO.read(@file)
          @text = (html ? BlueCloth.new(raw).to_html : raw)
          @text += Doc.topic_expand(@topic.name, html) if params.has_key? :expand
        elsif @file =~ /\.(jpg|png)$/
          html = false
          image = true
        end
      else
        @text = I18n.t('.topic_missing', :scope=>'docs.topic') + ": " + @file
      end
    rescue
      @text = I18n.t('.topic_error', :scope=>'docs.topic')  + ": " + @file
      flash[:notice] = @text
    end
    if image
       render :text=>open(@file, "rb").read, :content_type => :image, :content_disposition => "inline"
    elsif params.has_key? :expand
      if html
         render :layout => 'doc_export'
      else
         render :text=>@text, :content_type => :text
      end
    end
  end
  
end
