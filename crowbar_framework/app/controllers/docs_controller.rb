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
    
  require 'yaml'
  META = 'topic_meta_data'
  
  def index
    @index = Docs.find_by_handle 'root'
    if @index.nil? or Rails.env == 'development' 
      @index = gen_doc_index docs_path
    end
  end  

  def topic
    begin 
      @topic = Docs.find_by_handle params[:id]      
      file = File.join 'doc', 'default', @topic.handle.gsub('+','/')
      # navigation items
      @text = if File.exist? file
        html_safe %x[markdown #{from}]
      else
        I18n.t '.topic_missing', :scope=>'docs.topic'
      end
    rescue
      @text = I18n.t '.topic_missing', :scope=>'docs.topic'
      flash[:notice] = @text
    end
  end
  
  private 
  
  
  def meta_data(default, parent, topic)
    meta = default.clone
    parent.each { |k, v| meta[k] = v unless k.include? '+' }
    topic.each  { |k, v| meta[k] = v unless k.include? '+' }
  end
  
  def gen_doc_index(path)
    root = Doc.find_or_create {:handle=>'root', :barclamp=>'crowbar', :title=>I18n.t('docs.root'), :author=>'System', :license=>'Apache 2', :date=>'July 20, 2012')
    Dir.entries(path).each do |bc_index|
      # collect all the index files
      if bc_index =~ /(.*).yml$/
        bc = bc_index[/(.*).yml$/,1]
        
        topic = YAML.load_file(File.join(path, bc_index))['root'] rescue continue
        children = topic.delete_if { |k, v| !k.include? '+' }
      
        make_topics path, meta_data, bc, 'root', children
      end
    end
    root
  end

  def make_topics(path, meta_data, barclamp, parent, topics)
    return if topics.nil?
    topics.each do |id, details|
      if id != 'topic_meta_data'
        topic_meta_data = ((details.nil? or details['topic_meta_data'].nil?) ? meta_data : meta_data.merge!(details['topic_meta_data']))
        source = topic_meta_data['source'] || barclamp
        file = File.join path, 'default', source, id+'.md'
        if File.exist? file
          title = File.open(file, 'r').readline rescue id.humanize
          title = title[/(#*)(.*)/,2].strip rescue id.humanize
          # build the new topic
          t = { 'topic_meta_data' => {} }
          t['topic_meta_data']['title'] = title
          t['topic_meta_data']['file'] = file
          order = ("%06d" % topic_meta_data['order'].to_i) rescue "009999"
          t['topic_meta_data']['sort'] = order + title
          topic_meta_data.each { |k, v| t['topic_meta_data'][k] = v } 
          # walk the tree
        else
          t = { 'topic_meta_data'=> {'title'=>"topic pending", 'sort'=>"999999"  }}
        end
        p = parent.split('+')
        case p.length
        # recurse the children
        make_topics path, meta_data, barclamp, "#{parent}+#{id}", details
      end
    end
  end
  
end