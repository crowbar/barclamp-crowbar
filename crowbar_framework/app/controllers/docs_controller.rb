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
    doc_yml = File.join RAILS_ROOT, 'config', 'docs.yml'
    docs_path = File.join RAILS_ROOT, 'doc'
    if File.exist? doc_yml and RAILS_ENV != 'development'
      @index = YAML.load_file File.join('config', 'docs.yml')
    else #create yml
      @index = gen_doc_index docs_path
      File.open( doc_yml, 'w' ) { |out| YAML.dump( @index, out ) }
    end
  end  

  def topic
    begin 
      all = YAML.load_file File.join('config', 'docs.yml')
      @path = params[:id]
      id = @path.split('+')
      case id.length
      when 1
        @parent = nil
        @parent_link = nil
        @topic = nil
      when 2 
        @parent = nil
        @parent_link = nil
        @topic = all[id[1]]
      when 3 
        @parent = all[id[1]]
        @parent_link = id[0..1].join('+')
        @topic = all[id[1]][id[2]]
      when 4
        @parent_link = id[0..2].join('+')
        @parent = all[id[1]][id[2]]
        @topic = all[id[1]][id[2]][id[3]]
      when 5
        @parent_link = id[0..3].join('+')
        @parent = all[id[1]][id[2]][id[3]]
        @topic = all[id[1]][id[2]][id[3]][id[4]]
      when 6
        @parent_link = id[0..4].join('+')
        @parent = all[id[1]][id[2]][id[3]][id[4]]
        @topic = all[id[1]][id[2]][id[3]][id[4]][id[5]]
      else
        raise "documentation nested to too many levels, max is 6"
      end
          
      @meta = @topic['topic_meta_data']
      file = @meta['file']
      @index = {}
      # navigation items
      @next = 'foo' #all[@topic['nexttopic']] if @topic['nexttopic']
      @prev =  'foo' #@topic['prevtopic']
      @children = @topic.delete_if { |k, v| k == META }
      from = @meta['file']  
      raw = if File.exist? file
        %x[markdown #{from}]
      else
        I18n.t '.topic_missing', :scope=>'docs.topic'
      end
      #File.open(@topic['source'], 'r').each do |s|
      #  raw += s
      #end
      #markdown = Redcarpet.new "raw", []
      @text = raw #markdown.to_html
    rescue
      @text = I18n.t '.topic_missing', :scope=>'docs.topic'
      flash[:notice] = @text
    end
  end
  
  private 
  
  
  def gen_doc_index(path)
    @root = { } if @root.nil?
    root_meta_data = { 'author'=>'Multiple Authors', 'license'=>'Apache 2', 'copyright'=>'2012 by Dell, Inc', 'date'=>I18n.t('unknown'), 'order'=>'alpha', 'url'=>'/', 'format'=>'markdown' }
    Dir.entries(path).each do |bc_index|
      # collect all the index files
      if bc_index =~ /(.*).yml$/
        bc = bc_index[/(.*).yml$/,1]
        topic = YAML.load_file(File.join(path, bc_index))['root'] rescue continue
        meta_data = root_meta_data.merge! topic['topic_meta_data']
        children = topic.delete_if { |k, v| k=='topic_meta_data' }
        make_topics path, meta_data, bc, 'root', children
      end
    end
    @root
  end

  def make_topics(path, meta_data, barclamp, parent, topics)
    return if topics.nil?
    topics.each do |id, details|
      if id != 'topic_meta_data'
        topic_meta_data = ((details.nil? or details['topic_meta_data'].nil?) ? meta_data : meta_data.merge!(details['topic_meta_data']))
        source = topic_meta_data['source'] || barclamp
        file = File.join path, 'default', source, id+'.md'
puts "ROB2 #{File.exist? file} #{source} #{id} #{file}"
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
puts "ROB #{barclamp} #{p} #{id} #{t}"
        case p.length
        when 1
          @root[id] = t
        when 2 
          @root[p[1]][id] = t 
        when 3 
          @root[p[1]][p[2]][id] = t 
        when 4
          @root[p[1]][p[2]][p[3]][id] = t
        when 5 
          @root[p[1]][p[2]][p[3]][p[4]][id] = t
        when 6
          @root[p[1]][p[2]][p[3]][p[4]][p[5]][id] = t
        when 7
          @root[p[1]][p[2]][p[3]][p[4]][p[5]][p[6]][id] = t
        else
          raise "documentation nested to too many levels, max is 7"
        end
        # recurse the children
        make_topics path, meta_data, barclamp, "#{parent}+#{id}", details
      end
    end
  end
  
end