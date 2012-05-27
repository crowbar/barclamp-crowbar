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
  
  def index
    doc_yml = File.join RAILS_ROOT, 'config', 'docs.yml'
    docs_path = File.join RAILS_ROOT, 'doc'
    if File.exist? doc_yml and RAILS_ENV != 'development'
      @index = YAML.load_file File.join('config', 'docs.yml')
    else #create yml
      @index = gen_doc_index docs_path
      #@index = gen_barclamps({'root'=>{'title'=>t('docs.root')}}, File.join(RAILS_ROOT,'doc','default'), :default)
      #xref @index   #find the cross references from and update the index
      File.open( doc_yml, 'w' ) { |out| YAML.dump( @index, out ) }
    end
  end  

  def topic
    all = YAML.load_file File.join('config', 'docs.yml')
    id = params[:id].split('+')
    case id.length
    when 1
      @parent = nil
      @topic = nil
    when 2 
      @parent = nil
      @topic = all[id[1]]
    when 3 
      @parent = all[id[1]]
      @topic = all[id[1]][id[2]]
    when 4
      @parent = all[id[1]][id[2]]
      @topic = all[id[1]][id[2]][id[3]]
    else
      raise "documentation nested to too many levels, max is 7"
    end
        
    @meta = @topic['topic_meta_data']
    file = @meta['file']
    @index = {}
    # navigation items
    @next = 'foo' #all[@topic['nexttopic']] if @topic['nexttopic']
    @prev =  'foo' #@topic['prevtopic']
    #@prev.each { |p| @index[p] = all[p] } if @prev
    #parent = @topic['parent']
    #@parents = 0
    #while parent and all[parent] do 
    #  @parents += 1
    #  @index[parent] = all[parent]
    #  parent = all[parent]['parent']
    #end
    #if @topic['children']
    #  @topic['children'].split(',').each do |t|
    #    @index[t] = all[t]
    #  end
    #end
    raw = "roo"
    #File.open(@topic['source'], 'r').each do |s|
    #  raw += s
    #end
    #markdown = Redcarpet.new "raw", []
    @text = raw #markdown.to_html
  end
  
  private 
  
  
  def gen_doc_index(path)
    @root = { } if @root.nil?
    root_meta_data = { 'author'=>'Multiple Authors', 'license'=>'Apache 2', 'copyright'=>'2012 by Dell, Inc', 'date'=>I18n.t('unknown'), 'order'=>'alpha', 'source'=>'crowbar', 'url'=>'/', 'format'=>'markdown' }
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
        topic_meta_data = meta_data.merge! details['topic_meta_data'] if details['topic_meta_data']
        source = topic_meta_data['source']
        file = File.join path, 'default', source, id+'.md'
        continue unless File.exist? file
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
        p = parent.split('+')
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
        make_topics path, meta_data, barclamp, "#{parent}+#{id}", details.delete_if{ |k,v| k=='topic_meta_data' }
      end
    end
  end


  def gen_doc(meta, path, file)
    key = file[/(.*).md$/,1]
    meta['key']=key
    doc_path = File.join 'public', 'doc', (meta['language']==:default ? 'en' : meta['language']), meta['barclamp']
    html = File.join doc_path, file.gsub('.md','.html')
    begin FileUtils.mkdir_p doc_path rescue true end
    meta['url'] = "doc/#{meta['language']==:default ? 'en' : meta['language']}/#{meta['barclamp']}/#{file.gsub('.md','.html')}"
    meta['source'] = File.join path, file
    File.open(meta['source'], 'r').each do |s|
      break if s.strip.length==0 
      m = s.split(':')
      if m.length>1 
        meta_key = m[0][1..31].strip.downcase
        case meta_key
        when 'parent', 'nexttopic'
          meta[meta_key] = m[1].strip.downcase
        else
          meta[meta_key] = m[1].strip
        end 
      end
    end
    meta['parent'] = 'root' unless meta['parent']  # we don't want any orphans!
    meta['id'] = key # sometimes we get the object without the key
    meta['format'] = 'markdown' if meta['format'].nil?
    # from_type = meta['format'] || 'markdown+lhs'
    # %x[pandoc -f #{from_type} -t html -o '#{html}' '#{File.join(path,file)}']
    return meta
  end
  
  def gen_barclamps(index, path, language)
    Dir.entries(path).each do |bc|
      index = gen_html(index, File.join(path, bc), language, bc) unless bc.start_with? '.'
    end
    index
  end
  
  def gen_html(index, path, language, barclamp)
    Dir.entries(path).each do |f|
      if f =~ /.md$/
        meta = gen_doc({ 'language'=>language, 'barclamp'=>barclamp }, path, f)
        index["#{barclamp}\/#{meta['key']}"] = meta
      end
    end
    index
  end
  
  def xref(index)
    index.each do |k, v|
      if k and v
        if v['parent'] and index[v['parent']]
          unless index[v['parent']].key? 'children'
            index[v['parent']]['children'] = k
          else
            # now sort the children
            children = index[v['parent']]['children'].split(',')
            children << k
            child_order = {}
            children.each do |child|
              order = begin ("%06d" % index[child]['order'].to_i) rescue "009999"  end
              child_order[child] = "#{order}-#{index[child]['title']}"
            end
            child_order = child_order.sort_by { |c, cd| cd }
            child_list = child_order.map{|c ,cd| c}.join(',')
            index[v['parent']]['children'] = child_list
          end
        end
        if v['nexttopic'] and index[v['nexttopic']]
          index[v['nexttopic']]['prevtopic'] = [] unless index[v['nexttopic']]['prevtopic']
          index[v['nexttopic']]['prevtopic'] << k
        end
      end
    end
    index
  end
  
end