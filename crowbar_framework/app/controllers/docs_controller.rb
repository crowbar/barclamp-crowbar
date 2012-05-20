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
  require 'fileutils'
  
  def index
    doc_yml = File.join RAILS_ROOT, 'config', 'docs.yml'
    if File.exist? doc_yml and RAILS_ENV != 'development'
      @index = YAML.load_file File.join('config', 'docs.yml')
    else #create yml
      @index = gen_barclamps({'root'=>{'title'=>t('docs.root')}}, File.join(RAILS_ROOT,'doc','default'), :default)
      xref @index   #find the cross references from and update the index
      File.open( doc_yml, 'w' ) { |out| YAML.dump( @index, out ) }
    end
  end  

  def topic
    all = YAML.load_file File.join('config', 'docs.yml')
    @topic = all[params[:id]]
    @index = {}
    # navigation items
    @next = all[@topic['nexttopic']] if @topic['nexttopic']
    @prev = all[@topic['prevtopic'][0]] if @topic['prevtopic']
    parent = @topic['parent']
    @parents = 0
    while parent and all[parent] do 
      @parents += 1
      @index[parent] = all[parent]
      parent = all[parent]['parent']
    end
    if @topic['children']
      @topic['children'].each do |t|
        @index[t] = all[t]
      end
    end
  end
  
  private 
  
  def gen_doc(meta, path, file)
    key = file[/(.*).md$/,1]
    meta['key']=key
    doc_path = File.join 'public', 'doc', (meta['language']==:default ? 'en' : meta['language']), meta['barclamp']
    html = File.join doc_path, file.gsub('.md','.html')
    begin FileUtils.mkdir_p doc_path rescue true end
    meta['url'] = "doc/#{meta['language']==:default ? 'en' : meta['language']}/#{meta['barclamp']}/#{file.gsub('.md','.html')}"
    File.open(File.join(path, file), 'r').each do |s|
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
    from_type = meta['format'] || 'markdown+lhs'
    %x[pandoc -f #{from_type} -t html -o '#{html}' '#{File.join(path,file)}']
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
          index[v['parent']]['children'] = [] unless index[v['parent']]['children']
          index[v['parent']]['children'] << k
        end
        if v['nexttopic'] and index[v['nexttopic']]
          index[v['nexttopic']]['prevtopic'] = [] unless index[v['nexttopic']]['prevtopic']
          index[v['nexttopic']]['prevtopic'] << k
        end
      end
    end
  end
  
end