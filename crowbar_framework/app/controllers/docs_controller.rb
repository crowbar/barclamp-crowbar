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
  
  def gen_doc(meta, path, file)
    key = file[/(.*).md$/,1]
    meta['key']=key
    doc_path = File.join 'public', 'doc', (meta['language']==:default ? 'en' : meta['language']), meta['barclamp']
    html = File.join doc_path, file.gsub('.md','.html')
    puts "ROB 3 #{key}, #{doc_path}, #{html}"
    begin FileUtils.mkdir_p doc_path rescue true end
    %x[pandoc -f markdown+lhs -t html -o '#{html}' '#{File.join(path,file)}']
    meta['url'] = "doc/#{meta['language']==:default ? 'en' : meta['language']}/#{meta['barclamp']}/#{file.gsub('.md','.html')}"
    File.open(File.join(path, file), 'r').each do |s|
      break if s.strip.length==0 
      m = s.split(':')
      if m.length>1 and m[0].start_with? '%'
        meta_key = m[0][1..31].strip.downcase
        case meta_key
        when 'parent', 'nexttopic'
          meta[meta_key] = m[1].strip.downcase
        else
          meta[meta_key] = m[1].strip
        end 
        #puts "\t#{s}"
      end
    end
    return meta
  end
  
  def gen_barclamps(index, path, language)
    puts "ROB 1 #{path}"
    Dir.entries(path).each do |bc|
      index = gen_html(index, File.join(path, bc), language, bc) unless bc.start_with? '.'
    end
    index
  end
  
  def gen_html(index, path, language, barclamp)
    puts "ROB 2 #{path}"
    Dir.entries(path).each do |f|
      if f =~ /.md$/
        meta = gen_doc({ 'language'=>language, 'barclamp'=>barclamp }, path, f)
        index["#{barclamp}\\#{meta['key']}"] = meta
      end
    end
    index
  end
  
  def xref(index)
    index.each do |k, v|
      if k and v
        if v['parent']
          index[v['parent']]['children'] = [] if index[v['parent']]['children']
          index[v['parent']]['children'] << k
        end
        if v['nexttopic']
          index[v['nexttopic']]['prevtopic'] = [] if index[v['nexttopic']]['prevtopic']
          index[v['nexttopic']]['prevtopic'] << k
        end
      end
    end
  end
  
  def index
    @index = gen_barclamps({}, File.join(RAILS_ROOT,'doc','default'), :default)
    #xref(index)
    #File.open( File.join('docs.yml'), 'w' ) do |out|
    #  YAML.dump( @index, out )
    #end
  end  

end