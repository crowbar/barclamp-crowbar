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
# See the License for the specific language governnig permissions and
# limitations under the License.
#
class Doc < ActiveRecord::Base
  
  attr_accessible :id, :barclamp_id, :name, :parent_name, :description, :order

  belongs_to  :barclamp
  belongs_to  :parent,    :class_name => "Doc", :foreign_key => "parent_name"
  has_many    :children,  :class_name => "Doc", :foreign_key => "parent_name", :order => "[order]+[description] ASC"

  validates_uniqueness_of :name, :scope=>:barclamp_id, :on => :create, :case_sensitive => false, :message => I18n.t("db.notunique", :default=>"Doc handle must be unique")
  
  # creates the table of contents from the files
  def self.gen_doc_index
    # load crowbar docs    
    Doc.discover_docs 0, File.join('..','doc','framework')
    # load barclamp docs
    Barclamp.all.each { |bc| Doc.discover_docs bc.id, File.join(bc.source_path,'doc') }
    Doc.all
  end 
  
  def self.topic_expand(name, html=true)
    text = "\n"
    topic = Doc.find_by_name name
    if topic.children.size > 0
      topic.children.each do |t|
        file = page_path File.join('..','doc'), t.name
        if File.exist? file
          raw = IO.read(file)
          text += (html ? BlueCloth.new(raw).to_html : raw)
          text += topic_expand(t.name, html)
        end
      end
    end
    return text
  end
  
  # scan the directories and find files
  def self.discover_docs barclamp_id, doc_path
    files_list = %x[find #{doc_path} -iname *.md]
    files = files_list.split "\n"
    files.each do |f|
      # TODO ZEHICLE - figure out order by inspecting name
      #  order = name[/\/([0-9]+)_/,1]
      #  order = (props["order"] || "9999") unless order =~ /^[0-9]+$/
      #    t.order = order.to_s.rjust(6,'0') rescue "!error"
      # TODO ZEHICLE - figure out parent by stripping file
      # figure out description by looking in file
      title = if File.exist? f
          begin
            actual_title = File.open(f, 'r').readline
            actual_title[/^#+(.*)#*$/,1].strip       
          rescue 
            # if that fails, use the name/path
            name.gsub("/"," ").titleize
          end
        else
          name.gsub("/"," ").titleize
        end
      Doc.find_or_create_by_name :name=>f, :barclamp_id=>barclamp_id, :description=>title
    end
  end
  
  def git_url
    path = self.name.split '/'
    barclamp = path.first
    repo = (barclamp.eql?('framework') ? 'crowbar' : "barclamp-#{barclamp}")
    path[0] = "https://github.com/crowbar/#{repo}/tree/master/doc"
    return path.join('/') + ".md"
  end
    
end
