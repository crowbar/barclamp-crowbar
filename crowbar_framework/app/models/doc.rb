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
# Author: RobHirschfeld
#
class Doc < ActiveRecord::Base
  
  self.primary_key = "name"  
  attr_accessible :name, :parent_name, :description, :author, :license, :copyright, :date, :order

  belongs_to :parent, :class_name => "Doc", :foreign_key => "parent_name"
  has_many :children, :class_name => "Doc", :foreign_key => "parent_name", :order => "[order]+[description] ASC"

  validates_uniqueness_of :name, :on => :create, :case_sensitive => false, :message => I18n.t("db.notunique", :default=>"Doc handle must be unique")
  
  # creates the table of contents from the files
  def self.gen_doc_index(path)    
    root_default = {:name=>'root', :parent_name=>nil, :description=>I18n.t('root', :scope=>'docs', :default=>"System Documentation (Master Index)"), :author=>I18n.t('unknown'), :license=>I18n.t('unknown'), :order=>'000000', :date=>I18n.t('unknown')}
    root = Doc.find_or_create_by_name root_default
    Barclamp.all.sort.each do |barclamp|
      bc = barclamp.name
      bc_index = File.join path, "#{bc}.yml"
      # collect all the index files
      if File.exist? bc_index
        begin 
          topic = YAML.load_file bc_index
          default = topic.clone.delete_if{ |k, v| k.to_s.gsub("+","/").include? "/" }
          default = root_default.merge default 
          # explore the YML file for meta data
          topic.each { |t, p| create_doc(path, bc, 'root', t, p, default) }
          # pickup files just based on directoy search
          discover_docs(path, barclamp, default)
        rescue 
          flash[:notice] = I18n.t('docs.parseerror', :path=>bc_index)
        end
      end
    end
    root
  end 
  
  def self.topic_expand(name, html=true)
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
  
  # helper builds the path for routines later
  def self.page_path(path, name)
    File.join path, name.gsub("+", "/")+'.md'
  end

  # find the parent of a doc using the membership associations
  def self.find_parent(barclamp, directory)
    parts = directory.split('/')
    # look for parent
    p = Doc.find_by_name parts[2..1000].join('/')
    # if not found, try the barclamp parent
    if p.nil?
      barclamp.parents.each do |parent|
        parts[2] = parent.name
        p = Doc.find_by_name parts[2..1000].join('/')
        break if p
      end
    end
    return p
  end
  
  # scan the directories and find files that were not in the YML catalogs
  def self.discover_docs(path, barclamp, defaults, tree=nil)
    scan = (tree ? tree : File.join(path, barclamp.name))
    Dir.entries(scan).each do |doc_file|
      if doc_file =~ /(.*).md$/
        doc = doc_file[/(.*).md$/,1]
        name = "#{scan}/#{doc}"[/#{path}\/(.*)/,1]
        # don't add if we already have it
        unless Doc.find_by_name name
          # you must have a parent to add a doc
          parent = find_parent barclamp, scan
          doc_to_db(name, File.join(scan,doc_file), defaults, parent.name) if parent 
        else
        end
      elsif ['.', '..'].include? doc_file
        # nothing, it's the currrent dir
      else
        # recurse into the child directories
        tree = File.join(scan, doc_file)
        discover_docs(path, barclamp, defaults, tree) if File.directory?(tree)
      end
    end
  end
  
  # using information from the catalog, add the entries into the database
  def self.create_doc(path, barclamp, parent, name, values, defaults)
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
        doc_to_db name, file, props, parent
      else  #reference only from a different barclamp
        Doc.find_or_create_by_name(:name=>name, :parent_name=>parent, :order=>'?noref', :description=>I18n.t('.missing_title', :scope=>'docs', :bc=>barclamp)) unless name.start_with? '#'
      end
    end
    # recurse children
    children.each { |k, v| create_doc(path, barclamp, name, k, v, props) } unless children.nil?
  end
  
  # helper used by explicit and discovered adds
  def self.doc_to_db(name, file, props, parent)
    # title comes from the first line of the file
    title = if File.exist? file
      begin
        actual_title = File.open(file, 'r').readline
        actual_title[/(#*)(.*)/,2].strip       
      rescue 
        # if that fails, use the name/path
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
  end
  
  
end