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
  
  self.primary_key = "name"  
  attr_accessible :name, :parent_name, :description, :author, :license, :copyright, :date, :order

  belongs_to :parent, :class_name => "Doc", :foreign_key => "parent_name"
  has_many :children, :class_name => "Doc", :foreign_key => "parent_name", :order => "[order]+[description] ASC"

  validates_uniqueness_of :name, :on => :create, :case_sensitive => false, :message => I18n.t("db.notunique", :default=>"Doc handle must be unique")
  
  # creates the table of contents from the files
  def self.gen_doc_index(path)    
    root_default = {:name=>'root', :parent_name=>nil, :description=>I18n.t('root', :scope=>'docs', :default=>"System Documentation (Master Index)"), :author=>I18n.t('unknown'), :license=>I18n.t('unknown'), :order=>'000000', :date=>I18n.t('unknown')}
    root = Doc.find_or_create_by_name root_default
    barclamps = []
    barclamps << Barclamp.new(:name=>'framework')
    barclamps += Barclamp.all.sort
    barclamps.each do |barclamp|
      default = root_default
      bc = barclamp.name
      bc_index = File.join path, bc, "#{bc}.yml"
      # collect all the index files
      if File.exist? bc_index
        begin 
          topic = YAML.load_file bc_index
          default = topic.clone.delete_if{ |k, v| k.to_s.gsub("+","/").include? "/" }
          default = root_default.merge default 
          # explore the YML file for meta data
          topic.each { |t, p| create_doc(path, bc, 'root', t, p, default) }
        rescue 
          Rails.logger.warn I18n.t('docs.parseerror', :path=>bc_index)
        end
      end
      # pickup files just based on directoy search
      discover_docs(path, barclamp, default)
    end
    root
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
  
  # helper builds the path for routines later
  def self.page_path(path, name, ext='.md')
    File.join path, name.gsub("+", "/")+ext
  end

  # find the parent of a doc using the membership associations
  def self.find_parent(barclamp, directory)
    parts = directory.split('/')
    # look for parent
    p = Doc.find_by_name parts[2..1000].join('/')
    # if not found, try the barclamp parent
    if p.nil?
      parents = []
      parents = barclamp.parents if !barclamp.name.eql? 'framework'
      parents << Barclamp.new(:name=>'framework')   # we need to also search the framework
      parents.each do |parent|
        parts[2] = parent.name
        p = Doc.find_by_name parts[2..1000].join('/')
        break if p
      end
    end
    # if we are bootstrapping, then we need to make this swap
    p = Doc.find_by_name 'root' if p.nil? and barclamp.name.eql? 'framework' and parts.length == 3
    return p
  end
  
  # scan the directories and find files that were not in the YML catalogs
  def self.discover_docs(path, barclamp, defaults, tree=nil)
    scan = (tree ? tree : File.join(path, barclamp.name))
    if File.directory? scan
      # first we need to collect files and directories
      files = []
      dirs = []
      Dir.entries(scan).each do |doc_file|
        # we need all the markdown files
        files << doc_file if doc_file =~ /(.*).md$/
        # and all the subdirectories
        dirs << doc_file if !['.', '..'].include?(doc_file) and File.directory?(File.join(scan, doc_file))
      end
      # process all the files in the directory (needs to be before the subdirectories)
      files.each do |doc_file|
        doc = doc_file[/(.*).md$/,1]
        name = "#{scan}/#{doc}"[/#{path}\/(.*)/,1]
        # don't add if we already have it
        d = Doc.find_by_name name
        if d.nil?
          # you must have a parent to add a doc
          parent = find_parent barclamp, scan
          doc_to_db(name, File.join(scan,doc_file), defaults, parent.name) if parent   
        else
          # do nothing because we have an entry
        end
      end
      # recurse all the directories
      dirs.each do |doc_file|
        # recurse into the child directories
        tree = File.join(scan, doc_file)
        discover_docs(path, barclamp, defaults, tree) if File.directory?(tree)
      end
    else
      Rails.logger.warn "Barclamp #{barclamp.name} does not have any doc directory"
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
        Rails.logger.warn "Document '#{barclamp}' catalog '#{name}' assumes a reference '#{parent}' that is not there."
        Doc.find_or_create_by_name(:name=>name, :parent_name=>parent, :order=>'?noref', :description=>I18n.t('.missing_title', :scope=>'docs', :bc=>barclamp))
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
        actual_title[/^#+(.*)#*$/,1].strip       
      rescue 
        # if that fails, use the name/path
        name.gsub("/"," ").titleize
      end
    else
      name.gsub("/"," ").titleize
    end
    order = name[/\/([0-9]+)_/,1]
    order = (props["order"] || "9999") unless order =~ /^[0-9]+$/
    
    t = Doc.find_or_initialize_by_name(name) 
    t.parent_name = parent
    t.order = order.to_s.rjust(6,'0') rescue "!error"
    t.description = title
    t.author = props["author"]
    t.license = props["license"]
    t.copyright = props["copyright"]
    t.date = props["date"]
    t.save!
    Rails.logger.debug "added doc #{name} to system based on file #{file}"
  end
  
  def git_url
    path = self.name.split '/'
    barclamp = path.first
    repo = (barclamp.eql?('framework') ? 'crowbar' : "barclamp-#{barclamp}")
    path[0] = "https://github.com/crowbar/#{repo}/tree/master/doc"
    return path.join('/') + ".md"
  end
    
end
