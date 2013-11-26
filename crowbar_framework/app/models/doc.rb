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

  attr_accessible :id, :name, :description, :order

  belongs_to :barclamp
  belongs_to :parent, :class_name => "Doc"
  has_many :children, :class_name => "Doc", :foreign_key => "parent_id"

  validates_uniqueness_of :name, :scope=>:barclamp_id, :on => :create, :case_sensitive => false, :message => I18n.t("db.notunique", :default=>"Doc handle must be unique")

  scope :roots, where(:parent_id=>nil)
  scope :roots_by_barclamp, lambda { |barclamp_id| where(:parent_id=>nil, :barclamp_id=>barclamp_id) }

  def <=>(b)
    x = order <=> b.order if order and b.order
    x = description <=> b.description if x == 0
    return x
  end

  def self.root_directory
    File.join('../doc')
  end

  def self.doc_path(x)
    File.join '/docs', x
  end

  # creates the table of contents from the files
  def self.gen_doc_index
    roots=[]
    found={}
    # load crowbar docs
    Doc.discover_docs nil, 'framework', roots, found
    # load barclamp docs
    Barclamp.all.each { |bc| Doc.discover_docs bc, bc.name, roots, found }
    Doc.all
  end

  def self.topic_expand(name, html=true)
    text = "\n"
    topic = Doc.find_by_name name
    if topic.children.size > 0
      topic.children.each do |t|
        file = page_path root_directory, t.name
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
  def self.discover_docs barclamp, doc_path, roots, found
    files_list = `cd #{root_directory} && find #{doc_path} -iname *.md`
    files = files_list.split "\n"
    files = files.sort_by {|x| x.length} # to ensure that parents come before their children
    files.each do |name|
      f = File.join(root_directory, name)

      # figure out order by inspecting name
      order = name[/\/([0-9]+)_[^\/]*$/,1]
      order = "9999" unless order
      #order = (props["order"] || "9999") unless order
      order = order.to_s.rjust(6,'0') rescue "!error"

      # figure out description by looking in file
      title = if File.exist? f
                begin
                  actual_title = File.open(f, 'r').readline
                  # we require titles to star w/ # - anything else is considered extra content
                  next unless actual_title.starts_with? "#"
                  actual_title.strip[/^#+(.*?)#*$/,1].strip
                rescue
                  # if that fails, use the name/path
                  name.gsub("/"," ").titleize
                end
              else
                name.gsub("/"," ").titleize
              end

      # figure out parent by stripping file
      m = name.match(/^(.*)\/[^\/]*$/)
      parent_name = m[1] + ".md" if m
      parent = found[parent_name] if parent_name
      parent = Doc.find_by_name parent_name if parent_name and not parent

      x = Doc.find_or_create_by_name :name=>name, :description=>title.truncate(120), :order=>order
      x.barclamp = barclamp
      if parent.nil?
        roots << x
      else
        x.parent = parent
      end
      found[name]=x
      x.save
    end
  end

  def git_url
    path = self.name.split '/'
    barclamp = path.first
    repo = (barclamp.eql?('framework') ? 'crowbar' : "barclamp-#{barclamp}")
    path[0] = "https://github.com/crowbar/#{repo}/tree/master/doc"
    return path.join('/')
  end

end
