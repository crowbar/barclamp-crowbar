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
class Cmdb < ActiveRecord::Base

  attr_accessible :name, :description, :type, :order

  # 
  # Validate the name should unique 
  # and that it starts with an alph and only contains alpha,digist,hyphen,underscore
  #
  validates_uniqueness_of :name, :case_sensitive => false, :message => I18n.t("db.notunique", :default=>"Name item must be unique")
  validates_format_of :name, :with=> /^[a-zA-Z][_a-zA-Z0-9]*$/, :message => I18n.t("db.lettersnumbers", :default=>"Name limited to [_a-zA-Z0-9]")
  
  has_many :maps, :class_name => "cmdb_maps", :foreign_key => "cmdb_id"
  #TEMPORARY REMOVAL... has_many :cmdb_events, :inverse_of => Cmdb

  def init
    puts "RAH REMOVE: super class initialize"
  end
  
  # I'm totally not understanding the proposal configs/proposals
  # right now, so I'm going to wing it.
  def run(config_id)
    puts "RAH REMOVE: super event class #{config_id}"

    # just fake a bunch of stuff here
    self.save!
    e = CmdbEvent.new
    e.cmdb_id = self.id
    e
  end

  def node(name)
    puts "RAH REMOVE: super class node #{name}"
  end
  
  def data(key)
    puts "RAH REMOVE: super class data #{key}"
  end
  
  def as_json options={}
   {
     :name=> name,
     :order=> order,
     :id=> id,
     :description=> description,
     :type=> type,
     :created_at=> created_at,
     :updated_at=> updated_at
   }
  end
  
end
