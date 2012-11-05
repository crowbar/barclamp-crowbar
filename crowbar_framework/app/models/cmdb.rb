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
#

# A Cmdb is the interface to a particular Cmdb backend (puppet or chef).
# The table has the history of all the runs against all the Cmdb backends.
# It provides connection management to the backends.
# Its 'run' method is called with a 'proposal config' to start a run of the Cmdb
#
# A CmdbRun is a series of events that makes up a the application of a proposal config.
# Based upon the proposal config, it determines what can be executed in serial or in 
# parallel. CmdbRun looks up the Nodes and Roles based upon the Proposal Config
# and kicks off events to apply those roles to those nodes
#

class Cmdb < ActiveRecord::Base

  attr_accessible :name, :description, :type, :order

  # 
  # Validate the name should unique 
  # and that it starts with an alph and only contains alpha,digist,hyphen,underscore
  #
  validates_uniqueness_of :name, :case_sensitive => false, :message => I18n.t("db.notunique", :default=>"Name item must be unique")
  validates_format_of :name, :with=> /^[a-zA-Z][_a-zA-Z0-9]*$/, :message => I18n.t("db.lettersnumbers", :default=>"Name limited to [_a-zA-Z0-9]")
  
  #has_many :cmdb_runs, :inverse_of => Cmdb

  def init
    puts "RAH REMOVE: super class initialize"
  end
  
  # I'm totally not understanding the proposal configs/proposals
  # right now, so I'm going to wing it.
  def run(config_id)
    puts "RAH REMOVE: super run class #{config_id}"

    # just fake a bunch of stuff here
    self.save!
    r = CmdbRun.new
    r.cmdb_id = self.id
    r
  end

  def node(name)
    puts "RAH REMOVE: super class node #{name}"
  end
  
  def data(key)
    puts "RAH REMOVE: super class data #{key}"
  end
  
end
