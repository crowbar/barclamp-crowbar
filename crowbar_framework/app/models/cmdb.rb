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
  attr_accessible :name, :description, :order, :backend
  
  #validates_uniqueness_of :name, :case_sensitive => false, :message => I18n.t("db.notunique", :default=>"Name item must be unique")
  #validates_format_of :name, :with=>/[a-zA-Z][_a-zA-Z0-9]/, :message => I18n.t("db.lettersnumbers", :default=>"Name limited to [_a-zA-Z0-9]")
  
  has_many :cmdb_runs, :inverse_of => Cmdb

  def init( backend )
    if ( backend == 'chef' ) 
      chef_init
    else
      return nil
    end
  end

  # I'm totally not understanding the proposal configs/proposals
  # right now, so I'm going to wing it.
  def run(proposal_config_id)
    # just fake a bunch of stuff here
    self.save!
    r = CmdbRun.new
    r.cmdb_id = self.id
    r
  end


  @@CrowbarDomain = nil
  
  def query_chef
    begin
      chef_init
      return Chef::Search::Query.new
    rescue
      return Chef::Node.new
    end
  end

  def chef_init
    Chef::Config.node_name CHEF_NODE_NAME
    Chef::Config.client_key CHEF_CLIENT_KEY
    Chef::Config.chef_server_url CHEF_SERVER_URL
  end
  
  def chef_escape(str)
    str.gsub("-:") { |c| '\\' + c }
  end

  def node(name)
    if ( self.backend == 'chef' )
      node_chef(name)
    else
      nil
    end
  end

  def node_chef(name)
    begin 
      chef_init
      return Chef::Node.load(name)
    rescue Exception => e
      Rails.logger.warn("Could not recover Node on load #{name}: #{e.inspect}")
      return nil
    end
  end

  def data_chef(bag_item)
    begin 
      chef_init
      return Chef::DataBag.load "crowbar/#{bag_item}"
    rescue Exception => e
      Rails.logger.warn("Could not recover Chef Crowbar Data on load #{bag_item}: #{e.inspect}")
      return nil
    end
  end

end
