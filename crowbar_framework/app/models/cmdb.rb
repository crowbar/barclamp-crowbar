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
require 'chef'
require 'json'

class Cmdb < ActiveRecord::Base
  
  attr_accessible :name, :description, :order
  
  validates_uniqueness_of :name, :message => I18n.t("db.notunique", :default=>"Name item must be unique")
  validates_format_of :name, :with=>/[a-zA-Z][_a-zA-Z0-9]/, :message => I18n.t("db.lettersnumbers", :default=>"Name limited to [_a-zA-Z0-9]")
  
  has_many :cmdb_runs
  has_many :cmdb_events, :through => :cmdb_runs

  def init
    Chef::Config.node_name CHEF_NODE_NAME
    Chef::Config.client_key CHEF_CLIENT_KEY
    Chef::Config.chef_server_url CHEF_SERVER_URL
  end

  def run
    # create a CmdbRun object and run it and return it.
    return nil
  end

  def node(search)
    self.init
    node = Chef::Node.load('admin.crowbar.org')
    puts node.size
    m = CmdbMap.find(1)
    m.init_map
    m.all_maps.each { |k|
      v = m.map_get(k)
      nv = eval "node" + v
      a = CmdbAttribute.new( :name => k, :value => nv )
      puts a.inspect
      a.save!
    }
  end
  # self.find "name:#{chef_escape(name)}"
end

