# Copyright 2012, Dell
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
#
# TODO - this belongs in it's own barclamp!!!
module CmdbChef < Cmdb

  def initialize
    Chef::Config.node_name CHEF_NODE_NAME
    Chef::Config.client_key CHEF_CLIENT_KEY
    Chef::Config.chef_server_url CHEF_SERVER_URL
    super.initialize   
  end

  # I'm totally not understanding the proposal configs/proposals
  # right now, so I'm going to wing it.
  def run(config_id)
    super.run config_id
  end
  
  def node(name)
    begin 
      chef_init
      super.node name
      return Chef::Node.load(name)
    rescue Exception => e
      Rails.logger.warn("Could not recover Node on load #{name}: #{e.inspect}")
      return nil
    end
  end

  def data(bag_item)
    begin 
      chef_init
      super.data bag_item
      return Chef::DataBag.load "crowbar/#{bag_item}"
    rescue Exception => e
      Rails.logger.warn("Could not recover Chef Crowbar Data on load #{bag_item}: #{e.inspect}")
      return nil
    end
  end
  
  private
    
  def chef_escape(str)
    str.gsub("-:") { |c| '\\' + c }
  end
  
  def query_chef
    begin
      chef_init
      return Chef::Search::Query.new
    rescue
      return Chef::Node.new
    end
  end
  
end


