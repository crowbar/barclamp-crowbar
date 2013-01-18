# Copyright 2011, Dell 
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
class ChefObject

  @@CrowbarDomain = nil
  
  NOT_SET = 'not_set'

  def self.query_chef
    begin
      chef_init
      return Chef::Search::Query.new
    rescue
      return Chef::Node.new
    end
  end

  def self.chef_init
    Chef::Config.node_name CHEF_NODE_NAME
    Chef::Config.client_key CHEF_CLIENT_KEY
    Chef::Config.chef_server_url CHEF_SERVER_URL
    Rails.logger.debug("chef_init called: #{caller.inspect}")
  end
  
  def self.chef_escape(str)
    str.gsub("-:") { |c| '\\' + c }
  end

  def self.crowbar_node(name)
    begin 
      chef_init
      return Chef::Node.load(name)
    rescue Exception => e
      Rails.logger.warn("Could not recover Chef Crowbar Node on load #{name}: #{e.inspect}")
      return nil
    end
  end

  def self.crowbar_data(bag_item)
    begin 
      chef_init
      return Chef::DataBag.load "crowbar/#{bag_item}"
    rescue Exception => e
      Rails.logger.warn("Could not recover Chef Crowbar Data on load #{bag_item}: #{e.inspect}")
      return nil
    end
  end

end

