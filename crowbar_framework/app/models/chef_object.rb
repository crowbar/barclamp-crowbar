# -*- encoding : utf-8 -*-
# Copyright 2011-2013, Dell
# Copyright 2013, SUSE LINUX Products GmbH
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
# Author: Rob Hirschfeld
# Author: SUSE LINUX Products GmbH
#

class ChefObject
  class_attribute :chef_type

  @@CrowbarDomain = nil
  
  NOT_SET = 'not_set'
  
  def self.cloud_domain
    begin
      # NOTE: We are using a global here to avoid lookups.  We need to consider some better cache/expiration strategy
      if @@CrowbarDomain.nil?
        bag = ProposalObject.find_proposal('dns', 'default')
        @@CrowbarDomain = bag[:attributes][:dns][:domain] || %x{dnsdomainname}.strip
      end
      return @@CrowbarDomain
    rescue StandardError => e
      Rails.logger.warn("Could not lookup domain name from Crowbar DNS barclamp attributes/dns/domain key.  Error #{e.message}.")
      @@CrowbarDomain = nil # reset to make sure we do not cache it
      return %x{dnsdomainname}.strip
    end
  end

  def self.query_chef
    begin
      return Chef::Search::Query.new
    rescue
      return Chef::Node.new
    end
  end

  def self.chef_escape(str)
    str.gsub("-:") { |c| '\\' + c }
  end

  def self.crowbar_node(name)
    begin 
      return Chef::Node.load(name)
    rescue StandardError => e
      Rails.logger.warn("Could not recover Chef Crowbar Node on load #{name}: #{e.inspect}")
      return nil
    end
  end

  def self.crowbar_data(bag_item)
    begin 
      return Chef::DataBag.load "crowbar/#{bag_item}"
    rescue StandardError => e
      Rails.logger.warn("Could not recover Chef Crowbar Data on load #{bag_item}: #{e.inspect}")
      return nil
    end
  end

  def export(name = nil)
    name ||= self.respond_to?(:name) ? self.name : "unknown"
    file   = Rails.root.join("db", "#{self.chef_type}_#{name}.json")
    File.open(file, "w") { |f| f.write(self.to_json) }
  end
end

