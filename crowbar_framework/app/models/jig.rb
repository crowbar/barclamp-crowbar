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

=begin
  The Jig (base class) provides abstract interface for jig implementations. The general pattern
  is  that the base class provides class methods, that either:
   - locate the appropriate Jig instance and call the instance method on them
   - call the respective instance method on all jig's

  The exact pattern depends on the operation - some operations are 'broadcast' in nature, 
  while some should only target a particular jig instance.
=end


class Jig < ActiveRecord::Base

  attr_accessible :id, :name, :description, :type, :order
  attr_accessible :server, :client_name, :key, :active

  # 
  # Validate the name should unique 
  # and that it starts with an alph and only contains alpha,digist,hyphen,underscore
  #
  validates_uniqueness_of :name, :case_sensitive => false, :message => I18n.t("db.notunique", :default=>"Name item must be unique")
  validates_format_of     :name, :with=> /^[a-zA-Z][_a-zA-Z0-9]*$/, :message => I18n.t("db.lettersnumbers", :default=>"Name limited to [_a-zA-Z0-9]")
  
  has_many        :roles,      :dependent => :destroy

=begin 
Allocate a node, and start the node install process
=end
  def self.install_node(node)
  end

=begin 
Create a node in all jig. The exact actions depend on the jig.
=end
  def self.create_node(node)
    broadcast_to_jigs { |jig| jig.create_node(node) }    
  end

=begin 
Delete a node from all jig. The exact actions depend on the jig.
=end
  def self.delete_node(node)
    broadcast_to_jigs { |jig|  jig.delete_node(node) }    
  end

  #
  # Update node infomration from a Jig, and process node attributes.
  # Attributes are tied to Runs and to Events, so a new Event is created, using description passed in
  def self.refresh_node(descr, node)    
    jigs = find_jigs_for_node(node)
#Rails.logger.debug "ZEHICLE #{BarclampChef::Jig.all.first.inspect} ??"
#Rails.logger.debug "ZEHICLE #{BarclampChef::Jig.all.first.read_node_data(node)} ??"
#Rails.logger.debug "ZEHICLE #{node.name} Jig refresh #{jigs.join(',')}"
 #   bcs = node.deployments.map { |d| d.barclamp }.uniq
    jigs.each do |j| 
      d = j.read_node_data(node)
#Rails.logger.debug "ZEHICLE #{node.name} > jig #{j.name} got #{d}"
    end
  
#      next if  d.nil?
#      evt = j.create_event(nil)
#      evt.name="refesh:node:#{node.id}#{Time.now.to_i}"
#      barclamps={}
#      node.deployments.inject { | barclamps,dep|
#         barclamps[dep.barclamp] ||=[]
#         barclamps[dep.barclamp] << dep.name
#      }      
#      barclamps.each {|bc|
        ### this should be per deployment... but many other updates required.
#        bc.process_inbound_data  d
#      }


  end

  # OVERRIDE with actual delete effort
  def delete_node(node)
    Rails.logger.debug("jig.delete_node(#{node.name}) not implemented for #{self.class}.  This may be OK")
  end

  def create_node(node)
    Rails.logger.debug("jig.create_node(#{node.name}) not implemented for #{self.class}.  This may be OK")
  end

  # Return a JSON representation of the information this jig knows about this node.
  def read_node_data(node)
    Rails.logger.debug("jig.read_node_data(#{node.name}) not implemented for #{self.class}.  This may be OK")
  end

private


=begin
  Utility method to iterate over jigs, and callback to  the mandatory block.
  If an error occurs on one jig, iteration continues to others.
  Jigs are left to manage thier own transactions - if one jig failes, the 
  others should not be impacted (hence no over arching transaction)
=end
  def self.broadcast_to_jigs(desc="no description")
    raise "no block given" unless block_given?
    Jig.all.each { |x|
      begin
        yield x
      rescue => exc
        Rails.logger.warn("failed to invoke #{desc} on jig: #{x.inspect}")
        Rails.logger.warn("Exception: #{exc.inspect}")
        Rails.logger.warn("Backtrace: #{exc.backtrace}")
      end
    }
  end
end
