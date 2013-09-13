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
  attr_accessible :server, :client_name, :client_role_name, :key, :active

  # 
  # Validate the name should unique 
  # and that it starts with an alph and only contains alpha,digist,hyphen,underscore
  #
  validates_uniqueness_of :name, :case_sensitive => false, :message => I18n.t("db.notunique", :default=>"Name item must be unique")
  validates_format_of     :name, :with=> /^[a-zA-Z][_a-zA-Z0-9]*$/, :message => I18n.t("db.lettersnumbers", :default=>"Name limited to [_a-zA-Z0-9]")

  has_many    :roles,     :primary_key=>:name, :foreign_key=>:jig_name


  def self.active(jig)
    Jig.where(:name=>jig, :active=>true).length > 0
  end

  # Create a node in all jig. The exact actions depend on the jig.
  def self.create_node(node)
    broadcast_to_jigs { |jig| jig.create_node(node) }    
  end

  # Delete a node from all jig. The exact actions depend on the jig.
  def self.delete_node(node)
    broadcast_to_jigs { |jig|  jig.delete_node(node) }    
  end

  # OVERRIDE with actual methods
  def delete_node(node)
    Rails.logger.debug("jig.delete_node(#{node.name}) not implemented for #{self.class}.  This may be OK")
  end

  # expected to return JSON to be returned to the node
  def create_node(node)
    Rails.logger.debug("jig.create_node(#{node.name}) not implemented for #{self.class}.  This may be OK")
    {}
  end

  def client_role
    crn = client_role_name
    return nil if crn.nil?
    res = Role.where(:name => crn).first
    # Jig client roles must be implicit roles.
    raise "#{crn} is not an implicit role!" unless res.implicit
    # Jig client roles cannot be implemented by the jig they implement
    # client-side functionality for.
    raise "#{crn} is implemented by and requires #{name}!" if res.jig_name == name
    res
  end

  # Run a single noderole.
  # The noderole must be in TRANSITION state.
  # This function is intended to be overridden by the jig subclasses,
  # and only used for debugging purposes.
  def run(nr)
    raise "Cannot call run on the top-level Jig!"
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

class NoopJig < Jig

  def run(nr)
    nr.state = NodeRole::ACTIVE
    nr
  end

end
