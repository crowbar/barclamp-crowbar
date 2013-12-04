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

class Attrib < ActiveRecord::Base

  before_create :create_type_from_name

  attr_accessible :barclamp_id, :role_id, :type, :name, :description, :order, :map     # core relationship

  belongs_to      :role
  belongs_to      :barclamp

  scope           :by_name,              ->(name) { where(:name=>name) }

  # Return a deeply nested hash table built from the map with this attribute's
  # data at the end.
  def template(value)
    keys = map.split('/')
    raise "Cannot deal with an empty map!" if keys.empty?
    res = value
    while !keys.empty? do
      res = {keys.pop => res}
    end
    res
  end

  # Get the attribute value from the passed object.
  # For now, we are encoding information about the objects we can use directly in to
  # the Attrib class, and failing hard if we were passed something that
  # we do not know how to handle.
  def get(from)
    from = __resolve(from)
    d = case
        when from.is_a?(Node) then from.discovery
        when from.is_a?(DeploymentRole) then from.wall.deep_merge(from.data)
        when from.is_a?(NodeRole) then from.attrib_data
        when from.is_a?(Role) then from.template
        else raise("Cannot extract attribute data from #{from.class.to_s}")
        end
    begin
      map.split('/').each{|s|d = d[s]}
      return d
    rescue
      nil
    end
  end

  # Gets the requested value from the passed data, but returns it wrapped in template()
  # unless this attribute is not in the passed blob, in which case it returns nil.
  def extract(from)
    r = get(from)
    return r unless r
    template(r)
  end

  def wall_set(to,value)
    __set(to,value,:wall)
  end

  def user_set(to,value)
    __set(to,value,:user)
  end

  def system_set(to,value)
    __set(to,value,:system)
  end

  def set(to,value)
    Rails.logger.warn("Please do not use attrib.set")
    __set(to,value,:system)
  end

  private

  # This method ensures that we have a type defined for
  def create_type_from_name
    raise "attribs require a name" if self.name.nil?
    # remove the redundant part of the name (if any)
    name = self.name.gsub('-','_').camelize
    # Find the proper class to use to instantiate this attribute
    # 1. If the barclamp provides a specific class for this attribute, use it.
    # 2. Otherwise fall back on attrib class that the jig provides.
    # 3. Finally, fall back on the generic Attrib class.
    klassnames = []
    klassnames << "Barclamp#{self.barclamp.name.camelize}::Attrib::#{name}" if self.barclamp_id
    klassnames << "#{self.role.jig.type}Attrib" if self.role_id
    klassnames << "Attrib"
    klassnames.each do |t|
      if (t.constantize rescue nil)
        Rails.logger.info("Attrib: Using #{t} for #{self.name}")
        self.type = t
        return
      else
        Rails.logger.info("Attrib: #{t} cannot be used for #{self.name}")
      end
    end
    raise "Cannot find the appropriate class for attribute #{self.name}"
  end

  # If we were asked to do something with an attribute on a node,
  # but that attribute is part of a node role bound to that node,
  # use the node role instead.
  def __resolve(to)
    return to unless to.is_a?(Node) && self.role_id
    res = to.node_roles.where(:role_id => self.role_id).first
    raise("#{self.name} belongs to role #{role.name}, but node #{node.name} does not have a binding for it!") unless res
    res
  end

  # Set a new value for this attribute onto the passed object.
  # The last parameter is what area the new attribute should be placed on
  def __set(to,value,target=:system)
    to = __resolve(to)
    attr = case
           when to.is_a?(Node) then "discovery"
           when to.is_a?(Role) then "template"
           when to.is_a?(DeploymentRole) then target == :system ? "wall" : "data"
           when to.is_a?(NodeRole)
             case target
             when :system then 'systemdata'
             when :user then 'userdata'
             when :wall then 'wall'
             else raise("#{target} is not a valid target to write data to!")
             end
           else raise("Cannot write attribute data to #{to.class.to_s}")
           end
    transaction do
      data = JSON.parse(to.send(:read_attribute,attr))
      data.deep_merge(template(value))
      to.send(:write_attribute,attr,JSON.generate(data))
      to.save!
    end
  end

end
