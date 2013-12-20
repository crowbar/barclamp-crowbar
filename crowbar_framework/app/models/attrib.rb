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

  def self.get(name, from, source=:discovery)
    attrib = ( name.is_a?(ActiveRecord::Base) ? name : Attrib.find_key(name) )
    attrib.get(from, source)
  end

  # Get the attribute value from the passed object.
  # For now, we are encoding information about the objects we can use directly in to
  # the Attrib class, and failing hard if we were passed something that
  # we do not know how to handle.
  def get(from_orig,source=:all)
    from = __resolve(from_orig)
    d = case
        when from.is_a?(Node)
          source == :hint ? from.hint : from.discovery
        when from.is_a?(DeploymentRole)
          case source
          when :all then from.wall.deep_merge(from.data)
          when :wall then from.wall
          else from.data
          end
        when from.is_a?(NodeRole)
          case source
          when :all then from.attrib_data
          when :wall then from.wall
          when :system then from.sysdata
          when :user then from.data
          when :hint then from_orig.hint[role.name]
          else raise("#{from} is not a valid source to read noderole data from!")
          end
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

  def hint_set(to,value)
    __set(to,value,:hint)
  end

  def discovery_set(to,value)
    __set(to,value,:discovery)
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

  def set(to,value,type=:system)
    __set(to,value,type)
  end

  def self.set(name, to, value, type)
    a = Attrib.find_key name
    a.set(to,value,type)
  end

  private

  # used to create a mapping for discovery values from the map
  def map_set_value(map,value)
    return ( map =~ /^([^\/]*)\/(.*)/ ? { $1 => map_set_value($2, value)} : { map => value } )
  end


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
    klassnames << "#{self.role.jig.type}Attrib" if self.role_id && (Jig.where(:name => role.jig_name).count > 0)
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
    raise("#{self.name} belongs to role #{role.name}, but node #{to.name} does not have a binding for it!") unless res
    res
  end

  # Set a new value for this attribute onto the passed object.
  # The last parameter is what area the new attribute should be placed on
  def __set(to_orig,value,target=:system)
    to = __resolve(to_orig)
    case
    when to.is_a?(Node)
      case target
      when :hint then to.hint_update(map_set_value("#{role.name}/#{map}", value))
      when :discovery then  to.discovery_update(map_set_value(map,value))
      else raise("#{target} is not a valid target to write node data to!")
      end
    when to.is_a?(Role) then to.template_update(value)
    when to.is_a?(DeploymentRole)
      target == :system ? to.wall_update(value) : to.data_update(value)
    when to.is_a?(NodeRole)
      case target
      when :system then to.sysdata_update(value)
      when :user then to.data_update(value)
      when :hint then to_orig.hint_update(map_set_value("#{role.name}/#{map}", value))
      when :wall then to.wall_update(value)
      else raise("#{target} is not a valid target to write noderole data to!")
      end
    else raise("Cannot write attribute data to #{to.class.to_s}")
    end
  end

end
