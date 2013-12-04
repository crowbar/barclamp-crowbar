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

  # Get the requested value from the passed blob of data using map as a
  # sleector into the deeply nested hash table that we assume data is.
  def get(data)
    begin
      d = data
      map.split('/').each{|s|d = d[s]}
      return d
    rescue
      nil
    end
  end

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

  private

  # This method ensures that we have a type defined for
  def create_type_from_name
    raise "attribs require a name" if self.name.nil?
    raise "attribs require a role" if self.barclamp_id.nil?
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

end
