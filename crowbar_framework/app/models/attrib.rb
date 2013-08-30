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

  before_create :set_type_and_role

  attr_accessible :role_id, :type, :name, :description, :order, :map     # core relationship

  belongs_to      :role

  scope           :by_name,              ->(name) { where(:name=>name) }
    
  # this is designed to be over-ridden, but let's make the default useful
  # expects the json data from node.discovery
  def value(data)

    begin
      # this code does a simple drill into the hash using / as a delimeter
      nav = self.map.split '/'
      # add some optimization to avoid looping down through the structure
      case nav.length 
        when 1 
          data[nav[0]]
        when 2
          data[nav[0]][nav[1]]
        when 3
          data[nav[0]][nav[1]][nav[2]]
        when 4
          data[nav[0]][nav[1]][nav[2]][nav[3]]
        when 5
          data[nav[0]][nav[1]][nav[2]][nav[3]][nav[4]]
        when 6
          data[nav[0]][nav[1]][nav[2]][nav[3]][nav[4]][nav[5]]
        else 
          # we could use this without the optimized code, but it's not as fast
          nav.each { |key| data = data[key] }
      end
    rescue
      nil
    end
  end

 
  # this is designed to be over-ridden, but let's make the default useful
  # returns the json snippet that should be added to node.discovery
  def discovery(arg)

    # this code does a simple drill into the hash using / as a delimeter
    nav = self.map.split '/'
    # add some optimization to avoid looping down through the structure
    data = case nav.length 
      when 1 
        {nav[0] => arg}
      when 2
        {nav[0] => {nav[1] => arg}}
      when 3
        {nav[0] => {nav[1] => {nav[2] => arg}}}
      when 4
        {nav[0] => {nav[1] => {nav[2] => {nav[3] => arg }}}}
      when 5
        {nav[0] => {nav[1] => {nav[2] => {nav[3] => {nav[4] => arg }}}}}
      when 6
        {nav[0] => {nav[1] => {nav[2] => {nav[3] => {nav[4] => {nav[5] => arg }}}}}}
      else 
        # we could use this without the optimized code, but it's not as fast
        raise "too deep in attrib.value="
    end
  end

  private
  
  # make sure some safe values are set for the node
  def set_type_and_role
    # we need to have a type, cannot use the superclass!
    self.type = Attrib.to_s if self.type.nil?
  end
  
end
