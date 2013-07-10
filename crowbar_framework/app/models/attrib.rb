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

  DEFAULT_CLASS = BarclampCrowbar::AttribDefault rescue Attrib
  MARSHAL_EMPTY = "EMPTY"
  
  # for now, none of the proposed values are visible
  def value
    nr.value[map]
  end

  def node_values(node)
    active = node.deployments.first.active_snapshot
    nr = NodeRole.find :node_id=>node.id, :snapshot_id=>active.id, :role_id=>role_id
    nr.value
  end

  # SUBCLASS THIS METHOD if you want to change how data is found in the input data
  # Called by the barclamp.process_inbound_data routine
  # find a single attribute in a json data set
  # / is used as a delimiter
  # optimized to 6 levels without looping
  def find_attrib(data, path)
    nav = path.split '/'
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
        nav.each { |key| data = data[key] }
    end
  end
    
  private
  
  # make sure some safe values are set for the node
  def set_type_and_role
    # we need to have a type, cannot use the superclass!
    self.type = DEFAULT_CLASS.to_s if self.type.nil?
  end
  
end
