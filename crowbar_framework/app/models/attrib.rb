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
  
  # for now, none of the proposed values are visible
  def value
    nr.value[map]
  end

  def node_values(node)
    active = node.deployments.first.active_snapshot
    nr = NodeRole.find :node_id=>node.id, :snapshot_id=>active.id, :role_id=>role_id
    nr.value
  end
      
  def as_json options={}
   {
     :id=> id,
     :value=> value,
     :created_at=> created_at,
     :updated_at=> updated_at,
     :name => name,
     :order => order,
     :description=> description
   }
  end

  private
  
  # make sure some safe values are set for the node
  def set_type_and_role
    # we need to have a type, cannot use the superclass!
    self.type = DEFAULT_CLASS.to_s if self.type.nil?
  end
  
end
