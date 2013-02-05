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

#######
# An 'instance' of a configuration of a role.

class RoleInstance < ActiveRecord::Base

  attr_accessible :description, :order, :run_order, :states
  attr_accessible :barclamp_instance_id, :role_id
  
  validates_uniqueness_of :role_id, :scope => :barclamp_instance_id  
  
  belongs_to      :role,              :inverse_of => :role_instances
  belongs_to      :barclamp_instance
  alias_attribute :instance,          :barclamp_instance
  has_one         :barclamp,          :through => :barclamp_instance
  
  has_many        :attrib_instances,  :dependent => :destroy
  alias_attribute :values,            :attrib_instances
  has_many        :attribs,           :through => :attrib_instances
  
  # alias helper
  def name
    role.name
  end
  
  def add_attrib(attrib, value=nil, map=nil)
    a = Attrib.add attrib, barclamp.name
    begin 
      AttribInstance.find_by_attrib_id_and_role_instance_id! a.id, self.id
    rescue
      AttribInstance.create :attrib_id => a.id, :role_instance_id => self.id
    end
  end
  
  ##
  # Clone this role_instance
  # optionally, change parent too
  def deep_clone(bc_instance=nil)
    new_role = self.dup
    new_role.barclamp_instance_id = bc_instance.id if bc_instance
    new_role.save

    # clone the attributes
    # not there yet!

    new_role
  end

end
