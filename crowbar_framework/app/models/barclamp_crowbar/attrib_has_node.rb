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

class BarclampCrowbar::AttribHasNode < Attrib

  before_save :special_attrib
  
  attr_protected :attrib_id

  HASNODE_NAME = 'has_node'

  # we are not using this yet - will be empty until it has meaning!
  def state 
    :empty
  end
  
  private
  
  # this is a special purpose attrib and can only be this type
  def special_attrib
    # we're going to ENFORCE that this model uses the has_role type
    a = AttribType.find_by_id self.attrib_type_id
    if a.nil? or !a.name.eql? HASNODE_NAME
      has_node = AttribType.add :name=>HASNODE_NAME, :description=>I18n.t('model.attribs.role.has_node'), :order=>999999
      self.attrib_type_id = has_node.id
    end
  end
      
end
