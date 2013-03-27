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

class BarclampCrowbar::AttribHasDeployment < Attrib

  HASDEPLOYMENT_NAME = 'requires'

  before_save :special_attrib
  
  attr_protected :attrib_id

  has_one :deployment, :class_name => "Deployment", :primary_key => :id_actual, :foreign_key=> :id

  # Return the Deployment State
  def state
    deployment.state
  end
  
  # Compare the deployment state to known ready value
  def ready?
    self.state == 5
  end

  # internal use
  def actual=(value)
    self.value_actual = value
  end
  
  # internal use
  def actual
    self.value_actual
  end
  
  private
  
  # this is a special purpose attrib and can only be this type
  def special_attrib
    # we're going to ENFORCE that this model uses the has_role type
    a = AttribType.find_by_id self.attrib_type_id
    if a.nil? or !a.name.eql? HASDEPLOYMENT_NAME
      has_deployment = AttribType.add :name=>HASDEPLOYMENT_NAME, :description=>I18n.t('model.attribs.role.has_deployment'), :order=>999998
      self.attrib_type_id = has_deployment.id
    end
  end
      
end
