# Copyright 2012, Dell
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

class NodeAttribute < ActiveRecord::Base
  attr_accessible :actual_serialized, :proposed_serialized

  belongs_to  :attribute, :class_name=>"CmdbAttribute"
  belongs_to  :node
  belongs_to  :run, :class_name => "CmdbRun", :foreign_key => "cmdb_run_id"

  # Returns state of value (:ready or :pending)
  def state
    if proposed_serialized.nil?
      return :ready
    if actual_serialized.nil?
      return :pending
    if actual_serialized == proposed_serialized
      return :ready
    else
      return :pending
    end
  end   
  
  # for now, none of the proposed values are visible
  def value
    return self.actual
  end
  
  def actual=(value)
    self.actual_serialized = Marshal::dump(value)
  end
  
  def actual
    Marshal::load(self.actual_serialized)
  end
  
  def proposed=(value)
    self.proposed_serialized = Marshal::dump(value)
  end
  
  def proposed
    Marshal::load(self.proposed_serialized)
  end
 
end
