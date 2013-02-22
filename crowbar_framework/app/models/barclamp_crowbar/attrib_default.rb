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

class BarclampCrowbar::AttribDefault < Attrib

  # Returns state of value of :empty, :unready or :ready
  def state 
    Attrib.calc_state value_actual, value_request, jig_run_id
  end
  
  def request=(value)
    self.jig_run_id = 0 if self.jig_run_id.nil?
    self.value_request = Attrib.serial_in(value)
  end
  
  def request
    Attrib.serial_out value_request
  end
  
  # used by the API when values are set outside of Jig runs
  def actual=(value)
    self.jig_run_id = 0 if self.jig_run_id.nil?
    self.value_actual = Attrib.serial_in(value)
  end
  
  def actual
    Attrib.serial_out value_actual
  end

    
end
