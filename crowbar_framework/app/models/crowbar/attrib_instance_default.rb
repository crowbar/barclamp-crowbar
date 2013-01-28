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

class Crowbar::AttribInstanceDefault < AttribInstance

  MARSHAL_NIL   = "\004\b0"
  MARSHAL_EMPTY = "empty"
  
  # Returns state of value of :empty, :set (by API) or :managed (by Jig)
  def state 
    if value_actual.eql? MARSHAL_EMPTY and value_request.eql? MARSHAL_EMPTY
      return :empty
    elsif !value_actual.eql? value_request and !value_request.eql? MARSHAL_EMPTY
      return :active
    elsif jig_run_id == 0
      return :set
    else
      return :managed
    end
  end
  
  def request=(value)
    self.jig_run_id = 0 if self.jig_run_id.nil?
    self.value_request = Marshal::dump(value)
  end
  
  def request
    v = value_request
    if v.eql? MARSHAL_EMPTY
      nil
    else
      Marshal::load(v)
    end
  end
  
  # used by the API when values are set outside of Jig runs
  def actual=(value)
    self.jig_run_id = 0 if self.jig_run_id.nil?
    self.value_actual = Marshal::dump(value)
  end
  
  def actual
    v = value_actual
    if v.eql? MARSHAL_EMPTY
      nil
    else
      Marshal::load(v)
    end
  end

    
end
