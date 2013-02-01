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
require 'test_helper'
 
class BarclampInstanceModelTest < ActiveSupport::TestCase

  def setup
    @bc = Barclamp.create! :name=>'bc_instance_test'
  end
  
  test "add role to instance" do
    name = "foo"
    assert_nil Role.find_by_name name
    bi = BarclampInstance.create :name => "add_role", :barclamp_id => @bc.id
    assert_equal 0, bi.role_instances.count
    assert_not_nil bi
    # now add the role
    ri = bi.add_role name
    assert_not_nil ri
    assert_equal name, ri.role.name
    assert_not_nil Role.find_by_name name, "now we have the name"
    assert_equal 1, bi.role_instances.count
  end
  
end

