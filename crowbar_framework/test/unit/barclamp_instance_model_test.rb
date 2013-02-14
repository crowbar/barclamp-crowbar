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
  
  test "deep clone works" do
    b = Barclamp.import 'test'
    bi = BarclampInstance.create :name=>"deep_clone", :status => BarclampInstance::STATUS_APPLIED, :barclamp_id => b.id 
    r1 = Role.create :name=>"something"
    r2 = Role.create :name=>"anotherthing"
    ri1 = RoleInstance.create :role_id => r1.id, :barclamp_instance_id=>bi.id, :order=>100
    ri2 = RoleInstance.create :role_id => r2.id, :barclamp_instance_id=>bi.id, :order=>200
    assert_equal 2, bi.role_instances(true).count
    clone = bi.deep_clone nil, 'new_me'
    assert_not_nil clone
    assert_equal 'new_me', clone.name
    assert_equal clone.status, BarclampInstance::STATUS_NONE
    assert_equal 2, clone.role_instances(true).count
    assert_equal 'something', clone.role_instances.first.name
    assert_not_equal ri1.id, clone.role_instances.first.id
    assert_equal 'anotherthing', clone.role_instances.second.name
    assert_not_equal ri2.id, clone.role_instances.second.id
  end
  
end

