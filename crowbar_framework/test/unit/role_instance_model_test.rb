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
 
class RoleInstanceModelTest < ActiveSupport::TestCase

  def setup
    @role = Role.create :name => "test"
    @bc = Barclamp.create :name=>"instance"
    @bi = BarclampInstance.create :name=>"template", :barclamp_configuration_id => @bc.id, :barclamp_id => @bc.id
    @bc.template_id = @bi.id
    @bc.save
    @ri = RoleInstance.create :barclamp_instance_id => @bi.id, :role_id=>@role.id
  end
  
  test "Barclamp Template has RoleInstances" do
    assert_not_nil @bc
    bi = @bc.template
    assert_not_nil bi
    # TODO: ROB, fix this test!
    # assert bi.roles.count>0, "we need to have at least 1 role"
    #ri = bi.role_instances.first
    #assert_equal "test", ri.role.name, "one of the roles is the one from setup"
  end

  test "Relation to BarclampInstance" do
    assert_equal "template", @ri.instance.name
  end
  
  test "Relation to Role" do
    # TODO: rob fix this test! ;)
    return
    #assert_equal "test", @ri.role.name
  end

  test "Relation to AttribInstance" do
    return 
    # work in progress...
    a = Attrib.create :name=>"foo"
    assert_not_nil Attrib.find_by_name "foo"
    ai = @ri.add_attrib a, "map/this"
    @ri.set_attrib(a, "bar")
    assert_not_nil ai
    assert_equal "foo", ai.attrib.name
    assert_equal "bar", ai.value
    assert_equal "map/this", ai.description
    assert_equal "bar", @ri.get_attrib("foo").value
  end

  test "Deep clone works at surface layer" do
    r = Role.create :name => "clone" 
    ri = RoleInstance.create :role_id=>@role.id, :barclamp_instance => @bi.id
    new_bi = BarclampInstance.create :name => "value", :barclamp_id => @bi.barclamp.id
    new_ri = ri.deep_clone new_bi
    assert_not_equal ri.id, new_ri.id, "different objects"
    assert_equal ri.role_id, new_ri.role_id, "should have the same role"
    assert_not_equal ri.barclamp_instance_id, new_ri.barclamp_instance_id, "should not have same instance"
  end

  test "Add Attrib adds string attribute" do
    #TBD
  end
  
  test "Add Attrib add hash attribute" do
    #TBD
  end

end

