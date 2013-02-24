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
require 'test_helper'
 
class RoleModelTest < ActiveSupport::TestCase
  
  test "Naming Conventions" do
    assert_raise(ActiveRecord::RecordInvalid) { RoleType.create!(:name=>"1123") }
    assert_raise(ActiveRecord::RecordInvalid) { RoleType.create!(:name=>"1foo") }
    assert_raise(ActiveRecord::RecordInvalid) { RoleType.create!(:name=>"Ille!gal") }
    assert_raise(ActiveRecord::RecordInvalid) { RoleType.create!(:name=>" nospaces") }
    assert_raise(ActiveRecord::RecordInvalid) { RoleType.create!(:name=>"no spaces") }
    assert_raise(ActiveRecord::RecordInvalid) { RoleType.create!(:name=>"nospacesatall ") }
  end

  test "names can have dash" do
    assert_nothing_raised { RoleType.create!(:name=>"chef-role")  }
    assert_not_nil RoleType.find_by_name 'chef-role'
  end
      
  test "order is priority" do
    nr = RoleType.new
    nr.order = 666
    assert_equal nr.order, nr.priority
    nr.priority = 999
    assert_equal nr.order, nr.priority
  end
  
  test "private role" do
    p = RoleType.find_private
    assert_equal "private", p.name
    assert_equal 1, p.order
  end
  
  test "add by name" do
    name = "foo"
    c = RoleType.count
    r = RoleType.add name
    assert_equal c+1, RoleType.count
    assert_equal name, r.name
  end
  
  test "add finds existing" do
    name = "bar"
    r = RoleType.add name
    RoleType.add "ignore_me"
    n = RoleType.add name
    assert_equal r.id, n.id
    assert_equal name, n.name
  end

  test "add finds from role" do
    name = "bar"
    r = RoleType.add name
    n = RoleType.add r
    assert_equal r.id, n.id
    assert_equal name, n.name
  end
  
#  test "default category" do
#    g = RoleType.create!(:name=>"foo")
#    assert_not_nil g, "Node Created"
#    assert_equal g.category, 'ui', "default category"
#  end
  
#  test "alternate category" do
#    g = RoleType.create!(:name=>"foo", :category=>'rack')
#    assert_not_nil g, "Node created"
#    assert_equal g.name, 'foo', "name right"
#    assert_equal g.category, 'rack', "category right"
#  end
  
#  test "only allowed categories" do
#    assert_raise(ActiveRecord::RecordInvalid) { RoleType.create!(:name=>"foo", :category=>'foo') }
#  end

#  test "no dup name+category" do
#    g = RoleType.create! :name=>"foo", :category=>'rack'
#    assert_not_nil g, "Node Created"
#    assert_raise(ActiveRecord::RecordInvalid) { RoleType.create!(:name=>"foo", :category=>'rack') }
#  end


end

