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
require 'json'

class JigModelTest < ActiveSupport::TestCase


  test "Unique Name" do
    Jig.create! :name=>"nodups", :type=>"BarclampCrowbar::Jig", :description=>"unit tests"
    e = assert_raise(ActiveRecord::RecordInvalid, ActiveRecord::RecordNotUnique) { Jig.create!(:name => "nodups") }
    assert_equal "Validation failed: Name Item must be un...", e.message.truncate(42)
    assert_raise(ActiveRecord::RecordInvalid, ActiveRecord::RecordNotUnique) { b = Node.create! :name => "nodups" }
  end

  test "Naming Conventions" do
    assert_raise(ActiveRecord::RecordInvalid) { Jig.create!(:name=>"1123", :type=>"BarclampCrowbar::Jig")}
    assert_raise(ActiveRecord::RecordInvalid) { Jig.create!(:name=>"1foo", :type=>"BarclampCrowbar::Jig")}
    assert_raise(ActiveRecord::RecordInvalid) { Jig.create!(:name=>"Ille!gal", :type=>"BarclampCrowbar::Jig")}
    assert_raise(ActiveRecord::RecordInvalid) { Jig.create!(:name=>" nospaces", :type=>"BarclampCrowbar::Jig")}
    assert_raise(ActiveRecord::RecordInvalid) { Jig.create!(:name=>"no spaces", :type=>"BarclampCrowbar::Jig")}
    assert_raise(ActiveRecord::RecordInvalid) { Jig.create!(:name=>"nospacesatall ", :type=>"BarclampCrowbar::Jig")}
  end

  test "Must have override object-missing" do
     Jig.create!(:name=>"foo", :type=>"JigFoo")
     e = assert_raise(ActiveRecord::SubclassNotFound) { Jig.find_by_name "foo" }
     assert_equal "The single-table inheritance mechanism failed...", e.message.truncate(48)
  end

  test "Must have override object - present" do
     c = Jig.create! :name=>"subclass", :type=>"BarclampCrowbar::Jig"
     assert_equal false, c.nil?
     assert_equal c.name, "subclass"
     assert_equal c.type, "BarclampCrowbar::Jig"
  end
  
  test "Returns correct objective type" do
    c = Jig.create! :name=>"type_test", :type=>"BarclampCrowbar::Jig"
    t = Jig.find_by_name "type_test"
    assert_equal c.type, t.type
    assert_equal t.type, "BarclampCrowbar::Jig"
    assert_kind_of Jig, c
  end
  
end

