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
require 'json'

class JigEventModelTest < ActiveSupport::TestCase


  test "Unique Name" do
    JigEvent.create! :name=>"nodups", :type=>"JigEventTest", :description=>"unit tests"
    e = assert_raise(ActiveRecord::RecordInvalid, ActiveRecord::RecordNotUnique, SQLite3::ConstraintException) { JigEvent.create!(:name => "nodups") }
    assert_equal "Validation failed: Name Item must be un...", e.message.truncate(42)
    assert_raise(ActiveRecord::RecordInvalid, ActiveRecord::RecordNotUnique, SQLite3::ConstraintException) { b = Node.create! :name => "nodups" }
  end

  test "Naming Conventions" do
    assert_raise(ActiveRecord::RecordInvalid) { JigEvent.create!(:name=>"1123", :type=>"JigEventTest")}
    assert_raise(ActiveRecord::RecordInvalid) { JigEvent.create!(:name=>"1foo", :type=>"JigEventTest")}
    assert_raise(ActiveRecord::RecordInvalid) { JigEvent.create!(:name=>"Ille!gal", :type=>"JigEventTest")}
    assert_raise(ActiveRecord::RecordInvalid) { JigEvent.create!(:name=>" nospaces", :type=>"JigEventTest")}
    assert_raise(ActiveRecord::RecordInvalid) { JigEvent.create!(:name=>"no spaces", :type=>"JigEventTest")}
    assert_raise(ActiveRecord::RecordInvalid) { JigEvent.create!(:name=>"nospacesatall ", :type=>"JigEventTest")}
  end

  test "Must have override object-missing" do
     JigEvent.create!(:name=>"foo", :type=>"JigEventFoo")
     e = assert_raise(ActiveRecord::SubclassNotFound) { JigEvent.find_by_name "foo" }
     assert_equal "The single-table inheritance mechanism failed...", e.message.truncate(48)
  end

  test "Must have override object - present" do
     c = JigEvent.create! :name=>"subclass", :type=>"JigEventTest"
     assert_equal false, c.nil?
     assert_equal c.name, "subclass"
     assert_equal c.type, "JigEventTest"
  end
  
  test "Returns correct objective type" do
    c = JigEvent.create! :name=>"type_test", :type=>"JigEventTest"
    t = JigEvent.find_by_name "type_test"
    assert_equal c.type, t.type
    assert_equal t.type, "JigEventTest"
    assert_kind_of JigEvent, c
  end
  
  test "as_json routines returns correct items" do
    name = "json_test"
    type = "JigEventTest"
    description = "This is a unit test"
    c = JigEvent.create! :name=>name, :type=>type, :description => description, :order => 100
    j = JSON.parse(c.to_json)
    assert_equal j['name'], name
    assert_equal j['type'], type
    assert_equal j['description'], description
    assert_equal j['order'], 100
    assert_not_nil j['created_at']
    assert_not_nil j['updated_at']
    assert_equal j.length, 7
  end
  
end

