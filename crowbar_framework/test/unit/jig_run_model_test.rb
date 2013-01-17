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

class JigRunModelTest < ActiveSupport::TestCase

  test "Unique Name" do
    JigRun.create! :name=>"nodups", :type=>"JigRunTest", :description=>"unit tests"
    e = assert_raise(ActiveRecord::RecordInvalid, ActiveRecord::RecordNotUnique, SQLite3::ConstraintException) { JigRun.create!(:name => "nodups") }
    assert_equal "Validation failed: Name Item must be un...", e.message.truncate(42)
    assert_raise(ActiveRecord::RecordInvalid, ActiveRecord::RecordNotUnique, SQLite3::ConstraintException) { b = Node.create! :name => "nodups" }
  end

  test "Naming Conventions" do
    assert_raise(ActiveRecord::RecordInvalid) { JigRun.create!(:name=>"1123", :type=>"JigRunTest")}
    assert_raise(ActiveRecord::RecordInvalid) { JigRun.create!(:name=>"1foo", :type=>"JigRunTest")}
    assert_raise(ActiveRecord::RecordInvalid) { JigRun.create!(:name=>"Ille!gal", :type=>"JigRunTest")}
    assert_raise(ActiveRecord::RecordInvalid) { JigRun.create!(:name=>" nospaces", :type=>"JigRunTest")}
    assert_raise(ActiveRecord::RecordInvalid) { JigRun.create!(:name=>"no spaces", :type=>"JigRunTest")}
    assert_raise(ActiveRecord::RecordInvalid) { JigRun.create!(:name=>"nospacesatall ", :type=>"JigRunTest")}
  end

  test "Must have override object-missing" do
     JigRun.create!(:name=>"foo", :type=>"JigRunFoo")
     e = assert_raise(ActiveRecord::SubclassNotFound) { JigRun.find_by_name "foo" }
     assert_equal "The single-table inheritance mechanism failed...", e.message.truncate(48)
  end

  test "Must have override object - present" do
     c = JigRun.create! :name=>"subclass", :type=>"JigRunTest"
     assert_equal false, c.nil?
     assert_equal c.name, "subclass"
     assert_equal c.type, "JigRunTest"
  end
  
  test "Returns correct objective type" do
    c = JigRun.create! :name=>"type_test", :type=>"JigRunTest"
    t = JigRun.find_by_name "type_test"
    assert_equal c.type, t.type
    assert_equal t.type, "JigRunTest"
    assert_kind_of JigRun, c
  end
  
  test "as_json routines returns correct items" do
    name = "json_test"
    type = "JigRunTest"
    status = "Done"
    result = "255"
    jig_event_id = 4
    description = "This is a unit test"
    c = JigRun.create! :name=>name, :type=>type, :description => description, :order => 100, :result => result, :status => status, :jig_event_id => jig_event_id
    j = JSON.parse(c.to_json)
    assert_equal j['type'], type
    assert_equal j['name'], name
    assert_equal j['description'], description
    assert_equal j['order'], 100
    assert_equal j['result'], result
    assert_equal j['status'], status
    assert_equal j['jig_event_id'], jig_event_id
    assert_not_nil j['created_at']
    assert_not_nil j['updated_at']
    assert_equal j.length, 10
  end
end

