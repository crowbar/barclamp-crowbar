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

class CmdbRunModelTest < ActiveSupport::TestCase

  test "Unique Name" do
    CmdbRun.create! :name=>"nodups", :type=>"CmdbRunTest", :description=>"unit tests"
    e = assert_raise(ActiveRecord::RecordInvalid, ActiveRecord::RecordNotUnique, SQLite3::ConstraintException) { CmdbRun.create!(:name => "nodups") }
    assert_equal "Validation failed: Name Item must be un...", e.message.truncate(42)
    assert_raise(ActiveRecord::RecordInvalid, ActiveRecord::RecordNotUnique, SQLite3::ConstraintException) { b = Node.create! :name => "nodups" }
  end

  test "Naming Conventions" do
    assert_raise(ActiveRecord::RecordInvalid) { CmdbRun.create!(:name=>"1123", :type=>"CmdbRunTest")}
    assert_raise(ActiveRecord::RecordInvalid) { CmdbRun.create!(:name=>"1foo", :type=>"CmdbRunTest")}
    assert_raise(ActiveRecord::RecordInvalid) { CmdbRun.create!(:name=>"Ille!gal", :type=>"CmdbRunTest")}
    assert_raise(ActiveRecord::RecordInvalid) { CmdbRun.create!(:name=>" nospaces", :type=>"CmdbRunTest")}
    assert_raise(ActiveRecord::RecordInvalid) { CmdbRun.create!(:name=>"no spaces", :type=>"CmdbRunTest")}
    assert_raise(ActiveRecord::RecordInvalid) { CmdbRun.create!(:name=>"nospacesatall ", :type=>"CmdbRunTest")}
  end

  test "Must have override object-missing" do
     CmdbRun.create!(:name=>"foo", :type=>"CmdbRunFoo")
     e = assert_raise(ActiveRecord::SubclassNotFound) { CmdbRun.find_by_name "foo" }
     assert_equal "The single-table inheritance mechanism failed...", e.message.truncate(48)
  end

  test "Must have override object - present" do
     c = CmdbRun.create! :name=>"subclass", :type=>"CmdbRunTest"
     assert_equal false, c.nil?
     assert_equal c.name, "subclass"
     assert_equal c.type, "CmdbRunTest"
  end
  
  test "Returns correct objective type" do
    c = CmdbRun.create! :name=>"type_test", :type=>"CmdbRunTest"
    t = CmdbRun.find_by_name "type_test"
    assert_equal c.type, t.type
    assert_equal t.type, "CmdbRunTest"
    assert_kind_of CmdbRun, c
  end
  
  test "as_json routines returns correct items" do
    name = "json_test"
    type = "CmdbRunTest"
    status = "Done"
    result = "255"
    cmdb_event_id = 4
    description = "This is a unit test"
    c = CmdbRun.create! :name=>name, :type=>type, :description => description, :order => 100, :result => result, :status => status, :cmdb_event_id => cmdb_event_id
    j = JSON.parse(c.to_json)
    assert_equal j['type'], type
    assert_equal j['name'], name
    assert_equal j['description'], description
    assert_equal j['order'], 100
    assert_equal j['result'], result
    assert_equal j['status'], status
    assert_equal j['cmdb_event_id'], cmdb_event_id
    assert_not_nil j['created_at']
    assert_not_nil j['updated_at']
    assert_equal j.length, 10
  end
end

