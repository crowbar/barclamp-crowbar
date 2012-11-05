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
 
class CmdbModelTest < ActiveSupport::TestCase

  test "Unique Name" do
    Cmdb.create! :name=>"nodups"
    e = assert_raise(ActiveRecord::RecordInvalid, ActiveRecord::RecordNotUnique, SQLite3::ConstraintException) { Cmdb.create!(:name => "nodups") }
    assert_equal "Validation failed: Name Item must be un...", e.message.truncate(42)
    assert_raise(ActiveRecord::RecordInvalid, ActiveRecord::RecordNotUnique, SQLite3::ConstraintException) { b = Node.create! :name => "nodups" }
  end

  test "lower case required" do
    name = "THIS.ISALL.CAPS"
    c = Cmdb.create! :name=>name
    assert_not_equal c.name, name
    assert_equal c.name, name.downcase
    assert_raise(ActiveRecord::RecordInvalid, ActiveRecord::RecordInvalid, SQLite3::ConstraintException) { b = Cmdb.create! :name => name }
  end

  test "Naming Conventions" do
    assert_raise(ActiveRecord::RecordInvalid) { Cmdb.create!(:name=>"1123") }
    assert_raise(ActiveRecord::RecordInvalid) { Cmdb.create!(:name=>"1foo") }
    assert_raise(ActiveRecord::RecordInvalid) { Cmdb.create!(:name=>"Ille!gal")}
    assert_raise(ActiveRecord::RecordInvalid) { Cmdb.create!(:name=>" nospaces") }
    assert_raise(ActiveRecord::RecordInvalid) { Cmdb.create!(:name=>"no spaces") }
    assert_raise(ActiveRecord::RecordInvalid) { Cmdb.create!(:name=>"nospacesatall ") }
  end

  test "Must have override object - missing" do
     e = assert_raise(ActiveRecord::RecordInvalid, ActiveRecord::RecordNotUnique, SQLite3::ConstraintException) { Cmdb.create!(:name="foo") }
     assert_equal "Validation failed: Cannot create CMDB without matching class.", e.message
  end

  test "Must have override object - present" do
     c = Cmdb.create! :name="test"
     assert_not_null c
     assert_equal c.name, "test"
  end

end

