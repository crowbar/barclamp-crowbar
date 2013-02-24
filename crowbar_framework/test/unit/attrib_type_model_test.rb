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

class AttribTypeModelTest < ActiveSupport::TestCase

 # tests the relationship between nodes and attributes)
  def setup
    @crowbar = Barclamp.find_or_create_by_name :name=>"crowbar"
  end  
    
  test "Attribs exposed add hash" do
    name = "attrs_hash"
    description = "Attrib test"
    order = 666
    a = AttribType.add({:name=>name, :description=>description, :order=>order}, "foo")
    assert_not_nil a
    assert_equal name, a.name
    assert_equal description, a.description
    assert_equal order, a.order
  end

  test "Attribs exposed add simple" do
    name = "attrs_simple"
    order = 666
    a = AttribType.add name, "foo"
    assert_not_nil a
    assert_equal name, a.name
    assert a.description =~ /foo/
  end
  
  test "Unique Name" do
    AttribType.create! :name=>"nodups", :description=>"unit tests"
    e = assert_raise(ActiveRecord::RecordInvalid, ActiveRecord::RecordNotUnique, SQLite3::ConstraintException) { AttribType.create!(:name => "nodups") }
    assert_equal "Validation failed: Name Item must be un...", e.message.truncate(42)
  end

  test "Naming Conventions" do
    assert_raise(ActiveRecord::RecordInvalid) { AttribType.create!(:name=>"1123" )}
    assert_raise(ActiveRecord::RecordInvalid) { AttribType.create!(:name=>"1foo" )}
    assert_raise(ActiveRecord::RecordInvalid) { AttribType.create!(:name=>"Ille!gal" )}
    assert_raise(ActiveRecord::RecordInvalid) { AttribType.create!(:name=>" nospaces" )}
    assert_raise(ActiveRecord::RecordInvalid) { AttribType.create!(:name=>"no spaces" )}
    assert_raise(ActiveRecord::RecordInvalid) { AttribType.create!(:name=>"nospacesatall " )}
  end
  
  test "as_json routines returns correct items" do
    name = "json_test"
    description = "This is a unit test"
    c = AttribType.create! :name=>name, :description => description, :order => 100
    j = JSON.parse(c.to_json)
    assert_equal j['name'], name
    assert_equal j['description'], description
    assert_equal 100, j['order']
    assert_not_nil j['created_at']
    assert_not_nil j['updated_at']
    assert_equal 6, j.length
  end
  
end

