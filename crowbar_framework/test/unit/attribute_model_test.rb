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

class AttributeModelTest < ActiveSupport::TestCase

  test "Attributes exposed" do
    name = "attrs"
    description = "attribute test"
    order = 666
    a = Attribute.create :name=>name, :description=>description, :order=>order
    assert_not_nil a
    assert_equal name, a.name
    assert_equal description, a.description
    assert_equal order, a.order
  end
  
  test "Unique Name" do
    Attribute.create! :name=>"nodups", :description=>"unit tests"
    e = assert_raise(ActiveRecord::RecordInvalid, ActiveRecord::RecordNotUnique, SQLite3::ConstraintException) { Attribute.create!(:name => "nodups") }
    assert_equal "Validation failed: Name Item must be un...", e.message.truncate(42)
  end

  test "Naming Conventions" do
    assert_raise(ActiveRecord::RecordInvalid) { Attribute.create!(:name=>"1123" )}
    assert_raise(ActiveRecord::RecordInvalid) { Attribute.create!(:name=>"1foo" )}
    assert_raise(ActiveRecord::RecordInvalid) { Attribute.create!(:name=>"Ille!gal" )}
    assert_raise(ActiveRecord::RecordInvalid) { Attribute.create!(:name=>" nospaces" )}
    assert_raise(ActiveRecord::RecordInvalid) { Attribute.create!(:name=>"no spaces" )}
    assert_raise(ActiveRecord::RecordInvalid) { Attribute.create!(:name=>"nospacesatall " )}
  end
  
  test "as_json routines returns correct items" do
    name = "json_test"
    description = "This is a unit test"
    c = Attribute.create! :name=>name, :description => description, :order => 100
    j = JSON.parse(c.to_json)
    assert_equal j['name'], name
    assert_equal j['description'], description
    assert_equal 100, j['order']
    assert_not_nil j['created_at']
    assert_not_nil j['updated_at']
    assert_equal 6, j.length
  end
  
end

