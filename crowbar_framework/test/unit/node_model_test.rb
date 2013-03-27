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
 
class NodeModelTest < ActiveSupport::TestCase

  def setup
    @crowbar = Barclamp.import_1x "crowbar"
  end


  test "Unique Name" do
    Node.create! :name=>"foo.example.com"
    e = assert_raise(ActiveRecord::RecordInvalid, ActiveRecord::RecordNotUnique, SQLite3::ConstraintException) { Node.create!(:name => "foo.example.com") }
    assert_equal "Validation failed: Name Item must be un...", e.message.truncate(42)

    assert_raise(ActiveRecord::RecordInvalid, ActiveRecord::RecordNotUnique, SQLite3::ConstraintException) { b = Node.create! :name => "foo.example.com" }
  end

  test "name too long" do
    assert_raise(ActiveRecord::RecordInvalid, SQLite3::ConstraintException) { Node.create!(:name=>"12345678901234567890123456789012345678901234567890.12345678901234567890123456789012345678901234567890.12345678901234567890123456789012345678901234567890.12345678901234567890123456789012345678901234567890.12345678901234567890123456789012345678901234567890.com") }
  end

  test "state unknown" do
    n = Node.create! :name=>"unknown.node.com"
    assert_not_nil n, "created node"
    assert_equal 'empty', n.state
    assert_equal Node::UNKNOWN, n.state_attrib.state
  end
  
  test "lower case required" do
    name = "THIS.ISALL.CAPS"
    n = Node.create! :name=>name
    assert_not_equal n.name, name
    assert_equal n.name, name.downcase
    assert_raise(ActiveRecord::RecordInvalid, ActiveRecord::RecordInvalid, SQLite3::ConstraintException) { b = Node.create! :name => name }
  end
  
  test "not set group" do
    n = Node.create! :name=>"not-set.example.com"
    assert_not_nil n
    g = Group.find_by_name "not_set"
    assert_not_nil g
    assert_equal g.name, "not_set"
    assert_equal n.groups.size, 1
    assert_equal n.groups[0].id, g.id
  end
  
  test "Naming Conventions" do
    assert_raise(ActiveRecord::RecordInvalid, SQLite3::ConstraintException) { Node.create!(:name=>"fqdnrequired") }
    assert_raise(ActiveRecord::RecordInvalid, SQLite3::ConstraintException) { Node.create!(:name=>"1no.legal.domain") }
    assert_raise(ActiveRecord::RecordInvalid, SQLite3::ConstraintException) { Node.create!(:name=>"1123.foo.com") }
    assert_raise(ActiveRecord::RecordInvalid, SQLite3::ConstraintException) { Node.create!(:name=>"1foo.bar.net") }
    assert_raise(ActiveRecord::RecordInvalid, SQLite3::ConstraintException) { Node.create!(:name=>"Ille!gal.foo.org") }
    assert_raise(ActiveRecord::RecordInvalid, SQLite3::ConstraintException) { Node.create!(:name=>" nospaces.bar.it") }
    assert_raise(ActiveRecord::RecordInvalid, SQLite3::ConstraintException) { Node.create!(:name=>"no spaces.dell.com") }
    assert_raise(ActiveRecord::RecordInvalid, SQLite3::ConstraintException) { Node.create!(:name=>"nospacesatall.end.edu ") }
    assert_raise(ActiveRecord::RecordInvalid, SQLite3::ConstraintException) { Node.create!(:name=>"musthaveatleastthreedomains.com") }
  end

  test "Get Attribute for existing attribute gets value" do
    name = "foo"
    value = "bar"
    description = "unit test"
    n = Node.create :name=>"oldattribute.example.com"
    a = AttribType.add :name=>name, :description=>description
    na = n.get_attrib(a)
    assert_equal nil, na.value
    assert_equal :empty, na.state
    na.actual = value
    assert_equal value, na.value
    assert_equal :ready, na.state
    na.save
    v = Node.find(n.id).get_attrib(name)
    assert_equal name, v.attrib_type.name
    assert_equal description, v.attrib_type.description
    assert_equal value, v.value
    assert_equal :ready, v.state
  end


  test "Get Attrib for new attribute creates it" do
    name = "foo"
    node = "attrib.example.com"
    n = Node.create :name=>node
    assert_not_nil n
    a = n.get_attrib(name)
    assert_not_nil a
    assert_equal I18n.t('model.attribs.barclamp.default_create_description', :barclamp=>'unknown'), a.attrib_type.description
    assert_nil a.value
    attrib = AttribType.find_by_name name
    assert_equal name, attrib.name
    assert_equal I18n.t('model.attribs.barclamp.default_create_description', :barclamp=>'unknown'), attrib.description
    assert_equal nil, a.value
    assert_equal :empty, a.state
  end

  test "attribs without role get set as user defined" do
    name = "annie"
    node = "daddy.warbucks.com"
    n = Node.create :name=>node
    assert_not_nil n
    a = n.get_attrib(name)
    assert_not_nil a
    assert_equal name, a.attrib_type.name
    assert_equal "private", a.role.name
    assert_equal n.id, a.node_id
    assert_equal @crowbar.id, a.barclamp.id
  end
  
end

