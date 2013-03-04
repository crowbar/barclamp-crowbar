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

class AttribModelTest < ActiveSupport::TestCase

  # tests the relationship between nodes and attributes
  def setup
    # setup node w/ attribute
    @value = "unit test"
    @crowbar = Barclamp.find_or_create_by_name :name=>"crowbar"
    @node = Node.find_or_create_by_name :name=>"units.example.com"
    @attrib = AttribType.add :name=>"unit_test"
    @na = @node.set_attrib @attrib, @value
    assert_not_nil @na
    assert_equal @value, @na.value
    # Ruby 1.8 and 1.9 raise different exceptions in this case, so handle it
    # accordingly. Simplify once we remove 1.8 support.
    @error_class = (RUBY_VERSION == '1.8.7') ? NameError : ArgumentError
  end
  
  test "Attrib Instances must attrib" do
    assert_raise(ActiveRecord::StatementInvalid) { Attrib.create :node_id=>@node.id, :attrib_type_id=>nil }
  end
  
  test "Attrib Instance can have no run" do
    a = AttribType.add :name=>"no_run"
    v = Attrib.create :node_id=>@node.id, :attrib_type_id=>a.id, :jig_run_id=>nil
    assert_not_nil v
  end
  
  test "Attrib Instance actual values state correct" do
    v = @node.get_attrib('state_test')
    assert_nil v.jig_run_id
    v.actual = @value
    assert_equal @value, v.actual
    assert_equal :ready, v.state
    assert_not_nil v.jig_run_id
  end
  
  test "Attrib Instance stores values state correct" do
    n = Node.create :name=>"pending.example.com"
    a = AttribType.create :name=>"unset"
    assert_not_nil a
    assert_not_nil n
    v = Attrib.create :node_id=>n.id, :attrib_type_id=>a.id
    assert_not_nil v
    v = Attrib.find v.id
    assert_instance_of Attrib::DEFAULT_CLASS, v
    assert_equal :empty, v.state
    assert_nil v.actual
    assert_equal v.class::MARSHAL_EMPTY, v.value_actual
    value = "2b"
    v.actual = value
    assert_equal value, v.value
    assert_equal :ready, v.state
  end
    
  test "Attrib Instance stores actual values" do
    value = "foo"
    v = @na
    assert_not_nil v
    v.actual = value
    assert_raise(NoMethodError) { v.value = value }
    assert_equal value, v.value
    v.save!
    n = Node.find @node.id
    assert_not_nil n
    assert n.attrib_types.count > 0
    assert n.attribs.count > 0
    na = n.attribs[0]
    assert_not_nil na
    assert_equal value, na.value
    assert_equal ActiveSupport::JSON.encode(value), na.value_actual
    assert_equal :ready, na.state
  end
  
  test "Attrib Instance removed when node deleted" do
    name = "chain-delete.example.com"
    attrib = "killme"
    n = Node.create :name=>name
    assert_not_nil n
    n.save
    na = n.get_attrib(attrib)
    assert_not_nil na
    id = na.id
    na2 = Attrib.find id 
    assert_not_nil na2
    
    assert n.destroy
    assert_raise(ActiveRecord::RecordNotFound) { Node.find n.id }
    assert_raise(ActiveRecord::RecordNotFound) { Attrib.find id }
  end
  
  test "Attrib Instance removed when attribute deleted" do
    name = "chain-delete.example.com"
    attrib = "killme"
    n = Node.create :name=>name
    assert_not_nil n
    n.save
    na = n.get_attrib(attrib)
    a = AttribType.add attrib
    assert_not_nil na
    id = na.id
    assert a.destroy
    assert_raise(ActiveRecord::RecordNotFound) { AttribType.find a.id }
    assert_raise(ActiveRecord::RecordNotFound) { AttribType.find id }
  end
    
  test "Attrib Instance find_or_create" do
    a = AttribType.add :name=>"busted"
    assert_not_nil a
    assert_raise(RuntimeError) { Attrib.find_or_create_by_attrib_type_and_node(nil, @node) }
    na = Attrib.find_or_create_by_attrib_type_and_node(@node, a)
    assert_not_nil na
  end

  test "Attrib Instance preserves type of actual Value" do
    value = "foo"
    type = value.class
    v = @na
    v.actual = value
    assert_equal value, v.actual
    assert_equal type, v.actual.class
    value = 123
    type = value.class
    v.actual = value
    assert_equal value, v.actual
    assert_equal type, v.actual.class
  end
  
  test "Attrib Instance preserves type of request Value" do
    value = "bar"
    type = value.class
    v = @na
    v.request = value
    assert_equal value, v.request
    assert_equal type, v.request.class
    value = 123
    type = value.class
    v.request = value
    assert_equal value, v.request
    assert_equal type, v.request.class
  end
  
  test "Node.attribute works" do
    value = "foo"
    v = @na
    v.actual = value
    v.save
    assert_equal value, @node.get_attrib(@attrib).value
  end
  
  test "Node can have attributes" do
    attrib = AttribType.add :name=>"foo"
    assert_not_nil attrib
    node = Node.find_or_create_by_name :name=>"bar.test.com"
    assert_not_nil node
    nbefore = node.attribs.length
    abefore = attrib.nodes.length
    na = node.get_attrib attrib
    # node has attributes
    assert_equal nbefore+1, node.attribs(true).length
    assert node.attribs.include? na
    # attribute has nodes
    assert_equal abefore+1, attrib.nodes(true).length
    assert attrib.nodes.include? node
  end
  
  test "Attrib can have nodes" do
    attrib = AttribType.add :name=>"foo2"
    assert_not_nil attrib
    node = Node.find_or_create_by_name :name=>"bar2.test.com"
    assert_not_nil node
    nbefore = node.attribs.length
    abefore = attrib.nodes.length
    a = node.get_attrib attrib
    # node has attributes
    assert_equal nbefore+1, node.attribs(true).length
    assert node.attribs.include? a
    # attribute has nodes
    assert_equal abefore+1, attrib.nodes(true).length
    assert attrib.nodes(true).include? node
  end
  
  test "Attrib remove node" do
    attrib = AttribType.find_or_create_by_name :name=>"foo3"
    assert_not_nil attrib
    node = Node.find_or_create_by_name :name=>"bar3.test.com"
    assert_not_nil node
    attrib.nodes << node
    n = Node.find_by_name "bar3.test.com"
    a = AttribType.add "foo3"
    assert a.nodes.include? n
    a.nodes.delete n
    n_after = Node.find_by_name "bar3.test.com"
    a_after = AttribType.find_by_name "foo3"
    assert !a_after.nodes.include?(n_after)
    assert !n_after.attribs.include?(a_after)
  end
  
  test "Node remove attribute" do
    attrib = AttribType.find_or_create_by_name :name=>"foo4"
    assert_not_nil attrib
    node = Node.find_or_create_by_name :name=>"bar4.test.com"
    assert_not_nil node
    attrib.nodes << node
    n = Node.find_by_name "bar4.test.com"
    a = n.get_attrib "foo4"
    assert_equal a.node_id, n.id
    n.attribs.delete a
    assert !n.attribs(true).include?(a)
    assert !n.get_attribs("foo4").include?(a)
  end
  
  test "Node stores proposed attrib and sets state" do
    value = "proposed value"
    name = "unit_proposed"
    na = @node.get_attrib(name)
    assert_not_nil na
    assert_equal name, na.name
    assert_nil na.value
    assert_equal :empty, na.state
    assert_nil na.request
    assert_nil na.jig_run_id
    na.request = value
    na.save
    assert_equal :unready, na.state
    assert_equal value, na.request
    assert_not_nil na.jig_run_id
        
    na2 = Attrib.find na.id
    assert_not_nil na2
    assert_equal nil, na2.value
    assert_equal :unready, na2.state
    assert_equal value, na2.request
    assert_not_nil na2.jig_run_id
  end
  
  test "Node state reflects proposed state" do
    value = "state value"
    name = "unit_state"
    na = @node.get_attrib(name)
    assert_not_nil na
    assert_equal name, na.name
    assert_equal nil, na.value
    assert_equal :empty, na.state
    assert_equal nil, na.request
    na.request = value
    assert_equal :unready, na.state
    assert_equal value, na.request
    assert_equal nil, na.value
    na.actual = value
    assert_equal :ready, na.state
    assert_equal value, na.value
    assert_equal value, na.actual
    assert_equal value, na.request
  end
  
end
