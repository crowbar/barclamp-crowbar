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

class AttribInstanceModelTest < ActiveSupport::TestCase

  # tests the relationship between nodes and attributes
  def setup
    # setup node w/ attribute
    @value = "unit test"
    @crowbar = Barclamp.find_or_create_by_name :name=>"crowbar"
    @node = Node.find_or_create_by_name :name=>"units.example.com"
    @attrib = Attrib.find_or_create_by_name :name=>"unit_test"
    @na = @node.set_attrib @attrib, @value
    assert_not_nil @na
    assert_equal @value, @na.value
    # Ruby 1.8 and 1.9 throws different exceptions in this case, so handle it
    # accordingly. Simplify once we remove 1.8 support.
    @error_class = (RUBY_VERSION == '1.8.7') ? NameError : ArgumentError
  end
  
  test "Attrib Instances must attrib" do
    assert_raise(ActiveRecord::StatementInvalid) { AttribInstance.create :node_id=>@node.id, :attrib_id=>nil }
  end
  
  test "Attrib Instance can have no run" do
    a = Attrib.create :name=>"no_run"
    v = AttribInstance.create :node_id=>@node.id, :attrib_id=>a.id, :jig_run_id=>nil
    assert_not_nil v
  end
  
  test "Attrib Instance actual values state correct" do
    v = @node.attrib_get('state_test')
    assert_nil v.jig_run_id
    v.actual = @value
    assert_equal @value, v.actual
    assert_equal :set, v.state
    assert_not_nil v.jig_run_id
  end
  
  test "Attrib Instance stores values state correct" do
    n = Node.create :name=>"pending.example.com"
    a = Attrib.create :name=>"unset"
    assert_not_nil a
    assert_not_nil n
    v = AttribInstance.create :node_id=>n.id, :attrib_id=>a.id
    assert_not_nil v
    v = AttribInstance.find v.id
    assert_instance_of AttribInstance::DEFAULT_CLASS, v
    assert_equal :empty, v.state
    assert_nil v.actual
    assert_equal v.class::MARSHAL_EMPTY, v.value_actual
    value = "2b"
    v.actual = value
    assert_equal value, v.value
    assert_equal :set, v.state
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
    assert n.attribs.count > 0
    assert n.attrib_instances.count > 0
    na = n.attrib_instances[0]
    assert_not_nil na
    assert_equal value, na.value
    assert_equal Marshal::dump(value), na.value_actual
    assert_equal :set, na.state
  end
  
  test "Attrib Instance removed when node deleted" do
    name = "chain-delete.example.com"
    attrib = "killme"
    n = Node.create :name=>name
    assert_not_nil n
    n.save
    na = n.attrib_get(attrib)
    assert_not_nil na
    id = na.id
    na2 = AttribInstance.find id 
    assert_not_nil na2
    
    assert n.destroy
    assert_raise(ActiveRecord::RecordNotFound) { Node.find n.id }
    assert_raise(ActiveRecord::RecordNotFound) { AttribInstance.find id }
  end
  
  test "Attrib Instance removed when attribute deleted" do
    name = "chain-delete.example.com"
    attrib = "killme"
    n = Node.create :name=>name
    assert_not_nil n
    n.save
    na = n.attrib_get(attrib)
    a = Attrib.find_by_name attrib
    assert_not_nil na
    id = na.id
    assert a.destroy
    assert_raise(ActiveRecord::RecordNotFound) { Attrib.find a.id }
    assert_raise(ActiveRecord::RecordNotFound) { AttribInstance.find id }
  end
    
  test "Attrib Instance find_or_create" do
    a = Attrib.create :name=>"busted"
    assert_not_nil a
    assert_raise(ActiveRecord::StatementInvalid) { AttribInstance.find_or_create_by_attrib_and_node(nil, @node) }
    na = AttribInstance.find_or_create_by_attrib_and_node(@node, a)
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
    assert_equal value, @node.attrib_get(@attrib.name).value
  end
  
  test "Node can have attributes" do
    attrib = Attrib.find_or_create_by_name :name=>"foo"
    assert_not_nil attrib
    node = Node.find_or_create_by_name :name=>"bar.test.com"
    assert_not_nil node
    nbefore = node.attribs.length
    abefore = attrib.nodes.length
    node.attribs << attrib
    # retrieve from cache
    n = Node.find_by_name "bar.test.com"
    a = Attrib.find_by_name "foo"
    # node has attributes
    assert_equal nbefore+1, n.attribs.length
    assert n.attribs.include? a
    # attribute has nodes
    assert_equal abefore+1, a.nodes.length
    assert a.nodes.include? n
  end
  
  test "Attrib can have nodes" do
    attrib = Attrib.find_or_create_by_name :name=>"foo2"
    assert_not_nil attrib
    node = Node.find_or_create_by_name :name=>"bar2.test.com"
    assert_not_nil node
    nbefore = node.attribs.length
    abefore = attrib.nodes.length
    attrib.nodes << node
    # retrieve from cache
    n = Node.find_by_name "bar2.test.com"
    a = Attrib.find_by_name "foo2"
    # node has attributes
    assert_equal nbefore+1, n.attribs.length
    assert n.attribs.include? a
    # attribute has nodes
    assert_equal abefore+1, a.nodes.length
    assert a.nodes.include? n
  end
  
  test "Attrib remove node" do
    attrib = Attrib.find_or_create_by_name :name=>"foo3"
    assert_not_nil attrib
    node = Node.find_or_create_by_name :name=>"bar3.test.com"
    assert_not_nil node
    attrib.nodes << node
    n = Node.find_by_name "bar3.test.com"
    a = Attrib.find_by_name "foo3"
    assert a.nodes.include? n
    a.nodes.delete n
    n_after = Node.find_by_name "bar3.test.com"
    a_after = Attrib.find_by_name "foo3"
    assert !a_after.nodes.include?(n_after)
    assert !n_after.attribs.include?(a_after)
  end
  
  test "Node remove attribute" do
    attrib = Attrib.find_or_create_by_name :name=>"foo4"
    assert_not_nil attrib
    node = Node.find_or_create_by_name :name=>"bar4.test.com"
    assert_not_nil node
    attrib.nodes << node
    n = Node.find_by_name "bar4.test.com"
    a = Attrib.find_by_name "foo4"
    assert a.nodes.include? n
    n.attribs.delete a
    n_after = Node.find_by_name "bar4.test.com"
    a_after = Attrib.find_by_name "foo4"
    assert !a_after.nodes.include?(n_after)
    assert !n_after.attribs.include?(a_after)
  end
  
  test "Node stores proposed attrib and sets state" do
    value = "proposed value"
    name = "unit_proposed"
    na = @node.attrib_get(name)
    assert_not_nil na
    assert_equal name, na.attrib.name
    assert_nil na.value
    assert_equal :empty, na.state
    assert_nil na.request
    assert_nil na.jig_run_id
    na.request = value
    na.save
    assert_equal :active, na.state
    assert_equal value, na.request
    assert_not_nil na.jig_run_id
        
    na2 = AttribInstance.find na.id
    assert_not_nil na2
    assert_equal nil, na2.value
    assert_equal :active, na2.state
    assert_equal value, na2.request
    assert_not_nil na2.jig_run_id
  end
  
  test "Node state reflects proposed state" do
    value = "state value"
    name = "unit_state"
    na = @node.attrib_get(name)
    assert_not_nil na
    assert_equal name, na.attrib.name
    assert_equal nil, na.value
    assert_equal :empty, na.state
    assert_equal nil, na.request
    na.request = value
    assert_equal :active, na.state
    assert_equal value, na.request
    assert_equal nil, na.value
    na.actual = value
    assert_equal :set, na.state
    assert_equal value, na.value
    assert_equal value, na.actual
    assert_equal value, na.request
  end
  
end

