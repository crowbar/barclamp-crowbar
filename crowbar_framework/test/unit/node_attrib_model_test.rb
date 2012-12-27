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

class NodeAttribModelTest < ActiveSupport::TestCase

  # tests the relationship between nodes and attributes
  def setup
    # setup node w/ attribute
    @node = Node.create! :name=>"foo.example.com"
    @attrib = Attrib.create! :name=>"bar"
    @node.attribs << @attrib
    
    # to assign attribute, we need a CMDB event
    @cmdb = Cmdb.create! :name=>"unit", :type=>"CmdbTest"
    @run = @CmdbRun.create! :name=>"unittest1"
  end
  
  test "Node Attrib must have node" do
    NodeAttrib.create! :node_id=>nil, :attribute_id=>@attrib.id, :cmdb_run_id=>nil
    assert_fail
  end
  
  test "Node Attribs must have attrib" do
    NodeAttrib.create! :node_id=>@node.id, :attribute_id=>nil, :cmdb_run_id=>nil
    assert_fail
  end
  
  test "Node Attrib can have no run" do
    v = NodeAttrib.create! :node_id=>@node.id, :attribute_id=>@attrib.id, :cmdb_run_id=>nil
    assert_not_nil v
  end
  
  test "Node Attrib pending values state correct" do
    assert_fail
  end
  
  test "Node Attrib stores actual values" do
    value = "foo"
    v = NodeAttrib.create! :node_id=>@node.id, :attribute_id=>@attrib.id, :cmdb_run_id=>nil
    assert_not_nil v
    v.value = value
    assert_equal value, v.value
    v.save!
    n = Node.find @node.id
    assert_not_nil n
    a = Node.attribs[1]
    assert_not_nil a
    assert_equal value, a.value
    assert_equal Marshal::dump(value), a.value_serialized
    assert_equal :ready, a.state
  end
  

  test "Node Attrib preserves type of actual Value" do
    value = "foo"
    type = type_of(value)
    v = NodeAttrib.create! :node_id=>@node.id, :attribute_id=>@attrib.id, :cmdb_run_id=>nil
    v.actual = value
    assert_equal value, v.actual
    assert_equal type, type_of(v.actual)
    value = 123
    type = type_of(value)
    v.actual = value
    assert_equal value, v.actual
    assert_equal type, type_of(v.actual)
  end
  
  test "Node.attribute works" do
    value = "foo"
    v = NodeAttrib.create! :node_id=>@node.id, :attribute_id=>@attrib.id, :cmdb_run_id=>nil
    v.actual = value
    v.save!
    assert_equal value, @node.bar
    assert_equal value, @node.cmdb_get('bar')
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
end

