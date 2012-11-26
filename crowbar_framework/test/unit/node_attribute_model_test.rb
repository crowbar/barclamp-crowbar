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

class NodeAttributeModelTest < ActiveSupport::TestCase

  # tests the relationship between nodes and attributes
  
  test "Node can have attributes" do
    attrib = Attribute.find_or_create_by_name :name=>"foo"
    assert_not_nil attrib
    node = Node.find_or_create_by_name :name=>"bar.test.com"
    assert_not_nil node
    nbefore = node.attribs.length
    abefore = attrib.nodes.length
    node.attribs << attrib
    # retrieve from cache
    n = Node.find_by_name "bar.test.com"
    a = Attribute.find_by_name "foo"
    # node has attributes
    assert_equal nbefore+1, n.attribs.length
    assert n.attribs.include? a
    # attribute has nodes
    assert_equal abefore+1, a.nodes.length
    assert a.nodes.include? n
  end
  
  test "Attribute can have nodes" do
    attrib = Attribute.find_or_create_by_name :name=>"foo2"
    assert_not_nil attrib
    node = Node.find_or_create_by_name :name=>"bar2.test.com"
    assert_not_nil node
    nbefore = node.attribs.length
    abefore = attrib.nodes.length
    attrib.nodes << node
    # retrieve from cache
    n = Node.find_by_name "bar2.test.com"
    a = Attribute.find_by_name "foo2"
    # node has attributes
    assert_equal nbefore+1, n.attribs.length
    assert n.attribs.include? a
    # attribute has nodes
    assert_equal abefore+1, a.nodes.length
    assert a.nodes.include? n
  end
  
  test "Attribute remove node" do
    attrib = Attribute.find_or_create_by_name :name=>"foo3"
    assert_not_nil attrib
    node = Node.find_or_create_by_name :name=>"bar3.test.com"
    assert_not_nil node
    attrib.nodes << node
    n = Node.find_by_name "bar3.test.com"
    a = Attribute.find_by_name "foo3"
    assert a.nodes.include? n
    a.nodes.delete n
    n_after = Node.find_by_name "bar3.test.com"
    a_after = Attribute.find_by_name "foo3"
    assert !a_after.nodes.include?(n_after)
    assert !n_after.attribs.include?(a_after)
  end
  
  test "Node remove attribute" do
    attrib = Attribute.find_or_create_by_name :name=>"foo4"
    assert_not_nil attrib
    node = Node.find_or_create_by_name :name=>"bar4.test.com"
    assert_not_nil node
    attrib.nodes << node
    n = Node.find_by_name "bar4.test.com"
    a = Attribute.find_by_name "foo4"
    assert a.nodes.include? n
    n.attribs.delete a
    n_after = Node.find_by_name "bar4.test.com"
    a_after = Attribute.find_by_name "foo4"
    assert !a_after.nodes.include?(n_after)
    assert !n_after.attribs.include?(a_after)
  end
end

