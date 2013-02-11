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

class AttribInstanceHasNodeTest < ActiveSupport::TestCase

  HAS_NODE = "has_node"

  # tests the relationship between nodes and attributes
  def setup
    # setup node w/ attribute
    @crowbar = Barclamp.import_1x 'crowbar'
    assert_not_nil @crowbar
    assert_not_nil @crowbar.template
    assert_equal 2, @crowbar.template.role_instances.count
    @role = @crowbar.template.public_roles.first
    assert_not_nil @role
    
    @node1 = Node.find_or_create_by_name :name=>"units.example.com"
    @node2 = Node.find_or_create_by_name :name=>"tests.example.com"
    assert_not_nil @node1
    assert_not_nil @node2
    # manual assign
    @hasnode1 = BarclampCrowbar::AttribInstanceHasNode.create :role_instance_id=>@role.id, :node_id=>@node1.id
    assert_not_nil @hasnode1
    assert_instance_of BarclampCrowbar::AttribInstanceHasNode, @hasnode1
    assert @hasnode1.is_a? AttribInstance
    assert_equal HAS_NODE, @node1.attrib_instance_has_nodes.first.attrib.name
    assert_equal @role.id, @hasnode1.role_instance_id
    # Ruby 1.8 and 1.9 throws different exceptions in this case, so handle it
    # accordingly. Simplify once we remove 1.8 support.
    @error_class = (RUBY_VERSION == '1.8.7') ? NameError : ArgumentError
  end

  test "classes are right" do
    assert_equal BarclampCrowbar::AttribInstanceHasNode, Node::HAS_NODE_ROLE
  end
  test "special attrib is used correctly on create" do
    assert_instance_of BarclampCrowbar::AttribInstanceHasNode, @hasnode1
    a = @hasnode1.attrib
    assert_instance_of Attrib, a
    assert_equal HAS_NODE, a.name
    assert_equal I18n.t('model.attribs.role.has_node'), a.description
    assert_equal 999999, a.order
    assert_equal @role.id, @hasnode1.role_instance_id
  end
  
  test "attrib id is read only" do
    current_attrib = @hasnode1.attrib_id
    a = Attrib.add 'foo'
    @hasnode1.attrib_id = a.id
    assert_equal a.id, @hasnode1.attrib_id
    @hasnode1.save
    assert_equal current_attrib, @hasnode1.attrib_id
    assert_not_equal a.id, @hasnode1.attrib.id
  end
  
  test "assign role to node instance" do
    node3 = Node.find_or_create_by_name :name=>"assign.example.com"
    assert_not_nil node3
    assert_equal 0, node3.attrib_instance_has_nodes.count
    hasrole = node3.add_role @role
    assert_not_nil hasrole
    assert_equal 1, node3.attrib_instance_has_nodes.count
    assert_instance_of Node::HAS_NODE_ROLE, hasrole
    assert_equal HAS_NODE, hasrole.attrib.name
    assert_equal @role.id, hasrole.role_instance_id
    # and test unassign too
    node3.remove_role @role
    assert_equal 0, node3.attrib_instance_has_nodes.count
  end
    
  test "assign node instance to role" do
    count = @role.attrib_instance_has_nodes.count
    node3 = Node.find_or_create_by_name :name=>"assign2.example.com"
    assert_not_nil node3
    assert_equal 0, node3.attrib_instance_has_nodes.count
    hasrole = @role.add_node node3
    assert_not_nil hasrole
    assert_equal 1, node3.attrib_instance_has_nodes.count
    assert_equal count+1, @role.attrib_instance_has_nodes.count
    assert_instance_of Node::HAS_NODE_ROLE, hasrole
    assert_equal HAS_NODE, hasrole.attrib.name
    assert_equal @role.id, hasrole.role_instance_id
    # and test unassign too
    @role.remove_node node3
    assert_equal 0, node3.attrib_instance_has_nodes(true).count    
    assert_equal count, @role.attrib_instance_has_nodes(true).count
  end
  
end

