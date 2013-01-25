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
 
class ProposalConfigModelTest < ActiveSupport::TestCase

  def create_new_prop
    b = Barclamp.find_by_name("crowbar")
    p = Proposal.find_by_name_and_barclamp_id("template", b.id)
    pc = p.current_config
    [b, p, pc, pc.deep_clone]
  end

  test "Barclamp/Proposal relations" do
    b, p, pc, pc_new = create_new_prop
    assert_equal p, pc.proposal
    assert_equal b, pc.barclamp
  end

  test "Deep Clone" do
    b, p, pc, pc_new = create_new_prop

    assert_not_equal pc.id, pc_new.id
    assert_equal pc.status, pc_new.status
    assert_equal pc.config, pc_new.config
    assert_equal pc.failed_reason, pc_new.failed_reason
  end

  test "update_node_roles" do
    b, p, pc, pc_new = create_new_prop

    node1 = Node.create!(:name => "fred.dell.com") 
    assert_equal "fred.dell.com", node1.name
    node2 = Node.create!(:name => "greg.dell.com") 
    assert_equal "greg.dell.com", node2.name

    pc_new.update_node_roles({})
    assert_equal true, pc_new.nodes.empty?
    assert_equal true, pc_new.roles.empty?

    pc_new.update_node_roles( { "bozo" => [ ] } )
    assert_equal true, pc_new.nodes.empty?
    assert_equal true, pc_new.roles.empty?

    pc_new.update_node_roles( { "bozo" => [ "clown" ] } )
    assert_equal true, pc_new.nodes.empty?
    assert_equal true, pc_new.roles.empty?

    pc_new.update_node_roles( { "crowbar" => [ ] } )
    assert_equal true, pc_new.nodes.empty?
    assert_equal true, pc_new.roles.empty?

    pc_new.update_node_roles( { "crowbar" => [ "clown" ] } )
    assert_equal true, pc_new.nodes.empty?
    assert_equal true, pc_new.roles.empty?

    pc_new.update_node_roles( { "crowbar" => [ "fred.dell.com" ] } )
    assert_equal false, pc_new.nodes.empty?
    assert_equal false, pc_new.roles.empty?
    assert_equal node1.name, pc_new.nodes.first.name
    assert_equal "crowbar", pc_new.roles.first.name

    pc_new.update_node_roles( { "crowbar" => [ "greg.dell.com" ] } )
    assert_equal false, pc_new.nodes.empty?
    assert_equal false, pc_new.roles.empty?
    assert_equal 1, pc_new.nodes.size
    assert_equal 1, pc_new.roles.size
    assert_equal node2.name, pc_new.nodes.first.name
    assert_equal "crowbar", pc_new.roles.first.name
  end

  test "Deep Clone with nodes" do
    b, p, pc, pc_new = create_new_prop

    node = Node.create!(:name => "fred.dell.com") 
    pc_new.update_node_roles( { "crowbar" => [ "fred.dell.com" ] } )

    pc_new2 = pc_new.deep_clone
    nrs = NodeRole.find_all_by_proposal_config_id(pc_new2.id)
    assert_equal 1, nrs.size
    assert_not_equal pc.id, pc_new2.id
    assert_equal pc.status, pc_new2.status
    assert_equal pc.config, pc_new2.config
    assert_equal pc.failed_reason, pc_new2.failed_reason
    assert_equal false, pc_new2.nodes.empty?
    assert_equal false, pc_new2.roles.empty?
    assert_equal node.name, pc_new2.nodes.first.name
    assert_equal "crowbar", pc_new2.roles.first.name
  end

  def status_test(pc, status, f, a, q, c)
    assert_equal status, pc.status
    assert_equal f, pc.failed?
    assert_equal a, pc.applied?
    assert_equal q, pc.queued?
    assert_equal c, pc.committing?
  end

  def set_status_reload(pc, status)
    pc.status = status
    assert_equal true, pc.save
    ProposalConfig.find_by_id(pc.id)
  end

  test "Status and Status functions" do 
    b, p, pc, pc_new = create_new_prop
  
    pc_new = set_status_reload(pc_new, ProposalConfig::STATUS_NONE)
    status_test(pc_new, ProposalConfig::STATUS_NONE, false, false, false, false)
    pc_new = set_status_reload(pc_new, ProposalConfig::STATUS_QUEUED)
    status_test(pc_new, ProposalConfig::STATUS_QUEUED, false, false, true, false)
    pc_new = set_status_reload(pc_new, ProposalConfig::STATUS_COMMITTING)
    status_test(pc_new, ProposalConfig::STATUS_COMMITTING, false, false, false, true)
    pc_new = set_status_reload(pc_new, ProposalConfig::STATUS_FAILED)
    status_test(pc_new, ProposalConfig::STATUS_FAILED, true, false, false, false)
    pc_new = set_status_reload(pc_new, ProposalConfig::STATUS_APPLIED)
    status_test(pc_new, ProposalConfig::STATUS_APPLIED, false, true, false, false)
  end

  test "Config Hash functions" do
    b, p, pc, pc_new = create_new_prop

    hash = {}
    pc_new.config_hash = hash
    assert_equal "{}", pc_new.config
    assert_equal hash, pc_new.config_hash
  end

  test "add_node_to_role" do
    b, p, pc, pc_new = create_new_prop

    node1 = Node.create!(:name => "fred.dell.com") 
    role1 = Role.find_by_name("crowbar")

    answer = pc_new.add_node_to_role(node1, nil)
    assert_equal false, answer
    answer = pc_new.add_node_to_role(nil, role1)
    assert_equal false, answer
    answer = pc_new.add_node_to_role(nil, nil)
    assert_equal false, answer

    answer = pc_new.add_node_to_role(node1, role1)
    assert_equal true, answer
    assert_equal 1, pc_new.node_roles.size
    assert_equal node1.name, pc_new.node_roles.first.node.name
    assert_equal role1.name, pc_new.node_roles.first.role.name

    answer = pc_new.add_node_to_role(node1, role1)
    assert_equal true, answer
    assert_equal 1, pc_new.node_roles.size
    assert_equal node1.name, pc_new.node_roles.first.node.name
    assert_equal role1.name, pc_new.node_roles.first.role.name
  end

  test "remove_node_from_role" do
    b, p, pc, pc_new = create_new_prop

    node1 = Node.create!(:name => "fred.dell.com") 
    node2 = Node.create!(:name => "greg.dell.com") 
    role1 = Role.find_by_name("crowbar")

    answer = pc_new.remove_node_from_role(nil, nil)
    assert_equal false, answer
    answer = pc_new.remove_node_from_role(node1, nil)
    assert_equal false, answer
    answer = pc_new.remove_node_from_role(nil, role1)
    assert_equal false, answer

    answer = pc_new.add_node_to_role(node1, role1)
    assert_equal true, answer
    assert_equal 1, pc_new.node_roles.size
    assert_equal node1.name, pc_new.node_roles.first.node.name
    assert_equal role1.name, pc_new.node_roles.first.role.name

    answer = pc_new.remove_node_from_role(node1, role1)
    assert_equal true, answer
    assert_equal 0, pc_new.node_roles.size
  end

  test "remove_all_nodes" do
    b, p, pc, pc_new = create_new_prop

    # Add both a node/role pair and a config data element.
    node1 = Node.create!(:name => "fred.dell.com") 
    role1 = Role.find_by_name("crowbar")
    answer = pc_new.add_node_to_role(node1, role1)
    assert_equal true, answer
    answer = pc_new.set_node_config_hash(node1, { "data" => true })
    assert_equal 2, pc_new.node_roles.size

    # Make sure both go away.
    pc_new.remove_all_nodes
    assert_equal 0, pc_new.node_roles.size
  end

  test "get_node_config_hash" do
    b, p, pc, pc_new = create_new_prop

    empty_hash = {}
    node1 = Node.create!(:name => "fred.dell.com") 
    role1 = Role.find_by_name("crowbar")

    answer = pc_new.get_node_config_hash(nil)
    assert_equal nil, answer

    answer = pc_new.get_node_config_hash(node1)
    assert_equal empty_hash, answer

    answer = pc_new.add_node_to_role(node1, role1)
    assert_equal true, answer
    assert_equal 1, pc_new.node_roles.size
    assert_equal node1.name, pc_new.node_roles.first.node.name
    assert_equal role1.name, pc_new.node_roles.first.role.name

    # default value should be empty hash
    answer = pc_new.get_node_config_hash(node1)
    assert_equal empty_hash, answer

    # set value and return it
    data = { "data" => true }
    answer = pc_new.set_node_config_hash(node1, data)
    assert_equal true, answer
    answer = pc_new.get_node_config_hash(node1)
    assert_equal data, answer

    # If config is nil, then return empty hash
    nr = NodeRole.find_by_node_id_and_proposal_config_id_and_role_id(node1.id, pc_new.id, nil)
    nr.config = nil
    nr.save
    answer = pc_new.get_node_config_hash(node1)
    assert_equal empty_hash, answer
  end
 
  test "set_node_config_hash" do
    b, p, pc, pc_new = create_new_prop

    node1 = Node.create!(:name => "fred.dell.com") 
    role1 = Role.find_by_name("crowbar")

    # Add a node/role pair so that we don't get the data confused
    answer = pc_new.add_node_to_role(node1, role1)
    assert_equal true, answer

    # Set something
    data = { "data" => true }
    answer = pc_new.set_node_config_hash(node1, { "data" => true })
    assert_equal true, answer
    answer = pc_new.get_node_config_hash(node1)
    assert_equal data, answer

    # remove role/node pair
    answer = pc_new.remove_node_from_role(node1, role1)
    assert_equal true, answer

    # Make sure data stays
    answer = pc_new.get_node_config_hash(node1)
    assert_equal data, answer
  end

  test "get_nodes_by_role" do
    b, p, pc, pc_new = create_new_prop
    answer = pc_new.get_nodes_by_role("fred")
    assert_equal [], answer

    answer = pc_new.get_nodes_by_role("crowbar")
    assert_equal [], answer

    # Add something to find
    node1 = Node.create!(:name => "fred.dell.com") 
    node2 = Node.create!(:name => "greg.dell.com") 
    role1 = Role.find_by_name("crowbar")
    answer = pc_new.add_node_to_role(node1, role1)
    assert_equal true, answer
    answer = pc_new.add_node_to_role(node2, role1)
    assert_equal true, answer

    answer = pc_new.get_nodes_by_role("crowbar")
    assert_equal [node1, node2], answer
  end

  test "get_nodes_by_roles" do
    b, p, pc, pc_new = create_new_prop
    answer = pc_new.get_nodes_by_role("fred")
    assert_equal [], answer

    empty_hash = {}

    answer = pc_new.get_nodes_by_roles
    assert_equal empty_hash, answer

    # Add something to find
    node1 = Node.create!(:name => "fred.dell.com") 
    node2 = Node.create!(:name => "greg.dell.com") 
    role1 = Role.find_by_name("crowbar")
    answer = pc_new.add_node_to_role(node1, role1)
    assert_equal true, answer
    answer = pc_new.add_node_to_role(node2, role1)
    assert_equal true, answer

    the_answer = { "crowbar" => [ node1, node2 ] }
    answer = pc_new.get_nodes_by_roles
    assert_equal the_answer, answer
  end

  test "to_proposal_object_hash" do
    b, p, pc, pc_new = create_new_prop

    data = { "data" => true }
    pc_new.config_hash = data

    hash = pc_new.to_proposal_object_hash

    assert_equal data, hash["attributes"]
    assert_equal p.description, hash["description"]
    assert_equal 0, hash["deployment"][b.name]["crowbar-revision"]
    assert_equal b.transitions, hash["deployment"][b.name]["config"]["transitions"]
    assert_equal b.mode, hash["deployment"][b.name]["config"]["mode"]
    assert_equal "crowbar-config-template", hash["deployment"][b.name]["config"]["environment"]

    # TODO: validate elements
    # TODO: validate element_order
    # TODO: validate element_states
  end

end

