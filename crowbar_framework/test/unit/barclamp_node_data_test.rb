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
require 'chef'
 
class BarclampNodeDataTest < ActiveSupport::TestCase

  def setup
    BarclampCrowbar::Jig.find_or_create_by_name :name=>'chef'
    file = File.join 'test', 'data', 'barclamp_node_data_test.json'
    assert File.exist?(file), "source file #{file}"
    @sample = JSON::load File.open("#{file}", 'r')
    assert_not_nil @sample, "we have data"
    assert_kind_of Chef::Node, @sample
    @node = @sample.name
    @mynode = Node.find_or_create_by_name :name=>@node
    assert_not_nil @node, "for the exected node"
    @bc = Barclamp.create :name=>"node_data"
    @template = Snapshot.create :name=>'node template test', :barclamp_id=>@bc.id
    assert_equal 0, @template.roles.count
    @role = @template.add_role 'bndt'
    assert_equal 1, @template.roles(true).count
    @template.add_role 'private'
    assert_equal 2, @template.roles(true).count
    @bc.template_id = @template.id
    @bc.save    
    assert_equal 2, @bc.template.role_types(true).count
    assert_equal 2, @bc.roles.count
    assert_equal 'private', @bc.template.role_types(true).first.name
    assert_equal 'bndt', @bc.template.role_types(true).second.name
  end

  test "we have the expected jigs" do
    assert_not_nil Jig.find_by_name('test'), "test jig exists"
    assert_not_nil Jig.find_by_name 'chef', "chef jig exists"
    count = 2
    assert_equal count, Jig.count, "we should have two chef (chef & admin_chef) and test jig"
  end
  
  test "Use Hint to Extract Data" do
    jig = BarclampCrowbar::Jig.new
    assert_equal @node, jig.find_attrib_in_data(@sample, "fqdn")
    assert_equal "To Be Filled By O.E.M.", jig.find_attrib_in_data(@sample, "dmi/chassis/asset_tag")
  end

  test "Barclamp add attrib creates correct Snapshots" do
    role_count = @bc.template.roles.count
    assert_equal 2, role_count
    assert_equal "private", @bc.template.roles.first.name
    assert_equal "bndt", @bc.template.roles.second.name
    
    # add the attributes that we want to test
    a1 = @bc.add_attrib "eth0", "crowbar_ohai/detected/network/eth0", @role  #expected "0000:00/0000:00:01.0/0000:01:00.0"
    assert_equal "eth0", a1.attrib_type.name
    assert_equal "bndt", a1.role.name
    assert_nil a1.node_id
    an = @mynode.get_attrib("eth0")
    assert_not_equal a1.id, an.id
    assert_not_equal a1.node_id, an.node_id
    assert_equal a1.role_id, an.role_id
    assert_equal a1.attrib_type_id, an.attrib_type_id
    assert_nil an.value
    assert_equal :ready, @mynode.get_attrib(a1).state
  end
  
  test "Barclamp Register creates & stores attributes" do
    jig = BarclampCrowbar::Jig.find_or_create_by_name :name=>'test'
    jig_run = jig.run
    bc = @bc 
    c = bc.template.roles.first.attribs(true).count

    assert_equal "test", Jig.find(1).name
    assert_equal 2, BarclampCrowbar::Jig.count, "we should have the test and chef jig"
    assert_equal 0, @bc.jig_maps.count, "no jig mappings"
    
    # add the attributes that we want to test
    a1 = bc.add_attrib "eth0", "crowbar_ohai/detected/network/eth0"  #expected "0000:00/0000:00:01.0/0000:01:00.0"
    assert_equal "eth0", a1.name
    assert_equal "crowbar_ohai/detected/network/eth0", JigMap.get_map("chef","node_data", "eth0").map

    a2 = bc.add_attrib "eth0_switch_name", "crowbar_ohai/switch_config/eth1/switch_name" # expected "00:25:64:2e:61:f6"
    assert_equal "eth0_switch_name", a2.name
    assert_equal "crowbar_ohai/switch_config/eth1/switch_name", JigMap.get_map("chef","node_data", "eth0_switch_name").map

    a3 = bc.add_attrib "serial_number", "dmi/base_board/serial_number" # ".HR74KN1.CN7475106U0180.   "
    assert_equal "serial_number", a3.name
    assert_equal "dmi/base_board/serial_number", JigMap.get_map("chef","node_data", "serial_number").map

    assert_equal 2, @bc.template.roles(true).count
    assert_equal 3, @bc.template.roles.first.attribs.count, "this is the roles before nodes are assigned"

    assert_equal Jig.count*3, @bc.jig_maps.count, "this is the jig mappings"

    assert_nil @mynode.get_attrib(a1.name).value
    assert_equal :ready, @mynode.get_attrib(a1).state
    assert_nil @mynode.get_attrib(a2.name).value
    assert_equal :ready, @mynode.get_attrib(a2.name).state
    assert_nil @mynode.get_attrib(a3.name).value
    assert_equal :ready, @mynode.get_attrib(a3.name).state

    assert_equal 6, @bc.template.roles.first.attribs.count, "this is the roles after nodes are assigned"
    
    assert_equal "private", @bc.template.roles.first.name
    assert_equal "bndt", @bc.template.roles.second.name

    node = bc.process_inbound_data jig_run, @mynode, @sample
    # values should be updated
    assert_equal "0000:00/0000:00:01.0/0000:01:00.0", node.attrib_eth0
    assert_equal :ready, node.get_attrib("eth0").state
    assert_equal "00:25:64:2e:61:f6", node.attrib_eth0_switch_name
    assert_equal :ready, node.get_attrib("eth0_switch_name").state
    assert_equal ".HR74KN1.CN7475106U0180.   ", node.attrib_serial_number
    assert_equal :ready, node.get_attrib("serial_number").state
    
  end

end

