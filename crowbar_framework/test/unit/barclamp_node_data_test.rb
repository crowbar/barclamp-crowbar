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
    file = File.join 'test', 'data', 'barclamp_node_data_test.json'
    assert File.exist?(file), "source file #{file}"
    @sample = JSON::load File.open("#{file}", 'r')
    assert_not_nil @sample, "we have data"
    assert_kind_of Chef::Node, @sample
    @node = @sample.name
    @mynode = Node.find_or_create_by_name :name=>@node
    assert_not_nil @node, "for the exected node"
  end

  test "Use Hint to Extract Data" do
    jig = JigTest.new
    assert_equal @node, jig.find_attrib_in_data(@sample, "fqdn")
    assert_equal "To Be Filled By O.E.M.", jig.find_attrib_in_data(@sample, "dmi/chassis/asset_tag")
  end

  test "Barclamp Register creates & stores attributes" do
    jig = JigTest.find_or_create_by_name :name=>'test'
    jig_run = jig.run
    bc = Barclamp.create :name=>"gimme_data"
    c = bc.attribs.count
    # add the attributes that we want to test
    a1 = bc.add_attrib "eth0", "crowbar_ohai/detected/network/eth0"  #expected "0000:00/0000:00:01.0/0000:01:00.0"
    assert_equal "eth0", a1.attrib.name
    assert_nil @mynode.attrib_get(a1.attrib.name).value
    assert_equal :empty, @mynode.attrib_get(a1.attrib.name).state
    a2 = bc.add_attrib "eth0_switch_name", "crowbar_ohai/switch_config/eth1/switch_name" # expected "00:25:64:2e:61:f6"
    assert_equal "eth0_switch_name", a2.attrib.name
    assert_nil @mynode.attrib_get(a2.attrib.name).value
    assert_equal :empty, @mynode.attrib_get(a2.attrib.name).state
    a3 = bc.add_attrib "serial_number", "dmi/base_board/serial_number" # ".HR74KN1.CN7475106U0180.   "
    assert_equal "serial_number", a3.attrib.name
    assert_equal c+3, bc.attribs(true).count
    assert_nil @mynode.attrib_get(a3.attrib.name).value
    assert_equal :empty, @mynode.attrib_get(a3.attrib.name).state

    node = bc.process_inbound_data jig_run, @mynode, @sample
    # values should be updated
    assert_equal "0000:00/0000:00:01.0/0000:01:00.0", node.attrib_eth0
    assert_equal :managed, node.attrib_get("eth0").state
    assert_equal "00:25:64:2e:61:f6", node.attrib_eth0_switch_name
    assert_equal :managed, node.attrib_get("eth0_switch_name").state
    assert_equal ".HR74KN1.CN7475106U0180.   ", node.attrib_serial_number
    assert_equal :managed, node.attrib_get("serial_number").state
    
  end

end

