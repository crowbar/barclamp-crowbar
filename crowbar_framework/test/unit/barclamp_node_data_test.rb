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
    assert_not_nil @node, "for the exected node"
  end

  test "Use Hint to Extract Data" do
    jig = CmdbTest.new
    assert_equal @node, Barclamp.find_attrib_in_data_from_jig(jig, @sample, "fqdn")
    assert_equal "To Be Filled By O.E.M.", Barclamp.find_attrib_in_data_from_jig(jig, @sample, "dmi/chassis/asset_tag")
  end

  test "Barclamp Register creates attributes" do
    assert true, "test not created"
  end

  test "Barclamp run_data create mode data" do
    assert true, "test not created"
  end

end

