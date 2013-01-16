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
 
class BarclampNodeDataTest < ActiveSupport::TestCase

  def setup
    file = File.join 'test', 'data', 'barclamp_node_data_test.json'
    assert File.exist?(file), "source file #{file}"
    @sample = JSON::load File.open("#{file}", 'r')
    assert_not_nil @sample, "we have data"
    @node = @sample["name_for_test"]
    assert_not_nil @node, "for the exected node"
  end

  test "Use Hint to Extract Data" do
    bc = Barclamp.create_or_find_by_name :name=>'test'
    jig = CMDB.find_or_create_by_name :name=>'test', :type=>'CmdbTest'
    assert_equal @node, bc.find_attrib_in_data_from_jig(jig, @sample, "name_for_test")
    assert_equal "PERC H710", bc.find_attrib_in_data_from_jig(jig, @sample, "default_attributes/crowbar/disks/sda/model")
  end

  test "Barclamp Register creates attributes" do
    assert false, "test not created"
  end

  test "Barclamp run_data create mode data" do
    assert false, "test not created"
  end

end

