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

class NodeRoleModelTest < ActiveSupport::TestCase

  test "config_hash on new node_role should be {}" do
    nr = NodeRole.new
    assert_equal nr.config_hash, {}
  end

  test "assignment to config_hash should store as Json on config" do
    nr = NodeRole.new
    nr.config_hash = { "a" => "b"}
    assert_equal nr.config_hash, { "a" => "b" }
    assert_equal nr.config, { "a" => "b" }.to_json
  end

  test "assignment of nil to config_hash should store nil in config" do
    nr = NodeRole.new
    nr.config_hash = nil
    assert_equal nr.config_hash, {}
    assert_equal nr.config, nil
  end


end

