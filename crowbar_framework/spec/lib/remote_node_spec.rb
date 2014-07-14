#
# Copyright 2014, SUSE LINUX Products GmbH
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

require 'spec_helper'

describe RemoteNode do
  context "wait for uptime" do
    it "should timeout after many calls" do
      RemoteNode.stubs(:ssh_cmd_get_uptime).returns(*(1..100).to_a).times(31)
      RemoteNode.stubs(:sleep).times(30)
      RemoteNode.wait_for_uptime("localhost")
    end

    it "should return immediately because of unknown first uptime" do
      RemoteNode.stubs(:ssh_cmd_get_uptime).returns(*(0..10).to_a).times(1)
      RemoteNode.stubs(:sleep).times(0)
      RemoteNode.wait_for_uptime("localhost")
    end

    it "should return after a couple of uptime tests" do
      RemoteNode.stubs(:ssh_cmd_get_uptime).returns(10, 20, 1).times(3)
      RemoteNode.stubs(:sleep).times(1)
      RemoteNode.wait_for_uptime("localhost")
    end
  end
  context "ssh get uptime" do
    it "should handle unsucessful ssh command" do
      RemoteNode.stubs(:ssh_cmd_base).returns(["false", "&&"]).once
      RemoteNode.ssh_cmd_get_uptime("localhost").should eql 0
    end

    it "should handle sucessful ssh command" do
      RemoteNode.stubs(:ssh_cmd_base).returns([]).once
      RemoteNode.ssh_cmd_get_uptime("localhost").should > 0
    end

    it "should handle invalid input from ssh command" do
      RemoteNode.stubs(:ssh_cmd_base).returns(["echo", "invalid", "#"]).once
      RemoteNode.ssh_cmd_get_uptime("localhost").should eql 0
    end
  end
end
