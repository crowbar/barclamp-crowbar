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
  context "reboot_done?" do
    it "should return true because boottime is higher than reboot requesttime" do
      RemoteNode.stubs(:ssh_cmd_get_boottime).returns(100).once
      RemoteNode.reboot_done?("localhost", 1).should eql true
    end

    it "should return false because boottime is lower than reboot requesttime" do
      RemoteNode.stubs(:ssh_cmd_get_boottime).returns(100).once
      RemoteNode.reboot_done?("localhost", 999).should eql false
    end

    it "should return false because boottime equal to reboot requesttime" do
      RemoteNode.stubs(:ssh_cmd_get_boottime).returns(10).once
      RemoteNode.reboot_done?("localhost", 10).should eql false
    end

    it "should return true because reboot requesttime is unknown" do
      RemoteNode.reboot_done?("localhost", 0).should eql true
    end
  end
  context "ssh get boottime" do
    it "should handle unsucessful ssh command" do
      RemoteNode.stubs(:ssh_cmd_base).returns(["false", "&&"]).once
      RemoteNode.ssh_cmd_get_boottime("localhost").should eql 0
    end

    it "should handle sucessful ssh command" do
      RemoteNode.stubs(:ssh_cmd_base).returns(["eval"]).once
      RemoteNode.ssh_cmd_get_boottime("localhost").should > 0
    end

    it "should handle invalid input from ssh command" do
      RemoteNode.stubs(:ssh_cmd_base).returns(["echo", "invalid", "#"]).once
      RemoteNode.ssh_cmd_get_boottime("localhost").should eql 0
    end
  end
end
