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

require 'spec_helper'

describe CrowbarUtils do
  describe "Locking functions" do
    it "should raise an exception if file create fails" do
      File.stub(:new) {nil}
      expect {
        CrowbarUtils.lock_held?("fred")
      }.to raise_error(IOError, "File not available: tmp/fred.lock")
    end

    it "lock_held? should return true iff the lock is held" do
      f1 = double('fred')
      f1.should_receive(:flock).and_return(false)
      File.stub(:new) {f1}
      f1.stub(:close)
      CrowbarUtils.lock_held?("fred").should be_true
    end
    it "lock_held? should return false iff the lock is not held" do
      f1 = double('fred')
      f1.should_receive(:flock).exactly(2).and_return(true,true)
      File.stub(:new) {f1}
      f1.stub(:close)
      CrowbarUtils.lock_held?("fred").should be_false
    end

    it "with_lock should yield when lock grabbed" do
      f1 = double('fred')
      f1.should_receive(:flock).exactly(2).and_return(true,true)
      File.stub(:new) {f1}
      f1.stub(:close)
      CrowbarUtils.with_lock("fred") do
        "fred"
      end.should == "fred"
    end

    it "with_lock should die if it cannot grab lock" do
      f1 = double('fred')
      f1.should_receive(:flock).exactly(32).and_return(false)
      File.stub(:new) {f1}
      Kernel.stub(:sleep)
      f1.stub(:close)
      expect {
        CrowbarUtils.with_lock("fred") do
          "fred"
        end
      }.to raise_error(RuntimeError, "Unable to grab fred lock -- Probable deadlock.")
    end
  end
end

