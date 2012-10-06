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
    it "should acquire should throw an exception if file create fails" do
      File.stub(:new) {nil}
      lambda {CrowbarUtils.acquire_lock("fred")}.should raise_error(IOError, "File not available: tmp/fred.lock")
    end

    it "acquire should sleep if lock is not available" do
      f1 = mock(File)
      f1.should_receive(:flock).exactly(2).and_return(false, true)
      File.stub(:new) {f1}
      CrowbarUtils.stub!(:sleep)
      CrowbarUtils.should_receive(:sleep).exactly(1).times
      f = CrowbarUtils.acquire_lock("fred")
      f.should be f1
    end

    it "acquire should not sleep if lock is available" do
      f1 = mock(File)
      f1.should_receive(:flock).exactly(1).and_return(true)
      File.stub(:new) {f1}
      CrowbarUtils.stub!(:sleep)
      CrowbarUtils.should_receive(:sleep).exactly(0).times
      f = CrowbarUtils.acquire_lock("fred")
      f.should be f1
    end

    it "release should throw exception on nil file" do
      lambda {CrowbarUtils.release_lock(nil)}.should raise_error(IOError, "Invalid file")
    end

    it "release should call unlock and close" do
      f1 = mock(File)
      f1.should_receive(:flock).exactly(1).and_return(true)
      f1.should_receive(:close).exactly(1).and_return(true)
      CrowbarUtils.release_lock(f1)
    end

  end

end

