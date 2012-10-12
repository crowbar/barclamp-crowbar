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

describe ServiceObject  do
  
  before(:each) do
    @service_object = ServiceObject.new(nil)
  end
  
  it "should have a logger" do
    @service_object.logger.should be Rails.logger
  end
  
  it "should have my logger" do
    so = ServiceObject.new(true)
    so.logger.should be true
  end
  
  it "should have a bc_name of unknown and barclamp is nil" do
    @service_object.bc_name.should eq("unknown")
    @service_object.barclamp.should be nil
  end
  
  it "should allow bc_name to be set and update barclamp" do
    @service_object.bc_name = "crowbar"
    @service_object.bc_name.should eq("crowbar")
    @service_object.barclamp.should be Barclamp.find_by_name("crowbar")
  end
  
  describe "should have a password generator" do
    it "should default to twelve character passwords" do
      string = @service_object.random_password
      string.length.should be 12
      string.should_not match /i1loO0/
    end
    
    it "should allow other lengths passwords" do
      string = @service_object.random_password 15
      string.length.should be 15
      string.should_not match /i1loO0/
    end
  end

  describe "API functions" do
    it "should respond to transition and default to success" do
      answer = @service_object.transition(nil, nil, nil)
      answer.should be_an_instance_of Array
      answer[0].should be 200
      answer[1].should eq("")
    end

  end

end

