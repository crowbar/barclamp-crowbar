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
    @service_object.barclamp.should be_an_instance_of Crowbar::BarclampCrowbar
    @service_object.barclamp.name.should eq("crowbar")
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

  describe "Create Proposal" do
    it "should fail with 400 and message if parms is nil" do
      answer = @service_object.proposal_create(nil)
      answer.should be_an_instance_of Array
      answer[0].should be 400
      answer[1].should eq(I18n.t('model.service.empty_parameters'))
    end
    it "should fail with 400 and message if parms is missing id" do
      answer = @service_object.proposal_create({})
      answer.should be_an_instance_of Array
      answer[0].should be 400
      answer[1].should eq(I18n.t('model.service.missing_id'))
    end
    it "should fail with 400 and message if proposal name is empty" do
      answer = @service_object.proposal_create({"id" => ""})
      answer.should be_an_instance_of Array
      answer[0].should be 400
      answer[1].should eq(I18n.t('model.service.too_short'))
    end
    def name_test_failure(name)
      answer = @service_object.proposal_create({"id" => name})
      answer.should be_an_instance_of Array
      answer[0].should be 400
      answer[1].should eq(I18n.t('model.service.illegal_chars', :name => name))
    end
    it "should fail with 400 and message if proposal name is illegal" do
      name_test_failure("-=asg")
      name_test_failure("s g")
      name_test_failure("0012asdg")
    end
    it "should fail with exception if raw service_object is used" do
      lambda {@service_object.proposal_create({"id" => "fred"})}.should raise_error(CrowbarException)
    end
    it "should fail with 400 and message if prop already exists" do
      name = "fred"
      cb = Barclamp.find_by_name("crowbar")
      p = Proposal.new
      p.name = name
      p.barclamp = cb
      p.save!

      @service_object.bc_name = "crowbar"
      answer = @service_object.proposal_create({"id" => name})
      answer.should be_an_instance_of Array
      answer[0].should be 400
      answer[1].should eq(I18n.t('model.service.name_exists'))
    end
    it "should create a proposal with base data" do
      ProposalObject.stub(:write) { [200, ""] }
      @service_object.bc_name = "crowbar"
      answer = @service_object.proposal_create({"id" => "fred"})
      answer.should be_an_instance_of Array
      answer[0].should be 200
      answer[1].should eq("")
      p = Proposal.find_by_name("fred")
      p.name.should eq("fred")
      p.barclamp.name.should eq("crowbar")
      Proposal.find_all_by_barclamp_id(Barclamp.find_by_name("crowbar").id).length.should be 2
    end
  end

  describe "Apply Role" do
  end

end




