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

describe ProposalQueue do

  # Always make sure that acquire_lock and release_lock are called
  def validate_locking
    CrowbarUtils.stub!(:acquire_lock)
    CrowbarUtils.should_receive(:acquire_lock).exactly(1).times.and_return(:lock)
    CrowbarUtils.stub!(:require_lock)
    CrowbarUtils.should_receive(:release_lock).exactly(1).times do |arg|
      arg.should be :lock
    end
  end

  describe "Self.get_gueue" do
    it "should create a queue if one doesn't exist" do
      ProposalQueue.all.empty?.should be true
      ProposalQueue.all.size.should be 0
      q = ProposalQueue.get_queue("greg", :log)
      q.name.should eq("greg")
      q.logger.should be :log
      ProposalQueue.all.empty?.should be false
      ProposalQueue.all.first.id.should eq(q.id)
    end

    it "should create a queue if one doesn't exist" do
      nq = ProposalQueue.new
      nq.name = "greg"
      nq.save!

      ProposalQueue.all.empty?.should be false
      ProposalQueue.all.size.should be 1
      q = ProposalQueue.get_queue("greg", :log)
      q.name.should eq("greg")
      q.logger.should be :log
      ProposalQueue.all.empty?.should be false
      ProposalQueue.all.size.should be 1
      ProposalQueue.all.first.id.should eq(q.id)
      q.id.should eq(nq.id)
    end
  end

  describe "Self.update_proposal_status" do
    it "should return true if proposal is not active" do
      mock_p = mock(Proposal)
      mock_p.should_receive(:active?).exactly(1).times.and_return(false)
      mock_p.should_receive(:active_config).exactly(0).times
      mock_pc = mock(ProposalConfig)
      mock_pc.should_receive(:proposal).exactly(1).times.and_return(mock_p)
      mock_pc.should_receive(:status=).exactly(0).times
      mock_pc.should_receive(:failed_reason=).exactly(0).times
      mock_pc.should_receive(:save).exactly(0).times

      answer = ProposalQueue.update_proposal_status(mock_pc, "status", "message")
      answer.should be true
    end

    it "should return true if proposal is active and set status and message" do
      mock_p = mock(Proposal)
      mock_p.should_receive(:active?).exactly(1).times.and_return(true)
      mock_pc = mock(ProposalConfig)
      mock_pc.should_receive(:proposal).exactly(1).times.and_return(mock_p)
      mock_pc.should_receive(:status=).exactly(1).times do |arg|
        arg.should eq("status")
      end
      mock_pc.should_receive(:failed_reason=).exactly(1).times do |arg|
        arg.should eq("message")
      end
      mock_pc.should_receive(:save).exactly(1).times.and_return(true)
      mock_p.should_receive(:active_config).exactly(3).times.and_return(mock_pc)

      answer = ProposalQueue.update_proposal_status(mock_pc, "status", "message")
      answer.should be true
    end

    it "should return false if proposal is active and set status and message and save fails" do
      mock_p = mock(Proposal)
      mock_p.should_receive(:active?).exactly(1).times.and_return(true)
      mock_pc = mock(ProposalConfig)
      mock_pc.should_receive(:proposal).exactly(1).times.and_return(mock_p)
      mock_pc.should_receive(:status=).exactly(1).times do |arg|
        arg.should eq("status")
      end
      mock_pc.should_receive(:failed_reason=).exactly(1).times do |arg|
        arg.should eq("message")
      end
      mock_pc.should_receive(:save).exactly(1).times.and_return(false)
      mock_p.should_receive(:active_config).exactly(3).times.and_return(mock_pc)

      answer = ProposalQueue.update_proposal_status(mock_pc, "status", "message")
      answer.should be false
    end
  end

  describe "Self.elements_not_ready" do
    it "should return an empty list if nodes is empty" do
      answer = ProposalQueue.elements_not_ready([])
      answer.should eq([])
    end
    it "should return an empty list if nodes is nil" do
      answer = ProposalQueue.elements_not_ready(nil)
      answer.should eq([])
    end
    it "should return an empty list if nodes are ready" do
      n1 = mock(Node)
      n1.should_receive(:state).exactly(1).times.and_return("ready")
      n1.should_receive(:name).exactly(0).times
      n2 = mock(Node)
      n2.should_receive(:state).exactly(1).times.and_return("ready")
      n2.should_receive(:name).exactly(0).times
      answer = ProposalQueue.elements_not_ready([n1,n2])
      answer.should eq([])
    end
    it "should return a list of not ready nodes" do
      n1 = mock(Node)
      n1.should_receive(:state).exactly(1).times.and_return("ready")
      n1.should_receive(:name).exactly(0).times
      n2 = mock(Node)
      n2.should_receive(:state).exactly(1).times.and_return("unready")
      n2.should_receive(:name).exactly(1).times.and_return("n2")
      n3 = mock(Node)
      n3.should_receive(:state).exactly(1).times.and_return("unready")
      n3.should_receive(:name).exactly(1).times.and_return("n3")
      answer = ProposalQueue.elements_not_ready([n1,n2,n3])
      answer.should eq(["Node n2", "Node n3"])
    end
  end

  describe "make_applying_or_delay" do
    it "should return an empty list if an nil node list is given regardless of apply" do
      answer = ProposalQueue.make_applying_or_delay(nil, false)
      answer.should eq([])
      answer = ProposalQueue.make_applying_or_delay(nil, true)
      answer.should eq([])
    end

    def make_applying_or_delay_empty_list(apply)
      validate_locking
      answer = ProposalQueue.make_applying_or_delay([], apply)
      answer.should eq([])
    end
    it "should return an empty list if an empty node list is given if apply" do
      make_applying_or_delay_empty_list(true)
    end
    it "should return an empty list if an empty node list is given if not apply" do
      make_applying_or_delay_empty_list(false)
    end

    it "should return an empty list and not touch the nodes if apply is false and nodes are ready" do
      validate_locking

      n1 = mock(Node)
      n1.should_receive(:state).exactly(1).times.and_return("ready")
      n1.should_receive(:name).exactly(0).times
      n1.should_receive(:set_state).exactly(0).times
      n1.should_receive(:allocate).exactly(0).times

      answer = ProposalQueue.make_applying_or_delay([n1], false)
      answer.should eq([])
    end
    it "should return an empty list and set_state of nodes if apply and nodes are ready" do
      validate_locking

      n1 = mock(Node)
      n1.should_receive(:state).exactly(1).times.and_return("ready")
      n1.should_receive(:name).exactly(0).times
      n1.should_receive(:set_state).exactly(1).times do |arg|
        arg.should eq("applying")
      end
      n1.should_receive(:allocate).exactly(0).times
      answer = ProposalQueue.make_applying_or_delay([n1], true)
      answer.should eq([])
    end
    it "should return an empty list and set_state of nodes if apply and nodes (more than one) are ready" do
      validate_locking

      n1 = mock(Node)
      n1.should_receive(:state).exactly(1).times.and_return("ready")
      n1.should_receive(:name).exactly(0).times
      n1.should_receive(:set_state).exactly(1).times do |arg|
        arg.should eq("applying")
      end
      n1.should_receive(:allocate).exactly(0).times
      n2 = mock(Node)
      n2.should_receive(:state).exactly(1).times.and_return("ready")
      n2.should_receive(:name).exactly(0).times
      n2.should_receive(:set_state).exactly(1).times do |arg|
        arg.should eq("applying")
      end
      n2.should_receive(:allocate).exactly(0).times
      answer = ProposalQueue.make_applying_or_delay([n1,n2], true)
      answer.should eq([])
    end

    def make_applying_or_delay(apply)
      validate_locking

      n1 = mock(Node)
      n1.should_receive(:state).exactly(1).times.and_return("unready")
      n1.should_receive(:name).exactly(1).times.and_return("n1")
      n1.should_receive(:set_state).exactly(0).times 
      n1.should_receive(:allocate).exactly(1).times
      n2 = mock(Node)
      n2.should_receive(:state).exactly(1).times.and_return("ready")
      n2.should_receive(:name).exactly(0).times
      n2.should_receive(:set_state).exactly(0).times
      n2.should_receive(:allocate).exactly(1).times
      answer = ProposalQueue.make_applying_or_delay([n1,n2], apply)
      answer.should eq(["Node n1"])
    end
    it "should return an list of unready nodes and allocate all nodes if apply" do
      make_applying_or_delay(true)
    end
    it "should return an list of unready nodes and allocate all nodes if not apply" do
      make_applying_or_delay(false)
    end

    def make_applying_or_delay_exception(apply)
      validate_locking

      ProposalQueue.stub!(:elements_not_ready)
      ProposalQueue.should_receive(:elements_not_ready).and_raise(Exception.new("message"))

      answer = ProposalQueue.make_applying_or_delay([], apply)
      answer[0].should match /^Error: message/
    end
    it "should return an error in delay if an exception is thrown if apply" do
      make_applying_or_delay_exception(true)
    end
    it "should return an error in delay if an exception is thrown if not apply" do
      make_applying_or_delay_exception(false)
    end
  end

  describe "restore_to_ready" do
    it "should set all nodes' state to ready" do
      validate_locking

      n1 = mock(Node)
      n1.should_receive(:set_state).exactly(1).times do |arg|
        arg.should eq("ready")
      end
      n2 = mock(Node)
      n2.should_receive(:set_state).exactly(1).times do |arg|
        arg.should eq("ready")
      end
      ProposalQueue.restore_to_ready([n1,n2])
    end
    it "should return exception but unlock" do
      validate_locking

      n1 = mock(Node)
      n1.should_receive(:set_state).exactly(1).times do |arg|
        arg.should eq("ready")
      end
      n2 = mock(Node)
      n2.should_receive(:set_state).and_raise(Exception.new("message"))
      lambda {ProposalQueue.restore_to_ready([n1,n2])}.should raise_error(Exception, "message")
    end
  end

  describe "queue_proposal" do
  end

end

