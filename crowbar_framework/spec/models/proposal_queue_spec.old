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

  describe "item ordering" do
    it "should return empty with new queue" do
      q = ProposalQueue.get_queue("greg", :log)
      q.proposal_queue_items.empty?.should be true
    end

    def create_item(pcid, pos, mes)
      item = ProposalQueueItem.new
      item.proposal_config_id = pcid
      item.position = pos
      item.queue_reason = mes
      item.save
      item
    end

    it "should return item once item is added to queue" do
      item = create_item(9, 10, "message")

      q = ProposalQueue.get_queue("greg", :log)
      q.proposal_queue_items << item
      q.save

      q2 = ProposalQueue.get_queue("greg", :log)
      q2.proposal_queue_items.empty?.should be false
      q2.proposal_queue_items[0].proposal_config_id.should be 9
      q2.proposal_queue_items[0].position.should be 10
      q2.proposal_queue_items[0].queue_reason.should eq("message")
    end

    it "should return ordered item once items are added to queue" do
      item1 = create_item(9, 19, "message1")
      item2 = create_item(10, 20, "message2")
      item3 = create_item(11, 21, "message3")

      q = ProposalQueue.get_queue("greg", :log)
      q.proposal_queue_items << item2
      q.proposal_queue_items << item3
      q.proposal_queue_items << item1
      q.save

      q2 = ProposalQueue.get_queue("greg", :log)
      q2.proposal_queue_items.empty?.should be false
      q2.proposal_queue_items[0].proposal_config_id.should be 9
      q2.proposal_queue_items[0].position.should be 19
      q2.proposal_queue_items[0].queue_reason.should eq("message1")
      q2.proposal_queue_items[1].proposal_config_id.should be 10
      q2.proposal_queue_items[1].position.should be 20
      q2.proposal_queue_items[1].queue_reason.should eq("message2")
      q2.proposal_queue_items[2].proposal_config_id.should be 11
      q2.proposal_queue_items[2].position.should be 21
      q2.proposal_queue_items[2].queue_reason.should eq("message3")
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
    it "should return true if proposal is nil" do
      answer = ProposalQueue.update_proposal_status(nil, "status", "message")
      answer.should be true
    end

    it "should return true if proposal is active and set status and message" do
      mock_pc = mock(ProposalConfig)
      mock_pc.should_receive(:status=).exactly(1).times do |arg|
        arg.should eq("status")
      end
      mock_pc.should_receive(:failed_reason=).exactly(1).times do |arg|
        arg.should eq("message")
      end
      mock_pc.should_receive(:save).exactly(1).times.and_return(true)

      answer = ProposalQueue.update_proposal_status(mock_pc, "status", "message")
      answer.should be true
    end

    it "should return false if proposal is active and set status and message and save fails" do
      mock_pc = mock(ProposalConfig)
      mock_pc.should_receive(:status=).exactly(1).times do |arg|
        arg.should eq("status")
      end
      mock_pc.should_receive(:failed_reason=).exactly(1).times do |arg|
        arg.should eq("message")
      end
      mock_pc.should_receive(:save).exactly(1).times.and_return(false)

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
      n1 = mock(Node)
      n1.should_receive(:state).exactly(1).times.and_return("ready")
      n1.should_receive(:name).exactly(0).times
      n1.should_receive(:set_state).exactly(0).times
      n1.should_receive(:allocate).exactly(0).times

      answer = ProposalQueue.make_applying_or_delay([n1], false)
      answer.should eq([])
    end
    it "should return an empty list and set_state of nodes if apply and nodes are ready" do
      n1 = mock(Node)
      n1.should_receive(:state).exactly(1).times.and_return("ready")
      n1.should_receive(:name).exactly(0).times
      n1.should_receive(:set_state).exactly(1).times do |arg1, arg2|
        arg1.should eq("applying")
        arg2.should eq("ready")
      end.and_return([200, ""])
      n1.should_receive(:allocate).exactly(0).times
      answer = ProposalQueue.make_applying_or_delay([n1], true)
      answer.should eq([])
    end
    it "should return an empty list and set_state of nodes if apply and nodes (more than one) are ready" do
      nodes = [ mock(Node), mock(Node), mock(Node) ]
      nodes.each do |n|
        n.should_receive(:state).exactly(1).times.and_return("ready")
        n.should_receive(:name).exactly(0).times
        n.should_receive(:set_state).exactly(1).times do |arg1, arg2|
          arg1.should eq("applying")
          arg2.should eq("ready")
        end.and_return([200, ""])
        n.should_receive(:allocate).exactly(0).times
      end
      answer = ProposalQueue.make_applying_or_delay(nodes, true)
      answer.should eq([])
    end
    it "should return an empty list and set_state of nodes if apply and nodes (more than one with duplicates) are ready" do
      n = mock(Node)
      nodes = [ mock(Node), n, mock(Node) ]
      nodes.each do |n|
        n.should_receive(:state).exactly(1).times.and_return("ready")
        n.should_receive(:name).exactly(0).times
        n.should_receive(:set_state).exactly(1).times do |arg1, arg2|
          arg1.should eq("applying")
          arg2.should eq("ready")
        end.and_return([200, ""])
        n.should_receive(:allocate).exactly(0).times
      end
      nodes << n # Add the duplicate
      answer = ProposalQueue.make_applying_or_delay(nodes, true)
      answer.should eq([])
    end
    it "should return a list of failed nodes if apply and some nodes fail to set ready" do
      nodes = [ mock(Node), mock(Node), mock(Node) ]
      count = 0
      nodes.each do |n|
        n.should_receive(:state).exactly(1).times.and_return("ready")
        if count == 0
          n.should_receive(:name).exactly(0).times
        else
          n.should_receive(:name).exactly(1).times.and_return("node#{count}")
        end
        n.should_receive(:set_state).exactly(count == 0 ? 2 : 1).times.and_return([(count == 0 ? 200 : 409), ""])
        n.should_receive(:allocate).exactly(1).times
        count += 1
      end
      answer = ProposalQueue.make_applying_or_delay(nodes, true)
      answer.should eq(["Node node1", "Node node2"])
    end

    def make_applying_or_delay(apply)
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
      nodes = [ mock(Node), mock(Node) ]
      nodes.each do |n|
        n.should_receive(:set_state).exactly(1).times do |arg1, arg2|
          arg1.should eq("ready")
          arg2.should eq("applying")
        end
      end
      ProposalQueue.restore_to_ready(nodes)
    end
    it "should return exception" do
      n1 = mock(Node)
      n1.should_receive(:set_state).exactly(1).times do |arg1, arg2|
        arg1.should eq("ready")
        arg2.should eq("applying")
      end
      n2 = mock(Node)
      n2.should_receive(:set_state).and_raise(Exception.new("message"))
      lambda {ProposalQueue.restore_to_ready([n1,n2])}.should raise_error(Exception, "message")
    end
  end

  def make_proposal_config()
    barclamp = Barclamp.find_or_create_by_name("crowbar")

    # Not active - dep1
    proposal = barclamp.create_proposal("dep1")

    # Active but not applied - dep2
    proposal = barclamp.create_proposal("dep2")
    proposal.active_config = proposal.current_config
    proposal.current_config.status = ProposalConfig::STATUS_QUEUED
    proposal.save

    # Active and applied - dep3
    proposal = barclamp.create_proposal("dep3")
    proposal.active_config = proposal.current_config
    proposal.current_config.status = ProposalConfig::STATUS_APPLIED
    proposal.current_config.save
    proposal.save

    proposal = barclamp.create_proposal("temp")
    proposal.current_config
  end

  describe "can_proposal_config_run" do
    it "should return 1,[] for a proposal with no nodes and no deps and will apply" do
      pc = make_proposal_config
      q = ProposalQueue.get_queue("greg", Rails.logger)

      ProposalQueue.should_receive(:make_applying_or_delay).exactly(1).times do |arg1, arg2|
        arg2.should be true
      end.and_return([]) 

      pos, delay = q.can_proposal_config_run(pc, true)
      pos.should be 1
      delay.should eq([])
    end

    it "should return 1,[] for a proposal with no nodes and no deps and will not apply" do
      pc = make_proposal_config
      q = ProposalQueue.get_queue("greg", Rails.logger)

      ProposalQueue.should_receive(:make_applying_or_delay).exactly(1).times do |arg1, arg2|
        arg2.should be false
      end.and_return([]) 

      pos, delay = q.can_proposal_config_run(pc, false)
      pos.should be 1
      delay.should eq([])
    end

    it "should return 1,props for a proposal that is missing dependencies and no nodes could apply" do
      pc = make_proposal_config
      q = ProposalQueue.get_queue("greg", Rails.logger)

      pc.proposal.barclamp.operations(Rails.logger).should_receive(:proposal_dependencies).exactly(1).times.and_return([{ "inst" => "dep0", "barclamp" => "crowbar" },
                          { "inst" => "dep1", "barclamp" => "crowbar" },
                          { "inst" => "dep2", "barclamp" => "crowbar" },
                          { "inst" => "dep3", "barclamp" => "crowbar" }])

      ProposalQueue.should_receive(:make_applying_or_delay).exactly(1).times do |arg1, arg2|
        arg2.should be false
      end.and_return([]) 

      pos, delay = q.can_proposal_config_run(pc, true)
      pos.should be 1
      delay.should eq(["Proposal crowbar.dep0","Proposal crowbar.dep1","Proposal crowbar.dep2"])
    end

    it "should return 1,props for a proposal that is missing dependencies and no nodes wont apply" do
      pc = make_proposal_config
      q = ProposalQueue.get_queue("greg", Rails.logger)

      pc.proposal.barclamp.operations(Rails.logger).should_receive(:proposal_dependencies).exactly(1).times.and_return([{ "inst" => "dep0", "barclamp" => "crowbar" },
                          { "inst" => "dep1", "barclamp" => "crowbar" },
                          { "inst" => "dep2", "barclamp" => "crowbar" },
                          { "inst" => "dep3", "barclamp" => "crowbar" }])

      ProposalQueue.should_receive(:make_applying_or_delay).exactly(1).times do |arg1, arg2|
        arg2.should be false
      end.and_return([]) 

      pos, delay = q.can_proposal_config_run(pc, false)
      pos.should be 1
      delay.should eq(["Proposal crowbar.dep0","Proposal crowbar.dep1","Proposal crowbar.dep2"])
    end

    it "should return 1,nodes for a proposal that is not missing dependencies and nodes can apply" do
      pc = make_proposal_config
      q = ProposalQueue.get_queue("greg", Rails.logger)

      pc.proposal.barclamp.operations(Rails.logger).should_receive(:proposal_dependencies).exactly(1).times.and_return([])

      ProposalQueue.should_receive(:make_applying_or_delay).exactly(1).times do |arg1, arg2|
        arg2.should be true
      end.and_return(["Node n1", "Node n2"]) 

      pos, delay = q.can_proposal_config_run(pc, true)
      pos.should be 1
      delay.should eq(["Node n1","Node n2"])
    end

    it "should return 1,nodes for a proposal that is not missing dependencies and nodes not apply" do
      pc = make_proposal_config
      q = ProposalQueue.get_queue("greg", Rails.logger)

      pc.proposal.barclamp.operations(Rails.logger).should_receive(:proposal_dependencies).exactly(1).times.and_return([])

      ProposalQueue.should_receive(:make_applying_or_delay).exactly(1).times do |arg1, arg2|
        arg2.should be false
      end.and_return(["Node n1", "Node n2"]) 

      pos, delay = q.can_proposal_config_run(pc, false)
      pos.should be 1
      delay.should eq(["Node n1","Node n2"])
    end

    it "should return 1,probs for a proposal that is missing dependencies and nodes could apply" do
      pc = make_proposal_config
      q = ProposalQueue.get_queue("greg", Rails.logger)

      pc.proposal.barclamp.operations(Rails.logger).should_receive(:proposal_dependencies).exactly(1).times.and_return([{ "inst" => "dep0", "barclamp" => "crowbar" },
                          { "inst" => "dep1", "barclamp" => "crowbar" },
                          { "inst" => "dep2", "barclamp" => "crowbar" },
                          { "inst" => "dep3", "barclamp" => "crowbar" }])

      ProposalQueue.should_receive(:make_applying_or_delay).exactly(1).times do |arg1, arg2|
        arg2.should be true
      end.and_return(["Node n1", "Node n2"]) 

      pos, delay = q.can_proposal_config_run(pc, true)
      pos.should be 1
      delay.should eq(["Proposal crowbar.dep0","Proposal crowbar.dep1","Proposal crowbar.dep2","Node n1","Node n2"])
    end

    it "should return 1,probs for a proposal that is missing dependencies and nodes wont apply" do
      pc = make_proposal_config
      q = ProposalQueue.get_queue("greg", Rails.logger)

      pc.proposal.barclamp.operations(Rails.logger).should_receive(:proposal_dependencies).exactly(1).times.and_return([{ "inst" => "dep0", "barclamp" => "crowbar" },
                          { "inst" => "dep1", "barclamp" => "crowbar" },
                          { "inst" => "dep2", "barclamp" => "crowbar" },
                          { "inst" => "dep3", "barclamp" => "crowbar" }])

      ProposalQueue.should_receive(:make_applying_or_delay).exactly(1).times do |arg1, arg2|
        arg2.should be false
      end.and_return(["Node n1", "Node n2"]) 

      pos, delay = q.can_proposal_config_run(pc, false)
      pos.should be 1
      delay.should eq(["Proposal crowbar.dep0","Proposal crowbar.dep1","Proposal crowbar.dep2","Node n1","Node n2"])
    end

    it "should return 21,props for a proposal that is missing dependencies and pending items" do
      pc = make_proposal_config
      q = ProposalQueue.get_queue("greg", Rails.logger)

      pc.proposal.barclamp.operations(Rails.logger).should_receive(:proposal_dependencies).exactly(1).times.and_return([{ "inst" => "dep0", "barclamp" => "crowbar" },
                          { "inst" => "dep1", "barclamp" => "crowbar" },
                          { "inst" => "dep2", "barclamp" => "crowbar" },
                          { "inst" => "dep3", "barclamp" => "crowbar" }])

      ProposalQueue.should_receive(:make_applying_or_delay).exactly(1).times do |arg1, arg2|
        arg2.should be false
      end.and_return([]) 

      i1 = ProposalQueueItem.new
      i1.proposal_config_id = Proposal.find_by_name("dep1").current_config.id
      i1.proposal_queue_id = q.id
      i1.position = 10
      i1.queue_reason = "jjj"
      i1.save

      i2 = ProposalQueueItem.new
      i2.proposal_config_id = Proposal.find_by_name("dep2").current_config.id
      i2.proposal_queue_id = q.id
      i2.position = 20
      i1.queue_reason = "kkk"
      i2.save

      pos, delay = q.can_proposal_config_run(pc, true)
      pos.should be 21
      delay.should eq(["Proposal crowbar.dep0","Proposal crowbar.dep1","Proposal crowbar.dep2"])
    end
  end

  describe "queue_proposal" do  
    it "should return true for a proposal that is already queued" do
      pc = make_proposal_config
      q = ProposalQueue.get_queue("greg", Rails.logger)

      item = ProposalQueueItem.new
      item.queue_reason = "fred"
      item.proposal_config_id = pc.id
      item.proposal_queue_id = q.id
      item.position = 13
      item.save

      ProposalQueue.should_receive(:update_proposal_status).exactly(0).times
      answer, message = q.queue_proposal(pc)
      answer.should be true
      message.should eq("fred")
    end
    it "should return false for a proposal that can run" do
      pc = make_proposal_config
      q = ProposalQueue.get_queue("greg", Rails.logger)
      q.should_receive(:can_proposal_config_run).exactly(1).times.and_return([0, []])
      ProposalQueue.should_receive(:update_proposal_status).exactly(0).times

      answer, message = q.queue_proposal(pc)
      answer.should be false
      message.should eq("")
    end
    it "should return true for a proposal that can not run" do
      pc = make_proposal_config
      q = ProposalQueue.get_queue("greg", Rails.logger)
      q.should_receive(:can_proposal_config_run).exactly(1).times.and_return([3, ["a", "b"]])

      ProposalQueue.should_receive(:update_proposal_status).exactly(1).times do |a1,a2,a3|
        a1.id.should be pc.id
        a2.should be ProposalConfig::STATUS_QUEUED
        a3.should eq("")
      end

      answer, message = q.queue_proposal(pc)
      answer.should be true
      message.should eq("a,b")

      item = ProposalQueueItem.find_by_proposal_config_id_and_proposal_queue_id(pc.id, q.id)
      item.position.should be 3
      item.queue_reason.should eq("a,b")
    end
    it "should return true for an exception" do
      pc = make_proposal_config
      q = ProposalQueue.get_queue("greg", Rails.logger)
      q.should_receive(:can_proposal_config_run).exactly(1).times.and_raise(Exception.new("test"))
      ProposalQueue.should_receive(:update_proposal_status).exactly(0).times

      answer, message = q.queue_proposal(pc)
      answer.should be true
      message.should match /^Error: crowbar:temp: test/
    end
  end

  describe "dequeue_proposal_nolock" do
    it "should return true and if item is nil" do
      ProposalQueue.should_receive(:update_proposal_status).exactly(0).times
      q = ProposalQueue.get_queue("greg", Rails.logger)
      answer = q.dequeue_proposal_no_lock(nil)
      answer.should be true
    end
    it "should return true and remove the item" do
      item = mock(ProposalQueueItem)
      item.should_receive(:destroy).exactly(1).times
      item.should_receive(:proposal_config).exactly(1).times.and_return(nil)
      ProposalQueue.should_receive(:update_proposal_status).exactly(1).times
      q = ProposalQueue.get_queue("greg", Rails.logger)
      answer = q.dequeue_proposal_no_lock(item)
      answer.should be true
    end
    it "should return false on an exception" do
      item = mock(ProposalQueueItem)
      item.should_receive(:destroy).exactly(1).times.and_raise(Exception.new("test"))
      item.should_receive(:proposal_config).exactly(1).times.and_return(nil)
      ProposalQueue.should_receive(:update_proposal_status).exactly(1).times
      q = ProposalQueue.get_queue("greg", Rails.logger)
      answer = q.dequeue_proposal_no_lock(item)
      answer.should be false
    end
  end

  describe "dequeue_proposal" do
    it "should return false on an exception" do
      Proposal.should_receive(:find_by_name_and_barclamp_id).exactly(1).times.and_raise(Exception.new("test"))
      q = ProposalQueue.get_queue("greg", Rails.logger)
      answer = q.dequeue_proposal("cow", "crowbar")
      answer.should be false
    end
    it "should return true if no proposal is found" do
      Proposal.should_receive(:find_by_name_and_barclamp_id).exactly(1).times.and_return(nil)
      q = ProposalQueue.get_queue("greg", Rails.logger)
      answer = q.dequeue_proposal("cow", "crowbar")
      answer.should be true
    end
    it "should return true if proposal is found but not active" do
      p = mock(Proposal)
      p.should_receive(:active?).exactly(1).times.and_return(false)
      Proposal.should_receive(:find_by_name_and_barclamp_id).exactly(1).times.and_return(p)
      q = ProposalQueue.get_queue("greg", Rails.logger)
      answer = q.dequeue_proposal("cow", "crowbar")
      answer.should be true
    end
    it "should return true if proposal is found and active and not queued" do
      pc = mock(ProposalConfig)
      pc.should_receive(:id).exactly(1).times.and_return(3)
      p = mock(Proposal)
      p.should_receive(:active?).exactly(1).times.and_return(true)
      p.should_receive(:active_config).exactly(1).times.and_return(pc)
      Proposal.should_receive(:find_by_name_and_barclamp_id).exactly(1).times.and_return(p)
      ProposalQueueItem.should_receive(:find_by_proposal_config_id).exactly(1).times.and_return(nil)
      q = ProposalQueue.get_queue("greg", Rails.logger)
      q.should_receive(:dequeue_proposal_no_lock).exactly(1).times.and_return(true)
      answer = q.dequeue_proposal("cow", "crowbar")
      answer.should be true
    end
    it "should return true if proposal is found and active and queued" do
      pc = mock(ProposalConfig)
      pc.should_receive(:id).exactly(1).times.and_return(3)
      p = mock(Proposal)
      p.should_receive(:active?).exactly(1).times.and_return(true)
      p.should_receive(:active_config).exactly(1).times.and_return(pc)
      Proposal.should_receive(:find_by_name_and_barclamp_id).exactly(1).times.and_return(p)
      ProposalQueueItem.should_receive(:find_by_proposal_config_id).exactly(1).times.and_return(:item)
      q = ProposalQueue.get_queue("greg", Rails.logger)
      q.should_receive(:dequeue_proposal_no_lock).exactly(1).times.and_return(true)
      answer = q.dequeue_proposal("cow", "crowbar")
      answer.should be true
    end
  end

  describe "process_queue" do
    it "should return success with a nil queue" do
      q = ProposalQueue.get_queue("greg", Rails.logger)
      q.should_receive(:proposal_queue_items).exactly(1).times.and_return(nil)
      ret, count, message = q.process_queue
      ret.should be true
      count.should be 0
      message.should eq("")
    end
    it "should return success with an empty queue" do
      q = ProposalQueue.get_queue("greg", Rails.logger)
      q.should_receive(:proposal_queue_items).exactly(1).times.and_return([])
      ret, count, message = q.process_queue
      ret.should be true
      count.should be 0
      message.should eq("")
    end
    it "should return success and 0 with a queue of items not ready" do
      q = ProposalQueue.get_queue("greg", Rails.logger)
      item1 = mock(ProposalQueueItem)
      item1.should_receive(:proposal_config).exactly(1).times.and_return(:cow)
      item2 = mock(ProposalQueueItem)
      item2.should_receive(:proposal_config).exactly(1).times.and_return(:chicken)
      q.should_receive(:proposal_queue_items).exactly(1).times.and_return([item1, item2])
      q.should_receive(:can_proposal_config_run).exactly(2).times.and_return([1, ["dog"]])
      q.should_receive(:dequeue_proposal_no_lock).exactly(0).times
      ret, count, message = q.process_queue
      ret.should be true
      count.should be 0
      message.should eq("")
    end
    it "should return success and 1 with a queue of 1 ready and 1 not ready item" do
      q = ProposalQueue.get_queue("greg", Rails.logger)
      item1 = mock(ProposalQueueItem)
      item1.should_receive(:proposal_config).exactly(2).times.and_return(:cow)

      bo = mock(CrowbarService)
      bo.should_receive(:proposal_commit).exactly(1).times.and_return([200, ""])
      b = mock(Barclamp)
      b.should_receive(:operations).exactly(1).times.and_return(bo)
      p = mock(Proposal)
      p.should_receive(:barclamp).exactly(1).times.and_return(b)
      p.should_receive(:name).exactly(1).times.and_return("fred")
      pc = mock(ProposalConfig)
      pc.should_receive(:proposal).exactly(1).times.and_return(p)
      item2 = mock(ProposalQueueItem)
      item2.should_receive(:proposal_config).exactly(2).times.and_return(pc)
      q.should_receive(:proposal_queue_items).exactly(2).times.and_return([item2, item1],[item1])
      q.should_receive(:can_proposal_config_run).exactly(2).times.and_return([1, []], [1,["dog"]], [1,["dog"]]) 
      q.should_receive(:dequeue_proposal_no_lock).exactly(1).times do |arg|
        arg.should be item2
      end
      q.should_receive(:reload).exactly(1).times
      ret, count, message = q.process_queue
      message.should eq("")
      count.should be 1
      ret.should be true
    end

    it "should return success and 1 with a queue of 1 ready and 1 not ready item and loop around" do
      q = ProposalQueue.get_queue("greg", Rails.logger)
      item1 = mock(ProposalQueueItem)
      item1.should_receive(:proposal_config).exactly(1).times.and_return(:cow)

      bo = mock(CrowbarService)
      bo.should_receive(:proposal_commit).exactly(1).times.and_return([202, ""])
      b = mock(Barclamp)
      b.should_receive(:operations).exactly(1).times.and_return(bo)
      p = mock(Proposal)
      p.should_receive(:barclamp).exactly(1).times.and_return(b)
      p.should_receive(:name).exactly(1).times.and_return("fred")
      pc = mock(ProposalConfig)
      pc.should_receive(:proposal).exactly(1).times.and_return(p)
      item2 = mock(ProposalQueueItem)
      item2.should_receive(:proposal_config).exactly(2).times.and_return(pc)
      q.should_receive(:proposal_queue_items).exactly(1).times.and_return([item2, item1])
      q.should_receive(:can_proposal_config_run).exactly(2).times.and_return([1, []], [1,["dog"]])
      q.should_receive(:dequeue_proposal_no_lock).exactly(1).times do |arg|
        arg.should be item2
      end
      q.should_receive(:reload).exactly(0).times
      ret, count, message = q.process_queue
      ret.should be true
      count.should be 1
      message.should eq("")
    end

    it "should return false when receiving and exception while dequeuing items" do
      q = ProposalQueue.get_queue("greg", Rails.logger)
      q.should_receive(:proposal_queue_items).exactly(1).times.and_raise(Exception.new("test"))
      ret, count, message = q.process_queue
      ret.should be false
      count.should be 0
      message.should match /Error: test/
    end

  end
end

