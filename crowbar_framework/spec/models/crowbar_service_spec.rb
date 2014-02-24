# -*- encoding : utf-8 -*-
require File.expand_path(File.join(File.dirname(__FILE__), "..", "spec_helper"))

describe CrowbarService do
  before do
    CrowbarService.any_instance.stubs(:system).returns(false)
    CrowbarService.any_instance.stubs(:run_remote_chef_client).returns(0)
  end

  let(:crowbar) { c = CrowbarService.new(Logger.new("/dev/null")); c.bc_name = "crowbar"; c }

  describe "transition" do
    it "returns 404 without state" do
      response = crowbar.transition("default", "testing", nil)
      response.first.should == 404
    end

    it "returns 404 if node not found" do
      NodeObject.stubs(:find_node_by_name).returns(nil)
      response = crowbar.transition("default", "missing", "a state")
      response.first.should == 404
    end

    it "returns 200 on successful transition" do
      RoleObject.stubs(:find_roles_by_search).returns([])
      response = crowbar.transition("default", "testing", "a state")
      response.first.should == 200
    end

    describe "to another state" do
      before do
        @node = NodeObject.find_node_by_name("testing")
      end

      it "sets the state debug and state" do
        NodeObject.stubs(:find_node_by_name).returns(@node)
        crowbar.transition("default", @node.name, "a state")
        @node.state.should == "a state"
        @node.crowbar["crowbar"]["state_debug"].should_not be_empty
      end

      it "saves the node" do
        NodeObject.stubs(:find_node_by_name).returns(@node)
        @node.expects(:save).at_least_once
        crowbar.transition("default", @node.name, "a state")
      end
    end

    describe "to testing" do
      it "creates new node if not found" do
        NodeObject.expects(:create_new).with("missing").returns(nil).once
        crowbar.transition("default", "missing", "testing")
      end
    end

    describe "to discovering" do
      before do
        @node = NodeObject.find_node_by_name("testing")
      end

      it "adds role to the node if admin" do
        crowbar.expects(:add_role_to_instance_and_node).once
        crowbar.transition("default", "admin", "discovering")
      end

      it "creates new node if not found" do
        NodeObject.stubs(:find_node_by_name).returns(nil)
        NodeObject.expects(:create_new).returns(nil).once
        crowbar.transition("default", "missing", "discovering")
      end

      it "sets the node as initially not allocated" do
        NodeObject.stubs(:find_node_by_name).returns(@node)
        @node.allocated = nil
        crowbar.transition("default", "testing", "discovering")
        @node.allocated.should == false
      end
    end

    describe "to hardware-installing" do
      before do
        @node = NodeObject.find_node_by_name("testing")
      end

      it "forces nodes transition to a given state" do
        NodeObject.stubs(:find_node_by_name).returns(@node)
        crowbar.transition("default", "testing", "hardware-installing")
        @node.state.should == "hardware-installing"
      end
    end

    describe "to hardware-updating" do
      before do
        @node = NodeObject.find_node_by_name("testing")
      end

      it "forces nodes transition to a given state" do
        NodeObject.stubs(:find_node_by_name).returns(@node)
        crowbar.transition("default", "testing", "hardware-updating")
        @node.state.should == "hardware-updating"
      end
    end

    describe "to update" do
      before do
        @node = NodeObject.find_node_by_name("testing")
      end

      it "forces nodes transition to a given state" do
        NodeObject.stubs(:find_node_by_name).returns(@node)
        crowbar.transition("default", "testing", "update")
        @node.state.should == "update"
      end
    end
  end
end

