require 'spec_helper'

describe NodeObject do
  describe "finders" do
    describe "all" do
      it "returns all nodes" do
        nodes = NodeObject.all
        nodes.should_not be_empty
        nodes.all? { |n| n.is_a?(NodeObject) }.should be_true
      end
    end

    describe "find_nodes_by_name" do
      it "returns nodes with a given name only" do
        nodes = NodeObject.find_nodes_by_name("testing.crowbar.com")
        nodes.should_not be_empty
        nodes.all? { |n| n.name =~ /testing/ }.should be_true
      end
    end

    describe "find_node_by_name" do
      it "returns nodes matching name" do
        node = NodeObject.find_node_by_name("testing")
        node.should_not be_nil
        node.name.should =~ /testing/
      end
    end

    describe "find_node_by_alias" do
      it "returns nodes matching alias" do
        node = NodeObject.find_node_by_alias("testing")
        node.should_not be_nil
        node.alias.should == "testing"
      end
    end
  end

  describe "license_key" do
    describe "assignment" do
      let(:node) { NodeObject.find_node_by_name("testing") }
      let(:key) { "a key" }

      it "sets the key if the platform requires one" do
        CrowbarService.stubs(:require_license_key?).returns(true)
        node.license_key = key
        node.license_key.should == key
      end

      it "leaves it blank if platform does not need a key" do
        CrowbarService.stubs(:require_license_key?).returns(false)
        node.license_key = key
        node.license_key.should be_blank
      end
    end
  end
end

