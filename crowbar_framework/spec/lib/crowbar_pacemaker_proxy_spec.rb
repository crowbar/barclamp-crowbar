require 'spec_helper'
require 'ostruct'

describe CrowbarPacemakerProxy do
  let(:klass) do
    Class.new do
      class << self
        include CrowbarPacemakerProxy
      end
    end
  end

  context "cluster_status" do
    let(:nodes) { [OpenStruct.new(:status => "pending"), OpenStruct.new(:status => "failed"), OpenStruct.new(:status => "ready")] }
    let(:bad_nodes) { [OpenStruct.new(:status => "i don't exist")] }

    it "returns the lowest state of the nodes" do
      klass.cluster_status(nodes).should == "failed"
    end

    it "returns unknown for empty clusters" do
      klass.cluster_status([]).should == "unknown"
    end

    it "returns unknown for weird node state" do
      klass.cluster_status([]).should == "unknown"
    end
  end
end
