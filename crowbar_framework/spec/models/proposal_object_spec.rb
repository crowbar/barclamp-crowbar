require 'spec_helper'

describe ProposalObject do
  describe "barclamp" do
    it "returns barclamp for regular proposals" do
      proposal = ProposalObject.find_proposal_by_id("bc-crowbar-default")
      proposal.barclamp.should == 'crowbar'
    end

    it "returns barclamp for templates" do
      proposal = ProposalObject.find_barclamp("crowbar")
      proposal.barclamp.should == 'crowbar'
    end
  end

  describe "name" do
    it "returns name for regular proposals" do
      proposal = ProposalObject.find_proposal_by_id("bc-crowbar-default")
      proposal.name.should == 'default'
    end

    it "returns name for templates" do
      proposal = ProposalObject.find_barclamp("crowbar")
      proposal.name.should == 'template'
    end
  end

  describe "finders" do
    describe "find" do
      it "returns proposals matching a search" do
        proposals = ProposalObject.find("bc-crowbar-*")
        proposals.should_not be_empty
        proposals.all? { |p| p.id =~ /^bc-crowbar/ }.should be_true
      end
    end

    describe "find_data_bag_item" do
      it "returns a bag" do
        bag = ProposalObject.find_data_bag_item("crowbar-bc-crowbar-default")
        bag.should_not be_nil
        bag.id.should == "bc-crowbar-default"
      end
    end

    describe "all" do
      it "returns all proposals" do
        proposals = ProposalObject.all
        proposals.should_not be_empty
        proposals.all? { |p| p.is_a?(ProposalObject) }.should be_true
      end
    end

    describe "find_proposals" do
      it "returns all barclamp proposals with a given name" do
        proposals = ProposalObject.find_proposals("crowbar")
        proposals.should_not be_empty
        proposals.all? { |p| p.id == "bc-crowbar-default" }.should be_true
      end
    end

    describe "find_barclamp" do
      it "returns a barclamp with a given name" do
        barclamp = ProposalObject.find_barclamp("crowbar")
        barclamp.id.should == "bc-template-crowbar"
      end
    end

    describe "find_proposal_by_id" do
      it "returns a matching proposal" do
        proposal = ProposalObject.find_proposal_by_id("bc-crowbar-default")
        proposal.id.should == "bc-crowbar-default"
      end
    end
  end
end

