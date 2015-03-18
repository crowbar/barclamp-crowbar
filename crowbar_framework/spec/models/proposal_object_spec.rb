#
# Copyright 2011-2013, Dell
# Copyright 2013-2014, SUSE LINUX Products GmbH
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

require 'spec_helper'

describe ProposalObject do

  describe "sqlite sync" do
    let(:proposal) { ProposalObject.find_proposal_by_id("bc-crowbar-default") }

    before(:each) { Proposal.delete_all }

    before do
      proposal.item.stubs(:save).returns(true)
      proposal.item.stubs(:destroy).returns(true)
    end

    describe "save" do
      it "creates equivalent proposal model" do
        expect {
          proposal.save
        }.to change {
          Proposal.count
        }.by(1)

        # While raw_data are equivalent, the ProposalObject.item differs as
        # Proposal doesn't store this data.
        expect(proposal.raw_data.to_json).to eq(Proposal.last.raw_data.to_json)
      end

      it "updates changes" do
        proposal.save

        new_description = "Just a test"
        proposal.raw_data["description"] = new_description

        expect {
          proposal.save
        }.to_not change {
          Proposal.count
        }

        expect(Proposal.last.description).to eq(new_description)
      end
    end

    describe "delete" do
      it "removes the equivalent proposal model" do
        proposal.save

        expect {
          proposal.destroy
        }.to change {
          Proposal.count
        }.by(-1)

        expect(Proposal.where(:barclamp => proposal.barclamp, :name => proposal.name).first).to be nil
      end
    end
  end

  describe "barclamp" do
    it "returns barclamp for regular proposals" do
      proposal = ProposalObject.find_proposal_by_id("bc-crowbar-default")
      proposal.barclamp.should == 'crowbar'
    end

    it "returns barclamp for templates" do
      proposal = ProposalObject.find_barclamp("crowbar")
      proposal.barclamp.should == 'crowbar'
    end

    it "returns network for networks" do
      proposal = ProposalObject.find_proposal_by_id("admin_network")
      proposal.barclamp.should == 'network'
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

    it "returns name for networks" do
      proposal = ProposalObject.find_proposal_by_id("admin_network")
      proposal.name.should == 'admin'
    end
  end

  describe "finders" do
    describe "interface" do
      [
        :all,
        :find_data_bag_item,
        :find,
        :find_proposals,
        :find_barclamp,
        :find_proposal,
        :find_proposal_by_id,
      ].each do |method|
        it "responds to #{method}" do
          ProposalObject.should respond_to(method)
        end
      end
    end

    describe "find" do
      it "returns proposals matching a search" do
        proposals = ProposalObject.find("bc-crowbar-*")
        proposals.should_not be_empty
        proposals.all? { |p| p.id =~ /^bc-crowbar/ }.should be true
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
        proposals.all? { |p| p.is_a?(ProposalObject) }.should be true
      end
    end

    describe "find_proposals" do
      it "returns all barclamp proposals with a given name" do
        proposals = ProposalObject.find_proposals("crowbar")
        proposals.should_not be_empty
        proposals.all? { |p| p.id == "bc-crowbar-default" }.should be true
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
