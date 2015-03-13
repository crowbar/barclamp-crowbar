require 'spec_helper'

describe Proposal do
  let(:proposal) { Proposal.new }

  describe "validations" do
    it "is not valid without a barclamp" do
      proposal.barclamp = nil
      expect(proposal).to_not be_valid
      expect(proposal.errors[:barclamp]).to_not be_empty
    end

    it "is not valid without a name" do
      proposal.name = nil
      expect(proposal).to_not be_valid
      expect(proposal.errors[:name]).to_not be_empty
    end

    it "is not valid without any properties" do
      proposal.properties = nil
      expect(proposal).to_not be_valid
      expect(proposal.errors[:properties]).to_not be_empty
    end
  end
end
