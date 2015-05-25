require 'spec_helper'

describe Proposal do
  let(:proposal) { Proposal.new(:barclamp => "crowbar", :name => "default")}

  describe "Finders" do
    before do
      Proposal.delete_all
      Proposal.create(:barclamp => "crowbar", :name => "default")
    end

    after do
      Proposal.delete_all
    end

    describe "interface" do
      [
        :all,
        :find,
        :find_proposals,
        :find_barclamp,
        :find_proposal,
        :find_proposal_by_id,
      ].each do |method|
        it "responds to #{method}" do
          expect(proposal.class).to respond_to(method)
        end
      end
    end

    describe "find" do
      it "returns proposals matching a search" do
        skip("Incompatible API, fix call sites")
      end
    end

    describe "find_data_bag_item" do
      it "returns a bag" do
        skip("Data bag item still handled by ProposalObject")
      end
    end

    describe "all" do
      it "returns all proposals" do
        proposals = Proposal.all
        expect(proposals).to_not be_empty
        expect(proposals.all? { |p| p.is_a?(Proposal) }).to be true
      end
    end

    describe "find_proposals" do
      it "returns all barclamp proposals with a given name" do
        proposals = Proposal.find_proposals("crowbar")
        expect(proposals).to_not be_empty
        expect(proposals.all? { |p| p.key == "bc-crowbar-default" }).to be true
      end
    end

    describe "find_barclamp" do
      it "returns a barclamp with a given name" do
        barclamp = Proposal.find_barclamp("crowbar")
        expect(barclamp.key).to eq("bc-template-crowbar")
      end
    end

    describe "find_proposal_by_id" do
      it "returns a matching proposal" do
        proposal = Proposal.find_proposal_by_id("bc-crowbar-default")
        expect(proposal.key).to eq("bc-crowbar-default")
      end
    end
  end

  describe "API" do
    let(:proposal_object)   { ProposalObject.find_proposal_by_id("bc-template-crowbar") }
    let(:proposal_template) { Proposal.new(:barclamp => "crowbar", :name => "template")}

    let(:checked_methods) { proposal_object.public_methods(false) + Crowbar::ProposalMethods.public_instance_methods }

    it "quacks like a ProposalObject" do
      checked_methods.each do |m|
        expect(proposal_template).to respond_to(m)
      end
    end

    it "returns the same values as ProposalObject" do
      checked_methods.reject do |m|
        proposal_object.method(m).arity > 0 # Only getters
      end.reject do |m|
          [:id, :item, :save, :destroy, :export].include?(m) # These methods are incompatible and need attention
      end.each do |m|
        new_implementation = proposal_template.send(m)
        old_implementation = proposal_object.send(m)

        expect(old_implementation).to eq(new_implementation), "#{m.to_s} - old (#{old_implementation}) vs. new (#{new_implementation})"
      end
    end
  end

  it "updates the proposal id before save" do
    proposal.save
    expect(proposal.properties["id"]).to eq("bc-crowbar-default")
  end

  it "raises when barclamp is not specified in the constructor" do
    expect {
      Proposal.new
    }.to raise_error(ArgumentError)

    expect {
      Proposal.new(:name => "default")
    }.to raise_error(ArgumentError)
  end

  describe "default properties loading" do
    it "loads the properties based on barclamp" do
      expect(proposal.properties).to_not be_empty
    end

    it "raises an error if the barclamp template does not exist" do
      expect {
        Proposal.new(:barclamp => "nonexistent_barclamp")
      }.to raise_error(Proposal::TemplateMissing)
    end
  end

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

    it "is not valid if name is reserved" do
      # Template is a name for proposal grabbed from the JSON, this should not be allowed.
      # There is a bunch of other names that are illegal, but the list is private, see
      # model validations.
      proposal.name = "template"
      expect(proposal).to_not be_valid
      expect(proposal.errors[:name]).to_not be_empty
    end

    it "name and barclamp combination is unique" do
      proposal = nil
      begin
        proposal = Proposal.create!(:barclamp => "crowbar", :name => "default", :properties => { :foo => :bar })
        another  = Proposal.new(:barclamp => "crowbar", :name => "default", :properties => {:foo => :bar})

        expect(another).to_not be_valid
        expect(another.errors[:name]).to_not be_empty
      ensure
        proposal.destroy if proposal
      end
    end
  end
end
