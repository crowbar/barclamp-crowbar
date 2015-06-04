require 'spec_helper'

describe Proposal do
  before do
    Proposal.any_instance.stubs(:update_corresponding_proposal_object).returns(true)
  end

  let(:proposal) { Proposal.new(:barclamp => "crowbar", :name => "default")}

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

    it "is not valid with funky name" do
      ["foo bar", "notvalideither!"].each do |name|
        proposal.name = name
        expect(proposal).to_not be_valid
        expect(proposal.errors[:name]).to_not be_empty
      end
    end

    it "is not valid if name is reserved" do
      # Template is a name for proposal grabbed from the JSON, this should not be allowed.
      # There is a bunch of other names that are illegal, but the list is private, see
      # model validations.
      proposal.name = "template"
      expect(proposal).to_not be_valid
      expect(proposal.errors[:name]).to_not be_empty
    end

    it "a template cannot be saved" do
      prop = Proposal.new(:barclamp => "crowbar")
      expect(prop.name).to eq("template")
      expect(prop.save).to eq(false)
    end

    it "name and barclamp combination is unique" do
      proposal = nil
      begin
        proposal = Proposal.create!(:barclamp => "crowbar", :name => "default", :properties => { :foo => :bar })
        another  = Proposal.new(:barclamp => "crowbar", :name => "default", :properties => {:foo => :bar})

        expect(another).to_not be_valid
        expect(another.errors[:name]).to_not be_empty
      ensure
        proposal.delete if proposal
      end
    end
  end
end
