require "spec_helper"

describe ProposalQueue do
  queue = JSON.parse(File.read("spec/fixtures/proposal_queue.json"))

  let(:proposal_queue) do
    ProposalQueue.create(barclamp: "cinder", name: "default", properties: queue)
  end
  let(:proposal_queue_internals) { proposal_queue.properties }

  context "when there is a queue" do
    it "has a record in the database" do
      expect(proposal_queue).to_not be_nil
    end

    it "has a barclamp associated with it" do
      expect(proposal_queue.barclamp).to_not be_empty
    end

    it "has a proposal name associated with it" do
      expect(proposal_queue.name).to_not be_empty
    end

    it "has properties associated with it" do
      expect(proposal_queue_internals).to be_a(Hash)
      expect(proposal_queue_internals["elements"]).to be_a(Hash)
      expect(proposal_queue_internals["elements"]).to_not be_empty
      expect(proposal_queue_internals["deps"]).to be_an(Array)
      expect(proposal_queue_internals["deps"]).to_not be_empty
    end
  end
end
