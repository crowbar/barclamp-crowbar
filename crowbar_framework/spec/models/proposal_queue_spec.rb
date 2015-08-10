require "spec_helper"

describe ProposalQueue do
  queue = JSON.parse(File.read("spec/fixtures/proposal_queue.json"))

  let(:proposal_queue) { ProposalQueue.new(name: "queue", properties: queue) }
  let(:proposal_queue_empty) do
    ProposalQueue.new(name: "queue", properties: { "proposal_queue" => [] })
  end
  let(:proposal_queue_internals) { proposal_queue.properties["proposal_queue"] }

  context "proposal is queued" do
    describe "properties" do
      it "has properties" do
        expect(proposal_queue.properties).to_not be_empty
      end

      it "is of type Hash" do
        expect(proposal_queue.properties).to be_a(Hash)
      end

      describe "proposal queue property internals" do
        it "is of type Array" do
          expect(proposal_queue_internals).to be_an(Array)
        end

        it "has a barclamp, proposal relation, elements and dependencies" do
          proposal_queue_internals.each do |bc|
            expect(bc["barclamp"]).to_not be_empty
            expect(bc["inst"]).to_not be_empty
            expect(bc["elements"]).to_not be_empty
            expect(bc["deps"]).to_not be_empty
          end
        end

        it "has the correct data types" do
          proposal_queue_internals.each do |bc|
            expect(bc["barclamp"]).to be_a(String)
            expect(bc["inst"]).to be_a(String)
            expect(bc["elements"]).to be_a(Hash)
            expect(bc["deps"]).to be_an(Array)
          end
        end
      end
    end
  end

  context "no proposal is queued" do
    describe "properties" do
      it "has an empty proposal_queue Array" do
        expect(proposal_queue_empty.properties["proposal_queue"]).to be_empty
      end
    end
  end
end
