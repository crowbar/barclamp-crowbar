class CreateProposalQueues < ActiveRecord::Migration
  def change
    create_table :proposal_queues, id: false do |t|
      t.string :name
      t.text :properties
    end

    add_index :proposal_queues, :name, unique: true

    ProposalQueue.create(name: "queue", properties: { "proposal_queue" => [] })
  end
end
