class CreateProposalQueues < ActiveRecord::Migration
  def change
    create_table :proposal_queues do |t|
      t.string :barclamp
      t.string :name
      t.text :properties
      t.datetime :created_at
    end
  end
end
