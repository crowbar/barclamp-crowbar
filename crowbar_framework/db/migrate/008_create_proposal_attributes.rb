class CreateProposalAttributes < ActiveRecord::Migration
  def change
    create_table :proposal_attributes do |t|
      t.string :name
      t.string :value

      t.timestamps
    end
  end
end
