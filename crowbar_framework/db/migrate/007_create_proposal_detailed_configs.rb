class CreateProposalDetailedConfigs < ActiveRecord::Migration
  def change
    create_table :proposal_detailed_configs do |t|
      t.string :type
      t.integer :ref_id

      t.timestamps
    end
  end
end
