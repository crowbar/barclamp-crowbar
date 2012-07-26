class CreateProposalSampleConfigs < ActiveRecord::Migration
  def change
    create_table :proposal_sample_configs do |t|
      t.integer :count

      t.timestamps
    end
  end
end
