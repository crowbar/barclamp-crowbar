class CreateProposalConfigs < ActiveRecord::Migration
  def change
    create_table :proposal_configs do |t|
      t.belongs_to     :proposal     
      t.text           :config
      t.integer        :revision      
      t.belongs_to     :proposal
      t.timestamps
    end
    
    create_table :proposal_configs_proposals do |t|
      t.references    :proposal
      t.references    :proposal_config
    end

    create_table :node_role_proposal_config do |t|
      t.references    :role
      t.references    :proposal_config
    end

  end
end
