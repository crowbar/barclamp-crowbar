class CreateProposalVersions < ActiveRecord::Migration
  def change

    create_table :proposal_versions do |t|
      t.integer  :proposal_id,   :null => false

      t.string   :event,         :null => false

      t.string   :barclamp,      :null => false
      t.string   :name,          :null => false
      t.text     :properties

      t.datetime :created_at
    end

    add_index :proposal_versions, [:proposal_id]
  end
end
