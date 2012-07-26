class CreateProposals < ActiveRecord::Migration
  def change
    #####
    #  proposals, attached to barclamps.
    #  proposal_configs hold the actual date, with revision counts and node references.
    create_table :proposals do |t|
      t.references  :barclamp
      t.string      :name
      t.integer     :status
      t.integer     :last_applied_rev
      t.timestamps
    end
  end
end
