class CreateProposals < ActiveRecord::Migration
  def change
    create_table :proposals do |t|
      t.string :barclamp, :null => false
      t.string :name, :null => false
      t.text :properties
    end

    add_index :proposals, [:barclamp, :name], :unique => true
  end
end
