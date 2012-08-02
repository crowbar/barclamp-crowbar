class CreateCmdbAttributes < ActiveRecord::Migration
  def change
    create_table :cmdb_attributes do |t|
      t.string :name
      t.string :description
      t.string :order
      t.text :value
      t.integer :revision
      t.references :node_attribute
      t.references :cmdb_run
      t.references :cmdb_map

      t.timestamps
    end
    add_index :cmdb_attributes, :node_attribute_id
    add_index :cmdb_attributes, :cmdb_run_id
    add_index :cmdb_attributes, :cmdb_map_id
  end
end
