class CreateClawhammerEvents < ActiveRecord::Migration
  def change
    create_table :cmdb_events do |t|
      t.string :name
      t.text :attributes
      t.string :status
      t.string :result
      t.references :cmdb_run
      t.references :node
      t.string :direction
      t.string :type

      t.timestamps
    end
    add_index :cmdb_events, :node_id, :cmdb_run_id
  end
end
