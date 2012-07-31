class CreateClawhammerRuns < ActiveRecord::Migration
  def change
    create_table :cmdb_runs do |t|
      t.references :cmdb_event

      t.timestamps
    end
    add_index :cmdb_runs, :cmdb_event_id
  end
end
