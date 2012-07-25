class CreateNodes < ActiveRecord::Migration
  def change
    create_table :nodes do |t|
      t.string :name
      t.timestamps
    end
  end
end
