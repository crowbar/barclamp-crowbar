class CreateClawhammers < ActiveRecord::Migration
  def change
    create_table :cmdbs do |t|
      t.string :name

      t.timestamps
    end
  end
end
