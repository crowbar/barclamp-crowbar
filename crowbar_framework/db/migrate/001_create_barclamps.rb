class CreateBarclamps < ActiveRecord::Migration
  def change
    create_table :barclamps do |t|
      t.string :name
      t.string :version
      t.string :group
      t.string :description
      t.timestamps
    end

    create_table :barclamp_dependencies, :id=>false do |t|
      t.integer  :prereq_id
      t.integer  :barclamp_id
    end
  end
end
