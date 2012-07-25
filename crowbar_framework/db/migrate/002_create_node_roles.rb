class CreateNodeRoles < ActiveRecord::Migration
  def change
    create_table :node_roles do |t|
      t.integer      :status
      t.belongs_to   :role
      t.belongs_to   :node
      t.timestamps
    end
  end
end
