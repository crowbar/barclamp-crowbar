class AddAliasToNode < ActiveRecord::Migration
  def change
    add_column :nodes, :alias, :string
  end
end
