class CreateAdminNode < ActiveRecord::Migration
  def change
    admin = Node.find_or_initialize_by_name(%x{hostname --fqdn}.strip)
    return if admin.persisted?
    admin.alias = admin.name
    admin.description = "Crowbar Admin Node"
    admin.state = "ready"
    admin.admin = true
    admin.allocated = true
    admin.save!
  end
end
