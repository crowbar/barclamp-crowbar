class CreateAdminNode < ActiveRecord::Migration
  def up
    name = %x{hostname --fqdn}.strip
    name += '.cr0wbar.com' unless name =~ Node::FQDN_RE
    admin = Node.find_or_initialize_by_name(name)
    return if admin.persisted?
    admin.alias = admin.name
    admin.description = "Crowbar Admin Node"
    admin.state = "ready"
    admin.admin = true
    admin.allocated = true
    admin.save!
  end
end
