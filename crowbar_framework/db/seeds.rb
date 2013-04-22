# This file should contain all the record creation needed to seed the database with its default values.
# The data can then be loaded with the rake db:seed (or created alongside the db with db:setup).

include ActiveSupport::Benchmarkable

def logger
  @logger ||= Logger.new(STDOUT)
end

# ----------------------------
# Create groups
# ----------------------------

benchmark('Group.delete_all') do
  Group.delete_all
end

benchmark('Create groups') do
  @rack01 = Group.create!(:name => 'Rack01',     :description => 'First rack')
  @rack02 = Group.create!(:name => 'Rack02',     :description => 'Second rack')
  @prod   = Group.create!(:name => 'Production', :description => 'Production servers')
end

# ----------------------------
# Create nodes
# ----------------------------

benchmark('Node.delete_all') do
  Node.delete_all
end

benchmark('Create nodes') do
  for i in 1..2 do
    node = Node.create!(
      :name        => "node%02d.crowbar.com" % i,
      :description => "Load balancer #%d" % i
    )
    node.groups << @rack01
    node.groups << @prod
  end

  for i in 1..3 do
    node = Node.create!(
      :name        => "node%02d.crowbar.com" % (i+2),
      :description => "Database server #%d" % i
    )
    node.groups << @rack01
    node.groups << @prod
  end

  for i in 1..5 do
    node = Node.create!(
      :name        => "node%02d.crowbar.com" % (i+5),
      :description => "Application server #%d" % i,
    )
    node.groups << @rack02
    node.groups << @prod
  end
end
