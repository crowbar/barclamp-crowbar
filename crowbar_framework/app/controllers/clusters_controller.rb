class ClustersController < ApplicationController
  before_filter :load_records

  def index
    @clusters = ServiceObject.available_clusters

    @unallocated_nodes = NodeObject.unallocated(@nodes).sort_by(&:name)
    @unassigned_nodes  = NodeObject.unassigned(@nodes).sort_by(&:name)

    @assigned_roles   = core_roles_filter(RoleObject.assigned(@roles))
    @unassigned_roles = core_roles_filter(RoleObject.unassigned(@roles))

    @compute_roles    = RoleObject.compute_roles(@roles).sort_by(&:name)
  end

  def roles
    @unassigned_nodes = NodeObject.unassigned(@nodes).sort_by(&:name)

    @unassigned_roles = core_roles_filter(RoleObject.unassigned(@roles))
    @assigned_roles   = core_roles_filter(RoleObject.assigned(@roles))
  end

  private

  def core_roles_filter(roles)
    roles.reject { |r| r.proposal? || r.core_role? || r.ha? }.sort_by(&:name)
  end


  def load_records
    @nodes     = NodeObject.all
    @roles     = RoleObject.all
    @proposals = ProposalObject.all
  end
end
