class DashboardController < ApplicationController
  before_filter :load_records

  def clusters
    @clusters = ServiceObject.available_clusters
  end

  def active_roles
    @assigned_roles = RoleObject.assigned(@roles).reject { |r| r.proposal? || r.core_role? || r.ha? }.sort_by(&:name)
  end

  private

  def load_records
    @nodes     = NodeObject.all
    @roles     = RoleObject.all
    @proposals = ProposalObject.all
  end
end
