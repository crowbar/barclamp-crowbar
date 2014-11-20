class DashboardController < ApplicationController
  before_filter :load_records

  def clusters
    @clusters = ServiceObject.available_clusters
  end

  def active_roles
    @assigned_roles = RoleObject.assigned(@roles).reject do |r|
      r.proposal? || r.core_role? || r.ha?
    end.group_by do |r|
      r.barclamp
    end.sort_by do |barclamp, roles|
      barclamp
    end
  end

  private

  def load_records
    @nodes     = NodeObject.all
    @roles     = RoleObject.all
    @proposals = ProposalObject.all
  end
end
