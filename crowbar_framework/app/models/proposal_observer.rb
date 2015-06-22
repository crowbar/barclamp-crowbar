class ProposalObserver < ActiveRecord::Observer
  def after_create(proposal)
    record_version(proposal, "create")
  end

  def after_update(proposal)
    record_version(proposal, "update")
  end

  def after_destroy(proposal)
    record_version(proposal, "destroy")
  end

  private

  def record_version(proposal, action)
    return if !proposal.destroyed? && !proposal.changed?

    ProposalVersion.create!(event: action, proposal_id: proposal.id, name: proposal.name, barclamp: proposal.barclamp, properties: proposal.properties)
  end
end

