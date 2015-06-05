class ProposalVersion < ActiveRecord::Base
  belongs_to :proposal

  serialize :properties, Utils::JSONWithIndifferentAccess

  def reify
    Proposal.new(barclamp: self.barclamp, name: self.name, properties: self.properties)
  end

  def live?
    !proposal.nil?
  end

  def previous
    others = self.class.for(self.proposal_id)
    idx    = others.index(self)
    idx == 0 ? nil : others[idx-1]
  end

  def next
    others = self.class.for(self.proposal_id)
    idx    = others.index(self)
    idx == others.length - 1 ? nil : others[idx+1]
  end

  def self.for(proposal_id)
    where(proposal_id: self.proposal_id).order('id asc')
  end
end
