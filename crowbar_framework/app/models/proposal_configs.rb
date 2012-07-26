#######
#  configuration details for a given proposals.
#  proposal 
class ProposalConfig < ActiveRecord::Base
  attr_accessible :config
  attr_accessible :reversion
  belongs_to      :proposal, :inverse_of => :proposal_config
  has_many        :node_role
  has_many        :node, :through => :node_role
  has_one         :proposal_detailed_config
end
