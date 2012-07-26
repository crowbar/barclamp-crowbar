####################
#  Barclamp proposals can use the generic Proposal config, which provides a general hash
#  storage for proposal configuration.
#  Barclamps which store their configuration in a more normailized fashion should subclass
#  this base class.
#

class ProposalDetailedConfig < ActiveRecord::Base
  attr_accessible :ref_id, :type
end
