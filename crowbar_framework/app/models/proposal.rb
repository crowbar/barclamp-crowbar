############
# A proposal is a configuration for a particular barclamp.
# It has a ""history"" of configurations that were created and applied.
# 

class Proposal < ActiveRecord::Base
  attr_accessible :name, :status, :last_applied_rev
  belongs_to :barclamp
  has_many  :proposal_config, :inverse_of => :proposal
end
