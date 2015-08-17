class ProposalQueue < ActiveRecord::Base
  # FIXME: remove this when the export is properly implemented
  class_attribute :chef_type

  self.chef_type = "data_bag_item"

  serialize :properties, Utils::JSONWithIndifferentAccess
  scope :ordered, lambda {
    order("proposal_queues.created_at ASC")
  }
end
