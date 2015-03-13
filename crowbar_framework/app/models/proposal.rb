class Proposal < ActiveRecord::Base
  serialize :properties, JSON

  validates :name, :barclamp, :properties, :presence => true
end
