class Proposal < ActiveRecord::Base
  serialize :properties, JSON
end
