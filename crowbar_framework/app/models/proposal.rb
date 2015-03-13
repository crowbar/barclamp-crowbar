class Proposal < ActiveRecord::Base
  serialize :properties, JSON

  validates :name, :barclamp, :properties, :presence => true
  validate  :name, :name_not_on_blacklist
  validates :name, :uniqueness => { :scope => :barclamp }

  private

  def name_not_on_blacklist
    forbidden_names = ["template", "nodes", "commit", "status"]

    if forbidden_names.include?(self.name)
      self.errors.add(:name, "Name cannot be any of #{forbidden_names.to_sentence}.")
    end
  end
end
