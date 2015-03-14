class Proposal < ActiveRecord::Base
  # FIXME: add proper i18n to errors

  class TemplateMissing < StandardError; end
  class TemplateInvalid < StandardError; end

  serialize :properties, JSON

  validates :name, :barclamp, :properties, :presence => true
  validate  :name, :name_not_on_blacklist
  validates :name, :uniqueness => { :scope => :barclamp }

  after_initialize :load_properties_template
  before_save      :update_proposal_id

  # XXX: a 'registered' barclamp could have a has_many :proposals and have a factory
  # method for creating them. Then the check for barclamp arg would not be needed,
  # as we'd always know the barclamp exists.
  def initialize(attributes = nil, options = {})
    raise ArgumentError.new("Barclamp attribute is required") unless attributes && attributes.key?(:barclamp)

    super
  end

  private

  def name_not_on_blacklist
    forbidden_names = ["template", "nodes", "commit", "status"]

    if forbidden_names.include?(self.name)
      self.errors.add(:name, "Name cannot be any of #{forbidden_names.to_sentence}.")
    end
  end

  def load_properties_template
    self.properties ||= JSON.parse(File.read(properties_template_path))
  rescue Errno::ENOENT, Errno::EACCES
    raise TemplateMissing.new("Proposal template is missing or not readable for #{self.barclamp}. Please create #{properties_template_path}.")
  rescue JSON::ParserError
    raise TemplateInvalid.new("Please make sure template for #{self.barclamp} in #{properties_template_path} contains valid JSON.")
  end

  def properties_template_path
    Rails.root.join("../../barclamp-#{self.barclamp}/chef/data_bags/crowbar/bc-template-#{self.barclamp}.json").expand_path
  end

  def update_proposal_id
    self.properties["id"] = "bc-#{self.barclamp}-#{self.name}"
  end
end
