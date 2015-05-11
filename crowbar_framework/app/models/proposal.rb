class Proposal < ActiveRecord::Base
  include Crowbar::ProposalMethods

  # FIXME: remove this when the export is properly implemented
  class_attribute :chef_type

  self.chef_type = "data_bag_item"

  class TemplateMissing < StandardError; end
  class TemplateInvalid < StandardError; end

  serialize :properties, Utils::JSONWithIndifferentAccess

  validates :barclamp, :properties, presence: true
  validates :name, uniqueness: { scope: :barclamp, message: I18n.t('model.service.name_exists') }
  validates :name, presence: { message: I18n.t('model.service.too_short') }
  validate  :name, :name_without_invalid_chars
  validate  :name, :name_not_on_blacklist

  after_initialize :load_properties_template, :set_default_name
  before_save      :update_proposal_id

  # FIXME: these are safe to remove once all barclamps are converted to use
  # Proposal instead of ProposalObject
  before_destroy :drop_corresponding_proposal_object
  before_save    :update_corresponding_proposal_object

  # XXX: a 'registered' barclamp could have a has_many :proposals and have a factory
  # method for creating them. Then the check for barclamp arg would not be needed,
  # as we'd always know the barclamp exists.
  def initialize(attributes = nil, options = {})
    raise ArgumentError.new("Barclamp attribute is required") unless attributes && attributes.key?(:barclamp)

    super
  end

  def self.find_proposals(barclamp)
    where(:barclamp => barclamp)
  end

  def self.find_barclamp(barclamp)
    self.new(:barclamp => barclamp, :name => "template")
  end

  def self.find_proposal(barclamp, name)
    where(:barclamp => barclamp, :name => name).first
  end

  # XXX: the networks will still be backed by ProposalObject for now,
  # so it is not neccessary to handle them here.
  # We still need to handle lookups for templates, though.
  def self.find_proposal_by_id(id)
    _, barclamp_or_template, name = *id.split("-")
    if barclamp_or_template == "template"
      self.new(:barclamp => name, :name => "template")
    else
      where(:barclamp => barclamp_or_template, :name => name).first
    end
  end

  def to_json
    self.properties.to_json
  end

  # FIXME: equivalent to ProposalObject.id
  def key
    if name == "template"
      "bc-#{self.name}-#{self.barclamp}"
    else
      "bc-#{self.barclamp}-#{self.name}"
    end
  end

  def export
    ChefObject.export(self)
  end

  # FIXME: this is not correct, the item of ProposalObject returns
  # couchdb/serialization related attributes. This is equivalent to raw_data
  # instead.
  def item
    self.properties
  end

  def raw_data
    self.properties
  end

  def raw_data=(value)
    self.properties = value
  end

  private

  # XXX: we need to be careful to not create an endless loop
  def update_corresponding_proposal_object
    # XXX: Do not save other network proposals - its a network configuration
    # this is transitional code, as soon as network uses databags, this can go away.
    return if self.barclamp == "network" && self.name != "default"

    proposal_object = ProposalObject.find_proposal_by_id(self.key)
    if proposal_object
      if proposal_object.raw_data != self.raw_data
        increment_crowbar_revision!
        proposal_object.raw_data = self.raw_data
        proposal_object.save(sync: false, update_revision: false)
      end
    else
      bag = Chef::DataBagItem.json_create({"raw_data" => self.raw_data, "data_bag" => "crowbar"})
      proposal_object = ProposalObject.new(bag)
      proposal_object.save(sync: false, update_revision: false)
    end
  end

  def drop_corresponding_proposal_object
    # XXX: Do not save other network proposals - its a network configuration
    # this is transitional code, as soon as network uses databags, this can go away.
    return if self.barclamp == "network" && self.name != "default"

    proposal_object = ProposalObject.find_proposal_by_id(self.key)
    proposal_object.destroy(sync: false) if proposal_object
  end

  def name_not_on_blacklist
    forbidden_names = ["template", "nodes", "commit", "status"]

    if forbidden_names.include?(self.name)
      self.errors.add(:name, I18n.t('model.service.illegal_name', names: forbidden_names.to_sentence))
    end
  end

  def name_without_invalid_chars
    if self.name =~ /[^A-Za-z0-9_]/
      self.errors.add(:name, I18n.t('model.service.illegal_chars'))
    end
  end

  def load_properties_template
    self.properties ||= Utils::JSONWithIndifferentAccess.load(File.read(properties_template_path))
  rescue Errno::ENOENT, Errno::EACCES
    raise TemplateMissing.new(I18n.t('model.service.template_missing', name: self.name))
  rescue JSON::ParserError
    raise TemplateInvalid.new(I18n.t('model.service.template_invalid', name: self.name))
  end

  def properties_template_path
    if Rails.env.production?
      Rails.root.join("../chef/data_bags/crowbar/bc-template-#{self.barclamp}.json").expand_path
    else
      # XXX: this assumes barclamps are cloned in the same directory
      Rails.root.join("../../barclamp-#{self.barclamp}/chef/data_bags/crowbar/bc-template-#{self.barclamp}.json").expand_path
    end
  end

  def set_default_name
    self.name ||= "template"
  end

  def update_proposal_id
    self.properties["id"] = "bc-#{self.barclamp}-#{self.name}"
  end
end
