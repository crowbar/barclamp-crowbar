class Proposal < ActiveRecord::Base
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

  # XXX: a 'registered' barclamp could have a has_many :proposals and have a factory
  # method for creating them. Then the check for barclamp arg would not be needed,
  # as we'd always know the barclamp exists.
  def initialize(attributes = nil, options = {})
    raise ArgumentError.new("Barclamp attribute is required") unless attributes && attributes.key?(:barclamp)

    super
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

  def increment_crowbar_revision!
    item["deployment"] ||= {}
    item["deployment"][self.barclamp] ||= {}
    if item["deployment"][self.barclamp]["crowbar-revision"].nil?
      item["deployment"][self.barclamp]["crowbar-revision"] = 0
    else
      item["deployment"][self.barclamp]["crowbar-revision"] += 1
    end
  end

  def raw_attributes
    @raw_attributes ||= begin
      raw_data["attributes"][self.barclamp] || {}
    end
  end

  def pretty_attributes
    @pretty_attributes ||= begin
      Utils::ExtendedHash.new(
        raw_attributes.dup
      )
    end
  end

  def pretty_attributes_json
    JSON.pretty_generate(
      JSON.parse(
        (raw_data["attributes"][barclamp] || {}).to_json
      )
    )
  end

  def raw_deployment
    @raw_deployment ||= begin
      raw_data["deployment"][barclamp] || {}
    end
  end

  def pretty_deployment
    @pretty_deployment ||= begin
      Utils::ExtendedHash.new(
        raw_data["deployment"][barclamp].dup
      )
    end
  end

  def pretty_deployment_json
    JSON.pretty_generate(
      JSON.parse(
        (raw_data["deployment"][barclamp] || {}).to_json
      )
    )
  end

  def category
    @category ||= BarclampCatalog.category(barclamp)
  end

  def prop
    [barclamp, name].join("_")
  end

  def display_name
    @display_name ||= BarclampCatalog.display_name(barclamp)
  end

  def allow_multiple_proposals?
    ServiceObject.get_service(barclamp).allow_multiple_proposals?
  end

  #NOTE: Status is NOT accurate if the proposal has been deactivated!  You must check the role.
  def status
    bc = item["deployment"][self.barclamp]
    if bc.nil?
      "hold"
    else
      return "unready" if bc["crowbar-committing"]
      return "pending" if bc["crowbar-queued"]
      return "hold" if !bc.has_key? "crowbar-queued" and !bc.has_key? "crowbar-committing"
      if !bc.key? "crowbar-status" or bc["crowbar-status"] === "success"
        "ready"
      else
        "failed"
      end
    end
  end
  
  # nil if not applicable, true = if success, false if failed
  def failed?
     status === 'failed'
  end

  # for localization, will lookup text before the :  
  def fail_reason
     s = if failed?
       item["deployment"][self.barclamp]["crowbar-failed"].to_s
     elsif status === "ready"
       "Did not fail.  Successfully applied: #{barclamp}-#{name} (status #{status})"
     else
       "No success information for proposal: #{barclamp}-#{name} (status #{status})"
     end
     out = s.split(":")
     out[0] = I18n.t out[0], :default=> out[0]
     return out.join(":").to_s
  end
  
  def description
    item['description']
  end
  
  def elements
    raw_data['deployment'][self.barclamp]["elements"]
  end

  def all_elements
    raw_data['deployment'][self.barclamp]["element_order"].flatten.uniq
  end

  def role
    RoleObject.find_role_by_name("#{barclamp}-config-#{name}")
  end

  def crowbar_revision
    item["deployment"][barclamp]["crowbar-revision"].to_i rescue 0
  end

  def latest_applied?
    item["deployment"][barclamp]["crowbar-applied"] rescue false
  end

  def latest_applied=(applied)
    item["deployment"] ||= {}
    item["deployment"][barclamp] ||= {}
    item["deployment"][barclamp]["crowbar-applied"] = applied
  end

  def active?
    !role.nil?
  end

  def [](attrib)
    item[attrib]
  end

  def []=(attrib, value)
    item[attrib] = value
  end

  private

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
    raise TemplateMissing.new(I18n.t('model.service.template_missing', name: self.barclamp))
  rescue JSON::ParserError
    raise TemplateInvalid.new(I18n.t('model.service.template_invalid', name: self.barclamp))
  end

  def properties_template_path
    properties_template_dir.join("bc-template-#{self.barclamp}.json").expand_path
  end

  def properties_template_dir
    Rails.root.join("../chef/data_bags/crowbar/")
  end

  def set_default_name
    self.name ||= "template"
  end

  def update_proposal_id
    self.properties["id"] = "bc-#{self.barclamp}-#{self.name}"
  end
end
