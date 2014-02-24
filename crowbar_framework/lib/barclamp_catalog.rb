require 'singleton'

class BarclampCatalog
  include Singleton

  DEFAULT_CATEGORY   = "Unknown"
  DEFAULT_RUN_ORDER  = 1000
  DEFAULT_CHEF_ORDER = 1000

  attr_reader :categories, :catalog

  def initialize
    @catalog    = YAML.load_file(Rails.root.join('config', 'catalog.yml'))
    @categories = build_categories
  end

  def self.method_missing(method, *args, &block)
    BarclampCatalog.instance.send(method, *args, &block)
  end

  def barclamps
    catalog['barclamps']
  end

  def chef_order(barclamp)
    return DEFAULT_CHEF_ORDER if barclamp.nil?
    order = (barclamps[barclamp]["order"] || DEFAULT_CHEF_ORDER) rescue DEFAULT_CHEF_ORDER
    (barclamps[barclamp]["chef_order"] || order) rescue order
  end

  def run_order(barclamp)
    return DEFAULT_RUN_ORDER if barclamp.nil?
    order = (barclamps[barclamp]["order"] || DEFAULT_RUN_ORDER) rescue DEFAULT_RUN_ORDER
    (barclamps[barclamp]["run_order"] || order) rescue order
  end

  def members(barclamp)
    barclamps[barclamp].nil? ? [] : barclamps[barclamp]['members']
  end

  def category(barclamp)
    value = @categories.map do |parent, members|
      next unless members.include? barclamp
      barclamps[parent]["display"]
    end
    value.compact.first || DEFAULT_CATEGORY
  end

  def display_name(barclamp)
    display = barclamps[barclamp]['display']

    if display.nil? or display.empty?
      barclamp.titlecase
    else
      display
    end
  end

  private

  def build_categories
    {}.tap do |result|
      barclamps.each do |barclamp, attrs|
        next if attrs["members"].nil?
        result[barclamp] = attrs["members"].keys
      end
    end
  end
end

