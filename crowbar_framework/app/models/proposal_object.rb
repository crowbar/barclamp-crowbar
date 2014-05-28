#
# Copyright 2011-2013, Dell
# Copyright 2013-2014, SUSE LINUX Products GmbH
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

class ProposalObject < ChefObject
  attr_reader :item

  class << self
    include ChefFinders
    include Deprecate
  end

  def self.chef_type
    "crowbar"
  end

  def self.chef_class
    Chef::DataBag
  end

  def self.after_find_filter(proposals)
    proposals.compact.reject { |p| p.item.nil? }
  end

  BC_PREFIX = 'bc-template-'

  def self.find_data_bag_item(bag)
    deprecate_warning("load(bag)", __FILE__, __LINE__)
    load(bag)
  end

  def self.find(search)
    deprecate_warning("where(:id => search)", __FILE__, __LINE__)
    where(:id => search)
  end

  def self.all
    where(:id => 'bc-*')
  end

  def self.select_proposals(barclamp, all = ProposalObject.all)
    all.select { |p| p.id =~ /^bc-#{barclamp}-.*/ }
  end

  def self.find_proposals(barclamp)
    where(:id => "bc-#{barclamp}-*")
  end

  def self.find_proposal(barclamp, name)
    load("crowbar/bc-#{barclamp}-#{name}")
  end

  def self.find_barclamps
    where(:id => "bc-tempate-*")
  end

  def self.find_barclamp(barclamp)
    load("crowbar/bc-template-#{barclamp}")
  end

  def self.find_proposal_by_id(id)
    deprecate_warning('load("crowbar/#{id}")', __FILE__, __LINE__)
    load("crowbar/#{id}")
  end

  def raw_attributes
    @raw_attributes ||= begin
      raw_data["attributes"][barclamp] || {}
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

  def id
    @item['id']
  end

  def name
    match = @item.name.match(/crowbar_bc-(.*)-(.*)$/)
    if match.nil?
      match = @item.name.match(/crowbar_(.*)_network$/)
      match.nil? ? "" : match[1]
    else
      match[1] == 'template' ? match[1] : match[2]
    end
  end

  def barclamp
    match = @item.name.match(/crowbar_bc-(.*)-(.*)$/)
    if match.nil?
      match = @item.name.match(/crowbar_(.*)_network$/)
      match.nil? ? "" : "network"
    else
      match[1] == 'template' ? match[2] : match[1]
    end
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
    bc = @item["deployment"][self.barclamp]
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
  
  # nil if not appliciable, true = if success, false if failed
  def failed?
     status === 'failed'
  end

  # for locationlization, will lookup text before the :  
  def fail_reason
     s = if failed?
       @item["deployment"][self.barclamp]["crowbar-failed"].to_s
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
    @item['description']
  end
  
  def elements
    @item.raw_data['deployment'][self.barclamp]["elements"]
  end

  def all_elements
    @item.raw_data['deployment'][self.barclamp]["element_order"].flatten.uniq
  end

  def role
    RoleObject.find_role_by_name("#{barclamp}-config-#{name}")
  end

  def crowbar_revision
    @item["deployment"][barclamp]["crowbar-revision"].to_i rescue 0
  end

  def latest_applied?
    @item["deployment"][barclamp]["crowbar-applied"] rescue false
  end

  def latest_applied=(applied)
    @item["deployment"] ||= {}
    @item["deployment"][barclamp] ||= {}
    @item["deployment"][barclamp]["crowbar-applied"] = applied
  end

  def active?
    !role.nil?
  end

  def raw_data
    @item.raw_data
  end

  def raw_data=(value)
    @item.raw_data = value
  end

  def [](attrib)
    @item[attrib]
  end

  def []=(attrib, value)
    @item[attrib] = value
  end

  def initialize(x)
    @item = x
  end

  def increment_crowbar_revision!
    @item["deployment"] ||= {}
    @item["deployment"][barclamp] ||= {}
    if @item["deployment"][barclamp]["crowbar-revision"].nil?
      @item["deployment"][barclamp]["crowbar-revision"] = 0
    else
      @item["deployment"][barclamp]["crowbar-revision"] += 1
    end
  end

  def save(options = {})
    self.latest_applied = !!options[:applied]
    increment_crowbar_revision!
    Rails.logger.debug("Saving data bag item: #{@item["id"]} - #{crowbar_revision}")
    @item.save
    Rails.logger.debug("Done saving data bag item: #{@item["id"]} - #{crowbar_revision}")
  end

  def destroy
    Rails.logger.debug("Destroying data bag item: #{@item["id"]} - #{crowbar_revision}")
    @item.destroy(@item.data_bag, @item["id"])
    Rails.logger.debug("Done removal of data bag item: #{@item["id"]} - #{crowbar_revision}")
  end
  
  def export
    super("crowbar-bc-#{barclamp}-#{name}")
  end
  
  private
  
  # 'array' is the unsorted set of objects
  # 'att_sym' is the symbol of the attribute each object in array, that is represented in index_array
  # 'index_array' is the ordered array of values
  def sort_with_index(array, att_sym, index_array)
    return array.sort do |a, b|
      index_array.index(a.send(att_sym)).to_i <=> index_array.index(b.send(att_sym)).to_i
    end
  end
end
