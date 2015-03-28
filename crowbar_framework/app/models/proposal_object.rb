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
  include Crowbar::ProposalMethods

  self.chef_type = "data_bag_item"

  BC_PREFIX = 'bc-template-'

  def self.find_data_bag_item(bag)
    begin
      bag = ProposalObject.new(Chef::DataBag.load bag)  #should use new syntax
      return bag
    rescue Errno::ECONNREFUSED => e
      raise Crowbar::Error::ChefOffline.new
    rescue StandardError => e
      return nil
    end
  end
  
  def self.find(search)
    props = [] 
    begin
      arr = ChefObject.query_chef.search("crowbar", "id:#{chef_escape(search)}") 
      if arr[2] != 0
        props = arr[0].map do |x| 
          ProposalObject.new x 
        end
        props.delete_if { |x| x.nil? or x.item.nil? }
      end
    rescue StandardError => e
       Rails.logger.error("Could not recover Chef Crowbar data searching for '#{search}' due to '#{e.inspect}'")
    end
    return props
  end
    
  def self.all
    self.find 'bc-*'
  end

  def self.select_proposals(barclamp, all = ProposalObject.all)
    all.select { |p| p.id =~ /^bc-#{barclamp}-.*/ }
  end

  def self.find_proposals(barclamp)
    self.find "bc-#{barclamp}-*"
  end

  def self.find_barclamp(barclamp)
    self.find_proposal_by_id "bc-template-#{barclamp}"
  end

  def self.find_proposal(barclamp, name)
    self.find_proposal_by_id "bc-#{barclamp}-#{name}"
  end

  def self.find_proposal_by_id(id)
    val = begin
      Chef::DataBag.load "crowbar/#{id}"
    rescue Errno::ECONNREFUSED => e
      raise Crowbar::Error::ChefOffline.new
    rescue StandardError => e
      Rails.logger.warn("Could not recover Chef Crowbar Data on load #{id}: #{e.inspect}")
      nil
    end
    return val.nil? ? nil : ProposalObject.new(val)
  end

  def initialize(x)
    @item = x
  end

  def id
    @item['id']
  end

  def raw_data
    item.raw_data
  end

  def raw_data=(value)
    item.raw_data = value
  end

  def export
    super("crowbar-bc-#{barclamp}-#{name}")
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

  def save(options = {})
    self.latest_applied = !!options[:applied]
    increment_crowbar_revision!
    Rails.logger.debug("Saving data bag item: #{@item["id"]} - #{crowbar_revision}")
    @item.save
    save_proposal_in_sqlite
    Rails.logger.debug("Done saving data bag item: #{@item["id"]} - #{crowbar_revision}")
  end

  def destroy
    Rails.logger.debug("Destroying data bag item: #{@item["id"]} - #{crowbar_revision}")
    @item.destroy(@item.data_bag, @item["id"])
    delete_proposal_from_sqlite
    Rails.logger.debug("Done removal of data bag item: #{@item["id"]} - #{crowbar_revision}")
  end
  
  private

  def delete_proposal_from_sqlite
    attrs = { barclamp: self.barclamp, name: self.name }

    prop = Proposal.where(attrs).first
    prop.destroy if prop
  end

  def save_proposal_in_sqlite
    attrs = { barclamp: self.barclamp, name: self.name }

    prop = Proposal.where(attrs).first_or_initialize(attrs)
    prop.update(attrs.merge(properties: @item.raw_data)) if @item.raw_data != prop.raw_data
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

  
  # 'array' is the unsorted set of objects
  # 'att_sym' is the symbol of the attribute each object in array, that is represented in index_array
  # 'index_array' is the ordered array of values
  def sort_with_index(array, att_sym, index_array)
    return array.sort do |a, b|
      index_array.index(a.send(att_sym)).to_i <=> index_array.index(b.send(att_sym)).to_i
    end
  end
end
