# Copyright 2011, Dell 
# 
# Licensed under the Apache License, Version 2.0 (the "License"); 
# you may not use this file except in compliance with the License. 
# You may obtain a copy of the License at 
# 
#  http://www.apache.org/licenses/LICENSE-2.0 
# 
# Unless required by applicable law or agreed to in writing, software 
# distributed under the License is distributed on an "AS IS" BASIS, 
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
# See the License for the specific language governing permissions and 
# limitations under the License. 
# 
# Author: RobHirschfeld 
# 
#
# Also functions as a data bag item wrapper as well.
#
class ProposalObject < ChefObject

  BC_PREFIX = 'bc-template-'
  
  def self.find_data_bag_item(bag)
    begin
      chef_init #elimiate
      bag = ProposalObject.new(Chef::DataBag.load bag)  #should use new syntax
      return bag
    rescue
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
    rescue Exception => e
       Rails.logger.error("Could not recover Chef Crowbar data searching for '#{search}' due to '#{e.inspect}'")
    end
    return props
  end
    
  def self.all
    self.find 'bc-*'
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
    val = ChefObject.crowbar_data(id)
    return val.nil? ? nil : ProposalObject.new(val)
  end

  def self.human_attribute_name(attrib)
    #remove if possible, do in the view
    Rails.logger.info("please rewrite to not using I18n in models!  Use in views.")
    I18n.t attrib, :scope => "model.attributes.proposal"
  end

  def item
    @item
  end

  def id
    @item['id']
  end
  
  def name
    @item.name[/crowbar_bc-(.*)-(.*)$/,2]
  end
  
  def barclamp
    @item.name[/crowbar_bc-(.*)-(.*)$/,1]
  end

  #NOTE: Status is NOT accurate if the proposal has been deactivated!  You must check the role.
  def status
    bc = @item["deployment"][self.barclamp]
    if bc.nil?
      "hold"
    else
      return "unready" if bc.has_key? "crowbar-committing" and bc["crowbar-committing"]
      return "pending" if bc.has_key? "crowbar-queued" and bc["crowbar-queued"]
      return "hold" if !bc.has_key? "crowbar-queued" and !bc.has_key? "crowbar-committing"
      if !@item["deployment"][self.barclamp].key? "crowbar-status" or @item["deployment"][self.barclamp]["crowbar-status"] === "success"
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
  
  def active?
    inst = "#{barclamp}-config-#{name}"
    role = RoleObject.find_role_by_name(inst)
    return role.nil?
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

  def self.write(raw_data)
    # Write out chef object.
    data_bag_item = Chef::DataBagItem.new
    begin
      data_bag_item.raw_data = raw_data
      data_bag_item.data_bag "crowbar"

      prop = ProposalObject.new data_bag_item
      prop.save
      Rails.logger.info "saved proposal"
      [200, {}]
    rescue Net::HTTPServerException => e
      [e.response.code, {}]
    rescue Chef::Exceptions::ValidationFailed => e2
      [400, e2.message]
    end
  end

  def save
    return if DISABLE_CHEF
    ChefObject.chef_init
    @item["deployment"] = {} if @item["deployment"].nil?
    @item["deployment"][barclamp] = {} if @item["deployment"][barclamp].nil?
    if @item["deployment"][barclamp]["crowbar-revision"].nil?
      @item["deployment"][barclamp]["crowbar-revision"] = 0
    else
      @item["deployment"][barclamp]["crowbar-revision"] = @item["deployment"][barclamp]["crowbar-revision"] + 1
    end
    Rails.logger.debug("Saving data bag item: #{@item["id"]} - #{@item["deployment"][barclamp]["crowbar-revision"]}")
    @item.save
    Rails.logger.debug("Done saving data bag item: #{@item["id"]} - #{@item["deployment"][barclamp]["crowbar-revision"]}")
  end

  def destroy
    ChefObject.chef_init
    Rails.logger.debug("Destroying data bag item: #{@item["id"]} - #{@item["deployment"][barclamp]["crowbar-revision"]}")
    @item.destroy(@item.data_bag, @item["id"])
    Rails.logger.debug("Done removal of data bag item: #{@item["id"]} - #{@item["deployment"][barclamp]["crowbar-revision"]}")
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
