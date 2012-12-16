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
class RoleObject < ChefObject

  def self.all
    self.find_roles_by_search(nil)
  end
  
  def self.active(barclamp = nil, inst = nil)
    full = if barclamp.nil?
      RoleObject.find_roles_by_name "*-config-*"
    else
      RoleObject.find_roles_by_name "#{barclamp}-config-#{inst}"
    end
    full.map { |x| "#{x.barclamp}_#{x.inst}" }
  end
  
  def self.find_roles_by_name(name)
    roles = []
    #TODO this call could be moved to fild_roles_by_search
    arr = ChefObject.query_chef.search "role", "name:#{chef_escape(name)}"
    if arr[2] != 0
      roles = arr[0].map { |x| RoleObject.new x }
      roles.delete_if { |x| x.nil? or x.role.nil? }
    end
    roles
  end

  def self.find_roles_by_search(search)
    roles = []
    arr = if search.nil?
            ChefObject.query_chef.search "role"
          else
            ChefObject.query_chef.search "role", search
          end
    if arr[2] != 0
      roles = arr[0].map { |x| RoleObject.new x }
      roles.delete_if { |x| x.nil? or x.role.nil? }
    end
    roles
  end

  def self.find_role_by_name(name)
    begin
      chef_init
      return RoleObject.new Chef::Role.load(name)
    rescue
      return nil
    end
  end

  def self.human_attribute_name(attrib)
    I18n.t attrib, :scope => "model.attributes.role"
  end

  def self.proposal_hash_to_role(proposal, bc_name)
    role = Chef::Role.new
    role.name proposal["id"].gsub("bc-#{bc_name}-", "#{bc_name}-config-")
    role.description proposal["description"]
    role.default_attributes proposal["attributes"]
    role.override_attributes proposal["deployment"]
    r = RoleObject.new role
    r.save
  end

  def barclamp
    @role.name.split("-")[0]
  end

  def inst
    @role.name.gsub("#{self.barclamp}-config-", "")
  end

  def role
    @role
  end

  def name
    @role.name
  end

  def name=(value)
    @role.name value
  end

  def description
    @role.description
  end

  def description=(value)
    @role.description value
  end

  def default_attributes
    @role.default_attributes
  end

  def default_attributes=(value)
    @role.default_attributes value
  end

  def override_attributes
    @role.override_attributes
  end

  def override_attributes=(value)
    @role.override_attributes value
  end

  def initialize(x)
    @role = x
  end

  def save
    return if DISABLE_CHEF
    @role.override_attributes[barclamp] = {} if @role.override_attributes[barclamp].nil?
    if @role.override_attributes[barclamp]["crowbar-revision"].nil?
      @role.override_attributes[barclamp]["crowbar-revision"] = 0
    else
      @role.override_attributes[barclamp]["crowbar-revision"] = @role.override_attributes[barclamp]["crowbar-revision"] + 1
    end
    Rails.logger.debug("Saving role: #{@role.name} - #{@role.override_attributes[barclamp]["crowbar-revision"]}")
    @role.save
    Rails.logger.debug("Done saving role: #{@role.name} - #{@role.override_attributes[barclamp]["crowbar-revision"]}")
  end

  def destroy
    return if DISABLE_CHEF
    Rails.logger.debug("Destroying role: #{@role.name} - #{@role.override_attributes[barclamp]["crowbar-revision"]}")
    @role.destroy
    Rails.logger.debug("Done removing role: #{@role.name} - #{@role.override_attributes[barclamp]["crowbar-revision"]}")
  end

  def elements
    @role.override_attributes[self.barclamp]["elements"]
  end

  def run_list
    @role.run_list
  end

end

