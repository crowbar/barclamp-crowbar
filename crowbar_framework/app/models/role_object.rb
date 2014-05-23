# Copyright 2011-2013, Dell
# Copyright 2013, SUSE LINUX Products GmbH
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
# Author: Rob Hirschfeld
# Author: SUSE LINUX Products GmbH
#

class RoleObject < ChefObject
  self.chef_type = "role"

  def self.all
    self.find_roles_by_search(nil)
  end

  def self.all_dependencies
    active_roles = RoleObject.find_roles_by_name("*-config-*")
    dependencies = {}
    active_roles.map do |role|
       service = ServiceObject.get_service(role.barclamp).new(Rails.logger)
       dependencies[role.name] = service.proposal_dependencies(role).map { |dep| "#{dep['barclamp']}-config-#{dep['inst']}" }
    end
    dependencies
  end

  def self.dependencies(service)
    all_dependencies[service]
  end

  def self.reverse_dependencies(service)
    Hash[all_dependencies.select { |s, r| r.include?(service) }].keys
  end

  def self.active(barclamp = nil, inst = nil)
    full = if barclamp.nil?
      RoleObject.find_roles_by_name "*-config-*"
    else
      RoleObject.find_roles_by_name "#{barclamp}-config-#{inst || "*"}"
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
      return RoleObject.new Chef::Role.load(name)
    rescue Net::HTTPServerException => e
      return nil if e.response.code == "404"
      raise e
    end
  end

  def self.human_attribute_name(attrib)
    I18n.t attrib, :scope => "model.attributes.role"
  end

  def barclamp
    name = @role.name.split("-")[0]

    case name
    when "switch_config"
      "network"
    when "bmc"
      "ipmi"
    when "nfs"
      "nfs_client"
    else
      name
    end
  end

  def category
    @category ||= BarclampCatalog.category(barclamp)
  end

  def inst
    @role.name.gsub("#{self.barclamp}-config-", "")
  end

  def prop
    [barclamp, inst].join("_")
  end

  def revision
    @role.override_attributes[barclamp]["crowbar-revision"].to_i rescue 0
  end

  def display_name
    @display_name ||= BarclampCatalog.display_name(barclamp)
  end

  def allow_multiple_proposals?
    ServiceObject.get_service(barclamp).allow_multiple_proposals?
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

  def crowbar_revision
    override_attributes[barclamp] ? override_attributes[barclamp]["crowbar-revision"].to_i : 0
  end

  def increment_crowbar_revision!
    override_attributes[barclamp] ||= {}
    if override_attributes[barclamp]["crowbar-revision"].nil?
      override_attributes[barclamp]["crowbar-revision"] = 0
    else
      override_attributes[barclamp]["crowbar-revision"] += 1
    end
  end

  def save(options = {})
    sync_role = options.fetch(:sync) { false }

    Rails.logger.debug("Saving role: #{@role.name} - #{crowbar_revision}")
    role_lock = FileLock.acquire "role:#{@role.name}"
    begin
      if sync_role
        upstream_role = RoleObject.find_role_by_name(@role.name)
        @role = upstream_role.role
      end

      yield(@role) if block_given?

      increment_crowbar_revision!
      @role.save
    ensure
      FileLock.release role_lock
    end
    Rails.logger.debug("Done saving role: #{@role.name} - #{crowbar_revision}")
  end

  def destroy
    Rails.logger.debug("Destroying role: #{@role.name} - #{crowbar_revision}")
    role_lock = FileLock.acquire "role:#{@role.name}"
    begin
      @role.destroy
    ensure
      FileLock.release role_lock
    end
    Rails.logger.debug("Done removing role: #{@role.name} - #{crowbar_revision}")
  end

  def elements
    @role.override_attributes[self.barclamp]["elements"]
  end

  def run_list
    @role.run_list
  end
end
