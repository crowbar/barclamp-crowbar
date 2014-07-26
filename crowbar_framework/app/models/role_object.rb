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
  extend CrowbarOffline

  def self.all
    self.find_roles_by_search(nil)
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
    if CHEF_ONLINE
      #TODO this call could be moved to fild_roles_by_search
      arr = ChefObject.query_chef.search "role", "name:#{chef_escape(name)}"
      if arr[2] != 0
        roles = arr[0].map { |x| RoleObject.new x }
        roles.delete_if { |x| x.nil? or x.role.nil? }
      end
    else
      roles = find_roles_by_search name
    end
    roles
  end

  def self.find_roles_by_search(search)
    roles = []
    if CHEF_ONLINE
      arr = if search.nil?
        ChefObject.query_chef.search "role"
      else
        ChefObject.query_chef.search "role", search
      end
      if arr[2] != 0
        roles = arr[0].map { |x| RoleObject.new x }
        roles.delete_if { |x| x.nil? or x.role.nil? }
      end
    else
      files = offline_search 'role-', search
      roles = files.map! { |f| RoleObject.new(recover_json(f)) }
    end
    roles
  end

  def self.find_role_by_name(name)
    if CHEF_ONLINE
      begin
        return RoleObject.new Chef::Role.load(name)
      rescue Net::HTTPServerException => e
        return nil if e.response.code == "404"
        raise e
      end
    else
      answer = self.recover_json(self.nfile('role',name))
      return answer.nil? ? nil : RoleObject.new(answer)
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
    when "hawk"
      "pacemaker"
    else
      name
    end
  end

  def category
    @category ||= ServiceObject.barclamp_category(barclamp)
  end

  def inst
    @role.name.gsub("#{self.barclamp}-config-", "")
  end

  def prop
    [barclamp, inst].join("_")
  end

  def display_name
    @display_name ||= begin
      catalog = ServiceObject.barclamp_catalog
      display = catalog['barclamps'][barclamp]['display']

      if display.nil? or display.empty?
        barclamp.titlecase
      else
        display
      end
    end
  end

  def allow_multiple_proposals?
    Kernel.const_get("#{barclamp.camelize}Service").method(:allow_multiple_proposals?).call
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

  # Copied from ServiceObject, but with @logger replaced by Rails.logger.
  # For the record, there's also a very similar pair of locking functions
  # in chef/cookbooks/utils/providers/line.rb
  def acquire_lock(name)
    Rails.logger.debug("Acquire #{name} lock enter as uid #{Process.uid}")
    path = "tmp/#{name}.lock"
    begin
      f = File.new(path, File::RDWR|File::CREAT, 0644)
    rescue
      Rails.logger.error("Couldn't open #{path} for locking: #$!")
      Rails.logger.error("cwd was #{Dir.getwd})")
      raise "Couldn't open #{path} for locking: #$!"
    end
    Rails.logger.debug("Acquiring #{name} lock")
    rc = false
    count = 0
    while rc == false do
      count = count + 1
      Rails.logger.debug("Attempt #{name} Lock: #{count}")
      rc = f.flock(File::LOCK_EX|File::LOCK_NB)
      sleep 1 if rc == false
    end
    Rails.logger.debug("Acquire #{name} lock exit: #{f.inspect}, #{rc}")
    f
  end

  def release_lock(f)
    Rails.logger.debug("Release lock enter: #{f.inspect}")
    if f
      f.flock(File::LOCK_UN)
      f.close
    else
      Rails.logger.warn("release_lock called without valid file")
    end
    Rails.logger.debug("Release lock exit")
  end

  def save
    @role.override_attributes[barclamp] = {} if @role.override_attributes[barclamp].nil?
    if @role.override_attributes[barclamp]["crowbar-revision"].nil?
      @role.override_attributes[barclamp]["crowbar-revision"] = 0
    else
      @role.override_attributes[barclamp]["crowbar-revision"] = @role.override_attributes[barclamp]["crowbar-revision"] + 1
    end
    Rails.logger.debug("Saving role: #{@role.name} - #{@role.override_attributes[barclamp]["crowbar-revision"]}")
    role_lock = acquire_lock "role:#{@role.name}"
    begin
      if CHEF_ONLINE
        old_role = RoleObject.find_role_by_name(@role.name)
        if old_role
          old_role.override_attributes[barclamp] ||= {}
          old_rev = old_role.override_attributes[barclamp]["crowbar-revision"]
          new_rev = @role.override_attributes[barclamp]["crowbar-revision"]
          if old_rev && old_rev >= new_rev
            Rails.logger.warn("WARNING: revision race for role #{@role.name} (previous revision #{old_rev})")
          end
        end
        @role.save
      else
        RoleObject.offline_cache(@role, RoleObject.nfile('role', @role.name))
      end
    ensure
      release_lock role_lock
    end
    Rails.logger.debug("Done saving role: #{@role.name} - #{@role.override_attributes[barclamp]["crowbar-revision"]}")
  end

  def destroy
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
  
  def export
    RoleObject.dump @role, 'role', name 
  end
end
