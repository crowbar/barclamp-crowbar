# Copyright 2013, Dell
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

class Snapshot < ActiveRecord::Base

  STATUS_NONE        = 1  # Not applied, just created
  STATUS_QUEUED      = 2  # Attempt at commit, but is queued
  STATUS_COMMITTING  = 3  # Attempt at commit is in progress
  STATUS_FAILED      = 4  # Attempted commit failed
  STATUS_APPLIED     = 5  # Attempted commit succeeded

  ROLE_ORDER         = "'roles'.'order', 'roles'.'run_order'"
  
  attr_accessible :id, :name, :description, :order, :status, :failed_reason, :element_order
  attr_accessible :deployement_id, :barclamp_id, :jig_event_id
  
  belongs_to      :barclamp
  belongs_to      :deployment,        :inverse_of => :snapshot

  has_many        :roles,             :dependent => :destroy, :order => ROLE_ORDER
  has_many        :nodes,             :through => :roles
  has_many        :private_roles,     :class_name => "Role", :conditions=>'run_order<0', :order => ROLE_ORDER
  has_many        :public_roles,      :class_name => "Role", :conditions=>'run_order>=0', :order => ROLE_ORDER
  has_many        :role_types,        :through => :roles

  has_many        :attribs,           :through => :roles
  has_many        :attrib_types,      :through => :attribs
  
  has_many        :jig_events
  
  def active?
    deployment.active_snapshot_id == self.id
  end

  def committed? 
    deployment.committed_snapshot_id == self.id
  end
  
  def proposed?
    deployment.proposed_snapshot_id == self.id
  end
 
  # Add a role to a snapshot by creating the needed Role
  # Returns a Role (not a role_type)
  def add_role(role_type)
    role_type = RoleType.add role_type, self.name
    ri = Role.find_by_role_type_id_and_snapshot_id role_type.id, self.id 
    ri ||= Role.find_or_create_by_role_type_id_and_snapshot_id :role_type_id => role_type.id, :snapshot_id => self.id
    ri
  end

  # Add attrib to snapshot
  # assume first public role (fall back to private role) if none given
  def add_attrib(attrib_type, role_type=nil)
    if role_type.nil?
      role_type = public_roles.first.role_type
      role_type = private_roles.first.role_type if role_type.nil?
    else
      role_type = RoleType.add role_type, self.name
    end
    desc = I18n.t 'added', :scope => 'model.role', :name=>self.name
    ri = Role.find_or_create_by_role_type_id_and_snapshot_id :role_type_id=>role_type.id, 
                                                                         :snapshot_id=>self.id,
                                                                         :description=>desc
    ri.add_attrib attrib_type, nil, self.name
  end

  # determines the role run order using the imported element_order
  # return is a nested array of roles
  def role_order
    ro = []
    source = ActiveSupport::JSON.decode(self.element_order)
    if source
      source.each do |parent|
        children = []
        parent.each do |role|
          children << self.add_role(role) if role
        end
        ro << children
      end
    end
    ro
  end

  ##
  # Clone this snapshot
  # optionally, change parent too (you do NOT have parents for templates)
  def deep_clone(parent_deployment=nil, name=nil, with_nodes=true)

    new_snap = self.dup
    new_snap.deployment_id = parent_deployment.id if parent_deployment
    new_snap.name = name || "#{self.name}_#{self.id}"
    new_snap.status = STATUS_NONE
    new_snap.failed_reason = nil
    new_snap.save

    # clone the roles
    roles.each { |ri| ri.deep_clone(new_snap, with_nodes) }

    new_snap
  end

end
