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

  STATUS_CREATED     = 1  # Not applied, just created
  STATUS_QUEUED      = 2  # Attempt at commit, but is queued
  STATUS_COMMITTING  = 3  # Attempt at commit is in progress
  STATUS_FAILED      = 4  # Attempted commit failed
  STATUS_APPLIED     = 5  # Attempted commit succeeded

  attr_accessible :id, :name, :description, :order, 
  attr_accessible :deployement_id
  
  belongs_to      :deployment

  has_many        :deployments_roles, :dependent => :destroy
  has_many        :roles,             :through => :eployments_roles

  has_many        :nodes_roles,       :dependent => :destroy
  has_many        :nodes,             :through => :nodes_roles
 
  def active?
    deployment.active_snapshot_id == self.id
  end

  def committed? 
    deployment.committed_snapshot_id == self.id
  end
  
  def proposed?
    deployment.proposed_snapshot_id == self.id
  end
 
  # Add a role to a snapshot by creating the needed DeploymentRole
  # Returns a Role
  def add_role(role_name)
    r = Role.find_or_create_by_name_and_snapshot_id :name=>role_name, :snapshot_id => self.id
    r.save! unless r.id
    r
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
    new_snap.status = STATUS_CREATED
    new_snap.failed_reason = nil
    new_snap.save

    # clone the roles
    roles.each { |ri| ri.deep_clone(new_snap, with_nodes) }

    new_snap
  end

  def method_missing(m,*args,&block)
    if m.to_s =~ /(.*)_role$/
      r = Role.find_by_name_and_snapshot_id $1, self.id
      # temporary while we depricate the node role
      return r
    else
      super m,*args,&block
    end
  end

end
