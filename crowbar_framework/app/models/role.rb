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


class Role < ActiveRecord::Base

  attr_accessible :id, :description, :order, :run_order, :states
  attr_accessible :snapshot_id, :role_type_id
  
  HAS_NODE_ROLE  = BarclampCrowbar::AttribHasNode
  HAS_DEPLOYMENT = BarclampCrowbar::AttribHasDeployment

  validates_uniqueness_of :role_type_id, :scope => :snapshot_id  # decricate
  validates_uniqueness_of :name,         :scope => :snapshot_id  
  
  belongs_to      :role_type,         :inverse_of => :role
  belongs_to      :snapshot
  has_one         :barclamp,          :through => :snapshot
  has_one         :deployment,        :through => :snapshot
  
  has_many        :attribs,           :dependent => :destroy
  has_many        :role_attribs,      :class_name => "Attrib", :conditions=>'node_id IS NULL'
  has_many        :node_attribs,      :class_name => "Attrib", :conditions=>'node_id IS NOT NULL'
  has_many        :attrib_types,      :through => :attribs

  has_many        :attrib_has_deployments, :class_name => HAS_DEPLOYMENT, :foreign_key => :role_id
  has_many        :prerequisites,     :source => :deployment, :through => :attrib_has_deployments, :foreign_key=>:id_actual

  has_many        :attrib_has_nodes,  :class_name => HAS_NODE_ROLE, :foreign_key => :role_id
  has_many        :nodes,             :through => :attrib_has_nodes
  
  # alias helper
  def name
    role_type.name
  end
  
  def public?
    self.run_order>=0
  end
  
  def <=>(other)
    # use Array#<=> to compare the attributes
    [self.order, self.run_order, self.role.name] <=> [other.order, other.run_order, other.role.name]
  end
  
  def add_attrib(attrib_type, value=nil, map=nil)
    a = AttribType.add attrib_type, (barclamp.nil? ? nil : barclamp.name)
    begin 
      Attrib.find_by_attrib_type_id_and_role_id! a.id, self.id
    rescue
      Attrib::DEFAULT_CLASS.create :attrib_type_id => a.id, :role_id => self.id
    end
  end

  # links role to a barclamp it depends on
  # will create a default deployment if none exists
  def require_deployment(barclamp_name, deployment_name=Barclamp::DEFAULT_DEPLOYMENT_NAME, role_type=nil)
    bc = Barclamp.find_by_name barclamp_name
    deployment = Deployment.find_by_barclamp_id_and_name bc.id, deployment_name
    # if we did not find a deployment, then we have to create one
    if deployment.nil?
      # cannot create barclamps if we are a singleton and already have one
      deployment = if bc.allow_multiple_deployments or bc.deployments.count == 0
        bc.create_proposal deployment_name
      else
        bc.deployments.first
      end
    end
    if deployment
      HAS_DEPLOYMENT.find_or_create_by_role_id_and_id_actual :role_id     => self.id, 
                                                             :id_actual   => deployment.read_attribute(:id),
                                                             :value_actual=> role_type
    end
  end
  
  # Assignes a node to the role by creating a AttribInstanceHasRole
  def add_node(node)
    has_node = HAS_NODE_ROLE.find_by_node_id_and_role_id node.id, self.id
    HAS_NODE_ROLE.create :role_id => self.id, :node_id => node.id unless has_node
  end

  # Unassigns a node to the role by creating a AttribInstanceHasRole
  def remove_node(node)
    has_node = HAS_NODE_ROLE.find_by_node_id_and_role_id node.id, self.id
    HAS_NODE_ROLE.delete has_node
  end
  
  ##
  # Clone this role
  # optionally, change parent too
  # with_nodes allows for template copies that should have not nodes assigned yet
  def deep_clone(snapshot=nil, clone_with_nodes=true)
    new_role = self.dup
    new_role.snapshot_id = snapshot.read_attribute(:id) if snapshot
    new_role.save

    # clone the attributes (includes node-roles)
    attribs.each do |ai|
      # if we are cloning a template then we need the option of no nodes
      if clone_with_nodes or ai.node_id.nil?
        new_ai = ai.dup
        new_ai.role_id = new_role.id
        new_ai.jig_run_id = nil
        new_ai.save
      end
    end

    new_role
    
  end

end
