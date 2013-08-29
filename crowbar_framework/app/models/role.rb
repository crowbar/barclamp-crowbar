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

  class Role::MISSING_DEP < Exception
  end

  class Role::MISSING_JIG < Exception
  end
  
  before_create :create_type_from_name

  attr_accessible :id, :description, :name, :jig_name, :barclamp_id
  attr_accessible :library, :implicit, :bootstrap, :discovery     # flags
  attr_accessible :template

  validates_uniqueness_of   :name,  :scope => :barclamp_id
  validates_format_of       :name,  :with=>/^[a-zA-Z][-_a-zA-Z0-9]*$/, :message => I18n.t("db.lettersnumbers", :default=>"Name limited to [_a-zA-Z0-9]")

  belongs_to      :barclamp
  belongs_to      :jig,               :foreign_key=>:jig_name, :primary_key=>:name
  has_many        :role_requires,     :dependent => :destroy
  alias_attribute :requires,          :role_requires

  #has_many        :upstreams,         :through => :role_requires
  #scope           :downstreams,       ->(r) { joins(:role_requires).where(['requires=?', r.name]) }
  scope           :library,            -> { where(:library=>true) }
  scope           :implicit,           -> { where(:implicit=>true) }
  scope           :discovery,          -> { where(:discovery=>true) }
  scope           :bootstrap,          -> { where(:bootstrap=>true) }
  scope           :active,             -> { joins(:jig).where(["jigs.active = ?", true]) }

  # Magic to provide a mechanism for letting barclamps provide specific role behaviour.
  # This will mostly be used to implement the on_* helpers in a not-totally-insane way.
  after_initialize :mixin_specific_behaviour

  def parents
    res = []
    res << jig.client_role if jig.client_role
    role_requires.each do |r|
      res << Role.find_by_name!(r.requires)
    end
    res
  end

  def depends_on?(other)
    return false if self.id == other.id
    p = parents
    return false if p.empty?
    return true if p.any?{|i|i.id == other.id}
    p.each do |i|
      return true if i.depends_on?(other)
    end
    false
  end

  # Bind a role to a node in a snapshot.
  def add_to_snapshot(snap,node)
    # Roles can only be added to a node of their backing jig is active.
    unless active?
      raise MISSING_JIG.new("#{name} cannot be added to #{node.name} without #{jig_name} being active!")
    end
    # make sure there's a deployment role before we add a node role
    if DeploymentRole.snapshot_and_role(snap, self).size == 0
      DeploymentRole.transaction do
        DeploymentRole.create!({:role_id=>self.id, :snapshot_id=>snap.id, :data=>self.template}, :without_protection => true)
      end
    end
    # If we are already bound to this node in a snapshot, do nothing.
    res = NodeRole.where(:node_id => node.id, :role_id => self.id).first
    return res if res
    Rails.logger.info("Trying to add #{name} to #{node.name}")
    # Check to make sure that all my parent roles are bound properly.
    # If they are not, die unless it is an implicit role.
    # This logic will need to change as we start allowing roles to classify
    # nodes, but it will work for now.
    parent_node_roles = Array.new
    parents.each do |parent|
      # This will need to grow more ornate once we start allowing multiple
      # deployments.
      pnr = NodeRole.peers_by_role(snap,parent).first
      if pnr.nil?
        if parent.implicit
          pnr = parent.add_to_snapshot(snap,node)
        else
          raise MISSING_DEP.new("Role #{name} depends on role #{parent.name}, but #{parent.name} does not exist in deployment #{snap.deployment.name}")
        end
      end
      parent_node_roles << pnr
    end
    # By the time we get here, all our parents are bound recursively.
    # Bind ourselves the same way.
    res = nil
    NodeRole.transaction do
      res = NodeRole.create({:node => node, :role => self, :snapshot => snap}, :without_protection => true)
      parent_node_roles.each do |pnr|
        res.parents << pnr
      end
      res.save!
    end
    # If there is an on_proposed hook for this role, call it now with our fresh node_role.
    self.send(:on_proposed,res) if self.respond_to?(:on_proposed) && res
    res
  end


  def jig
    Jig.where(["name = ?",jig_name]).first
  end

  def active?
    j = jig
    return false unless j
    j.active
  end

  def <=>(other)
    return 0  if self.id == other.id
    return 1  if self.depends_on?(other)
    return -1 if other.depends_on?(self)
    0
  end

  private

  # If there is a module in the form of BarclampName::Role::RoleName,
  # extend this object with its methods.
  def mixin_specific_behaviour
    raise "Roles require a name" if self.name.nil?
    Rails.logger.info("Seeing if #{self.name} has a mixin...")
    mod = "barclamp_#{barclamp.name}".camelize.to_sym
    return self unless Module::const_defined?(mod)
    mod = Module::const_get(mod)
    ["role",
     self.name].map{|m|m.tr("-","_").camelize.to_sym}.each do |m|
      return self unless mod.const_defined?(m)
      mod = mod.const_get(m)
      return self unless mod.kind_of?(Module)
    end
    Rails.logger.info("Extending #{self.name} with #{mod}")
    self.extend(mod)
  end

  # This method ensures that we have a type defined for 
  def create_type_from_name
    raise "roles require a name" if self.name.nil?
    raise "roles require a barclamp" if self.barclamp_id.nil?
    namespace = "Barclamp#{self.barclamp.name.camelize}"
    # remove the redundant part of the name (if any)
    name = self.name.sub("#{self.barclamp.name}-", '').camelize
    # these routines look for the namespace & class
    m = Module::const_get(namespace) rescue nil
    test = m.const_get(name).superclass == Role rescue false
    # if they dont' find it we fall back to BarclampFramework (this should go away!)
    self.type = unless test
      Rails.logger.warn "Role #{self.name} created with fallback Model!"
      "Role"
    else 
      "#{namespace}::#{name}"
    end
    
  end

end
