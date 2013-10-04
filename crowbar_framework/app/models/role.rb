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
  ### Flags for roles
  # Unused by any role.
  attr_accessible :library
  # Indicates that this role can be automatically added to any node that
  # requires it as a dependency.  Otherwise, roles must be bound to nodes
  # in dependency order.
  attr_accessible :implicit
  # Indicates that this role will be automatically bound to the first
  # admin node.
  attr_accessible :bootstrap
  # Indicates that this role will be automatically bound to all newly
  # discovered nodes.
  attr_accessible :discovery
  # Indicates that userdata, system data, and wall data for this node
  # will be visible to child nodes.  As the name of the flag indicates,
  # the only roles that will generally require that are ones that implement
  # servers that other roles need to talk to.
  attr_accessible :server
  attr_accessible :template

  validates_uniqueness_of   :name,  :scope => :barclamp_id
  validates_format_of       :name,  :with=>/^[a-zA-Z][-_a-zA-Z0-9]*$/, :message => I18n.t("db.lettersnumbers", :default=>"Name limited to [_a-zA-Z0-9]")

  belongs_to      :barclamp
  belongs_to      :jig,               :foreign_key=>:jig_name, :primary_key=>:name
  has_many        :role_requires,     :dependent => :destroy
  has_many        :node_roles
  alias_attribute :requires,          :role_requires

  #has_many        :upstreams,         :through => :role_requires
  #scope           :downstreams,       ->(r) { joins(:role_requires).where(['requires=?', r.name]) }
  scope           :library,            -> { where(:library=>true) }
  scope           :implicit,           -> { where(:implicit=>true) }
  scope           :discovery,          -> { where(:discovery=>true) }
  scope           :bootstrap,          -> { where(:bootstrap=>true) }
  scope           :server,             -> { where(:server => true) }
  scope           :active,             -> { joins(:jig).where(["jigs.active = ?", true]) }

  # update just one value in the template (assumes just 1 level deep!)
  # use via /api/v2/roles/[role]/template/[key]/[value]
  def update_template(key, value)
    t = { key => value }
    raw = read_attribute("template")
    d = raw.nil? ? {} : JSON.parse(raw)
    merged = d.deep_merge(t)
    self.template = JSON.generate(merged)
    self.save!
  end

  # Given a list of roles, find any implicits and add them to the list.
  # Overall list order will be preserved.
  def self.expand(roles)
    res = []
    roles.each do |r|
      r.parents.each do |rent|
        res << rent if rent.implicit
      end
      res << r
    end
    res.uniq
  end

  # State Transistion Overrides

  def on_error(node_role, *args)
    Rails.logger.debug "No override for #{self.class.to_s}.on_error event: #{node_role.role.name} on #{node_role.node.name}"
  end

  def on_active(node_role, *args)
    Rails.logger.debug "No override for #{self.class.to_s}.on_active event: #{node_role.role.name} on #{node_role.node.name}"
  end

  def on_todo(node_role, *args)
    Rails.logger.debug "No override for #{self.class.to_s}.on_todo event: #{node_role.role.name} on #{node_role.node.name}"
  end

  def on_transition(node_role, *args)
    Rails.logger.debug "No override for #{self.class.to_s}.on_transition event: #{node_role.role.name} on #{node_role.node.name}"
  end

  def on_blocked(node_role, *args)
    Rails.logger.debug "No override for #{self.class.to_s}.on_blocked event: #{node_role.role.name} on #{node_role.node.name}"
  end

  def on_proposed(node_role, *args)
    Rails.logger.debug "No override for #{self.class.to_s}.on_proposed event: #{node_role.role.name} on #{node_role.node.name}"
  end

  # Event triggers for node creation and destruction.
  # roles should override if they want to handle node addition
  def on_node_create(node)
    true
  end

  # Event triggers for node creation and destruction.
  # roles should override if they want to handle node destruction
  def on_node_delete(node)
    true
  end

  # Event hook that will be called every time a node is saved if any attributes changed.
  # Roles that are interested in watching nodes to see what has changed should
  # implement this hook.
  def on_node_change(node)
    true
  end

  def parents
    res = []
    res << jig.client_role if jig.client_role
    role_requires.each do |r|
      res << Role.find_by_name!(r.requires) rescue next
    end
    res
  end

  def reset_cohort
    Role.transaction do
      cohort = nil
      save!
      RoleRequire.where(:requires => name).each do |rr|
        rr.role.reset_cohort
      end
    end
  end

  def cohort
    c = read_attribute("cohort")
    if c.nil?
      c = 0
      begin
        parents.each do |parent|
          p_c = parent.cohort || 0
          c = p_c + 1 if p_c >= c
        end
        write_attribute("cohort",c)
      rescue
        Rails.logger.info "Could not calculate cohort for #{self.name} because requested parent role does not exist (could be OK due to late binding)"
      end
    end
    return c
  end

  def depends_on?(other)
    return false if self.id == other.id
    rents = parents
    tested = Hash.new
    loop do
      return false if rents.empty?
      new_parents = []
      rents.each do |parent|
        next if tested[parent.id] == true
        raise "Role dependency graph for #{self.barclamp.name}:#{name} is circular!" if parent.id == self.id
        return true if parent.id == other.id
        tested[parent.id] = true
        new_parents << parent.parents
      end
      rents = new_parents.flatten.reject{|i|tested[i.id]}
    end
    raise "Cannot happen examining dependencies for #{name} -> #{other.name}"
  end

  # Make sure there is a deployment role for ourself in the snapshot.
  def add_to_snapshot(snap)
    # make sure there's a deployment role before we add a node role
    if DeploymentRole.snapshot_and_role(snap, self).size == 0
      DeploymentRole.create!({:role_id=>self.id, :snapshot_id=>snap.id, :data=>self.template}, :without_protection => true)
    end
  end

  # Bind a role to a node in a snapshot.
  def add_to_node_in_snapshot(node,snap)
    # Roles can only be added to a node of their backing jig is active.
    unless active?
      raise MISSING_JIG.new("#{name} cannot be added to #{node.name} without #{jig_name} being active!")
    end
    # make sure that we also have a deployment role
    add_to_snapshot(snap)
    # If we are already bound to this node in a snapshot, do nothing.
    res = NodeRole.where(:node_id => node.id, :role_id => self.id).first
    return res if res
    Rails.logger.info("Role: Trying to add #{name} to #{node.name}")
    # Check to make sure that all my parent roles are bound properly.
    # If they are not, die unless it is an implicit role.
    # This logic will need to change as we start allowing roles to classify
    # nodes, but it will work for now.
    parent_node_roles = Array.new
    parents.each do |parent|
      # This will need to grow more ornate once we start allowing multiple
      # deployments.
      # Look for a noderole on the current node that satisfies this dep.
      pnr = NodeRole.peers_by_node_and_role(snap,node,parent).first
      if pnr.nil?
        Rails.logger.info("Role: Did not find #{name} on #{node.name}")
        if parent.implicit
          # Bind our parent on this node if we did not find an already-deployed noderole.
          Rails.logger.info("Role: Parent #{parent.name} of #{name} is implicit, binding it to #{node.name}")
          pnr = parent.add_to_node_in_snapshot(node,snap)
        else
          # Look for noderoles that can satisfy this dep on other nodes.
          # Start with the current snapshot, and we will work our way up
          # in the deployment tree from there.
          csnap = snap
          loop do
            Rails.logger.info("Role: Looking for #{parent.name} binding in #{snap.deployment.name}")
            pnr = NodeRole.peers_by_role(csnap,parent).first
            break unless pnr.nil?
            csnap = (csnap.deployment.parent.snapshot rescue nil)
            break if csnap.nil?
          end
        end
      end
      # For now, raise this error.
      # We may want to wind up unconditionally trying to bind our parent
      # role to the same node we want to bind to here..
      if pnr.nil?
        raise MISSING_DEP.new("Role #{name} depends on role #{parent.name}, but #{parent.name} does not exist in deployment #{snap.deployment.name}")
      end
      Rails.logger.info("Role: Choosing #{pnr.role.name} on #{pnr.node.name} as a parent of #{name} on #{node.name}")
      parent_node_roles << pnr
    end
    # By the time we get here, all our parents are bound recursively.
    # Bind ourselves the same way.
    res = nil
    NodeRole.transaction do
      cohort = 0
      res = NodeRole.create({:node => node, :role => self, :snapshot => snap}, :without_protection => true)
      parent_node_roles.each do |pnr|
        cohort = pnr.cohort + 1 if pnr.cohort >= cohort
        res.parents << pnr
      end
      res.cohort = cohort
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
    return 0 if self.id == other.id
    self.cohort <=> other.cohort
  end

  private

  # This method ensures that we have a type defined for
  def create_type_from_name
    raise "roles require a name" if self.name.nil?
    raise "roles require a barclamp" if self.barclamp_id.nil?
    namespace = "Barclamp#{self.barclamp.name.camelize}"
    # remove the redundant part of the name (if any)
    name = self.name.sub("#{self.barclamp.name}-", '').camelize
    # these routines look for the namespace & class
    # barclamps can override specific roles
    test_specific =  ("#{namespace}::#{name}".constantize ? true : false) rescue false
    # barclamps can provide a generic fallback  "BarclampName::Role"
    test_generic = ("#{namespace}::Role".constantize ? true : false) rescue false
    # if they dont' find it we fall back to the core Role
    self.type = if test_specific
      "#{namespace}::#{name}"
    elsif test_generic
      "#{namespace}::Role"
    else
      Rails.logger.info "Role #{self.name} created with fallback Model!"
      "Role"
    end
  end

end
