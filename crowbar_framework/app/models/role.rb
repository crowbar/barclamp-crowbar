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

  attr_accessible :id, :description, :name, :jig_name, :barclamp_id, :template
  ### Flags for roles described in [[/doc/devguide/model/role.md]]
  attr_accessible :library
  attr_accessible :implicit
  attr_accessible :bootstrap
  attr_accessible :discovery
  attr_accessible :server
  attr_accessible :cluster
  attr_accessible :destructive
  attr_accessible :template

  validates_uniqueness_of   :name,  :scope => :barclamp_id
  validates_format_of       :name,  :with=>/^[a-zA-Z][-_a-zA-Z0-9]*$/, :message => I18n.t("db.lettersnumbers", :default=>"Name limited to [_a-zA-Z0-9]")

  belongs_to      :barclamp
  belongs_to      :jig,               :foreign_key=>:jig_name, :primary_key=>:name
  has_many        :role_requires,     :dependent => :destroy
  has_many        :role_require_attribs, :dependent => :destroy
  has_many        :attribs
  has_many        :wanted_attribs, :through => :role_require_attribs, :class_name => "Attrib", :source => :attrib
  has_many        :role_parents, :through => :role_requires, :class_name => "Role", :source => :upstream
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

  # update just one value in the template
  # for >1 level deep, add method matching key to role!
  # use via /api/v2/roles/[role]/template/[key]/[value]
  def update_template(key, value)
    t = self.send(key.to_sym,value) rescue { key => value }
    raw = read_attribute("template")
    d = raw.nil? ? {} : JSON.parse(raw)
    merged = d.deep_merge(t)
    self.template = JSON.generate(merged)
    self.save!
  end

  # incremental update (merges with existing)
  def template_update(val)
    Role.transaction do
      d = JSON.parse(read_attribute(template))
      d.deep_merge!(val)
      write_attribute("template",JSON.generate(d))
    end
  end

  # replaces existing
  def template=(val)
    val = JSON.generate(val) unless val.is_a?(String)
    write_attribute("template",val)
  end

  def template
    t = read_attribute("template")
    return {} if t.nil? || t.empty?
    JSON.parse(t) rescue {}
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

  # Event hook that is called whenever a new deployment role is bound to a deployment.
  # Roles that need do something on a per-deployment basis should override this
  def on_deployment_create(dr)
    true
  end

  # Event hook that is called whenever a deployment role is deleted from a deployment.
  def on_deployment_delete(dr)
    true
  end

  # returns list of roles that are the parents of this role
  def parents
    res = []
    res << jig.client_role if jig.client_role
    res + role_parents
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
    Role.transaction do
      c = read_attribute("cohort")
      if c.nil?
        c = 0
        begin
          parents.each do |parent|
            p_c = parent.cohort || 0
            c = p_c + 1 if p_c >= c
          end
          write_attribute("cohort",c)
          save!
        rescue
          Rails.logger.info "Could not calculate cohort for #{self.name} because requested parent role does not exist (could be OK due to late binding)"
        end
      end
      return c
    end
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
      Rails.logger.info("Role: Adding deployment role #{name} to #{snap.name}")
      DeploymentRole.create!({:role_id=>self.id, :snapshot_id=>snap.id, :data=>self.template}, :without_protection => true)
    end
  end

  def find_noderoles_for_role(role,snap)
    csnap = snap
    loop do
      Rails.logger.info("Role: Looking for role '#{role.name}' binding in '#{snap.deployment.name}' deployment")
      pnrs = NodeRole.peers_by_role(csnap,role)
      return pnrs unless pnrs.empty?
      csnap = (csnap.deployment.parent.snapshot rescue nil)
      break if csnap.nil?
    end
    Rails.logger.info("Role: No bindings for #{role.name} in #{snap.deployment.name} or any parents.")
    []
  end

  def add_to_node(node)
    add_to_node_in_snapshot(node,node.deployment.head)
  end
  
  # Bind a role to a node in a snapshot.
  def add_to_node_in_snapshot(node,snap)
    # Roles can only be added to a node of their backing jig is active.
    unless active?
      # if we are testing, then we're going to just skip adding and keep going
      if Jig.active('test')
        Rails.logger.info("Role: Test mode allows us to coerce role #{name} to use the 'test' jig instead of #{jig_name} when it is not active")
        self.jig = Jig.find_key 'test'
        self.save
      else
        raise MISSING_JIG.new("Role: role '#{name}' cannot be added to node '#{node.name}' without '#{jig_name}' being active!")
      end
    end
    # If we are already bound to this node in a snapshot, do nothing.
    res = NodeRole.where(:node_id => node.id, :role_id => self.id).first
    return res if res
    Rails.logger.info("Role: Trying to add #{name} to #{node.name}")

    # First pass throug the parents -- we just create any needed parent noderoles.
    # We will actually bind them after creating the noderole binding.
    parents.each do |parent|
      pnrs = find_noderoles_for_role(parent,snap)
      if pnrs.empty? || (parent.implicit && !pnrs.any?{|nr|nr.node_id == node.id})
        # If there are none, or the parent role has the implicit flag,
        # then bind the parent to ourself as well.
        # This logic will need to grow into bind the parent to the best suited
        # noderole in the current deployment eventually.
        Rails.logger.info("Role: Parent #{parent.name} not bound in scope, binding it to #{node.name} in #{snap.deployment.name}")
        parent.add_to_node_in_snapshot(node,snap)
      end
    end
    # At this point, all the parent noderoles we need are bound.
    # make sure that we also have a deployment role, then
    # create ourselves and bind our parents.
    NodeRole.transaction do
      add_to_snapshot(snap)
      res = NodeRole.create({ :node => node,
                              :role => self,
                              :snapshot => snap,
                              :cohort => 0}, :without_protection => true)
      Rails.logger.info("Role: Creating new noderole #{res.name}")
      # Second pass through our parent array.  Since we created all our
      # parent noderoles earlier, we can just concern ourselves with creating the bindings we need.
      parents.each do |parent|
        pnrs = find_noderoles_for_role(parent,snap)
        if parent.cluster
          # If the parent role has a cluster flag, then all of the found
          # parent noderoles will be bound to this one.
          Rails.logger.info("Role: Parent #{parent.name} of role #{name} has the cluster flag, binding all instances in deployment #{pnrs[0].deployment.name}")
          pnrs.each do |pnr|
            res.add_parent(pnr)
          end
        else
          # Prefer a parent noderole from the same node we are on, otherwise
          # just pick one at random.
          pnr = pnrs.detect{|nr|nr.node_id == node.id} ||
            pnrs[Random.rand(pnrs.length)]
          res.add_parent(pnr)
        end
      end
      # If I am a new noderole binding for a cluster node, find all the children of my peers
      # and bind them too.
      if self.cluster
        NodeRole.peers_by_role(snap,self).each do |peer|
          peer.children.each do |c|
            c.add_parent(res)
            c.deactivate
            c.save!
          end
        end
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
    name = self.name.sub("#{self.barclamp.name}-", '').gsub('-','_').camelize
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
