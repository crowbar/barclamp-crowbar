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

require 'digest/md5'

class Node < ActiveRecord::Base

  before_validation :default_population
  after_create :add_default_roles
  before_destroy :tear_down_roles
  after_save :after_save_handler

  attr_accessible   :id, :name, :description, :alias, :order, :admin, :allocated, :deployment_id
  attr_accessible   :alive, :available, :bootenv

  # Make sure we have names that are legal
  # requires at least three domain elements "foo.bar.com", cause the admin node shouldn't
  # be a top level domain ;p
  FQDN_RE = /(?=^.{1,254}$)(^(?:(?!\d+\.)[a-zA-Z0-9_\-]{1,63}\.){2,}(?:[a-zA-Z]{2,})$)/
  # for to_api_hash
  API_ATTRIBUTES = ["id", "name", "description", "order", "admin", "available", "alive",
                    "allocated", "created_at", "updated_at"]

  #
  # Validate the name should unique (no matter the case)
  # and that it starts with a valid FQDN
  #
  validates_uniqueness_of :name, :case_sensitive => false, :message => I18n.t("db.notunique", :default=>"Item must be unique")
  validates_format_of     :name, :with=>FQDN_RE, :message => I18n.t("db.fqdn", :default=>"Name must be a fully qualified domain name.")
  validates_length_of     :name, :maximum => 255

  # TODO: 'alias' will move to DNS BARCLAMP someday, but will prob hang around here a while
  validates_uniqueness_of :alias, :case_sensitive => false, :message => I18n.t("db.notunique", :default=>"Name item must be unique")
  validates_format_of :alias, :with=>/^([A-Za-z]|[A-Za-z][A-Za-z0-9\-]*[A-Za-z0-9])$/, :message => I18n.t("db.fqdn", :default=>"Name must be a fully qualified domain name.")
  validates_length_of :alias, :maximum => 100

  has_and_belongs_to_many :groups, :join_table => "node_groups", :foreign_key => "node_id"

  has_many    :node_roles,         :dependent => :destroy
  has_many    :runs,               :dependent => :destroy
  has_many    :roles,              :through => :node_roles
  has_many    :snapshots,          :through => :node_roles
  has_many    :deployments,        :through => :snapshots
  belongs_to  :deployment
  belongs_to  :target_role,        :class_name => "Role", :foreign_key => "target_role_id"

  scope    :admin,              -> { where(:admin => true) }
  scope    :alive,              -> { where(:alive => true) }
  scope    :available,          -> { where(:available => true) }

  # Get all the attributes applicable to a node.
  # This includes:
  # * All attributes that are defined for our node roles, by virtue of
  #   being defined as part of the role that the node role is bound to, and
  # * All attributes that are not defined as part of a node.
  def attribs
    Attrib.where(["attribs.role_id IS NULL OR attribs.role_id in (select role_id from node_roles where node_id = ?)",self.id])
  end

  # look at Node state by scanning all node roles.
  def state
    node_roles.each do |nr|
      if nr.proposed?
        return NodeRole::PROPOSED
      elsif nr.error?
        return NodeRole::ERROR
      elsif [NodeRole::BLOCKED, NodeRole::TODO, NodeRole::TRANSITION].include? nr.state
        return NodeRole::TODO
      end
    end
    return NodeRole::ACTIVE
  end

  # returns a hash with all the node error status information
  def status
    s = []
    node_roles.each { |nr| s[nr.id] = nr.status if nr.error?  }
  end

  def self.name_hash
    Digest::SHA1.hexdigest(Node.select(:name).order("name ASC").map{|n|n.name}.join).to_i(16)
  end

  def self.make_admin!
    Node.transaction do
      raise "Already have an admin node" unless where(:admin => true).empty?
      Node.create(:name => %x{hostname -f}.strip, :admin => true)
    end
  end

  def v6_hostpart
    d = Digest::MD5.hexdigest(name)
    "#{d[16..19]}:#{d[20..23]}:#{d[24..27]}:#{d[28..32]}"
  end

  def auto_v6_address(net)
    return nil if net.v6prefix.nil?
    IP.coerce("#{net.v6prefix}:#{v6_hostpart}/64")
  end

  def addresses
    net = BarclampNetwork::Network.where(:name => "admin").first
    raise "No admin network" if net.nil?
    net.node_allocations(self)
  end

  def address
    addresses.detect{|a|a.reachable?}
  end
  #
  # Helper function to test admin without calling admin. Style-thing.
  #
  def is_admin?
    admin
  end

  def virtual?
    virtual = [ "KVM", "VMware Virtual Platform", "VMWare Virtual Platform", "VirtualBox" ]
    virtual.include? get_attrib('hardware')
  end

  def bmc_set?
    # TODO ZEHICLE place holder
    true
  end

  # retrieves the Attrib from Attrib
  def get_attrib(attrib)
    Attrib.get(attrib, self, :discovery) rescue nil
  end

  def active_node_roles
    res = NodeRole.on_node(self).in_state(NodeRole::ACTIVE).committed.order("cohort ASC")
    res.each do |nr|
      Rails.logger.info("Node: Found active noderole #{nr.name} in cohort #{nr.cohort}")
    end
    res
  end

  def all_active_data
    dres = {}
    res = {}
    active_node_roles.each do |nr|
      dres.deep_merge!(nr.deployment_data)
      res.deep_merge!(nr.all_parent_data)
    end
    dres.deep_merge!(res)
    dres
  end

  def method_missing(m,*args,&block)
    method = m.to_s
    if method.starts_with? "attrib_"
      return get_attrib method[7..-1]
    else
      super
    end
  end

  def <=>(other)
    # use Array#<=> to compare the attributes
    [self.order, self.name] <=> [other.order, other.name]
  end

  def group
    groups.first
  end

  def group= group
    db_group = group.is_a?(Group) ? group : Group.find_or_create_by_name({'name' => group, 'description' => group, 'category' => 'ui'})
    if db_group
      category = db_group.category
      groups.each { |g| g.nodes.delete(self) if g.category.eql?(category) }
      groups << db_group unless db_group.nodes.include? self
    end
  end

  def hint
    d = read_attribute("hint")
    return {} if d.nil? || d.empty?
    JSON.parse(d) rescue {}
  end

  def hint=(arg)
    arg = JSON.parse(arg) unless arg.is_a? Hash
    data = hint.deep_merge arg
    write_attribute("hint",JSON.generate(data))
    data
  end

  def hint_update(val)
    Node.transaction do
      h = hint
      h.deep_merge!(val)
      write_attribute("hint",JSON.generate(h))
      h
    end
  end

  def discovery
    d = read_attribute("discovery")
    return {} if d.nil? || d.empty?
    JSON.parse(d) rescue {}
  end

  def discovery=(arg)
    arg = JSON.parse(arg) unless arg.is_a? Hash
    data = discovery.merge arg
    write_attribute("discovery",JSON.generate(data))
    data
  end

  def discovery_update(val)
    Node.transaction do
      d = discovery
      d.deep_merge!(val)
      write_attribute("discovery",JSON.generate(d))
    end
  end

  def reboot
    BarclampCrowbar::Jig.ssh("root@#{self.name} reboot")
  end
  
  def debug
    self.alive = false
    self.bootenv = "sledgehammer"
    self.target = Role.where(:name => "crowbar-managed-node").first
    self.reboot
  end

  def undebug
    self.alive = false
    self.bootenv = "local"
    self.target = nil
    self.reboot
  end

  def target
    return target_role
  end
  # Set the annealer target for this node, which will restrict the annealer
  # to considering the noderole for this role bound to this node and its parents
  # for converging.  If nil is passed, then all the noderoles are marked as available.
  def target=(r)
    if r.nil?
      NodeRole.transaction do
        old_alive = self.alive
        self.save!
        node_roles.each do |nr|
          nr.available = true
        end
        self.target_role_id = nil
        self.alive = old_alive
        self.save!
      end
      return self
    elsif r.kind_of?(Role) &&
        roles.member?(r) &&
        r.barclamp.name == "crowbar" &&
        r.jig.name == "noop"
      NodeRole.transaction do
        old_alive = self.alive
        self.alive = false
        self.save!
        self.target_role = r
        node_roles.each do |nr|
          nr.available = false
        end
        target_nr = self.node_roles.where(:role_id => r.id).first
        target_nr.all_parents.each do |nr|
          next unless nr.node_id == self.id
          nr.available = true
        end
        target_nr.available = true
        self.save!
      end
      return self
    else
      raise("Cannot set target role #{r.name} for #{self.name}")
    end
  end

  def alive?
    return false if alive == false
    return true unless Rails.env == "production"
    a = address
    return true if a && BarclampCrowbar::Jig.ssh("root@#{a.addr} -- echo alive")[1]
    self[:alive] = false
    save!
    false
  end

  private

  def after_save_handler
    return unless changed?
    Rails.logger.info("Node: calling all role on_node_change hooks for #{name}")
    Role.all.each do |r|
      #Rails.logger.debug("\tNode: calling role #{r.name} for #{name}")
      r.on_node_change(self)
    end
    if changes["available"] || changes["alive"]
      if alive && available
        Rails.logger.info("Node: #{name} is alive and available, enqueing noderoles to run.")
        Run.run!
      end
      if changes["alive"] && !alive
        Rails.logger.info("Node: #{name} is not alive, deactivating noderoles on this node.")
        node_roles.deactivatable.each do |nr|
          nr.deactivate
        end
      end
    end
  end

  # make sure some safe values are set for the node
  def default_population
    self.admin = true if Node.admin.count == 0    # first node, needs to be admin
    self.name = self.name.downcase
    self.alias ||= self.name.split(".")[0]
    self.deployment ||= Deployment.system_root.first
    # the line belowrequires a crowbar deployment to which the status attribute is tied
    if self.groups.count == 0
      g = Group.find_or_create_by_name :name=>'not_set', :description=>I18n.t('not_set', :default=>'Not Set')
      self.groups << g rescue nil
    end
  end

  # Call the on_node_delete hooks.
  def tear_down_roles
    Role.all.each do |r|
      begin
        r.on_node_delete(self)
      rescue Exception => e
        Rails.logger.error "node #{name} attempting to cleanup role #{r.name} failed with #{e.message}"
      end
    end
  end

  def add_default_roles
    raise "you must have at least 1 deployment" unless Deployment.count > 0
    Deployment.system_root.first.recommit do |snap|
      (self.admin ? Role.bootstrap.active : Role.discovery.active).sort.each do |r|
        r.add_to_node_in_snapshot(self,snap)
      end
    end

    # Call all role on_node_create hooks with ourself.
    # These should happen synchronously.
    Role.all.each do |r|
      r.on_node_create(self)
    end
  end
end
