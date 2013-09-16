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

class Node < ActiveRecord::Base

  before_validation :default_population
  after_create :add_default_roles

  attr_accessible   :id, :name, :description, :alias, :order, :admin, :allocated

  # Make sure we have names that are legal
  # old:
  #  FQDN_RE = /^(([a-zA-Z]|[a-zA-Z][a-zA-Z0-9\-]*[a-zA-Z0-9]))*\.([A-Za-z]|[A-Za-z][A-Za-z0-9\-]*[A-Za-z0-9])*\.([A-Za-z]|[A-Za-z][A-Za-z0-9\-]*[A-Za-z0-9])$/
  
  # requires at least three domain elements "foo.bar.com", cause the admin node shouldn't 
  # be a top level domain ;p
  FQDN_RE = /(?=^.{1,254}$)(^(?:(?!\d+\.)[a-zA-Z0-9_\-]{1,63}\.){2,}(?:[a-zA-Z]{2,})$)/
  # for to_api_hash
  API_ATTRIBUTES = ["id", "name", "description", "order", "admin",
                    "allocated", "created_at", "updated_at"]

  # 
  # Validate the name should unique (no matter the case)
  # and that it starts with a valid FQDN
  #
  validates_uniqueness_of :name, :case_sensitive => false, :message => I18n.t("db.notunique", :default=>"Item must be unique")
  validates_format_of     :name, :with=>FQDN_RE, :message => I18n.t("db.fqdn", :default=>"Name must be a fully qualified domain name.")
  validates_length_of     :name, :maximum => 255

  # TODO: 'alias' will move to DNS BARCLAMP someday, but will prob hang around here a while
  # validates_uniqueness_of :alias, :case_sensitive => false, :message => I18n.t("db.notunique", :default=>"Name item must be unique")
  validates_format_of :alias, :with=>/^([A-Za-z]|[A-Za-z][A-Za-z0-9\-]*[A-Za-z0-9])$/, :message => I18n.t("db.fqdn", :default=>"Name must be a fully qualified domain name.")
  validates_length_of :alias, :maximum => 100

  has_and_belongs_to_many :groups, :join_table => "node_groups", :foreign_key => "node_id", :order=>"[order], [name] ASC"

  has_many :node_roles,         :dependent => :destroy
  has_many :roles,              :through => :node_roles
  has_many :snapshots,          :through => :node_roles
  has_many :deployments,        :through => :snapshots

  scope    :admin,              -> { where(:admin=>true) }

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

  #
  # This is an hack for now.
  # XXX: Once networking is better defined, we should use those routines
  #
  def address(net = "admin")
    raise "CB1 do not use - use attrib_admin_address"
    jig_hash.address(net)
  end

  def public_ip
    raise "CB1 do not use - use attrib_public_ip"
    jig_hash.public_ip
  end

  #
  # Helper function to test admin without calling admin. Style-thing.
  #
  def is_admin?
    admin
  end
  
  # CB1 these are really IMPI actions - please move!
  def ipmi_cmd(cmd)
    bmc          = jig_hash.address("bmc").addr rescue nil
    bmc_user     = jig_hash.get_bmc_user
    bmc_password = jig_hash.get_bmc_password
    system("ipmitool -I lanplus -H #{bmc} -U #{bmc_user} -P #{bmc_password} #{cmd}") unless bmc.nil?
  end

  # CB1 these are really IMPI actions - please move!
  def reboot
    set_state("reboot")
    ipmi_cmd("power cycle")
  end

  # CB1 these are really IMPI actions - please move!
  def shutdown
    set_state("shutdown")
    ipmi_cmd("power off")
  end

  # CB1 these are really IMPI actions - please move!
  def poweron
    set_state("poweron")
    ipmi_cmd("power on")
  end

  # CB1 these are really IMPI actions - please move!
  def identify
    ipmi_cmd("chassis identify")
  end
  
  def virtual?
    # TODO ZEHICLE place holder
    true
  end
  
  def bmc_set?
    # TODO ZEHICLE place holder
    true
  end
  
  # retrieves the Attrib from Attrib
  def get_attrib(attrib_name)
    a = Attrib.by_name(attrib_name).first
    unless a.nil? 
      a.value(self.discovery)
    end
  end

  def active_node_roles
    NodeRole.where(:state => NodeRole::ACTIVE, :node_id => self.id).order("cohort")
  end

  def all_active_data
    dres = {}
    res = {}
    active_node_roles.each do |nr|
      dres.deep_merge!(nr.deployment_data)
      res.deep_merge!(nr.all_my_data)
    end
    dres.deep_merge!(res)
    dres
  end
  
  def method_missing(m,*args,&block)
    method = m.to_s
    if method.starts_with? "attrib_"
      return get_attrib method[7..100]
    else
      super
    end
  end
  
  def <=>(other)
    # use Array#<=> to compare the attributes
    [self.order, self.name] <=> [other.order, other.name]
  end

  def ip
    #TODO reference jig_hash.ip somehow?
    "unknown"
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

  private

  # make sure some safe values are set for the node
  def default_population
    self.admin = true if Node.admin.count == 0    # first node, needs to be admin
    self.name = self.name.downcase
    self.alias = self.name.split(".")[0]
    # the line belowrequires a crowbar deployment to which the status attribute is tied
    if self.groups.size == 0
      g = Group.find_or_create_by_name :name=>'not_set', :description=>I18n.t('not_set', :default=>'Not Set')
      self.groups << g rescue nil
    end
  end

  def add_default_roles
    raise "you must have at least 1 deployment" unless Deployment.count > 0
    Deployment.system_root.first.recommit do |snap|
      Role.expand(self.admin ? Role.bootstrap.active : Role.discovery.active).sort.each do |r|
        r.add_to_node_in_snapshot(self,snap)
      end
    end
  end
end
