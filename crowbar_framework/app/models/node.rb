# Copyright 2012, Dell
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
  before_destroy    :jig_delete
  
  attr_accessible :name, :description, :alias, :order, :admin, :allocated
  
  # Make sure we have names that are legal
  # old:
  #  FQDN_RE = /^(([a-zA-Z]|[a-zA-Z][a-zA-Z0-9\-]*[a-zA-Z0-9]))*\.([A-Za-z]|[A-Za-z][A-Za-z0-9\-]*[A-Za-z0-9])*\.([A-Za-z]|[A-Za-z][A-Za-z0-9\-]*[A-Za-z0-9])$/
  
  # requires at least three domain elements "foo.bar.com", cause the admin node shouldn't 
  # be a top level domain ;p
  FQDN_RE = /(?=^.{1,254}$)(^(?:(?!\d+\.)[a-zA-Z0-9_\-]{1,63}\.){2,}(?:[a-zA-Z]{2,})$)/
  # for to_api_hash
  API_ATTRIBUTES = ["id", "name", "description", "order", "state", "admin",
                    "allocated", "os_id", "created_at", "updated_at"]

  READY = 0
  UNREADY = 1
  PENDING = 2
  BUILDING = 69
  ERROR = 666
  UNKNOWN = 999

  # for Node-Role relationship
  HAS_NODE_ROLE = BarclampCrowbar::AttribHasNode

  # 
  # Validate the name should unique (no matter the case)
  # and that it starts with a valid FQDN
  #
  validates_uniqueness_of :name, :case_sensitive => false, :message => I18n.t("db.notunique", :default=>"Name item must be unique")
  validates_format_of :name, :with=>FQDN_RE, :message => I18n.t("db.fqdn", :default=>"Name must be a fully qualified domain name.")
  validates_length_of :name, :maximum => 255

  # TODO: 'alias' will move to DNS BARCLAMP someday, but will prob hang around here a while
  # validates_uniqueness_of :alias, :case_sensitive => false, :message => I18n.t("db.notunique", :default=>"Name item must be unique")
  validates_format_of :alias, :with=>/^([A-Za-z]|[A-Za-z][A-Za-z0-9\-]*[A-Za-z0-9])$/, :message => I18n.t("db.fqdn", :default=>"Name must be a fully qualified domain name.")
  validates_length_of :alias, :maximum => 100

  has_and_belongs_to_many :groups, :join_table => "node_groups", :foreign_key => "node_id", :order=>"[order], [name] ASC"

  has_many :attribs,            :dependent => :destroy
  has_many :attrib_types,       :through => :attribs

  has_many :attrib_has_nodes,   :class_name => HAS_NODE_ROLE, :foreign_key => :node_id
  has_many :roles,              :through => :attrib_has_nodes
  has_many :snapshots,          :through => :roles
  
  belongs_to :os, :class_name => "Os" #, :foreign_key => "os_id"

  #CB1 ?? 
  def reset_jig_access
    if ["discovered","hardware-installed","hardware-updated","reset", "delete",
        "hardware-installing","hardware-updating","reinstall",
        "update","installing","installed"].member?(state) and !is_admin?
      Rails.logger.info("Crowbar transition: should be deleting a client entry for #{name}")
      client = ClientObject.find_client_by_name name
      Rails.logger.info("Crowbar transition: found and trying to delete a client entry for #{name}") unless client.nil?
      client.destroy unless client.nil?

      # Make sure that the node can be accessed by knife ssh or ssh
      if ["reset","reinstall","update","delete"].member?(state)
        system("sudo rm /root/.ssh/known_hosts")
      end
    end
  end

  def self.name_hash
    Digest::SHA1.hexdigest(Node.select(:name).order("name ASC").map{|n|n.name}.join).to_i(16)
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

  #
  # Helper function for allocated
  #
  def allocated?
    get_attrib("allocated").value || false
  end

  def allocate
    set_attrib("allocated", true) 
  end

  # this mechanism requires that nodes are members of the Crowbar.crowbar role
  def state_attrib
    # TODO this needs to be optimized to eliminate all the lookups
    cb_role = Barclamp.find_by_name('crowbar').default_proposal.private_roles.first
    BarclampCrowbar::AttribHasNode.find_or_create_by_node_id_and_role_id self.id, cb_role.id
  end
  #
  # Use transition function to set state.
  #
  def set_state(new_state, old_state = nil)
    # depricated, use state instead
    state = new_state
  end
  # set the state using the state attribute 
  def state=(value)
    a = state_attrib
    a.state = value
    a.save
  end
  # get the state using the state attribute
  def state
    state_attrib.state_text
  end
  
  def ready?
    state_attrib.ready?
  end
  
  # Makes the open ended state information into a subset of items for the UI
  def status
    state_attrib.status
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

# CB1
  def run_list(pending = true)
#    nrs = NodeRole.find_all_by_node_id(self.id)
#    # Get the active ones
#    nrs = nrs.select { |x| x.proposal_config_id == x.proposal_config.proposal.active_config_id }
#    nrs.map { |x| x.role }
  end

  # Associate the node to a barclamp via the role
  # This will create a AttribHasRole object to the requested Role
  def add_role(role)
    has_node = HAS_NODE_ROLE.find_by_node_id_and_role_id self.id, role.id
    HAS_NODE_ROLE.create :role_id => role.id, :node_id => self.id unless has_node
  end

  # Deassociate the node to a barclamp via the role
  # This will delete the AttribHasRole object to the requested Role
  def remove_role(role)
    has_node = HAS_NODE_ROLE.find_by_node_id_and_role_id self.id, role.id
    HAS_NODE_ROLE.delete has_node if has_node
  end
  
  # XXX: Make this better one day.  Perf is not good.  Direct select would be better
  # A custom query should be able to build the list straight up.
  #
  # update_run_list:
  #   Rebuilds the run_list for the Jig system for this node based upon its active proposal
  #   membership and its state.
  #
  #   This includes updating the Jig node role with node specific data.
  #
  # CB1
  def update_run_list
#    nrs = NodeRole.find_all_by_node_id(self.id)
#    # Get the active ones
#    nrs = nrs.select { |x| x.proposal_config_id == x.proposal_config.proposal.active_config_id }

    # For each of the roles
#    cno = jig_hash
#    cno.clear_run_list_map
#    nrs.each do |nr|
#      if nr.role
        # This is node role that defines run_list entry
#        cno.add_to_run_list(nr.role.name, nr.role.barclamp.jig_order, nr.role.states.split(","))
#        config_name = "#{nr.role.barclamp.name}-config-#{nr.proposal_config.proposal.name}"
#        cno.add_to_run_list(config_name, nr.role.barclamp.jig_order, ["all"])
#      end
      # Has custom data.
#      if nr.config
#        hash = nr.config_hash
#        cno.crowbar.merge(hash)
#      end
#    end
#    cno.save
  end
  
  def virtual?
    jig_hash.virtual?
  end
  
  def bmc_set?
    # TODO place holder
    true
  end
  
  def links
    # TODO place holder for barclamp defined links
    []
  end


  # retrieves the Attrib from Attrib
  def get_attrib(attrib_type)
    get_attribs(attrib_type).first
  end

  # get the attributes (adds if missing)    
  def get_attribs(attrib_type, use_class=Attrib::DEFAULT_CLASS)
    a = AttribType.add attrib_type
    attribs = Attrib.find_all_by_attrib_type_id_and_node_id a.id, self.id
    # did we get something?  no, then add it
    if attribs.empty?
      # work from defined roles first
      from_attrib = a.attribs.first
      if from_attrib.nil?
        attribs << Attrib.find_or_create_by_attrib_type_and_node(a, self, use_class)
      else
        new_attrib = from_attrib.dup
        new_attrib.node_id = self.id
        new_attrib.save
        attribs << new_attrib
      end
    end
    attribs
  end
  
  # if you set the attribute from the new, then we require that you have a crowbar barclamp association
  def set_attrib(attrib_type, value=nil, jig_run=0, use_class=Attrib::DEFAULT_CLASS)    
    # get the attributes (adds if missing)
    attribs = get_attribs(attrib_type, use_class)
    # iterate over all the attribs and set them to the new value
    attribs.each do |na|
      na.actual = value
      na.jig_run_id = (jig_run.is_a?(JigRun) ? jig_run.id : jig_run)
      na.save
    end
    attribs.first
  end
    
  def method_missing(m,*args,&block)
    method = m.to_s
    if method.starts_with? "attrib_"
      return get_attrib(method[7..100]).value
    else
      super m,*args,&block
    end
  end
  
  def <=>(other)
    # use Array#<=> to compare the attributes
    [self.order, self.name] <=> [other.order, other.name]
  end

  def to_s
    "Node: #{name}"
  end
  
  def ip
    #TODO reference jig_hash.ip somehow?
    "unknown"
  end

  def group
    groups[0]
  end

  def group= group
    db_group = group.is_a?(Group) ? group : Group.find_or_create_by_name({'name' => group, 'description' => group, 'category' => 'ui'})
    if db_group
      category = db_group.category
      groups.each { |g| g.nodes.delete(self) if g.category.eql?(category) }
      groups << db_group unless db_group.nodes.include? self
    end
  end

  private

  # make sure we do housekeeping before we remove the DB object
  def jig_delete
    Jig.all.each { |c| c.delete_node(self) }
  end

  # make sure some safe values are set for the node
  def default_population
    self.name = self.name.downcase
    self.alias = self.name.split(".")[0]
    # the line belowrequires a crowbar deployment to which the status attribute is tied
    self.state ||= 'unknown' 
    if self.groups.size == 0
      g = Group.find_or_create_by_name :name=>'not_set', :description=>I18n.t('not_set', :default=>'Not Set')
      self.groups << g rescue nil
    end
  end

end
