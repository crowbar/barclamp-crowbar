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

class Barclamp < ActiveRecord::Base

  attr_accessible :id, :name, :description, :display, :version, :online_help, :user_managed
  attr_accessible :proposal_schema_version, :layout, :order, :run_order, :cmdb_order
  attr_accessible :commit, :build_on, :mode, :transitions, :transition_list
  attr_accessible :template, :allow_multiple_proposals
  
  # 
  # Validate the name should unique 
  # and that it starts with an alph and only contains alpha,digist,hyphen,underscore
  #
  validates_uniqueness_of :name, :case_sensitive => false, :message => I18n.t("db.notunique", :default=>"Name item must be unique")
  validates_format_of :name, :with=>/^[a-zA-Z][_a-zA-Z0-9]*$/, :message => I18n.t("db.lettersnumbers", :default=>"Name limited to [_a-zA-Z0-9]")
  
  #
  # Proposals are all stored in the proposal table
  # There is a special proposal barclamp.  This is the template proposal
  # that defines the initial content for a new proposal
  #
  # The three helpers are for:
  #   all proposals (that aren't template)
  #   all active proposals (that aren't template and have attempted application)
  #   the template proposal
  # 
  has_many :proposals, :conditions => 'name != "template"'
  has_many :active_proposals, 
                :class_name => "Proposal", 
                :conditions => [ 'name <> ? AND active_config_id IS NOT NULL', "template"]
  has_one :template, :class_name => "Proposal", :conditions => 'name = "template"'

  has_many :roles

  has_and_belongs_to_many :packages, :class_name=>'OsPackage', :join_table => "barclamp_packages", :foreign_key => "barclamp_id"
  has_and_belongs_to_many :prereqs, :class_name=>'Barclamp', :join_table => "barclamp_dependencies", :foreign_key => "prereq_id"
  has_and_belongs_to_many :members, :class_name=>'Barclamp', :join_table=>'barclamp_members', :foreign_key => "member_id", :order => "[order], [name] ASC"
  has_and_belongs_to_many :parents, :class_name=>'Barclamp', :join_table=>'barclamp_members', :foreign_key => "barclamp_id", :association_foreign_key => "member_id", :order => "[order], [name] ASC"

  #
  # Helper function to load the service object
  #
  # This is used to allow callers to get access to the barclamp's
  # service functions.  There are two primary use cases,
  # 1. barclamp controller wants a generic method to call a common routine.
  #   @barclamp.operations.proposal_create
  # 2. Barclamp wants to call other barclamp
  #   Barclamp.find_by_name("network").operations(@logger).allocate_ip(...)
  #
  def operations(logger = nil)
    @service = eval("#{name.camelize}Service.new logger") unless @service
    @service.bc_name = name
    @service
  end

  def allow_multiple_proposals?
    return allow_multiple_proposals
  end

  #
  # Order barclamps by their order value and then their name
  #
  def <=>(other)
    # use Array#<=> to compare the attributes
    [self.order, self.name] <=> [other.order, other.name]
  end
  
  #
  # We should set this to something one day.
  #
  def versions
    [ "1.0" ]
  end

  #
  # Override function:
  #
  # Creates a new proposal from the template object.  
  # Barclamps can override this function to tweak config or add nodes
  #
  # Overriding functions should call super to get the template object.
  #
  # Input: Optional: Name
  # Output: Proposal Object Based upon template.
  #
  def create_proposal(name = nil)
    template.deep_clone(name || "created_#{Time.now.strftime("%y%m%d_%H%M%S")}")
  end

  # XXX: This may be too much for what Andi planned.  This could be done as 
  # deleted flag and not removed from the database.
  def delete_proposal(prop)
    prop.destroy
  end

  def get_proposal(name)
    Proposal.find_by_name_and_barclamp_id(name, self.id)
  end

  def get_role(name)
    Role.find_by_name_and_barclamp_id(name, self.id)
  end

  #
  # Get the roles group by the role orders.
  # This is used to order cmdb runs by role sets
  # This used to be the element_order structure in the json
  #
  def get_roles_by_order
    run_order = []
    roles.each do |role|
      role.role_element_orders.each do |roe|
        run_order[roe.order] = [] unless run_order[roe.order]
        run_order[roe.order] << role
      end
    end
    run_order
  end

  #legacy approach - expects name of barclamp for YML import
  def self.import_1x(bc_name)
    bc_file = File.join('barclamps', bc_name+'.yml')
    throw "Barclamp import file #{bc_file} not found" unless File.exist? bc_file
    bc = YAML.load_file bc_file
    throw 'Barclamp name must match name from YML file' unless bc['barclamp']['name'].eql? bc_name
    # Can't do the || trick booleans because nil is false.
    amp = bc['barclamp']['allow_multiple_proposals']
    amp = false if amp.nil?
    um = bc['barclamp']['user_managed']
    um = true if um.nil?
    gitcommit = "unknown" if bc['git'].nil? or bc['git']['commit'].nil?
    gitdate = "unknown" if bc['git'].nil? or bc['git']['date'].nil?
    barclamp = Barclamp.create(
        :name        => bc_name,
        :display     => bc['barclamp']['display'] || bc_name.humanize,
        :description => bc['barclamp']['description'] || bc_name.humanize,
        :online_help => bc['barclamp']['online_help'],
        :version     => bc['barclamp']['version'] || 2,
        :user_managed=> um,
        :allow_multiple_proposals => amp,
        
        :proposal_schema_version => bc['crowbar']['proposal_schema_version'] || 2,
        :layout      => bc['crowbar']['layout'] || 2,
        :order       => bc['crowbar']['order'] || 0,
        :run_order   => bc['crowbar']['run_order'] || 0,
        :cmdb_order  => bc['crowbar']['chef_order'] || 0,

        :mode        => "full",
        :transitions => false,

        :commit      => gitcommit,
        :build_on    => gitdate
      )
      
    # memberships (if memembership is missing, we'll let you into the club anyway)
    if bc['barclamp']['member']
      bc['barclamp']['member'].each do |owner|
        o = Barclamp.find_by_name owner
        o.members << barclamp if o
      end
    end

    # requires (will fail if prereq is missing)
    if bc['barclamp']['requires']
      bc['barclamp']['requires'].each do |prereq|
        prereq = prereq[1..100] if prereq.starts_with? "@"
        pre = Barclamp.find_by_name prereq
        throw "ERROR: Cannot load barclamp #{bc_name} because prerequisite #{prereq} has not been imported" if pre.nil?
        barclamp.prereqs << pre
      end
    end
    
    # packages (only import 1.x for latest OS)
    begin    
      debs = Os.find_by_name "ubuntu-12.04"
      bc['debs'].each do |k, v|
        if k.eql? 'pkgs'
          v.each { |pkg| barclamp.packages << OsPackage.find_or_create_by_name_and_os_id(:name=>pkg, :os_id=>debs.id) }
        elsif k.eql? debs.name
          v['pkgs'].each { |pkg| barclamp.packages << OsPackage.find_or_create_by_name_and_os_id(:name=>pkg, :os_id=>debs.id) }
        end
      end
    rescue Exception => e
      #nothing
    end
    begin
      rpms = Os.find_by_name "centos-6.2"
      bc['rpms'].each do |k, v|
        if k.eql? 'pkgs'
          v.each { |pkg| barclamp.packages << OsPackage.find_or_create_by_name_and_os_id(:name=>pkg, :os_id=>prms.id) }
        elsif k.eql? rpms.name
          v['pkgs'].each { |pkg| barclamp.packages << OsPackage.find_or_create_by_name_and_os_id(:name=>pkg, :os_id=>rpms.id) }
        end
      end
    rescue Exception => e
      #nothing
    end
    
    template_file = File.join('barclamps', 'templates', "bc-template-#{bc_name}.json")
    if File.exists? template_file
      json = JSON::load File.open(template_file, 'r')

      barclamp.mode = json["deployment"][bc_name]["config"]["mode"] rescue "full"
      barclamp.description = (json["description"] rescue bc_name.humanize) unless bc['barclamp']['description']
      barclamp.transitions = json["deployment"][bc_name]["config"]["transitions"] rescue false
      barclamp.transition_list = json["deployment"][bc_name]["config"]["transition_list"].join(",") rescue ""

      element_order = json["deployment"][bc_name]["element_order"] rescue []
      element_order.each_with_index do |role_array, index|
        role_array.each do |role_name|
          role = Role.find_by_name_and_barclamp_id(role_name, barclamp.id)
          unless role 
            states = json["deployment"][bc_name]["element_states"][role_name].join(",") rescue "all"
            role = Role.create(:name => role_name, :states => states)
            role.barclamp = barclamp
            role.save!
          end
          reo = RoleElementOrder.create(:order => index)
          role.role_element_orders << reo
        end
      end
      
      # import users from databags
      if json["attributes"] and json["attributes"]["crowbar"] and json["attributes"]["crowbar"]["users"]
        json["attributes"]["crowbar"]["users"].each do |user, password|
          pass = password['password']
          u = User.find_or_create_by_username!(:username=>user.dup, :password=>pass.dup, :is_admin=>true)
          u.digest_password(pass)
          u.save!
        end
      end

      prop = Proposal.create(:name => "template", :description => "template")
      prop_config = ProposalConfig.create(:config => json["attributes"].to_json)
      prop_config.proposal = prop
      prop_config.save!

      prop.current_config = prop_config
      prop.save!

      barclamp.template = prop
      barclamp.save!
    end

    return barclamp
  end

end

