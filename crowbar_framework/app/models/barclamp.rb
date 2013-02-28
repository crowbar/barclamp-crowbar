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

class Barclamp < ActiveRecord::Base

  attr_accessible :id, :name, :description, :display, :version, :online_help, :user_managed, :type, :source_path
  attr_accessible :proposal_schema_version, :layout, :order, :run_order, :jig_order
  attr_accessible :commit, :build_on, :mode, :transitions, :transition_list
  attr_accessible :allow_multiple_deployments, :template_id
  attr_accessible :api_version, :api_version_accepts, :liscense, :copyright
  before_create :create_type_from_name

  # legacy for CB1 remove after 2013-03-01
  alias_attribute :allow_multiple_proposals, :allow_multiple_deployments
  
  # 
  # Validate the name should unique 
  # and that it starts with an alph and only contains alpha,digits,underscore
  #
  validates_uniqueness_of :name, :case_sensitive => false, :message => I18n.t("db.notunique", :default=>"Name item must be unique")
  validates_exclusion_of :name, :in => %w(framework barclamp docs machines users support application), :message => I18n.t("db.barclamp_excludes", :default=>"Illegal barclamp name")
    
  validates_format_of :name, :with=>/^[a-zA-Z][_a-zA-Z0-9]*$/, :message => I18n.t("db.lettersnumbers", :default=>"Name limited to [_a-zA-Z0-9]")
      
  # Template Data
  has_one  :template,                 :class_name => "Snapshot", :dependent => :destroy, :foreign_key=>:id, :primary_key=>:template_id
  has_many :attribs,                  :through => :template
  has_many :roles,                    :through => :template
  has_many :role_types,               :through => :roles, :order=>'"roles"."order"'
  
  # Deployment Chains (may not all be the same deployment!)
  has_many :deployments,              :dependent => :destroy
  has_many :snapshots,                :through => :deployments
  alias_attribute :proposals,         :deployments    # legacy support
  
  # Jig Interactions
  has_many :jig_maps,                 :dependent => :destroy
  has_many :jigs,                     :through => :jig_maps
  # reminder! this is NOT the same as .template.attribs!!!
  has_many :attrib_types,             :through => :jig_maps

  has_and_belongs_to_many :packages, :class_name=>'OsPackage', :join_table => "barclamp_packages", :foreign_key => "barclamp_id"
  has_and_belongs_to_many :prereqs, :class_name=>'Barclamp', :join_table => "barclamp_dependencies", :foreign_key => "prereq_id"
  has_and_belongs_to_many :members, :class_name=>'Barclamp', :join_table=>'barclamp_members', :foreign_key => "member_id", :order => "[order], [name] ASC"
  has_and_belongs_to_many :parents, :class_name=>'Barclamp', :join_table=>'barclamp_members', :foreign_key => "barclamp_id", :association_foreign_key => "member_id", :order => "[order], [name] ASC"

  alias_attribute :configsallow_multiple_proposals?,      :allow_multiple_proposals

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
    [ "2.0" ]
  end

  # 
  # Barclamps are responsible to creating the attributes that they will manage
  # INPUTS: 
  #   Attrib name or object to assign to barclamp
  #   map (optional) hash provides information to help barclamp resolve inbound data from jigs
  # RETURNS: Attrib
  # name is required, all other fields are optional
  # attributes cannot be reassigned to a different barclamp
  # add_attrib attaches an attribute to the barclamp.  Assigns optional description & order values
  #
  def add_attrib(attrib_type, map=nil, role_type=nil)
    # find the attrib
    a = AttribType.add attrib_type, self.name
    r = role_type.nil? ? nil : RoleType.add(role_type, self.name)
    # map it
    JigMap.add a, self, map if map
    # add the attrib type to the barclamp snapshot
    template.add_attrib a, r if template
  end


  DEFAULT_DEPLOYMENT_NAME = I18n.t('default')

  #
  # Possible Override function
  #
  # Creates a new proposal from the template object.  
  # Barclamps can override this function to tweak config or add nodes
  #
  # Overriding functions should call super to get the template object.
  #
  # Inputs: 
  #  Optional: Config Name / Hash (default to default)
  # Output: Config Object Based upon template.
  #
  def create_proposal(deployment=nil)
    deployment = {:name=>deployment} if deployment.is_a? String
    deployment ||= { :name => DEFAULT_DEPLOYMENT_NAME}
    proposal = nil
    if allow_multiple_deployments or deployments.count==0
      # setup required items
      deployment[:barclamp_id]  = self.id
      deployment[:description]  ||= "#{I18n.t 'created_on'} #{Time.now.strftime("%y%m%d_%H%M%S")}"
      Deployment.transaction do 
        # create a new deployment
        proposal = Deployment.create deployment
        # one day, we could use non-templates for the base!
        based_on ||= self.template    
        # create the snapshot 
        snapshot = based_on.deep_clone proposal, deployment.name, false
        # attach the snapshot to the config
        proposal.proposed_snapshot_id = snapshot.id
        proposal.save
      end
    end
    proposal
  end

  
  # take run data from the jig and process it into attributes
  # returns the node
  # WARNING - this has NOT been optimized!
  def process_inbound_data jig_run, node, data
    jig = jig_run.jig
    maps = JigMap.where :jig_id=>jig.id, :barclamp_id=>self.id
    maps.each do |map|
      # there is only 1 map per barclamp/jig/attrib
      a = map.attrib_type
      # there can be multiple Attribs per node/barclamp snapshot
      attribs = Attrib.where :attrib_type_id=>a.id, :node_id=>node.id
      if attribs.empty?
        # create the AIs for the data using the unbound role attribes that are already there
        unset_attribs = Attrib.where :attrib_type_id=>a.id, :node_id => nil
        unset_attribs.each do |na|
          # attach node to barclamp data (from role association)
          if na.barclamp.id == self.id
            # create a node specific version of it
            node_attrib = na.dup
            node_attrib.node_id = node.id
            node_attrib.save
          end
        end
      end
      # THIS NEEDS TO BE UPDATED TO ONLY UPDATE THE ACTIVE SNAPSHOTS!
      attribs.each do |ai|
        # we only update the attribs linked to this barclamp 
        # performance note: this is an expensive thing to figure out!
        if !ai.role_id.nil? and ai.barclamp.id == self.id 
          # get the value
          value = jig.find_attrib_in_data data, map.map
          # store the value
          target = Attrib.find ai.id
          target.actual = value
          target.jig_run_id = jig_run.id
          target.save!
        end
      end
    end
    node
  end

  # Parse the deployment section of a barclamps template 
  # The following sections are parsed:
  #  - element_order - grouping of roles to execute in parallel/serial.
  #  - element_states - node states in which roles are allowed to execute
  #  - element_run_list_order - role priorities
  #  - transitions - should transitions be passed to the bc.
  #  - transition_list - which state transitions to pass to barclamp
  def import_template(json=nil, template_file=nil)

    template_file ||= File.expand_path(File.join(source_path,"bc-template-#{name}.json"))
    template_file = File.expand_path(File.join(source_path,"config-template.json")) unless File.exists? template_file 

    return unless File.exists?(template_file)

    json = JSON::load File.open(template_file, 'r') if json.nil?

    create_template template_file

    # add the roles & attributes
    jdeploy = json["deployment"][name]
    jdeploy["element_order"].each_with_index do |role_hash, top_index|
      role_hash.each_with_index do |role, index|
        unless role.nil?
          states = jdeploy["element_states"][role].join(",") rescue "all"
          order = 100+(top_index*100)+index
          run_order = jdeploy["element_run_list_order"][role] rescue order
          ri = self.template.add_role role
          ri.update_attributes( :states => states,
                                :order => order,
                                :run_order => run_order, 
                                :description=> I18n.t('imported', :scope => 'model.barclamp', :file=>template_file)  
                              )
        end
      end
    end

    # theses need to move into AttribInstnaces
    mode = jdeploy["config"]["mode"] rescue "full"
    transitions = jdeploy["config"]["transitions"] rescue false
    transition_list = jdeploy["config"]["transition_list"].join(",") rescue ""
    # add environment

    jattrib = json["attributes"][name]
    role = self.template.add_role name
    jattrib.each do |key, value|
      # this will handle strings or hashes
      role.add_attrib key, value
    end unless jattrib.nil?

    # import users 
    users = json["attributes"]["crowbar"]["users"] rescue Hash.new
    users.each do |user, password|
      pass = password['password']
      u = User.find_or_create_by_username!(:username=>user.dup, :password=>pass.dup, :is_admin=>true)
      u.digest_password(pass)   # this is required if we want API access
      u.save!
    end
    # Create the machine-install user.
    unless User.find_by_username("machine-install")
      if File.exists?('/etc/crowbar.install.key')
        user,pass = IO.read('/etc/crowbar.install.key').strip.split(':',2)
      else
        user="machine-install"
        pass = %x{dd if=/dev/urandom bs=65536 count=1 2>/dev/null |sha1sum - 2>/dev/null}.strip.split[0]
        system("sudo -i 'echo \"#{user}:#{pass}\" > /etc/crowbar.install.key'")
      end
      u = User.create(:username => "machine-install", :password => pass, :is_admin => true)
      u.digest_password(pass)
      u.save!
    end
  end


  # Import from existing Config data
  def self.import_1x(bc_name, bc=nil, source_path=nil)
    self.import bc_name, bc, source_path
  end
  def self.import(bc_name, bc=nil, source_path=nil)
    barclamp = Barclamp.find_or_create_by_name(bc_name)
    source_path ||= "../barclamps/#{bc_name}"    # would be nice to be smarter, but this is OK for now
    bc_file = File.expand_path(File.join(source_path, "crowbar.yml"))
    # load JSON
    if bc.nil?
      raise "Barclamp metadata #{bc_file} for #{bc_name} not found" unless File.exists?(bc_file)
      bc = YAML.load_file bc_file
      raise 'Barclamp name must match name from YML file' unless bc['barclamp']['name'].eql? bc_name
    end
    
    Barclamp.transaction do
    
      # Can't do the || trick booleans because nil is false.
      amp = bc['barclamp']['allow_multiple_deployments'] rescue nil
      amp ||= bc['barclamp']['allow_multiple_proposals'] rescue false
      um = bc['barclamp']['user_managed'] rescue true
      gitcommit = "unknown" if bc['git'].nil? or bc['git']['commit'].nil?
      gitdate = "unknown" if bc['git'].nil? or bc['git']['date'].nil?
      barclamp.update_attributes( :display     => bc['barclamp']['display'] || bc_name.humanize,
                                  :description => bc['barclamp']['description'] || bc_name.humanize,
                                  :online_help => bc['barclamp']['online_help'],
                                  :version     => bc['barclamp']['version'] || 2,
                                  :api_version => bc['barclamp']['api_version'] || "v2",
                                  :api_version_accepts => bc['barclamp']['api_version_accepts'] || "|v2|",
                                  :license     => bc['barclamp']['license'] || "apache2",
                                  :copyright   => bc['barclamp']['copyright'] || "Dell, Inc 2013",
                                  :source_path => source_path,
                                  :user_managed=> um || true,
                                  :allow_multiple_proposals => amp || false,
                                  :proposal_schema_version => bc['crowbar']['proposal_schema_version'] || 2,
                                  :layout      => bc['crowbar']['layout'] || 2,
                                  :order       => bc['crowbar']['order'] || 0,
                                  :run_order   => bc['crowbar']['run_order'] || 0,
                                  :jig_order  => bc['crowbar']['chef_order'] || 0,
                                  :mode        => "full",
                                  :transitions => false,
                                  :build_on    => (gitdate || 'unknown'),
                                  :commit      => (gitcommit || 'unknown')   )
      barclamp.save
      
      # memberships (if memembership is missing, we'll let you into the club anyway)
      if bc['barclamp']['member']
        bc['barclamp']['member'].each do |owner|
          o = Barclamp.find_by_name owner
          o.members << barclamp if o and !o.members.include? barclamp
        end
      end
  
      # requires (will fail if prereq is missing)
      # Don't do this for now, as it will be broken until
      # all the barclamps are engine-ized.
      if bc['barclamp']['requires'] && false
        bc['barclamp']['requires'].each do |prereq|
          prereq = prereq[1..-1] if prereq.starts_with? "@"
          pre = Barclamp.find_by_name prereq
          raise "ERROR: Cannot load barclamp #{bc_name} because prerequisite #{prereq} has not been imported" if pre.nil?
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
  
      barclamp.create_template bc_file
      # all deployment details get imported into the template
      barclamp.import_template
    
    end # transaction  

    return barclamp
  end

  # make the our tempate
  def create_template(bc_file)

    if self.template_id.nil?
      t = Snapshot.create(
                :name => I18n.t('template', :scope => "model.barclamp", :name=>self.name.humanize),
                :barclamp_id=>self.id,
                :description=> I18n.t('imported', :scope => 'model.barclamp', :file=>bc_file)
              )
      self.template_id = t.id
      # attach the default private role
      ri = t.add_role RoleType.find_private
      ri.order = 1
      ri.run_order = -1   # this tells Crowbar NOT to give the information to the Jig
      ri.description = I18n.t('model.barclamp.private_role_description'),
      ri.save
      save
    end
    
  end
  
  private 
  
  # This method ensures that we have a type defined for 
  def create_type_from_name
    raise "barclamps require a name" if self.name.nil?
    file = "#{self.name}"
    myclass = "Barclamp#{self.name.camelize}::Barclamp"
    # this will need to be fixed for the engines!!
    file = File.join 'app','models',"barclamp_#{self.name}", "barclamp.rb"
    if !self.type.nil?
      # do nothing - everything is OK
    elsif File.exist? file
      self.type = myclass
    else
      Rails.logger.warn "Creating barclamp #{self.name} using the generic model because the #{file} was not found."
      self.type = "BarclampFramework"     # fall back to generic model
    end
  end
     
end

