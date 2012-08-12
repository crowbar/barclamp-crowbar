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
  
  validates_uniqueness_of :name, :message => I18n.t("db.notunique", :default=>"Name item must be unique")
  validates_format_of :name, :with=>/[a-zA-Z][_a-zA-Z0-9]/, :message => I18n.t("db.lettersnumbers", :default=>"Name limited to [_a-zA-Z0-9]")
  
  has_many :proposals, :conditions => 'name != "template"'
  has_many :active_proposals, 
                :class_name => "Proposal", 
                :conditions => [ 'name <> ? AND active_config_id IS NOT NULL', "template"]
  has_one :template, :class_name => "Proposal", :conditions => 'name = "template"'

  has_many :roles

  has_and_belongs_to_many :packages, :class_name=>'OsPackage', :join_table => "barclamp_packages"
  has_and_belongs_to_many :prereqs, :class_name=>'Barclamp', :join_table => "barclamp_dependencies", :foreign_key => "prereq_id"
  has_and_belongs_to_many :members, :class_name=>'Barclamp', :join_table=>'barclamp_members', :foreign_key => "member_id", :order => "[order], [name] ASC"

  #
  # Helper function to load the service object
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
  # We should set this to something one day.
  #
  def versions
    [ "1.0" ]
  end

  #
  # Proposal manipulation functions
  #
  def create_proposal(name = nil)
    prop = template.deep_clone
    prop.name = name || "created#{Time.now}"
    prop.save!
    prop
  end

  def delete_proposal(prop)

  end

  def get_proposal(name)
    Proposal.find_by_name_and_barclamp_id(name, self.id)
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

        :commit      => bc['git']['commit'],
        :build_on    => bc['git']['date'] 
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

      barclamp.mode = json["deployment"][bc_name]["mode"] rescue "full"
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

