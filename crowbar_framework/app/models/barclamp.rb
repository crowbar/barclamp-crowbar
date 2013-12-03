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

  attr_accessible :id, :name, :description, :display, :version, :online_help
  attr_accessible :user_managed, :type, :source_path, :layout, :commit
  attr_accessible :requirements, :members
  attr_accessible :build_on, :mode, :allow_multiple_deployments
  attr_accessible :api_version, :api_version_accepts, :license, :copyright, :proposal_schema_version
  before_create :create_type_from_name
  #
  # Validate the name should unique
  # and that it starts with an alph and only contains alpha,digits,underscore
  #
  validates_uniqueness_of :name, :case_sensitive => false, :message => I18n.t("db.notunique", :default=>"Name item must be unique")
  validates_exclusion_of :name, :in => %w(framework api barclamp docs machines jigs roles groups users support application), :message => I18n.t("db.barclamp_excludes", :default=>"Illegal barclamp name")

  validates_format_of :name, :with=>/^[a-zA-Z][_a-zA-Z0-9]*$/, :message => I18n.t("db.lettersnumbers", :default=>"Name limited to [_a-zA-Z0-9]")

  # Deployment
  has_many :roles,              :dependent => :destroy

  #
  # Order barclamps by their dependency trees and then their name
  #
  def <=>(other)
    [parents.length,name] <=> [other.parents.length,other.name]
  end

  #
  # We should set this to something one day.
  #
  def versions
    [ "2.0" ]
  end

  #
  # Human printable random password generator
  #
  def self.random_password(size = 12)
    chars = (('a'..'z').to_a + ('0'..'9').to_a) - %w(o 0 O i 1 l)
    (1..size).collect{|a| chars[rand(chars.size)] }.join
  end

  # indended to be OVERRIDEN for barclamps that want to validate deployments
  # called before the proposal is committed
  # was validate deployment in CB1
  def is_valid?(deployment)
    true
  end

  # The barclamp groups of which I am a member.
  def groups
    members.split(",")
  end

  # The names of all the barclamps that are my parents.
  def parents(bcs = Barclamp.all)
    pnames,grps = requirements.split(',').partition{|i|i[0] != '@'}
    immediate_parents= bcs.select{|bc|pnames.member?(bc.name)}
    grps.each do |g|
      immediate_parents += bcs.select{|bc|bc.groups.include?(g)}
    end
    immediate_parents.uniq!
    res = Array.new
    immediate_parents.each do |p|
      res += p.parents(bcs)
    end
    res += immediate_parents
    res.uniq
  end

  # called by the jig when the node changes it's state
  def transition(role, nodes, state, status)

    Rails.logger.debug "Barclamp transition enter: #{name} to #{state} with #{status}"

    # TODO ZEHICLE change node-role to new state

  end

  def self.import_1x(bc_name, bc=nil, source_path=nil)
    puts "ALERT!! change to Barclamp.import for #{bc_name}!"
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

    # barclamp data import
    Barclamp.transaction do
      # Can't do the || trick booleans because nil is false.
      amp = bc['barclamp']['allow_multiple_deployments'] rescue nil
      amp ||= bc['barclamp']['allow_multiple_proposals'] rescue false
      um = bc['barclamp']['user_managed'] rescue true
      gitcommit = "unknown" if bc['git'].nil? or bc['git']['commit'].nil?
      gitdate = "unknown" if bc['git'].nil? or bc['git']['date'].nil?
      ## TODO: Add checking to validate that adding this barclamp will not
      ## result in circular dependencies.
      reqs = if bc['barclamp']['requires'] &&
                 !bc['barclamp']['requires'].empty?
               bc['barclamp']['requires'].join(",")
             elsif bc_name == "crowbar"
               ""
             else
               "crowbar"
             end
      Rails.logger.info("#{bc_name} requires #{reqs}")
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
                                  :allow_multiple_deployments => amp || false,
                                  :proposal_schema_version => bc['crowbar']['proposal_schema_version'] || 2,
                                  :members     => (bc['barclamp']['members'] || []).join(","),
                                  :requirements => reqs,
                                  :layout      => bc['crowbar']['layout'] || 2,
                                  :mode        => "full",
                                  :build_on    => (gitdate || 'unknown'),
                                  :commit      => (gitcommit || 'unknown')   )
      barclamp.save
    end
    # load the jig information.
    bc['jigs'].each do |jig|
      raise "Jigs must have a name" unless jig['name'] && !jig['name'].empty?
      raise "Jigs must have a type" unless jig['class'] && !jig["class"].empty?
      jig_name = jig["name"]
      jig_desc = jig['description'] || "Imported by #{barclamp.name}"
      jig_type = jig['class']
      jig_client_role = jig["implementor"]
      jig_active = if (Rails.env == "production")
                     jig_name != "test"
                   else
                     ["noop","test"].include? jig_name
                   end
      jig = jig_type.constantize.find_or_create_by_name(:name => jig_name)
      jig.update_attributes(:order => 100,
                            :active => jig_active,
                            :description => jig_desc,
                            :type => jig_type,
                            :client_role_name => jig_client_role)
      jig.save!
    end if bc["jigs"]

    # iterate over the roles in the yml file and load them all.
    # Jigs are now late-bound, so we just load everything.
    bc['roles'].each do |role|
      role_name = role["name"]
      role_jig = role["jig"]
      prerequisites = role['requires'] || []
      wanted_attribs = role['wants-attribs'] || []
      flags = role['flags'] || []
      description = role['descripion'] || role_name.gsub("-"," ").titleize
      template = File.join barclamp.source_path, role_jig || "none", 'roles', role_name, 'role-template.json'
      # roles data import
      ## TODO: Verify that adding the roles will not result in circular role dependencies.
      r = nil
      Role.transaction do
        r = Role.find_or_create_by_name(:name=>role_name, :jig_name => role_jig, :barclamp_id=>barclamp.id)
        r.update_attributes(:description=>description,
                            :barclamp_id=>barclamp.id,
                            :template=>(IO.read(template) rescue "{}"),
                            :library=>flags.include?('library'),
                            :implicit=>flags.include?('implicit'),
                            :bootstrap=>flags.include?('bootstrap'),
                            :discovery=>flags.include?('discovery'),
                            :server=>flags.include?('server'),
                            :destructive=>flags.include?('destructive'),
                            :cluster=>flags.include?('cluster'))
        RoleRequire.where(:role_id=>r.id).delete_all
        RoleRequireAttrib.where(:role_id => r.id).delete_all
        r.save!
        prerequisites.each { |req| RoleRequire.create :role_id => r.id, :requires => req }
        wanted_attribs.each{ |attr| RoleRequireAttrib.create :role_id => r.id, :attrib_name => attr }
      end
      role['attribs'].each do |attrib|
        attrib_name = attrib["name"]
        attrib_desc = attrib['description'] || ""
        attrib_map = attrib['map'] || ""
        a = Attrib.find_or_create_by_name(:name => attrib_name,
                                          :description => attrib_desc,
                                          :map => attrib_map,
                                          :role_id => r.id,
                                          :barclamp_id => barclamp.id)
        a.save!
      end if r && role['attribs']
    end if bc['roles']
    bc['attribs'].each do |attrib|
      attrib_name = attrib["name"]
      attrib_desc = attrib['description'] || ""
      attrib_map = attrib['map'] || ""
      a = Attrib.find_or_create_by_name(:name => attrib_name,
                                        :description => attrib_desc,
                                        :map => attrib_map,
                                        :barclamp_id => barclamp.id)
      a.save!
    end if bc['attribs']
    barclamp
  end

  private

  # This method ensures that we have a type defined for
  def create_type_from_name
    raise "barclamps require a name" if self.name.nil?
    namespace = "Barclamp#{self.name.camelize}"
    # these routines look for the namespace & class,
    m = Module::const_get(namespace) rescue nil
    if m
      c = m.const_get("Barclamp") rescue nil
    end
    # if they dont' find it we fall back to BarclampFramework (this should go away!)
    self.type = if c.nil?
      Rails.logger.warn "Barclamp #{self.name} created with fallback Model!"
      "BarclampFramework"
    else
      "#{namespace}::Barclamp"
    end

  end

end
