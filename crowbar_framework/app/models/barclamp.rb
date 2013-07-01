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
  attr_accessible :layout, :order
  attr_accessible :commit, :build_on, :mode
  attr_accessible :allow_multiple_deployments
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
                                  :layout      => bc['crowbar']['layout'] || 2,
                                  :order       => bc['crowbar']['order'] || 0,
                                  :mode        => "full",
                                  :build_on    => (gitdate || 'unknown'),
                                  :commit      => (gitcommit || 'unknown')   )
      barclamp.save

    end

    if bc['jigs'] 

      # iterate over the jigs in the YML file and load each role
      bc['jigs'].each do |jig_name, role_list|

        jig = Jig.find_by_name jig_name
        # we only import if the jig exists, we assume that adding a jig will for a review!
        if jig and role_list

          # in each jig, inspect each role
          role_list['roles'].each do |role_name, details|
            # retrieve info from the role meta data
            details ||= {}
            role_template = File.join source_path, jig_name, 'roles', role_name, 'role_template.json'
            node_template = File.join source_path, jig_name, 'roles', role_name, 'node_template.json'
            requires = details['requires'] || []
            flags = details['flags'] || []
            description = details['descripion'] || "imported by #{barclamp.name}"
            # roles data import
            Role.transaction do
              r = Role.create :name=>role_name, :jig_id=>jig.id, :description=>description, :barclamp_id=>barclamp.id, 
                      :library=>flags.include?('library'), :implicit=>flags.include?('implicit'), 
                      :bootstrap=>flags.include?('bootstrap'), :discovery=>flags.include?('discovery')
              requires.each { |req| RolesRequire.create :role_id=>r.id, :require=>req }
            end
          end
        end
      end
    end

    return barclamp
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

