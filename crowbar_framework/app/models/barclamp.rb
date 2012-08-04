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
  attr_accessible :name, :description, :display, :version, :online_help
  attr_accessible :proposal_schema_version, :layout, :order, :run_order, :cmdb_order
  attr_accessible :commit, :build_on
  
  validates_uniqueness_of :name, :on => :create, :message => I18n.t("db.notunique", :default=>"Name item must be unique")
  
  has_many :proposals, :conditions => 'name != "template"'
  has_many :active_proposals, 
                :class_name => "Proposal", 
                :conditions => [ 'name <> ? AND active_config_id IS NOT NULL', "template"]
  has_one :template, :class_name => "Proposal", :conditions => 'name = "template"'

  has_many :roles

  ## dependnecies are tracked using an explict join-table, barclamp_dependncies
  ## to add a dependency, create one of those, setting prereq to the depend
  # A quick way to achieve that is (b1,b4 are barclamp instances)
  # b1.barclamp_dependencies << BarclampDependency.create( { :barclamp =>b1, :prereq =>b4} )
  has_many :barclamp_dependencies, :inverse_of => :barclamp
  has_many :prereqs, :class_name => "Barclamp", :through => :barclamp_dependencies

  #
  # Helper Barclamp function to get a list of barclamps and descriptions.
  #
  def self.get_all_descriptions
    bcs = find(:all)
    ans = {}
    bcs.each do |x|
      ans[x.name] = x.description
    end if bcs
    ans
  end

  #
  # Helper function to load the service object
  #
  def operations(logger = nil)
    @service = eval("#{name.camelize}Service.new logger") unless @service
    @service
  end

  #
  # We should set this to something one day.
  #
  def versions
    [ "1.0" ]
  end

  
  #legacy approach - expects name of barclamp for YML import
  def self.import_1x(bc_name)
    bc_file = File.join('barclamps', bc_name+'.yml')
    throw "Barclamp import file #{bc_file} not found" unless File.exist? bc_file
    bc = YAML.load_file bc_file
    throw 'Barclamp name must match name from YML file' unless bc['barclamp']['name'].eql? bc_name
    barclamp = Barclamp.create(
        :name        => bc_name,
        :display     => bc['barclamp']['display'] || bc_name.humanize,
        :description => bc['barclamp']['description'] || bc_name.humanize,
        :online_help => bc['barclamp']['online_help'],
        :version     => bc['barclamp']['version'] || 2,
        
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

    template_file = File.join('barclamps', 'templates', "bc-template-#{bc_name}.json")
    if File.exists? template_file
      json = JSON::load File.open(template_file, 'r')

      barclamp.mode = json["deployment"][bc_name]["mode"] rescue "full"
      barclamp.description = (json["description"] rescue bc_name.humanize) unless bc['barclamp']['description']
      barclamp.transitions = json["deployment"][bc_name]["config"]["transitions"] rescue false
      barclamp.transition_list = json["deployment"][bc_name]["config"]["transition_list"].join(",") rescue ""

      element_order = json["deployment"][bc_name]["element_order"].flatten.uniq rescue []
      element_order.each do |role|
        states = json["deployment"][bc_name]["element_states"][role].join(",") rescue "all"
        role = Role.create(:name => role, :states => states)
        role.barclamp = barclamp
        role.save
      end

      prop = Proposal.create(:name => "template", :status => "template")
      prop_config = ProposalConfig.create(:config => json["attributes"].to_json)
      prop_config.proposal = prop
      prop_config.save

      barclamp.template = prop
      barclamp.save
    end

    return barclamp
  end

end

