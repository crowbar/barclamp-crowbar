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
  
  has_many :proposals

  ## dependnecies are tracked using an explict join-table, barclamp_dependncies
  ## to add a dependency, create one of those, setting prereq to the depend
  # A quick way to achieve that is (b1,b4 are barclamp instances)
  # b1.barclamp_dependencies << BarclampDependency.create( { :barclamp =>b1, :prereq =>b4} )
  has_many :barclamp_dependencies, :inverse_of => :barclamp
  has_many :prereqs, :class_name => "Barclamp", :through => :barclamp_dependencies

  
  #legacy approach - expects name of barclamp for YML import
  def self.import_1x(barclamp)
    bc_file = File.join('barclamps', barclamp+'.yml')
    throw "Barclamp import file #{bc_file} not found" unless File.exist? bc_file
    bc = YAML.load_file bc_file
    throw 'Barclamp name must match name from YML file' unless bc['barclamp']['name'].eql? barclamp
    barclamp = Barclamp.create(
        :name        => barclamp,
        :display     => bc['barclamp']['display'] || barclamp.humanize,
        :description => bc['barclamp']['description'] || barclamp.humanize,
        :online_help => bc['barclamp']['online_help'],
        :version     => bc['barclamp']['version'] || 2,
        
        :proposal_schema_version => bc['crowbar']['proposal_schema_version'] || 2,
        :layout      => bc['crowbar']['layout'] || 2,
        :order       => bc['crowbar']['order'] || 0,
        :run_order   => bc['crowbar']['run_order'] || 0,
        :cmdb_order  => bc['crowbar']['chef_order'] || 0,

        :commit      => bc['git']['commit'],
        :build_on    => bc['git']['date'] 
      )
    return barclamp
  end
  
end

