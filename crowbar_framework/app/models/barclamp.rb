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
  attr_accessible :commit, :build_on
  
  validates_uniqueness_of :name, :on => :create, :message => I18n.t("db.notunique", :default=>"Name item must be unique")
  
  has_many :proposals

  has_many :barclamp_dependencies
  has_many :prereqs, :through=>:barclamp_dependencies
  
  has_many :barclamp_members
  has_many :members, :through=>:barclamp_members

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
        :user_managed=> bc['barclamp']['user_managed'] || true,
        
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

