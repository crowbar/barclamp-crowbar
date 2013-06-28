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


class Role < ActiveRecord::Base

  attr_accessible :id, :description, :order, :name, :jig_id, :barclamp_id
  attr_accessible :library, :implicit, :bootstrap, :discovery

  validates_uniqueness_of   :name,         :scope => :barclamp_id
  validates_uniqueness_of   :name,         :scope => :jig_id
  validates_format_of       :name, :with=>/^[a-zA-Z][-_a-zA-Z0-9]*$/, :message => I18n.t("db.lettersnumbers", :default=>"Name limited to [_a-zA-Z0-9]")

  belongs_to      :snapshot
  has_one         :barclamp,          :through => :snapshot
  has_one         :deployment,        :through => :snapshot
  
  has_many        :attribs,           :dependent => :destroy
  has_many        :role_attribs,      :class_name => "Attrib", :conditions=>'node_id IS NULL'
  has_many        :node_attribs,      :class_name => "Attrib", :conditions=>'node_id IS NOT NULL'
  has_many        :attrib_types,      :through => :attribs

  has_many        :roles_requires,    :class_name => "RolesRequire"
  alias_attribute :requires,          :roles_requires

  # determines if the requirements have been met for this barclamp
  def deployable?
    # TODO
  end

  ##
  # Clone this role
  # optionally, change parent too
  # with_nodes allows for template copies that should have not nodes assigned yet
  def deep_clone(snapshot=nil, clone_with_nodes=true)
    new_role = self.dup
    new_role.snapshot_id = snapshot.read_attribute(:id) if snapshot
    new_role.save

    # clone the attributes (includes node-roles)
    attribs.each do |ai|
      # if we are cloning a template then we need the option of no nodes
      if clone_with_nodes or ai.node_id.nil?
        new_ai = ai.dup
        new_ai.role_id = new_role.id
        new_ai.jig_run_id = nil
        new_ai.save
      end
    end

    new_role
    
  end
  
  # take run data from the jig and process it into attributes
  # returns the node
  # WARNING - this has NOT been optimized!
  def process_inbound_data jig, node, data
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

end
