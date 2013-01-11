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
class Cmdb < ActiveRecord::Base

  attr_accessible :name, :description, :type, :order

  # 
  # Validate the name should unique 
  # and that it starts with an alph and only contains alpha,digist,hyphen,underscore
  #
  validates_uniqueness_of :name, :case_sensitive => false, :message => I18n.t("db.notunique", :default=>"Name item must be unique")
  validates_format_of :name, :with=> /^[a-zA-Z][_a-zA-Z0-9]*$/, :message => I18n.t("db.lettersnumbers", :default=>"Name limited to [_a-zA-Z0-9]")
  
  has_many :maps, :class_name => CmdbMap.name, :foreign_key => "cmdb_id"
  #TEMPORARY REMOVAL... has_many :cmdb_events, :inverse_of => Cmdb

  #####
  #  Find the right instance to use for applying the given configuration.
  # At this point, there's one - return it.
  # Some future possible directions:
  #   - Choose cmdb type by the barclamp being applied - allows different barclamps
  #     to be implemented using different technologies
  #   - Choose a cmdb instance, by node location. Could allow mixing cmdb domains
  #     where different instances (probably of the same type) manage different 
  #     domains of nodes
  def self.find_cmdb_for_config(config)
    Cmdb.find_by_name('admin_chef')
  end

  def self.prepare_proposal(new_config)
    # collect all the depenent configurations

    Cmdb.transaction do 
      cmdb = Cmdb.find_cmdb_for_config(new_config)    
      evt = cmdb.create_event(new_config)
  
      # create a CmdbEvent for each unique cmdb execution that needs to be performed
      # on each node. Events are tied in dependency list (to allow subsequent events)
      # to be fired when their dependencies complete.
      nrs = {}
      new_config.node_roles.each { |nr| 
        name = nr.role.name
        nrs[name] =[] unless nrs[name]
        nrs[name] << nr
      }
      logger.debug("Node roles, #{nrs.inspect}")
      #node_roles = new_config.get_nodes_by_roles    # hash of role -> list of nodes     
      ordered_roles = new_config.barclamp.get_roles_by_order # array of: arry of role.
      order = 1
      ordered_roles.each { |r_list|             
        r_list.each { |r| 
          logger.debug("handling #{r}, which has #{nrs[r.name].inspect} ")
          nrs[r.name].each { |nr| 
            # create events for nodes that are not currently in the right state,
            # they won't be executed until the node does transition to the right place.
            # next unless  r.states.include?("all") or n.states.include?(n.state)
            cmdb.create_run_for(evt,nr,order);
          } if nrs[r.name]  ### barclamp might have roles that have no nodes...
        }
        order +=1
      }
      evt.save!
      evt
    end
  end

  # compute event for execution by computing whatever the cmdb backend needs
  def prepare_for_execution(evt,config)
    # here for sub-classes to override.
  end
    
  def as_json options={}
   {
     :name=> name,
     :order=> order,
     :id=> id,
     :description=> description,
     :type=> type,
     :created_at=> created_at,
     :updated_at=> updated_at
   }
  end
  


end
