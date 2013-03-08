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
class Jig < ActiveRecord::Base

  attr_accessible :name, :description, :type, :order

  # 
  # Validate the name should unique 
  # and that it starts with an alph and only contains alpha,digist,hyphen,underscore
  #
  validates_uniqueness_of :name, :case_sensitive => false, :message => I18n.t("db.notunique", :default=>"Name item must be unique")
  validates_format_of :name, :with=> /^[a-zA-Z][_a-zA-Z0-9]*$/, :message => I18n.t("db.lettersnumbers", :default=>"Name limited to [_a-zA-Z0-9]")

  has_many        :jig_events,    :dependent => :destroy 
  alias_attribute :events,        :jig_events
  has_many        :jig_runs,      :through => :jig_events
  alias_attribute :runs,          :jig_runs
  
  has_many        :jig_maps,      :dependent => :destroy
  alias_attribute :maps,          :jig_maps
  has_many        :barclamps,     :through => :jig_maps
  has_many        :attrib_types,  :through => :jig_maps

  #####
  #  Find the right snapshot to use for applying the given deployment.
  # At this point, there's one - return it.
  # Some future possible directions:
  #   - Choose jig type by the barclamp being applied - allows different barclamps
  #     to be implemented using different technologies
  #   - Choose a jig snapshot, by node location. Could allow mixing jig domains
  #     where different snapshots (probably of the same type) manage different 
  #     domains of nodes
  def self.find_jig_for_config(deployment)
    Jig.find_by_name('admin_chef')
  end


=begin 
Allocate a node, and start the node install process
=end
  def self.install_node(node)

  end

  # OVERRIDE action to add a node to the environment
  def create_node(node)
    # do nothing, 
  end
  
=begin
 Update node infomration from a Jig, and process node attributes 
=end
  def self.refresh_node(node)
  end

=begin 
Start the process of instantiating a deployment into the environemt.
Expecting the deployment to be "static" - i.e. not actively being modified.
=end
  def self.commit_proposal(deployment)
    deployment.commit
    # update the status of the deployment to committed 

     # function role - created during barclamp import.
     # need a list of the roles:
     #   - the role list
     #   - according to run element order: [[r1,r2,r3] [r4,r5,r6]]
     #This comes from: deployment.
     # deployment elements, nodes they apply to.
     # This info is used to created:
     #   1. the runs needed - each role x node combination generates a Run.
     #   2. a set of jobs to reflect the dependencies between the runs, as reflected in the elements order
     # these will be named e.g. crowbar-bc-ntp

 
     # snapshot config role - holds the configuraiton from the deployments configuration
     #  e.g. crowbar-deploy-ntp-<MyNTP>
     # To populate it need: - non-node specific configuration for the deployment.
     # From snapshot.roles.role_attribs
     # (for now this will not include private role info.)
     #

     # node role - node specific info e.g crowbar-node-<node-name>
     # snapshot -> role -> node.attribs


  end


  def self.prepare_proposal(new_deployment)
    # collect all the depenent deployments

    Jig.transaction do 
      jig = Jig.find_jig_for_config(new_deployment)    
      evt = jig.create_event(new_deployment)
  
      # create a JigEvent for each unique jig execution that needs to be performed
      # on each node. Events are tied in dependency list (to allow subsequent events)
      # to be fired when their dependencies complete.
      nrs = {}
      new_deployment.proposed.roles.each { |nr| 
        name = nr.role.name
        nrs[name] =[] unless nrs[name]
        nrs[name] << nr
      }
      logger.debug("Node roles, #{nrs.inspect}")
      #node_roles = new_deployment.get_nodes_by_roles    # hash of role -> list of nodes     
      ordered_roles = new_deployment.proposed.roles # array of: arry of role.
      order = 1
      ordered_roles.each { |r_list|             
        r_list.each { |r| 
          logger.debug("handling #{r}, which has #{nrs[r.name].inspect} ")
          nrs[r.name].each { |nr| 
            # create events for nodes that are not currently in the right state,
            # they won't be executed until the node does transition to the right place.
            # next unless  r.states.include?("all") or n.states.include?(n.state)
            jig.create_run_for(evt,nr,order);
          } if nrs[r.name]  ### barclamp might have roles that have no nodes...
        }
        order +=1
      }
      evt.save!
      evt
    end
  end

  # compute event for execution by computing whatever the jig backend needs
  def prepare_for_execution(evt,config)
    # here for sub-classes to override.
  end

  # OVERRIDE with actual delete effort
  def delete_node(node)
    Rails.logger.debug("jig.delete_node(#{node.name}) not implemented for #{self.class}.  This may be OK")
  end
  
  # setup the Jig event and ` events
  # RETURNS JigRun object approprate for the Jig  
  def run
    event = JigEvent.create :name=>DateTime.now.to_s, :jig_id => self.id, :description=>"Running #{self.name}"
    JigRun.create :jig_event_id => event.id, :name => event.name+"_1", :description=>"Running #{self.name}"
  end
  
  # SUBCLASS THIS METHOD if you want to change how data is found in the input data
  # Called by the barclamp.process_inbound_data routine
  # find a single attribute in a json data set
  # / is used as a delimiter
  # optimized to 6 levels without looping
  def find_attrib_in_data(data, path)
    nav = path.split '/'
    # add some optimization to avoid looping down through the structure
    case nav.length 
      when 1 
        data[nav[0]]
      when 2
        data[nav[0]][nav[1]]
      when 3
        data[nav[0]][nav[1]][nav[2]]
      when 4
        data[nav[0]][nav[1]][nav[2]][nav[3]]
      when 5
        data[nav[0]][nav[1]][nav[2]][nav[3]][nav[4]]
      when 6
        data[nav[0]][nav[1]][nav[2]][nav[3]][nav[4]][nav[5]]
      else 
        nav.each { |key| data = data[key] }
    end
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
