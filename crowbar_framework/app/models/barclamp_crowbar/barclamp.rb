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

# This class is the fall back class for barclamps that are missing Barclamp subclasses
class BarclampCrowbar::Barclamp < Barclamp

  def create_proposal(name=nil)
    
    deployment = super name
    deployment ||= deployments.first
    
    # add links for the dependenant barclamps
    requires = ['deployer', 'provisioner', 'ntp', 'network', 'chef', 'dns', 'logging']
    requires.each do |bc|
      if Barclamp.find_by_name bc
        begin 
          attrib = deployment.proposal.crowbar_role.require_deployment bc, deployment.name
          Rails.logger.info "#{self.name} added required barclamp #{bc} from attrib id #{attrib.id}"
        rescue Exception => e
          Rails.logger.error "#{self.name} could not add required barclamp #{bc} due to error #{e.message}"
        end
      else
        Rails.logger.error "#{self.name} requires barclamp #{bc} but it does not exist in the database"
      end
    end
    
    return deployment
    
  end

  # called when a deployment is committed
  # creates jobs needed for the commit
  def commit_deployment(deployment)

    # let's check all the roles in this deployment
    deployment.proposed.roles.each do |role|
      
      # make sure that we have committed dependent barclamps
      role.prerequisites.each do |prereq|
        prereq.commit unless prereq.active? 
      end
      
      # create the prereq jobs that you need here!!
      
    end

    # create the jobs that you need here!!
        
  end
  
  # called by the jib when the node changes it's state
  # creates jobs needed for the state
  def transition(snapshot, node, state, role_name=nil)
    Rails.logger.debug "Crowbar transition enter: #{name} to #{state}"

    # for all transitions
    Snapshot.transaction do
      case state
      when 'discovering','testing' 
        Rails.logger.debug "Crowbar transition: creating new node for #{name} to #{state}"
        # not sure if this should do anything, place holder ofr now
      else
        # nothing
      end
    end

    super snapshot, node, state, role_name

  end

end
