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
    
    # create the jobs that you need heere!!
    
    # WIP by Rob H
    #self.members.each do |bc|
    #  if bc.deployments.count == 0
    #    bc.create_proposal name
    #  end
    #end
    super name
    
  end

  # called when a deployment is committed
  # creates jobs needed for the commit
  def commit_deployment(deployment)
    
    # create the jobs that you need here!!
    self.members.each do |bc|
      if bc.deployments.count == 0
        bc.create_deployment Barclamp::DEFAULT_DEPLOYMENT_NAME
      end
    end
        
  end
  
  # called by the jib when the node changes it's state
  # creates jobs needed for the state
  def transition(snapshot, node, state, role_type_name=nil)
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

    super snapshot, node, state, role_type_name

  end

end
