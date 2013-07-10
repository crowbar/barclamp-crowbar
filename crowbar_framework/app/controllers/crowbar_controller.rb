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

class CrowbarController < ApplicationController

  THROTTLE = 1

  def turn(deployment)
  
    # ZEHICLE TODO - this is in develoment!!

    count = 1
    return unless deployment.committed?

    candidates = deployment.node_roles
    myturn = nil

    # remove candidates that are not todo or proposed
    # we'd like to have some randomization of which canidate get run next

    if candidates.length == 0
      deployment.activate 
    else
      NodeRole.transaction do 
        return if candidates.any? { |c| c.error? }
        # zehicle - we need to do turns per jig? yes
        myturn = Turn.create 
        candidates.each do |c| 
          c.turn = myturn if c.excutable? 
          count += 1
          # we want a way to limit the number of canidates processed in each turn
          break if count > THROTTLE
        end
      end

      Jig.turn(myturn)  # background runner started w/ turn to each jig

    end

  end
      
end

