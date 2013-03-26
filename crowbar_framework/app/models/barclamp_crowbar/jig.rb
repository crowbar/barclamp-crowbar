# Copyright 2013, Dell
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
#
# This model is a stub for the Jig override system
# It is NOT installed by default, but can be used for testing or as a model

require 'json'

class BarclampCrowbar::Jig < Jig

  def create_event(config=nil)
    JigEvent.create :type=>"JigEvent", :jig_id => self.id, 
          :status => JigEvent::EVT_PENDING, :name=>"placeholder"
  end
  
  def create_run_for(evt, nr=nil, order=1000)
    JigRun.create(:type=> "JigRun", :jig_event_id => evt.id, 
      :role => nr, :order=>order, :status => JigRun::RUN_PENDING, 
      :name=>"run_#{evt.id}_#{order}")
  end

  def create_node(node)
    Rails.logger.info("TestJig Creating node: #{node.name}")
  end

  def delete_node(node)
    Rails.logger.info("TestJig Deleting node: #{node.name}")    
  end

  def read_node_data(node)
    ## Return some dummy data to enable unit-tests, for now just safe default
    JSON.parse("{}")
  end   
  
end


