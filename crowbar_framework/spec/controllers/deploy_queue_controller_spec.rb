#
# Copyright 2011-2013, Dell
# Copyright 2013-2014, SUSE LINUX Products GmbH
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#

require 'spec_helper'

describe DeployQueueController do
  render_views

  describe "GET index" do
    it "is successful" do
      get :index
      response.should be_success
    end

    it "is successful when there are clusters in the queue" do
      # Proposal is in the queue
      prop = ProposalObject.find_proposal("database", "default")
      prop["deployment"][prop.barclamp]["crowbar-committing"] = true

      # Appropriately structured queue item
      queue_item = { "barclamp" => prop.barclamp, "inst" => prop.name, "elements" => prop.elements, "deps" => [] }
      @controller.stubs(:deployment_queue).returns([queue_item])

      get :index
      response.should be_success
    end
  end
end
