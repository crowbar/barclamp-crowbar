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
    before do
      # We don't have Pacemaker at hand and the helper returns false because of
      # that, need to stub this
      ServiceObject.stubs(:is_cluster?).returns(true)
    end

    let(:prop) { Proposal.where(barclamp: "crowbar", name: "default").first_or_create(barclamp: "crowbar", name: "default") }

    it "is successful" do
      get :index
      response.should be_success
    end

    describe "with existing nodes" do
      before do
        # Simulate the expansion to a node whose look up we can fake
        ServiceObject.stubs(:expand_nodes_for_all).returns([["testing.crowbar.com"],[]])
      end

      it "is successful when a prop with clusters is deployed" do
        # Is now deploying
        @controller.stubs(:currently_deployed).returns(prop)

        get :index
        response.should be_success
      end

      it "is successful when there are clusters in the queue" do
        # Is queued
        queue_item = { "barclamp" => prop.barclamp, "inst" => prop.name, "elements" => prop.elements, "deps" => [] }
        @controller.stubs(:deployment_queue).returns([queue_item])

        get :index
        response.should be_success
      end
    end

    describe "with non-existing nodes" do
      before do
        # Cluster referencing a non-existent node (deleted)
        ServiceObject.stubs(:expand_nodes_for_all).returns([["I just dont exist"],[]])
      end

      it "is successful when a prop with clusters is deployed" do
        # Is now deploying
        @controller.stubs(:currently_deployed).returns(prop)

        get :index
        response.should be_success
      end

      it "is successful for clusters in the queue" do
        queue_item = { "barclamp" => prop.barclamp, "inst" => prop.name, "elements" => prop.elements, "deps" => [] }
        @controller.stubs(:deployment_queue).returns([queue_item])

        get :index
        response.should be_success
      end
    end
  end
end
