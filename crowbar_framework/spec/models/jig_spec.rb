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
require 'spec_helper'
require 'factory_girl_rails'


describe "jig proposal manipulation" do
  # make sure that the there's a crowbar deploment (named 'test')
  include_context "crowbar test deployment"
  # just 2 nodes.
  include_context "2 dummy nodes"


  context "test barclamp with 2 nodes" do
    before(:all) {
      barclamp = Barclamp.find_by_name("test")
      params = { "id" =>"test_prop", "attributes" => {} }
      dep = barclamp.create_deployment "foo"
      barclamp.create_proposal(params)
      # add node
      dep.proposed.roles.first.add_node node1
      dep.proposed.roles.second.add_node node2
      dep.commit
    }


    it "should create event and runs" do

    end
  end
end

