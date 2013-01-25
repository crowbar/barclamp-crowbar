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
require 'spec_helper'
require 'factory_girl_rails'


FactoryGirl.define do 
  sequence :node_id do  |n| 
    "dtest-node-#{n}.foo.bar" 
  end
  sequence :ids do  |n| 
    "#{n}"  
  end
  factory :node do
    name {Factory.next(:node_id)}
    admin false
  end
  factory :role_element_order do
    order 1
  end
  factory :role do | role | 
    priority 1
    # has-many association... funky
    role_element_orders { [association(:role_element_order)] }
  end
  factory :base_barclamp, :class=>Barclamp  do
    name "testing_barclamp"
    version "1.1"
    # has-many association... funky
    roles  { [association(:role,:name => "run_first")]} 
  end 
  factory :prop_config, :class=>ProposalConfig do
  end
  factory :proposal do
    name { "#{barclamp.name}_#{Factory.next(:ids)}" } 
    proposal_configs { [FactoryGirl.build(:prop_config)] }
  end
end


describe "jig proposal manipulation" do

  def setup_test_barclamp_with_2_nodes
    n1 = Factory(:node)
    n2 = Factory(:node)
    barclamp = Barclamp.find_by_name("test")
    params = { "id" =>"test_prop", "attributes" => {} }
    barclamp.operations.proposal_create(params)
    barclamp
  end

  before :each do
        #@barclamp = Factory(:base_barclamp)
        #@proposal = Factory(:proposal, {:barclamp=> @barclamp })
  end

  describe "start a run for a proposal" do

    it "should find the right jig type/instance" do
#      jig = Jig.find_jig_for_config(nil)      
#      assert jig,"have a jig instace of type #{jig.type}"
#      jig.prepare_chef_api 
    end

    it "can create a test proposal with 2 nodes" do
# CB1 TODO - needs to migrate to Config/Instance
#      barclamp = setup_test_barclamp_with_2_nodes()
#      assert_equal 1, barclamp.proposals.length, "proposals length"
#      pc = barclamp.proposals[0].current_config
#      assert_equal 2, pc.node_roles.length, "2 node roles"

#      evt = Jig.prepare_proposal(pc)

      # there should now be: 1 jigChef event, 2 runs - 1 for test-multi-head, 1 for test-multi-rest
#      assert_equal 1, JigEvent.all.length, "1 jig events"
#      assert_equal 2, JigRunChef.all.length, "2 jig chef runs"

      ## Verify that evt <-> run relationship holds, 
#      JigRunChef.all.each { |r| r.jig_event == evt } 
      # verify run status .
#      evt.jig_run.each { |r| assert_equal JigRun::RUN_PENDING, r.status, "runs are pending #{r.inspect}" }            
#      assert_equal JigEvent::EVT_PENDING, evt.status, "event pending #{evt.inspect}"
    end

    it "creates proposal configuration as part of event" do
    end
  end
end

