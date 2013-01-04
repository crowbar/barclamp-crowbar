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


describe "cmdb proposal manipulation" do

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

    it "should find the right cmdb type/instance" do
      cmdb = Cmdb.find_cmdb_for_config(nil)
      assert cmdb,"have a cmdb instace"
    end

    it "can create a test proposal with 2 nodes" do
      barclamp = setup_test_barclamp_with_2_nodes()
      assert_equal 1, barclamp.proposals.length, "proposals length"
      pc = barclamp.proposals[0].current_config
      assert_equal 2, pc.node_roles.length, "2 node roles"

      evt = Cmdb.prepare_proposal(pc)

      # there should now be: 1 cmdbChef event, 2 runs - 1 for test-multi-head, 1 for test-multi-rest
      assert_equal 1, CmdbEvent.all.length, "1 cmdb events"
      assert_equal 2, CmdbRunChef.all.length, "2 cmdb chef runs"

      ## Verify that evt <-> run relationship holds, 
      CmdbRunChef.all.each { |r| r.cmdb_event == evt } 
      # verify run status .
      evt.cmdb_run.each { |r| assert_equal CmdbRun::RUN_PENDING, r.status, "runs are pending #{r.inspect}" }            
      assert_equal CmdbEvent::EVT_PENDING, evt.status, "event pending #{evt.inspect}"
    end

    it "creates proposal configuration as part of event" do
    end
  end
end

