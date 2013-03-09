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
  factory :role do | role | 
    priority 1
  end
  factory :base_barclamp, :class=>Barclamp  do
    name "testing_barclamp"
    version "1.1"
    # has-many association... funky
    roles  { [association(:role,:name => "run_first")]} 
  end 
end


describe "jig proposal manipulation" do
  # make sure that the there's a crowbar deploment (named 'test')
  include_context "crowbar test deployment"

  def setup_test_barclamp_with_2_nodes
    n1 = Factory(:node)
    n2 = Factory(:node)
    barclamp = Barclamp.find_by_name("test")
    params = { "id" =>"test_prop", "attributes" => {} }
    barclamp.proposal_create(params)
    barclamp
  end

  def create_basic_prop
    test = Barclamp.import 'test'
    node1 = Node.create :name=>"unit1.test.com"
    node2 = Node.create :name=>"unit2.test.com"
    dep = test.create_deployment "foo"
    # add node
    dep.proposed.roles.first.add_node node1
    dep.proposed.roles.second.add_node node2
    # get to an active snapshot
    dep.commit
  end

  it "can create basic proposal with 2 nodes" do
    create_basic_prop
  end


end

