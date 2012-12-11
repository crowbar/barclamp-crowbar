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
		"node-#{n}"	
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
		# has-many association... funky
		 role_element_orders { [association(:role_element_order)] }
	end


	factory :base_barclamp, :class=>Barclamp  do
		name "testing_barclamp"
		version "1.1"
		# has-many association... funky
		roles  { [association(:role,:name => "run_first")]} 
	end	


	factory :proposal do		
		name { "#{barclamp.name}_#{Factory.next(:ids)}" } 
	end
end


describe CmdbRun do
	before :each do
		@barclamp = Factory(:base_barclamp)
		@proposal = Factory(:proposal, {:barclamp=> @barclamp })		
	end

	describe "FG setup" do
		it "should have a proposal" do
			@barclamp.proposals.length.should be 1 			
		end
	end
end

