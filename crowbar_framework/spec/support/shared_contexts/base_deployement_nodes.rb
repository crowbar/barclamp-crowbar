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



# This context establsihes a base Crowbar deployment.
# it should be included in all tests that manipulate node attributes
shared_context "crowbar test deployment" do
  
  before(:all) do
    # we need this to ensure that we have the crowbar barclamp
    Barclamp.import 'crowbar'
    c = Barclamp.find_by_name('crowbar')
    c.create_proposal :name=>'test'
    # we also need to have the test jig
    BarclampCrowbar::Jig.find_or_create_by_name :name=>'test'
  end
end  

# Just 2 dummy nodes
shared_context "2 dummy nodes" do
  let(:node1) { Node.create :name=>"unit1.test.com" }
  let(:node2) { Node.create :name=>"unit2.test.com" }
end
