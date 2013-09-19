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

describe "admin create" do

  include_context "crowbar test deployment"
  subject { Node.create :name=>'rspec_admin.crowbar.com', :admin => true   }

  it {should be_is_admin} 

  it "must have bootstrap and implicit roles" do
    expect {node_roles.any?{ |e| e.role.bootstrap }}.to be_true
    expect {node_roles.any?{ |e| e.role.implicit }}.to be_true
  end

end

describe "node create" do

  include_context "crowbar test deployment"
  subject { Node.create :name=>'rspec_node.crowbar.com', :admin => false  }


  it "must have inplicit roles" do
    expect {its.node_roles.any?{ |e| e.role.implicit }}.to be_true
    expect {its.node_roles.any?{ |e| e.role.discovery }}.to be_true
  end

  it "must be added to the deployment" do
    expect {deployment.active.node_roles.any?{ |e| e.node.id == its.id }}.to be_true
  end

end

