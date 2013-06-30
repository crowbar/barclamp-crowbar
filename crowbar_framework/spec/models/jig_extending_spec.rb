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

describe Jig do 

  # make sure that the there's a crowbar deploment (named 'test')
  include_context "crowbar test deployment"
  include_context "2 dummy nodes"

  let(:jig1) { double('jig1') }
  let(:jig2) { double('jig2') }
  let(:jigs) { [jig1, jig2] }
  before(:each) { Jig.stub(:all) { jigs } }

  def set_expectation(what,_with)
    jigs.each { |j| j.should_receive(what).with(_with) }
  end

  it "should broadcast create to all jigs" do
    node = Node.create :name=>"test"
    set_expectation(:create_node,node)
    Jig.create_node(node)
  end

  it "should broadcast delete to all jigs" do
    node = Node.create :name=>"test"
    set_expectation(:delete_node,node)
    Jig.delete_node(node)
  end

  it "should broadcast refresh to all jigs" do
    node = Node.create :name=>"test"
    #jig1.should_receive(:read_node_data,node)
    #jig2.should_receive(:read_node_data,node)
    #Jig.refresh_node("just a test", node)
  end


 
end