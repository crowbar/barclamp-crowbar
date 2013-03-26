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
require 'test_helper'
 
class SnapshotModelTest < ActiveSupport::TestCase

  def setup
    @bc = Barclamp.create! :name=>'bc_instance_test'
  end
  
  test "add role to instance" do
    name = "foo"
    bi = Snapshot.create :name => "add_role", :barclamp_id => @bc.id
    assert_equal 0, bi.roles.count
    assert_not_nil bi
    # now add the role
    ri = bi.add_role name
    assert_not_nil ri
    assert_equal 1, bi.roles.count
  end
  
  test "Check proections on illegal config names" do
    assert_raise(ActiveRecord::RecordInvalid) { Deployment.create!(:name => "template") }
  end
  
  test "deep clone works" do
    b = Barclamp.import 'test'
    bi = Snapshot.create :name=>"deep_clone", :status => Snapshot::STATUS_APPLIED, :barclamp_id => b.id 
    ri1 = Role.create :name=>"something", :snapshot_id=>bi.id, :order=>100
    ri2 = Role.create :name=>"anotherthing", :snapshot_id=>bi.id, :order=>200
    assert_equal 2, bi.roles(true).count
    clone = bi.deep_clone nil, 'new_me'
    assert_not_nil clone
    assert_equal 'new_me', clone.name
    assert_equal clone.status, Snapshot::STATUS_NONE
    assert_equal 2, clone.roles(true).count
    assert_equal 'something', clone.roles.first.name
    assert_not_equal ri1.id, clone.roles.first.id
    assert_equal 'anotherthing', clone.roles.second.name
    assert_not_equal ri2.id, clone.roles.second.id
  end
  
  test "method missing for roles" do
    b = Barclamp.find_by_name 'test'
    snap = b.template
    assert_equal "test", snap.test_role.name
    assert_equal "private", snap.private_role.name
  end
  
end

