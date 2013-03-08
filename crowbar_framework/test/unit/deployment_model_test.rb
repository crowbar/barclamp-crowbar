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
 
class DeploymentModelTest < ActiveSupport::TestCase

  def setup
    @bc = Barclamp.import 'foo', nil, 'test/data/foo'
  end
  
  test "Unique per Barclamp Name" do
    b1 = Barclamp.create! :name=>"nodup1"
    b2 = Barclamp.create! :name=>"nodup2"
    assert_not_nil b1
    assert_not_nil b2
    bc1 = Deployment.create :name=>"nodup", :barclamp_id=>b1.id
    assert_not_nil bc1
    bc2 = Deployment.create :name=>"nodup", :barclamp_id=>b2.id
    assert_not_nil bc2
    assert_not_equal bc1.id, bc2.id
    
    e = assert_raise(ActiveRecord::RecordInvalid, ActiveRecord::RecordNotUnique, SQLite3::ConstraintException) { Deployment.create!(:name => "nodup", :barclamp_id=>b1.id) }
  end
  
  test "Check protections on illegal names" do
    assert_raise(ActiveRecord::RecordInvalid) { Deployment.create!(:name => "1123", :barclamp_id=>@bc.id) }
    assert_raise(ActiveRecord::RecordInvalid) { Deployment.create!(:name => "1foo", :barclamp_id=>@bc.id) }
    assert_raise(ActiveRecord::RecordInvalid) { Deployment.create!(:name => "Ille!gal", :barclamp_id=>@bc.id) }
    assert_raise(ActiveRecord::RecordInvalid) { Deployment.create!(:name => " nospaces", :barclamp_id=>@bc.id) }
    assert_raise(ActiveRecord::RecordInvalid) { Deployment.create!(:name => "no spaces", :barclamp_id=>@bc.id) }
    assert_raise(ActiveRecord::RecordInvalid) { Deployment.create!(:name => "nospacesatall ", :barclamp_id=>@bc.id) }
  end

  test "Active works" do
    config = @bc.create_proposal "active"
    assert_not_nil config
    assert_equal 1, config.snapshots.count
    assert       !config.active?
    assert_nil   config.active_snapshot
    assert_nil   config.active_snapshot
    # now activate
    config.active_snapshot = config.proposed_snapshot
    config.save
    assert_equal 1, config.snapshots(true).count
    assert       config.active?
    assert_not_nil config.active_snapshot
  end
  
  test "status check missing" do
    config = Deployment.create :name=>"status", :barclamp_id=>@bc.id
    snapshot = Snapshot.create :name=>"status2", :deployment_id=>config.id, :barclamp_id => @bc.id
    c = Deployment.find config.id
    assert_equal  'inactive', c.status
  end
  
  test "status check none" do
    config = Deployment.create :name=>"status", :barclamp_id=>@bc.id
    snapshot = Snapshot.create :name=>"status2", :deployment_id=>config.id, :status =>Snapshot::STATUS_NONE, :barclamp_id => @bc.id
    config.active_snapshot = snapshot
    config.save!
    c = Deployment.find config.id
    assert_equal  'none', c.status
  end
  
  test "status check pending" do    
    config = Deployment.create :name=>"status", :barclamp_id=>@bc.id
    snapshot = Snapshot.create :name=>"status2", :deployment_id=>config.id, :status =>Snapshot::STATUS_QUEUED, :barclamp_id => @bc.id
    config.active_snapshot = snapshot
    config.save!
    c = Deployment.find config.id
    assert_equal  'pending', c.status  
  end
  
  test "status check unready" do
    config = Deployment.create :name=>"status", :barclamp_id=>@bc.id
    snapshot = Snapshot.create :name=>"status2", :deployment_id=>config.id, :status =>Snapshot::STATUS_COMMITTING, :barclamp_id => @bc.id
    config.active_snapshot = snapshot
    config.save!
    c = Deployment.find config.id
    assert_equal  'unready', c.status
  end
  
  test "status check failed" do
    config = Deployment.create :name=>"status", :barclamp_id=>@bc.id
    snapshot = Snapshot.create :name=>"status2", :deployment_id=>config.id, :status =>Snapshot::STATUS_FAILED, :barclamp_id => @bc.id
    config.active_snapshot = snapshot
    config.save!
    c = Deployment.find config.id
    assert_equal  'failed', c.status
  end
  
  test "status check applied" do
    config = Deployment.create :name=>"status", :barclamp_id=>@bc.id
    snapshot = Snapshot.create :name=>"status2", :deployment_id=>config.id, :status => Snapshot::STATUS_APPLIED, :barclamp_id => @bc.id
    config.active_snapshot = snapshot
    config.save!
    c = Deployment.find config.id
    assert_equal  'ready', c.status
  end
  
  test "status check hold" do
    config = Deployment.create :name=>"status", :barclamp_id=>@bc.id
    snapshot = Snapshot.create :name=>"status2", :deployment_id=>config.id, :status => -1, :barclamp_id => @bc.id
    config.active_snapshot = snapshot
    config.save!
    c = Deployment.find config.id
    assert_equal  'hold', c.status    
  end
  
  test "create proposal without name is default" do
    test = Barclamp.import 'test'
    assert_equal 0, test.deployments.count
    assert_not_nil test.template
    config = test.create_proposal
    assert_not_nil config
    assert_equal 1, test.deployments(true).count
    assert_equal config.id, test.deployments.first.id
    assert_equal Barclamp::DEFAULT_DEPLOYMENT_NAME, config.name
    assert_equal test.id, config.barclamp_id
  end
  
  test "Can create config from barclamp" do
    test = Barclamp.import 'test'
    assert !test.allow_multiple_deployments, "need this to be 1 for this test"
    assert_equal 0, test.deployments.count
    assert_not_nil test.template
    config = test.create_proposal 'foo'
    assert_not_nil config
    assert_equal 1, test.deployments(true).count
    assert_equal config.id, test.deployments.first.id
    assert_equal 'foo', config.name
    assert_equal test.id, config.barclamp_id
  end
  
  test "Allow multiple proposals works" do
    test = Barclamp.import 'test'
    assert !test.allow_multiple_deployments, "need this to be 1 for this test"
    assert_equal 0, test.deployments.count
    assert_not_nil test.template
    config = test.create_proposal 'foo'
    assert_not_nil config
    # this will fail
    c2 = test.create_proposal 'bar'
    assert_nil c2
    assert_equal 'foo', test.deployments(true).first.name
    assert_equal 1, test.deployments(true).count
    # now change the setting and try again
    test.allow_multiple_deployments = true
    c3 = test.create_proposal 'bar'
    assert_not_nil c3
    assert_equal 'bar', c3.name
    assert_equal 2, test.deployments(true).count
  end
  
  test "create proposal clones roles" do    
    test = Barclamp.import  'test'
    assert_not_nil test
    count = test.template.roles.count
    r = test.template.add_role "clone_me"
    assert_not_nil r
    # make sure this role is first in the list
    r.run_order = 1
    r.order = 1
    r.save
    assert_equal "clone_me", r.role_type.name
    assert_equal r.role_type_id, test.template.roles.second.role_type_id, "confirm that added role is second after private"
    # now make sure it shiows up in the cone
    config = test.create_proposal "cloned"
    assert_not_nil config
    assert_not_equal test.template_id, config.id
    assert_equal test.template.roles.count, config.proposed.roles.count
    assert_equal test.template.roles.second.role_type_id, config.proposed.roles.second.role_type_id
  end
  
  test "commit proposal clones" do
    test = Barclamp.import 'test'
    dep = test.create_deployment "foo"
    prop = dep.proposal
    assert_not_nil dep.proposal
    assert_nil dep.committed_snapshot_id
    assert !dep.committed?
    commit = dep.commit
    assert_equal commit.id, dep.committed_snapshot_id, "pointers moved"
    assert_not_equal prop.id, dep.proposed_snapshot_id, "new prop different"
    assert dep.committed?
  end
  
  test "recall commit deletes" do
    test = Barclamp.import 'test'
    dep = test.create_deployment "foo"
    prop_old = dep.proposal
    prop_new = dep.commit
    assert_not_equal prop_old.id, dep.proposed.id
    dep.recall
    assert_nil dep.committed_snapshot_id, "pointer is null"
    assert_equal [], Snapshot.where(:id=>prop_new.id), "it's deleted"
  end
  
  test "deallocate has no nodes" do
    test = Barclamp.import 'test'
    node = Node.create :name=>"unit.test.com"
    dep = test.create_deployment "foo"
    # add node
    dep.proposed.roles.first.add_node node
    # get to an active snapshot
    dep.commit
    active = dep.activate_committed
    assert_nil dep.committed_snapshot_id, "active = none committed"
    # make sure we have a node
    assert active.nodes.count > 0 
    # node deallocate
    commit = dep.commit_deallocate_active
    assert_equal 0, commit.nodes.count, "deallocate means no nodes"
  end
  
  test "commit_to_apply removes commit" do
    test = Barclamp.import 'test'
    dep = test.create_deployment "foo"
    prop_old = dep.proposal
    commit_new = dep.commit
    prop_new = dep.proposal
    assert_nil dep.active
    assert_not_equal prop_old.id, dep.proposed.id
    act = dep.activate_committed
    assert_nil dep.committed_snapshot_id
    assert_equal commit_new.id, dep.active_snapshot_id
    assert_not_equal dep.proposed.id, act.id
    # test with TWO cycles!
    prop_new = dep.commit
    act_old = dep.active
    dep.activate_committed
    assert_equal prop_new.id, dep.active.id
    assert_equal [], Snapshot.where(:id=>act_old.id), "remove the next old one"
  end
  
  test "snapshot status tests" do
    test = Barclamp.import 'test'
    dep = test.create_deployment "foo"
    assert dep.proposed.proposed? 
    assert !dep.proposed.active? 
    assert !dep.proposed.committed? 
    dep.commit
    assert dep.committed.committed?
    assert !dep.committed.active?
    assert !dep.committed.proposed?
    dep.activate_committed
    assert dep.active.active?
    assert !dep.active.proposed?
    assert !dep.active.committed?
  end
  
end

