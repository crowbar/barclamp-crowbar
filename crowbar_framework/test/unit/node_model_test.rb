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
 
class NodeModelTest < ActiveSupport::TestCase

  def setup
    @crowbar = Barclamp.find_by_name("crowbar") 
    assert_not_nil @crowbar
    d = Deployment.find_or_create_by_name :name=>'system', :description=>'automatic'
    assert_not_nil d, 'we need at least 1 Deployment'
    #  We need a system deployment to create default proposals in.
    d.send(:write_attribute,"system",true)
    # We also need a snapshot.
    snap = Snapshot.create(:deployment_id=>d.id, :name => d.name, :description => d.description)
    snap.save!
    d.active_snapshot_id = snap.id
    d.save!
  end


  test "Unique Name" do
    Node.create! :name=>"foo.example.com"
    e = assert_raise(ActiveRecord::RecordInvalid, ActiveRecord::RecordNotUnique, SQLite3::ConstraintException) { Node.create!(:name => "foo.example.com") }
    assert_equal "Validation failed: Name Item must be un...", e.message.truncate(42)

    assert_raise(ActiveRecord::RecordInvalid, ActiveRecord::RecordNotUnique, SQLite3::ConstraintException) { b = Node.create! :name => "foo.example.com" }
  end

  test "name too long" do
    assert_raise(ActiveRecord::RecordInvalid, SQLite3::ConstraintException) { Node.create!(:name=>"12345678901234567890123456789012345678901234567890.12345678901234567890123456789012345678901234567890.12345678901234567890123456789012345678901234567890.12345678901234567890123456789012345678901234567890.12345678901234567890123456789012345678901234567890.com") }
  end
  
  test "lower case required" do
    name = "THIS.ISALL.CAPS"
    n = Node.create! :name=>name
    assert_not_equal n.name, name
    assert_equal n.name, name.downcase
    assert_raise(ActiveRecord::RecordInvalid, ActiveRecord::RecordInvalid, SQLite3::ConstraintException) { b = Node.create! :name => name }
  end
  
  test "not set group" do
    n = Node.create! :name=>"not-set.example.com"
    assert_not_nil n
    g = Group.find_by_name "not_set"
    assert_not_nil g
    assert_equal g.name, "not_set"
    assert_equal n.groups.size, 1
    assert_equal n.groups[0].id, g.id
  end
  
  test "Naming Conventions" do
    assert_raise(ActiveRecord::RecordInvalid, SQLite3::ConstraintException) { Node.create!(:name=>"fqdnrequired") }
    assert_raise(ActiveRecord::RecordInvalid, SQLite3::ConstraintException) { Node.create!(:name=>"1no.legal.domain") }
    assert_raise(ActiveRecord::RecordInvalid, SQLite3::ConstraintException) { Node.create!(:name=>"1123.foo.com") }
    assert_raise(ActiveRecord::RecordInvalid, SQLite3::ConstraintException) { Node.create!(:name=>"1foo.bar.net") }
    assert_raise(ActiveRecord::RecordInvalid, SQLite3::ConstraintException) { Node.create!(:name=>"Ille!gal.foo.org") }
    assert_raise(ActiveRecord::RecordInvalid, SQLite3::ConstraintException) { Node.create!(:name=>" nospaces.bar.it") }
    assert_raise(ActiveRecord::RecordInvalid, SQLite3::ConstraintException) { Node.create!(:name=>"no spaces.dell.com") }
    assert_raise(ActiveRecord::RecordInvalid, SQLite3::ConstraintException) { Node.create!(:name=>"nospacesatall.end.edu ") }
    assert_raise(ActiveRecord::RecordInvalid, SQLite3::ConstraintException) { Node.create!(:name=>"musthaveatleastthreedomains.com") }
  end

end

