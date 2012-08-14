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
require 'test_helper'
 
class NodeModelTest < ActiveSupport::TestCase

  test "Unique Name" do
    Node.create :name=>"foo"
    e = assert_raise(ActiveRecord::RecordInvalid) { Node.create!(:name => "foo") }
    assert_equal "Validation failed: Name Name item must be unique", e.message

    b = Node.create(:name => "foo")
    b = b.save
    assert_equal false, b
  end

  test "not set group" do
    n = Node.create :name=>"not_set"
    g = Group.find_by_name "not_set"
    assert_not_nil g
    assert_true n.groups.size > 0
    assert_true n.groups.include? g
  end

  test "fingerprint" do
    n = Node.create :name=>"fingerprint"
    assert_not_nil n.fingerprint
    assert_not_equal n.fingerprint, 0
    fp = n.fingerprint
    n.name = "hash"
    n.save
    assert_not_equal n.fingerprint, fp
  end
  
  test "Naming Conventions" do
    assert_raise(ActiveRecord::RecordInvalid) { Node.create(:name=>"1123") }
    assert_raise(ActiveRecord::RecordInvalid) { Node.create(:name=>"1foo") }
    assert_raise(ActiveRecord::RecordInvalid) { Node.create(:name=>"Ille!gal") }
    assert_raise(ActiveRecord::RecordInvalid) { Node.create(:name=>" nospaces") }
    assert_raise(ActiveRecord::RecordInvalid) { Node.create(:name=>"no spaces") }
    assert_raise(ActiveRecord::RecordInvalid) { Node.create(:name=>"nospacesatall ") }
  end

end

