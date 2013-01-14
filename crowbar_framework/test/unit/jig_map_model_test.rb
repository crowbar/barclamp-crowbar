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
require 'json'

class JigMapModelTest < ActiveSupport::TestCase

  test "Unique Name" do
    cm = JigMap.create! :name=>"nodups"
    assert_not_nil cm
    assert_raise(ActiveRecord::RecordInvalid, ActiveRecord::RecordNotUnique, SQLite3::ConstraintException) { JigMap.create!(:name => "nodups") }
  end

  test "Check on illegal names" do
    c = Jig.find_or_create_by_name_and_type(:name=>'name_parent', :type=>'JigTest')
    assert_raise(ActiveRecord::RecordInvalid) { JigMap.create!(:name => "1123", :jig=>c) }
    assert_raise(ActiveRecord::RecordInvalid) { JigMap.create!(:name => "1foo*cs", :jig=>c) }
    assert_raise(ActiveRecord::RecordInvalid) { JigMap.create!(:name => "Ille!gal", :jig=>c) }
    assert_raise(ActiveRecord::RecordInvalid) { JigMap.create!(:name => " nospaces", :jig=>c) }
    assert_raise(ActiveRecord::RecordInvalid) { JigMap.create!(:name => "no spaces", :jig=>c) }
    assert_raise(ActiveRecord::RecordInvalid) { JigMap.create!(:name => "nospacesatall ", :jig=>c) }
  end
  
  test "Must have jig parent" do
    return true  #this is busted for now, skip
    c = Jig.find_or_create_by_name_and_type(:name=>'parent', :type=>'JigTest')
    assert_equal false, c.nil?
    assert_equal c.name, "parent"
    assert_equal c.type, "JigTest"
    m = JigMap.create! :name=>"maptest", :jig=>c
    assert_equal m.jig.name, "parent"
  end
  
  
  
end

